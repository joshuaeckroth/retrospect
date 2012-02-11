/*
 * Copyright 2011 Cedric Dandoy
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package org.bug4j.client;

import org.bug4j.common.FullStackHashCalculator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Reports exceptions to the bug4j server.
 * Exceptions are processed by a background thread which is started by {@link Bug4jStarter}.
 * The Bug4jAgent will be started automatically with the first exception report.
 */
public class Bug4jAgent {
    /**
     * Maximum number of reports to enqueue when the server is not responding.
     */
    private static final int MAX_ENQUEUE = 200;

    /**
     * When the server does not respond, this is how long we wait until we retry
     */
    private static final int RETRY_MILLIS = 10 * 1000;

    /**
     * Avoid re-entrance caused by calls to log4j during the initialization
     */
    private static AtomicBoolean _isEntered = new AtomicBoolean(false);
    private static final BlockingQueue<ReportableEvent> _queue = new LinkedBlockingQueue<ReportableEvent>();
    private static final ReportableEvent STOP = new ReportableEvent("stop", new String[0], "stop");
    private static Thread _clientThread;
    private static int _reported;
    private final ReportLRU _reportLRU = new ReportLRU();

    private HttpConnector _connector;
    private final Settings _settings;

    private Bug4jAgent(Settings settings) {
        _settings = settings;
    }

    static void start(Settings settings) {
        if (_isEntered.compareAndSet(false, true)) {
            try {
                if (!isStarted()) {
                    synchronized (Bug4jAgent.class) {
                        _reported = 0;

                        final Bug4jAgent client = new Bug4jAgent(settings);

                        final Object tellMeWhenYouAreReady = new Object();
                        final Thread thread = new Thread() {
                            @Override
                            public void run() {
                                synchronized (tellMeWhenYouAreReady) {
                                    tellMeWhenYouAreReady.notify();
                                }

                                try {
                                    client.run();
                                } catch (InterruptedException ignored) {
                                }
                            }
                        };
                        thread.setName("bug4j");
                        thread.setDaemon(true);
                        thread.start();
                        //noinspection SynchronizationOnLocalVariableOrMethodParameter
                        synchronized (tellMeWhenYouAreReady) {
                            try {
                                tellMeWhenYouAreReady.wait();
                                _clientThread = thread;
                            } catch (InterruptedException e) {
                                throw new IllegalStateException(e);
                            }
                        }
                    }
                }
            } finally {
                _isEntered.set(false);
            }
        }
    }

    private static boolean isStarted() {
        return _clientThread != null;
    }

    private void init() {
        if (_connector == null) {
            final String serverUrl = _settings.getServerUrl();
            final String applicationName = _settings.getApplicationName();
            final String applicationVersion = _settings.getApplicationVersion();
            final String proxyHost = _settings.getProxyHost();
            final int proxyPort = _settings.getProxyPort();
            _connector = HttpConnector.createHttpConnector(serverUrl, applicationName, applicationVersion, proxyHost, proxyPort);
        }
    }

    /**
     * Shuts down the bug4j thread.
     */
    public static void shutdown() {

        // if the client thread has not been started yet then there is nothing to stop
        // but the client may be starting up => sync.
        synchronized (Bug4jAgent.class) {
            if (_clientThread == null) {
                return;
            }
        }

        enqueue(STOP);
        try {
            _clientThread.join(RETRY_MILLIS * 2);
            _clientThread = null;
        } catch (InterruptedException e) {
            //
        }
        _queue.clear();
    }

    private void run() throws InterruptedException {
        while (true) {
            final ReportableEvent reportableEvent = getNextReportableEvent();
            if (reportableEvent == null) {
                break;
            }
            while (!processSafely(reportableEvent)) {
                Thread.sleep(RETRY_MILLIS); // Try again in 10 seconds
            }
        }
    }

    private boolean processSafely(ReportableEvent reportableEvent) {
        boolean ret = false;
        try {
            init();
            process(reportableEvent);
            ret = true;
        } catch (Exception e) {
            // ret stays false
        }
        return ret;
    }

    private ReportableEvent getNextReportableEvent() {
        ReportableEvent ret = null;
        try {
            final ReportableEvent top = _queue.take();
            if (top != STOP) {
                ret = top;
            }
        } catch (InterruptedException e) {
            // ret stays null
        }
        return ret;
    }

    private void process(ReportableEvent reportableEvent) {
        final String textHash = hash(reportableEvent);
        if (_reportLRU.put(textHash)) { // Refrain from sending the same eror
            final String message = reportableEvent.getMessage();
            final String user = reportableEvent.getUser();
            final boolean isNew = _connector.reportHit(message, user, textHash);
            if (isNew) {
                final String[] throwableStrRep = reportableEvent.getThrowableStrRep();
                _connector.reportBug(
                        message,
                        user,
                        throwableStrRep
                );
            }
            _reported++;
        }
    }

    @NotNull
    private static String hash(ReportableEvent reportableEvent) {
        final String ret;
        final String[] throwableStrRep = reportableEvent.getThrowableStrRep();
        final List<String> hashable = throwableStrRep == null
                                      ? Collections.singletonList(reportableEvent.getMessage())
                                      : Arrays.asList(throwableStrRep);
        ret = FullStackHashCalculator.getTextHash(hashable);
        return ret;
    }

    /**
     * Reports an exception to the server
     *
     * @param message   An error message
     * @param throwable the exception to report
     */
    public static void report(@Nullable String message, @Nullable Throwable throwable) {
        if (message != null || throwable != null) { // do not allow both to be null. We wouldn't have much to do
            final ReportableEvent reportableEvent = ReportableEvent.createReportableEvent(message, throwable);
            enqueue(reportableEvent);
        }
    }

    /**
     * Reports an exception to the server
     *
     * @param throwable the exception to report
     */
    public static void report(Throwable throwable) {
        report(null, throwable);
    }

    static void enqueue(ReportableEvent reportableEvent) {
        if (_queue.size() < MAX_ENQUEUE) {
            if (!isStarted()) {
                new Bug4jStarter().start();
            }
            _queue.add(reportableEvent);
        }
    }

    public static int getReported() {
        return _reported;
    }
}
