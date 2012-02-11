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

/**
 * This class can be used as the default exception handler for a thread.
 *
 * @see #install()
 */
public class Bug4jUncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {

    private Bug4jUncaughtExceptionHandler() {
    }

    @Override
    public void uncaughtException(Thread thread, Throwable throwable) {
        Bug4jAgent.report("Uncaught exception", throwable);
    }

    /**
     * Installs a bug4j default exception handler.
     *
     * @see Thread#setDefaultUncaughtExceptionHandler(java.lang.Thread.UncaughtExceptionHandler)
     */
    public static void install() {
        final Bug4jUncaughtExceptionHandler bug4jUncaughtExceptionHandler = new Bug4jUncaughtExceptionHandler();
        Thread.setDefaultUncaughtExceptionHandler(bug4jUncaughtExceptionHandler);
    }
}
