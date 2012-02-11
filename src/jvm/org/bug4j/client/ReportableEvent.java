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

import org.bug4j.common.TextToLines;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

class ReportableEvent {
    private final String _message;
    private final String[] _throwableStrRep;
    private final String _user;

    public ReportableEvent(@Nullable String message, @Nullable String[] throwableStrRep, @Nullable String user) {
        _message = message;
        _throwableStrRep = throwableStrRep;
        _user = user;
    }

    @Nullable
    public String getMessage() {
        return _message;
    }

    @Nullable
    public String[] getThrowableStrRep() {
        return _throwableStrRep;
    }

    @Nullable
    public String getUser() {
        return _user;
    }

    @NotNull
    public static ReportableEvent createReportableEvent(@Nullable String message, @Nullable Throwable throwable) {
        final String[] throwableStrRep = createStringRepresentation(throwable);
        final String user = System.getProperty("user.name", null);
        return new ReportableEvent(message, throwableStrRep, user);
    }

    @Nullable
    private static String[] createStringRepresentation(Throwable throwable) {
        String[] ret = null;
        if (throwable != null) {
            final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            final PrintStream printStream = new PrintStream(byteArrayOutputStream);
            try {
                throwable.printStackTrace(printStream);
            } finally {
                printStream.close();
            }
            final String stackText = byteArrayOutputStream.toString();
            ret = TextToLines.toLineArray(stackText);
        }
        return ret;
    }
}
