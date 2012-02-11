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
 * A common place to log which currently uses System.err.
 * We can't use log4j or slf4j 1) because do not want to have too many dependencies and 2) there is a risk of re-entrance.
 * Both problems can be solved which is why I have put this class in place.
 */
public class SimpleLogger {
    public static void error(String s) {
        System.err.println(s);
    }
}
