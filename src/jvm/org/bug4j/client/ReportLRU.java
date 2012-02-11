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

import java.util.LinkedHashMap;
import java.util.Map;

class ReportLRU extends LinkedHashMap<String, Long> {
    private static final int MAX_ENTRIES = 30;

    public ReportLRU() {
        super(16, .75f, true);
    }

    @Override
    protected boolean removeEldestEntry(Map.Entry<String, Long> eldest) {
        return size() > MAX_ENTRIES;
    }

    /**
     * @return true if the bug should be reported.
     */
    public boolean put(String textHash) {
        final Long old = super.put(textHash, System.currentTimeMillis());
        return old == null;
    }
}
