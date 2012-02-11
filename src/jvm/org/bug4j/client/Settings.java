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

class Settings {
    private String _serverUrl;
    private String _applicationName;
    private String _applicationVersion;
    private String _proxyHost;
    private int _proxyPort = 80;

    Settings() {
        _applicationName = "no-name";
        _applicationVersion = "1.0";
        _serverUrl = "http://report.bug4j.org:8063/bug4j/";
    }

    public String getServerUrl() {
        return _serverUrl;
    }

    public void setServerUrl(String serverUrl) {
        _serverUrl = serverUrl;
    }

    public String getApplicationName() {
        return _applicationName;
    }

    public void setApplicationName(String applicationName) {
        _applicationName = applicationName;
    }

    public String getApplicationVersion() {
        return _applicationVersion;
    }

    public void setApplicationVersion(String applicationVersion) {
        _applicationVersion = applicationVersion;
    }

    public String getProxyHost() {
        return _proxyHost;
    }

    public void setProxyHost(String proxyHost) {
        _proxyHost = proxyHost;
    }

    public int getProxyPort() {
        return _proxyPort;
    }

    public void setProxyPort(int proxyPort) {
        _proxyPort = proxyPort;
    }
}
