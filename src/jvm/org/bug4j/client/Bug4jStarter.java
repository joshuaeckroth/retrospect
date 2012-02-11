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

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Builder class to start the bug4j client.
 * Example:<pre>
 *     new Bug4jStarter()
 *          .serverUrl("http://host-name:8063/bug4j")
 *          .start();
 * </pre>
 */
public final class Bug4jStarter {
    private final Settings _settings = new Settings();

    public Bug4jStarter() {
        readDefaultSettings();
    }

    private void readDefaultSettings() {
        final Properties properties = readProperties();
        for (String name : properties.stringPropertyNames()) {
            final String value = properties.getProperty(name);
            if ("server".equals(name)) {
                setServerUrl(value);
                SimpleLogger.error("bug4j warning: property 'server' is deprecated. Please use 'server.url' instead");
            } else if ("server.url".equals(name)) {
                setServerUrl(value);
            } else if ("application.name".equals(name)) {
                setApplicationName(value);
            } else if ("application.version".equals(name)) {
                setApplicationVersion(value);
            } else if ("proxy".equals(name)) {
                setProxy(value);
            } else {
                SimpleLogger.error("Unknown property: " + name);
            }
        }
    }

    private static Properties readProperties() {
        final Properties ret = new Properties();
        final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        final InputStream inputStream = classLoader.getResourceAsStream("bug4j.properties");
        if (inputStream != null) {
            try {
                try {
                    ret.load(inputStream);
                } finally {
                    inputStream.close();
                }
            } catch (IOException e) {
                final String message = e.getMessage();
                SimpleLogger.error(message);
            }
        }
        return ret;
    }

    /**
     * Sets the server URL.
     * The server url should be something like http://host-name:8063/bug4j
     */
    public Bug4jStarter setServerUrl(String serverUrl) {
        _settings.setServerUrl(serverUrl);
        return this;
    }

    /**
     * Sets the application name.
     */
    public Bug4jStarter setApplicationName(String applicationName) {
        _settings.setApplicationName(applicationName);
        return this;
    }

    /**
     * Sets the application version.
     * Example: "2011-06-15 08:32:17"
     */
    public Bug4jStarter setApplicationVersion(String applicationVersion) {
        _settings.setApplicationVersion(applicationVersion);
        return this;
    }

    /**
     * Set the proxy.
     *
     * @param proxy the proxy host name and port separated by a ':'. <br/>For example "proxy.example.com:8080".
     */
    public Bug4jStarter setProxy(String proxy) {

        final String proxyHost;
        int proxyPort = 80;

        final int pos = proxy.indexOf(':');
        if (pos > -1) {
            proxyHost = proxy.substring(0, pos);
            final String portValue = proxy.substring(pos + 1);
            try {
                proxyPort = Integer.parseInt(portValue);
            } catch (NumberFormatException e) {
                SimpleLogger.error("Invalid proxy port: " + portValue);
            }
        } else {
            proxyHost = proxy;
        }

        _settings.setProxyHost(proxyHost);
        _settings.setProxyPort(proxyPort);

        return this;
    }

    /**
     * Start the bug4j thread
     */
    public void start() {
        Bug4jAgent.start(_settings);
    }
}
