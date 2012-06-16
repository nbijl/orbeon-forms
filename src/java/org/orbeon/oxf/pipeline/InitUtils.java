/**
 * Copyright (C) 2010 Orbeon, Inc.
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
 */
package org.orbeon.oxf.pipeline;

import org.orbeon.exception.*;
import org.apache.log4j.Logger;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.QName;
import org.orbeon.oxf.cache.ObjectCache;
import org.orbeon.oxf.common.OXFException;
import org.orbeon.oxf.common.ValidationException;
import org.orbeon.oxf.pipeline.api.ExternalContext;
import org.orbeon.oxf.pipeline.api.PipelineContext;
import org.orbeon.oxf.pipeline.api.ProcessorDefinition;
import org.orbeon.oxf.processor.*;
import org.orbeon.oxf.processor.generator.DOMGenerator;
import org.orbeon.oxf.properties.Properties;
import org.orbeon.oxf.resources.ResourceNotFoundException;
import org.orbeon.oxf.util.AttributesToMap;
import org.orbeon.oxf.util.PipelineUtils;
import org.orbeon.oxf.webapp.JWebAppContext;
import org.orbeon.oxf.webapp.WebAppExternalContext;
import org.orbeon.oxf.xml.dom4j.Dom4jUtils;
import org.orbeon.oxf.xml.dom4j.LocationData;
import org.orbeon.saxon.om.NodeInfo;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.net.URI;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;

public class InitUtils {


    private static final String CACHE_SIZE_PROPERTY = "oxf.cache.size";

    public static final String PROLOGUE_PROPERTY = "oxf.prologue";
    public static final String DEFAULT_PROLOGUE = "oxf:/processors.xml";

    private static boolean processorDefinitionsInitialized;

    /**
     * Run a processor with an ExternalContext.
     */
    public static void runProcessor(Processor processor, ExternalContext externalContext, PipelineContext pipelineContext, Logger logger) throws Exception {

        // Record start time for this request
        final long tsBegin = logger.isInfoEnabled() ? System.currentTimeMillis() : 0;
        final ExternalContext.Request request = externalContext.getRequest();
        final String requestPath = (request != null) ? request.getRequestPath() : null;

        // Set ExternalContext
        if (externalContext != null) {
            if (logger.isInfoEnabled()) {
                final String startLoggerString = externalContext.getStartLoggerString();
                if (startLoggerString != null && startLoggerString.length() > 0)
                    logger.info(startLoggerString);
            }
            pipelineContext.setAttribute(PipelineContext.EXTERNAL_CONTEXT, externalContext);
        }

        boolean success = false;
        try {
            // Set cache size
            final Integer cacheMaxSize = Properties.instance().getPropertySet().getInteger(CACHE_SIZE_PROPERTY);
            if (cacheMaxSize != null)
                ObjectCache.instance().setMaxSize(cacheMaxSize);

            // Start execution
            processor.reset(pipelineContext);
            processor.start(pipelineContext);
            success = true;
        } catch (Throwable e) {
            final Throwable throwable = Exceptions.getRootThrowable(e);
            final LocationData locationData = ValidationException.getRootLocationData(e);
            if (throwable instanceof HttpStatusCodeException) {
                // Special exception used to set an error status code
                // TODO: HttpStatusCodeException should support an optional message
                externalContext.getResponse().sendError(((HttpStatusCodeException) throwable).code);
                final String message = locationData == null
                        ? "HttpStatusCodeException with no location data"
                        : "HttpStatusCodeException at " + locationData.toString();
                logger.info(message);
            } else if (throwable instanceof ResourceNotFoundException) {
                // Special exception used when a resource is not found
                // NOTE: This could be handled as an internal server error vs. a not found error. It's a choice to say
                // that ResourceNotFoundException shows as a 404 to the outside world.
                final String resource = ((ResourceNotFoundException) throwable).resource;
                externalContext.getResponse().sendError(404);
                final String message = locationData == null
                        ? " with no location data"
                        : " at " + locationData.toString();
                logger.info("Resource not found" + ((resource != null) ? ": " + resource : "") + message);
            } else {
                throw new OXFException(e);
            }
        } finally {

            if (logger.isInfoEnabled()) {
                // Add timing
                final long timing = System.currentTimeMillis() - tsBegin;
                final StringBuilder sb = new StringBuilder(requestPath != null ? requestPath : "Done running processor");
                sb.append(" - Timing: ");
                sb.append(Long.toString(timing));

                logger.info(sb.toString());
            }

            try {
                pipelineContext.destroy(success);
            } catch (Throwable f) {
                logger.debug("Exception while destroying context after exception" + OrbeonFormatter.format(f));
            }
        }
    }

    /**
     * Create a processor and connect its inputs to static URLs.
     */
    public static Processor createProcessor(ProcessorDefinition processorDefinition) {
        final Processor processor = ProcessorFactoryRegistry.lookup(processorDefinition.getName()).createInstance();
        for (Iterator i = processorDefinition.getEntries().keySet().iterator(); i.hasNext();) {
            final String inputName = (String) i.next();
            final Object o = processorDefinition.getEntries().get(inputName);

            if (o instanceof String) {
                final String url = (String) o;
                final Processor urlGenerator = PipelineUtils.createURLGenerator(url);
                PipelineUtils.connect(urlGenerator, ProcessorImpl.OUTPUT_DATA, processor, inputName);
            } else if (o instanceof Element) {
                final Element elt = (Element) o;
                final LocationData ld = ProcessorUtils.getElementLocationData(elt);
                final String lsid = ld == null ? null : ld.getSystemID();
                final String sid = lsid == null ? DOMGenerator.DefaultContext : lsid;
                final DOMGenerator domGenerator = PipelineUtils.createDOMGenerator
                        (elt, "init input", DOMGenerator.ZeroValidity, sid);
                PipelineUtils.connect(domGenerator, ProcessorImpl.OUTPUT_DATA, processor, inputName);
            } else if (o instanceof Document) {
                final Document document = (Document) o;
                final LocationData locationData = ProcessorUtils.getElementLocationData(document.getRootElement());
                final String locationDataSystemId = locationData == null ? null : locationData.getSystemID();
                final String systemId = locationDataSystemId == null ? DOMGenerator.DefaultContext : locationDataSystemId;
                final DOMGenerator domGenerator = PipelineUtils.createDOMGenerator
                        (document, "init input", DOMGenerator.ZeroValidity, systemId);
                PipelineUtils.connect(domGenerator, ProcessorImpl.OUTPUT_DATA, processor, inputName);
            } else if (o instanceof NodeInfo) {
                final NodeInfo nodeInfo = (NodeInfo) o;

                final String nodeInfoSystemId = nodeInfo.getSystemId();
                final String systemId = nodeInfoSystemId == null ? DOMGenerator.DefaultContext : nodeInfoSystemId;
                final DOMGenerator domGenerator = PipelineUtils.createDOMGenerator
                        (nodeInfo, "init input", DOMGenerator.ZeroValidity, systemId);
                PipelineUtils.connect(domGenerator, ProcessorImpl.OUTPUT_DATA, processor, inputName);
            } else
                throw new IllegalStateException("Incorrect type in map.");
        }
        return processor;
    }

    /**
     * Run a processor based on definitions found in properties or the web app context. This is
     * useful for context/session listeners. If a definition is not found, no exception is thrown.
     *
     * @param servletContext            required ServletContext instance
     * @param session                   optional HttpSession object
     * @param localMap
     * @param logger                    required logger
     * @param logMessagePrefix          required prefix for log messages
     * @param message                   optional message to display whether there is a processor to run or not
     * @param uriNamePropertyPrefix     required prefix of the property or parameter containing the processor name
     * @param processorInputProperty    required prefix of the properties or parameters containing processor input names
     * @throws Exception
     */
    public static void run(ServletContext servletContext, HttpSession session, Map localMap,
                           Logger logger, String logMessagePrefix, String message,
                           String uriNamePropertyPrefix, String processorInputProperty) throws Exception {

        // Make sure the Web app context is initialized
        JWebAppContext.instance(servletContext);

        // Log message if provided
        if (message != null)
            logger.info(logMessagePrefix + " - " + message);

        ProcessorDefinition processorDefinition = null;
        // Try to obtain a local processor definition
        if (localMap != null) {
            processorDefinition = getDefinitionFromMap(localMap, uriNamePropertyPrefix, processorInputProperty);
        }

        // Try to obtain a processor definition from the properties
        if (processorDefinition == null)
            processorDefinition = getDefinitionFromProperties(uriNamePropertyPrefix, processorInputProperty);

        // Try to obtain a processor definition from the context
        if (processorDefinition == null)
            processorDefinition = getDefinitionFromServletContext(servletContext, uriNamePropertyPrefix, processorInputProperty);

        // Create and run processor
        if (processorDefinition != null) {
            logger.info(logMessagePrefix + " - About to run processor: " +  processorDefinition.toString());
            final Processor processor = createProcessor(processorDefinition);
            final ExternalContext externalContext = (servletContext != null) ? new WebAppExternalContext(JWebAppContext.instance(servletContext), session) : null;

            boolean success = false;
            final PipelineContext pipelineContext = new PipelineContext();
            try {
                runProcessor(processor, externalContext, pipelineContext, logger);
                success = true;
            } finally {
                pipelineContext.destroy(success);
            }
        }
        // Otherwise, just don't do anything
    }

    /**
     * Register processor definitions with the default XML Processor Registry. This defines the
     * mapping of processor names to class names
     */
    public static synchronized void initializeProcessorDefinitions() {
        if (!processorDefinitionsInitialized) {
            synchronized (InitUtils.class) {
                if (!processorDefinitionsInitialized) {
                    // Register initial processors with the default XML Processor Registry
                    Processor processorDefinitions = PipelineUtils.createURLGenerator(DEFAULT_PROLOGUE, true);
                    Processor registry = new XMLProcessorRegistry();
                    PipelineUtils.connect(processorDefinitions, "data", registry, "config");

                    {
                        final PipelineContext pipelineContext = new PipelineContext();
                        boolean success = false;
                        try {
                            processorDefinitions.reset(pipelineContext);
                            registry.reset(pipelineContext);
                            registry.start(pipelineContext);
                            success = true;
                        } finally {
                            pipelineContext.destroy(success);
                        }
                    }

                    // If user defines a PROLOGUE_PROPERTY, overrides the defaults
                    final String prologueSrc = Properties.instance().getPropertySet().getString(PROLOGUE_PROPERTY);
                    if (prologueSrc != null) {
                        processorDefinitions = PipelineUtils.createURLGenerator(prologueSrc, true);
                        registry = new XMLProcessorRegistry();
                        PipelineUtils.connect(processorDefinitions, "data", registry, "config");

                        final PipelineContext pipelineContext = new PipelineContext();
                        boolean success = false;
                        try {
                            processorDefinitions.reset(pipelineContext);
                            registry.reset(pipelineContext);
                            registry.start(pipelineContext);
                            success = true;
                        } finally {
                            pipelineContext.destroy(success);
                        }
                    }

                    processorDefinitionsInitialized = true;
                }
            }
        }
    }

    public static ProcessorDefinition getDefinitionFromServletContext(ServletContext servletContext, String uriNamePropertyPrefix, String inputPropertyPrefix) {
        return getDefinitionFromMap(new ServletContextInitMap(servletContext), uriNamePropertyPrefix, inputPropertyPrefix);
    }

    public static ProcessorDefinition getDefinitionFromProperties(String uriNamePropertyPrefix, String inputPropertyPrefix) {
        return getDefinitionFromMap(new OXFPropertiesMap(), uriNamePropertyPrefix, inputPropertyPrefix);
    }

    /**
     * Create a ProcessorDefinition from a Map. Only Map.get() and Map.keySet() are used.
     */
    public static ProcessorDefinition getDefinitionFromMap(Map<String, String> map, String uriNamePropertyPrefix, String inputPropertyPrefix) {
        ProcessorDefinition processorDefinition = null;
        final String processorURI = (String) map.get(uriNamePropertyPrefix + "uri");
        final Object processorName = map.get(uriNamePropertyPrefix + "name");

        if (processorURI != null || processorName != null) {
            processorDefinition = new ProcessorDefinition();
            // Support both xs:string or xs:QName for processor name
            processorDefinition.setName((processorName instanceof String) ? Dom4jUtils.explodedQNameToQName((String) processorName) : (QName) processorName);
            for (Iterator i = map.keySet().iterator(); i.hasNext();) {
                final String name = (String) i.next();
                if (name.startsWith(inputPropertyPrefix)) {
                    final Object value = map.get(name);
                    // Support both xs:string and xs:anyURI for processor input
                    final String stringValue;
                    if (value instanceof String) {
                        stringValue = (String) value;
                    } else if (value instanceof URI) {
                        stringValue = value.toString();
                    } else {
                        throw new OXFException("Value must be a String or URI, found instead: " + value.getClass().getName());
                    }
                    processorDefinition.addInput(name.substring(inputPropertyPrefix.length()), stringValue);
                }
            }
        }
        return processorDefinition;
    }

    /**
     * Present a read-only view of the properties as a Map.
     */
    public static class OXFPropertiesMap extends AttributesToMap<String> {
        public OXFPropertiesMap() {
            super(new Attributeable<String>() {
                public String getAttribute(String s) {
                    final Object o = Properties.instance().getPropertySet().getObject(s);
                    return (o != null) ? o.toString() : null;
                }

                public java.util.Enumeration<String> getAttributeNames() {
                    return Collections.enumeration(Properties.instance().getPropertySet().keySet());
                }

                public void removeAttribute(String s) {
                    throw new UnsupportedOperationException();
                }

                public void setAttribute(String s, String o) {
                    throw new UnsupportedOperationException();
                }
            });
        }
    }

    /**
     * Present a read-only view of the ServletContext initialization parameters as a Map.
     */
    public static class ServletContextInitMap extends AttributesToMap<String> {
        public ServletContextInitMap(final ServletContext servletContext) {
            super(new Attributeable<String>() {
                public String getAttribute(String s) {
                    return servletContext.getInitParameter(s);
                }

                public java.util.Enumeration<String> getAttributeNames() {
                    return servletContext.getInitParameterNames();
                }

                public void removeAttribute(String s) {
                    throw new UnsupportedOperationException();
                }

                public void setAttribute(String s, String o) {
                    throw new UnsupportedOperationException();
                }
            });
        }
    }

    /**
     * Present a view of the HttpSession properties as a Map.
     */
    public static class SessionMap extends AttributesToMap<Object> {
        public SessionMap(final HttpSession httpSession) {
            super(new Attributeable<Object>() {
                public Object getAttribute(String s) {
                    return httpSession.getAttribute(s);
                }

                public java.util.Enumeration<String> getAttributeNames() {
                    return httpSession.getAttributeNames();
                }

                public void removeAttribute(String s) {
                    httpSession.removeAttribute(s);
                }

                public void setAttribute(String s, Object o) {
                    httpSession.setAttribute(s, o);
                }
            });
        }
    }

    /**
     * Present a view of the HttpServletRequest properties as a Map.
     */
    public static class RequestMap extends AttributesToMap<Object> {
        public RequestMap(final HttpServletRequest httpServletRequest) {
            super(new Attributeable<Object>() {
                public Object getAttribute(String s) {
                    return httpServletRequest.getAttribute(s);
                }

                public Enumeration<String> getAttributeNames() {
                    return httpServletRequest.getAttributeNames();
                }

                public void removeAttribute(String s) {
                    httpServletRequest.removeAttribute(s);
                }

                public void setAttribute(String s, Object o) {
                    httpServletRequest.setAttribute(s, o);
                }
            });
        }
    }

    private static class HttpStatusCodeException extends RuntimeException {
        public final int code;

        public HttpStatusCodeException(int code) {
            this.code = code;
        }
    }

    /**
     * Interrupt current processing and send an error code to the client.
     *
     * This assumes that the response has not yet been committed.
     */
    public static void sendError(int code) {
        throw new HttpStatusCodeException(code);
    }
}
