/**
 *  Copyright (C) 2011 Orbeon, Inc.
 *
 *  This program is free software; you can redistribute it and/or modify it under the terms of the
 *  GNU Lesser General Public License as published by the Free Software Foundation; either version
 *  2.1 of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *  See the GNU Lesser General Public License for more details.
 *
 *  The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
 */
package org.orbeon.oxf.portlet

import java.net.URLEncoder
import javax.portlet._

import com.liferay.portal.kernel.util.{PropsUtil, WebKeys}
import com.liferay.portal.kernel.workflow._
import com.liferay.portal.model.Layout
import com.liferay.portal.service.ServiceContext
import com.liferay.portal.theme.ThemeDisplay
import com.liferay.portal.util.PortalUtil
import com.worthit.tsb.model.{Application, Competition, WorkflowFormDefinition, WorkflowFormInstance, FormConfiguration}
import com.worthit.tsb.service._
import com.worthit.tsb.service.simulfy.Support;
import org.orbeon.errorified.Exceptions
import org.orbeon.exception.OrbeonFormatter
import org.orbeon.oxf.externalcontext.WSRPURLRewriter
import org.orbeon.oxf.fr.embedding._
import org.orbeon.oxf.http.{ApacheHttpClient, HttpClient, HttpClientSettings, StreamedContent}
import org.orbeon.oxf.portlet.liferay.LiferaySupport
import org.orbeon.oxf.util.ScalaUtils.{withRootException ⇒ _, _}

import scala.collection.breakOut
import scala.util.control.NonFatal

/**
 * Orbeon Forms Form Runner proxy portlet.
 *
 * This portlet allows access to a remote Form Runner instance.
 */
class OrbeonProxyPortlet extends GenericPortlet with ProxyPortletEdit with BufferedPortlet {

    import org.orbeon.oxf.portlet.OrbeonProxyPortlet._

    private case class PortletSettings(
        forwardHeaders: Map[String, String], // lowercase name → original name
        forwardParams : Set[String],
        httpClient    : HttpClient
     )

    // For BufferedPortlet
    def title(request: RenderRequest) = getTitle(request)

    private var settingsOpt: Option[PortletSettings] = None

    override def init(config: PortletConfig) {
        APISupport.Logger.info("initializing Form Runner proxy portlet with WORTH modifications")
        super.init(config)
        settingsOpt = Some(
            PortletSettings(
                forwardHeaders = stringToSet(config.getInitParameter("forward-headers")).map(name ⇒ name.toLowerCase → name)(breakOut),
                forwardParams  = stringToSet(config.getInitParameter("forward-parameters")),
                httpClient     = new ApacheHttpClient(HttpClientSettings(config.getInitParameter))
            )
        )
    }

    override def destroy() = {
        APISupport.Logger.info("destroying Form Runner proxy portlet")
        settingsOpt foreach (_.httpClient.shutdown())
        settingsOpt = None
        super.destroy()
    }

    // Try to find getHttpServletRequest only the first time this is accessed
    private lazy val getHttpServletRequest =
        try Some(PortalUtil.getHttpServletRequest _)
        catch { case (_: NoClassDefFoundError) | (_: ClassNotFoundException) ⇒ None }

    private def findServletRequest(request: PortletRequest) =
        getHttpServletRequest flatMap (f ⇒ Option(f(request)))

    private val FormRunnerHome         = """/fr/(\?(.*))?""".r
    private val FormRunnerPath         = """/fr/([^/]+)/([^/]+)/(new|summary)(\?(.*))?""".r
    private val FormRunnerDocumentPath = """/fr/([^/]+)/([^/]+)/(new|edit|view)/([^/?]+)?(\?(.*))?""".r

    // Portlet render
    override def doView(request: RenderRequest, response: RenderResponse): Unit =
        settingsOpt foreach { settings ⇒
            withRootException("view render", new PortletException(_)) {

                implicit val ctx = new PortletEmbeddingContextWithResponse(
                    getPortletContext,
                    request,
                    response,
                    settings.httpClient
                )

                bufferedRender(
                    request,
                    response,
                    APISupport.callService(createRequestDetails(settings, request, response.getNamespace))
                )
            }
        }

    // Portlet action
    override def processAction(request: ActionRequest, response: ActionResponse): Unit =
        settingsOpt foreach { settings ⇒
            request.getPortletMode match {
                case PortletMode.VIEW ⇒ doViewAction(request, response)
                case PortletMode.EDIT ⇒ doEditAction(request, response)
                case _ ⇒ // NOP
            }
        }

    private def doViewAction(request: ActionRequest, response: ActionResponse): Unit =
        settingsOpt foreach { settings ⇒
            withRootException("view action", new PortletException(_)) {
                implicit val ctx = new PortletEmbeddingContext(
                    getPortletContext,
                    request,
                    response,
                    settings.httpClient
                )
                bufferedProcessAction(
                    request,
                    response,
                    APISupport.callService(createRequestDetails(settings, request, response.getNamespace))
                )
            }
        }

    // Portlet resource
    override def serveResource(request: ResourceRequest, response: ResourceResponse): Unit =
        settingsOpt foreach { settings ⇒
            withRootException("resource", new PortletException(_)) {
                implicit val ctx = new PortletEmbeddingContextWithResponse(
                    getPortletContext,
                    request,
                    response,
                    settings.httpClient
                )
                val resourceId = request.getResourceID
                val url = APISupport.formRunnerURL(baseUrl, resourceId, embeddable = false)

                val requestDetails =
                    newRequestDetails(
                        settings,
                        request,
                        contentFromRequest(request, response.getNamespace),
                        url
                    )

                APISupport.proxyResource(requestDetails)
            }
        }

    private def preferenceFromPortalQuery(request: PortletRequest, pref: Pref) =
        if (getBooleanPreference(request, EnableURLParameters))
            None
        else
            portalQuery(request) collectFirst { case (pref.nameLabel.publicName, value) ⇒ value}

    private def getPreferenceOrRequested(request: PortletRequest, pref: Pref) =
        preferenceFromPortalQuery(request, pref) getOrElse getPreference(request, pref)

    private def createRequestDetails(settings: PortletSettings, request: PortletRequest, namespace: String): RequestDetails = {
        // Determine URL based on preferences and request
        val path = {
            initWorthRequest(request)

            def pathParameterOpt =
                Option(request.getParameter(WSRPURLRewriter.PathParameterName))

            def defaultPath =
                if (getPreference(request, Page) == "home")
                    APISupport.formRunnerHomePath(None)
                else
                    getWorthFormRunnerPath(request)

            def filterMode(mode: String) = getWorthViewMode(request)

            pathParameterOpt getOrElse defaultPath match {
                case path @ "/xforms-server-submit" ⇒
                    path
                case path @ "/dont-show-list-with-forms" ⇒
                    // this will show a "Orbeon Forms - Page Not Found" message.
                    // (that is what we want when there is not application.)
                    path
                // Incoming path is Form Runner path without document id
                case FormRunnerPath(appName, formName, mode, _, query) ⇒
                    APISupport.formRunnerPath(appName, formName, filterMode(mode), None, Option(query))
                // Incoming path is Form Runner path with document id
                case FormRunnerDocumentPath(appName, formName, mode, documentId, _, query) ⇒
                    APISupport.formRunnerPath(appName, formName, mode, Some(documentId), Option(query))
                // Incoming path is Form Runner Home page
                case FormRunnerHome(_, query) ⇒
                    APISupport.formRunnerHomePath(Option(query))
                // Unsupported path
                case otherPath ⇒
                    throw new PortletException("Unsupported path: " + otherPath)
            }
        }

        newRequestDetails(
            settings,
            request,
            contentFromRequest(request, namespace),
            APISupport.formRunnerURL(baseUrl, path, embeddable = true)
        )
    }

    private def portalQuery(request: PortletRequest) =
        collectByErasedType[String](request.getAttribute("javax.servlet.forward.query_string")) map decodeSimpleQuery getOrElse Nil

    private def newRequestDetails(
        settings: PortletSettings,
        request : PortletRequest,
        content : Option[StreamedContent],
        url     : String
    ): RequestDetails = {

        def clientHeaders =
            findServletRequest(request).toList flatMap APISupport.requestHeaders

        def paramsToSet =
            for {
                pair @ (name, _) ← portalQuery(request)
                if settings.forwardParams(name)
            } yield
                pair

        val sendLanguage = getBooleanPreference(request, SendLiferayLanguage)
        val sendUser     = getBooleanPreference(request, SendLiferayUser)

        // Language information
        // NOTE: Format returned is e.g. "en_US" or "fr_FR".
        def languageHeader =
            if (sendLanguage)
                LiferaySupport.languageHeader(request)
            else
                None

        // User information
        def userHeaders =
            if (sendUser)
                for {
                    request   ← findServletRequest(request).toList
                    user      ← Option(PortalUtil.getUser(request)).toList
                    nameValue ← LiferaySupport.userHeaders(user)
                } yield
                    nameValue
            else
                Nil

        def headersToSet =
            APISupport.headersToForward(clientHeaders, settings.forwardHeaders).toList ++ languageHeader.toList ++ userHeaders

        RequestDetails(
            content,
            url,
            headersToSet,
            paramsToSet
        )
    }

    private def contentFromRequest(request: PortletRequest, namespace: String): Option[StreamedContent] =
        request match {
            case clientDataRequest: ClientDataRequest if clientDataRequest.getMethod == "POST" ⇒
                Some(
                    StreamedContent(
                        clientDataRequest.getPortletInputStream,
                        Option(clientDataRequest.getContentType),
                        Some(clientDataRequest.getContentLength.toLong) filter (_ >= 0),
                        None
                    )
                )
            case _ ⇒
                None
        }


    private def getWorthViewMode(request: PortletRequest) : String = {
        if(worthRequest == null){
            return View.name
        }
        if (!worthRequest.wfTask.isDefined) {
            APISupport.Logger.debug("WF Task is null, so view mode")
            return View.name
        }

                // show edit view when (user is owner or collaborator) AND (simulfyIsDisabled or thisBrowserIsSupported)
        // OR it's a subform task on the main workflow owned by the current user
        val simulfyEnabled = Support.isSimulfyEnabledForThisRequest(worthRequest.request)
        val simulfyBrowserSupported = Support.isBrowserSupportedForSimulfy(request)
        APISupport.Logger.debug("simulfyEnabled  " + simulfyEnabled.toString())
        APISupport.Logger.debug("simulfyBrowserSupported  " + simulfyBrowserSupported.toString())

        if ((worthRequest.userId.equals(worthRequest.assigneeId) && !simulfyEnabled) || (simulfyEnabled && simulfyBrowserSupported)) {
            // TODO: move this logic to the service layer and reuse in SubmitApplication

            if (!worthRequest.wfFormDefinition.isDefined) {
                APISupport.Logger.debug("No workflow form definition")
            } else {
                APISupport.Logger.debug("Workflow form definition " + worthRequest.wfFormDefinition.get.getName());
            }
            if (!worthRequest.wfFormInstance.isDefined) {
                APISupport.Logger.debug("No workflow form definition")
            } else {
                APISupport.Logger.debug("Workflow form instance " + worthRequest.wfFormInstance.get.getName());
            }
            // show main form in view mode when when it wasn't specifically requested
            if ((worthRequest.isCurrentlyOverriddenBySubflow && worthRequest.wfFormInstance == null) ||
                    (worthRequest.wfFormInstance.isDefined && !worthRequest.wfFormDefinition.isDefined && worthRequest.wfFormInstance.get.isOverrideMainFlow()) ||
                    (worthRequest.wfFormInstance.isDefined && worthRequest.wfFormDefinition.isDefined && !worthRequest.wfFormInstance.get.getName().equals(worthRequest.wfFormDefinition.get.getName()))
            ) {
                APISupport.Logger.debug("View mode reason 1")
                View.name
            } else {
                Edit.name
            }
        } else {
            APISupport.Logger.debug("View mode reason default")
            View.name
        }

    }

    private def getWorthApplicationParams(request: PortletRequest, docId: String): String = {
        // Fields to return :
        // roleId, workflowTaskId, serverHostname, applicationTitle, userId, groupId,
        // workflowId, workflowTaskId, workflowFormInstanceId, formConfigurationId, token, returnUrl

        var scopeGroupId = PortalUtil.getScopeGroupId(request).toString
        APISupport.Logger.debug("scopeGroupId " + scopeGroupId)
        var userId : Option[String] = Option(PortalUtil.getUserId(request).toString)
        var groupId : Option[String] = Option(scopeGroupId)


        var applicationParams = "orbeon-embeddable=true"+
                "&applicationTitle=" + worthRequest.applicationTitle +
                (userId map ("&userid=" + _) getOrElse "") +
                (groupId map ("&groupid=" + _) getOrElse "") +
                ("&document=" + docId) +
                (worthRequest.wfId map ("&workflowid=" + _) getOrElse "") +
                (worthRequest.wfTask map ("&workflowtaskid=" + _.getWorkflowTaskId.toString) getOrElse "") +
                (worthRequest.wfFormInstance map ("&workflowforminstanceid=" + _.getWorkflowFormInstanceId.toString) getOrElse "") +
                ("&formconfigurationid=" + worthRequest.formConfigurationId.toString) +
                (worthRequest.token map ("&token=" + _) getOrElse "") +
                (worthRequest.returnUrl map ("&returnurl=" + _) getOrElse "") +
                (worthRequest.serverHostname map ("&serverhostname=" + _) getOrElse "") +
                ("&roleid=" + worthRequest.roleIds)

        APISupport.Logger.debug(" got applicationParams   " + applicationParams)
        applicationParams
    }

    private def getWorthApplicationDocumentId(request: PortletRequest) : String = {
        var scopeGroupId = PortalUtil.getScopeGroupId(request)
        var application = ApplicationLocalServiceUtil.getApplicationByGroupId(scopeGroupId)

        if(application == null){
            ""
        }else{
            APISupport.Logger.debug("application id  " + application.getApplicationId.toString)
            //Option(application.getApplicationId.toString)
            var docId = application.getApplicationId.toString

            if (worthRequest.wfFormInstance.isDefined) {
                // for subforms.
                docId = WorkflowFormInstanceLocalServiceUtil.getOrbeonDocId(worthRequest.wfFormInstance.get).toString
            }

            docId
        }
    }
    private def getWorthFormRunnerPath(request: PortletRequest): String = {
        if(request == null){
            APISupport.Logger.debug("No request found, so return formRunnerHomePath ")
            return "/dont-show-list-with-forms"
        }
        if(worthRequest == null){
            APISupport.Logger.debug("worthRequest is null, so return formRunnerHomePath ")
            return "/dont-show-list-with-forms"
        }

        APISupport.Logger.debug("DocumentId " + getWorthApplicationDocumentId(request))
        APISupport.Logger.debug("DocumentId " + getPreferenceOrRequested(request, DocumentId))
        APISupport.Logger.debug("page  " + getPreferenceOrRequested(request, Page).toString)

        // BEFORE: Option(getPreferenceOrRequested(request, DocumentId))
        var appName = getPreferenceOrRequested(request, AppName)
        var formName = getPreferenceOrRequested(request, FormName)

        // if the proxy portlet has no appName and formName, then use the competition appName/formName
        // or from the form configuration
        if (appName == "" && worthRequest.competition.isDefined) {
            appName = worthRequest.competition.get.getOrbeonAppName
        }

        if(formName == "" && worthRequest.competition.isDefined){
            formName = worthRequest.competition.get.getOrbeonFormName
        }

        APISupport.Logger.debug("phase enabled: " + worthRequest.competition.get.isPhasesEnabled);
        APISupport.Logger.debug("form config id: " + worthRequest.formConfigurationId);

        if(worthRequest.wfFormInstance.isDefined){
            // for subforms.
            appName = worthRequest.wfFormInstance.get.getOrbeonAppName()
            formName = worthRequest.wfFormInstance.get.getOrbeonFormName()
        }
        else if(worthRequest.competition.isDefined && worthRequest.competition.get.isPhasesEnabled && worthRequest.formConfigurationId!=0L) {
            var formConfiguration = FormConfigurationLocalServiceUtil.getFormConfiguration(worthRequest.formConfigurationId);
            APISupport.Logger.debug("phase is enabled id: " + formConfiguration.getOrbeonAppName + " " + formConfiguration.getOrbeonFormName)
            appName = formConfiguration.getOrbeonAppName
            formName = formConfiguration.getOrbeonFormName
        }

        val docId = getWorthApplicationDocumentId(request)
        val url = APISupport.formRunnerPath(
            appName,
            formName,
            getWorthViewMode(request),
            Option(docId),
            Option(getWorthApplicationParams(request, docId))
        )
        APISupport.Logger.debug("url  " + url)
        url
    }
    private case class WorthRequest(
                                           wfId : Option[String] = None,
                                           wfInstance: WorkflowInstance, // lowercase name → original name
                                           wfTask : Option[WorkflowTask],
                                           application: Application,
                                           request: PortletRequest,
                                           userId : String,
                                           assigneeId : String,
                                           competition: Option[Competition],
                                           wfFormDefinition : Option[WorkflowFormDefinition],
                                           wfFormInstance : Option[WorkflowFormInstance],
                                           formConfigurationId : Long,
                                           isCurrentlyOverriddenBySubflow : Boolean,
                                           token : Option[String],
                                           returnUrl : Option[String],
                                           roleIds : String,
                                           serverHostname: Option[String],
                                           applicationTitle: String

                                           )
    private var worthRequest : WorthRequest = null
    def initWorthRequest(request: PortletRequest): Any = {
        if(request == null){
            return;
        }
        //        var worthRequest :WorthRequest
        var className = classOf[Application].getName
        var primKey: Long = 0
        var assigneeId = "0"
        var roleIds : String = "applicant"
        var competition : Option[Competition] = None
        var wfFormInstance : Option[WorkflowFormInstance] = None
        var wfFormDefinition: Option[WorkflowFormDefinition] = None
        var formConfigurationId : Long = 0
        var userId = PortalUtil.getUserId(request).toString
        var owningUserId : Long = 0L
        var isCurrentlyOverriddenBySubflow: Boolean = false
        var applicationTitle: String = null
        var token:Option[String] = None
        var returnUrl: Option[String] = None

        var scopeGroupId = PortalUtil.getScopeGroupId(request)
        var application = ApplicationLocalServiceUtil.getApplicationByGroupId(scopeGroupId)

        if (application == null){
            // might be requesting the site template..
            return;
        }
        competition = Option(CompetitionLocalServiceUtil.getCompetition(application.getCompetitionId))
        owningUserId = if (application.getOwnerUserId > 0) application.getOwnerUserId else application.getUserId
        primKey = application.getApplicationId
        isCurrentlyOverriddenBySubflow = ApplicationLocalServiceUtil.isCurrentlyOverriddenBySubflow(primKey, application.getStatus());
        try {
            applicationTitle = URLEncoder.encode(application.getTitle, "UTF-8")
        } catch {
            case e : Throwable ⇒ APISupport.Logger.error(e.getMessage())
        }

        // check if maybe we need to display a subform
        var themeDisplay:ThemeDisplay = request.getAttribute(WebKeys.THEME_DISPLAY).asInstanceOf[ThemeDisplay]
        var layout:Layout = themeDisplay.getLayout
        if (layout != null && application != null) {
            val layoutName:String = layout.getName("GB")
            if (layoutName != null && layoutName.trim().length() > 0) {
                wfFormInstance = Option(WorkflowFormInstanceLocalServiceUtil.getByNameAndApplication(layoutName, application))
            }
        }


        // yes, we found out that we are on a subform page
        if (wfFormInstance.isDefined && !wfFormInstance.get.isOverrideMainFlow()) {
            owningUserId = wfFormInstance.get.getUserId
            className = classOf[WorkflowFormInstance].getName
            primKey = wfFormInstance.get.getWorkflowFormInstanceId
        }

        var wfInstance = CurrentWorkflowTaskLocalServiceUtil.lookupCurrentWorkflowInstance(PortalUtil.getCompanyId(request), application.getUserId, primKey, className)
        var wfTask  : WorkflowTask = null;
        if(wfInstance != null){
            wfTask = CurrentWorkflowTaskLocalServiceUtil.lookupCurrentTask(PortalUtil.getCompanyId(request), wfInstance)
            if(wfTask != null){
                // no task found, maybe workflow is finished?
                assigneeId = wfTask.getAssigneeUserId().toString
            }
        }

        APISupport.Logger.debug("roleId = applicant" + userId +" - "+ owningUserId.toString())
        if(!userId.equals(owningUserId.toString())){
            APISupport.Logger.debug("user is not the application owner, so get roleid external " + RoleMapperLocalServiceUtil.getRoleIds(PortalUtil.getUser(request)))
            roleIds = RoleMapperLocalServiceUtil.getRoleIds(PortalUtil.getUser(request))
        }else{
            APISupport.Logger.debug("user is  the application owner, otherwise would have gotten these roles: " + RoleMapperLocalServiceUtil.getRoleIds(PortalUtil.getUser(request)))
        }

        var wfId = Option(WorkflowStatusMapperLocalServiceUtil.fromId(application.getStatus()).toString)
        val wfFormInstanceId = if (!wfFormInstance.isDefined) 0 else wfFormInstance.get.getWorkflowFormInstanceId

        if(wfTask != null){
            token = Option(NavigationSupportLocalServiceUtil.getTokenizedWorkflowIdentifier(
                application.getApplicationId, wfTask.getWorkflowTaskId, wfFormInstanceId))
        }

        if(application != null && competition.isDefined){
            if(competition.get.isPhasesEnabled) {
                val workflowInstance = WorkflowInstanceManagerUtil.getWorkflowInstance(application.getCompanyId, wfTask.getWorkflowInstanceId)
                val serviceContext = workflowInstance.getWorkflowContext.get(WorkflowConstants.CONTEXT_SERVICE_CONTEXT).asInstanceOf[ServiceContext]
                formConfigurationId = serviceContext.getAttribute("formConfigurationId").toString.toLong
                wfFormDefinition = Option(WorkflowFormDefinitionLocalServiceUtil.getFormDefinitionOverridingMainFlowByFormConfig(formConfigurationId, application.getStatus()))
            } else {
                wfFormDefinition = Option(WorkflowFormDefinitionLocalServiceUtil.getFormDefinitionOverridingMainFlow(competition.get.getCompetitionId(), application.getStatus()))
            }
        }

        var serverHostname: Option[String] = Some(request.getServerName() + ":" + request.getServerPort());
        if (request.isSecure()) {
            serverHostname = Some("https://" + serverHostname.get);
        } else {
            serverHostname = Some("http://" + serverHostname.get);
        }

        APISupport.Logger.debug("assigneeId " + assigneeId);
        APISupport.Logger.debug("userId " + userId);

        worthRequest = WorthRequest(wfId, wfInstance, Option(wfTask), application, request, userId, assigneeId, competition, wfFormDefinition, wfFormInstance, formConfigurationId, isCurrentlyOverriddenBySubflow, token, returnUrl, roleIds, serverHostname, applicationTitle)
    }
    private def baseUrl() =
        PropsUtil.get("orbeon-pe.form.runner.url")
}

object OrbeonProxyPortlet {
    def withRootException[T](action: String, newException: Throwable ⇒ Exception)(body: ⇒ T)(implicit ctx: PortletContext): T =
        try body
        catch {
            case NonFatal(t) ⇒
                ctx.log("Exception when running " + action + '\n' + OrbeonFormatter.format(t))
                throw newException(Exceptions.getRootThrowable(t))
        }
}