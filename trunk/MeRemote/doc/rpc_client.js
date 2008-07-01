/*
 * JSON/XML-RpcClient JavaScript Client for WebBrowser
 * Version: 1.0.0.0 (2008-6-28)
 * Copyright: 2007, Weston Ruter <http://weston.ruter.net/>
 * Copyright: 2008, Riceball LEE
 * The Original Code is $RCSfile: RpcClient.js ,v $.
 * License: GNU General Public License, Free Software Foundation<http://creativecommons.org/licenses/GPL/2.0/>
 *          or LGPL3 GNU LESSER GENERAL PUBLIC LICENSE<http://www.opensource.org/licenses/lgpl-3.0.html>
 *          or MPL 1.1 Mozilla Public License<http://www.mozilla.org/MPL/MPL-1.1.html>
 *
 * Original inspiration for the design of this implementation is from jsolait, from which 
 * are taken the "Service" name and the interface for synchronous method calls.
 * 
 * See the following specifications:
 *   - XML-RpcClient: <http://www.xmlrpc.com/spec>
 *   - JSON-RpcClient 1.0: <http://json-RpcClient.org/wiki/specification>
 *   - JSON-RpcClient 1.1 (draft): <http://json-RpcClient.org/wd/JSON-RpcClient-1-1-WD-20060807.html>
 *
 * Usage:
 * var rpc = new RpcClient.Service("/app/service", {
 *                         IsAsync: true,   //default: true
 *                         Sanitize: true,       //default: true
 *                         Methods: ['greet'],   //default: null (synchronous introspection populates)
 *                         Protocol: cJSONRpc, //default: cJSONRpc
 * }); 
 * rpc.greet({
 *    params:{name:"World"},
 *    onSuccess:function(message){
 *        alert(message);
 *    },
 *    onException:function(e){
 *        alert("Unable to greet because: " + e);
 *        return true;
 *    }
 * });
 *
 * If you create the service proxy with asynchronous set to false you may execute
 * the previous as follows:
 *
 * try {
 *    var message = rpc.greet("World");
 *    alert(message);
 * }
 * catch(e){
 *    alert("Unable to greet because: " + e);
 * }
 *
 * Finally, if the URL provided is on a site that violates the same origin policy,
 * then you may only create an asynchronous proxy, the resultant data may not be
 * sanitized, and you must provide the methods yourself. In order to obtain the
 * method response, the JSON-RpcClient server must be provided the name of a callback
 * function which will be generated in the JavaScript (json-in-script) response. The HTTP GET
 * parameter for passing the callback function is currently non-standardized and so
 * varies from server to server. Create a service proxy with the option
 * 'CallbackParamName' in order to specify the callback function name parameter;
 * the default is 'JSON-response-callback', as used by associated JSON/XML-RpcClient
 * Server project. For example, getting Google Calendar data:
 *
 * var gcalService = new RpcClient.Service("http://www.google.com/calendar/feeds/myemail%40gmail.com/public", {
 *                         IsAsync: true,  //true (default) required, otherwise error raised
 *                         Sanitize: false,     //explicit false required, otherwise error raised
 *                         Methods: ['full']    //explicit list required, otherwise error raised
 *                         CallbackParamName: 'callback'
 *                         }); 
 * gcalService.full({
 *      params:{
 *          alt:'json-in-script' //required for this to work
 *          'start-min':new Date() //automatically converted to ISO8601
 *          //other Google Calendar parameters
 *      },
 *      onSuccess:function(json){
 *          json.feed.entry.each(function(entry){
 *              //do something
 *          });
 *      }
 * });

Web 浏览器客户端 JavaScript [[JSON-RPC]] 客户端实现草案

= 说明 =

基于[http://www.coolcode.cn/show-185-1.html PHPRPC 与 JSON-RPC、XML-RPC 的比较]一文对RPC 协议的比较。选定以JSON-RPC标准为基础进行扩展。不选择 PHPRPC 是因为不赞同将其数据格式牢牢的绑定在PHP的序列格式之上，这样降低了跨语言的能力。

WebBrowser的跨域问题：按照PHPRPC一文，可以通过web浏览器客户端JavaScript程序中动态追加js的方式方式加以解决。而其它弊端也可以通过JSON-RPC标准本身加以改造解决。


= JavaScript RPC Client =
本rpc.js实现了对json-rpc的异步以及同步调用。并且通过异步调用的方式实现对Web浏览器跨域调用的支持。JavaScript RPC Client实现了 2006-8-7公布的JSON-RPC 1.1草案内容，同时支持老的JSON-RPC 1.0 版本。

JavaScript RPC Client通过实现客户端的Service调用类来完成对JSON-RPC的调用。
<code language="javascript">
var service = new RpcClient.Service("/app/service", {--options--});
</code>

Service类一旦实例化后，该服务上的所有API都可以直接调用。假如上述service上有如下的API: wadd(a,b)和echo(s)，wadd将a,b两数相加并返回结果,echo将输入的参数s原样返回。你可以直接使用“service.wadd(23,12)”和“echo('hello')”来调用服务上的API.

初始化Service的配置参数如下：

{|cellspacing="0" border="1"
!参数  || 默认值 || 说明
|-
|IsAsync: Boolean || true ||Synchronous calls are not possible to cross-site servers. This option may be changed later by calling: rpc.setAsynchronous(service, bIsAsync) or service.setAsynchronous(bIsAsync)
|-
|Sanitize:Boolean || true  || Forces the response data to be validated as legal JSON. This option is relevant to JSON-RPC only, and must be set to 'false' when making calls to a cross-site server.
|-
|User:String || null || Username for HTTP authentication.
|-
|Passwd: String || null || Password for HTTP authentication.
|-
|CallbackParamName:String || "JSON-response-callback" || This option is only relevant to cross-site JSON-RPC calls. The value is the name of the HTTP GET parameter which the server will use to generate the json-in-script response.
|-
|Protocol: String || cJSONRpc || May also be cXMLRpc.
|-
|DateEncoding: String || "ISO8601"|| Dates are by default encoded as ISO 8601 strings. If the server expects dates to be encoded differently, you may also supply the values "classHinting", "@timestamp@", and "ASP.NET".
|-
|DecodeISO8601:Boolean || true || Determines whether ISO 8601 date strings are automatically constructed into Date objects. UTC is assumed.
|-
|Methods: Array || none || When this option is not supplied, the service performs introspection via system.listMethods. If the service resides on a cross-site server, this option containing the method list must be provided since synchronous calls cannot be made in this case.
|}

= 教程 =

== 如何使用 rpc-client.js 调用json-RPC? ==

通过使用提供的 Service 类来实现在js中对api的调用，用如下的方法初始化

同步调用的方法如下:
<code language="javascript">
 var vDemoService = new RpcClient.Service(
    // API URL
    "/demo.ashx", 
   
   {//参数列表，如果没有就启用默认值
    //是否异步调用,默认为是。 
    asynchronous: false, 
    //API名称列表，如果该列表没有提供,将自动去调用服务端的
    //system.listMethods去获取API名称列表。
    //注意：当跨域调用时，不会获取API名称列表，必须手动设置API名称表。
    methods: ['counter','wadd'], 
    // 是否强制返回数据为有效的json格式，默认为真（注意跨域必须改成false!）
    sanitize: true  
  }); 
</code>

设置是否异步模式也可以在运行时候设置：
<code language="javascript">
    vDemoService.setAsynchronous(false);
</code>

同步调用最为简单和我们直接使用函数没有太大差别： 
<code language="javascript">
  try {
    var sum = vDemoService.wadd(1,1);
    alert("The sum is: " + sum);}
  catch(e){
    alert("Unable to add numbers because: " + e);
  }
</code>

异步调用稍微复杂些，除了设置参数外，你还需要设好回调函数：  
<code language="javascript">
  function myResultCallback(aResult){
     alert("The sum is: " + aResult);
  }
  function myExpcetionHandler(aError){
     alert("The Error is: " + aError);
  }
  vDemoService.setAsynchronous(true);
  vDemoService.wadd([10,2], myResultCallback, myExpcetionHandler);
</code>


[[Category:RPC-Client]]
[[Category:JSON-RPC]]

 */

var RpcClient = {
	//private members
	__XMLHttpNameCache: null,

  //public members
	Version:"1.0.0.0",	
	RequestCount: 0
};

var cErrorCanNotSanitized = 1;
var cErrorXMLCanNotCrossSite = 2;
var cErrorNeedAsyncForCrossSite = 3;
var cErrorAPINamesRequired  = 4;
var cErrorSuccessHandler    = 5;
var cErrorExceptionHandler  = 6;
var cErrorCompleteHandler   = 7;
var cErrorAsyncParamType    = 8;
var cErrorXMLParamsType     = 9;
var cErrorResponseObject    = 10;
var cErrorResponseIdMissed  = 11;
var cErrorResponseIdUnkonwn = 12;
var cErrorQueryStringParamType = 13;
var cErrorQueryStringParamNestedType = 14;
var cErrorQueryStringParamObjectType = 15;
var cErrorXMLDoubleTypeInvalid = 16;
var cErrorXMLBoolTypeInvalid   = 17;
var cErrorXMLISODateTypeInvalid = 18;
var cErrorBase64FuncNotInstalled = 19;
var cErrorXMLArrayDataMissed = 20;
var cErrorXMLArrayValueMissed = 21;
var cErrorXMLArrayIllegalElement = 22;
var cErrorXMLDocRequired = 23;
var cErrorXMLRpcDocInvalid = 24;
var cErrorXMLMethodResponse = 25;

var cJSONRpc = "JSON-RPC";
var cXMLRpc  = "XML-RPC";

/*
 * the function createXMLHttp() modified from
 * http://webfx.eae.net/dhtml/xmlextras/xmlextras.html and
 * http://www.ugia.cn/?p=85
 */
RpcClient.createXMLHttp = function () {
            if (window.XMLHttpRequest) {
                var objXMLHttp = new XMLHttpRequest();
                // some older versions of Moz did not support the readyState property
                // and the onreadystate event so we patch it!
                if (objXMLHttp.readyState == null) {
                    objXMLHttp.readyState = 0;
                    objXMLHttp.addEventListener(
                        "load",
                        function () {
                            objXMLHttp.readyState = 4;
                            if (typeof(objXMLHttp.onreadystatechange) == "function") {
                                objXMLHttp.onreadystatechange();
                            }
                        },
                        false
                    );
                }
                return objXMLHttp;
            }
            else if (RpcClient.__XMLHttpNameCache != null) {
                // Use the cache name first.
                 return new ActiveXObject(RpcClient.__XMLHttpNameCache);
            }
            else {
                var MSXML = ['MSXML2.XMLHTTP.6.0',
                             'MSXML2.XMLHTTP.5.0',
                             'MSXML2.XMLHTTP.4.0',
                             'MSXML2.XMLHTTP.3.0',
                             'MsXML2.XMLHTTP.2.6',
                             'MSXML2.XMLHTTP',
                             'Microsoft.XMLHTTP.1.0',
                             'Microsoft.XMLHTTP.1',
                             'Microsoft.XMLHTTP'];
                var n = MSXML.length;
                for(var i = 0; i < n; i++) {
                    try {
                        objXMLHttp = new ActiveXObject(MSXML[i]);
                        // Cache the XMLHttp ActiveX object name.
                        RpcClient.__XMLHttpNameCache = MSXML[i];
                        return objXMLHttp;
                    }
                    catch(e) {}
                }
                return null;
            }
        }

RpcClient.Service = function(serviceURL, options){
	//if(typeof Prototype == 'undefined')
	//	throw Error("The RpcClient client currently requires the use of Prototype.");
	this.FServiceURL = serviceURL;
	
	//Determine if accessing the server would violate the same origin policy
	this.FIsCrossSite = false;
	var vUrlParts = this.FServiceURL.match(/^(\w+:)\/\/([^\/]+?)(?::(\d+))?(?:$|\/)/);
	if(vUrlParts){
		this.FIsCrossSite = (
			location.protocol !=  vUrlParts[1] ||
			document.domain   !=  vUrlParts[2] ||
			location.port     != (vUrlParts[3] || "")
		);
	}

/* //to reduce the size of the file.
  var rsErrorCanNotSanitized = "You are attempting to access a service on another site, and the JSON data returned " +
						"by cross-site requests cannot be sanitized. You must therefore explicitly set the " +
						"'Sanitize' option to false (it is true by default) in order to proceed with making " +
						"potentially insecure cross-site RpcClient calls.";
  var rsErrorXMLCanNotCrossSite = "Unable to use the XML-RpcClient protocol to access services on other domains.";
  var rsErrorNeedAsyncForCrossSite = "It is not possible to establish a synchronous connection to a cross-site RpcClient service.";
  var rsErrorAPINamesRequired = "You must manually supply the service's method names since auto-introspection is not permitted for cross-site services.";
  var rsErrorAsyncHandler = "The asynchronous handler callback function you provided is invalid";
  var rsErrorAsyncParamType = "When making asynchronous calls, the parameters for the method must be passed as an array (or a hash)";
  var rsErrorXMLParamsType =  "Unable to pass associative arrays to XML-RpcClient services.";
  var rsErrorResponseObject = "The server did not respond with a response object.";
  var rsErrorResponseIdMissed = "The server did not respond with the required response id for asynchronous calls.";
  var rsErrorResponseIdUnkonwn = "Fatal error with RpcClient code: no such ID found in pendingRequests.";
  var rsErrorQueryStringParamType = "You must supply either an array ,date or object type to convert into a query string. ";
  var rsErrorQueryStringParamNestedType = "Unable to pass nested arrays nor objects as parameters while in making a cross-site request.";
  var rsErrorBase64FuncNotInstalled = "Not able to parse base64 data yet.";
  var rsErrorXMLArrayDataMissed = "XML-Rpc Parse Error: Expected 'data' element as sole child element of 'array'.";
  var rsErrorXMLArrayIllegalElement = "XML-RpcClient Parse Error: Illegal element.";
  var rsErrorXMLDocRequired = "Malformed XML document.";
  var rsErrorXMLRpcDocInvalid = "Invalid XML-Rpc document.";
  var rsErrorXMLMethodResponse = "Malformed XML-Rpc methodResponse document.";
  var rsErrorQueryStringParamObjectType = "Unable to pass objects as parameters while in making a cross-site request.";
*/
  var rsErrorCanNotSanitized = "";
  var rsErrorXMLCanNotCrossSite = "";
  var rsErrorNeedAsyncForCrossSite = "";
  var rsErrorAPINamesRequired = "";
  var rsErrorAsyncHandler = "";
  var rsErrorAsyncParamType = "";
  var rsErrorXMLParamsType =  "";
  var rsErrorResponseObject = "";
  var rsErrorResponseIdMissed = "";
  var rsErrorResponseIdUnkonwn = "";
  var rsErrorQueryStringParamType = "";
  var rsErrorQueryStringParamNestedType = "";
  var rsErrorBase64FuncNotInstalled = "";
  var rsErrorXMLArrayDataMissed = "";
  var rsErrorXMLArrayIllegalElement = "";
  var rsErrorXMLDocRequired = "";
  var rsErrorXMLRpcDocInvalid = "";
  var rsErrorXMLMethodResponse = "";
  var rsErrorQueryStringParamObjectType = "";

	//Set other default options
	var vProvidedMethodList;
	this.FIsAsynchronous = true;
	this.FIsResponseSanitized = true;
	this.FUserName = null;
	this.FPassword = null;
	this.FCallbackParamName = 'JSON-response-callback';
	this.FProtocol = cJSONRpc;
	this.FDateEncoding = 'ISO8601'; // ("@timestamp@" || "@ticks@") || "classHinting" || "ASP.NET"
	this.FDecodeISO8601 = true; //JSON only
	
	//Get the provided options
	if(options instanceof Object){
		if(options.IsAsync !== undefined){
			this.FIsAsynchronous = !!options.IsAsync;
			if(!this.FIsAsynchronous && this.FIsCrossSite)
				throw Error(cErrorNeedAsyncForCrossSite, rsErrorNeedAsyncForCrossSite);
		}
		if(options.Sanitize != undefined)
			this.FIsResponseSanitized = !!options.Sanitize;
		if(options.User != undefined)
			this.FUserName = options.User;
		if(options.Passwd != undefined)
			this.FPassword = options.Passwd;
		if(options.CallbackParamName != undefined)
			this.FCallbackParamName = options.CallbackParamName;
		if(String(options.Protocol) == cXMLRpc)
			this.FProtocol = cXMLRpc;
		if(options.DateEncoding != undefined)
			this.FDateEncoding = options.DateEncoding;
		if(options.DecodeISO8601 != undefined)
			this.FDecodeISO8601 = !!options.DecodeISO8601;
		vProvidedMethodList = options.Methods;
	}

	if(this.FIsCrossSite){
		if(this.FIsResponseSanitized){
			throw Error(cErrorCanNotSanitized, rsErrorCanNotSanitized);
		}
		else if(this.FProtocol == cXMLRpc)
			throw Error(cErrorXMLCanNotCrossSite, rsErrorXMLCanNotCrossSite);
	}
	
	//Obtain the list of methods made available by the server
	if(this.FIsCrossSite && !vProvidedMethodList)
		throw Error(cErrorAPINamesRequired, rsErrorAPINamesRequired);
	if(vProvidedMethodList)
		this.FMethodList = vProvidedMethodList;
	else {
		//Introspection must be performed synchronously
		var async = this.FIsAsynchronous;
		this.FIsAsynchronous = false;
		this.FMethodList = this.iCallMethod("system.listMethods", []);
		this.FIsAsynchronous = async;
	}
	this.FMethodList.push('system.listMethods');
	this.FMethodList.push('system.describe');
	
	//Create local "wrapper" functions which reference the methods obtained above
	for(var methodName, i = 0; methodName = this.FMethodList[i]; i++){
		//Make available the received methods in the form of chained property lists (eg. "parent.child.methodName")
		var methodObject = this;
		var propChain = methodName.split(/\./);
		for(var j = 0; j+1 < propChain.length; j++){
			if(!methodObject[propChain[j]])
				methodObject[propChain[j]] = {};
			methodObject = methodObject[propChain[j]];
		}

		//Create a wrapper to this.iCallMethod with this instance and this methodName bound
		var wrapper = (function(instance, methodName){
			var call = {instance:instance, methodName:methodName}; //Pass parameters into closure
			return function(){
				if(call.instance.FIsAsynchronous){
					if(arguments.length == 1 && arguments[0] instanceof Object){
						call.instance.iCallMethod(call.methodName,
												 arguments[0].params,
												 arguments[0].onSuccess,
												 arguments[0].onException,
												 arguments[0].onComplete);
					}
					else {
						call.instance.iCallMethod(call.methodName,
												 arguments[0],
												 arguments[1],
												 arguments[2],
												 arguments[3]);
					}	
					return undefined;
				}
				else return call.instance.iCallMethod(call.methodName, RpcClient.toArray(arguments));
			};
		})(this, methodName);
		
		methodObject[propChain[propChain.length-1]] = wrapper;
	}
};

RpcClient.setAsynchronous = function(Service, isAsynchronous){
	if(!isAsynchronous && Service.FIsCrossSite)
		throw Error(cErrorNeedAsyncForCrossSite, rsErrorNeedAsyncForCrossSite);
	Service.FIsAsynchronous = !!isAsynchronous;
};

RpcClient.Service.prototype.iCallMethod = function(methodName, params, successHandler, exceptionHandler, completeHandler){
	RpcClient.RequestCount++;
	//Verify that successHandler, exceptionHandler, and completeHandler are functions
	if(this.FIsAsynchronous){
		if(successHandler && typeof successHandler != 'function')
		  //'The asynchronous onSuccess handler callback function you provided is invalid; the value you provided (' + successHandler.toString() + ') is of type "' + typeof(successHandler) + '".'
			throw Error(cErrorSuccessHandler, rsErrorAsyncHandler);
		if(exceptionHandler && typeof exceptionHandler != 'function')
			//, 'The asynchronous onException handler callback function you provided is invalid; the value you provided (' + exceptionHandler.toString() + ') is of type "' + typeof(exceptionHandler) + '".'
			throw Error(cErrorExceptionHandler, rsErrorAsyncHandler);
		if(completeHandler && typeof completeHandler != 'function')
			//, 'The asynchronous onComplete handler callback function you provided is invalid; the value you provided (' + completeHandler.toString() + ') is of type "' + typeof(completeHandler) + '".'
			throw Error(cErrorCompleteHandler, rsErrorAsyncHandler);
	}	

	try {
		//Assign the provided callback function to the response lookup table
		if(this.FIsAsynchronous || this.FIsCrossSite){
			RpcClient.pendingRequests[String(RpcClient.RequestCount)] = {
				//method:methodName,
				onSuccess:successHandler,
				onException:exceptionHandler,
				onComplete:completeHandler
			};
		}
			
		//Asynchronous cross-domain call (JSON-in-Script) -----------------------------------------------------
		if(this.FIsCrossSite){ //then this.FIsAsynchronous is implied
			
			//Create an ad hoc function specifically for this cross-site request; this is necessary because it is 
			//  not possible pass an JSON-RpcClient request object with an id over HTTP Get requests.
			RpcClient.callbacks['r' + String(RpcClient.RequestCount)] = (function(instance, id){
				var call = {instance: instance, id: id}; //Pass parameter into closure
				return function(response){
					if(response instanceof Object && (response.result || response.error)){
						response.id = call.id;
						instance.__doCallback(response);
					}
					else {//Allow data without response wrapper (i.e. GData)
						instance.__doCallback({id: call.id, result: response});
					}
				}
			})(this, RpcClient.RequestCount);
			//RpcClient.callbacks['r' + String(RpcClient.RequestCount)] = new Function("response", 'response.id = ' + RpcClient.RequestCount + '; this.__doCallback(response);');
			
			//Make the request by adding a SCRIPT element to the page
			var script = document.createElement('script');
			script.setAttribute('type', 'text/javascript');
			var src = this.FServiceURL +
						'/' + methodName +
						'?' + this.FCallbackParamName + '=RpcClient.callbacks.r' + (RpcClient.RequestCount);
			if(params)
				src += '&' + RpcClient.toQueryString(params);
			script.setAttribute('src', src);
			script.setAttribute('id', 'RpcClient' + RpcClient.RequestCount);
			var head = document.getElementsByTagName('head')[0];
			RpcClient.pendingRequests[RpcClient.RequestCount].scriptElement = script;
			head.appendChild(script);
			
			return undefined;
		}
		//Calls made with XMLHttpRequest ------------------------------------------------------------
		else {
			//Obtain and verify the parameters
			if(params){
				if(!(params instanceof Object) || params instanceof Date) //JSON-RpcClient 1.1 allows params to be a hash not just an array
				  // 'When making asynchronous calls, the parameters for the method must be passed as an array (or a hash); the value you supplied (' + String(params) + ') is of type "' + typeof(params) + '".'
					throw Error(cErrorAsyncParamType, rsErrorAsyncParamType);
				//request.params = params;
			}
			
			//Prepare the XML-RpcClient request
			var request,postData;
			switch(this.FProtocol) {
			  case cJSONRpc: {//Prepare the JSON-RpcClient request
  				request = {
  					version:"1.1",
  					method:methodName,
  					id:RpcClient.RequestCount
  				};
  				if(params)
  					request.params = params;
  				postData = this.__toJSON(request);
			    break;
			  }
			  case cXMLRpc:{
  				if(!(params instanceof Array))
  					throw Error(cErrorXMLParamsType, rsErrorXMLParamsType);
  				
  				var xml = ['<?xml version="1.0"?><methodCall><methodName>' + methodName + '</methodName>'];
  				if(params){
  					xml.push('<params>');
  					for(var i = 0; i < params.length; i++)
  						xml.push('<param>' + this.__toXMLRPC(params[i]) + '</param>');
  					xml.push('</params>');
  				}
  				xml.push('</methodCall>');
  				postData = xml.join('');
  				
  				//request = new Document();
  				//var methodCallEl = document.createElement('methodCall');
  				//var methodNameEl = document.createElement('methodName');
  				//methodNameEl.appendChild(document.createTextNode(methodName));
  				//methodCallEl.appendChild(methodNameEl);
  				//if(params){
  				//	var paramsEl = document.createElement('params');
  				//	for(var i = 0; i < params.length; i++){
  				//		var paramEl = document.createElement('param');
  				//		paramEl.appendChild(this.__toXMLRPC(params[i]));
  				//		paramsEl.appendChild(paramEl);
  				//	}
  				//	methodCallEl.appendChild(paramsEl);
  				//}
  				//request.appendChild(methodCallEl);
  				//postData = request.serializeXML();
			    break;
			  }
			}
			
			//XMLHttpRequest chosen (over Ajax.Request) because it propogates uncaught exceptions
			var xhr = RpcClient.createXMLHttp();
			/*
			if(window.XMLHttpRequest)
				xhr = new XMLHttpRequest();
			else if(window.ActiveXObject){
				try {
					xhr = new ActiveXObject('Msxml2.XMLHTTP');
				} catch(err){
					xhr = new ActiveXObject('Microsoft.XMLHTTP');
				}
			}
			*/
			xhr.open('POST', this.FServiceURL, this.FIsAsynchronous, this.FUserName, this.FPassword);
			if(this.FProtocol == cXMLRpc){
				xhr.setRequestHeader('Content-Type', 'text/xml');
				xhr.setRequestHeader('Accept', 'text/xml');
			}
			else {
				xhr.setRequestHeader('Content-Type', 'application/json');
				xhr.setRequestHeader('Accept', 'application/json');
			}
			
			//Asynchronous same-domain call -----------------------------------------------------
			if(this.FIsAsynchronous){
				//Send the request
				xhr.send(postData);
				
				//Handle the response
				var instance = this;
				var requestInfo = {id:RpcClient.RequestCount}; //for XML-RpcClient since the 'request' object cannot contain request ID
				xhr.onreadystatechange = function(){
					//QUESTION: Why can't I use this.readyState?
					if(xhr.readyState == 4){
						//XML-RpcClient
						if(instance.FProtocol == cXMLRpc){
							var response = instance.__getXMLRPCResponse(xhr, requestInfo.id);
							instance.__doCallback(response);
						}
						//JSON-RpcClient
						else {
							var response = instance.__evalJSON(xhr.responseText, instance.FIsResponseSanitized);
							if(!response.id)
								response.id = requestInfo.id;
							instance.__doCallback(response);
						}
					}
				};
				
				return undefined;
			}
			//Synchronous same-domain call -----------------------------------------------------
			else {
				//Send the request
				xhr.send(postData);
				var response;
				if(this.FProtocol == cXMLRpc)
					response = this.__getXMLRPCResponse(xhr, RpcClient.RequestCount);
				else
					response = this.__evalJSON(xhr.responseText, this.FIsResponseSanitized);
				
				//Note that this error must be caught with a try/catch block instead of by passing a onException callback
				if(response.error)
					throw Error('Unable to call "' + methodName + '". Server responsed with error (code ' + response.error.code + '): ' + response.error.message);
				
				this.__upgradeValuesFromJSON(response);
				return response.result;
			}
		}
	}
	catch(err){
		//err.locationCode = PRE-REQUEST Cleint
		var isCaught = false;
		if(exceptionHandler)
			isCaught = exceptionHandler(err); //add error location
		if(completeHandler)
			completeHandler();
			
		if(!isCaught)
			throw err;
	}
};

//This acts as a lookup table for the response callback to execute the user-defined
//   callbacks and to clean up after a request
RpcClient.pendingRequests = {};

//Ad hoc cross-site callback functions keyed by request ID; when a cross-site request
//   is made, a function is created 
RpcClient.callbacks = {};

RpcClient.Service.prototype.setAsynchronous = function(aIsAsync){
	if(!aIsAsync && this.FIsCrossSite)
		throw Error(cErrorNeedAsyncForCrossSite, rsErrorNeedAsyncForCrossSite);
  this.FIsAsynchronous = aIsAsync;
}

//Called by asychronous calls when their responses have loaded
RpcClient.Service.prototype.__doCallback = function(response){
	if(typeof response != 'object')
		throw Error(cErrorResponseObject, rsErrorResponseObject);
	if(!response.id)
		throw Error(cErrorResponseIdMissed, rsErrorResponseIdMissed);

	if(!RpcClient.pendingRequests[response.id])
	  //'Fatal error with RpcClient code: no such ID "' + response.id + '" found in pendingRequests.'
		throw Error(cErrorResponseIdUnkonwn, rsErrorResponseIdUnkonwn);
	
	//Remove the SCRIPT element from the DOM tree for cross-site (JSON-in-Script) requests
	if(RpcClient.pendingRequests[response.id].scriptElement){
		var script = RpcClient.pendingRequests[response.id].scriptElement;
		script.parentNode.removeChild(script);
	}
	//Remove the ad hoc cross-site callback function
	if(RpcClient.callbacks[response.id])
		delete RpcClient.callbacks['r' + response.id];
	
	var uncaughtExceptions = [];
	
	//Handle errors returned by the server
	if(response.error !== undefined){
		var err = new Error(response.error.message);
		err.code = response.error.code;
		//err.locationCode = SERVER
		if(RpcClient.pendingRequests[response.id].onException){
			try{
				if(!RpcClient.pendingRequests[response.id].onException(err))
					uncaughtExceptions.push(err);
			}
			catch(err2){ //If the onException handler also fails
				uncaughtExceptions.push(err);
				uncaughtExceptions.push(err2);
			}
		}
		else uncaughtExceptions.push(err);
	}
	
	//Process the valid result
	else if(response.result !== undefined){
		//iterate over all values and substitute date strings with Date objects
		//Note that response.result is not passed because the values contained
		//  need to be modified by reference, and the only way to do so is
		//  but accessing an object's properties. Thus an extra level of
		//  abstraction allows for accessing all of the results members by reference.
		this.__upgradeValuesFromJSON(response);
		
		if(RpcClient.pendingRequests[response.id].onSuccess){
			try {
				RpcClient.pendingRequests[response.id].onSuccess(response.result);
			}
			//If the onSuccess callback itself fails, then call the onException handler as above
			catch(err){
				//err3.locationCode = CLIENT;
				if(RpcClient.pendingRequests[response.id].onException){
					try {
						if(!RpcClient.pendingRequests[response.id].onException(err))
							uncaughtExceptions.push(err);
					}
					catch(err2){ //If the onException handler also fails
						uncaughtExceptions.push(err);
						uncaughtExceptions.push(err2);
					}
				}
				else uncaughtExceptions.push(err);
			}
		}
	}
	
	//Call the onComplete handler
	try {
		if(RpcClient.pendingRequests[response.id].onComplete)
			RpcClient.pendingRequests[response.id].onComplete(response);
	}
	catch(err){ //If the onComplete handler fails
		//err3.locationCode = CLIENT;
		if(RpcClient.pendingRequests[response.id].onException){
			try {
				if(!RpcClient.pendingRequests[response.id].onException(err))
					uncaughtExceptions.push(err);
			}
			catch(err2){ //If the onException handler also fails
				uncaughtExceptions.push(err);
				uncaughtExceptions.push(err2);
			}
		}
		else uncaughtExceptions.push(err);
	}
	
	delete RpcClient.pendingRequests[response.id];
	
	//Merge any exception raised by onComplete into the previous one(s) and throw it
	if(uncaughtExceptions.length){
		var code;
		var message = 'There ' + (uncaughtExceptions.length == 1 ?
							 'was 1 uncaught exception' :
							 'were ' + uncaughtExceptions.length + ' uncaught exceptions') + ': ';
		for(var i = 0; i < uncaughtExceptions.length; i++){
			if(i)
				message += "; ";
			message += uncaughtExceptions[i].message;
			if(uncaughtExceptions[i].code)
				code = uncaughtExceptions[i].code;
		}
		var err = new Error(message);
		err.code = code;	
		throw err;
	}
};


/*******************************************************************************************
 * JSON-RpcClient Specific Functions
 ******************************************************************************************/
RpcClient.Service.prototype.__toJSON = function(value){
	switch(typeof value){
		case 'number':
			return isFinite(value) ? value.toString() : 'null';
		case 'boolean':
			return value.toString();
		case 'string':
			//Taken from Ext JSON.js
			var specialChars = {
				"\b": '\\b',
				"\t": '\\t',
				"\n": '\\n',
				"\f": '\\f',
				"\r": '\\r',
				'"' : '\\"',
				"\\": '\\\\',
				"/" : '\/'
			};
			return '"' + value.replace(/([\x00-\x1f\\"])/g, function(a, b) {
				var c = specialChars[b];
				if(c)
					return c;
				c = b.charCodeAt();
				//return "\\u00" + Math.floor(c / 16).toString(16) + (c % 16).toString(16);
				return '\\u00' + RpcClient.zeroPad(c.toString(16));
			}) + '"';
		case 'object':
			if(value === null)
				return 'null';
			else if(value instanceof Array){
				var json = ['['];  //Ext's JSON.js reminds me that Array.join is faster than += in MSIE
				for(var i = 0; i < value.length; i++){
					if(i)
						json.push(',');
					json.push(this.__toJSON(value[i]));
				}
				json.push(']');
				return json.join('');
			}
			else if(value instanceof Date){
				switch(this.FDateEncoding){
					case 'classHinting': //{"__jsonclass__":["constructor", [param1,...]], "prop1": ...}
						return '{"__jsonclass__":["Date",[' + value.valueOf() + ']]}';
					case '@timestamp@':
					case '@ticks@':
						return '"@' + value.valueOf() + '@"';
					case 'ASP.NET':
						return '"\\/Date(' + value.valueOf() + ')\\/"';
					default:
						return '"' + RpcClient.dateToISO8601(value) + '"';
				}
			}
			else if(value instanceof Number || value instanceof String || value instanceof Boolean)
				return this.__toJSON(value.valueOf());
			else {
				var useHasOwn = {}.hasOwnProperty ? true : false; //From Ext's JSON.js
				var json = ['{'];
				for(var key in value){
					if(!useHasOwn || value.hasOwnProperty(key)){
						if(json.length > 1)
							json.push(',');
						json.push(this.__toJSON(key) + ':' + this.__toJSON(value[key]));
					}
				}
				json.push('}');
				return json.join('');
			}
		//case 'undefined':
		//case 'function':
		//case 'unknown':
		//default:
	}
	throw new TypeError('Unable to convert the value of type "' + typeof(value) + '" to JSON.'); //(' + String(value) + ') 
};

RpcClient.isJSON = function(string){ //from Prototype String.isJSON()
    var testStr = string.replace(/\\./g, '@').replace(/"[^"\\\n\r]*"/g, '');
    return (/^[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]*$/).test(testStr);
};

RpcClient.Service.prototype.__evalJSON = function(json, sanitize){ //from Prototype String.evalJSON()
	//Remove security comment delimiters
	json = json.replace(/^\/\*-secure-([\s\S]*)\*\/\s*$/, "$1");
	var err;
    try {
		if(!sanitize || RpcClient.isJSON(json))
			return eval('(' + json + ')');
    }
	catch(e){err = e;}
    throw new SyntaxError(e.code, 'Badly formed JSON string: ' + json + " ... " + (err ? err.message : ''));
};

//This function iterates over the properties of the passed object and converts them 
//   into more appropriate data types, i.e. ISO8601 strings are converted to Date objects.
RpcClient.Service.prototype.__upgradeValuesFromJSON = function(obj){
	var matches, useHasOwn = {}.hasOwnProperty ? true : false;
	for(var key in obj){
		if(!useHasOwn || obj.hasOwnProperty(key)){
			//Parse date strings
			if(typeof obj[key] == 'string'){
				//ISO8601
				if(this.FDecodeISO8601 && (matches = obj[key].match(/^(?:(\d\d\d\d)-(\d\d)(?:-(\d\d)(?:T(\d\d)(?::(\d\d)(?::(\d\d)(?:\.(\d+))?)?)?)?)?)$/))){
					obj[key] = new Date(0);
					if(matches[1]) obj[key].setUTCFullYear(parseInt(matches[1]));
					if(matches[2]) obj[key].setUTCMonth(parseInt(matches[2]-1));
					if(matches[3]) obj[key].setUTCDate(parseInt(matches[3]));
					if(matches[4]) obj[key].setUTCHours(parseInt(matches[4]));
					if(matches[5]) obj[key].setUTCMinutes(parseInt(matches[5]));
					if(matches[6]) obj[key].setUTCMilliseconds(parseInt(matches[6]));
				}
				//@timestamp@ / @ticks@
				else if(matches = obj[key].match(/^@(\d+)@$/)){
					obj[key] = new Date(parseInt(matches[1]))
				}
				//ASP.NET
				else if(matches = obj[key].match(/^\/Date\((\d+)\)\/$/)){
					obj[key] = new Date(parseInt(matches[1]))
				}
			}
			else if(obj[key] instanceof Object){

				//JSON 1.0 Class Hinting: {"__jsonclass__":["constructor", [param1,...]], "prop1": ...}
				if(obj[key].__jsonclass__ instanceof Array){
					//console.info('good1');
					if(obj[key].__jsonclass__[0] == 'Date'){
						//console.info('good2');
						if(obj[key].__jsonclass__[1] instanceof Array && obj[key].__jsonclass__[1][0])
							obj[key] = new Date(obj[key].__jsonclass__[1][0]);
						else
							obj[key] = new Date();
					}
				}
				else this.__upgradeValuesFromJSON(obj[key]);
			}
		}
	}
};


/*******************************************************************************************
 * XML-RpcClient Specific Functions
 ******************************************************************************************/

RpcClient.Service.prototype.__toXMLRPC = function(value){
	var xml = ['<value>'];
	switch(typeof value){
		case 'number':
			if(!isFinite(value))
				xml.push('<nil/>');
			else if(parseInt(value) == Math.ceil(value)){
				xml.push('<int>');
				xml.push(value.toString());
				xml.push('</int>');
			}
			else {
				xml.push('<double>');
				xml.push(value.toString());
				xml.push('</double>');
			}
			break;
		case 'boolean':
			xml.push('<boolean>');
			xml.push(value ? '1' : '0');
			xml.push('</boolean>');
			break;
		case 'string':
			xml.push('<string>');
			xml.push(value.replace(/[<>&]/, function(ch){
				
			})); //escape for XML!
			xml.push('</string>');
			break;
		case 'object':
			if(value === null)
				xml.push('<nil/>');
			else if(value instanceof Array){
				xml.push('<array><data>');
				for(var i = 0; i < value.length; i++)
					xml.push(this.__toXMLRPC(value[i]));
				xml.push('</data></array>');
			}
			else if(value instanceof Date){
				xml.push('<dateTime.iso8601>' + RpcClient.dateToISO8601(value) + '</dateTime.iso8601>');
			}
			else if(value instanceof Number || value instanceof String || value instanceof Boolean)
				return RpcClient.dateToISO8601(value.valueOf());
			else {
				xml.push('<struct>');
				var useHasOwn = {}.hasOwnProperty ? true : false; //From Ext's JSON.js
				for(var key in value){
					if(!useHasOwn || value.hasOwnProperty(key)){
						xml.push('<member>');
						xml.push('<name>' + key + '</name>'); //Excape XML!
						xml.push(this.__toXMLRPC(value[key]));
						xml.push('</member>');
					}
				}
				xml.push('</struct>');
			}
			break;
		//case 'undefined':
		//case 'function':
		//case 'unknown':
		default:
			throw new TypeError('Unable to convert the value of type "' + typeof(value) + '" to XML-RpcClient.'); //(' + String(value) + ')
	}
	xml.push('</value>');
	return xml.join('');
};

//RpcClient.Service.prototype.toXMLRPC = function(value){ //documentNode
//	var valueEl = document.createElement('value');
//	//var xml = ['<value>'];
//	switch(typeof value){
//		case 'number':
//			if(!isFinite(value))
//				//xml.push('<nil/>');
//				valueEl.appendChild(document.createElement('nil'));
//			//else if(parseInt(value) == Math.ceil(value)){
//			//	var intEl = document.createElement('int');
//			//	intEl.appendChild(document.createTextNode(value.toString()));
//			//	valueEl.appendChild(intEl);
//			//	//xml.push('<int>');
//			//	//xml.push(value.toString());
//			//	//xml.push('</int>');
//			//}
//			//else {
//			//	var doubleEl = document.createElement('double');
//			//	doubleEl.appendChild(document.createTextNode(value.toString()));
//			//	valueEl.appendChild(doubleEl);
//			//	//xml.push('<double>');
//			//	//xml.push(value.toString());
//			//	//xml.push('</double>');
//			//}
//			else {
//				var numEl = document.createElement(parseInt(value) == Math.ceil(value) ? 'int' : 'double');
//				numEl.appendChild(document.createTextNode(value.toString()));
//				valueEl.appendChild(numEl);
//			}
//			return valueEl;
//		case 'boolean':
//			var boolEl = document.createElement('boolean');
//			boolEl.appendChild(document.createTextNode(value ? '1' : '0'));
//			valueEl.appendChild(boolEl);
//			return valueEl;
//			//xml.push('<boolean>');
//			//xml.push(value ? '1' : '0');
//			//xml.push('</boolean>');
//		case 'string':
//			var stringEl = document.createElement('string');
//			stringEl.appendChild(document.createTextNode(value));
//			valueEl.appendChild(stringEl);
//			return valueEl;
//		case 'object':
//			if(value === null)
//				valueEl.appendChild(document.createElement('nil'));
//			else if(value instanceof Array){
//				var arrayEl = document.createElement('array');
//				var dataEl = document.createElement('data');
//				for(var i = 0; i < value.length; i++)
//					dataEl.appendChild(this.__toXMLRPC(value[i]));
//				arrayEl.appendChild(dataEl);
//				valueEl.appendChild(arrayEl);
//			}
//			else if(value instanceof Date){
//				var dateEl = document.createElement('datetime.ISO8601');
//				dateEl.appendChild(document.createTextNode(RpcClient.dateToISO8601(value)));
//				valueEl.appendChild(dateEl);
//			}
//			else if(value instanceof Number || value instanceof String || value instanceof Boolean)
//				return RpcClient.dateToISO8601(value.valueOf());
//			else {
//				var structEl = document.createElement('struct');
//				var useHasOwn = {}.hasOwnProperty ? true : false; //From Ext's JSON.js
//				for(var key in value){
//					if(!useHasOwn || value.hasOwnProperty(key)){
//						var memberEl = document.createElement('member');
//						var nameEl = document.createElement('name')
//						nameEl.appendChild(document.createTextNode(key));
//						memberEl.appendChild(nameEl);
//						memberEl.appendChild(this.__toXMLRPC(value[key]));
//						structEl.appendChild(memberEl);
//					}
//				}
//				valueEl.appendChild(structEl);
//			}
//			return valueEl;
//		//case 'undefined':
//		//case 'function':
//		//case 'unknown':
//		//default:
//	}
//	throw new TypeError('Unable to convert the value of type "' + typeof(value) + '" to XML-RpcClient.'); //(' + String(value) + ')
//};

RpcClient.Service.prototype.__parseXMLRPC = function(valueEl){
	if(valueEl.childNodes.length == 1 &&
	   valueEl.childNodes.item(0).nodeType == 3)
	{
		return valueEl.childNodes.item(0).nodeValue;
	}
	for(var i = 0; i < valueEl.childNodes.length; i++){
		if(valueEl.childNodes.item(i).nodeType == 1){
			var typeEL = valueEl.childNodes.item(i);
			switch(typeEL.nodeName.toLowerCase()){
				case 'i4':
				case 'int':
					//An integer is a 32-bit signed number. You can include a plus or minus at the
					//   beginning of a string of numeric characters. Leading zeros are collapsed.
					//   Whitespace is not permitted. Just numeric characters preceeded by a plus or minus.
					var intVal = parseInt(typeEL.firstChild.nodeValue);
					if(isNaN(intVal))
						throw Error("XML-Rpc Parse Error: The value provided as an integer '" + typeEL.firstChild.nodeValue + "' is invalid.");
					return intVal;
				case 'double':
					//There is no representation for infinity or negative infinity or "not a number".
					//   At this time, only decimal point notation is allowed, a plus or a minus,
					//   followed by any number of numeric characters, followed by a period and any
					//   number of numeric characters. Whitespace is not allowed. The range of
					//   allowable values is implementation-dependent, is not specified.
					var floatVal = parseFloat(typeEL.firstChild.nodeValue);
					if(isNaN(floatVal))
						throw Error(cErrorXMLDoubleTypeInvalid, "XML-Rpc Parse Error: The value provided as a double '" + typeEL.firstChild.nodeValue + "' is invalid.");
					return floatVal;
				case 'boolean':
					if(typeEL.firstChild.nodeValue != '0' && typeEL.firstChild.nodeValue != '1')
						throw Error(cErrorXMLBoolTypeInvalid, "XML-Rpc Parse Error: The value provided as a boolean '" + typeEL.firstChild.nodeValue + "' is invalid.");
					return Boolean(parseInt(typeEL.firstChild.nodeValue));
				case 'string':
					if(!typeEL.firstChild)
						return "";
					return typeEL.firstChild.nodeValue;
				case 'datetime.iso8601':
					var matches, date = new Date(0);
					if(matches = typeEL.firstChild.nodeValue.match(/^(?:(\d\d\d\d)-(\d\d)(?:-(\d\d)(?:T(\d\d)(?::(\d\d)(?::(\d\d)(?:\.(\d+))?)?)?)?)?)$/)){
						if(matches[1]) date.setUTCFullYear(parseInt(matches[1]));
						if(matches[2]) date.setUTCMonth(parseInt(matches[2]-1));
						if(matches[3]) date.setUTCDate(parseInt(matches[3]));
						if(matches[4]) date.setUTCHours(parseInt(matches[4]));
						if(matches[5]) date.setUTCMinutes(parseInt(matches[5]));
						if(matches[6]) date.setUTCMilliseconds(parseInt(matches[6]));
						return date;
					}
					throw Error(cErrorXMLISODateTypeInvalid, "XML-Rpc Parse Error: The provided value does not match ISO8601.");
				case 'base64':
					//base64_decode
					if (typeof(atob) != "undefined") {
  					return atob(typeEL.firstChild.nodeValue)
  			  else
					  throw Error(cErrorBase64FuncNotInstalled, rsErrorBase64FuncNotInstalled);
				case 'nil':
					return null;
				case 'struct':
					//A <struct> contains <member>s and each <member> contains a <name> and a <value>.
					var obj = {};
					for(var memberEl, j = 0; memberEl = typeEL.childNodes.item(j); j++){
						if(memberEl.nodeType == 1 && memberEl.nodeName == 'member'){
							var name = '';
							valueEl = null;
							for(var child, k = 0; child = memberEl.childNodes.item(k); k++){
								if(child.nodeType == 1){
									if(child.nodeName == 'name')
										name = child.firstChild.nodeValue;
									else if(child.nodeName == 'value')
										valueEl = child;
								}
							}
							//<struct>s can be recursive, any <value> may contain a <struct> or
							//   any other type, including an <array>, described below.
							if(name && valueEl)
								obj[name] = this.__parseXMLRPC(valueEl);
						}
					}
					return obj;
				case 'array':
					//An <array> contains a single <data> element, which can contain any number of <value>s.
					var arr = [];
					var dataEl = typeEL.firstChild;
					while(dataEl && (dataEl.nodeType != 1 || dataEl.nodeName != 'data'))
						dataEl = dataEl.nextSibling;
					
					if(!dataEl)
						throw Error(cErrorXMLArrayDataMissed, rsErrorXMLArrayDataMissed);
					
					valueEl = dataEl.firstChild;
					while(valueEl){
						if(valueEl.nodeType == 1){
							//<arrays>s can be recursive, any value may contain an <array> or
							//   any other type, including a <struct>, described above.
							if(valueEl.nodeName == 'value')
								arr.push(this.__parseXMLRPC(valueEl));
							else
								throw Error(cErrorXMLArrayValueMissed, rsErrorXMLArrayValueMissed);
						}
						valueEl = valueEl.nextSibling;
					}
					return arr;
				default:
				  //"XML-RpcClient Parse Error: Illegal element '" + typeEL.nodeName + "' child of the 'value' element."
					throw Error(cErrorXMLArrayIllegalElement, rsErrorXMLArrayIllegalElement);
			}
		}
	}
	return '';
}

RpcClient.Service.prototype.__getXMLRPCResponse = function(xhr, id){
	var response = {};
	if(!xhr.responseXML)
		throw Error(cErrorXMLDocRequired, rsErrorXMLDocRequired);
	var doc = xhr.responseXML.documentElement;
	if(doc.nodeName != 'methodResponse')
		throw Error(cErrorXMLRpcDocInvalid, rsErrorXMLRpcDocInvalid);
	
	var valueEl = doc.getElementsByTagName('value')[0];
	if(valueEl.parentNode.nodeName == 'param' &&
	   valueEl.parentNode.parentNode.nodeName == 'params')
	{
		response.result = this.__parseXMLRPC(valueEl);
	}
	else if(valueEl.parentNode.nodeName == 'fault'){
		var fault = this.__parseXMLRPC(valueEl);
		response.error = {
			code: fault.faultCode,
			message: fault.faultString
		};
	}
	else throw Error(cErrorXMLRpcDocInvalid, rsErrorXMLRpcDocInvalid);
	
	if(!response.result && !response.error)
		throw Error(cErrorXMLMethodResponse, rsErrorXMLMethodResponse);
	
	response.id = id; //XML-RpcClient cannot pass and return request IDs
	return response;
};

/*******************************************************************************************
 * Other helper functions
 ******************************************************************************************/

//Takes an array or hash and coverts it into a query string, converting dates to ISO8601
//   and throwing an exception if nested hashes or nested arrays appear.
RpcClient.toQueryString = function(params){
	if(!(params instanceof Object || params instanceof Array) || params instanceof Date)
	  //'You must supply either an array ,date or object type to convert into a query string. You supplied: ' + params.constructor
		throw Error(cErrorQueryStringParamType, rsErrorQueryStringParamType);

	var str = '';
	var useHasOwn = {}.hasOwnProperty ? true : false;
	
	for(var key in params){
		if(useHasOwn && params.hasOwnProperty(key)){
			//Process an array
			if(params[key] instanceof Array){
				for(var i = 0; i < params[key].length; i++){
					if(str)
						str += '&';
					str += encodeURIComponent(key) + "=";
					if(params[key][i] instanceof Date)
						str += encodeURIComponent(RpcClient.dateToISO8601(params[key][i]));
					else if(params[key][i] instanceof Object)
					  //'Unable to pass nested arrays nor objects as parameters while in making a cross-site request. The object in question has this constructor: ' + params[key][i].constructor);
						throw Error(cErrorQueryStringParamNestedType, rsErrorQueryStringParamNestedType);
					else str += encodeURIComponent(String(params[key][i]));
				}
			}
			else {
				if(str)
					str += '&';
				str += encodeURIComponent(key) + "=";
				if(params[key] instanceof Date)
					str += encodeURIComponent(RpcClient.dateToISO8601(params[key]));
				else if(params[key] instanceof Object)
				  //'Unable to pass objects as parameters while in making a cross-site request. The object in question has this constructor: ' + params[key].constructor
					throw Error(cErrorQueryStringParamObjectType, rsErrorQueryStringParamObjectType);
				else str += encodeURIComponent(String(params[key]));
			}
		}
	}
	return str;
};

//Converts an iterateable value into an array; similar to Prototype's $A function
RpcClient.toArray = function(value){
	//if(value && value.length){
		if(value instanceof Array)
			return value;
		var array = [];
		for(var i = 0; i < value.length; i++)
			array.push(value[i]);
		return array;
	//}
	//throw Error("Unable to convert to an array the value: " + String(value));
};

//Returns an ISO8601 string *in UTC* for the provided date (Prototype's Date.toJSON() returns localtime)
RpcClient.dateToISO8601 = function(date){
	//var jsonDate = date.toJSON();
	//return jsonDate.substring(1, jsonDate.length-1); //strip double quotes
	
	return date.getUTCFullYear()             + '-' +
	       RpcClient.zeroPad(date.getUTCMonth()+1) + '-' +
		   RpcClient.zeroPad(date.getUTCDate())    + 'T' +
	       RpcClient.zeroPad(date.getUTCHours())   + ':' +
		   RpcClient.zeroPad(date.getUTCMinutes()) + ':' +
		   RpcClient.zeroPad(date.getUTCSeconds()) + '.' +
		   //Prototype's Date.toJSON() method does not include milliseconds
		   RpcClient.zeroPad(date.getUTCMilliseconds(), 3);
};

RpcClient.zeroPad = function(value, width){
	if(!width)
		width = 2;
	value = (value == undefined ? '' : String(value))
	while(value.length < width)
		value = '0' + value;
	return value;
};