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
 *                         asynchronous: true,   //default: true
 *                         sanitize: true,       //default: true
 *                         methods: ['greet'],   //default: null (synchronous introspection populates)
 *                         protocol: cJSONRpc, //default: JSON-RpcClient
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
 * 'callbackParamName' in order to specify the callback function name parameter;
 * the default is 'JSON-response-callback', as used by associated JSON/XML-RpcClient
 * Server project. For example, getting Google Calendar data:
 *
 * var gcalService = new RpcClient.Service("http://www.google.com/calendar/feeds/myemail%40gmail.com/public", {
 *                         asynchronous: true,  //true (default) required, otherwise error raised
 *                         sanitize: false,     //explicit false required, otherwise error raised
 *                         methods: ['full']    //explicit list required, otherwise error raised
 *                         callbackParamName: 'callback'
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
 */

var RpcClient = {
	version:"1.0.0.0",	
	requestCount: 0
};

var cErrorCanNotSanitized = 1;
var cErrorXMLCanNotCrossSite = 2;
var cErrorNeedAsyncForCrossSite = 3;
var cErrorAPINamesRequired = 4;

var cJSONRpc = "JSON-Rpc";
var cXMLRpc  = "XML-Rpc";

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
						"'sanitize' option to false (it is true by default) in order to proceed with making " +
						"potentially insecure cross-site RpcClient calls.";
  var rsErrorXMLCanNotCrossSite = "Unable to use the XML-RpcClient protocol to access services on other domains.";
  var rsErrorNeedAsyncForCrossSite = "It is not possible to establish a synchronous connection to a cross-site RpcClient service.";
  var rsErrorAPINamesRequired = "You must manually supply the service's method names since auto-introspection is not permitted for cross-site services.";
*/
  var rsErrorCanNotSanitized = "";
  var rsErrorXMLCanNotCrossSite = "";
  var rsErrorNeedAsyncForCrossSite = "";
  var rsErrorAPINamesRequired = "";

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
		if(options.asynchronous !== undefined){
			this.FIsAsynchronous = !!options.asynchronous;
			if(!this.FIsAsynchronous && this.FIsCrossSite)
				throw Error(cErrorNeedAsyncForCrossSite, rsErrorNeedAsyncForCrossSite);
		}
		if(options.sanitize != undefined)
			this.FIsResponseSanitized = !!options.sanitize;
		if(options.user != undefined)
			this.FUserName = options.user;
		if(options.password != undefined)
			this.FPassword = options.password;
		if(options.callbackParamName != undefined)
			this.FCallbackParamName = options.callbackParamName;
		if(String(options.protocol) == cXMLRpc)
			this.FProtocol = cXMLRpc;
		if(options.dateEncoding != undefined)
			this.FDateEncoding = options.dateEncoding;
		if(options.decodeISO8601 != undefined)
			this.FDecodeISO8601 = !!options.decodeISO8601;
		vProvidedMethodList = options.methods;
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
	RpcClient.requestCount++;
	
	//Verify that successHandler, exceptionHandler, and completeHandler are functions
	if(this.FIsAsynchronous){
		if(successHandler && typeof successHandler != 'function')
			throw Error('The asynchronous onSuccess handler callback function you provided is invalid; the value you provided (' + successHandler.toString() + ') is of type "' + typeof(successHandler) + '".');
		if(exceptionHandler && typeof exceptionHandler != 'function')
			throw Error('The asynchronous onException handler callback function you provided is invalid; the value you provided (' + exceptionHandler.toString() + ') is of type "' + typeof(exceptionHandler) + '".');
		if(completeHandler && typeof completeHandler != 'function')
			throw Error('The asynchronous onComplete handler callback function you provided is invalid; the value you provided (' + completeHandler.toString() + ') is of type "' + typeof(completeHandler) + '".');
	}	

	try {
		//Assign the provided callback function to the response lookup table
		if(this.FIsAsynchronous || this.FIsCrossSite){
			RpcClient.pendingRequests[String(RpcClient.requestCount)] = {
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
			RpcClient.callbacks['r' + String(RpcClient.requestCount)] = (function(instance, id){
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
			})(this, RpcClient.requestCount);
			//RpcClient.callbacks['r' + String(RpcClient.requestCount)] = new Function("response", 'response.id = ' + RpcClient.requestCount + '; this.__doCallback(response);');
			
			//Make the request by adding a SCRIPT element to the page
			var script = document.createElement('script');
			script.setAttribute('type', 'text/javascript');
			var src = this.FServiceURL +
						'/' + methodName +
						'?' + this.FCallbackParamName + '=RpcClient.callbacks.r' + (RpcClient.requestCount);
			if(params)
				src += '&' + RpcClient.toQueryString(params);
			script.setAttribute('src', src);
			script.setAttribute('id', 'RpcClient' + RpcClient.requestCount);
			var head = document.getElementsByTagName('head')[0];
			RpcClient.pendingRequests[RpcClient.requestCount].scriptElement = script;
			head.appendChild(script);
			
			return undefined;
		}
		//Calls made with XMLHttpRequest ------------------------------------------------------------
		else {
			//Obtain and verify the parameters
			if(params){
				if(!(params instanceof Object) || params instanceof Date) //JSON-RpcClient 1.1 allows params to be a hash not just an array
					throw Error('When making asynchronous calls, the parameters for the method must be passed as an array (or a hash); the value you supplied (' + String(params) + ') is of type "' + typeof(params) + '".');
				//request.params = params;
			}
			
			//Prepare the XML-RpcClient request
			var request,postData;
			if(this.FProtocol == cXMLRpc){
				if(!(params instanceof Array))
					throw Error("Unable to pass associative arrays to XML-RpcClient services.");
				
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
			}
			//Prepare the JSON-RpcClient request
			else {
				request = {
					version:"1.1",
					method:methodName,
					id:RpcClient.requestCount
				};
				if(params)
					request.params = params;
				postData = this.__toJSON(request);
			}
			
			//XMLHttpRequest chosen (over Ajax.Request) because it propogates uncaught exceptions
			var xhr;
			if(window.XMLHttpRequest)
				xhr = new XMLHttpRequest();
			else if(window.ActiveXObject){
				try {
					xhr = new ActiveXObject('Msxml2.XMLHTTP');
				} catch(err){
					xhr = new ActiveXObject('Microsoft.XMLHTTP');
				}
			}
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
				var requestInfo = {id:RpcClient.requestCount}; //for XML-RpcClient since the 'request' object cannot contain request ID
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
					response = this.__getXMLRPCResponse(xhr, RpcClient.requestCount);
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

//Called by asychronous calls when their responses have loaded
RpcClient.Service.prototype.__doCallback = function(response){
	if(typeof response != 'object')
		throw Error('The server did not respond with a response object.');
	if(!response.id)
		throw Error('The server did not respond with the required response id for asynchronous calls.');

	if(!RpcClient.pendingRequests[response.id])
		throw Error('Fatal error with RpcClient code: no ID "' + response.id + '" found in pendingRequests.');
	
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
    throw new SyntaxError('Badly formed JSON string: ' + json + " ... " + (err ? err.message : ''));
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
						throw Error("XML-RpcClient Parse Error: The value provided as an integer '" + typeEL.firstChild.nodeValue + "' is invalid.");
					return intVal;
				case 'double':
					//There is no representation for infinity or negative infinity or "not a number".
					//   At this time, only decimal point notation is allowed, a plus or a minus,
					//   followed by any number of numeric characters, followed by a period and any
					//   number of numeric characters. Whitespace is not allowed. The range of
					//   allowable values is implementation-dependent, is not specified.
					var floatVal = parseFloat(typeEL.firstChild.nodeValue);
					if(isNaN(floatVal))
						throw Error("XML-RpcClient Parse Error: The value provided as a double '" + typeEL.firstChild.nodeValue + "' is invalid.");
					return floatVal;
				case 'boolean':
					if(typeEL.firstChild.nodeValue != '0' && typeEL.firstChild.nodeValue != '1')
						throw Error("XML-RpcClient Parse Error: The value provided as a boolean '" + typeEL.firstChild.nodeValue + "' is invalid.");
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
					throw Error("XML-RpcClient Parse Error: The provided value does not match ISO8601.");
				case 'base64':
					throw Error("Not able to parse base64 data yet.");
					//return base64_decode(typeEL.firstChild.nodeValue);
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
						new Error("XML-RpcClient Parse Error: Expected 'data' element as sole child element of 'array'.");
					
					valueEl = dataEl.firstChild;
					while(valueEl){
						if(valueEl.nodeType == 1){
							//<arrays>s can be recursive, any value may contain an <array> or
							//   any other type, including a <struct>, described above.
							if(valueEl.nodeName == 'value')
								arr.push(this.__parseXMLRPC(valueEl));
							else
								throw Error("XML-RpcClient Parse Error: Illegal element child '" + valueEl.nodeName + "' of an array's 'data' element.");
						}
						valueEl = valueEl.nextSibling;
					}
					return arr;
				default:
					throw Error("XML-RpcClient Parse Error: Illegal element '" + typeEL.nodeName + "' child of the 'value' element.");
			}
		}
	}
	return '';
}

RpcClient.Service.prototype.__getXMLRPCResponse = function(xhr, id){
	var response = {};
	if(!xhr.responseXML)
		throw Error("Malformed XML document.");
	var doc = xhr.responseXML.documentElement;
	if(doc.nodeName != 'methodResponse')
		throw Error("Invalid XML-RpcClient document.");
	
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
	else throw Error("Invalid XML-RpcClient document.");
	
	if(!response.result && !response.error)
		throw Error("Malformed XML-RpcClient methodResponse document.");
	
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
		throw Error('You must supply either an array or object type to convert into a query string. You supplied: ' + params.constructor);

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
						throw Error('Unable to pass nested arrays nor objects as parameters while in making a cross-site request. The object in question has this constructor: ' + params[key][i].constructor);
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
					throw Error('Unable to pass objects as parameters while in making a cross-site request. The object in question has this constructor: ' + params[key].constructor);
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