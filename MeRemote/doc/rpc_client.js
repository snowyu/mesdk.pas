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

 */

/** helper function for inject */
function Arguments(args) {
  //convert arguments object to array
  this.value = [].slice.call(args);
}
/**
  @desc  inject the function
  @param aOrgFunc     the original function to be injected.
  @param aBeforeExec  this is called before the execution of the aOrgFunc.
                      you must return the arguments(new Arguments(arguments)) if you wanna modify the arguments value of the aOrgFunc.
                      it will stop the execution of the aOrgFunc if you return a value not an Arguments object nor a undefined value 
  @param aAtferExec   this is called after the execution of the aOrgFunc.
                      you must add a result argument at the last argument of the aAtferExec function if you wanna get the result value of the aOrgFunc.
                      you must add a isDenied argument following the result argument if you wanna know whether the aOrgFunc is executed.
                      you must return the result if you wanna modify the result value of the aOrgFunc .

  @Usage  Obj.prototype.Method = Inject(Obj.prototype.Method, aFunctionBeforeExec[, aFunctionAtferExec]);
  @version 1.1
  @author  Aimingoo&Riceball
  @history
    V1.0 -- fiest released.
    V1.1 -- 
      Supports to denie the aOrgFunc execution in aBeforeExec.
      Supports around in the aAtferExec, the aAtferExec be always executed even though denie the aOrgFunc execution in aBeforeExec.
        + isDenied argument to the aAtferExec function. notice the aAtferExec whether the aOrgFunc is executed

  eg:
  var doTest = function (a) {return a};
  function beforeTest(a) { alert('before exec: a='+a); a += 3; return new Arguments(arguments);};
  function afterTest(a, result, isDenied) { alert('after exec: a='+a+'; result='+result+';isDenied='+isDenied); return result+5;};
  
  doTest = Inject(doTest, beforeTest, afterTest);
  
  alert (doTest(2));
  the result should be 10.

*/
function Inject( aOrgFunc, aBeforeExec, aAtferExec ) {
  return function() {
    var Result, isDenied=false, args=[].slice.call(arguments);
    if (typeof(aBeforeExec) == 'function') {
      Result = aBeforeExec.apply(this, args);
      if (Result instanceof Arguments) //(Result.constructor === Arguments)
        args = Result.value;
      else if (isDenied = Result !== undefined)
        args.push(Result)
    }

    !isDenied && args.push(aOrgFunc.apply(this, args)); //if (!isDenied) args.push(aOrgFunc.apply(this, args));

    if (typeof(aAtferExec) == 'function')
      Result = aAtferExec.apply(this, args.concat(isDenied));
    else 
      Result = undefined;

    return (Result !== undefined ? Result : args.pop());
  }
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


 /*
 * the function createXMLHttp() modified from
 * http://webfx.eae.net/dhtml/xmlextras/xmlextras.html and
 * http://www.ugia.cn/?p=85
 */
//patch the Browser.Request of the Mootools .
Browser['__XHRNameCache'] = null;
Browser.Request = function () {
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
  else if (Browser.__XHRNameCache != null) {
      // Use the cache name first.
       return new ActiveXObject(Browser.__XHRNameCache);
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
              Browser.__XHRNameCache = MSXML[i];
              return objXMLHttp;
          }
          catch(e) {}
      }
      return null;
  }
};
Browser.Features.xhr = !!(Browser.Request());

var RpcClient = {
  //public members
	Version:"1.0.0.0",	
	RequestCount: 0,
  //resource stirng
  rs: {
    ErrorCanNotSanitized : "You are attempting to access a service on another site, and the JSON data returned " +
            "by cross-site requests cannot be sanitized. You must therefore explicitly set the " +
            "'Sanitize' option to false (it is true by default) in order to proceed with making " +
            "potentially insecure cross-site RpcClient calls.",
    ErrorXMLCanNotCrossSite : "Unable to use the XML-RpcClient protocol to access services on other domains.",
    ErrorNeedAsyncForCrossSite : "It is not possible to establish a synchronous connection to a cross-site RpcClient service.",
    ErrorAPINamesRequired : "You must manually supply the service's method names since auto-introspection is not permitted for cross-site services.",
    ErrorAsyncHandler : "The asynchronous handler callback function you provided is invalid",
    ErrorAsyncParamType : "When making asynchronous calls, the parameters for the method must be passed as an array (or a hash)",
    ErrorXMLParamsType :  "Unable to pass associative arrays to XML-RpcClient services.",
    ErrorResponseObject : "The server did not respond with a response object.",
    ErrorResponseIdMissed : "The server did not respond with the required response id for asynchronous calls.",
    ErrorResponseIdUnkonwn : "Fatal error with RpcClient code: no such ID found in pendingRequests.",
    ErrorQueryStringParamType : "You must supply either an array ,date or object type to convert into a query string. ",
    ErrorQueryStringParamNestedType : "Unable to pass nested arrays nor objects as parameters while in making a cross-site request.",
    ErrorBase64FuncNotInstalled : "Not able to parse base64 data yet.",
    ErrorXMLArrayDataMissed : "XML-Rpc Parse Error: Expected 'data' element as sole child element of 'array'.",
    ErrorXMLArrayIllegalElement : "XML-RpcClient Parse Error: Illegal element.",
    ErrorXMLDocRequired : "Malformed XML document.",
    ErrorXMLRpcDocInvalid : "Invalid XML-Rpc document.",
    ErrorXMLMethodResponse : "Malformed XML-Rpc methodResponse document.",
    ErrorQueryStringParamObjectType : "Unable to pass objects as parameters while in making a cross-site request."
  },

  //This acts as a lookup table for the response callback to execute the user-defined
  //   callbacks and to clean up after a request
  pendingRequests: {},
  //Ad hoc cross-site callback functions keyed by request ID; when a cross-site request
  //   is made, a function is created 
  callbacks: {},

  //Takes an array or hash and coverts it into a query string, converting dates to ISO8601
  //   and throwing an exception if nested hashes or nested arrays appear.
  toQueryString : function(params){
  	if(!(params instanceof Object || params instanceof Array) || params instanceof Date)
  	  //'You must supply either an array ,date or object type to convert into a query string. You supplied: ' + params.constructor
  		throw Error({id: cErrorQueryStringParamType, msg: RpcClient.rs.ErrorQueryStringParamType});

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
  						throw Error({id: cErrorQueryStringParamNestedType, msg: RpcClient.rs.ErrorQueryStringParamNestedType});
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
  					throw Error({id: cErrorQueryStringParamObjectType, msg: RpcClient.rs.ErrorQueryStringParamObjectType});
  				else str += encodeURIComponent(String(params[key]));
  			}
  		}
  	}
  	return str;
  },

  //Converts an iterateable value into an array; similar to Prototype's $A function
  toArray : function(value){
  	//if(value && value.length){
  		if(value instanceof Array)
  			return value;
  		var array = [];
  		for(var i = 0; i < value.length; i++)
  			array.push(value[i]);
  		return array;
  	//}
  	//throw Error("Unable to convert to an array the value: " + String(value));
  },

  //Returns an ISO8601 string *in UTC* for the provided date (Prototype's Date.toJSON() returns localtime)
  dateToISO8601 : function(date){
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
  },

  zeroPad : function(value, width){
  	if(!width)
  		width = 2;
  	value = (value == undefined ? '' : String(value))
  	while(value.length < width)
  		value = '0' + value;
  	return value;
  },

  toErrorString: function(e){
    var result = 'ERROR occur, the error';
    switch ($type(e)) {
      case 'object' :
        result += 'id:'+ e['id'] + ' msg:' + e['msg'];
        break;
      case 'string':
        result += ' msg:' + e;
        break;
    };
    return result;
  }
};


RpcClient.Service = new Class({
  RequestCount: 0,
	Implements: [Chain, Events, Options],
  
	options: {
		//url: '',
		data: '',
		headers: {
			'X-Requested-With': 'XMLHttpRequest',
			'Accept': 'text/javascript, text/html, application/xml, text/xml, */*'
		},
		IsAsync: true,
		format: false,
		methods: '',
		link: 'ignore',
		isSuccess: null,
		emulation: true,
		urlEncoded: true,
    Sanitize: true,
    User: null,
    Password: null,
    CallbackParamName: 'JSON-response-callback',
    DateEncoding: 'ISO8601',
    DecodeISO8601: true,
		encoding: 'utf-8',
		evalScripts: false,
		evalResponse: false
	},
  
  initialize: function(serviceURL, options){
		//this.xhr = new Browser.Request();
		this.setOptions(options);
    this.FServiceURL = serviceURL;
    //this.options.url = serviceURL;
		this.options.isSuccess = this.options.isSuccess || this.isSuccess;
		this.headers = new Hash(this.options.headers);
  
  	this.FIsCrossSite = false;
  	var vUrlParts = this.FServiceURL.match(/^(\w+:)\/\/([^\/]+?)(?::(\d+))?(?:$|\/)/);
  	if(vUrlParts){
  		this.FIsCrossSite = (
  			location.protocol !=  vUrlParts[1] ||
  			document.domain   !=  vUrlParts[2] ||
  			location.port     != (vUrlParts[3] || "")
  		);
  	};
		if (!this.options.IsAsync && this.FIsCrossSite)
			throw Error({id: cErrorNeedAsyncForCrossSite, msg: RpcClient.rs.ErrorNeedAsyncForCrossSite});

    var vProvidedMethodList = options.Methods;

  	if(this.FIsCrossSite){
  		if(this.options.Sanitize){
  			throw Error({id: cErrorCanNotSanitized, msg: RpcClient.rs.ErrorCanNotSanitized});
  		}
  	}

  	//Obtain the list of methods made available by the server
  	if(this.FIsCrossSite && !vProvidedMethodList)
  		throw Error({id: cErrorAPINamesRequired, msg: RpcClient.rs.ErrorAPINamesRequired});
  	if(vProvidedMethodList)
  		this.FMethodList = vProvidedMethodList;
  	else {
  		//Introspection must be performed synchronously
  		var async = this.options.IsAsync;
  		this.options.IsAsync = false;
  		this.FMethodList = this.iCallMethod("system.listMethods", []);
  		this.options.IsAsync = async;
  	}
  	this.FMethodList.push('system.listMethods');
  	//this.FMethodList.push('system.describe');
    
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
  				if(call.instance.options.IsAsync){
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
  },

  toSerialize: function(data){
  },

  iCallMethod : function(methodName, httpMethod, params, successHandler, exceptionHandler, completeHandler){
  	RpcClient.RequestCount++;
  	//Verify that successHandler, exceptionHandler, and completeHandler are functions
  	if(this.options.IsAsync){
  		if(successHandler && typeof successHandler != 'function')
  		  //'The asynchronous onSuccess handler callback function you provided is invalid; the value you provided (' + successHandler.toString() + ') is of type "' + typeof(successHandler) + '".'
  			throw Error({id:cErrorSuccessHandler, msg: RpcClient.rs.ErrorAsyncHandler});
  		if(exceptionHandler && typeof exceptionHandler != 'function')
  			//, 'The asynchronous onException handler callback function you provided is invalid; the value you provided (' + exceptionHandler.toString() + ') is of type "' + typeof(exceptionHandler) + '".'
  			throw Error({id:cErrorExceptionHandler, msg: RpcClient.rs.ErrorAsyncHandler});
  		if(completeHandler && typeof completeHandler != 'function')
  			//, 'The asynchronous onComplete handler callback function you provided is invalid; the value you provided (' + completeHandler.toString() + ') is of type "' + typeof(completeHandler) + '".'
  			throw Error({id:cErrorCompleteHandler, msg: RpcClient.rs.ErrorAsyncHandler});
  	}	

  	try {
  		//Assign the provided callback function to the response lookup table
  		if(this.options.IsAsync || this.FIsCrossSite){
  			RpcClient.pendingRequests[String(RpcClient.RequestCount)] = {
  				//method:methodName,
  				onSuccess:successHandler,
  				onException:exceptionHandler,
  				onComplete:completeHandler
  			};
  		}
  			
  		//Asynchronous cross-domain call (JSON-in-Script) -----------------------------------------------------
  		if(this.FIsCrossSite){ //then this.options.IsAsync is implied
  			
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
  						'?' + this.options.CallbackParamName + '=RpcClient.callbacks.r' + (RpcClient.RequestCount);
  			if(params)
  				src += '&' + RpcClient.toQueryString(params);
  			script.setAttribute('src', src);
  			script.setAttribute('id', 'RpcClient' + RpcClient.RequestCount);
  			var head = document.getElementsByTagName('head')[0];
  			RpcClient.pendingRequests[RpcClient.RequestCount].scriptElement = script;
  			head.appendChild(script);
  			
  			return undefined;
  		}
  		//Calls made with XHR ------------------------------------------------------------
  		else {
  			//Obtain and verify the parameters
  			if(params){
  				if(!(params instanceof Object) || params instanceof Date) //JSON-RpcClient 1.1 allows params to be a hash not just an array
  				  // 'When making asynchronous calls, the parameters for the method must be passed as an array (or a hash); the value you supplied (' + String(params) + ') is of type "' + typeof(params) + '".'
  					throw Error({id: cErrorAsyncParamType, msg: RpcClient.rs.ErrorAsyncParamType});
  				//request.params = params;
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
   },
  
	onStateChange: function(){
		if (this.xhr.readyState != 4 || !this.running) return;
		this.running = false;
		this.status = 0;
		$try(function(){
			this.status = this.xhr.status;
		}.bind(this));
		if (this.options.isSuccess.call(this, this.status)){
			this.response = {text: this.xhr.responseText, xml: this.xhr.responseXML};
			this.success(this.response.text, this.response.xml);
		} else {
			this.response = {text: null, xml: null};
			this.failure();
		}
		this.xhr.onreadystatechange = $empty;
	},
  
	isSuccess: function(){
		return ((this.status >= 200) && (this.status < 300));
	},

  isJSON: function(string){ //from Prototype String.isJSON()
      var testStr = string.replace(/\\./g, '@').replace(/"[^"\\\n\r]*"/g, '');
      return (/^[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]*$/).test(testStr);
  },

  __toJSON : function(value){
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
  				switch(this.options.DateEncoding){
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
  },

  __evalJSON: function(json, sanitize){ //from Prototype String.evalJSON()
  	//Remove security comment delimiters
  	json = json.replace(/^\/\*-secure-([\s\S]*)\*\/\s*$/, "$1");
  	var err;
      try {
  		if(!sanitize || RpcClient.isJSON(json))
  			return eval('(' + json + ')');
      }
  	catch(e){err = e;}
      throw new SyntaxError(e.code, 'Badly formed JSON string: ' + json + " ... " + (err ? err.message : ''));
  },

  //This function iterates over the properties of the passed object and converts them 
  //   into more appropriate data types, i.e. ISO8601 strings are converted to Date objects.
  __upgradeValuesFromJSON: function(obj){
  	var matches, useHasOwn = {}.hasOwnProperty ? true : false;
  	for(var key in obj){
  		if(!useHasOwn || obj.hasOwnProperty(key)){
  			//Parse date strings
  			if(typeof obj[key] == 'string'){
  				//ISO8601
  				if(this.options.DecodeISO8601 && (matches = obj[key].match(/^(?:(\d\d\d\d)-(\d\d)(?:-(\d\d)(?:T(\d\d)(?::(\d\d)(?::(\d\d)(?:\.(\d+))?)?)?)?)?)$/))){
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
  }
});

