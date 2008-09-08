/*
Script: Request.js
@fileOverview Powerful all purpose Request Class. Uses XMLHTTPRequest and  Cross-Site Script.
@Author:   original come from mootools 1.2
@Author:   Riceball LEE(riceballl@hotmail.com )
@version   1.0.0.0

License:
	MIT-style license.


*/

/**@constant */
var cErrorNeedAsyncForCrossSite = 3;
/**@constant */
var cErrorCrossSiteSupportGetMethodOnly = 4;
/**@constant */
var cErrorResponseObject    = 10;
/**@constant */
var cErrorResponseIdMissed  = 11;
/**@constant */
var cErrorResponseIdUnkonwn = 12;
/**@constant */
var cErrorReturnStatusCode  = 13;
/**@constant */
var cErrorCanNotSetRequestHeader = 14;


//var RequestCount = 0;

/**
          实现了对 Http 的GET, POST, PUT, DELETE 请求的功能包装.
          通过Cross-Site Scrpit实现跨域请求的支持。

          @name Request
          @class  Http 请求的功能包装类
          //extends  Chain, Event, Options
          @example
 var vRequest = new Request({url: vServiceURL, CallbackParamName:'callback'
   , onComplete: function(aResult){
  $("result").set('text', JSON.encode(aResult));
  }});
          @example
 var vRequest = new Request({url: vServiceURL, CallbackParamName:'callback' });
  function onComplete(aResult){
          $("result").set('text', JSON.encode(aResult));
  }
  vRequest.addEvent('complete', onComplete);
	@events
	  onException, onComplete, onFailure
*/
var Request = new Class({

  Implements: [Chain, Events, Options],

	/**
       (object)请求的参数数据
	*/
	options: {
	/**
		       (string: 默认为 null))待请求的URL.
	*/
		url: '',
	/**
	*/
		data: '',
	/**
		(object) 请求发送的HTTP 报头数据
	*/
		headers: {
			'X-Requested-With': 'XMLHttpRequest',
			'Accept': 'text/javascript, text/html, application/xml, text/xml, */*'
		},
    	//XSS hack ----------
	/**
		   (string: 默认为 "callback"))回调函数名
	*/
   	CallbackParamName: 'callback',
    	//XSS hack ----------
	/**
		(boolean 默认为 true)是否异步执行调用（跨域总是异步，而且跨域只能是GET）
	*/
		async: true,
	/**
           string: 默认为 'post') HTTP请求方法, 可以是: get, post, put, delete
	*/
		method: 'post',
	/**
	*/
		link: 'ignore',
	/**
           (function) 可覆盖内置的isSuccess函数,可自定义请求成功的规则
		   isSuccess(this, this.Status): Boolean
	*/
		isSuccess: null,
	/**
	*/
		emulation: false,
	/**
	*/
		urlEncoded: true,
	/**
	*/
		encoding: 'utf-8',
	/**
	*/
		evalScripts: false,
		evalResponse: false
	},

	/*
           本类中可以被国际化的字符串资源,。
           @constant
	*/
  rs: {
    ErrorNeedAsyncForCrossSite : "It is not possible to establish a synchronous connection to a cross-site request.",
    ErrorCrossSiteSupportGetMethodOnly : 'the cross-site request supports the http GET method only.',
    ErrorResponseObject : "The server did not respond with a response object.",
    ErrorResponseIdMissed : "The server did not respond with the required response id for asynchronous calls.",
    ErrorResponseIdUnkonwn : "Fatal error with Request code: no such ID found in pendingRequests.",
	ErrorReturnStatusCode : 'Fatal Return Status Code Error:',
	ErrorCanNotSetRequestHeader : 'Error Can not Set RequestHeader'
	},

	/** 
     @construct 
	@memberOf Request
    */
  initialize: function(options){
		this.setOptions(options);
		this.options.isSuccess = this.options.isSuccess || this.isSuccess;
		this.headers = new Hash(this.options.headers);

  	//XSS hack----------------------------------
    this.FIsCrossSite = false;
  	var vUrlParts = this.options.url.match(/^(\w+:)\/\/([^\/:]+?)(?::(\d+))?(?:$|\/)/);
  	if(vUrlParts){
  		this.FIsCrossSite = (
  			location.protocol !=  vUrlParts[1] ||
  			document.domain   !=  vUrlParts[2] ||
  			location.port     != (vUrlParts[3] || "")
  		);
  	};
    //console.log(this.FIsCrossSite);
		if (!this.options.async && this.FIsCrossSite)
			throw Error({code: cErrorNeedAsyncForCrossSite, message: this.rs.ErrorNeedAsyncForCrossSite});
    //XSS hack -------------------------------

		this.xhr = new Browser.Request();
	}
});
//XSS hack ----------
//add the static field.
/** 类变量 请求发生的次数  */
Request['Count'] = 0;
Request['pendingRequests'] = {};
Request['callbacks'] = {};
//XSS hack ----------

//Request 类实现的方法
Request.implement({
	onStateChange: function(){
		if (this.xhr.readyState != 4 || !this.running) return;
		this.running = false;
		this.status = 0;
		$try(function(){
			this.status = this.xhr.status;
		}.bind(this));
		try{
			if (this.options.isSuccess.call(this, this.status)){
				this.response = {text: this.xhr.responseText, xml: this.xhr.responseXML};
				this.success(this.response.text, this.response.xml);
			} else {
				this.response = {text: null, xml: null};
				this.failure({code: cErrorReturnStatusCode, message: this.rs.ErrorReturnStatusCode + this.status, object: this.xhr});
			}
		}
		catch(e) {
		  this.failure(e);
		  //this.fireEvent('exception', e);
		}
		this.xhr.onreadystatechange = $empty;
	},

	isSuccess: function(){
	    if ((this.status >= 200) && (this.status < 300))
		  return true
		else {
		  throw Error({code: cErrorReturnStatusCode, message: this.rs.ErrorReturnStatusCode + this.status});
		  return false;
		}

	},

	processScripts: function(text){
		if (this.options.evalResponse || (/(ecma|java)script/).test(this.getHeader('Content-type'))) return $exec(text);
		return text.stripScripts(this.options.evalScripts);
	},

	success: function(text, xml){
		this.onSuccess(this.processScripts(text), xml);
	},

	onSuccess: function(){
     //console.log('OnSuccess: %s',JSON.encode(b))
		this.fireEvent('complete', arguments).fireEvent('success', arguments).callChain();
	},

	failure: function(aError){
		this.onFailure(aError);
	},

	onFailure: function(){
		this.fireEvent('complete', arguments).fireEvent('failure', arguments);
	},

	setHeader: function(name, value){
		this.headers.set(name, value);
		return this;
	},

	getHeader: function(name){
		return $try(function(){
			return this.xhr.getResponseHeader(name);
		}.bind(this));
	},

	check: function(caller){
		if (!this.running) return true;
		switch (this.options.link){
			case 'cancel': this.cancel(); return true;
			case 'chain': this.chain(caller.bind(this, Array.slice(arguments, 1))); return false;
		}
		return false;
	},

	toSerialize: function(data, method){
		switch ($type(data)){
			case 'element': data = $(data).toQueryString(); break;
			case 'object':
			case 'hash':
			  data = Hash.toQueryString(data, method);
		}

		if (this.options.emulation && ['put', 'delete'].contains(method)){
			var _method = '_method=' + method;
			data = (data) ? _method + '&' + data : _method;
			method = 'post';
		}

		if (this.options.urlEncoded && method == 'post'){
			var encoding = (this.options.encoding) ? '; charset=' + this.options.encoding : '';
			this.headers.set('Content-type', 'application/x-www-form-urlencoded' + encoding);
		}

	  return data;
	},

  XRSSend: function (url, method, data) {
    this.xhr.open(method.toUpperCase(), url, this.options.async);

    this.xhr.onreadystatechange = this.onStateChange.bind(this);

    this.headers.each(function(value, key){
      if (!$try(function(){
        this.xhr.setRequestHeader(key, value);
        return true;
      }.bind(this))) this.failure({code: cErrorCanNotSetRequestHeader, message: ErrorCanNotSetRequestHeader+ ' Key:'+ key +  ' value:'+ value});//this.fireEvent('exception', {code: cErrorCanNotSetRequestHeader, message: ErrorCanNotSetRequestHeader+ ' Key:'+ key +  ' value:'+ value});
    }, this);

    this.fireEvent('request');
    this.xhr.send(data);
    if (!this.options.async) this.onStateChange();
  },

  /**
         @function
         @name Send
         @param options (object) the send data or options.
        @memberOf Request
        @example
        Send({data: someData, url:'api.sdo.com'})
        Send(someData);
        */
  send: function(options){

		if (!this.check(arguments.callee, options)) return this;

    //XSS hack -------------------------------
		if (this.FIsCrossSite && options.method != 'get')
			throw Error({code: cErrorCrossSiteSupportGetMethodOnly, message: this.rs.ErrorCrossSiteSupportGetMethodOnly});
    //XSS hack -------------------------------

		this.running = true;
    Request.Count++;

		var type = $type(options);
		if (type == 'string' || type == 'element') options = {data: options};

		var old = this.options;
		options = $extend({data: old.data, url: old.url, method: old.method}, options);
		var data = options.data, url = options.url, method = options.method;
    if (data.params) {
      if (data.params.CallbackParamName) delete data.params.CallbackParamName;
      if (!data.params.id) data.params.id = Request.Count;
    }
    //console.log('data.CallbackParamName=%s, exists=%s', data['CallbackParamName'], data.hasOwnProperty('CallbackParamName'));
    //console.log('data type =%s', $type(data));

    //console.log('data:%s', JSON.encode(data));
    data = this.toSerialize(data, method);
    //console.log('data:%s', data);

		if (data && method == 'get'){
			url = url + (url.contains('?') ? '&' : '?') + data;
			data = null;
		}

		//XSS hack ----------------------------------
    if (this.FIsCrossSite) {
  		this.fireEvent('request');
      //Create an ad hoc function specifically for this cross-site request
      Request.callbacks['r' + String(Request.Count)] = (function(instance, id){
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
      })(this, Request.Count);

      //Make the request by adding a SCRIPT element to the page
      var script = document.createElement('script');
      script.setAttribute('type', 'text/javascript');
			url = url + (url.contains('?') ? '&' : '?') + this.options.CallbackParamName + '=Request.callbacks.r' + (Request.Count);
      script.setAttribute('src', url);
      script.setAttribute('id', 'RpcClient' + Request.Count);
      var head = document.getElementsByTagName('head')[0];
      Request.pendingRequests[Request.Count] = script;
      head.appendChild(script);
    }
    else {
      this.XRSSend(url, method, data);
    }
		//XSS hack ----------------------------------

		return this;
	},

	cancel: function(){
		if (!this.running) return this;
		this.running = false;
    if (this.FIsCrossSite) {
    }
		this.xhr.abort();
		this.xhr.onreadystatechange = $empty;
		this.xhr = new Browser.Request();
		this.fireEvent('cancel');
		return this;
	},

  __doCallback : function(response){
	  this.running = false;
    this.status = 0;

  	if(typeof response != 'object')
  		throw Error({code: cErrorResponseObject, message: rsErrorResponseObject});
  	if(!response.id)
  		throw Error({code: cErrorResponseIdMissed,message: rsErrorResponseIdMissed});

  	if(!Request.pendingRequests[response.id])
  	  //'Fatal error with RpcClient code: no such ID "' + response.id + '" found in pendingRequests.'
  		throw Error({code: cErrorResponseIdUnkonwn, message: rsErrorResponseIdUnkonwn});

  	//Remove the SCRIPT element from the DOM tree for cross-site (JSON-in-Script) requests
  	if(Request.pendingRequests[response.id]){
  		var script = Request.pendingRequests[response.id];
  		script.parentNode.removeChild(script);
  	}
  	//Remove the ad hoc cross-site callback function
  	if(Request.callbacks[response.id])
  		delete Request.callbacks['r' + response.id];

  	var uncaughtExceptions = [];

  	//Handle errors returned by the server
  	if(response.error !== undefined){
  		var err = new Error(response.error.message);
  		err.code = response.error.code;
  		//err.locationCode = SERVER
  		if(Request.pendingRequests[response.id].onException){
  			try{
  				if(!Request.pendingRequests[response.id].onException(err))
  					uncaughtExceptions.push(err);
  			}
  			catch(err2){ //If the onException handler also fails
  				uncaughtExceptions.push(err);
  				uncaughtExceptions.push(err2);
  			}
  		}
  		else uncaughtExceptions.push(err);
  	}
  	else {
    	//Process the valid result
      //console.log("success!");
			this.response = {text: response, xml: response};
 			this.success(response);
      //console.log("The success response(%d): %s", requestId, JSON.encode(response));
  	}

  	delete Request.pendingRequests[response.id];

  	//Merge any exception raised by onComplete into the previous one(s) and throw it
  	if(uncaughtExceptions.length){
			this.response = {text: null, xml: null};
			this.failure();

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
      //console.log("The err response(%d): %s", requestId, message);
  		throw err;
  	}
  }

});

(function(){
  var methods = {};
  ['get', 'post', 'put', 'delete', 'GET', 'POST', 'PUT', 'DELETE'].each(function(method){
  	methods[method] = function(){
  		var params = Array.link(arguments, {url: String.type, data: $defined});
  		return this.send($extend(params, {method: method.toLowerCase()}));
  	};
  });

  Request.implement(methods);
})();

Element.Properties.send = {
	set: function(options){
		var send = this.retrieve('send');
		if (send) send.cancel();
		return this.eliminate('send').store('send:options', $extend({
			data: this, link: 'cancel', method: this.get('method') || 'post', url: this.get('action')
		}, options));
	},

	get: function(options){
		if (options || !this.retrieve('send')){
			if (options || !this.retrieve('send:options')) this.set('send', options);
			this.store('send', new Request(this.retrieve('send:options')));
		}
		return this.retrieve('send');
	}

};

Element.implement({

	send: function(url){
		var sender = this.get('send');
		sender.send({data: this, url: url || sender.options.url});
		return this;
	}

});


/*
Script: Request.HTML.js
	Extends the basic Request Class with additional methods for interacting with HTML responses.

License:
	MIT-style license.
*/

Request.HTML = new Class({

	Extends: Request,

	options: {
		update: false,
		evalScripts: true,
		filter: false
	},

	processHTML: function(text){
		var match = text.match(/<body[^>]*>([\s\S]*?)<\/body>/i);
		text = (match) ? match[1] : text;

		var container = new Element('div');

		return $try(function(){
			var root = '<root>' + text + '</root>', doc;
			if (Browser.Engine.trident){
				doc = new ActiveXObject('Microsoft.XMLDOM');
				doc.async = false;
				doc.loadXML(root);
			} else {
				doc = new DOMParser().parseFromString(root, 'text/xml');
			}
			root = doc.getElementsByTagName('root')[0];
			for (var i = 0, k = root.childNodes.length; i < k; i++){
				var child = Element.clone(root.childNodes[i], true, true);
				if (child) container.grab(child);
			}
			return container;
		}) || container.set('html', text);
	},

	success: function(text){
		var options = this.options, response = this.response;

		response.html = text.stripScripts(function(script){
			response.javascript = script;
		});

		var temp = this.processHTML(response.html);

		response.tree = temp.childNodes;
		response.elements = temp.getElements('*');

		if (options.filter) response.tree = response.elements.filter(options.filter);
		if (options.update) $(options.update).empty().adopt(response.tree);
		if (options.evalScripts) $exec(response.javascript);

		this.onSuccess(response.tree, response.elements, response.html, response.javascript);
	}

});

Element.Properties.load = {

	set: function(options){
		var load = this.retrieve('load');
		if (load) send.cancel();
		return this.eliminate('load').store('load:options', $extend({data: this, link: 'cancel', update: this, method: 'get'}, options));
	},

	get: function(options){
		if (options || ! this.retrieve('load')){
			if (options || !this.retrieve('load:options')) this.set('load', options);
			this.store('load', new Request.HTML(this.retrieve('load:options')));
		}
		return this.retrieve('load');
	}

};

Element.implement({

	load: function(){
		this.get('load').send(Array.link(arguments, {data: Object.type, url: String.type}));
		return this;
	}

});


/*
Script: Request.JSON.js
	Extends the basic Request Class with additional methods for sending and receiving JSON data.

License:
	MIT-style license.
*/

Request.JSON = new Class({

	Extends: Request,

	options: {
		secure: true
	},

	initialize: function(options){
		this.parent(options);
		this.headers.extend({'Accept': 'application/json', 'X-Request': 'JSON'});
	},

	toSerialize: function(data, method){
    switch (method) {
      case 'post':
  	    return JSON.encode(data);
      case 'get':
        //alert(method+':' + data);
        //alert(this.toQueryString(data));
  	    return this.toQueryString(data);
      default :
  	    return this.parent(data, method);
    }

		if (this.options.emulation && ['put', 'delete'].contains(method)){
			var _method = '_method=' + method;
			data = (data) ? _method + '&' + data : _method;
			method = 'post';
		}

		if (this.options.urlEncoded && method == 'post'){
			var encoding = (this.options.encoding) ? '; charset=' + this.options.encoding : '';
			this.headers.set('Content-type', 'application/x-www-form-urlencoded' + encoding);
		}

	},

	success: function(text){
    //console.log('Success: %s',JSON.encode(text))
    if (!text instanceof Object)
		  this.response.json = JSON.decode(text, this.options.secure)
    else
      this.response.json = text;
		this.onSuccess(this.response.json, text);
	},

	toQueryString: function(data){
		var queryString = [];
		Hash.each(data, function(value, key){
			//if (data) key = data + '[' + key + ']';
      //alert('key='+key+' value=' + value);
			var result;
			switch ($type(value)){
				case 'object':
          result = Hash.toQueryString(value);
          break;
				case 'array':
					var qs = {};
					value.each(function(val, i){
						qs[i] = val;
					});
					result = Hash.toQueryString(value);
				  break;
				default:
          result = key + '=' + encodeURIComponent(value);
			}
			if (value != undefined) queryString.push(result);
		});

		return queryString.join('&');
	},

  toArray: function(value){
  	//if(value && value.length){
  		if(value instanceof Array)
  			return value;
  		var array = [];
  		for(var i = 0; i < value.length; i++)
  			array.push(value[i]);
  		return array;
  	//}
  	//throw Error("Unable to convert to an array the value: " + String(value));
  }

});


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

