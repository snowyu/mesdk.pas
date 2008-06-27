/**********************************************************\
|                                                          |
| WebBrowser JavaScript json-RPC Client Invoker            |
|                                                          |
| rpc_client.js                                            |
|                                                          |
| Release 1.0.0                                            |
| Copyright (c) 2008 by Riceball LEE                       |
|                                                          |
|                                                          |
| Contributors:                                            |
|  Original inspiration for the design of this             |
|  implementation is from JSON-XML-RPC , jsolait and PHPRPC|
|                                                          |
|                                                          |
| This file may be distributed and/or modified under the   |
| terms of the GNU Lesser General Public License (LGPL)    |
| version 3.0 as published by the Free Software Foundation |
| and appearing in the included file LICENSE.              |
|                                                          |
\**********************************************************/

/* json-RPC Client Invoker for WebBrowser JavaScript
 *
 * Copyright (C) 2008 Riceball LEE
 * Version: 1.0.0
 * LastModified: Jun 27, 2008
 * This library is free.  You can redistribute it and/or modify it.
 */

/*
 * Objects:
 *  RpcError
 *  RpcClient
 *
 * Usage:
 * var rpc = new RpcClient("/app/service", {
 *                         asynchronous: true,   //default: true
 *                         sanitize: true,       //default: true
 *                         methods: ['greet'],   //default: null (synchronous introspection populates)
 *                         protocol: 'JSON-RPC', //default: JSON-RPC
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
 * If you create the service proxy with asynchronous set to false you may execute
 * the previous as follows:
 *
 * try {
 *    var message = service.greet("World");
 *    alert(message);
 * }
 * catch(e){
 *    alert("Unable to greet because: " + e);
 * }
 *
 */

/*
 * public class RpcError
 * You never need to create RpcError object by yourself, 
 * when an error occurred during invoking remote function,
 * this object will be created automatically as the result of callback function.
 */
function RpcError(aErrStr, aErrNo) {

    // public methods

    /*
     * Return the error number.
     */
    this.getNumber = function() {
        return aErrNo;
    }
    /*
     * Return the error message.
     */
    this.getMessage = function() {
        return aErrStr;
    }
    /*
     * Return a string which include the error number and the error message.
     */
    this.toString = function() {
      if typeof(aErrNo) != "undefined") {
        return aErrNo + ":" + aErrStr;
      }
      else
        return aErrStr;
        
    }
}

/* public class RpcClient
 * static encapsulation environment for RpcClient
 */

var RpcClient = (function () {

    return (function() {
        // static private members

        /*
         * to save all PHPRPC clients of one page
         */
        var s_clientList = [];

        /*
         * to save the last remote procedure id
         */
        var s_lastID = 0;

        /*
         * the XMLHttp ActiveX object name cache
         * Only IE use it.
         */
        var s_XMLHttpNameCache = null;

        // static private methods

        /*
         * the function createXMLHttp() modified from
         * http://webfx.eae.net/dhtml/xmlextras/xmlextras.html and
         * http://www.ugia.cn/?p=85
         */
        function createXMLHttp() {
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
            else if (s_XMLHttpNameCache != null) {
                // Use the cache name first.
                 return new ActiveXObject(s_XMLHttpNameCache);
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
                        s_XMLHttpNameCache = MSXML[i];
                        return objXMLHttp;
                    }
                    catch(e) {}
                }
                return null;
            }
        }

        /*
         * create a remote procedure id
         */
        function createID() {
            return s_lastID++;
        }

        /*
         * abort the specified remote procedure on the specified PHPRPC client.
         * clientID is the PHPRPC client id.
         * id is the remote procedure id.
         */
        function abort(clientID, id) {
            if (typeof(s_clientList[clientID]) != "undefined") {
                s_clientList[clientID].abort(id);
            }
        }

        /* You can create a PHPRPC Client object by this constructor in javascript.
         * The username and password can be contained in serverURL for HTTP Basic Authorization,
         * but it is NOT recommended (see also useService method for the recommended usage).
         * If you hope that the PHPRPC Client initialize remote functions without connecting to the PHPRPC Server,
         * you can specify the functions parameter.
         */
        function RpcClient(serviceURL, options) {
            // public methods
            /*
             * Abort all remoting functions and then kill itself.
             */
            this.dispose = function() {
                this.abort();
                s_clientList[m_clientID] = null;
                delete s_clientList[m_clientID];
            }

        }

        // static public methods
        /*
         * This method is for VBScript to create PHPRPC Client object,
         * because VBScript can't use new keyword to create a JavaScript Object.
         * but you can use this method in JavaScript too.
         */
        RpcClient.create = function(serviceURL, functions) {
            if (typeof(functions) == "undefined") {
                functions = null;
            }
            return new RpcClient(serviceURL, functions);
        }

        // static public method but not supplied to the developer
        RpcClient.__getClient = function(clientID) {
            return s_clientList[clientID];
        }

        return RpcClient;
    })();
})();