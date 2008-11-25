/** 
@author Riceball LEE(riceballl@hotmail.com)
*/
/** helper function for inject */
function Arguments(args) {
  //convert arguments object to array
  this.value = [].slice.call(args);
};

(function sdInit() {
  var gCacheFriends = new Hash();
  //var vJsonRequest = new Request.JSON(CallbackParamName:'callback');
  var cErrorFunctionTypeNeed = 1;
  var _Service = new Class({
      Implements: [Chain, Events, Options],
      options: {
      },
      initialize: function(options){
    		this.setOptions(options);
    	},

    	success: function(){
    		this.onSuccess(arguments);
    	},

    	onSuccess: function(){
    		this.fireEvent('complete', arguments).fireEvent('success', arguments).callChain();
    	},

    	failure: function(aError){
    		this.onFailure(aError);
    	},

    	onFailure: function(){
    		this.fireEvent('complete', arguments).fireEvent('failure', arguments).callChain();
    	}
  }); //*/

  window.SNDA =
  {
    //var vLastPtId;
    //var vDemoService;
    //var vPtId = Browser.getQueryStringValue("ptid");

    isDebugged: false,
    serviceUrl: 'http://dev.api.sdo.com',
    appKey: '',
  	/**
             可以被国际化的字符串资源,。
             @constant
          */
    rs: {
      ErrorFunctionTypeNeed : "the type should be function type."
  	},
    getIsDebugged: function() {
      return (Browser.Engine=='gecko' && SNDA.isDebugged);
    },

    expectFunctionParam: function (aParam) {
      if (aParam) {
        if ($type(aParam) != 'function') {
          throw Error({code: cErrorFunctionTypeNeed, message: rs.ErrorFunctionTypeNeed});
          return false;
        }
        else
          return true;
      }
      else
        return false;
    },

    zeroPad: function(value, width){
    	if(!width)
    		width = 2;
    	value = (value == undefined ? '' : String(value))
    	while(value.length < width)
    		value = '0' + value;
    	return value;
    },

    //CONVERT the string to date.
    strToDate : function(aDateStr){
			//Parse date strings
			if($type(aDateStr) == 'string'){
				//try ISO8601(UTC)
        var matches;
				if (matches = aDateStr.match(/^(\d\d\d\d)(?:-)?(\d\d)(?:-)?(\d\d)(?:(?:T)?(\d\d)(?::)?(\d\d)(?::)?(\d\d)(\.\d+)?)?(Z|([+-])(\d\d)(?::)?(\d\d))?$/)){
					aDateStr = new Date(0);
					aDateStr.setUTCFullYear(parseInt(matches[1]));
					aDateStr.setUTCMonth(parseInt(matches[2]-1));
					aDateStr.setUTCDate(parseInt(matches[3]));
					if(matches[4]) aDateStr.setUTCHours(parseInt(matches[4]));
					if(matches[5]) aDateStr.setUTCMinutes(parseInt(matches[5]));
          if(matches[6]) aDateStr.setUTCSeconds(parseInt(matches[6]));
          if(matches[7]) aDateStr.setUTCMilliseconds(parseFloat(matches[7] * 1000));
          if(matches[8] && matches[8] != "Z") {
            var offset = (matches[10] * 60) + parseInt(matches[11]);  
            offset *= ((matches[9] == '-') ? -1 : 1);  
            aDateStr.setTime(aDateStr.getTime() - offset * 60 * 1000);   //*/
          }
				}
				//@timestamp@ / @ticks@
				else if(matches = aDateStr.match(/^@(\d+)@$/)){
					aDateStr = new Date(parseInt(matches[1]))
				}
				//ASP.NET
				else if(matches = aDateStr.match(/^\/Date\((\d+)\)\/$/)){
					aDateStr = new Date(parseInt(matches[1]))
				}
			}
      return aDateStr;
    },

    //Returns an ISO8601 string *in UTC* for the provided date (Prototype's Date.toJSON() returns localtime)
    dateToStr : function(date){
    	return date.getUTCFullYear()             + '-' +
    	     SNDA.zeroPad(date.getUTCMonth()+1) + '-' +
    		   SNDA.zeroPad(date.getUTCDate())    + 'T' +
    	     SNDA.zeroPad(date.getUTCHours())   + ':' +
    		   SNDA.zeroPad(date.getUTCMinutes()) + ':' +
    		   SNDA.zeroPad(date.getUTCSeconds()) + '.' +
    		   //Prototype's Date.toJSON() method does not include milliseconds
    		   SNDA.zeroPad(date.getUTCMilliseconds(), 3)+"Z";
    },

    __upgradeValuesFromJSON : function(obj){
      if ($type(obj) == "object") {
        for (var key in obj) {
          if ($type(obj[key]) == "string") 
            obj[key] = SNDA.strToDate(obj[key]);
          else if ($type(obj[key]) == "object" || $type(obj[key]) == "array") SNDA.__upgradeValuesFromJSON(obj[key]);
        }
      }
      else if ($type(obj) == "array") {
        obj.each(function(item, index){
           if ($type(obj[key]) == "string") 
             obj[key] = SNDA.strToDate(obj[key]);
           else if ($type(item) == "object" || $type(item) == "array") SNDA.__upgradeValuesFromJSON(item);
        })
      }
    },

    Activities: new Class({
      /*Implements: [Chain, Events, Options],
      options: {
      },
      initialize: function(options){  
    		this.setOptions(options);
      }, //*/

      Extends: _Service,
      /** 
                   get the public activities of the ptId by async
                   @param  aPitId  the activities of the ptId
                   @param  aSource [optional] only get public activities of  the specified source if any
                   @param  aOnComplete the callback event when async over.
                */
      get: function(aPtId, aOnComplete, aSource){
         if (SNDA.expectFunctionParam(aOnComplete)) {
            var vActivities = false;
            var vUrl = window.SNDA.serviceUrl + '/sdo/users/' + aPtId + '/activities?key=' + aSource;
            var vJsonRequest = new Request.JSON({
              url: vUrl, 
              onComplete: function(aResult){
                if (aResult["error"]) {
                  aOnComplete(aResult)
                }
                else {
                  vActivities = aResult["result"];
                  if (vActivities) {
                    //gCacheFriends.include(vActivities.ptId, vActivities);
                    SNDA.__upgradeValuesFromJSON(vActivities);
                    aOnComplete(vActivities);
                  }
                }
              }
            });
            vJsonRequest.get();
         }
      },
      //get the firends' activities of the ptId by async
      getFriends: function(aPtId, aOnComplete, aSource){
         if (SNDA.expectFunctionParam(aOnComplete)) {
            var vActivities = false;
            var vUrl = window.SNDA.serviceUrl + '/sdo/users/' + aPtId + '/friends/activities?key=' + aSource;
            var vJsonRequest = new Request.JSON({
              url: vUrl, 
              onComplete: function(aResult){
                if (aResult["error"]) {
                  aOnComplete(aResult)
                }
                else {
                  vActivities = aResult["result"];
                  if (vActivities) {
                    __upgradeValuesFromJSON(vActivities);
                    aOnComplete(vActivities);
                  }
                }
              }
            });
            vJsonRequest.get();
         }
      },
      //add a new activity of the PtId by async
      add: function(aPtId, aOnComplete, aContent, aTitle, aType, aContentType, aNickname, aWorld, aProtectLevel, aPrior){
         if (SNDA.expectFunctionParam(aOnComplete)) {
            var vNewAppId = false;
            var vUrl = window.SNDA.serviceUrl + '/sdo/users/'+ aPtId +'/activities/add?content='+escape(aContent)+'&type='+aType+'&key='+SNDA.appKey;
            if (aTitle)
              vUrl += '&title='+escape(aTitle);
            if (aContentType)
              vUrl += '&contentType='+aContentType;
            if (aProtectLevel)
              vUrl += '&protectLevel='+aProtectLevel;
            if (aPrior)
              vUrl += '&prior='+aPrior;
            if (aNickname)
              vUrl += '&nickname='+escape(aNickname);
            if (aWorld)
              vUrl += '&world='+aWorld;
            var vJsonRequest = new Request.JSON({
              url: vUrl, 
              onComplete: function(aResult){
                if (aResult["error"]) {
                  aOnComplete(aResult)
                }
                else {
                  var vNewAppId = aResult["result"];
                  if (vNewAppId) {
                    aOnComplete(vNewAppId);
                  }
                }
              }
            });
            vJsonRequest.get();
         }
      }
    }),

  /**
                  封装 SNDA OpenAPI  Friends RPC 调用，通过异步方式实现获取指定PtId（盛大通行证数字帐号）的应用好友。
                  可以缓存获取的指定PtId（盛大通行证数字帐号）的应用好友列表。
             @name Friends
             @class  Friends 请求的功能包装类
                  //extends  Chain, Event, Options
             @example
                SNDA.serviceUrl = 'http://dev.api.sdo.com';
                SNDA.appKey = "55D2CAD969DD019D993F4471132A4B64";
                var vFriends = new SNDA.Friends();
                vFriends.get(aPtId, doFriendsComplete);
              function doFriendsComplete(aResult){
                var vError = "";
                try{
                if (aResult["error"]){
                  vError = JSON.encode(aResult["error"]);
                } else {
                  if (aResult.count > 0) {
                    aResult.apps.each(function(aApp, aAppIndex){
                      aApp.friends.each(function(aFriend, aFriendIndex){
                        var s = '<a href="#" onclick="javascript:execute('+aFriend.ptId+')">';
                        if (aApp.logoUrl) s = s + '<img src="' + aApp.logoUrl + '" />';
                        s = s + aFriend.nickname+"</a>";
                        c = aFriend.friendCount;
                        if (c == null) c = 0;
                      })
                    });
                  }
                  else {
                    vError = "似乎这家伙一个好友也没有！";
                  }
                }
              }
                catch(e){
                   vError = JSON.encode(e);
                  	//$("result").set('text', "Unable to execute because: " + JSON.encode(e));
                }
              }
        */
    Friends: new Class({
      /*Implements: [Chain, Events, Options],
      initialize: function(options){  
    		this.setOptions(options);
      }, //*/
      Extends: _Service,
      options: {
        cacheable: true
      },
      //get the friends of the ptId by async
      get: function(aPtId, aOnComplete){
          if (gCacheFriends.has(aPtId)){
            var vResult = gCacheFriends.get(aPtId);

            //if (getIsDebugged) console.log('getFriends:'+'('+aPtId+')'+' hit cache');

            if (aOnComplete) aOnComplete(vResult);
            return vResult;
          }
          if (aOnComplete) {
            if ($type(aOnComplete) != 'function')
              throw Error({code: cErrorFunctionTypeNeed, message: rs.ErrorFunctionTypeNeed});
            var vFriends = false;
            var vUrl = window.SNDA.serviceUrl + '/sdo/users/' + aPtId + '/friends?key=' + SNDA.appKey;
            var vJsonRequest = new Request.JSON({
              url: vUrl, 
              onComplete: function(aResult){
                if (aResult["error"]) {
                  aOnComplete(aResult)
                }
                else {
                  vFriends = aResult["result"];
                  if (vFriends) {
                    if (this.options.cacheable) {
                      gCacheFriends.include(vFriends.ptId, vFriends); 
                      //console.log(vFriends.ptId+ ' cached')
                    } 
                    //else
                      //console.log(vFriends.ptId+ ' not cached');
                    aOnComplete(vFriends);
                  }
                }
              }
            });
            vJsonRequest.get();
          }
      }
    }),
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
    Inject: function ( aOrgFunc, aBeforeExec, aAtferExec ) {
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
    }
  }

})();
