(function() {
  var gCacheFriends = new Hash();
  //var vJsonRequest = new Request.JSON(CallbackParamName:'callback');
  var cErrorFunctionTypeNeed = 1;
  

  window.SNDA =
  {
    //var vLastPtId;
    //var vDemoService;
    //var vPtId = Browser.getQueryStringValue("ptid");

    serviceUrl: 'http://dev.api.sdo.com',
    appKey: '55D2CAD969DD019D993F4471132A4B64',
  	/*
             可以被国际化的字符串资源,。
             @constant
          */
    rs: {
      ErrorFunctionTypeNeed : "the type should be function type.",
  	},

    Friends: new Class({
      Implements: [Chain, Events, Options],
      options: {
        cacheable: true
      },
      /** 
                 @construct 
          	     @memberOf Friends
               */
      initialize: function(options){  
    		this.setOptions(options);
      },

      //get the friends of the ptId by async
      get: function(aPtId, aOnComplete){
          if (gCacheFriends.has(aPtId)){
            var vResult = gCacheFriends.get(aPtId);
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
                    gCacheFriends.include(vFriends.ptId, vFriends);
                    aOnComplete(vFriends);
                  }
                }
              }
            });
            vJsonRequest.get();
          }
      }
    }),
  }

})();
