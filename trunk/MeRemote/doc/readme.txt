MeRemote SDK Library
The MeRemote SDK Library is the remote function and remote object Library for delphi. It's the feature-layer of the MeAOP.
Only Supports the simple types: integer, float, string, set, enumeration.
todo: supports the class type instance(published properties only)?

uMeRemoteUtils:  MeRemote Utils class and funcs.
  * TMeStreamProxy
  * SaveParamsToStream, LoadParamsFromStream

uMeRemoteFormater.pas: not used yet.
uMeRTCClientTransport.pas: abondon
uMeRTCServerTransport.pas: abondon

Server:
uMeRemoteServerFunc: MeRemote Server Function Invoker.
  call service functions.
uMeIndyServerTransport.pas: for Indy 10

Client:
uMeTransport: Abstract MeTransport class for client.
uMeRemoteFuncFeature:  MeRemote Function Client Invoker
uMeIndyClientTransport.pas: for Indy 10