

{Summary The RTC MeTransport class.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.00 $

  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeTransport.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeRTCTransport;

interface

{$I Setting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeObject
  , uMeTransport
  ;

type
  TMeRTCTransport = class(TMeTransport)
  protected
    procedure DataProviderCheckRequest(Sender: TRtcConnection);
    procedure DataProviderDisconnect(Sender: TRtcConnection);
    procedure DataProviderListenStart(Sender: TRtcConnection);
    procedure DataProviderListenStop(Sender: TRtcConnection);
    procedure DataProviderDataReceived(Sender: TRtcConnection);

    procedure iSendAsyn(const aRequest: TStream; const aReply: TStream; const aTimeOut: Integer = 0);virtual;abstract;
    procedure iSend(const aRequest: TStream; const aReply: TStream);virtual;abstract;
  public
    DataProvider: TRtcDataProvider;

    constructor Create();
    destructor Destroy(); override;
  end;

implementation
var
  FMyProvider: TMyProvider;

function GetDataProvider:TMyProvider;
begin
  if not assigned(FMyProvider) then
    FMyProvider:=TMyProvider.Create();
  Result:=FMyProvider;
end;


procedure TMyProvider.DataProviderListenStart(Sender: TRtcConnection);
  begin
  try
    FReady:=True;
    XLog('MyProvider Ready.');
  except
    on E:Exception do
      XLog('Error starting MyProvider: '+E.Message);
    end;
  end;

procedure TMyProvider.DataProviderListenStop(Sender: TRtcConnection);
  begin
  if FReady then
    begin
    try
    except
      on E:Exception do
        XLog('Error shuting down MyProvider: '+E.Message);
      end;
    FReady:=False;
    end;
  end;

procedure TMyProvider.DataProviderCheckRequest(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do
    //use the Query property to get the query part in the url.
    //if UpperCase(Request.FileName)='/TIME' then
    with Request do if (Method='GET') or (Method='POST') or (Method='HEAD') then
      Accept;
end;

procedure TMyProvider.DataProviderDataReceived(Sender: TRtcConnection);
  begin
  // start processing when complete request body was received
  with Sender as TRtcDataServer do
    if Request.Complete then
    begin
      if UpperCase(Request.FileName)='/TIME' then
        Write('Current time is: '+TimeToStr(Now))
      else if UpperCase(Request.FileName)='/DATE' then
        Write('Current date is: '+DateToStr(Now))
      else
        Write(Request.Method +' Unknown cmd:' + Request.FileName)
    end;
  end;

procedure TMyProvider.DataProviderDisconnect(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.DataSize>Request.DataIn then
      begin
      // did not receive a complete request
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while receiving a Request ('+IntToStr(Request.DataIn)+' of '+IntToStr(Request.DataSize)+' bytes received).');
      end
    else if Response.DataSize>Response.DataOut then
      begin
      // did not send a complete result
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' -'+IntToStr(Response.DataSize-Response.DataOut)+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while sending a Result ('+IntToStr(Response.DataOut)+' of '+IntToStr(Response.DataSize)+' bytes sent).');
      end;
    end;
  end;

constructor TMyProvider.Create();
begin
  inherited;
  DataProvider := TRtcDataProvider.Create(nil);
  with DataProvider do
  begin
    OnListenStart := DataProviderListenStart;
    OnListenStop := DataProviderListenStop;
    OnCheckRequest := DataProviderCheckRequest;
    OnDataReceived := DataProviderDataReceived;
    OnDisconnect := DataProviderDisconnect;
  end;
  
end;

destructor TMyProvider.Destroy();
begin
  DataProvider.Free;
  inherited;
end;


initialization
finalization

  if assigned(FMyProvider) then
  begin
    FMyProvider.Free;
    FMyProvider:=nil;
  end;
end.


(*
unit uMyServer;

interface

uses
  SysUtils
  , rtcLog, rtcSyncObjs
  , rtcInfo, rtcConn, rtcThrPool
  , rtcDataSrv, rtcHttpSrv
  ;

type
  TMyServer = class
  protected
    FOnError: TRtcErrorEvent;
    FOnStart: TRtcNotifyEvent;
    FOnStop: TRtcNotifyEvent;
    FOnConnect: TRtcNotifyEvent;
    FOnDisconnect: TRtcNotifyEvent;
    FLogEnabled: Boolean;

    CS:TRtcCritSec;
    CliCnt:integer;

    function GetClientCount: integer;

    procedure SetLogEnabled(const Value: Boolean);

    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure ServerDisconnecting(Sender: TRtcConnection);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerInvalidRequest(Sender: TRtcConnection);
    procedure ServerDisconnect(Sender: TRtcConnection);
  public
    Server: TRtcHttpServer;

    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property ClientCount:integer read GetClientCount;
    property LogEnabled: Boolean read FLogEnabled write SetLogEnabled;

    property OnStart:TRtcNotifyEvent read FOnStart write FOnStart;
    property OnStop:TRtcNotifyEvent read FOnStop write FOnStop;
    property OnError:TRtcErrorEvent read FOnError write FOnError;
    property OnConnect:TRtcNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

implementation
  
{ TMyServer }
constructor TMyServer.Create;
begin
  inherited;
  StartLog;

  CS := TRtcCritSec.Create;
  Server := TRtcHttpServer.Create(nil);
  with Server do
  begin
    MultiThreaded := True;
    Timeout.AfterConnecting := 60;
    ServerPort := '80';
    OnConnecting := ServerConnecting;
    OnDisconnecting := ServerDisconnecting;
    OnDisconnect := ServerDisconnect;
    RestartOn.ListenLost := True;
    OnListenStart := ServerListenStart;
    OnListenStop := ServerListenStop;
    OnListenError := ServerListenError;
    OnRequestNotAccepted := ServerRequestNotAccepted;
    MaxRequestSize := 128000;
    MaxHeaderSize := 16000;
    OnInvalidRequest := ServerInvalidRequest;
  end;

end;

destructor TMyServer.destroy; 
begin
  CS.Free;
  FreeAndNil(Server);
  inherited;
end;

function TMyServer.GetClientCount: integer;
begin
  CS.Enter;
  try
    Result:=CliCnt;
  finally
    CS.Leave;
  end;
end;

procedure TMyServer.Start;
begin
  Server.ServerPort := '80';
  Server.Listen;
end;

procedure TMyServer.Stop;
begin
  Server.StopListen;
end;

procedure TMyServer.SetLogEnabled(const Value: Boolean);
begin
  if Value then StartLog else StopLog;
  FLogEnabled := Value;
end;

procedure TMyServer.ServerListenError(Sender: TRtcConnection; E: Exception);
begin
  XLog('Error starting Web Server!'#13#10 + E.ClassName+'>'+E.Message);
  if assigned(OnError) then
    OnError(Sender,E);
end;

procedure TMyServer.ServerListenStart(Sender: TRtcConnection);
begin
  XLog('SERVER STARTED ...');
  if assigned(OnStart) then
    OnStart(Sender);
end;

procedure TMyServer.ServerListenStop(Sender: TRtcConnection);
begin
  if assigned(OnStop) then
    OnStop(Sender);
  XLog('SERVER STOPPED.');
end;

procedure TMyServer.ServerConnecting(Sender: TRtcConnection);
begin
  CS.Enter;
  try
    Inc(CliCnt);
    with Sender do
      XLog('++++ '+PeerAddr+':'+PeerPort+' ['+IntToStr(CliCnt)+' open]');
  finally
    CS.Leave;
  end;
  if assigned(OnConnect) then
    OnConnect(Sender);
end;

procedure TMyServer.ServerDisconnecting(Sender: TRtcConnection);
begin
  CS.Enter;
  try
    Dec(CliCnt);
    with Sender do
      XLog('---- '+PeerAddr+':'+PeerPort+' ['+IntToStr(CliCnt)+' open]');
  finally
    CS.Leave;
  end;
  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
end;

procedure TMyServer.ServerRequestNotAccepted(Sender: TRtcConnection);
begin
  // Anything that comes this far is not acceptable by any DataProvider component.
  with TRtcDataServer(Sender) do
  begin
    XLog('BAD! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Method "'+Request.Method+'" not supported.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    
    Disconnect;
  end;
end;

procedure TMyServer.ServerInvalidRequest(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do
  begin
    XLog('ERR! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
  end;
end;

procedure TMyServer.ServerDisconnect(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do
  begin
    if Request.DataSize > Request.DataIn then
    begin
      // did not receive a complete request
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while receiving a Request ('+IntToStr(Request.DataIn)+' of '+IntToStr(Request.DataSize)+' bytes received).');
    end;
  end;
end;

end.
*)