{Summary Abstract MeTransport class and factory.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision$

HTTP Method   CRUD Action   Description
POST          CREATE        Create a new resource
GET           RETRIEVE      Retrieve a representation of a resource
PUT           UPDATE        Update a resource
DELETE        DELETE        Delete a resource  


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
unit uMeTransport;

{$I MeSetting.inc}

{$IFNDEF MeRTTI_EXT_SUPPORT}
  {$Message Fatal 'need MeRTTI_EXT_SUPPORT'}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeObject
  , uMeSyncObjs
  ;

type
  TMeTransportClass = class of TMeTransport;
  //how to convert the params to stream?
  {
  http://host/cat/aMethod?param=xx
  i will use REST protocol to do?
  }
  TMeReceivedDataEvent = procedure (const Sender: TObject; const aCmd: string; const aReply: PMeStream) of object;
  //abstract Transport class
  TMeTransport = class
  protected
    //for Async method:
    //the default timeout
    FTimeOut: Integer;
    FLock: PMeCriticalSection;
    FIsBusy: Integer;
    FConnected: Boolean;
    FURL: string;
    FKeepAlive: Boolean;
    FOnReceived: TMeReceivedDataEvent;

    procedure SetURL(const Value: string);
    //procedure iSendAsyn(const aCmd: string; const aRequest: PMeStream; const aReply: PMeStream; const aTimeOut: Integer = 0);virtual;abstract;
    procedure iSend(const aCmd: string; const aRequest: PMeStream; const aReply: PMeStream);virtual;abstract;
    procedure iConnect();virtual;abstract;
    procedure iDisconnect();virtual;abstract;
    procedure iURLChanged;virtual;abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Connect();
    procedure Disconnect();
    //ProcessStream(Data, Message)?
    //send the aRequest, received the aReply.
    procedure Send(const aCmd: string; const aRequest: PMeStream; const aReply: PMeStream);
    //aTimeOut: -1 means the default timeout(FTimeout property), 0 means for ever.
    //procedure SendAsyn(const aCmd: string; const aRequest: PMeStream; const aReply: PMeStream; const aTimeOut: Integer = 0);

    property TimeOut: Integer read FTimeOut write FTimeOut;
    property URL: string read FURL write SetURL;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property OnReceived: TMeReceivedDataEvent read FOnReceived write FOnReceived;
  end;

{
  PMeRemoteParamInfo = ^ TMeRemoteParamInfo;
  TMeRemoteParamInfo = record
    Index: Integer;
    Name: string; //
    //if i use the TypeId? how can be sure this typeid is unique both the server and the client?
    //or the internal type i used id, the extented type used the string.
    TypeName: string;
    Value: string;
  end;

  TMeStreamFormater = class
  protected
    function TypeNameToId(const aTypeName: string): TypeId;
  public
    //convert stream to params
    procedure ToParams(const aStream: TStream; const aParams: TMeRemoteParams);virtual;
    //convert Params to Stream
    procedure ToStream(const aParams: TMeRemoteParams; const aStream: TStream);virtual;
  end;
//}

var
  DefaultTransportClass: TMeTransportClass = nil;

implementation

{ TMeTransport }
constructor TMeTransport.Create;
begin
  inherited;
  New(FLock, Create);
end;

destructor TMeTransport.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TMeTransport.Connect();
begin
  if not FConnected then
  begin
    FLock.Enter;
    try
      iConnect;
      FConnected := True;
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TMeTransport.Disconnect();
begin
  if FConnected then
  begin
    FLock.Enter;
    try
      iDisconnect;
      FConnected := False;
    finally
      FLock.Leave;
    end;
  end;
end;

{
procedure TMeTransport.SendAsyn(const aCmd: string; const aRequest: PMeStream; const aReply: PMeStream; aTimeOut: Integer = 0);
begin
  if Assigned(aRequest) and Assigned(aReply) then
  begin
    if aTimeOut = -1 then aTimeOut := FTimeOut;
    InterlockedIncrement(FIsBusy);
    Connect;
    try
      iSendAsyn(aCmd. aRequest, aReply, aTimeOut);
    finally
      InterlockedDecrement(FIsBusy);
      if not FKeepAlive then Disconnect;
    end;
  end;
end;
//}

procedure TMeTransport.Send(const aCmd: string; const aRequest: PMeStream; const aReply: PMeStream);
begin
  if Assigned(aRequest) and Assigned(aReply) then
  begin
    InterlockedIncrement(FIsBusy);
    Connect;
    FLock.Enter;
    try
      iSend(aCmd, aRequest, aReply);
    finally
      FLock.Leave;
      InterlockedDecrement(FIsBusy);
      if not FKeepAlive then Disconnect;
    end;
  end;
end;

procedure TMeTransport.SetURL(const Value: string);
begin
  if (FURL <> Value) then
  begin
    FLock.Enter;
    try
      if FConnected then 
      begin
        iDisconnect;
        FConnected := False;
      end;
      FURL := Value;
      iURLChanged;
    finally
      FLock.Leave;
    end;
  end;
end;

initialization
end.
