{$IFNDEF MeRTTI_EXT_SUPPORT}
  $Message Fatal 'need MeRTTI_EXT_SUPPORT'}
{$ENDIF}

{Summary Abstract MeTransport class and factory.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.00 $

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

interface

{$I Setting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeObject
  ;

type
  TMeTransportClass = class of TMeTransport;
  //how to convert the params to stream?
  {
  http://host/cat/aMethod?param=xx
  i will use REST protocol to do?
  }
  TMeReceivedDataEvent = procedure (const Sender: TObject; const aReply: PMeStream) of object;
  //abstract Transport class
  TMeTransport = class()
  protected
    //for Async method:
    //the default timeout
    FTimeOut: Integer;
    FOnReceived: TMeReceivedDataEvent;

    procedure iSendAsyn(const aRequest: TStream; const aReply: PMeStream; const aTimeOut: Integer = 0);virtual;abstract;
    procedure iSend(const aRequest: TStream; const aReply: PMeStream);virtual;abstract;
  public
    //ProcessStream(Data, Message)?
    //send the aRequest, received the aReply.
    procedure Send(const aRequest: PMeStream; const aReply: PMeStream);
    procedure SendAsyn(const aRequest: PMeStream; const aReply: PMeStream; const aTimeOut: Integer = 0);

    property TimeOut: Integer read FTimeOut write FTimeOut;
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

procedure TMeTransport.SendAsyn(const aRequest: PMeStream; const aReply: PMeStream; aTimeOut: Integer = 0);
begin
  if Assigned(aRequest) and Assigned(aReply) then
  begin
    if aTimeOut = -1 then aTimeOut := FTimeOut;
    iSendAsyn(aRequest, aReply, aTimeOut);
  end;
end;

procedure TMeTransport.Send(const aRequest: TStream; const aReply: TStream);
begin
  if Assigned(aRequest) and Assigned(aReply) then
  begin
    iSend(aRequest, aReply);
  end;
end;

initialization
end.
