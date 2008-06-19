
{Summary The Indy Server MeTransport class.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision$

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
    * The Original Code is $RCSfile: uMeIndyServerTransport.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeIndyServerTransport;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeObject
  //, uMeTransport
  , uMeRemoteServerFunc
  , uMeRemoteUtils
  , uMeStrUtils
  , IdCommandHandlers
  , IdContext
  , IdIOHandler
  , IdReply
  , IdTCPServer
  ;

type
  TMeIndyRemoteFunctionServer = class(TIdTCPServer)
  protected
    FRemoteFunctions: PMeRemmoteFunctions;

    function ReadCommandLine(AContext: TIdContext): string;
    function HandleCommand(const aContext: TIdContext; aLine: string): Boolean;
    function DoExecute(AContext: TIdContext): Boolean; override;
  public
    constructor Create(aComponent: TComponent); override;
  end;

  {TMeIndyRemoteFunctionServer = class
  protected
    FSerer: TMeIndyBinServer
    FRemoteFunctions: PMeRemmoteFunctions;
    //procedure iSend(const aCmd: string; const aRequest: PMeStream; const aReply: PMeStream);
  public
    constructor Create();
    destructor Destroy(); override;
  end; //}

implementation

{ TMeIndyRemoteFunctionServer }
constructor TMeIndyRemoteFunctionServer.Create(aComponent: TComponent); 
begin
  inherited;
end;

function TMeIndyRemoteFunctionServer.DoExecute(aContext: TIdContext): Boolean;
var
  vLine: string;
begin
    if aContext.Connection.Connected then 
    begin
      vLine := ReadCommandLine(aContext);
      Result := (vLine <> '');
      if Result then 
      begin
        if not HandleCommand(aContext, vLine) then
        begin
          aContext.Connection.IOHandler.Write(401); //unkown 
        end;
      end;
    end;
end;

function TMeIndyRemoteFunctionServer.HandleCommand(const aContext: TIdContext; aLine: string): Boolean;
var
  vCmd: string;
  vStream: PMeMemoryStream;
  vStreamProxy: IStream;
begin
  vCmd := StrFetch(aLine, ' ');
  Result := vCmd = 'cmd';
  if Result then
  begin
    aContext.Connection.IOHandler.Write(200);
    while InputBufferIsEmpty and aContext.Connection.Connected do
      Sleep(50);
    Result := aContext.Connection.Connected;
    if Result then
    begin
      New(vStream, Create);
      vStreamProxy := TMeStreamProxy.Create(vStream);
      try
        aContext.Connection.IOHandler.ReadStream(vStreamProxy);
        if vStream.GetSize > 0 then
        begin //process parameters
        end;
      finally
        vStream.Free;
      end;
    end;
  end
end;

function TMeIndyRemoteFunctionServer.ReadCommandLine(AContext: TIdContext): string;
begin
  Result := AContext.Connection.IOHandler.ReadLn;
end;

initialization
finalization
end.
