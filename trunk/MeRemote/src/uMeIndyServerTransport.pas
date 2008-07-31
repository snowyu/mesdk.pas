
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
  , uMeStream
  //, uMeTransport
  , uMeRemoteServerFunc
  , uMeRemoteUtils
  , uMeStrUtils
  //, IdCommandHandlers
  , IdContext
  , IdIOHandler
  , IdReply
  , IdCustomTCPServer
  ;

type
  //return -1 means not found.
  TMeCmdSearchEvent = function(const aCmd: string): Integer of object;
  TMeCmdExecuteEvent = procedure(const aContext: TIdContext; const aCmd: Integer; const aParams: PMeStream; var aSuccessful: Boolean) of object;
  TMeIndyRemoteFunctionServer = class(TIdCustomTCPServer)
  protected
    FOnCmdExecute: TMeCmdExecuteEvent;
    FOnCmdSearch: TMeCmdSearchEvent;

    function ReadCommandLine(AContext: TIdContext): string;
    //return 200 means ok, others mwans error code.
    function HandleCommand(const aContext: TIdContext; aLine: string): WordBool;
    function DoExecute(AContext: TIdContext): Boolean; override;
  public
    constructor Create(aComponent: TComponent); //override;
    property OnCmdSearch: TMeCmdSearchEvent read FOnCmdSearch write FOnCmdSearch;
    property OnCmdExecute: TMeCmdExecuteEvent read FOnCmdExecute write FOnCmdExecute;
  end;

  TMeIndyServerTransport = class
  protected
    FServer: TMeIndyRemoteFunctionServer;
    FRemoteFunctions: PMeRemmoteFunctions;
    function SearchCmd(const aCmd: string): Integer;
    procedure ExecuteCmd(const aContext: TIdContext; const aCmd: Integer; const aParams: PMeStream; var aSuccessful: WordBool);
  public
    constructor Create();
    destructor Destroy(); override;

    property Server: TMeIndyRemoteFunctionServer read FServer;
    property RemoteFunctions: PMeRemmoteFunctions read FRemoteFunctions;
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
        Result := Word(HandleCommand(aContext, vLine)) = 200;
      end;
    end;
  //if return false to stop.
  //Result := False;
  if Result and Assigned(AContext.Connection) then begin
    Result := AContext.Connection.Connected;
  end;
end;

function TMeIndyRemoteFunctionServer.HandleCommand(const aContext: TIdContext; aLine: string): WordBool;
var
  vCmdId: Integer;
  vStream: PMeMemoryStream;
  vStreamProxy: TMeStreamProxy;
  s: string;
begin
  Result := StrFetch(aLine, ' ') = 'cmd';
  if Result then
  begin
    vCmdId := -1;
    if Assigned(FOnCmdSearch) then
      vCmdId := FOnCmdSearch(aLine);
    Result := vCmdId >= 0;
    if Result then 
    begin
      aContext.Connection.IOHandler.Write(200);
      Result := aContext.Connection.Connected;
      if Result then
      begin
        New(vStream, Create);
        vStreamProxy := TMeStreamProxy.Create(vStream);
        try
          aContext.Connection.IOHandler.LargeStream := False;
          aContext.Connection.IOHandler.ReadStream(vStreamProxy);
          Result := False;
          if Assigned(FOnCmdExecute) then
          begin
            vStream.SetPosition(0);
            try
              s := '';
              FOnCmdExecute(aContext, vCmdId, vStream, Result);
              Word(Result) := 200;
            except
              On E: Exception do
              begin
              	Word(Result) := 400; //the execution error.
                s := 'Exception(' + E.ClassName + ') occur :' + E.Message;
                vStreamProxy.Size := 0;
                vStreamProxy.WriteBuffer(s[1], Length(s));
              end;
            end;
            if Result then
            begin
            	aContext.Connection.IOHandler.Write(Word(Result));
              vStream.SetPosition(0);
              aContext.Connection.IOHandler.Write(vStreamProxy, 0, True);
            end;
          end;
        finally
          vStream.Free;
          FreeAndNil(vStreamProxy);
        end;
      end;
    end
    else
      aContext.Connection.IOHandler.Write(401); //unkown 
  end
end;

function TMeIndyRemoteFunctionServer.ReadCommandLine(AContext: TIdContext): string;
begin
  Result := AContext.Connection.IOHandler.ReadLn;
end;

{ TMeIndyServerTransport }
constructor TMeIndyServerTransport.Create();
begin
  inherited;
  FServer := TMeIndyRemoteFunctionServer.Create(nil);
  New(FRemoteFunctions, Create);
  FServer.OnCmdSearch := SearchCmd;
  FServer.OnCmdExecute := ExecuteCmd;
end;

destructor TMeIndyServerTransport.Destroy();
begin
  FreeAndNil(FServer);
  MeFreeAndNil(FRemoteFunctions);
  inherited;
end;

procedure TMeIndyServerTransport.ExecuteCmd(const aContext: TIdContext; const aCmd: Integer; const aParams: PMeStream; var aSuccessful: WordBool);
var
  vResult: PMeMemoryStream;
begin
  //aSuccessful := False;
  begin
    New(vResult, Create);
    try
      FRemoteFunctions.Execute(aCmd, aParams, vResult);
      aParams.SetSize(0);
      aParams.CopyFrom(vResult, 0);
      //aSuccessful := true;
    finally
      vResult.Free;
    end;
  end;
end;

function TMeIndyServerTransport.SearchCmd(const aCmd: string): Integer;
begin
  Result := FRemoteFunctions.IndexOfName(aCmd);
end;

initialization
finalization
end.
