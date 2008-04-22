

{ Summary: the AX Script Debug


 How to activate script debug from host?
The big differences between being a smart host and a dumb host are:

1) If you're a dumb host and an error occurs, the script engine asks _the
user_ if the debugger should be started up.  If you're a smart host, the
script engine asks _the host_ if the debugger should be started up -- you
might not want your users to get a "want to start the debugger?" message
every time an error occurs.

2) If you're a dumb host and the debugger starts up, the script engine
supplies the source code to the debugger.  If you're a smart host, _you_
supply the source code to the debugger.  The script engine has no idea that
the source code is embedded in HTML or an Active Server Page or whatever --
but that context might be important to the user.  In ASP, particularly, the
script which is run by the script engine does not look much like the page
that it's on.

To answer your other questions:

A smart host implements a debug site so that the script engine can
communicate with the host and ask it whether it should start up the
debugger, etc.

What do you mean by "debug scripts in my application?"  If you mean "can I
write an application that can debug scripts?"  then you are asking "can I
write a debugger?"  Well, sure, you can write a debugger if you want.  If
you mean "My application contains some scripts -- can I debug them?"  then
yes, you can do that without being a smart host -- just put a "stop" or "debugger"
statement in the script where you want the debugger to start.

Wait, are you trying to write a _smart host_, or are you trying to implement
your own _debugger_, or both?  Why do you have a custom Debug Application
object?  A Debug Application is the debugger.  If you don't want to create
your own debugger then just have the site call GetDefaultApplication on the
Process Debug Manager and return that application.

Assuming that you ARE trying to write your own debugger, I'll answer your
question:  it depends on what you do to the script engine.  But typically
first the script engine QI's the application to get it's
ConnectionPointContainer so that it can sink the events thrown by the debug
application.  Once that's taken care of, it adds a stack frame sniffer.
Then it starts adding nodes for the code blocks.

Frankly, I don't recommend writing your own debugger -- the interfaces are
extremely complicated and not very well documented.



If you've implemented Active Debugging in your Script Host, usually by
implementing a Smart Host as described in the Active Debugging
documentation, you should be able to call IDebugApplication::CauseBreak() to
produce the Break at Next Statement functionality.  Please see the previous
posting for the status of the Active Scripting and Active Debugging
documentation. 

First of all, your host must implement IActiveScriptSiteDebug. The simplest
way is to recive insnances of IDebugApplication and IDebugDocumentHelper
from Process Debug Manager and use them as you wish. It allows you to set a
break point from within your application , to display your project and
script block descriptions in Running Documents window of Script Debugger or
Interdev. But still you can not edit your script there. When error occurs
while running your script, you can deside should the debugger be started up.

You can also implement IApplicationDebugger and connect debugger on it. This
feature allows you to handle errors and breakpoints in your
onHandleBreakPoint function: you can decide what debugger should do next -
step over, step into, continue and so on.  But there is one problem:
resources of your application are not avaleble at this moment because the
main thread is stopped by the debugger. Application can't debug itself while
its not  multi-threaded or it's not using another process like Script
Debugger or InterDev. 

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
    * The Original Code is $RCSfile: uAXScriptDebug.pas,v $.
    * The Initial Developers of the Original Code are Riceball
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.
    * Contributor(s):
}
unit uAXScriptDebug;

interface

uses
  Windows, SysUtils, ActiveX, ComObj
  , uAXScriptInf
  , uAXScript
  ;

type
  TAXErrorDebugEvent = procedure(const Sender: TObject; const aErrorDebug: IActiveScriptErrorDebug;
                var aEnterDebugger : BOOL;var aCallOnScriptErrorWhenContinuing : BOOL) of object;

  TAXScriptSiteDebug = class(TAXScriptSite, IActiveScriptSiteDebug)
  protected
    FProcessDebugManager: IProcessDebugManager;
    FDebugApp: IDebugApplication;
    FDebugDocHelper: IDebugDocumentHelper;
    FAppCookie: DWORD;
    FAppName: WideString;
    FCanDebugError: Boolean;
    FBreakOnStart: Boolean;
    FOnErrorDebug: TAXErrorDebugEvent;

    procedure SetAppName(const Value: WideString);

    procedure InitDebugApplication;
    procedure iExecute(const aCode: WideString);override;
    procedure CreateScriptEngine(const Language: string);override;

    {IActiveScriptSiteDebug Interface}
    function GetDocumentContextFromPosition(const dwSourceContext : DWORD;
          const uCharacterOffset: ULONG; const uNumChars : ULONG;
          out ppsc : IDebugDocumentContext) : HRESULT; stdcall;

    function GetApplication(out ppda : IDebugApplication) : HRESULT; stdcall;
    function GetRootApplicationNode(out ppdanRoot : IDebugApplicationNode) : HRESULT; stdcall;
    function OnScriptErrorDebug(const pErrorDebug : IActiveScriptErrorDebug;
                out pfEnterDebugger : BOOL;out pfCallOnScriptErrorWhenContinuing : BOOL) : HRESULT; stdcall;
  public
    constructor Create({$IFDEF UseComp}aOwner: TComponent = nil{$ENDIF}); override;

    property AppName: WideString read FAppName write SetAppName;
    property CanDebugError: Boolean read FCanDebugError write FCanDebugError default true;
    property BreakOnStart: Boolean read FBreakOnStart write FBreakOnStart;
    property OnErrorDebug: TAXErrorDebugEvent read FOnErrorDebug write FOnErrorDebug;
  end;

implementation

{ TAXScriptSiteDebug }
constructor TAXScriptSiteDebug.Create;
begin
  inherited;
  FCanDebugError := True;
end;

procedure TAXScriptSiteDebug.CreateScriptEngine(const Language: string);
begin
  InitDebugApplication;
  inherited;
end;

procedure TAXScriptSiteDebug.iExecute(const aCode: WideString);
var
  hr: HRESULT;
  dw: Longword;
begin
  hr := FDebugDocHelper.AddUnicodeText(PWideChar(aCode));
  OleCheck(hr);

  hr := FDebugDocHelper.DefineScriptBlock(0, Length(aCode), FEngine, False, dw);
  OleCheck(hr);

  inherited;
  if FBreakOnStart then
  begin
    //startup the debugger session
    hr := FDebugDocHelper.BringDocumentToTop();
    if hr <> S_OK then 
      raise Exception.Create('Can not startup the debugger session');
    //OleCheck(hr);

    hr := FDebugApp.CauseBreak();
    if hr <> S_OK then 
      raise Exception.Create('Can not CauseBreak');
    //OleCheck(hr);
  end;
end;

procedure TAXScriptSiteDebug.InitDebugApplication;
var
  hr: HRESULT;
begin
  hr := CoCreateInstance(CLSID_ProcessDebugManager, nil,
        CLSCTX_INPROC_SERVER or CLSCTX_INPROC_HANDLER
        or CLSCTX_LOCAL_SERVER, IProcessDebugManager, FProcessDebugManager);
  OleCheck(hr);

  hr := FProcessDebugManager.CreateApplication(FDebugApp);
  OleCheck(hr);

  hr := FDebugApp.SetName(PWideChar(FAppName));
  OleCheck(hr);

  hr := FProcessDebugManager.AddApplication(FDebugApp, FAppCookie);
  OleCheck(hr);

  hr := FProcessDebugManager.CreateDebugDocumentHelper(nil, FDebugDocHelper);
  OleCheck(hr);

  hr := FDebugDocHelper.Init(FDebugApp, PWideChar(AppName), 'Scripted Current Text', TEXT_DOC_ATTR_READONLY);
  OleCheck(hr);

  hr := FDebugDocHelper.Attach(nil);
  OleCheck(hr);

  hr := FDebugDocHelper.SetDocumentAttr(TEXT_DOC_ATTR_READONLY);
  OleCheck(hr);
end;

// Used by the language engine to delegate IDebugCodeContext::GetSourceContext. 
function TAXScriptSiteDebug.GetDocumentContextFromPosition(const dwSourceContext : DWORD;
      const uCharacterOffset: ULONG; const uNumChars : ULONG;
      out ppsc : IDebugDocumentContext) : HRESULT;
var
  vStartPos: LongWord;
begin
  vStartPos := 0;
  if Assigned(FDebugDocHelper) then
  begin
    Result := FDebugDocHelper.GetScriptBlockInfo(dwSourceContext, IActiveScript(vStartPos), vStartPos, vStartPos);
    if Result = S_OK then
      Result := FDebugDocHelper.CreateDebugDocumentContext(vStartPos + uCharacterOffset, uNumChars, ppsc);
  end
  else
    Result := E_NOTIMPL;
end;


// Returns the debug application object associated with this script site. Provides 
// a means for a smart host to define what application object each script belongs to. 
// Script engines should attempt to call this method to get their containing application 
// and resort to IProcessDebugManager::GetDefaultApplication if this fails. 
function TAXScriptSiteDebug.GetApplication(out ppda : IDebugApplication) : HRESULT; 
begin
  {if Assigned(FDebugApp) then
  begin
    FDebugApp.AddRef();
  end; //}

  ppda := FDebugApp;

	Result := S_OK;
end;

// Gets the application node under which script documents should be added 
// can return NULL if script documents should be top-level. 
function TAXScriptSiteDebug.GetRootApplicationNode(out ppdanRoot : IDebugApplicationNode) : HRESULT; 
begin
  if Assigned(FDebugDocHelper) then
    Result := FDebugDocHelper.GetDebugApplicationNode(ppdanRoot)
  else
    Result := E_NOTIMPL;
end;

function TAXScriptSiteDebug.OnScriptErrorDebug(const pErrorDebug : IActiveScriptErrorDebug;
            out pfEnterDebugger : BOOL;out pfCallOnScriptErrorWhenContinuing : BOOL) : HRESULT; 
begin
  pfEnterDebugger := FCanDebugError;
  if Assigned(FOnErrorDebug) Then
    FOnErrorDebug(Self, pErrorDebug, pfEnterDebugger, pfCallOnScriptErrorWhenContinuing);
  Result := S_OK;
end;

procedure TAXScriptSiteDebug.SetAppName(const Value: WideString);
begin
  if Value <> FAppName Then
  begin
    if Assigned(FDebugApp) then
    begin
      OleCheck(FDebugApp.SetName(PWideChar(Value)));
    end;
    FAppName := Value;
  end;
end;


end.
