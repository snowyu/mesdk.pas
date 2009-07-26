
{Summary The MePlugin is a mini general SOA(service-oriented architecture) system framework for Client to impl api features.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.1 $

Description

you need export three functions:
  ServiceInfo,
  InitializeService,
  TerminateService;


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
    * The Original Code is $RCSfile: uMePlugin.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):

}
unit uMePlugin;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils //, Classes
  , TypInfo
  , uMeObject
  , uMeProcType
  , uMeServiceTypes
  , uMeService
  ;

type
  PMePluginService = ^ TMePluginService;

  TMePluginService = object(TMeAbstractService)
  protected
    FInitInfo: TMeServiceInitInfo;
    FInitialized: Boolean;

    procedure Init; virtual; {override}

    function DoInitialize: Integer; virtual;
    function DoTerminate: Integer; virtual;
    procedure DoAllModulesLoaded; virtual;

    function OnAllModulesLoaded(const wParam, lParam, lResult: Cardinal): Integer; stdcall;
  public
    destructor Destroy; virtual; {override}

    function Initialize(aInitInfo: TMeServiceInitInfo): Integer;
    function Terminate: Integer;

    function CreateService(const ServiceName: TMeIdentity): HMeService;
    function CreateFunction(const aName: TMeIdentity; const aProc: TMeServiceFunctionProc): HMeServiceFunction;
    //note: U must keep the object instance alive!!
    function CreateMethod(const aName: TMeIdentity; const aMethod: TMeServiceMethod): HMeServiceFunction;
    function GetService(const aName: TMeIdentity): HMeService;
    function GetFunction(const aName: TMeIdentity): HMeServiceFunction;
    function Call(aFunctionHandle: HMeServiceFunction; wParam, lParam: Cardinal): Integer; overload;
    function Call(aFunctionName: TMeIdentity; wParam, lParam: Cardinal): Integer; overload;
    function CreateEvent(EventName: TMeIdentity): HMeServiceEvent;
    function GetEvent(EventName: TMeIdentity): HMeServiceEvent;
    function NotifyEvent(EventHandle: HMeServiceEvent; wParam, lParam: Cardinal): Integer;
    function HookEvent(const aEventName: TMeIdentity; const aProc: TMeServiceEventProc): Integer; overload;
    function HookEvent(const aEventName: TMeIdentity; const aMethod: TMeServiceEventMethod): Integer; overload;
    function UnHookEvent(const aEventName: TMeIdentity; const aProc: TMeServiceEventProc): Integer; overload;
    function UnHookEvent(const aEventName: TMeIdentity; const aMethod: TMeServiceEventMethod): Integer; overload;
  public
    property Initialized: Boolean read FInitialized;
  end;

function ServiceInfo(nInfoNo: Integer; lpBuffer: LPTSTR; nSize: Integer): Integer; stdcall;
function Initialize(lpPluginInitInfo: PMeServiceInitInfo): Integer; stdcall;
function Terminate: Integer; stdcall;


exports
  ServiceInfo name EXPORT_FUNC_GETINFO,
  Initialize name EXPORT_FUNC_INITIALIZE,
  Terminate name EXPORT_FUNC_TTERMINATE;


function GPlugin: PMePluginService;
procedure SetPlugin(const aPlugin: PMePluginService);

implementation

var
  FPlugin: PMePluginService;

function GPlugin: PMePluginService;
begin
  Result := FPlugin;
  Assert(Assigned(Result), 'u must SetPlugin first.');
end;

procedure SetPlugin(const aPlugin: PMePluginService);
begin
  if Assigned(FPlugin) then FPlugin.Free;
  FPlugin := aPlugin;
end;

function ServiceInfo(nInfoNo: Integer; lpBuffer: LPTSTR; nSize: Integer): Integer; stdcall;
var
  vInfo : TMeIdentity;
begin
  vInfo := GPlugin.Info[nInfoNo];

  if (vInfo <> '') then
  begin
    lstrcpyn(lpBuffer, LPTSTR(vInfo), nSize-1);
    LPTSTR(lpBuffer)[nSize-1] := #0;
    if nSize-1 < Length(vInfo) then
      Result := nSize - 1
    else
      Result := Length(vInfo);
  end else
    Result := 0;
end;

function Initialize(lpPluginInitInfo: PMeServiceInitInfo): Integer; stdcall;
begin
  Result := GPlugin.Initialize(lpPluginInitInfo^)
end;

function Terminate: Integer; stdcall;
begin
  if GPlugin.FInitialized then
    Result := GPlugin.Terminate
  else
    Result := MEAPI_ERR_UNINITIALIZED;
end;

{ TMePluginService }
procedure TMePluginService.Init; 
begin
  inherited;
end;

destructor TMePluginService.Destroy;
begin
  inherited;
end;

function TMePluginService.Initialize(aInitInfo: TMeServiceInitInfo): Integer;
begin
  if not FInitialized then
  begin
    FInitialized := True;
    FInitInfo := aInitInfo;
    Result := DoInitialize;
  end
  else
    Result := MEAPI_ERR_ALREADY_INITIALIZED;
end;

function TMePluginService.Terminate: Integer;
begin
  if FInitialized then
  begin
    FInitialized := False;
    Result := DoTerminate;
  end
  else
    Result := MEAPI_ERR_UNINITIALIZED;
end;

function TMePluginService.DoInitialize: Integer;
begin
  HookEvent(MESRV_SYSTEM_MODULESLOADED, OnAllModulesLoaded);
  Result := MEAPI_OK;
end;

function TMePluginService.DoTerminate: Integer;
begin
  Result := MEAPI_OK;
end;

procedure TMePluginService.DoAllModulesLoaded;
begin
end;

function TMePluginService.CreateService(const ServiceName: TMeIdentity): HMeService;
begin
  Result := FInitInfo.RegisterService(LPTSTR(ServiceName));
end;

function TMePluginService.OnAllModulesLoaded(const wParam, lParam, lResult: Cardinal): Integer; 
begin
  DoAllModulesLoaded;
  Result := 0;
end;

initialization
  SetMeVirtualMethod(TypeOf(TMePluginService), ovtVmtParent, TypeOf(TMeAbstractService));

  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMePluginService), ovtVmtClassName, nil);
  {$ENDIF}
end.
