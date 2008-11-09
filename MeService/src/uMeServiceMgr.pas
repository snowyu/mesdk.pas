{Summary The MeService Manager is an abstract Service Manager for the general SOA(service-oriented architecture) system framework.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.2 $

Description

TMeCustomServiceManager

TMeCustomSerivces

Local:
  PluginMgr load plugin from DLL.
  PluginMgr ---> SerivceMgr
    manage the ServiceInfo --> CustomService 

Remote:
  1. Client: Crete RPC Plugin to do so.
       RemoteClientTransportPlugin
       RemoteClientPlugin
  2. Server: 
       RemoteServiceMgr --> PluginMgr
       RemoteServerTransportPlugin --> Plugin


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
    * The Original Code is $RCSfile: uMeServiceMgr.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeServiceMgr;

interface

{$I MeService.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeSysUtils
  , uMeStrUtils
  , uMeProcType
  , uMeServiceTypes
  , uMeService
  , uMeServiceFunction
  ;

type
  PMeCustomServiceManager = ^ TMeCustomServiceManager;
  PMeCustomService = ^ TMeCustomService;
  PMeServiceEvent = ^ TMeServiceEvent;

 
  TMeCustomService = object(TMeAbstractService)
  protected
    FFunctions: PMeThreadSafeList; //List of PMeServiceFunction
    FEvents: PMeThreadSafeList;    //List of PMeServiceEvent

    function GetFunctions: PMeThreadSafeList;
    function GetEvents: PMeThreadSafeList;

    procedure Init; virtual; {override}
  public
    destructor Destroy; virtual; {override}

    function RegisterFunction(const aName: TMeIdentity; const aFuncEntry: Pointer; aProcParams: PTypeInfo = nil; const aInstance: Pointer = nil): PMeServiceFunction;
    function RegisterEvent(const aName: TMeIdentity): PMeServiceEvent;
    function FindFunction(const aName: TMeIdentity; aProcParams: PTypeInfo = nil): PMeServiceFunction;
    function FindEvent(const aName: TMeIdentity): PMeServiceEvent;

  public
    property Functions: PMeThreadSafeList read GetFunctions;
    property Events: PMeThreadSafeList read GetEvents;
  end;

  TMeServiceEvent = object(TMeDynamicObject)
  protected
    FName: TMeIdentity;
    FInstance: Pointer;
    //List of PMethod
    FListeners: PMeThreadSafeList;

    procedure Init; virtual; {override}
  public
    destructor Destroy; virtual; {override}

    function AddListener(const aMethod: TMethod): Integer;
    {-1 means not found}
    function RemoveListener(const aMethod: TMethod): Integer;
    function Notify(const wParam, lParam: Cardinal): Integer;
    function IndexOfListener(const aMethod: TMethod): Integer;
    property Name: TMeIdentity read FName write FName;
  end;

  { this is the singleton }
  TMeCustomServiceManager = object(TMeDynamicObject)
  protected
    FServices: PMeThreadSafeList; 

    procedure Init; virtual; //override
    class function ServiceClass: TMeClass; virtual;
    function GetCount: Integer;
  public
    destructor Destroy; virtual; {override}

    function FindService(const aServiceName: TMeIdentity): PMeCustomService;
    function IndexOfService(const aServiceName: TMeIdentity): Integer;
    function AddService(const aService: PMeCustomService): Integer;
    //delete the service 
    procedure Delete(const index: Integer);

    {: Create a Service by name }
    {
    Description
      @param aName  the service name(URL)
    return 
     返回 0 表示失败 }
    function CreateService(const aName: TMeIdentity): HMeService;
    // if ProcInfo is nil means fixed parameter mode.
    function CreateServiceFunction(aFuncName: TMeIdentity; 
      const aProcAddr: Pointer; const aProcParams: PTypeInfo = nil; const aInstance: Pointer = nil): HMeServiceFunction;
    function GetService(const aName: TMeIdentity): HMeService;
    //the aName is a Full URN functon name.
    function GetServiceFunction(aName: TMeIdentity; const aProcParams: PTypeInfo = nil): HMeServiceFunction;
    function CallServiceFunction(const aFuncHandle: HMeServiceFunction; const wParam, lParam: Cardinal): Integer;
    //调用增强模式下的函数
    function CallServiceFunctionEx(const aFuncHandle: HMeServiceFunction; const aParams: Variant): Integer;
    function CreateEvent(aName: TMeIdentity): HMeServiceEvent;
    function GetEvent(aName: TMeIdentity): HMeServiceEvent;
    function NotifyEvent(const aHandle: HMeServiceEvent; const wParam, lParam: Cardinal): Integer;
    {return 0 means failed.}
    function HookEvent(aName: TMeIdentity; const ProcAddr: Pointer; const lpInstance: Pointer = nil): Integer;
    function UnhookEvent(aName: TMeIdentity; const ProcAddr: Pointer; const lpInstance: Pointer = nil): Integer;

    //the service count
    property Count: Integer read GetCount;
  end;

function GMeServiceManager: PMeCustomServiceManager;
procedure SetMeServiceManager(const aServiceMgr: PMeCustomServiceManager);

function GetServiceInitInfo: TMeServiceInitInfo;

implementation

var
  FMeServiceManager: PMeCustomServiceManager = nil; //NsmCom

function GMeServiceManager: PMeCustomServiceManager;
begin
  Result := FMeServiceManager;
  Assert(Assigned(FMeServiceManager), 'FMeServiceManager should be assigned!!');
end;

procedure SetMeServiceManager(const aServiceMgr: PMeCustomServiceManager);
begin
  if Assigned(FMeServiceManager) then
    FMeServiceManager.Free;
  FMeServiceManager := aServiceMgr;
end;

function _CreateService(const lpServiceName: LPCTSTR): HMeService; stdcall;
begin
  Result := FMeServiceManager.CreateService(lpServiceName);
end;

function _CreateServiceFunction(const lpServiceFunctionName: LPCTSTR
  ; const lpProcAddr: Pointer; const lpProcParams: Pointer = nil; const lpInstance: Pointer = nil): HMeServiceFunction; stdcall;
begin
  Result := FMeServiceManager.CreateServiceFunction(lpServiceFunctionName, lpProcAddr, lpProcParams, lpInstance);
end;

function _GetService(const lpServiceName: LPCTSTR): HMeService; stdcall;
begin
  Result := FMeServiceManager.GetService(lpServiceName);
end;

function _GetServiceFunction(const lpServiceFunctionName: LPCTSTR; const aProcParams: PTypeInfo): HMeServiceFunction; stdcall;
begin
  Result := FMeServiceManager.GetServiceFunction(lpServiceFunctionName, aProcParams);
end;

function _CallFunction(const hFuncHandle: HMeServiceFunction; const wParam, lParam: Cardinal): Integer; stdcall;
begin
  Result := FMeServiceManager.CallServiceFunction(hFuncHandle, wParam, lParam);
end;

function _CallFunctionEx(const hFuncHandle: HMeServiceFunction; const Params: Variant): Integer; stdcall;
begin
  Result := FMeServiceManager.CallServiceFunctionEx(hFuncHandle, Params);
end;

function _CreateEvent(const lpEventName: LPCTSTR): HMeServiceEvent; stdcall;
begin
  Result := FMeServiceManager.CreateEvent(lpEventName);
end;

function _GetEvent(const lpEventName: LPCTSTR): HMeServiceEvent; stdcall;
begin
  Result := FMeServiceManager.GetEvent(lpEventName);
end;

function _NotifyEvent(const hEventHandle: HMeServiceEvent; wParam, lParam: Cardinal): Integer; stdcall;
begin
  Result := FMeServiceManager.NotifyEvent(hEventHandle, wParam, lParam);
end;

function _HookEvent(const lpEventName: LPCTSTR; const lpProcAddr: Pointer; const lpInstance: Pointer = nil): Integer; stdcall;
begin
  Result := FMeServiceManager.HookEvent(lpEventName, lpProcAddr, lpInstance);
end;

function _UnhookEvent(const lpEventName: LPCTSTR; const lpProcAddr: Pointer; const lpInstance: Pointer = nil): Integer; stdcall;
begin
  Result := FMeServiceManager.UnHookEvent(lpEventName, lpProcAddr, lpInstance);
end;

function GetServiceInitInfo: TMeServiceInitInfo;
begin
  with Result do
  begin
    cbSize := SizeOf(TMeServiceInitInfo);
    //只有系统Service或授权的Service才能有资格创建!!应该在合适的地方对于没有权限的设置为nil.
    RegisterService := _CreateService;
    RegisterFunction := _CreateServiceFunction;
    GetService := _GetService;
    GetFunction := _GetServiceFunction;
    CallFunction := _CallFunction;
    CallFunctionEx := _CallFunctionEx;
    CreateEvent := _CreateEvent;
    GetEvent := _GetEvent;
    NotifyEvent := _NotifyEvent;
    HookEvent := _HookEvent;
    UnhookEvent := _UnhookEvent;
  end;
end;

{ TMeCustomServiceManager }
procedure TMeCustomServiceManager.Init;
begin
  inherited;
  New(FServices, Create);
end;

destructor TMeCustomServiceManager.Destroy;
begin
  with FServices.LockList^ do
  try
    FreeMeObjects;
  finally
    FServices.UnLockList;
  end;
  MeFreeAndNil(FServices);
  inherited;
end;

class function TMeCustomServiceManager.ServiceClass: TMeClass;
begin
  Result := TypeOf(TMeCustomService);
end;

function TMeCustomServiceManager.AddService(const aService: PMeCustomService): Integer;
Var
  vService: PMeCustomService;
begin
  Result := -1;
  if Assigned(aService) then
  with FServices.LockList^ do
  try
    for Result := 0 to Count - 1 do
    begin
      vService := PMeCustomService(Items[Result]);
      if Assigned(vService) and (vService.Name = aService.Name) then
        exit;
    end;
    Result := Add(aService);
  finally
    FServices.UnLockList;
  end;
end;

function TMeCustomServiceManager.CallServiceFunction(const aFuncHandle: HMeServiceFunction; const wParam, lParam: Cardinal): Integer;
begin
  if (aFuncHandle <> 0) then
    Result := PMeServiceFunction(aFuncHandle).ExecuteByParam(wParam, lParam)
  else
    Result := 0;
end;

function TMeCustomServiceManager.CallServiceFunctionEx(const aFuncHandle: HMeServiceFunction; const aParams: Variant): Integer;
begin
  {$IFDEF SUPPORTS_MESERVICE_CALLEX}
  if (aFuncHandle <> 0) then
    Result := PMeServiceFunction(aFuncHandle).ExecuteByVariant(aParams);
  else
  {$ENDIF}
    Result := 0;
end;

function TMeCustomServiceManager.CreateEvent(aName: TMeIdentity): HMeServiceEvent;
var
  vServiceName: TMeIdentity;
  vService: PMeCustomService;
begin
  vServiceName := StrRFetch(aName, MEID_SPLITER);
  vService := FindService(vServiceName);
  if Assigned(vService) then
  begin
    Result := HMeServiceFunction(vService.RegisterEvent(aName));
  end
  else
    Result := 0;
end;

function TMeCustomServiceManager.CreateService(const aName: TMeIdentity): HMeService;
var
  vService: PMeCustomService;
begin
  Result := 0;
  with FServices.LockList^ do
  try
    for Result := 0 to Count - 1 do
    begin
      vService := PMeCustomService(Items[Result]);
      if Assigned(vService) and (vService.Name = aName) then
        exit;
    end;
    vService := PMeCustomService(NewMeObject(ServiceClass));
    vService.Name := aName;
    Add(vService);
    Result := HMeService(vService);
  finally
    FServices.UnLockList;
  end;
end;

function TMeCustomServiceManager.CreateServiceFunction(aFuncName: TMeIdentity; 
  const aProcAddr: Pointer; const aProcParams: PTypeInfo = nil; const aInstance: Pointer = nil): HMeServiceFunction;
var
  vServiceName: TMeIdentity;
  vService: PMeCustomService;
begin
  vServiceName := StrRFetch(aFuncName, MEID_SPLITER);
  vService := FindService(vServiceName);
  if Assigned(vService) then
  begin
    Result := HMeServiceFunction(vService.RegisterFunction(aFuncName, aProcAddr, aProcParams, aInstance));
  end
  else
    Result := 0;
end;

procedure TMeCustomServiceManager.Delete(const index: Integer);
var
  vService: PMeCustomService;
begin
  with FServices.LockList^ do
  try
  vService := PMeCustomService(Items[index]);
    if Assigned(vService) then 
    begin
      vService.Free;
      Delete(index);
    end;
  finally
    FServices.UnLockList;
  end;
end;

function TMeCustomServiceManager.FindService(const aServiceName: TMeIdentity): PMeCustomService;
var
  i: Integer;
begin
  with FServices.LockList^ do
  try
    for i := 0 to Count - 1 do
    begin
      Result := PMeCustomService(Items[i]);
      if Assigned(Result) and (Result.Name = aServiceName) then
        exit;
    end;
  finally
    FServices.UnLockList;
  end;
  Result := nil;
end;
function TMeCustomServiceManager.GetCount: Integer;
begin
  Result := FServices.Count;
end;

function TMeCustomServiceManager.GetEvent(aName: TMeIdentity): HMeServiceEvent;
var
  vServiceName: TMeIdentity;
  vService: PMeCustomService;
begin
  vServiceName := StrRFetch(aName, MEID_SPLITER);
  vService := FindService(vServiceName);
  if Assigned(vService) then
  begin
    Result := HMeServiceEvent(vService.FindEvent(aName));
  end
  else
    Result := 0;
end;

function TMeCustomServiceManager.GetService(const aName: TMeIdentity): HMeService;
begin
  Result := HMeService(FindService(aName));
end;

function TMeCustomServiceManager.GetServiceFunction(aName: TMeIdentity; const aProcParams: PTypeInfo): HMeServiceFunction;
var
  vServiceName: TMeIdentity;
  vService: PMeCustomService;
begin
  vServiceName := StrRFetch(aName, MEID_SPLITER);
  vService := FindService(vServiceName);
  if Assigned(vService) then
  begin
    Result := HMeServiceFunction(vService.FindFunction(aName, aProcParams));
  end
  else
    Result := 0;
end;

function TMeCustomServiceManager.IndexOfService(const aServiceName: TMeIdentity): Integer;
var
  vService: PMeCustomService;
begin
  with FServices.LockList^ do
  try
    for Result := 0 to Count - 1 do
    begin
      vService := PMeCustomService(Items[Result]);
      if Assigned(vService) and (vService.Name = aServiceName) then
        exit;
    end;
  finally
    FServices.UnLockList;
  end;
  Result := -1;
end;

function TMeCustomServiceManager.HookEvent(aName: TMeIdentity; const ProcAddr: Pointer; const lpInstance: Pointer): Integer;
var
  vServiceName: TMeIdentity;
  v: Pointer;
  vMethod: TMethod;
begin
  vServiceName := StrRFetch(aName, MEID_SPLITER);
  PMeCustomService(v) := FindService(vServiceName);
  if Assigned(v) then
  begin
    v := PMeCustomService(v).FindEvent(aName);
    if Assigned(v) then
    begin
      vMethod.Code := ProcAddr;
      vMethod.Data := lpInstance;
      Result := PMeServiceEvent(v).AddListener(vMethod) + 1;
      exit;
    end;
  end;
  Result := 0;
end;

function TMeCustomServiceManager.UnhookEvent(aName: TMeIdentity; const ProcAddr: Pointer; const lpInstance: Pointer): Integer;
var
  vServiceName: TMeIdentity;
  v: Pointer;
  vMethod: TMethod;
begin
  vServiceName := StrRFetch(aName, MEID_SPLITER);
  PMeCustomService(v) := FindService(vServiceName);
  if Assigned(v) then
  begin
    v := PMeCustomService(v).FindEvent(aName);
    if Assigned(v) then
    begin
      vMethod.Code := ProcAddr;
      vMethod.Data := lpInstance;
      Result := PMeServiceEvent(v).RemoveListener(vMethod) + 1;
      exit;
    end;
  end;
  Result := 0;
end;

function TMeCustomServiceManager.NotifyEvent(const aHandle: HMeServiceEvent; const wParam, lParam: Cardinal): Integer;
begin
  if (aHandle <> 0) then
    Result := PMeServiceEvent(aHandle).Notify(wParam, lParam)
  else 
    Result := 0;
end;

{ TMeCustomService }
procedure TMeCustomService.Init;
begin
  inherited;
  New(FFunctions, Create);
  New(FEvents, Create);
end;

destructor TMeCustomService.Destroy;
begin
  with FFunctions.LockList^ do
  try
    FreeMeObjects;
  finally
    FFunctions.UnLockList;
  end;

  with FEvents.LockList^ do
  try
    FreeMeObjects;
  finally
    FEvents.UnLockList;
  end;

  MeFreeAndNil(FFunctions);
  MeFreeAndNil(FEvents);
  inherited;
end;

function TMeCustomService.FindEvent(const aName: TMeIdentity): PMeServiceEvent;
var
  i: Integer;
begin
  Result := nil;
  with FEvents.LockList^ do
  try
    for i := 0 to Count - 1 do
    begin
      Result := PMeServiceEvent(Items[i]);
      if Assigned(Result) and (Result.FName = aName) then
        exit;
    end;
  finally
    FEvents.UnLockList;
  end;
end;


function TMeCustomService.FindFunction(const aName: TMeIdentity; aProcParams: PTypeInfo = nil): PMeServiceFunction;
var
  i: Integer;
begin
  Result := nil;
  with FFunctions.LockList^ do
  try
    for i := 0 to Count - 1 do
    begin
      Result := PMeServiceFunction(Items[i]);
      if Assigned(Result) and (Result.Name = aName) and (Result.ProcParams = aProcParams) then
        exit;
    end;
  finally
    FFunctions.UnLockList;
  end;
end;

function TMeCustomService.GetFunctions: PMeThreadSafeList;
begin
  if not Assigned(FFunctions) then New(FFunctions, Create);
  Result := FFunctions;
end;

function TMeCustomService.GetEvents: PMeThreadSafeList;
begin
  if not Assigned(FEvents) then New(FEvents, Create);
  Result := FEvents;
end;

function TMeCustomService.RegisterEvent(const aName: TMeIdentity): PMeServiceEvent;
var
  i: Integer;
begin
  Result := nil;
  with FEvents.LockList^ do
  try
    for i := 0 to Count - 1 do
    begin
      Result := PMeServiceEvent(Items[i]);
      if Assigned(Result) and (Result.Name = aName) then
        exit;
    end;
    New(Result, Create);
    Result.Name := aName;
    Add(Result);
  finally
    FEvents.UnLockList;
  end;
end;

function TMeCustomService.RegisterFunction(const aName: TMeIdentity; const aFuncEntry: Pointer; aProcParams: PTypeInfo; const aInstance: Pointer): PMeServiceFunction;
var
  i: Integer;
  vClass: TClass;
begin
  Result := nil;
  with FFunctions.LockList^ do
  try
    for i := 0 to Count - 1 do
    begin
      Result := PMeServiceFunction(Items[i]);
      if Assigned(Result) and (Result.Name = aName) then
        exit;
    end;
    New(Result, Create);
    vClass := nil;
    if not Assigned(aProcParams) then
      aProcParams := TypeInfo(TMeServiceMethod);
    if Assigned(aInstance) and IsObject(aInstance) then
      vClass := TObject(aInstance).ClassType;
    Result.InitFromType(RegisterProcTypeInfo(aProcParams, vClass));
    Result.Name := aName;
    Result.Instance := aInstance;
    Result.FuncEntry := aFuncEntry;
    Result.ProcParams := aProcParams;
    Add(Result);
  finally
    FFunctions.UnLockList;
  end;
end;

{ TMeServiceEvent }
type
  PMethod = ^ TMethod;

procedure TMeServiceEvent.Init;
begin
  inherited;
  New(FListeners, Create);
end;

destructor TMeServiceEvent.Destroy;
begin
  FName := '';
  with FListeners.LockList^ do
  try
    FreePointers;
  finally
    FListeners.UnLockList;
  end;
  MeFreeAndNil(FListeners);
  inherited;
end;

function TMeServiceEvent.AddListener(const aMethod: TMethod): Integer;
var
  vItem: PMethod;
begin
  with FListeners.LockList^ do
  try
    for Result := 0 to Count - 1 do
    begin
      vItem := PMethod(Items[Result]);
      if Assigned(vItem) and (vItem.Code = aMethod.Code) and (vItem.Data = aMethod.Data) then
        exit;
    end;
    New(vItem);
    vItem^ := aMethod;
    Result := Add(vItem);
  finally
    FListeners.UnLockList;
  end;
end;

function TMeServiceEvent.IndexOfListener(const aMethod: TMethod): Integer;
var
  vItem: PMethod;
begin
  with FListeners.LockList^ do
  try
    for Result := 0 to Count - 1 do
    begin
      vItem := PMethod(Items[Result]);
      if Assigned(vItem) and (vItem.Code = aMethod.Code) and (vItem.Data = aMethod.Data) then
        exit;
    end;
  finally
    FListeners.UnLockList;
  end;
  Result :=  -1;
end;

function TMeServiceEvent.RemoveListener(const aMethod: TMethod): Integer;
var
  vItem: PMethod;
begin
  with FListeners.LockList^ do
  try
    for Result := 0 to Count - 1 do
    begin
      vItem := PMethod(Items[Result]);
      if Assigned(vItem) and (vItem.Code = aMethod.Code) and (vItem.Data = aMethod.Data) then
      begin
        Delete(Result);
        Dispose(vItem);
        exit;
      end;
    end;
    Result := -1;
  finally
    FListeners.UnLockList;
  end;
end;

function TMeServiceEvent.Notify(const wParam, lParam: Cardinal): Integer;
var
  i: Integer;
  vItem: PMethod;
begin
  with FListeners.LockList^ do
  try
    Result := 0;
    for i := 0 to Count - 1 do
    begin
      vItem := PMethod(Items[i]);
      if Assigned(vItem.Code) then
      begin
        if Assigned(vItem.Data) then
          Result := TMeServiceEventMethod(vItem^)(wParam, lParam, Result)
        else
          Result := TMeServiceEventProc(vItem.Code)(wParam, lParam, Result);
      end;
    end;
  finally
    FListeners.UnLockList;
  end;
end;


{$IFDEF MeRTTI_SUPPORT}
const
  cMeCustomServiceClassName: PChar = 'TMeCustomService';
{$ENDIF}

initialization
  SetMeVirtualMethod(TypeOf(TMeCustomService), ovtVmtParent, TypeOf(TMeAbstractService));
  SetMeVirtualMethod(TypeOf(TMeCustomServiceManager), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeServiceEvent), ovtVmtParent, TypeOf(TMeDynamicObject));


  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeCustomService), ovtVmtClassName, cMeCustomServiceClassName);
  SetMeVirtualMethod(TypeOf(TMeCustomServiceManager), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeServiceEvent), ovtVmtClassName, nil);
  {$ENDIF}
end.
