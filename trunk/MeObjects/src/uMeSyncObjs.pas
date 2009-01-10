
{ Summary collects the thread synchronization objects. }
{
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
    * The Original Code is $RCSfile: uMeSyncObjs.pas,v $.
    * The Initial Developers of the Original Code are Borland.
    * Portions created by Borland is Copyright (C) 1995-2007
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):
}

unit uMeSyncObjs;

{$I MeSetting.inc}

{$H+,X+}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  SysUtils
  , uMeObject
  ;

type
{$IFNDEF MSWINDOWS}
  PSecurityAttributes = pointer;
{$ENDIF}

  TMeSynchroObject = object(TMeDynamicObject)
  public
    procedure Acquire; virtual;
    procedure Release; virtual;
  end;

  TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError);

  TMeHandleObject = object(TMeSynchroObject)
{$IFDEF MSWINDOWS}
  protected
    FHandle: THandle;
    FLastError: Integer;
    FUseCOMWait: Boolean;
  public
    { Specify UseCOMWait to ensure that when blocked waiting for the object
      any STA COM calls back into this thread can be made. }
    constructor Create(UseCOMWait: Boolean = False);
    destructor Destroy; virtual; {override;}
{$ENDIF}
  public
    function WaitFor(Timeout: LongWord): TWaitResult; virtual;
{$IFDEF MSWINDOWS}
    property LastError: Integer read FLastError;
    property Handle: THandle read FHandle;
{$ENDIF}
  end;

  PMeEvent = ^ TMeEvent;
  TMeEvent = object(TMeHandleObject)
{$IFDEF LINUX}
  private
    FEvent: TSemaphore;
    FManualReset: Boolean;
{$ENDIF}
  public
    constructor Create(EventAttributes: PSecurityAttributes; ManualReset,
      InitialState: Boolean; const Name: string; UseCOMWait: Boolean = False); overload;
    constructor Create(UseCOMWait: Boolean = False); overload;
{$IFDEF LINUX}
    function WaitFor(Timeout: LongWord): TWaitResult; virtual; {override}
{$ENDIF}
    procedure SetEvent;
    procedure ResetEvent;
  end;

  PMeSimpleEvent = ^ TMeSimpleEvent;
  TMeSimpleEvent = TMeEvent;

  PMeMutex = ^ TMeMutex;
  TMeMutex = object(TMeHandleObject)
  public
    constructor Create(UseCOMWait: Boolean = False); overload;
    constructor Create(MutexAttributes: PSecurityAttributes; InitialOwner: Boolean; const Name: string; UseCOMWait: Boolean = False); overload;
    constructor Create(DesiredAccess: LongWord; InheritHandle: Boolean; const Name: string; UseCOMWait: Boolean = False); overload;
    procedure Acquire; virtual; {override;}
    procedure Release; virtual; {override;}
  end;

  PMeCriticalSection = ^ TMeCriticalSection;
  TMeCriticalSection = object(TMeSynchroObject)
  protected
    FSection: TRTLCriticalSection;

    procedure Init; virtual; {override}
  public
    destructor Destroy; virtual; {override;}
    procedure Acquire; virtual; {override;}
    procedure Release; virtual; {override;}
    function TryEnter: Boolean;
    procedure Enter;
    procedure Leave;
  end;

{$IFDEF LINUX}
const
  INFINITE = $FFFFFFFF;
{$ENDIF LINUX}

implementation

{$IFDEF MSWINDOWS}
type
  TCoWaitForMultipleHandlesProc = function (dwFlags: DWORD; dwTimeOut: DWORD;
    cHandles: LongWord; var Handles; var lpdwIndex: DWORD): HRESULT; stdcall;

var
  CoWaitForMultipleHandlesProc: TCoWaitFormultipleHandlesProc;

threadvar
  OleThreadWnd: HWND;

const
  OleThreadWndClassName = 'OleMainThreadWndClass'; //do not localize
  COWAIT_WAITALL = $00000001;
  COWAIT_ALERTABLE = $00000002;

  {$IFNDEF COMPILER8_UP}
  //the delphi7 below have not these constants.
  { OLE has sent a request and is waiting for a reply. }
  RPC_S_CALLPENDING = HRESULT($80010115);
  {$EXTERNALSYM RPC_S_CALLPENDING}

  { This operation returned because the timeout period expired. }
  RPC_E_TIMEOUT = HRESULT($8001011F);
  {$EXTERNALSYM RPC_E_TIMEOUT}
  {$ENDIF}

function GetOleThreadWindow: HWND;
var
  ChildWnd: HWND;
  ParentWnd: HWND;
begin
  if (OleThreadWnd = 0) or not IsWindow(OleThreadWnd) then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
      ParentWnd := HWND(HWND_MESSAGE)
    else
      ParentWnd := 0;
    ChildWnd := 0;
    repeat
      OleThreadWnd := FindWindowEx(ParentWnd, ChildWnd, OleThreadWndClassName, nil);
      ChildWnd := OleThreadWnd;
    until (OleThreadWnd = 0) or (GetWindowThreadProcessId(OleThreadWnd, nil) = GetCurrentThreadId);
  end;
  Result := OleThreadWnd;
end;

function InternalCoWaitForMultipleHandles(dwFlags: DWORD; dwTimeOut: DWORD;
  cHandles: LongWord; var Handles; var lpdwIndex: DWORD): HRESULT; stdcall;
var
  WaitResult: DWORD;
  OleThreadWnd: HWnd;
  Msg: TMsg;
begin
  WaitResult := 0; // supress warning
  OleThreadWnd := GetOleThreadWindow;
  if OleThreadWnd <> 0 then
    while True do
    begin
      WaitResult := MsgWaitForMultipleObjectsEx(cHandles, Handles, dwTimeOut, QS_ALLEVENTS, dwFlags);
      if WaitResult = WAIT_OBJECT_0 + cHandles then
      begin
        if PeekMessage(Msg, OleThreadWnd, 0, 0, PM_REMOVE) then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end else
        Break;
    end
  else
    WaitResult := WaitForMultipleObjectsEx(cHandles, @Handles,
      dwFlags and COWAIT_WAITALL <> 0, dwTimeOut, dwFlags and COWAIT_ALERTABLE <> 0);
  if WaitResult = WAIT_TIMEOUT then
    Result := RPC_E_TIMEOUT
  else if WaitResult = WAIT_IO_COMPLETION then
    Result := RPC_S_CALLPENDING
  else
  begin
    Result := S_OK;
    if (WaitResult >= WAIT_ABANDONED_0) and (WaitResult < WAIT_ABANDONED_0 + cHandles) then
      lpdwIndex := WaitResult - WAIT_ABANDONED_0
    else
      lpdwIndex := WaitResult - WAIT_OBJECT_0;
  end;
end;

function CoWaitForMultipleHandles(dwFlags: DWORD; dwTimeOut: DWORD;
  cHandles: LongWord; var Handles; var lpdwIndex: DWORD): HRESULT;

  procedure LookupProc;
  var
    Ole32Handle: HMODULE;
  begin
    Ole32Handle := GetModuleHandle('ole32.dll'); //do not localize
    if Ole32Handle <> 0 then
      CoWaitForMultipleHandlesProc := GetProcAddress(Ole32Handle, 'CoWaitForMultipleHandles'); //do not localize
    if not Assigned(CoWaitForMultipleHandlesProc) then
      CoWaitForMultipleHandlesProc := InternalCoWaitForMultipleHandles;
  end;

begin
  if not Assigned(CoWaitForMultipleHandlesProc) then
    LookupProc;
  Result := CoWaitForMultipleHandlesProc(dwFlags, dwTimeOut, cHandles, Handles, lpdwIndex)
end;
{$ENDIF MSWINDOWS}

{ TMeSynchroObject }

procedure TMeSynchroObject.Acquire;
begin
end;

procedure TMeSynchroObject.Release;
begin
end;

{ TMeHandleObject }

{$IFDEF MSWINDOWS}
constructor TMeHandleObject.Create(UseComWait: Boolean);
begin
  inherited Create;
  FUseCOMWait := UseCOMWait;
end;

destructor TMeHandleObject.Destroy;
begin
  CloseHandle(FHandle);
  inherited Destroy;
end;

{$ENDIF}

function TMeHandleObject.WaitFor(Timeout: LongWord): TWaitResult;
var
  Index: DWORD;
begin
{$IFDEF MSWINDOWS}
  if FUseCOMWait then
  begin
    case CoWaitForMultipleHandles(0, TimeOut, 1, FHandle, Index) of
      S_OK: Result := wrSignaled;
      RPC_S_CALLPENDING,
      RPC_E_TIMEOUT: Result := wrTimeout;
    else
      Result := wrError;
      FLastError := GetLastError;
    end;
  end else
  begin
    case WaitForSingleObject(Handle, Timeout) of
      WAIT_ABANDONED: Result := wrAbandoned;
      WAIT_OBJECT_0: Result := wrSignaled;
      WAIT_TIMEOUT: Result := wrTimeout;
      WAIT_FAILED:
        begin
          Result := wrError;
          FLastError := GetLastError;
        end;
    else
      Result := wrError;
    end;
  end;
{$ENDIF}
{$IFDEF LINUX}
  Result := wrError;
{$ENDIF}
end;

{ TMeEvent }
constructor TMeEvent.Create(EventAttributes: PSecurityAttributes; ManualReset,
  InitialState: Boolean; const Name: string; UseCOMWait: Boolean);
{$IFDEF MSWINDOWS}
begin
  inherited Create(UseCOMWait);
  FHandle := CreateEvent(EventAttributes, ManualReset, InitialState, PChar(Name));
end;
{$ENDIF}
{$IFDEF LINUX}
var
   Value: Integer;
begin
  if InitialState then
    Value := 1
  else
    Value := 0;

  FManualReset := ManualReset;

  sem_init(FEvent, False, Value);
end;
{$ENDIF}

constructor TMeEvent.Create(UseCOMWait: Boolean);
begin
  Create(nil, True, False, '', UseCOMWait);
end;

{$IFDEF LINUX}
function TMeEvent.WaitFor(Timeout: LongWord): TWaitResult;
begin
  if Timeout = LongWord(INFINITE) then
  begin
    sem_wait(FEvent);
    Result := wrSignaled;
  end
  else if FManualReset then
    sem_post(FEvent)
  else
    Result := wrError;
end;
{$ENDIF}

procedure TMeEvent.SetEvent;
{$IFDEF MSWINDOWS}
begin
  Windows.SetEvent(Handle);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  I: Integer;
begin
  sem_getvalue(FEvent, I);
  if I = 0 then
    sem_post(FEvent);
end;
{$ENDIF}

procedure TMeEvent.ResetEvent;
begin
{$IFDEF MSWINDOWS}
  Windows.ResetEvent(Handle);
{$ENDIF}
{$IFDEF LINUX}
  while sem_trywait(FEvent) = 0 do { nothing };
{$ENDIF}
end;

{ TMeCriticalSection }

procedure TMeCriticalSection.Init;
begin
  inherited;
  InitializeCriticalSection(FSection);
end;

destructor TMeCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

procedure TMeCriticalSection.Acquire;
begin
  EnterCriticalSection(FSection);
end;

procedure TMeCriticalSection.Release;
begin
  LeaveCriticalSection(FSection);
end;

function TMeCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FSection);
end;

procedure TMeCriticalSection.Enter;
begin
  Acquire;
end;

procedure TMeCriticalSection.Leave;
begin
  Release;
end;

{ TMeMutex }

procedure TMeMutex.Acquire;
begin
  if WaitFor(INFINITE) = wrError then
    RaiseLastOSError;
end;

constructor TMeMutex.Create(UseCOMWait: Boolean);
begin
  Create(nil, False, '', UseCOMWait);
end;

constructor TMeMutex.Create(MutexAttributes: PSecurityAttributes;
  InitialOwner: Boolean; const Name: string; UseCOMWait: Boolean);
var
  lpName: PChar;
begin
  inherited Create(UseCOMWait);
  if Name <> '' then
    lpName := PChar(Name)
  else
    lpName := nil;
  {$IFDEF MSWINDOWS}
  FHandle := CreateMutex(MutexAttributes, InitialOwner, lpName);
  if FHandle = 0 then
    RaiseLastOSError;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  {$Message Warning 'the mutex has not been implemeted yet on linux.'}
  {$ENDIF LINUX}
end;

constructor TMeMutex.Create(DesiredAccess: LongWord; InheritHandle: Boolean;
  const Name: string; UseCOMWait: Boolean);
var
  lpName: PChar;
begin
  inherited Create(UseCOMWait);
  if Name <> '' then
    lpName := PChar(Name)
  else
    lpName := nil;
  {$IFDEF MSWINDOWS}
  FHandle := OpenMutex(DesiredAccess, InheritHandle, lpName);
  if FHandle = 0 then
    RaiseLastOSError;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  {$Message Warning 'the mutex has not been implemeted yet on linux.'}
  {$ENDIF LINUX}
end;

procedure TMeMutex.Release;
begin
  {$IFDEF MSWINDOWS}
  if not ReleaseMutex(FHandle) then
    RaiseLastOSError;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  {$Message Warning 'the mutex has not been implemeted yet on linux.'}
  {$ENDIF LINUX}
end;

initialization
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeSynchroObject), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeHandleObject), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeEvent), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeMutex), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeCriticalSection), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeSynchroObject), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeHandleObject), ovtVmtParent, TypeOf(TMeSynchroObject));
  SetMeVirtualMethod(TypeOf(TMeEvent), ovtVmtParent, TypeOf(TMeHandleObject));
  SetMeVirtualMethod(TypeOf(TMeMutex), ovtVmtParent, TypeOf(TMeHandleObject));
  SetMeVirtualMethod(TypeOf(TMeCriticalSection), ovtVmtParent, TypeOf(TMeSynchroObject));
end.
