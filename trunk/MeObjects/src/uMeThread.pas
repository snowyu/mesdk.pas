{Summary MeThread - the Thread object for the MeSDK Core.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.15 $

  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
      or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeThread.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.

    * Contributor(s):
}

unit uMeThread;

interface

{$I MeSetting.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  SysUtils
  //, Classes
  , uMeObject
  , uMeSystem
  , uMeSyncObjs
  ;

type
  PMeCustomThread = ^ TMeCustomThread;

  TMeCustomThreadMethod = procedure of object;
{$IFDEF MSWINDOWS}
  TMeThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest,
    tpTimeCritical);
{$ENDIF}

  PSynchronizeRecord = ^TSynchronizeRecord;
  TSynchronizeRecord = record
    FThread: PMeCustomThread;
    FMethod: TMeCustomThreadMethod;
    FSynchronizeException: TObject;
  end;

  { used to instead of the Borland Thread Class }
  TMeCustomThread = object(TMeDynamicObject)
  protected
{$IFDEF MSWINDOWS}
    FHandle: THandle;
    FThreadID: THandle;
{$ENDIF}
{$IFDEF LINUX}
    // ** FThreadID is not THandle in Linux **
    FThreadID: Cardinal;
    FCreateSuspendedSem: TSemaphore;
    FInitialSuspendDone: Boolean;
{$ENDIF}
    FCreateSuspended: Boolean;
    FTerminated: Boolean;
    FSuspended: Boolean;
    FFreeOnTerminate: Boolean;
    FFinished: Boolean;
    FReturnValue: Integer;
    FOnTerminate: TMeNotifyEvent;
    FSynchronize: TSynchronizeRecord;
    FFatalException: TObject;
    procedure CallOnTerminate;
    class procedure Synchronize(ASyncRec: PSynchronizeRecord; QueueEvent: Boolean = False); overload;
{$IFDEF MSWINDOWS}
    function GetPriority: TMeThreadPriority;
    procedure SetPriority(Value: TMeThreadPriority);
{$ENDIF}
{$IFDEF LINUX}
    // ** Priority is an Integer value in Linux
    function GetPriority: Integer;
    procedure SetPriority(Value: Integer);
    function GetPolicy: Integer;
    procedure SetPolicy(Value: Integer);
{$ENDIF}
    procedure SetSuspended(Value: Boolean);
  protected
    procedure CheckThreadError(ErrCode: Integer); overload;
    procedure CheckThreadError(Success: Boolean); overload;
    procedure DoTerminate; virtual;
    procedure Execute; virtual; abstract;
    procedure Queue(AMethod: TMeCustomThreadMethod); overload;
    procedure Synchronize(AMethod: TMeCustomThreadMethod); overload;
    property ReturnValue: Integer read FReturnValue write FReturnValue;
    property Terminated: Boolean read FTerminated;
    procedure Init; virtual; //override
  public
    constructor Create(const CreateSuspended: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS}= True {$ENDIF});
    destructor Destroy; virtual; //override;
    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    function WaitFor: LongWord;
    class procedure Queue(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod); overload;
    class procedure RemoveQueuedEvents(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod);
    class procedure StaticQueue(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod);
    class procedure Synchronize(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod); overload;
    class procedure StaticSynchronize(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod);
    property FatalException: TObject read FFatalException;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
{$IFDEF MSWINDOWS}
    property Handle: THandle read FHandle;
    property Priority: TMeThreadPriority read GetPriority write SetPriority;
{$ENDIF}
{$IFDEF LINUX}
    // ** Priority is an Integer **
    property Priority: Integer read GetPriority write SetPriority;
    property Policy: Integer read GetPolicy write SetPolicy;
{$ENDIF}
    property Suspended: Boolean read FSuspended write SetSuspended;
{$IFDEF MSWINDOWS}
    property ThreadID: THandle read FThreadID;
{$ENDIF}
{$IFDEF LINUX}
    // ** ThreadId is Cardinal **
    property ThreadID: Cardinal read FThreadID;
{$ENDIF}
    property OnTerminate: TMeNotifyEvent read FOnTerminate write FOnTerminate;
  end;

{ Assign a method to WakeMainThread in order to properly force an event into
  the GUI thread's queue.  This will make sure that non-GUI threads can quickly
  synchronize with the GUI thread even if no events are being processed due to
  an idle state }
var
  WakeMainThread: TMeNotifyEvent = nil;
{$IF Defined(MSWINDOWS)}
{ SyncEvent is an Event handle that is signaled every time a thread wishes to
  synchronize with the main thread or is terminating.  This handle us suitable
  for use with WaitForMultipleObjects.  When this object is signaled,
  CheckSynchronize *must* be called in order to reset the event.  Do not call
  ResetEvent on this handle, or background threads may hang waiting for
  Synchronize to return.
}
  SyncEvent: THandle;
{$ELSEIF Defined(LINUX)}
{ SyncEvent is a set of file descriptors representing a pipe.  The ReadDes field
  is suitable for use within a select or poll call.  When this file descriptor
  is signaled, a background thread wishes to synchronize with the main thread
  or is terminating.  When the ReadDes file descriptor is signaled,
  CheckSynchronize *must* be called to reset the file descriptor.  Do *not*
  actually call __read (or any other "read" function) with this file descriptor
  as that may cause a background thread to hang waiting for Synchronize to return.
}
  SyncEvent: TPipeDescriptors;
{$IFEND}

implementation

{$IFDEF MSWINDOWS}
uses RTLConsts, SysConst, Types;
{$ENDIF}
{$IFDEF LINUX}
uses KernelIoctl, RTLConsts, SysConst;
{$ENDIF}

type
  TSyncProc = record
    SyncRec: PSynchronizeRecord;
    Queued: Boolean;
{$IFDEF MSWINDOWS}
    Signal: THandle;
{$ENDIF}
{$IFDEF LINUX}
    Signal: TCondVar;
{$ENDIF}
  end;
  PSyncProc = ^TSyncProc;

var
  SyncList: PMeList = nil;
  ThreadLock: TRTLCriticalSection;
  ThreadCount: Integer;

procedure InitThreadSynchronization;
begin
  InitializeCriticalSection(ThreadLock);
{$IF Defined(MSWINDOWS)}
  SyncEvent := CreateEvent(nil, True, False, '');
  if SyncEvent = 0 then
    RaiseLastOSError;
{$ELSEIF Defined(LINUX)}
  if pipe(SyncEvent) < 0 then
    RaiseLastOSError;
{$IFEND}
end;

procedure DoneThreadSynchronization;
begin
  DeleteCriticalSection(ThreadLock);
{$IF Defined(MSWINDOWS)}
  CloseHandle(SyncEvent);
{$ELSEIF Defined(LINUX)}
  __close(SyncEvent.ReadDes);
  __close(SyncEvent.WriteDes);
{$IFEND}
end;

procedure ResetSyncEvent;
{$IF Defined(LINUX)}
var
  nRead: Integer;
  Dummy: Byte;
{$IFEND}
begin
{$IF Defined(MSWINDOWS)}
  ResetEvent(SyncEvent);
{$ELSEIF Defined(LINUX)}
  if (ioctl(SyncEvent.ReadDes, FIONREAD, @nRead) = 0) and (nRead > 0) then
    __read(SyncEvent.ReadDes, Dummy, SizeOf(Dummy));
{$IFEND}
end;

procedure WaitForSyncEvent(Timeout: Integer);
{$IF Defined(LINUX)}
var
  EventFds: TFDSet;
  Tm: TTimeVal;
{$IFEND}
begin
{$IF Defined(MSWINDOWS)}
  if WaitForSingleObject(SyncEvent, Timeout) = WAIT_OBJECT_0 then
    ResetSyncEvent;
{$ELSEIF Defined(LINUX)}
  FD_ZERO(EventFds);
  FD_SET(SyncEvent.ReadDes, EventFds);
  Tm.tv_sec := Timeout div 1000;
  Tm.tv_usec := (Timeout mod 1000) * 1000;
  if select(SyncEvent.ReadDes + 1, @EventFds, nil, nil, @Tm) > 0 then
    ResetSyncEvent;
{$IFEND}
end;

procedure SignalSyncEvent;
{$IF Defined(LINUX)}
const
  Dummy: Byte = 0;
var
  nRead: Integer;
{$IFEND}
begin
{$IF Defined(MSWINDOWS)}
  SetEvent(SyncEvent);
{$ELSEIF Defined(LINUX)}
  if (ioctl(SyncEvent.ReadDes, FIONREAD, @nRead) = 0) and (nRead = 0) then
    __write(SyncEvent.WriteDes, Dummy, SizeOf(Dummy));
{$IFEND}
end;

procedure AddThread;
begin
  InterlockedIncrement(ThreadCount);
end;

procedure RemoveThread;
begin
  InterlockedDecrement(ThreadCount);
end;

function CheckSynchronize(Timeout: Integer = 0): Boolean;
var
  SyncProc: PSyncProc;
  LocalSyncList: PMeList;
begin
  if GetCurrentThreadID <> MainThreadID then
    raise EMeError.CreateResFmt(@SCheckSynchronizeError, [GetCurrentThreadID]);
  if Timeout > 0 then
    WaitForSyncEvent(Timeout)
  else
    ResetSyncEvent;
  LocalSyncList := nil;
  EnterCriticalSection(ThreadLock);
  try
    Integer(LocalSyncList) := InterlockedExchange(Integer(SyncList), Integer(LocalSyncList));
    try
      Result := (LocalSyncList <> nil) and (LocalSyncList.Count > 0);
      if Result then
      begin
        while LocalSyncList.Count > 0 do
        begin
          SyncProc := LocalSyncList.Items[0];
          LocalSyncList.Delete(0);
          LeaveCriticalSection(ThreadLock);
          try
            try
              SyncProc.SyncRec.FMethod;
            except
              if not SyncProc.Queued then
                SyncProc.SyncRec.FSynchronizeException := AcquireExceptionObject
              else
                raise;
            end;
          finally
            EnterCriticalSection(ThreadLock);
          end;
          if not SyncProc.Queued then
{$IFDEF MSWINDOWS}
            SetEvent(SyncProc.Signal)
{$ENDIF}
{$IFDEF LINUX}
            pthread_cond_signal(SyncProc.Signal)
{$ENDIF}
          else
          begin
            Dispose(SyncProc.SyncRec);
            Dispose(SyncProc);
          end;
        end;
      end;
    finally
      MeFreeAndNil(LocalSyncList);
    end;
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

function ThreadProc(Thread: PMeCustomThread): Integer;
var
  FreeThread: Boolean;
begin
{$IFDEF LINUX}
  if Thread.FSuspended then sem_wait(Thread.FCreateSuspendedSem);
{$ENDIF}
  try
    if not Thread.Terminated then
    try
      Thread.Execute;
    except
      Thread.FFatalException := AcquireExceptionObject;
    end;
  finally
    FreeThread := Thread.FFreeOnTerminate;
    Result := Thread.FReturnValue;
    Thread.DoTerminate;
    Thread.FFinished := True;
    SignalSyncEvent;
    if FreeThread then Thread.Free;
{$IFDEF MSWINDOWS}
    EndThread(Result);
{$ENDIF}
{$IFDEF LINUX}
    // Directly call pthread_exit since EndThread will detach the thread causing
    // the pthread_join in TMeCustomThread.WaitFor to fail.  Also, make sure the EndThreadProc
    // is called just like EndThread would do. EndThreadProc should not return
    // and call pthread_exit itself.
    if Assigned(EndThreadProc) then
      EndThreadProc(Result);
    pthread_exit(Pointer(Result));
{$ENDIF}
  end;
end;

procedure TMeCustomThread.Init;
{$IFDEF LINUX}
var
  ErrCode: Integer;
{$ENDIF}
begin
  inherited;
  AddThread;
{$IFDEF MSWINDOWS}
  FHandle := BeginThread(nil, 0, @ThreadProc, @Self, CREATE_SUSPENDED, FThreadID);
  if FHandle = 0 then
    raise EMeError.CreateResFmt(@SThreadCreateError, [SysErrorMessage(GetLastError)]);
{$ENDIF}
{$IFDEF LINUX}
  sem_init(FCreateSuspendedSem, False, 0);
  ErrCode := BeginThread(nil, @ThreadProc, Pointer(Self), FThreadID);
  if ErrCode <> 0 then
    raise EMeError.CreateResFmt(@SThreadCreateError, [SysErrorMessage(ErrCode)]);
{$ENDIF}
end;

constructor TMeCustomThread.Create(const CreateSuspended: Boolean);
begin
  inherited Create;
  FCreateSuspended := CreateSuspended;
  if not FCreateSuspended then
    Resume;
end;

destructor TMeCustomThread.Destroy;
begin
  if (FThreadID <> 0) and not FFinished then
  begin
    Terminate;
    if FCreateSuspended then
      Resume;
    WaitFor;
  end;
  RemoveQueuedEvents(@Self, nil);
{$IFDEF MSWINDOWS}
  if FHandle <> 0 then CloseHandle(FHandle);
{$ENDIF}
{$IFDEF LINUX}
  // This final check is to ensure that even if the thread was never waited on
  // its resources will be freed.
  if FThreadID <> 0 then pthread_detach(FThreadID);
  sem_destroy(FCreateSuspendedSem);
{$ENDIF}
  inherited Destroy;
  FFatalException.Free;
  RemoveThread;
end;

procedure TMeCustomThread.CheckThreadError(ErrCode: Integer);
begin
  if ErrCode <> 0 then
    raise EMeError.CreateResFmt(@SThreadError, [SysErrorMessage(ErrCode), ErrCode]);
end;

procedure TMeCustomThread.CheckThreadError(Success: Boolean);
begin
  if not Success then
    CheckThreadError(GetLastError);
end;

procedure TMeCustomThread.CallOnTerminate;
begin
  if Assigned(FOnTerminate) then FOnTerminate(@Self);
end;

procedure TMeCustomThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then Synchronize(CallOnTerminate);
end;

{$IFDEF MSWINDOWS}
const
  Priorities: array [TMeThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

function TMeCustomThread.GetPriority: TMeThreadPriority;
var
  P: Integer;
  I: TMeThreadPriority;
begin
  P := GetThreadPriority(FHandle);
  CheckThreadError(P <> THREAD_PRIORITY_ERROR_RETURN);
  Result := tpNormal;
  for I := Low(TMeThreadPriority) to High(TMeThreadPriority) do
    if Priorities[I] = P then Result := I;
end;

procedure TMeCustomThread.SetPriority(Value: TMeThreadPriority);
begin
  CheckThreadError(SetThreadPriority(FHandle, Priorities[Value]));
end;
{$ENDIF}
{$IFDEF LINUX}
function TMeCustomThread.GetPriority: Integer;
var
  P: Integer;
  J: TSchedParam;
begin
  {
    Linux Priority is based on the Schedule policy.
    There are 3 different kinds of policy.  See SetPolicy.

        Policy          Type         Priority
      ----------      --------       --------
      SCHED_RR        RealTime         1-99
      SCHED_FIFO      RealTime         1-99
      SCHED_OTHER     Regular           0

    SCHED_RR and SCHED_FIFO can only be set by root.
  }
  CheckThreadError(pthread_getschedparam(FThreadID, P, J));
  Result := J.sched_priority;
end;

{
  Note that to fully utilize Linux Scheduling, see SetPolicy.
}
procedure TMeCustomThread.SetPriority(Value: Integer);
var
  P: TSchedParam;
begin
  if Value <> Priority then
  begin
    P.sched_priority := Value;
    CheckThreadError(pthread_setschedparam(FThreadID, Policy, @P));
  end;
end;

function TMeCustomThread.GetPolicy: Integer;
var
  J: TSchedParam;
begin
  CheckThreadError(pthread_getschedparam(FThreadID, Result, J));
end;

{
  Note that to fully utilize Linux Scheduling, SetPolicy needs to
  be used as well.  See SetPriority for the relationship between these
  methods.
}
procedure TMeCustomThread.SetPolicy(Value: Integer);
var
  P: TSchedParam;
begin
  if Value <> Policy then
  begin
    P.sched_priority := GetPriority;
    CheckThreadError(pthread_setschedparam(FThreadID, Value, @P));
  end;
end;
{$ENDIF}

procedure TMeCustomThread.Queue(AMethod: TMeCustomThreadMethod);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  try
    LSynchronize.FThread := @Self;
    LSynchronize.FSynchronizeException := nil;
    LSynchronize.FMethod := AMethod;
    Synchronize(LSynchronize, True);
  finally
    if MainThreadID = GetCurrentThreadID then
      Dispose(LSynchronize);
  end;
end;

class procedure TMeCustomThread.Queue(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod);
var
  LSynchronize: PSynchronizeRecord;
begin
  if AThread <> nil then
    AThread.Queue(AMethod)
  else
  begin
    New(LSynchronize);
    try
      LSynchronize.FThread := nil;
      LSynchronize.FSynchronizeException := nil;
      LSynchronize.FMethod := AMethod;
      Synchronize(LSynchronize, True);
    finally
      if MainThreadID = GetCurrentThreadID then
        Dispose(LSynchronize);
    end;
  end;
end;

class procedure TMeCustomThread.RemoveQueuedEvents(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod);
var
  I: Integer;
  SyncProc: PSyncProc;
begin
  EnterCriticalSection(ThreadLock);
  try
    if SyncList <> nil then
      for I := SyncList.Count - 1 downto 0 do
      begin
        SyncProc := SyncList.Items[I];
        if (SyncProc.Signal = 0) and
          (((AThread <> nil) and (SyncProc.SyncRec.FThread = AThread)) or
            (Assigned(AMethod) and (TMethod(SyncProc.SyncRec.FMethod).Code = TMethod(AMethod).Code) and
             (TMethod(SyncProc.SyncRec.FMethod).Data = TMethod(AMethod).Data))) then
        begin
          SyncList.Delete(I);
          Dispose(SyncProc.SyncRec);
          Dispose(SyncProc);
        end;
      end;
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

class procedure TMeCustomThread.StaticQueue(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod);
begin
  Queue(AThread, AMethod);
end;

class procedure TMeCustomThread.Synchronize(ASyncRec: PSynchronizeRecord; QueueEvent: Boolean = False);
var
  SyncProc: TSyncProc;
  SyncProcPtr: PSyncProc;
begin
  if GetCurrentThreadID = MainThreadID then
    ASyncRec.FMethod
  else
  begin
    if QueueEvent then
      New(SyncProcPtr)
    else
      SyncProcPtr := @SyncProc;
{$IFDEF MSWINDOWS}
    if not QueueEvent then
      SyncProcPtr.Signal := CreateEvent(nil, True, False, nil)
    else
      SyncProcPtr.Signal := 0;
    try
{$ENDIF}
{$IFDEF LINUX}
      FillChar(SyncProcPtr^, SizeOf(SyncProcPtr^), 0);  // This also initializes the cond_var
{$ENDIF}
      EnterCriticalSection(ThreadLock);
      try
        SyncProcPtr.Queued := QueueEvent;
        if SyncList = nil then
          New(SyncList, Create);
        SyncProcPtr.SyncRec := ASyncRec;
        SyncList.Add(SyncProcPtr);
        SignalSyncEvent;
        if Assigned(WakeMainThread) then
          WakeMainThread(SyncProcPtr.SyncRec.FThread);
        if not QueueEvent then
{$IFDEF MSWINDOWS}
        begin
          LeaveCriticalSection(ThreadLock);
          try
            WaitForSingleObject(SyncProcPtr.Signal, INFINITE);
          finally
            EnterCriticalSection(ThreadLock);
          end;
        end;
{$ENDIF}
{$IFDEF LINUX}
          pthread_cond_wait(SyncProcPtr.Signal, ThreadLock);
{$ENDIF}
      finally
        LeaveCriticalSection(ThreadLock);
      end;
{$IFDEF MSWINDOWS}
    finally
      if not QueueEvent then
        CloseHandle(SyncProcPtr.Signal);
    end;
{$ENDIF}
    if not QueueEvent and Assigned(ASyncRec.FSynchronizeException) then
      raise ASyncRec.FSynchronizeException;
  end;
end;

procedure TMeCustomThread.Synchronize(AMethod: TMeCustomThreadMethod);
begin
  FSynchronize.FThread := @Self;
  FSynchronize.FSynchronizeException := nil;
  FSynchronize.FMethod := AMethod;
  Synchronize(@FSynchronize);
end;

class procedure TMeCustomThread.Synchronize(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod);
var
  SyncRec: TSynchronizeRecord;
begin
  if AThread <> nil then
    AThread.Synchronize(AMethod)
  else
  begin
    SyncRec.FThread := nil;
    SyncRec.FSynchronizeException := nil;
    SyncRec.FMethod := AMethod;
    TMeCustomThread.Synchronize(@SyncRec);
  end;
end;

class procedure TMeCustomThread.StaticSynchronize(AThread: PMeCustomThread; AMethod: TMeCustomThreadMethod);
begin
  Synchronize(AThread, AMethod);
end;

procedure TMeCustomThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend
    else
      Resume;
end;

procedure TMeCustomThread.Suspend;
var
  OldSuspend: Boolean;
begin
  OldSuspend := FSuspended;
  try
    FSuspended := True;
{$IFDEF MSWINDOWS}
    CheckThreadError(Integer(SuspendThread(FHandle)) >= 0);
{$ENDIF}
{$IFDEF LINUX}
    CheckThreadError(pthread_kill(FThreadID, SIGSTOP));
{$ENDIF}
  except
    FSuspended := OldSuspend;
    raise;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TMeCustomThread.Resume;
var
  SuspendCount: Integer;
begin
  SuspendCount := ResumeThread(FHandle);
  CheckThreadError(SuspendCount >= 0);
  if SuspendCount = 1 then
    FSuspended := False;
end;
{$ENDIF}
{$IFDEF LINUX}
{
  About Suspend and Resume. POSIX does not support suspending/resuming a thread.
  Suspending a thread is considerd dangerous since it is not guaranteed where the
  thread would be suspend. It might be holding a lock, mutex or it might be inside
  a critical section.  In order to simulate it in Linux we've used signals. To
  suspend, a thread SIGSTOP is sent and to resume, SIGCONT is sent. Note that this
  is Linux only i.e. according to POSIX if a thread receives SIGSTOP then the
  entire process is stopped. However Linux doesn't entirely exhibit the POSIX-mandated
  behaviour. If and when it fully complies with the POSIX standard then suspend
  and resume won't work.
}
procedure TMeCustomThread.Resume;
begin
  if not FInitialSuspendDone then
  begin
    FInitialSuspendDone := True;
    sem_post(FCreateSuspendedSem);
  end else
    CheckThreadError(pthread_kill(FThreadID, SIGCONT));
  FSuspended := False;
end;
{$ENDIF}

procedure TMeCustomThread.Terminate;
begin
  FTerminated := True;
end;

function TMeCustomThread.WaitFor: LongWord;
{$IFDEF MSWINDOWS}
var
  H: array[0..1] of THandle;
  WaitResult: Cardinal;
  Msg: TMsg;
begin
  H[0] := FHandle;
  if GetCurrentThreadID = MainThreadID then
  begin
    WaitResult := 0;
    H[1] := SyncEvent;
    repeat
      { This prevents a potential deadlock if the background thread
        does a SendMessage to the foreground thread }
      if WaitResult = WAIT_OBJECT_0 + 2 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      WaitResult := MsgWaitForMultipleObjects(2, H, False, 1000, QS_SENDMESSAGE);
      CheckThreadError(WaitResult <> WAIT_FAILED);
      if WaitResult = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
    until WaitResult = WAIT_OBJECT_0;
  end else WaitForSingleObject(H[0], INFINITE);
  CheckThreadError(GetExitCodeThread(H[0], Result));
end;
{$ENDIF}
{$IFDEF LINUX}
var
  X: Pointer;
  ID: Cardinal;
begin
  ID := FThreadID;
  if GetCurrentThreadID = MainThreadID then
    while not FFinished do
      CheckSynchronize(1000);
  FThreadID := 0;
  X := @Result;
  CheckThreadError(pthread_join(ID, X));
end;
{$ENDIF}

initialization
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeCustomThread), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeCustomThread), ovtVmtParent, TypeOf(TMeDynamicObject));

  InitThreadSynchronization;
finalization
  MeFreeAndNil(SyncList);
  DoneThreadSynchronization;
  //writeln('ThreadCount=',ThreadCount);
end.
