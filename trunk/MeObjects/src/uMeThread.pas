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
    * Portions created by Andreas Hausladen are Copyright (C) 2006-2008 Andreas Hausladen.
    * Portions created by Indy Pit Crew are Copyright (C) 1993-2005, Chad Z. Hower and the Indy Pit Crew
    * All rights reserved.

    * Contributor(s):
      Andreas Hausladen
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

resourcestring
  RsAsyncCallNotFinished = 'The asynchronous call is not finished yet';
  RsLeaveMainThreadNestedError = 'Unpaired call to AsyncCalls.LeaveMainThread()';
  RsLeaveMainThreadThreadError = 'AsyncCalls.LeaveMainThread() was called outside of the main thread';
  RsThreadTerminateAndWaitFor  = 'Cannot call TerminateAndWaitFor on FreeAndTerminate threads';

const
  cWaitAllThreadsTerminatedCount = 1 * 60 * 1000;
  cWaitAllThreadsTerminatedStep  = 250;

type
  PMeCustomThread = ^ TMeCustomThread;
  PMeThread = ^ TMeThread;
  PMeYarn = ^ TMeYarn;
  PMeThreadWithTask = ^ TMeThreadWithTask;
  PMeTask = ^ TMeTask;

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
    procedure Synchronize(const aMethod: TMeCustomThreadMethod); overload;
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

  { Summary: the abstract task object }
  TMeTask = object(TMeDynamicObject)
  protected
    FBeforeRunDone: Boolean;

    procedure AfterRun; virtual;
    procedure BeforeRun; virtual;
    function Run: Boolean; virtual; abstract;
    procedure HandleException(const aException: Exception); virtual;
  public
    // The Do's are separate so we can add events later if necessary without
    // needing the inherited calls to perform them, as well as allowing
    // us to keep the real runs as protected
    procedure DoAfterRun;
    procedure DoBeforeRun;
    function DoRun: Boolean;
    procedure DoException(const aException: Exception);
    // BeforeRunDone property to allow flexibility in alternative schedulers
    // it will be set to true before executing the DoBeforeRun.
    property BeforeRunDone: Boolean read FBeforeRunDone;
  end;

  TMeYarn = object(TMeDynamicObject)
  protected
    //generate the VMT for the object
    class function ParentClassAddress: TMeClass;virtual;abstract; //override
  end;

  TMeNotifyThreadEvent = procedure(aThread: PMeThread) of object;
  TMeExceptionThreadEvent = procedure(aThread: PMeThread; aException: Exception) of object;
  TMeThreadStopMode = (smTerminate, smSuspend);
  TMeThreadOptions = set of (itoStopped, itoReqCleanup, itoDataOwner, itoTag);
  {
    Define: Thread Task; Thread Yarn
    the thread control the life-time of the Yarn Object !
    One thread can run a task again and again until stop.
      a task: beforeRun, Run, afterRun Cleanup
      you can change task after Cleanup.
    One thread can run different task one by one.
  }
  TMeThread = object(TMeCustomThread)
  protected
    FLock: PMeCriticalSection;
    FYarn: PMeYarn;
    FLoop: Boolean;
    FStopMode: TMeThreadStopMode;
    FOptions: TMeThreadOptions;
    //FTerminatingException: String;
    //FTerminatingExceptionClass: TClass;
    FOnStopped: TMeNotifyThreadEvent;
    FOnException: TMeExceptionThreadEvent;
    
    procedure AfterRun; virtual; //3* not abstract - otherwise it is required
    procedure AfterExecute; virtual;//5 not abstract - otherwise it is required
    procedure BeforeExecute; virtual;//1 not abstract - otherwise it is required
    procedure BeforeRun; virtual; //2* not abstract - otherwise it is required
    procedure Cleanup; virtual;//4*
    procedure DoException(const aException: Exception); virtual;
    procedure DoStopped; virtual;
    procedure Execute; virtual; //override
    function GetStopped: Boolean;
    function HandleRunException(AException: Exception): Boolean; virtual;
    procedure Run; virtual; abstract;
    class procedure WaitAllThreadsTerminated(AMSec: Integer = cWaitAllThreadsTerminatedCount);
  public
    constructor Create(aCreateSuspended: Boolean = True;
     aLoop: Boolean = True); 
    destructor Destroy; virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Synchronize(const Method: TMeCustomThreadMethod); overload;
    procedure Terminate; virtual;
    procedure TerminateAndWaitFor; virtual;

    //Represents the thread or fiber for the scheduler of the thread.
    //it will be free when the thread free.
    property Yarn: PMeYarn read FYarn write FYarn;
    property Loop: Boolean read FLoop write FLoop;
    property ReturnValue;
    property StopMode: TMeThreadStopMode read FStopMode write FStopMode;
    property Stopped: Boolean read GetStopped;
    property Terminated;
    property OnStopped: TMeNotifyThreadEvent read FOnStopped write FOnStopped;
  end;

  TMeThreadWithTask = object(TMeThread)
  protected
    FTask: PMeTask;
    //
    procedure AfterRun; virtual; //override;
    procedure BeforeRun; virtual; //override;
    procedure Run; virtual; //override;
    procedure DoException(const aException: Exception); virtual; //override;
  public
    // Defaults because
    // Must always create suspended so task can be set
    // And a bit crazy to create a non looped task
    constructor Create(aTask: PMeTask = nil); reintroduce;
    destructor Destroy; virtual; //override;
    //
    // Must be writeable because tasks are often created after thread or
    // thread is pooled
    property Task: PMeTask read FTask write FTask;
  end;

  TMeThreadPool = object(TMeList)
  
  end;


{ Assign a method to WakeMainThread in order to properly force an event into
  the GUI thread's queue.  This will make sure that non-GUI threads can quickly
  synchronize with the GUI thread even if no events are being processed due to
  an idle state }
var
  WakeMainThread: TMeNotifyEvent = nil;
{$IFDEF MSWINDOWS}
{ SyncEvent is an Event handle that is signaled every time a thread wishes to
  synchronize with the main thread or is terminating.  This handle us suitable
  for use with WaitForMultipleObjects.  When this object is signaled,
  CheckSynchronize *must* be called in order to reset the event.  Do not call
  ResetEvent on this handle, or background threads may hang waiting for
  Synchronize to return.
}
  SyncEvent: THandle;
{$ENDIF}
{$IFDEF LINUX}
{ SyncEvent is a set of file descriptors representing a pipe.  The ReadDes field
  is suitable for use within a select or poll call.  When this file descriptor
  is signaled, a background thread wishes to synchronize with the main thread
  or is terminating.  When the ReadDes file descriptor is signaled,
  CheckSynchronize *must* be called to reset the file descriptor.  Do *not*
  actually call __read (or any other "read" function) with this file descriptor
  as that may cause a background thread to hang waiting for Synchronize to return.
}
  SyncEvent: TPipeDescriptors;
{$ENDIF}


{
   EnterMainThread/LeaveMainThread can be used to temporary switch to the
   main thread. The code that should be synchonized (blocking) has to be put
   into a try/finally block and the LeaveMainThread() function must be called
   from the finally block. A missing try/finally will lead to an access violation.
   
   * All local variables can be used. (EBP points to the thread's stack while
     ESP points the the main thread's stack)
   * Unhandled exceptions are passed to the surrounding thread.
   * The integrated Debugger is not able to follow the execution flow. You have
     to use break points instead of "Step over/in".
   * Nested calls to EnterMainThread/LeaveMainThread are ignored. But they must
     strictly follow the try/finally structure.

   Example:

     procedure MyThreadProc;
     var
       S: string;
     begin
       Assert(GetCurrentThreadId <> MainThreadId);
       S := 'Hallo, I''m executed in the main thread';

       EnterMainThread;
       try
         Assert(GetCurrentThreadId = MainThreadId);
         ShowMessage(S);
       finally
         LeaveMainThread;
       end;

       Assert(GetCurrentThreadId <> MainThreadId);
     end;

    @author  Andreas Hausladen
}
procedure EnterMainThread;
procedure LeaveMainThread;

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
{$IFDEF MSWINDOWS}
  SyncEvent := CreateEvent(nil, True, False, '');
  if SyncEvent = 0 then
    RaiseLastOSError;
{$ENDIF}
{$IFDEF LINUX}
  if pipe(SyncEvent) < 0 then
    RaiseLastOSError;
{$ENDIF}
end;

procedure DoneThreadSynchronization;
begin
  DeleteCriticalSection(ThreadLock);
{$IFDEF MSWINDOWS}
  CloseHandle(SyncEvent);
{$ENDIF}
{$IFDEF LINUX}
  __close(SyncEvent.ReadDes);
  __close(SyncEvent.WriteDes);
{$ENDIF}
end;

procedure ResetSyncEvent;
{$IFDEF LINUX}
var
  nRead: Integer;
  Dummy: Byte;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ResetEvent(SyncEvent);
{$ENDIF}
{$IFDEF LINUX}
  if (ioctl(SyncEvent.ReadDes, FIONREAD, @nRead) = 0) and (nRead > 0) then
    __read(SyncEvent.ReadDes, Dummy, SizeOf(Dummy));
{$ENDIF}
end;

procedure WaitForSyncEvent(Timeout: Integer);
{$IFDEF LINUX}
var
  EventFds: TFDSet;
  Tm: TTimeVal;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if WaitForSingleObject(SyncEvent, Timeout) = WAIT_OBJECT_0 then
    ResetSyncEvent;
{$ENDIF}
{$IFDEF LINUX}
  FD_ZERO(EventFds);
  FD_SET(SyncEvent.ReadDes, EventFds);
  Tm.tv_sec := Timeout div 1000;
  Tm.tv_usec := (Timeout mod 1000) * 1000;
  if select(SyncEvent.ReadDes + 1, @EventFds, nil, nil, @Tm) > 0 then
    ResetSyncEvent;
{$ENDIF}
end;

procedure SignalSyncEvent;
{$IFDEF LINUX}
const
  Dummy: Byte = 0;
var
  nRead: Integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  SetEvent(SyncEvent);
{$ENDIF}

{$IFDEF LINUX}
  if (ioctl(SyncEvent.ReadDes, FIONREAD, @nRead) = 0) and (nRead = 0) then
    __write(SyncEvent.WriteDes, Dummy, SizeOf(Dummy));
{$ENDIF}
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

procedure TMeCustomThread.Synchronize(const aMethod: TMeCustomThreadMethod);
begin
  FSynchronize.FThread := @Self;
  FSynchronize.FSynchronizeException := nil;
  FSynchronize.FMethod := aMethod;
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

{ TMeTask }
procedure TMeTask.AfterRun;
begin
end;

procedure TMeTask.BeforeRun;
begin
end;

procedure TMeTask.HandleException(const aException: Exception);
begin
end;

procedure TMeTask.DoAfterRun;
begin
  AfterRun;
end;

procedure TMeTask.DoBeforeRun;
begin
  FBeforeRunDone := True;
  BeforeRun;
end;

function TMeTask.DoRun: Boolean;
begin
  Result := Run;
end;

procedure TMeTask.DoException(const aException: Exception);
begin
  HandleException(aException);
end;

{ TMeThread }
class procedure TMeThread.WaitAllThreadsTerminated(AMSec: Integer);
begin
  while AMSec > 0 do 
  begin
    if ThreadCount = 0 then 
    begin
      Break;
    end;
    Sleep(cWaitAllThreadsTerminatedStep);
    AMSec := AMSec - cWaitAllThreadsTerminatedStep;
  end;
end;

procedure TMeThread.TerminateAndWaitFor;
begin
  if FreeOnTerminate then 
  begin
    raise EMeError.Create(RsThreadTerminateAndWaitFor);
  end;
  Terminate;
  Start; //resume
  WaitFor;
end;

procedure TMeThread.BeforeRun;
begin
end;

procedure TMeThread.AfterRun;
begin
end;

procedure TMeThread.BeforeExecute;
begin
end;

procedure TMeThread.AfterExecute;
begin
end;

procedure TMeThread.Execute;
begin
  try
    BeforeExecute;
    try
      while not Terminated do 
      begin
        if Stopped then 
        begin
          DoStopped;
          // It is possible that either in the DoStopped or from another thread,
          // the thread is restarted, in which case we dont want to restop it.
          if Stopped then 
          begin // DONE: if terminated?
            if Terminated then 
            begin
              Break;
            end;
            Suspend; // Thread manager will revive us
            if Terminated then 
            begin
              Break;
            end;
          end;
        end;

        Include(FOptions, itoReqCleanup);
        try
          try
            BeforeRun;
            try
              if Loop then 
              begin
                while not Stopped do 
                begin
                  try
                    Run;
                  except
                    on E: Exception do 
                    begin
                      if not HandleRunException(E) then 
                      begin
                        Terminate;
                        raise;
                      end;
                    end;
                  end; //try-except
                end; //while
              end 
              else begin
                try
                  Run;
                except
                  on E: Exception do 
                  begin
                    if not HandleRunException(E) then 
                    begin
                      Terminate;
                      raise;
                    end;
                  end;
                end; //try-except
              end;
            finally
              AfterRun;
            end;
          except
            Terminate;
            raise;
          end;
        finally
          Cleanup;
        end;
      end;
    finally
      AfterExecute;
    end;
  except
    on E: Exception do 
    begin
      //FTerminatingExceptionClass := E.ClassType;
      //FTerminatingException := E.Message;
      DoException(E);
      Terminate;
    end;
  end;
end;

constructor TMeThread.Create(aCreateSuspended: Boolean; aLoop: Boolean);
begin
  {$IFDEF DOTNET}
  inherited Create(True);
  {$ENDIF}
  //FOptions := [itoDataOwner];
  if aCreateSuspended then 
  begin
    Include(FOptions, itoStopped);
  end;
  New(FLock, Create);
  Loop := aLoop;
  //
  {$IFDEF DOTNET}
  if not aCreateSuspended then 
  begin
    Resume;
  end;
  {$ELSE}
  //
  // Most things BEFORE inherited - inherited creates the actual thread and if
  // not suspended will start before we initialize
  inherited Create(aCreateSuspended);
    {$IFNDEF COMPILER6_UP}
    // Delphi 6 and above raise an exception when an error occures while
    // creating a thread (eg. not enough address space to allocate a stack)
    // Delphi 5 and below don't do that, which results in a TMeThread
    // instance with an invalid handle in it, therefore we raise the
    // exceptions manually on D5 and below
  if (ThreadID = 0) then 
  begin
    Raise EMeError.Create('TMeThread: invalid thread handle');;
  end;
    {$ENDIF}
  {$ENDIF}
  // Last, so we only do this if successful
end;

destructor TMeThread.Destroy;
begin
  FreeOnTerminate := False; //prevent destroy between Terminate & WaitFor
  Terminate;
  try
    if itoReqCleanup in FOptions then 
    begin
      Cleanup;
    end;
  finally
    // RLebeau- clean up the Yarn one more time, in case the thread was
    // terminated after the Yarn was assigned but the thread was not
    // re-started, so the Yarn would not be freed in Cleanup()
    try
      MeFreeAndNil(FYarn);
    finally
      // Protect FLock if thread was resumed by Start Method and we are still there.
      // This usually happens if Exception was raised in BeforeRun for some reason
      // And thread was terminated there before Start method is completed.
      FLock.Enter; try
      finally FLock.Leave; end;

      MeFreeAndNil(FLock);
    end;
  end;
  //FTerminatingException := '';
  inherited Destroy; //+WaitFor!
end;

procedure TMeThread.Start;
begin
  FLock.Enter; try
    if Stopped then 
    begin
      // Resume is also called for smTerminate as .Start can be used to initially start a
      // thread that is created suspended
      if Terminated then 
      begin
        Include(FOptions,itoStopped);
      end 
      else begin
        Exclude(FOptions,itoStopped);
      end;
      Resume;
      {APR: [in past] thread can be destroyed here! now Destroy wait FLock}
    end;
  finally FLock.Leave; end;
end;

procedure TMeThread.Stop;
begin
  FLock.Enter; try
    if not Stopped then 
    begin
      case FStopMode of
        smTerminate: Terminate;
        smSuspend: {DO not suspend here. Suspend is immediate. See Execute for implementation};
      end;
      Include(FOptions, itoStopped);
    end;
  finally FLock.Leave; end;
end;

function TMeThread.GetStopped: Boolean;
begin
  if Assigned(FLock) then 
  begin
    FLock.Enter; try
      // Suspended may be True if checking stopped from another thread
      Result := Terminated or (itoStopped in FOptions) or Suspended;
    finally FLock.Leave; end;
  end 
  else begin
    Result := True; //user call Destroy
  end;
end;

procedure TMeThread.DoStopped;
begin
  if Assigned(OnStopped) then 
  begin
    OnStopped(@Self);
  end;
end;

procedure TMeThread.DoException(const aException: Exception);
begin
  if Assigned(FOnException) then 
  begin
    FOnException(@Self, aException);
  end;
end;

procedure TMeThread.Terminate;
begin
  //this assert can only raise if terminate is called on an already-destroyed thread
  Assert(FLock<>nil);
  
  FLock.Enter; try
    Include(FOptions, itoStopped);
    inherited Terminate;
  finally FLock.Leave; end;
end;

procedure TMeThread.Cleanup;
begin
  Exclude(FOptions, itoReqCleanup);
  MeFreeAndNil(FYarn);
  {if itoDataOwner in FOptions then begin
    FreeAndNil(FData);
  end;//}
end;

function TMeThread.HandleRunException(AException: Exception): Boolean;
begin
  // Default behavior: Exception is death sentence
  Result := False;
end;

procedure TMeThread.Synchronize(const Method: TMeCustomThreadMethod);
begin
  inherited Synchronize(Method);
end;

{ TMeThreadWithTask }

procedure TMeThreadWithTask.AfterRun;
begin
  FTask.DoAfterRun;
  inherited AfterRun;
end;

procedure TMeThreadWithTask.BeforeRun;
begin
  inherited BeforeRun;
  FTask.DoBeforeRun;
end;

procedure TMeThreadWithTask.DoException(const aException: Exception);
begin
  inherited DoException(aException);
  FTask.DoException(aException);
end;

constructor TMeThreadWithTask.Create(aTask: PMeTask);
begin
  inherited Create(True, True);
  FTask := aTask;
end;

destructor TMeThreadWithTask.Destroy;
begin
  MeFreeAndNil(FTask);
  inherited Destroy;
end;

procedure TMeThreadWithTask.Run;
begin
  if not FTask.DoRun then begin
    Stop;
  end;
end;

{----------------------------------------------------------------------------}

type
  TMainThreadContext = record
    MainThreadEntered: Longint;
    MainThreadOpenBlockCount: Longint;

    IntructionPointer: Pointer;
    BasePointer: Pointer;
    RetAddr: Pointer;

    MainBasePointer: Pointer;
    ContextRetAddr: Pointer;
    FinallyRetAddr: Pointer;

    MainRegEBX, MainRegEDI, MainRegESI: Pointer;
    ThreadRegEBX, ThreadRegEDI, ThreadRegESI: Pointer;
  end;

var
  MainThreadContext: TMainThreadContext;
  MainThreadContextCritSect: TRTLCriticalSection;

procedure ExecuteInMainThread(Data: TObject);
asm
  push ebp

  mov eax, OFFSET MainThreadContext

  { Backup main thread state }
  mov edx, OFFSET @@Leave
  mov [eax].TMainThreadContext.ContextRetAddr, edx
  mov [eax].TMainThreadContext.MainBasePointer, ebp

  { Backup main thread registers }
  mov [eax].TMainThreadContext.MainRegEBX, ebx
  mov [eax].TMainThreadContext.MainRegEDI, edi
  mov [eax].TMainThreadContext.MainRegESI, esi

  { Set "nested call" control }
  mov ecx, [eax].TMainThreadContext.MainThreadOpenBlockCount
  mov [eax].TMainThreadContext.MainThreadEntered, ecx
  inc ecx
  mov [eax].TMainThreadContext.MainThreadOpenBlockCount, ecx

  { Switch to the thread state }
  mov ebp, [eax].TMainThreadContext.BasePointer
  mov edx, [eax].TMainThreadContext.IntructionPointer

  { Swicth to the thread registers }
  mov ebx, [eax].TMainThreadContext.ThreadRegEBX
  mov edi, [eax].TMainThreadContext.ThreadRegEDI
  mov esi, [eax].TMainThreadContext.ThreadRegESI

  { Jump to the user's synchronized code }
  jmp edx

  { LeaveMainThread() will jump to this address after it has restored the main
    thread state. }
@@Leave:
  pop ebp
end;

procedure LeaveMainThreadError(ErrorMode: Integer);
begin
  case ErrorMode of
    0: raise Exception.Create(RsLeaveMainThreadNestedError);
    1: raise Exception.Create(RsLeaveMainThreadThreadError);
  end;
end;

function GetMainThreadId: LongWord;
begin
  Result := MainThreadId;
end;

procedure LeaveMainThread;
asm
  { Check if we are in the main thread }
  call GetCurrentThreadId
  mov ecx, eax
  call GetMainThreadId
  cmp eax, ecx
  jne @@ThreadError

  { "nested call" control }
  mov eax, OFFSET MainThreadContext
  mov ecx, [eax].TMainThreadContext.MainThreadOpenBlockCount
  dec ecx
  js @@NestedError
  mov [eax].TMainThreadContext.MainThreadOpenBlockCount, ecx
  cmp ecx, [eax].TMainThreadContext.MainThreadEntered
  jne @@Leave
  { Release "nested call" control }
  mov [eax].TMainThreadContext.MainThreadEntered, -1

  { Save the current registers for the return, the compiler might have
    generated code that changed the registers in the synchronized code. }
  mov [eax].TMainThreadContext.ThreadRegEBX, ebx
  mov [eax].TMainThreadContext.ThreadRegEDI, edi
  mov [eax].TMainThreadContext.ThreadRegESI, esi
  { Restore main thread registers }
  mov ebx, [eax].TMainThreadContext.MainRegEBX
  mov edi, [eax].TMainThreadContext.MainRegEDI
  mov esi, [eax].TMainThreadContext.MainRegESI

  { If there is an exception the Classes.CheckSynchronize function will handle the
    exception and thread switch for us. Will also restore the EBP regíster. }
  call System.ExceptObject
  or eax, eax
  jnz @@InException

  mov eax, OFFSET MainThreadContext
  { Backup the return addresses }
  pop edx // procedure return address
  pop ecx // finally return address
  mov [eax].TMainThreadContext.FinallyRetAddr, ecx
  mov [eax].TMainThreadContext.RetAddr, edx

  { Restore the main thread state }
  mov ebp, [eax].TMainThreadContext.MainBasePointer
  mov edx, [eax].TMainThreadContext.ContextRetAddr
  jmp edx

@@NestedError:
  xor eax, eax
  call LeaveMainThreadError
@@ThreadError:
  mov eax, 1
  call LeaveMainThreadError

@@InException:
@@Leave:
end;

procedure EnterMainThread;
asm
  { There is nothing to do if we are already in the main thread }
  call GetCurrentThreadId
  mov ecx, eax
  call GetMainThreadId
  cmp eax, ecx
  je @@InMainThread

  { Enter critical section => implicit waiting queue }
  mov eax, OFFSET MainThreadContextCritSect
  push eax
  call EnterCriticalSection

  { Take the return address from the stack to "clean" the stack }
  pop edx

  { Backup the current thread state }
  mov eax, OFFSET MainThreadContext
  mov [eax].TMainThreadContext.MainThreadEntered, ecx
  mov [eax].TMainThreadContext.IntructionPointer, edx
  mov [eax].TMainThreadContext.BasePointer, ebp
  { Backup the current thread registers }
  mov [eax].TMainThreadContext.ThreadRegEBX, ebx
  mov [eax].TMainThreadContext.ThreadRegEDI, edi
  mov [eax].TMainThreadContext.ThreadRegESI, esi

  { Begin try/finally }
@@Try:
  xor eax, eax
  push ebp
  push OFFSET @@HandleFinally
  push dword ptr fs:[eax]
  mov fs:[eax], esp

  { Call Synchronize(nil, TMethod(ExecuteInMainThread)) }
  //xor eax, eax // ClassType isn't used in StaticSynchronize/Synchronize
  xor edx, edx
  push edx
  mov ecx, OFFSET ExecuteInMainThread
  push ecx
  call TMeCustomThread.StaticSynchronize

  { Clean up try/finally }
  xor eax,eax
  pop edx
  pop ecx
  pop ecx
  mov fs:[eax], edx

  { Restore thread state }
  mov eax, OFFSET MainThreadContext
  mov ebp, [eax].TMainThreadContext.BasePointer
  mov ecx, [eax].TMainThreadContext.FinallyRetAddr
  mov edx, [eax].TMainThreadContext.RetAddr

  push ecx  // put finally return address back to the stack
  push edx  // put return address back to the stack

  { End try/finally }
@@Finally:
  { Restore thread registers } 
  mov eax, OFFSET MainThreadContext
  mov ebx, [eax].TMainThreadContext.ThreadRegEBX
  mov edi, [eax].TMainThreadContext.ThreadRegEDI
  mov esi, [eax].TMainThreadContext.ThreadRegESI

  { Leave critical section }
  mov eax, OFFSET MainThreadContextCritSect
  push eax
  call LeaveCriticalSection
  ret
@@HandleFinally:
  jmp System.@HandleFinally
  jmp @@Finally
@@LeaveFinally:
  ret

@@InMainThread:
  { Adjust "nested call" control.
    Threadsafe because we are in the main thread and only the main thread
    manipulates MainThreadOpenBlockCount }
  inc [MainThreadContext].TMainThreadContext.MainThreadOpenBlockCount
end;

initialization
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeCustomThread), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeThread), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeThreadWithTask), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeTask), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeYarn), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeCustomThread), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeThread), ovtVmtParent, TypeOf(TMeCustomThread));
  SetMeVirtualMethod(TypeOf(TMeThreadWithTask), ovtVmtParent, TypeOf(TMeThread));
  SetMeVirtualMethod(TypeOf(TMeTask), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeYarn), ovtVmtParent, TypeOf(TMeDynamicObject));

  InitThreadSynchronization;

  MainThreadContext.MainThreadEntered := -1;
  InitializeCriticalSection(MainThreadContextCritSect);


finalization
  DeleteCriticalSection(MainThreadContextCritSect);

  MeFreeAndNil(SyncList);
  DoneThreadSynchronization;
  //writeln('ThreadCount=',ThreadCount);
end.
