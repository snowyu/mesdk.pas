MeSDK Library

* MeObjects *
MeObjects Library for Delphi is a light object extension to make object type small and powerful. It makes the object type
supports the ClassType, InheritsFrom and ClassName like the Class Type, but it's mini and faster than the delphi class type.
You can treat it as mini-class type. The MeObjects Library is the MeSDK core too.

** uMeObjects.pas **

TMeDynamicObject
summary: the abstract MeObject mini-class.

TMeNamedObject
  add the name property and assign virtual method.

TMeComponent
  set the name property will be added to GComponentNameList.

TMeInterfacedObject
  supports the FreeNotifies and Destroyed only when reference count < 0 

TMeContainer
  abstract data Container

TMeList
  Simple list of pointers. It is used instead of standard VCL TList.
  TMeList stores any kind data (or pointers to these ones). Can be created
  calling function New(PMeList, Create) or use as the record object directly. 

TMeStrings
  maintains a list of strings. 

TMeDynamicMemory
  supports the Dynamic Memory can auto increase the memory size

TMeStream
  abstract stream object

TMeNamedObjects
  maintains the PMeNamedObject list.

helper functions:

function NewMeObject(const aClass: TMeClass): PMeDynamicObject;
summary: create a instance by the aClass:TypeOf(TMeList).

procedure MeFreeAndNil(var Obj); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
summary: Frees an object reference and replaces the reference with nil.

function MeTypeOf(const aObj: TMeVMTHelper): TMeClass; {$IFDEF PUREPASCAL}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
 MeTypeOf(aObj) = TypeOf(aObj) = TMeClass(aObj) 
 It Should cast the aObj directly !! the MeTypeOf is not useful!

function MeSizeOf(const aObj: TMeVMTHelper): Integer; {$IFDEF PUREPASCAL}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
summary return the size of aObj.

function MeInheritsFrom(aClass: TMeClass; const aParentClass: TMeClass): Boolean;
Summary: Determines the relationship of two object types.
Use InheritsFrom to determine if a particular class type or object is an instance of a class or one of its descendants. InheritsFrom returns true if the object type specified in the aClass parameter is an ancestor of the object type or the type of the object itself. Otherwise, it returns false.

function SetMeVirtualMethod(const aClass: TMeClass; const Offset: Integer; const Method: Pointer): Pointer;
summary: replace the VirtualMethod of the MeObject to new Method Pointer. return the original method entry pointer.
Note: first you must set the proper parent class type via SetMeVirtualMethod in the initiliazation section:
  SetMeVirtualMethod(TypeOf(TMeInterfacedObject), ovtVmtParent, TypeOf(TMeDynamicObject));

function FindMeComponent(const Name: String): PMeComponent;

function GComponentNameList: PMeList;

procedure SortIntegerArray(var A : array of Integer);
Summary procedure to sort array of integers. 

procedure SortDwordArray(var A : array of LongWord);
Summary Procedure to sort array of unsigned 32-bit integers.

procedure SortData(const Data: Pointer; const aCount: LongWord; const CompareFun: TCompareEvent;
                    const SwapProc: TSwapEvent);
Summary Call it to sort any array of data of any kind, passing total
   number of items in an array and two defined (regular) function
   and procedure to perform custom compare and swap operations.
   First procedure parameter is to pass it to callback function
   CompareFun and procedure SwapProc. Items are enumerated from
   0 to aCount-1.

procedure FillListIn(List: TMeList; FromIdx, Count, Value: Integer);
Summary Very fast fill Value to List elements from List[FromIdx] to List[FromIdx+Count-1].
   Given elements must exist. Count must be > 0.


** uMeStream.pas **
the stream objects It is used instead of standard VCL Stream class.
TMeFileStream
TMeMemoryStream

** uMeLog.pas **
the Logging of Application class.

TMeCustomLogger
the abstract logger class for logging .
  TMeCustomLogger is an abstract class that
  defines a framework for logging general
  purposed information.

TMeRootLogger
the singleton root logger class for logging
  AddLogger the logger object here. the logging message can be sent to the AddLoggered logger.

TMeStringsLogger
  for logging information to the MeStrings.

TMeStreamLogger
  abstract stream logger for logging information to the MeStream.

TMeDebugLogger
  for logging information to the DebugOutput stream.
  extends the framework for logging information to the DebugOutput stream.
  
  TMeDebugLogger provides a flexible means of selecting 
  the destination of log messages. Messages can be 
  written to either a file or the WIN32 API Debug 
  Output stream.
  
  TMeDebugLogger is very useful for capturing information 
  for debugging, trouble-shooting, and general 
  feedback purposes.


function GLogger: PMeRootLogger;

how to usage:
  // do not free these loggers added to the GLogger will do this for u..
  GLogger.AddLogger(New(PMeStringsLogger, Create(vStrs)));
  GLogger.AddLogger(New(PMeDebugLogger, Create));

  GLogger.Info(aText);

** uMeLoggerEx **
the extented logger objects.

TMeFileLogger
Summary for logging information to the file.


** uMeCoroutine **
implements the coroutine object. (main algo code come from sjrd (based on an idea of Bart van der Werf))
    CoRoutines management classes CoRoutines provides two main classes. TMeCustomCoRoutine is the base class for CoRoutine management. TMeCoRoutineEnumerator is a derived class, specialised in Delphi 2005 enumerators implementation. This unit needs tests under Windows 95/98/Me, in case of growth of the stack, because PAGE_GUARD is not supported under these versions.

TMeCustomCoRoutine
CoRoutine support class
    The Resume method can't be executed twice at the same moment. It can't be
    called simultaneously in two separate threads ; nor can it be called from
    Execute (which should cause recursive call).
    However, it can be called successively by two separate threads.

    The Execute method shoud test for the Terminating property after each call
    to Yield, and terminate itself gracefully if Terminating is True. It will
    be set so when the TMeCustomCoRoutine object must release itself, before running
    the CoRoutine again. If you call Yield when Terminating is True, an
    ECoRoutineTerminating exception will be raised in order to ensure that
    Execute ends immediately.

    The amount of simultaneous instances of TMeCustomCoRoutine must never exceed 32 K,
    because each one must reserve a virtual memory range of 64 Ko minimum.

TMeCoRoutine

TMeCoRoutineEnumerator
the abstract enumerator running in a CoRoutine
    In order to obtain a concrete enumerator, you must override the Execute and
    SetNextValue methods, and define a Current property. The Execute method can
    call Yield many times with any value as a parameter. The SetNextValue must
    store this value, and the Current property should read it.
    @author sjrd, based on an idea of Sergey Antonov
    @version 1.0


** uMeYield.pas **
the another CoRoutine implementation(no switch stack) (main algo code come from Sergey Antonov.)

TMeCustomCoRoutine
TMeCoRoutineEnumerator

** uRegExpr.pas **
  The Initial Developers of the Original Code are Andrey V. Sorokin(RegExpr).
 v. 0.948+ 2003-12-17 by riceball
  + SubExprName define and parse: "():SubExprName:"
  + property MatchStrPos, MatchStrLen, MatchStr by SubExprName index.
  + function GetSubExprIndexByName
  + property SubExprNames to get the SubExprName by integer index.
  + Subtitute the SubExprName by : $[SubExprName].
  * the SubExprName is CaseSensitive!
 v. 0.980 2008-5-30 by riceball
  * more speedup for a large-text search.
    * remove Length(aPChar) in ExecPrim 
      //len := Length(aPChar); it will be very slow on a large pchar.
  + procedure SetSubjectStr(const Value: RegExprString);
  + procedure SetSubjectStr(const Value: PRegExprChar);
      it do not assigned the InputString, just point it directly. so u must do not free the string(be careful)!!
  + keep the more compatible with the Perl RegExpr: (?:....)
      the non-capuring Group, but not whole impl it.
  + \-[1..9]: means prev SubExpr in Expression.
    (['|"])\S+\-1
    match: 'dddd', "word".
  + add more compatible with the Perl RegExpr:  (?<name>...) or (?'name'...)
  + add compatible with the Python RegExpr:  (?P<name>...)

** uMeRegExpr.pas **
the RegExpr extension object 

** uMeRegExprCoRoutine.pas **
the CoRountine object of the MeRegExpr 


** uMeThread.pas **
the Thread object for the MeSDK Core.

TMeAbstractThread
  used to instead of the Borland Thread Class 

TMeTask
  the abstract task object

TMeCustomThread
  the abstract thread with task

TMeThread
  the thread with task supported.
    the task can be schedule task. the task can be destroyed when thread free.
    One thread can run a task again and again until stop.
      a task: beforeRun, Run, afterRun Cleanup
      you can change task after Cleanup.
    One thread can run different task one by one.
    Thread.Terminate: Terminate the thread. the thread can not be re-used.
    thread.Stop: if the StopMode is smSuspend then it can be re-used.
                 it always be sure the AfterRun can be executed.

    
    aThread := NewThreadTask(myTask);
    aThread.Start;

TMeThreadMgrTask
    the TMeThreadMgrTask task uses manage the threads with pool(if set the FMaxThreads is greater than 0).
    the task will be free automatic after done if FreeTask is true.
    note: if the exception occur even the FreeTask is false, it will still free the task when thread free.
    unless override the task.HandleException method and set the aThread.task := nil;
      
  
    Usage: 
     FThreadForMgr := New(PMeThread, Create(New(PMeThreadMgr, Create)));
     //FThreadForMgr.Name := 'The Thread For Mgr'; //need check the compiler directive: NamedThread!

    vMgr := PMeThreadMgr(FThreadForMgr.Task);
    for i := 1 to 3 do
    begin
      New(vTask, Create);
      vTask.Id := i;
      vTask.Count := i;
      vMgr.Add(vTask); //the task can be added after running too.
    end;
    FThreadMgr.Start;
    Writeln('Run....');
    Sleep(3000);
    FThreadForMgr.TerminateAndWaitFor;
    MeFreeAndNil(FThreadForMgr);


TMeThreadMgr
  the thread run the the TMeThreadMgrTask task.

function NewThreadTask(const aTask: PMeTask): PMeThread;
Summary: create a thread instance with aTask.

procedure EnterMainThread;
procedure LeaveMainThread;
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

** uMeSyncObjs.pas **
collects the thread synchronization objects instead of the standard SyncObjs of Delphi.


TMeEvent

TMeMutex

TMeCriticalSection


** uMeSysUtils.pas **
some helper functions and classes.

TMeThreadSafeList
Summary: represents a thread-safe list.

procedure AddFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);
 Summary: Ensures that aProc is notified that the aInstance is going to be destroyed.}
 Desccription: Use AddFreeNotification to register aProc that should be notified when the aInstance is about to be destroyed. 

procedure RemoveFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);
 Summary: Disables destruction notification that was enabled by AddFreeNotification.}
 Description: RemoveFreeNotification removes the NotificationProc specified by the aProc parameter
    from the internal list of procedures to be notified that the aInstance is about to be destroyed. 
    aProc is added to this list by a previous call to the AddFreeNotification function.

//the thread safe version:
function FormatDateTimeS(const Format: string; aDateTime: TDateTime): string;
function FormatS(const aFormat: string; const Args: array of const): string;
function TimeToStrS(const aTime: TDateTime): string;
function DateToStrS(const aDate: TDateTime): string;
function DateTimeToStrS(const aDateTime: TDateTime): string;

procedure GetDefaultFormatSettings(var Result: TFormatSettings);

//return GMT now.
function GMTNow: TDateTime;


** uMeInjector **
the method(procedure) Code Injector
  Provide the lightest and simplest injector object -- TMeInjector.
  This object do not use any virtual method, so you can use it directly.
  Each injector only take 36 bytes about in the memory. One injector 
  maintains the one injected method(procedure) only. Call the InjectXXX 
  Method to inject. The injector object is the smallest, simplest and 
  fastest object in the MeAOP .

TMeInjector
Provide the lightest and simplest injector object to inject function or method.
  This object do not use any virtual method, so you can use it directly.
  Each injector only take 36 bytes about in the memory. One injector 
  maintains the one injected method(procedure) only. Call the InjectXXX 
  Method to inject. The injector object is the smallest, simplest and 
  fastest object in the MeAOP .

Usage:
var
  OldMessageBoxFunc: function (hWnd: HWND; lpText, lpCaption: PChar;
    uType: UINT): Integer; stdcall = nil;

function NewMessageBoxFunc(hWnd: HWND; lpText, lpCaption: PChar;
  uType: UINT): Integer; stdcall;
var
  S: String;
begin
  S := UpperCase(lpText);
  Result := OldMessageBoxFunc(hWnd, PChar(S), PChar('MeInjector:'+lpCaption), uType)
end;

var
  vMsgBoxInjector: TMeInjector;

begin
  vMsgBoxInjector.InjectProcedure(@MessageBox, @NewMessageBoxFunc);
  @OldMessageBoxFunc := vMsgBoxInjector.OriginalProc

  //the string 'the injected message box' should be UpperCase now.
  MessageBox(0, 'the injected message box','Demo2', 0);

  vMsgBoxInjector.Enabled := False;
end.


** uMeURI.pas **
Represents the Uniform Resource Identifier object.

** uMeURL.pas **
the abstract Uniform/Universal Resource Accessor class and factory.


* MeRTTI *
the Mini Run-time Type Infomation of Object. the MeType is stream-able.

* MeAOP *
The MeAOP Library is Aspect Oriented Programming for delphi. IMO, the aspect is the general feature in fact. It's a helper for 
object-oriented programming to re-use the feature(function), such as the Login feature. Aspect Oriented Programming 
attempts to aid programmers in the separation of features, or the breaking down of a program into distinct parts that 
overlap in functionality as little as possible. 

  * distill/separate the general features from many classes;
  * many classes can share the feature, so it need not to modify many classes if the feature've been changed.
  * no necessary to modify the method when adding a new feature to the method.

I treat the Aspect Oriented Programming as Feature Oriented Programming.

* MeRemote *
The MeRemote Features Library is the remote function and remote object Library for delphi. It's the feature-layer of the MeAOP.
not ready yet.

* MeScript *
the MeSDK Script Engine Core. It's the functional language script engine. but not ready yet.

