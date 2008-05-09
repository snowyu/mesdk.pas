unit uMeThreadTest;

{$I MeSetting.inc}

{.$DEFINE Debug_WriteToConsole_Support}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, //QueryPerformanceCounter
  {$ENDIF}
  {$IFDEF DEBUG}
  DbugIntf,
  {$ENDIF}
  Classes,
  SysUtils,
  TypInfo,
  IniFiles,
  //Dialogs,
  Forms,
  TestFramework
  , uMeObject
  , uMeStrUtils
  , uMeThread
  ;

type
  PMeT = ^TMeT;
  TMeT = object(TMeAbstractThread)
  protected
    procedure Execute;virtual;//override;
  public
  end;

  TTest_MeCustomThread = class(TTestCase)
  protected
    FThread: PMeAbstractThread;

    procedure SetUp;override;
    procedure TearDown;override;

  public
  published
    procedure Test_Run;virtual;
  end;

  PMyTask = ^ TMyTask;
  TMyTask = object(TMeTask)
  protected
    Id: Integer;
    Count: Integer;
    procedure AfterRun; virtual;
    function Run: Boolean; virtual;
  end;

  TTest_MeThread = class(TTest_MeCustomThread)
    procedure SetUp;override;
  published
    procedure Test_Run;override;
  end;

  TTest_MeThreadMgr = class(TTestCase)
  protected
    //FThreadMgr: PMeThread;
    FThreadMgr: PMeThreadMgr;

    procedure SetUp;override;
    procedure TearDown;override;

  public
  published
    procedure Test_Run;virtual;
  end;

implementation


var
  AppPath: string;
  GCount: Integer = 0;


function TMyTask.Run: Boolean;
begin
  Result := InterlockedIncrement(Count) > 0;
  Sleep(100);
end;

procedure TMyTask.AfterRun;
begin
  {$IFDEF DEBUG}
  if Count > 0 then Senddebug('Task'+IntToStr(Id)+ ':'+ IntToStr(Count));
  EnterMainThread;
  try
    if Count > 0 then Writeln('Task'+IntToStr(Id)+ ':'+ IntToStr(Count));
  finally
    LeaveMainThread;
  end;
  {$ENDIF}
end;

{ TTest_MeThread }
procedure TTest_MeThread.SetUp;
var
  vTask: PMyTask;
begin
  New(vTask, Create);
  FThread := New(PMeThread, Create(vTask));
end;

procedure TTest_MeThread.Test_Run;
var
  I: Integer;
  vTask: PMyTask;
begin
  FThread.Priority := tpTimeCritical;
  vTask := PMyTask(PMeThread(FThread).Task);
  PMeThread(FThread).Start;
  Sleep(50);
  I := -1;
  I := InterlockedExchange(vTask.Count, I);
  CheckEquals(1, I, ' the count is error.');
  PMeThread(FThread).TerminateAndWaitFor;
end;

procedure TMeT.Execute;
var
  S: string;
begin
  while (InterlockedIncrement(GCount) > 0) and not Terminated do
    Sleep(100);
  S := 'Hallo, I''m executed in the main thread:';
  Assert(GetCurrentThreadId <> MainThreadId);
  EnterMainThread;
  try
    Assert(GetCurrentThreadId = MainThreadId);
    //Writeln(S, GetCurrentThreadId = MainThreadId);
  finally
    LeaveMainThread;
  end;
  Assert(GetCurrentThreadId <> MainThreadId); //}
end;

{ TTest_MeCustomThread }
procedure TTest_MeCustomThread.SetUp;
begin
  FThread := New(PMeT, Create(True));
end;

procedure TTest_MeCustomThread.TearDown;
begin
  //FThread.Terminate;
  //wait for thread Terminated.
  //FThread.WaitFor;
  //Sleep(100); 
  MeFreeAndNil(FThread);
end;

procedure TTest_MeCustomThread.Test_Run();
var
  I: Integer;
begin
  FThread.Priority := tpTimeCritical;
  FThread.Resume;
  Sleep(50);
  I := -1;
  I := InterlockedExchange(GCount, I);
  //FThread.Terminate;
  CheckEquals(1, I, ' the count is error.');
end;

{ TTest_MeThreadMgr }
procedure TTest_MeThreadMgr.SetUp;
begin
  //FThreadMgr := New(PMeThread, Create(New(PMeThreadMgrTask, Create)));
  FThreadMgr := New(PMeThreadMgr, Create);
  {$IFDEF NamedThread}
  FThreadMgr.Name := 'ThreadMgr';
  {$ENDIF}
end;

procedure TTest_MeThreadMgr.TearDown;
begin
  MeFreeAndNil(FThreadMgr);
end;

procedure TTest_MeThreadMgr.Test_Run();
var
  i : Integer;
  vTask: PMyTask;
  //vMgr: PMeThreadMgrTask;
begin
  //vMgr := PMeThreadMgrTask(FThreadMgr.Task);
  FThreadMgr.Start;
  for i := 1 to 3 do
  begin
    New(vTask, Create);
    vTask.Id := i;
    vTask.Count := i;
    //vMgr.Add(vTask);
    FThreadMgr.AddTask(vTask);
  end;
  //Writeln('Run....');
  //while vMgr.TaskQueue.Count > 0 do
    //Sleep(100);
  //Sleep(3000); //note: the MainThread is blocked now. so if u EnterMainThread then the deadlock occur.
  i := GetTickCount + 4000;
  while i > GetTickCount do
    Application.ProcessMessages;

  FThreadMgr.TerminateAndWaitFor;
end;

Initialization

  AppPath := ExtractFilePath(ParamStr(0));
  RegisterTests('MeThread suites',
                [
                 TTest_MeCustomThread.Suite
                 , TTest_MeThread.Suite
                 , TTest_MeThreadMgr.Suite
                 //, TTest_MeCustomThread.Suite
                ]);//}
finalization
end.
