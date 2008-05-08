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

    procedure Setup;override;
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
    procedure Setup;override;
  published
    procedure Test_Run;override;
  end;

  TTest_MeThreadMgr = class(TTestCase)
  protected
    FThreadMgr: PMeThread;

    procedure Setup;override;
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
  EnterMainThread;
  try
    if Count > 0 then Writeln(Id, ':', Count);
  finally
    LeaveMainThread;
  end;
end;

{ TTest_MeThread }
procedure TTest_MeThread.Setup;
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
  Assert(GetCurrentThreadId <> MainThreadId);
end;

{ TTest_MeCustomThread }
procedure TTest_MeCustomThread.Setup;
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
procedure TTest_MeThreadMgr.Setup;
begin
  FThreadMgr := New(PMeThread, Create(New(PMeThreadMgr, Create)));
end;

procedure TTest_MeThreadMgr.TearDown;
begin
  MeFreeAndNil(FThreadMgr);
end;

procedure TTest_MeThreadMgr.Test_Run();
var
  i : Integer;
  vTask: PMyTask;
  vMgr: PMeThreadMgr;
begin
  vMgr := PMeThreadMgr(FThreadMgr.Task);
  FThreadMgr.Start;
  for i := 1 to 3 do
  begin
    New(vTask, Create);
    vTask.Id := i;
    vTask.Count := i;
    vMgr.AddTask(vTask);
  end;
  Writeln('Assert');
  Sleep(5000);
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
