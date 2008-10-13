unit uMeInterceptorTest;

{$I MeSetting.inc}

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
  Math,
  TestFramework
  //uStringListEx,
  , uMeSystem
  {$IFDEF MeRTTI_SUPPORT}
  , uMeTypes
  , uMeProcType
  {$ENDIF}
  , uMeTypInfo
  , uMeInjector
  , uMeInterceptor
  , uMeTypInfoTest
  , uInternalTestObj
  ;

type
  ETestError = Class(Exception);
  ETestErrorBefore = Class(Exception);
  TTestExceptionObj = class
  published
    procedure Method1;
  end;
  TMeMyTestCustomInterceptor = class(TMeCustomInterceptor);
  TMeAbstractInterceptorAccess = class(TMeAbstractInterceptor);
  TMeInterceptorClassAccess = class of TMeAbstractInterceptorAccess;

type
  TTestInterceptResultRec = packed record
    Sender: TObject;
    //only for DoOnAfterException
    //E: Exception;  //it maybe free by finally section.
    ET: ExceptionClass;
    EMsg: string;
    
    Tick: integer; //the time to execute.
    Method: Pointer;
    ResultState: TMeExecuteStates;
  end;
  {
  0: AllowExecute
  1: BeforeExecute
  2: AfterExecute
  3: AfterException
  }
  TTestInterceptResults = array [0..3] of TTestInterceptResultRec;
  {abstract tester }
  TTestMeCustomInterceptor = class (TCustomTest_Method)
  protected
    FStartCount: Int64;
    FStopCount: Int64;
    FInterceptor: TMeAbstractInterceptor;
    FInterceptorClass: TMeInterceptorClassAccess;
    FAllowExecute: Boolean;
    FRaiseException: Boolean;
    FResults: TTestInterceptResults;
    procedure InitInterceptor(aInterceptor: TMeAbstractInterceptor);virtual;
    function DoOnAllowExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const Params: PMeProcParams = nil): Boolean;virtual;
    procedure DoOnBeforeExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const Params: PMeProcParams = nil);virtual;
    procedure DoOnAfterExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const Params: PMeProcParams = nil);virtual;
    function DoOnAfterException(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const E: Exception; const Params: PMeProcParams = nil): Boolean;virtual;
    procedure CheckResults(Sender: TObject; aMethod: Pointer; E: ExceptionClass = nil; thisState: TMeExecuteStates = [esAllowed, esBefore, esAfter]);
  protected
    procedure SetUp;override;
    procedure TearDown;override;
  end;

  TTest_MeCustomInterceptor = class (TTestMeCustomInterceptor)
  protected
    //function TestMethodStr: string;
    procedure SetUp;override;
  published
    procedure Test_AddToPublishedMethod;
    //the exception in the method
    procedure Test_AddToMethodExceptionIn;
    //the exception before the method
    procedure Test_AddToMethodExceptionBefore;
    procedure Test_AddToProc;
    procedure Test_AddToDLLProc;
    procedure Test_AddToProperty;

    {procedure Test_AddToFunctionInt;
    procedure Test_AddToFunctionFloat;
    procedure Test_AddToFunctionLittleRec;
    procedure Test_AddToFunctionLittleAry;
    procedure Test_AddToFunctionInt64;
    procedure Test_AddToFunctionStr;
    procedure Test_AddToFunctionMethodStr;
    procedure Test_AddToFunctionInt64InDLL;
    //}
  end;

  TTest_MeInterceptor = class (TTest_MeCustomInterceptor)
  protected
    //function TestMethodStr: string;
    procedure SetUp;override;
  published
    {procedure Test_AddToFunctionInt;
    procedure Test_AddToFunctionFloat;
    procedure Test_AddToFunctionLittleRec;
    procedure Test_AddToFunctionLittleAry;
    procedure Test_AddToFunctionInt64;
    procedure Test_AddToFunctionStr;
    procedure Test_AddToFunctionMethodStr;
    procedure Test_AddToFunctionInt64InDLL;
    //}
  end;

var
  EmptyResults: TTestInterceptResults;

const
  cResultStr = '@@Hello test return str@@!!';

implementation

function TestInt64DLL: Int64; external 'dlltest.dll' name 'TestInt64';
procedure TestDLLProc; external 'dlltest.dll' name 'TestDLLProc';
procedure ClearTestDLLProc; external 'dlltest.dll' name 'ClearTestDLLProc';
function GetResultByDLL: PShortString; external 'dlltest.dll' name 'GetResultByDLL';

procedure TTestExceptionObj.Method1;
begin
  {$IFDEF Debug_WriteToConsole_Support}
  writeln('TTestExceptionObj.Method1');
  {$ENDIF}
  raise ETestError.Create('Hi this is a Test Error!');
end;
{------------------------------------------------------------------}
procedure TTestMeCustomInterceptor.InitInterceptor(aInterceptor: TMeAbstractInterceptor);
begin
  aInterceptor.OnBeforeExecute := DoOnBeforeExecute;
  aInterceptor.OnAllowExecute := DoOnAllowExecute;
  aInterceptor.OnAfterExecute := DoOnAfterExecute;
  aInterceptor.OnAfterException := DoOnAfterException;
  FAllowExecute := True;
  FRaiseException := False;
  QueryPerformanceCounter(FStartCount);
end;

procedure TTestMeCustomInterceptor.SetUp;
begin
  FAllowExecute := True;
end;

procedure TTestMeCustomInterceptor.TearDown;
begin
  FreeAndNil(FInterceptor);
end;

function TTestMeCustomInterceptor.DoOnAllowExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const Params: PMeProcParams): Boolean;
begin
  QueryPerformanceCounter(FStartCount);
  FResults[0].Sender := Sender;
  Result := FAllowExecute;
  FResults[0].Method := MethodItem.Injector.MethodOriginalLocation;
  QueryPerformanceCounter(FStopCount);
  FResults[0].Tick := FStopCount - FStartCount;
  {$IFDEF Debug_WriteToConsole_Support}
  if Assigned(Sender) then write(Sender.ClassName+'.');
  writeln(MethodItem.Name, ' OnAllowExecute ', FResults[0].Tick);
  {$ENDIF}
end;

procedure TTestMeCustomInterceptor.DoOnBeforeExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const Params: PMeProcParams);
begin
  FResults[1].Sender := Sender;
  FResults[1].Method := MethodItem.Injector.MethodOriginalLocation;
  QueryPerformanceCounter(FStopCount);
  FResults[1].Tick := FStopCount - FStartCount;
  {$IFDEF Debug_WriteToConsole_Support}
  if Assigned(Sender) then write(Sender.ClassName+'.');
  writeln(MethodItem.Name, ' OnBeforeExecute ', FResults[1].Tick);
  {$ENDIF}
end;

procedure TTestMeCustomInterceptor.DoOnAfterExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem
  ; const thisState: TMeExecuteStates
  ; const Params: PMeProcParams);
var
  s: string;
begin
  FResults[2].Sender := Sender;
  FResults[2].Method := MethodItem.Injector.MethodOriginalLocation;
  QueryPerformanceCounter(FStopCount);
  FResults[2].Tick := FStopCount - FStartCount;
  FResults[2].ResultState := thisState;
  {$IFDEF Debug_WriteToConsole_Support}
  if Assigned(Sender) then write(Sender.ClassName+'.');
  writeln(MethodItem.Name, ' OnAfterExecute ', FResults[2].Tick);
  s := '';
  if esAllowed in thisState then s := s + 'Allowed';
  if esBefore in thisState then s := s + ' Before';
  if esAfter in thisState then s := s + ' After';
  if esException in thisState then s := s + ' Exception';
  if s <> '' then writeln('  thisState:',s);
  {$ENDIF}
end;

function TTestMeCustomInterceptor.DoOnAfterException(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const E: Exception; const Params: PMeProcParams): Boolean;
begin
  FResults[3].Sender := Sender;
  FResults[3].Method := MethodItem.Injector.MethodOriginalLocation;
  //FResults[3].E := E;
  FResults[3].ET := ExceptionClass(E.ClassType);
  FResults[3].EMsg := E.Message;
  QueryPerformanceCounter(FStopCount);
  FResults[3].Tick := FStopCount - FStartCount;
  {$IFDEF Debug_WriteToConsole_Support}
  if Assigned(Sender) then write(Sender.ClassName+'.');
  writeln(MethodItem.Name, ' OnAfterException('+E.ClassName+'): '+ E.Message+' ', FResults[3].Tick);
  {$ENDIF}
  Result := FRaiseException; //if FRaiseException then raise;
end;

procedure TTestMeCustomInterceptor.CheckResults(Sender: TObject; aMethod: Pointer; E: ExceptionClass; thisState: TMeExecuteStates);
begin
  CheckSame(Sender, FResults[0].Sender, 'AllowExecute Sender mismatch.');
  CheckSame(Sender, FResults[1].Sender, 'BeforeExecute Sender mismatch.');
  CheckSame(Sender, FResults[2].Sender, 'AfterExecute Sender mismatch.');
  CheckEquals(Byte(thisState), Byte(FResults[2].ResultState), 'AfterExecute ResultState mismatch.');

  CheckEquals(Integer(aMethod), Integer(FResults[0].Method), 'the Method Item mismatch in the AllowExecute ');
  CheckEquals(Integer(aMethod), Integer(FResults[1].Method), 'the Method Item mismatch in the BeforeExecute ');
  CheckEquals(Integer(aMethod), Integer(FResults[2].Method), 'the Method Item mismatch in the AfterExecute ');

  Check(FResults[0].Tick <= FResults[1].Tick, 'AllowExecute should occur before BeforeExecute .');
  Check(FResults[1].Tick <= FResults[2].Tick, 'BeforeExecute should occur before AfterExecute .');

  if E <> nil then
  begin
    CheckSame(Sender, FResults[3].Sender, 'AfterException Sender mismatch.');
    //it seems that the Delphi7 use anther Exception instance on try-finally... and it will be free on out try-finally section.
    {$IFDEF Compiler8_UP}
    //CheckIs(FResults[3].E, E, 'AfterException Exception mismatch.');
    {$ENDIF}
    CheckEquals(E, FResults[3].ET, 'AfterException ExceptionClass mismatch.');

    CheckEquals(Integer(aMethod), Integer(FResults[3].Method), 'the Method Item mismatch in the AfterException ');

    Check(FResults[3].Tick <= FResults[2].Tick, 'AfterException should occur before AfterExecute .');
    Check(FResults[1].Tick <= FResults[3].Tick, 'AfterException should occur after BeforeExecute .');
  end;
end;

procedure TestProc;
begin
  RunResult := cResultStr;
end;

procedure TTest_MeCustomInterceptor.SetUp;
begin
  inherited;
  TMeInterceptorClass(FInterceptorClass) := TMeCustomInterceptor;
end;

procedure TTest_MeCustomInterceptor.Test_AddToDLLProc;
var
  i: Integer;
  vS: string;
begin
  i :=Pos('_', ClassName);
  vS := Copy(ClassName, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(' Check '+ vS +' Test_AddToDLLProc');
  {$ENDIF}
  FInterceptor := FInterceptorClass.AddTo(@TestDLLProc, 'TestDLLProc');
  InitInterceptor(FInterceptor);
  try
    ClearTestDLLProc;
    FillChar(FResults, SizeOf(FResults), 0);
    TestDLLProc;
    CheckResults(nil, @TestDLLProc);
    CheckEquals(cResultStr, GetResultByDLL^, 'the Run Result mismatch!');
  finally
    FInterceptorClass.RemoveFrom(@TestDLLProc);
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToProc;
var
  i: Integer;
  vS: string;
begin
  i :=Pos('_', ClassName);
  vS := Copy(ClassName, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(' Check '+ vS +' Test_AddToProc');
  {$ENDIF}
  FInterceptor := FInterceptorClass.AddTo(@TestProc, 'TestProc');
  InitInterceptor(FInterceptor);
  try
    RunResult := '';
    FillChar(FResults, SizeOf(FResults), 0);
    TestProc;
    CheckResults(nil, @TestProc);
    CheckEquals(cResultStr, RunResult, 'the Run Result mismatch!');
  finally
    FInterceptorClass.RemoveFrom(@TestProc);
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToProperty;
var
  i: Integer;
  vS: string;
begin
  i :=Pos('_', ClassName);
  vS := Copy(ClassName, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(' Check '+ vS +' Test_AddToProperty');
  {$ENDIF}
  FInterceptor := FInterceptorClass.AddToProperty(TTestPropObj, 'Name', [mpsGet]);
  InitInterceptor(FInterceptor);
  try
    RunResult := '';
    FillChar(FResults, SizeOf(FResults), 0);
    TestProc;
    CheckResults(nil, @TestProc);
    CheckEquals(cResultStr, RunResult, 'the Run Result mismatch!');
  finally
    FInterceptorClass.RemoveFrom(@TestProc);
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToPublishedMethod;
var
  i: Integer;
  vS: string;
  vProc: TNewMethodProc;
  vObj: TTestBaseObj;
  vChildObj: TTestChildObj;
  s: string;
begin
  i :=Pos('_', ClassName);
  vS := Copy(ClassName, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(' Check '+ vS +' Test_AddToPublishedMethod');
  {$ENDIF}
  //FInterceptor := FInterceptorClass.AddTo(TTestBaseObj, @TTestBaseObj.PublishedMethod3, 'PublishedMethod3');
  FInterceptor := FInterceptorClass.AddTo(TTestBaseObj, 'PublishedMethod3');
  FInterceptorClass.AddTo(TTestChildObj, @TTestChildObj.DynamicMethod6, 'DynamicMethod6');
  InitInterceptor(FInterceptor);
  s := 'TTestBaseObj.PublishedMethod3 Run!';
  {$WARNINGS OFF}
  vObj := TTestBaseObj.Create;
  {$WARNINGS ON}
 try
  try
    RunResult := '';
    FillChar(FResults, SizeOf(FResults), 0);
    vObj.PublishedMethod3;
    CheckResults(vObj, @TTestBaseObj.PublishedMethod3);
    CheckEquals(s, RunResult, '1the '+vS+' Run Result error');

    RunResult := '';
    FillChar(FResults, SizeOf(FResults), 0);
    vObj.PublishedMethod3;
    CheckResults(vObj, @TTestBaseObj.PublishedMethod3);
    CheckEquals(s, RunResult, '2the '+vS+' Run Result error');

    RunResult := '';
    FillChar(FResults, SizeOf(FResults), 0);
    vObj.PublishedMethod3;
    CheckResults(vObj, @TTestBaseObj.PublishedMethod3);
    CheckEquals(s, RunResult, '3the '+vS+' Run Result error');

    RunResult := '';
    FillChar(FResults, SizeOf(FResults), 0);
    {$WARNINGS OFF}
    vChildObj := TTestChildObj.Create;
    {$WARNINGS ON}
    try
      vChildObj.DynamicMethod6;
      CheckResults(vChildObj, @TTestChildObj.DynamicMethod6);
    finally
     vChildObj.free;
    end;
    CheckEquals('TTestChildObj.DynamicMethod6 Run!', RunResult, 'ChildObj1 the '+vS+' Run Result error');

    RunResult := '';
    FillChar(FResults, SizeOf(FResults), 0);
    vObj.PublishedMethod3;
    CheckResults(vObj, @TTestBaseObj.PublishedMethod3);
    CheckEquals(s, RunResult, '4the '+vS+' Run Result error');
  finally
    FInterceptorClass.RemoveFrom(TTestBaseObj, @TTestBaseObj.PublishedMethod3);
  end;
   FillChar(FResults, SizeOf(FResults), 0);
   RunResult := '';
   vObj.PublishedMethod3;
   CheckEquals(s, RunResult, 'Unpatch_the '+vS+' Run Result error');
   CheckEqualsMem(@EmptyResults, @FResults, SizeOf(FResults), 'the interceptor is still work, NOT RemoveFrom!!');
 finally
   vObj.Free;
   //FreeAndNil(FInterceptor);
 end;

    RunResult := '';
    FillChar(FResults, SizeOf(FResults), 0);
    {$WARNINGS OFF} //Do not show WARNINGS info about abstract methods includes.
    vChildObj := TTestChildObj.Create;
    {$WARNINGS ON}
    try
      vChildObj.DynamicMethod6;
      CheckResults(vChildObj, @TTestChildObj.DynamicMethod6);
    finally
     vChildObj.free;
    end;
    CheckEquals('TTestChildObj.DynamicMethod6 Run!', RunResult, 'ChildObj2 the '+vS+' Run Result error');
    FInterceptorClass.RemoveFrom(TTestChildObj, @TTestChildObj.DynamicMethod6);
end;

procedure TTest_MeCustomInterceptor.Test_AddToMethodExceptionIn;
var
  vObj: TTestExceptionObj;
begin
  FInterceptor := FInterceptorClass.AddTo(TTestExceptionObj, @TTestExceptionObj.Method1, 'Method1');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    vObj := TTestExceptionObj.Create;
    try
      //StartExpectingException(ETestError);
      vObj.Method1;
      CheckResults(vObj, @TTestExceptionObj.Method1, ETestError, [esAllowed, esBefore, esException]);
      //StopExpectingException('Execute');
    finally
      vObj.Free;
    end;
  finally
    Check(FInterceptorClass.RemoveFrom(TTestExceptionObj, @TTestExceptionObj.Method1), 'Can not remove from TTestExceptionObj.Method1');
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToMethodExceptionBefore;
begin
  FInterceptor := FInterceptorClass.AddTo(TTestExceptionObj, @TTestExceptionObj.Method1, 'Method1');
  InitInterceptor(FInterceptor);
  try
    with TTestExceptionObj.Create do
    try
      FRaiseException := True;
      StartExpectingException(ETestError);
      Method1;
      StopExpectingException('BeforeExecute');
    finally
      Free;
    end;
  finally
    Check(FInterceptorClass.RemoveFrom(TTestExceptionObj, @TTestExceptionObj.Method1), 'Can not remove from TTestExceptionObj.Method1');
  end;
end;

(*
const
  cResult = 123454321;
  cResult64 = $123456787654321;
  cResultStr = '@@Hello test return str@@!!';

function TestStr: string;
begin
  Result := cResultStr;
  RunResult := Result;
end;

function TestInt64: Int64;
begin
{$I uMeMakeHole.inc}
  Result := cResult64;
  RunResult := IntToStr(Result);
end;

function TestInt: Integer;
begin
{$I uMeMakeHole.inc}
  Result := cResult;
  RunResult := IntToStr(Result);
end;

function TestFloat: double;
begin
  Result := cResult;
  RunResult := FloatToStr(Result);
end;

type
  TLArray = array [0..3] of byte;

  TLRec = packed record
    a,b,c,d: byte;
  end;
const
  cLRecResult: TLRec = (a:$11; b:$22; c:$33; d:$44); 
  cLAryResult: TLArray = ($11, $22, $33, $44); 

function TestLRec: TLRec;
begin
  Result := cLRecResult;
  RunResult := IntToStr(Integer(Result));
end;

function TestLAry: TLArray;
begin
  Result := cLAryResult;
  RunResult := IntToStr(Integer(Result));
end;

procedure TTest_MeCustomInterceptor.Test_AddToFunctionInt;
begin
  FInterceptor := FInterceptorClass.AddTo(@TestInt, 'TestInt');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    //StartExpectingException(ETestError);
    RunResult := '';
    CheckEquals(cResult, TestInt, 'function TestInt result is mismatch.');
    CheckEquals(IntToStr(cResult), RunResult, 'function TestInt do not run.');
    CheckResults(nil, @TestInt);
    //StopExpectingException('Execute');
  finally
    Check(FInterceptorClass.RemoveFrom(@TestInt), 'Can not remove from TestInt');
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToFunctionFloat;
begin
  FInterceptor := FInterceptorClass.AddTo(@TestFloat, 'TestFloat');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    //StartExpectingException(ETestError);
    RunResult := '';
    CheckEquals(cResult, TestFloat, 'function TestFloat result is mismatch.');
    CheckEquals(FloatToStr(cResult), RunResult, 'function TestFloat do not run.');
    CheckResults(nil, @TestFloat);
    //StopExpectingException('Execute');
  finally
    Check(FInterceptorClass.RemoveFrom(@TestFloat), 'Can not remove from TestFloat');
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToFunctionLittleRec;
var
  vRec: TLRec;
begin
  FInterceptor := FInterceptorClass.AddTo(@TestLRec, 'TestLRec');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    //StartExpectingException(ETestError);
    RunResult := '';
    vRec := TestLRec;
    CheckEquals(Integer(cLRecResult), Integer(vRec), 'function TestLRec result is mismatch.');
    CheckEquals(IntToStr(Integer(cLRecResult)), RunResult, 'function TestLRec do not run.');
    CheckResults(nil, @TestLRec);
    //StopExpectingException('Execute');
  finally
    Check(FInterceptorClass.RemoveFrom(@TestLRec), 'Can not remove from TestLRec');
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToFunctionLittleAry;
var
  vRec: TLArray;
begin
  FInterceptor := FInterceptorClass.AddTo(@TestLAry, 'TestLAry');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    //StartExpectingException(ETestError);
    RunResult := '';
    vRec := TestLAry;
    CheckEquals(Integer(cLAryResult), Integer(vRec), 'function TestLAry result is mismatch.');
    CheckEquals(IntToStr(Integer(cLAryResult)), RunResult, 'function TestLAry do not run.');
    CheckResults(nil, @TestLAry);
    //StopExpectingException('Execute');
  finally
    Check(FInterceptorClass.RemoveFrom(@TestLAry), 'Can not remove from TestLAry');
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToFunctionInt64;
begin
  FInterceptor := FInterceptorClass.AddTo(@TestInt64, 'TestInt64');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    RunResult := '';
    CheckEquals(cResult64, TestInt64, 'function TestInt64 result is mismatch.');
    CheckEquals(IntToStr(cResult64), RunResult, 'function TestInt64 do not run.');
    CheckResults(nil, @TestInt64);

    FInterceptor := TMeMyTestCustomInterceptor.AddTo(@TestInt64, 'TestInt64');
    InitInterceptor(FInterceptor);
    RunResult := '';
    CheckEquals(cResult64, TestInt64, 'function TestInt64 result is mismatch.');
    CheckEquals(IntToStr(cResult64), RunResult, 'function TestInt64 do not run.');
    CheckResults(nil, @TestInt64);
  finally
    Check(FInterceptorClass.RemoveFrom(@TestInt64), 'Can not remove from TestInt64');
  end;
end;

procedure TTest_MeCustomInterceptor.Test_AddToFunctionStr;
begin
  FInterceptor := FInterceptorClass.AddTo(@TestStr, 'TestStr');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    //StartExpectingException(ETestError);
    RunResult := '';
    CheckEquals(cResultStr, TestStr, 'function TestStr result is mismatch.');
    CheckEquals(cResultStr, RunResult, 'function TestStr do not run.');
    CheckResults(nil, @TestStr);
    //StopExpectingException('Execute');
  finally
    Check(FInterceptorClass.RemoveFrom(@TestStr), 'Can not remove from TestStr');
  end;
end;

function TTest_MeCustomInterceptor.TestMethodStr: string;
begin
  Result := cResultStr;
  RunResult := Result;
end;

procedure TTest_MeCustomInterceptor.Test_AddToFunctionMethodStr;
begin
  FInterceptor := FInterceptorClass.AddTo(TTest_MeCustomInterceptor, @TTest_MeCustomInterceptor.TestMethodStr, 'TestMethodStr');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    //StartExpectingException(ETestError);
    RunResult := '';
    CheckEquals(cResultStr, TestMethodStr, 'function TestMethodStr result is mismatch.');
    CheckEquals(cResultStr, RunResult, 'function TestMethodStr do not run.');
    CheckResults(Self, @TTest_MeCustomInterceptor.TestMethodStr);
    //StopExpectingException('Execute');
  finally
    Check(FInterceptorClass.RemoveFrom(TTest_MeCustomInterceptor, @TTest_MeCustomInterceptor.TestMethodStr), 'Can not remove from TestMethodStr');
  end;
end;

function TestInt64DLL: Int64; external 'dlltest.dll' name 'TestInt64';
const
  cResult64DLL = $1122334455667788;
procedure TTest_MeCustomInterceptor.Test_AddToFunctionInt64InDLL;
begin
  FInterceptor := FInterceptorClass.AddTo(@TestInt64DLL, 'TestInt64DLL');
  InitInterceptor(FInterceptor);
  try
    FillChar(FResults, SizeOf(FResults), 0);
    RunResult := '';
    CheckEquals(cResult64DLL, TestInt64DLL, 'function TestInt64DLL result is mismatch.');
    //CheckEquals(IntToStr(cResult64DLL), RunResult, 'function TestInt64DLL do not run.');
    CheckResults(nil, @TestInt64DLL);
  finally
    Check(FInterceptorClass.RemoveFrom(@TestInt64DLL), 'Can not remove from TestInt64DLL');
  end;
end;
*)

procedure TTest_MeInterceptor.SetUp;
begin
  inherited;
  //FInterceptorClass := TMeInterceptor;
end;

Initialization
  FillChar(EmptyResults, SizeOf(EmptyResults), 0);

  RegisterTests('MeInterceptor suites',
                [
                 TTest_MeCustomInterceptor.Suite
                 //, TTest_MeInterceptor.Suite

                 //TTestDQueue.Suite,
                 //TTestDStack.Suite }
                ]);//}
finalization
end.
