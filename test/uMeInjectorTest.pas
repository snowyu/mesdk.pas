unit uMeInjectorTest;

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
  , uMeTypInfo
  , uMeInjector
  , uMeTypInfoTest
  , uInternalTestObj
  ;

type
  //for virutal, dynamic method
  TInjectVirtualMethodProc = function (aClass: TClass; aIndex: Integer; aNewLocation: Pointer): Boolean of object;
  TTest_MeInjector = class (TCustomTest_Method)
  protected
    FInjector: TMeInjector;
    procedure TestInjectStaticMethod(aClass: TClass; aTestMethods: TTestMethods);
    procedure TestInjectPublishedMethod(aClass: TClass; aTestMethods: TTestMethods);
    //for virutal, dynamic method
    procedure TestInjectVirtualMethod(aClass: TClass; aTestMethods: TTestMethods; aInjectProc: TInjectVirtualMethodProc; aGetProc: TGetMethodByIndexProc);
  published
    procedure Test_InjectStaticMethod;
    procedure Test_InjectVirtualMethod;
    procedure Test_InjectDynamicMethod;
    procedure Test_InjectPublishedMethod;
  end;


implementation

{------------------------------------------------------------------}
procedure TTest_MeInjector.TestInjectPublishedMethod(aClass: TClass; aTestMethods: TTestMethods);
var
  i: Integer;
  vS: string;
  vProc: TNewMethodProc;
begin
  i :=Pos('_', ClassName);
  vS := Copy(ClassName, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(aClass.ClassName + ': Check '+ vS +' Test_InjectPublishedMethod');
  {$ENDIF}
  CreateObject(aClass);
  FInjector.Enabled := False;
  for i := 0 to High(aTestMethods) do
  begin
    {$IFDEF Debug_WriteToConsole_Support}
    //Status(aClass.ClassName + ': Check Before'+ aTestMethods[i].Name + '('+ IntToStr(i)+')');
    Status(aClass.ClassName + ': Check '+ aTestMethods[i].Name + '('+ IntToStr(i)+')');
    {$ENDIF}
    CheckFalse(FInjector.Enabled, 'the FInjector.Enabled is not false.');
    Check(FInjector.Inject(aTestMethods[i].Name, @TCustomTest_Method.TheNewMethod, aClass), aTestMethods[i].Name + ' Inject failed.');
    vProc := GetPublishedMethod(aClass, aTestMethods[i].Name);
    CheckEquals(cNewMethodResult, vProc(FObject), 'the Method '+aTestMethods[i].Name+ '(' + IntToStr(i) + ') called result mismatch.!');
    FInjector.Enabled := False;
  end;  //for
end;

procedure TTest_MeInjector.TestInjectStaticMethod(aClass: TClass; aTestMethods: TTestMethods);
var
  i: Integer;
  vS: string;
  vProc: TNewMethodProc;
begin
  i :=Pos('_', ClassName);
  vS := Copy(ClassName, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(aClass.ClassName + ': Check '+ vS +' Test_InjectStaticMethod');
  {$ENDIF}
  CreateObject(aClass);
  FInjector.Enabled := False;
  for i := 0 to High(aTestMethods) do
  begin
    {$IFDEF Debug_WriteToConsole_Support}
    //Status(aClass.ClassName + ': Check Before'+ aTestMethods[i].Name + '('+ IntToStr(i)+')');
    Status(aClass.ClassName + ': Check '+ aTestMethods[i].Name + '('+ IntToStr(i)+')');
    {$ENDIF}
    CheckFalse(FInjector.Enabled, 'the FInjector.Enabled is not false.');
    Check(FInjector.InjectStaticMethod(aClass, @aTestMethods[i].Method, @TCustomTest_Method.TheNewMethod, cX86JumpDirective), aTestMethods[i].Name + ' Inject failed.');
    vProc := @aTestMethods[i].Method;
    CheckEquals(cNewMethodResult, vProc(FObject), 'the Method '+aTestMethods[i].Name+ '(' + IntToStr(i) + ') called result mismatch.!');
    FInjector.Enabled := False;
  end;  //for
end;

procedure TTest_MeInjector.TestInjectVirtualMethod(aClass: TClass; aTestMethods: TTestMethods; aInjectProc: TInjectVirtualMethodProc
  ; aGetProc: TGetMethodByIndexProc);
var
  i: Integer;
  vS: string;
  vProc: TNewMethodProc;
begin
  i :=Pos('_', ClassName);
  vS := Copy(ClassName, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(aClass.ClassName + ': Check '+ vS +' Test_InjectVirtual/Dynamic Method');
  {$ENDIF}
  CreateObject(aClass);
  FInjector.Enabled := False;
  for i := 0 to High(aTestMethods) do
  begin
    {$IFDEF Debug_WriteToConsole_Support}
    //Status(aClass.ClassName + ': Check Before'+ aTestMethods[i].Name + '('+ IntToStr(i)+')');
    Status(aClass.ClassName + ': Check '+ aTestMethods[i].Name + '('+ IntToStr(i)+')');
    {$ENDIF}
    CheckFalse(FInjector.Enabled, 'the FInjector.Enabled is not false.');
    Check(aInjectProc(aClass, i, @TCustomTest_Method.TheNewMethod), aTestMethods[i].Name + ' Inject failed.');
    vProc := TNewMethodProc(aGetProc(aClass, i));
    CheckEquals(cNewMethodResult, vProc(FObject), 'the Method '+aTestMethods[i].Name+ '(' + IntToStr(i) + ') called result mismatch.!');
    FInjector.Enabled := False;
  end;  //for
end;

procedure TTest_MeInjector.Test_InjectStaticMethod;
begin
  with TestBaseObj do
    TestInjectStaticMethod(Obj, StaticMethods);
  with TestChildObj do
    TestInjectStaticMethod(Obj, StaticMethods);
end;

procedure TTest_MeInjector.Test_InjectVirtualMethod;
begin
  with TestBaseObj do
    TestInjectVirtualMethod(Obj, VirtualMethods, FInjector.InjectVirtualMethod, @GetVirtualMethod);
  with TestChildObj do
    TestInjectVirtualMethod(Obj, VirtualMethods, FInjector.InjectVirtualMethod, @GetVirtualMethod);
end;

procedure TTest_MeInjector.Test_InjectDynamicMethod;
begin
  with TestBaseObj do
    TestInjectVirtualMethod(Obj, DynamicMethods, FInjector.InjectDynamicMethod, @GetDynamicMethodBySlot);
  with TestChildObj do
    TestInjectVirtualMethod(Obj, DynamicMethods, FInjector.InjectDynamicMethod, @GetDynamicMethodBySlot);
end;

procedure TTest_MeInjector.Test_InjectPublishedMethod;
begin
  with TestBaseObj do
    TestInjectPublishedMethod(Obj, PublishedMethods);
  with TestChildObj do
    TestInjectPublishedMethod(Obj, PublishedMethods);
end;

Initialization
  RegisterTests('MeInjector suites',
                [
                 TTest_MeInjector.Suite

                 //TTestDQueue.Suite,
                 //TTestDStack.Suite }
                ]);//}
finalization
end.
