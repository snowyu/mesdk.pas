unit uMeSysUtilsTest;

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
  TestFramework
  , uMeObject
  , uMeSysUtils
  ;

type
  TTest_MeSysUtils = class(TTestCase)
  protected
    FObj: Pointer;
    FObj1: Pointer;
    FObjCreated: Boolean;
    FObj1Created: Boolean;
    FDone: Boolean;

    procedure DoNotifyFree(Instance: Pointer);
    procedure DoNotify1Free(Instance: Pointer);

    procedure Setup;override;
    procedure TearDown;override;

  public
  published
    procedure Test_FreeNotification;
  end;

implementation

{ TTest_MeSysUtils }
procedure TTest_MeSysUtils.Setup;
begin
  //FObj := TObject.Create;
end;

procedure TTest_MeSysUtils.TearDown;
begin
  //FreeAndNil(FObj);
end;

procedure TTest_MeSysUtils.Test_FreeNotification;
begin
  FObj := TObject.Create;
  FObj1 := New(PMeDynamicObject, Create);
  try
    FDone := False;
    FObjCreated := True;
    FObj1Created := True;
    AddFreeNotification(TObject(FObj), DoNotifyFree);
    AddFreeNotification(PMeDynamicObject(FObj1), DoNotify1Free);
    FDone := True;
  finally
    TObject(FObj).Free;
    FObj := nil;
    PMeDynamicObject(FObj1).Free;
    FObj1 := nil;
  end;
  CheckEquals(True, FDone, ' the TObject.FObjCreated is not done.');
  CheckEquals(False, FObjCreated, ' the TObject.FObjCreated Error.');
  CheckEquals(False, FObj1Created, ' the TMeDynamicObject.FObj1Created Error.');

  FObj := TList.Create;
  try
    FDone := False;
    FObjCreated := True;
    AddFreeNotification(FObj, DoNotifyFree);
    FDone := True;
  finally
    TObject(FObj).Free;
    FObj := nil;
  end;
  CheckEquals(False, FObjCreated, ' the TList.FObjCreated Error.');
  CheckEquals(True, FDone, ' the TList.FObjCreated is not done.');
end;

procedure TTest_MeSysUtils.DoNotify1Free(Instance: Pointer);
begin
  FObj1Created := False;
  CheckEquals(Integer(FObj1), Integer(Instance), ' the Instance should equ FObj1.');
end;

procedure TTest_MeSysUtils.DoNotifyFree(Instance: Pointer);
begin
  FObjCreated := False;
  CheckEquals(Integer(FObj), Integer(Instance), ' the Instance should equ FObj.');
end;

Initialization

  RegisterTests('MeSysUtils suites',
                [
                 TTest_MeSysUtils.Suite
                 //, TTest_MeSysUtils.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
