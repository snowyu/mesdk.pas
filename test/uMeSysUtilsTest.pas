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
    FObj: TObject;
    FObjCreated: Boolean;

    procedure DoNotifyFree(Instance: TObject);

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
  try
    FObjCreated := True;
    AddFreeNotification(FObj, DoNotifyFree);
  finally
    FObj.Free;
    FObj := nil;
  end;
  CheckEquals(False, FObjCreated, ' the FObjCreated Error.');
end;

procedure TTest_MeSysUtils.DoNotifyFree(Instance: TObject);
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
