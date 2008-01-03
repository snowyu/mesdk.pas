unit uMeYieldTest;

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
  , uMeYield
  ;

type
  TTest_CoRoutine = class(TTestCase)
  protected
    FCoRoutine: {$IFDEF YieldClass_Supports}TMeCoRoutine {$ELSE}PMeCoRoutine{$ENDIF};
    procedure Setup;override;
    procedure TearDown;override;
    function CreateObject: {$IFDEF YieldClass_Supports}TMeCoRoutine {$ELSE}PMeCoRoutine{$ENDIF};virtual; abstract;
  public
  
  published
    procedure Test_CCC;
  end;


implementation


var
  AppPath: string;


{ TTest_CoRoutine }
procedure TTest_CoRoutine.Setup;
begin
  inherited;
  FCoRoutine := CreateObject;
end;

procedure TTest_CoRoutine.TearDown;
begin
  if Assigned(FCoRoutine) then
  begin
    FCoRoutine.Free;
    FCoRoutine := nil;
  end;
end;

Initialization

  AppPath := ExtractFilePath(ParamStr(0));
  RegisterTests('MeCoRoutine suites',
                [
                 TTest_SetType.Suite
                 //, TTest_EnumerationType.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
