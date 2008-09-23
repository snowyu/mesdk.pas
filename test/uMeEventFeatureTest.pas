unit uMeEventFeatureTest;

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
  , uMeSystem
  , uMeEventFeature
  ;

type
  TTest_MeEventFeature = class(TTestCase)
  protected
    procedure Setup;override;
    procedure TearDown;override;

  public
  published
    procedure Test_Event;
  end;

implementation


var
  AppPath: string;


{ TTest_MeEventFeature }
procedure TTest_MeEventFeature.Setup;
begin
  //New(FScript, Create);
end;

procedure TTest_MeEventFeature.TearDown;
begin
  //MeFreeAndNil(FScript);
end;

procedure TTest_MeEventFeature.Test_Event();
begin
  //CheckEquals(Ord(opPush), Ord(c^), ' the OpCode opPush error.');
  //Inc(c);
  //CheckEquals(0, PInteger(c)^, ' the ParamCount error.');
end;

Initialization

  AppPath := ExtractFilePath(ParamStr(0));
  RegisterTests('MeEvent suites',
                [
                 TTest_MeEventFeature.Suite
                 //, TTest_MeEventFeature.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
