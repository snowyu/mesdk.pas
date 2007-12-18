unit uMeScriptTest;

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
  , uMeStrUtils
  , uMeTokenizer
  , uMeScriptConst
  , uMeScript
  ;

type
  TTest_MeScript = class(TTestCase)
  protected
    FScript: PMeScriptGlobalFunction;

    procedure Setup;override;
    procedure TearDown;override;

  public
  published
    procedure Test_Compile;
  end;

implementation


var
  AppPath: string;


{ TTest_MeScript }
procedure TTest_MeScript.Setup;
begin
  New(FScript, Create);
end;

procedure TTest_MeScript.TearDown;
begin
  MeFreeAndNil(FScript);
end;

procedure TTest_MeScript.Test_Compile();
begin
  //CheckEquals(aExcept.Size, aFact.Size, aExcept.Token+ ' the Token Size is error.');
end;

Initialization

  AppPath := ExtractFilePath(ParamStr(0));
  RegisterTests('MeScripts suites',
                [
                 TTest_MeScript.Suite
                 //, TTest_MeScript.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
