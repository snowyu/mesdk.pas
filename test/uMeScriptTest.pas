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
  , uMeScriptConsts
  , uMeScript
  , uMeScriptInterpreter
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
var
  c: PChar;
begin
  FScript.Parser('{aaa();}');
  c := FScript.Body.Memory;
  CheckEquals(Ord(opPush), Ord(c^), ' the OpCode opPush error.');
  Inc(c);
  CheckEquals(0, PInteger(c)^, ' the ParamCount error.');
  
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
