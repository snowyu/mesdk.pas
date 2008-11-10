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
    procedure CheckOpPush(var Mem: Pointer; const value: Integer);
    procedure CheckOpCallBind(var Mem: Pointer; const aFuncName: AnsiString);
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

procedure TTest_MeScript.CheckOpCallBind(var Mem: Pointer; const aFuncName: AnsiString);
begin
  CheckEquals(Ord(opCallBind), PByte(Mem)^, ' the OpCode opCallBind error.');
  Inc(Longword(Mem));
  CheckEquals(Length(aFuncName), PByte(Mem)^, ' the OpCode opCallBind error.');
  Inc(Longword(Mem));
  CheckEqualsMem(@aFuncName[1], Mem, Length(aFuncName), ' the OpCode opCallBind error.');
  Inc(Longword(Mem), Length(aFuncName));
end;

procedure TTest_MeScript.CheckOpPush(var Mem: Pointer; const value: Integer);
begin
  CheckEquals(Ord(opPush), PByte(Mem)^, ' the OpCode opPush error.');
  Inc(Longword(Mem));
  CheckEquals(value, PInteger(Mem)^, ' the OpPush value error.');
  Inc(Longword(Mem), SizeOf(Integer));
end;

procedure TTest_MeScript.Test_Compile();
var
  c: Pointer;
begin
  {
    opPush ParamCount
    opCallBind len(Name) Name
  }
  FScript.Parser('{aaa();}');
  c := FScript.Body.Memory;
{
  CheckEquals(Ord(opPush), Ord(c^), ' the OpCode opPush error.');
  Inc(c);
  CheckEquals(0, PInteger(c)^, ' the ParamCount error.');
}
  CheckOpPush(c, 0);
  CheckOpCallBind(c, 'aaa');
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
