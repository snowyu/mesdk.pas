unit uMeServiceMgrTest;

{$I MeService.inc}

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
  Variants,
  TestFramework
  , uMeObject
  , uMeStrUtils
  , uMeServiceTypes
  , uMeService
  , uMeServiceFunction
  , uMeServiceMgr
  , uMeServiceTest
  ;

type
  TAddProc = function (const a,b:integer): integer of object;

  TTest_MeCustomService = class(TTest_MeAbstractService)
  protected
    procedure Setup;override;
  public
  published
    procedure Test_RegisterFunction;
  end;

implementation

function Add(const a,b: integer): integer;
begin
  result := a+b;
end;

{ TTest_MeCustomService }
procedure TTest_MeCustomService.Setup;
begin
  FService := New(PMeCustomService, Create);
end;

type
  PMeServiceAccess = ^ TMeServiceAccess;
  TMeServiceAccess = object(TMeCustomService)
  end;

procedure TTest_MeCustomService.Test_RegisterFunction();
var
  vFunc: PMeServiceFunction;
  i, a, b: integer;
begin
  vFunc := PMeServiceAccess(FService).RegisterFunction('add', @Add, TypeInfo(TAddProc));
  for i := 0 to 9 do
  begin
    a := Random(MaxInt);
    b := Random(MaxInt);
    vFunc.ExecuteByArray([a,b]);
    CheckEquals(a+b, vFunc.ResultParam.Value, ' the function add result error.');
  
    {$IFDEF SUPPORTS_MESERVICE_CALLEX}
    vFunc.ExecuteByVariant(VarArrayOf([a,b]));
    CheckEquals(a+b, vFunc.ResultParam.AsInteger, ' the function add result error.');
    {$ENDIF}
  end;
end;

Initialization
  RegisterTests('MeService suites',
                [
                 TTest_MeCustomService.Suite
                 //, TTest_MeCustomService.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
