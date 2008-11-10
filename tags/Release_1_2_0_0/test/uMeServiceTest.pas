unit uMeServiceTest;

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
  TestFramework
  , uMeObject
  , uMeStrUtils
  , uMeServiceTypes
  , uMeService
  ;

type
  TTest_MeAbstractService = class(TTestCase)
  protected
    FService: PMeAbstractService;

    procedure Setup;override;
    procedure TearDown;override;

  public
  published
    procedure Test_Info;
  end;

implementation

{ TTest_MeAbstractService }
procedure TTest_MeAbstractService.Setup;
begin
  New(FService, Create);
end;

procedure TTest_MeAbstractService.TearDown;
begin
  MeFreeAndNil(FService);
end;

type
  TMeServiceAccess = object(TMeAbstractService)
  end;

procedure TTest_MeAbstractService.Test_Info();
var
  i: Integer;

begin
  for i := MEAPI_INFO_FIRST to MEAPI_INFO_LAST do
  begin
    TMeServiceAccess(FService^).FInfo[i] := 'MInfo' + IntToStr(i);
    CheckEquals(TMeServiceAccess(FService^).FInfo[i], FService.Info[i], ' the FService.Info[' + IntToStr(i) + '] error.');
  end;
  CheckEquals(TMeServiceAccess(FService^).FInfo[MEAPI_NAME], FService.Name, ' the FService.Name error.');
  CheckEquals(TMeServiceAccess(FService^).FInfo[MEAPI_VER], FService.ProtocolVersion, ' the FService.ProtocolVersion error.');
  CheckEquals(TMeServiceAccess(FService^).FInfo[MEAPI_SERVICE_VER], FService.Version, ' the FService.Version error.');
  
  FService.Name := 'MyServiceName';
  CheckEquals('MyServiceName', TMeServiceAccess(FService^).FInfo[MEAPI_NAME], ' the FService.Info[Name] set error.');
  CheckEquals(FService.Name, TMeServiceAccess(FService^).FInfo[MEAPI_NAME], ' the FService.Name set error.');

  FService.ProtocolVersion := '0.0.0.1';
  CheckEquals('0.0.0.1', TMeServiceAccess(FService^).FInfo[MEAPI_VER], ' the FService.Info[MEAPI_VER] set error.');
  CheckEquals(FService.ProtocolVersion, TMeServiceAccess(FService^).FInfo[MEAPI_VER], ' the FService.ProtocolVersion set error.');

  FService.Version := 'Ver23232';
  CheckEquals('Ver23232', TMeServiceAccess(FService^).FInfo[MEAPI_SERVICE_VER], ' the FService.Info[Version] set error.');
  CheckEquals(FService.Version, TMeServiceAccess(FService^).FInfo[MEAPI_SERVICE_VER], ' the FService.Version set error.');
end;

Initialization

  RegisterTests('MeService suites',
                [
                 TTest_MeAbstractService.Suite
                 //, TTest_MeAbstractService.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
