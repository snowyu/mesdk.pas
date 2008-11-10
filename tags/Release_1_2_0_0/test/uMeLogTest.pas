unit uMeLogTest;

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
  , uMeLog
  ;

type
  TTest_MeLogger = class(TTestCase)
  protected
    FLogger: PMeCustomLogger;

    procedure Setup;override;
    procedure TearDown;override;
    procedure VerifyLogInfo(const aLevel: TMeLogLevel; const aMsg: string);virtual;

  public
  published
    procedure Test_Logger;
  end;

  TTest_MeStringsLogger = class(TTest_MeLogger)
  protected
    FStrings: PMeStrings;
    procedure Setup;override;
    procedure TearDown;override;
    procedure VerifyLogInfo(const aLevel: TMeLogLevel; const aMsg: string);override;
  public
  end;

implementation

{ TTest_MeLogger }
procedure TTest_MeLogger.Setup;
begin
  //FObj := TObject.Create;
  GLogger.Open;
end;

procedure TTest_MeLogger.TearDown;
begin
  //FreeAndNil(FObj);
end;

procedure TTest_MeLogger.VerifyLogInfo(const aLevel: TMeLogLevel; const aMsg: string);
begin
  //CheckEquals(False, FObjCreated, ' the FObjCreated Error.');
end;

procedure TTest_MeLogger.Test_Logger;
begin
  GLogger.Level := vlAll;
  GLogger.Close;

  GLogger.Open;
  GLogger.Log(vlError, 'test my Logger!');
  VerifyLogInfo(vlError, 'test my Logger!');
  GLogger.Close;

  //GLogger.Open;
end;


{ TTest_MeStringsLogger }
procedure TTest_MeStringsLogger.Setup;
begin
  New(FStrings, Create);
  GLogger.Register(New(PMeStringsLogger, Create(FStrings)));
  inherited;
end;

procedure TTest_MeStringsLogger.TearDown;
begin
  inherited;
  FStrings.Free;
end;

procedure TTest_MeStringsLogger.VerifyLogInfo(const aLevel: TMeLogLevel; const aMsg: string);
begin
  //CheckEquals(False, FObjCreated, ' the FObjCreated Error.');
end;

Initialization

  RegisterTests('MeLogger suites',
                [
                 TTest_MeStringsLogger.Suite
                 //, TTest_MeLogger.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
