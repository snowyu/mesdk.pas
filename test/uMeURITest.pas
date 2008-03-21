unit uMeURITest;

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
  , uMeURI
  ;

type
  TTest_MeURI = class(TTestCase)
  protected
    FURI: PMeURI;

    procedure Setup;override;
    procedure TearDown;override;

  public
  published
    procedure Test_Http;
    procedure Test_MailTo;
    procedure Test_RelavtiveURI;
  end;

implementation


var
  AppPath: string;


{ TTest_MeURI }
procedure TTest_MeURI.Setup;
begin
  New(FURI, Create);
end;

procedure TTest_MeURI.TearDown;
begin
  MeFreeAndNil(FURI);
end;

procedure TTest_MeURI.Test_Http();
begin
  FURI.URI := 'http://user:passd@host:port/root/doc/test.asp?param=123&p2=hahha#book2';
  CheckEquals('http', FURI.Protocol, ' the Protocol is error.');
  CheckEquals('host', FURI.Host, ' the Host is error.');
  CheckEquals('port', FURI.Port, ' the Port is error.');
  CheckEquals('param=123&p2=hahha', FURI.Params, ' the Params is error.');
  CheckEquals('test.asp', FURI.Document, ' the Document is error.');
  CheckEquals('user', FURI.UserName, ' the UserName is error.');
  CheckEquals('passd', FURI.Password, ' the Password is error.');

  FURI.URI := 'http:///user:passd@host:port/root/doc/test.asp?param=123&p2=hahha#book2';
  CheckEquals('http', FURI.Protocol, ' the Protocol is error.');
  CheckEquals('host', FURI.Host, ' the Host is error.');
  CheckEquals('port', FURI.Port, ' the Port is error.');
  CheckEquals('param=123&p2=hahha', FURI.Params, ' the Params is error.');
  CheckEquals('test.asp', FURI.Document, ' the Document is error.');
  CheckEquals('user', FURI.UserName, ' the UserName is error.');
  CheckEquals('passd', FURI.Password, ' the Password is error.');
end;

procedure TTest_MeURI.Test_MailTo();
begin
  FURI.URI := 'mailto://aa@tws.com';
  CheckEquals('mailto', FURI.Protocol, ' the Protocol is error.');
  CheckEquals('aa', FURI.UserName, ' the UserName is error.');
  CheckEquals('tws.com', FURI.Host, ' the Host is error.');

  FURI.URI := 'mailto:///aa@tws.com';
  CheckEquals('mailto', FURI.Protocol, ' the Protocol is error.');
  CheckEquals('aa', FURI.UserName, ' the UserName is error.');
  CheckEquals('tws.com', FURI.Host, ' the Host is error.');
  CheckEquals('mailto:///aa@tws.com', FURI.GetFullURI, ' the GetFullURI is error.');
end;

procedure TTest_MeURI.Test_RelavtiveURI;
begin
  FURI.URI := '/root/doc/test.asp?param=123&p2=hahha#book2';
  CheckEquals('', FURI.Protocol, ' the Protocol is error.');
  CheckEquals('', FURI.Host, ' the Host is error.');
  CheckEquals('', FURI.Port, ' the Port is error.');
  CheckEquals('param=123&p2=hahha', FURI.Params, ' the Params is error.');
  CheckEquals('test.asp', FURI.Document, ' the Document is error.');
  CheckEquals('', FURI.UserName, ' the UserName is error.');
  CheckEquals('', FURI.Password, ' the Password is error.');
end;

Initialization

  AppPath := ExtractFilePath(ParamStr(0));
  RegisterTests('MeURI suites',
                [
                 TTest_MeURI.Suite
                 //, TTest_MeURI.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
