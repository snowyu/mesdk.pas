{
  ##Project Name: DTest.dpr
  ##Initial Date: 2006-5-4
  Summary
    MeSDK Test Suite Project.

  Description
    the MeSDK for Delphi Library Test Suite 

  See Also
    参阅

  Bugs
    已知问题。

  Internal
    内部开发人员参阅，不会对外。

  TODO
    待作事项。

  Author
    Riceball LEE(riceball@cq118.com)
    Riceball LEE(riceballl@hotmail.com)

  Copyright
    Copyright(C) 2006-2008 by Riceball LEE

  Current Version
    $Revision: 1.3 $

  History
    版本历史。
}
program DTest;

{$I MeSetting.inc}

uses
  {$IFDEF MemCheck}
    {$IFNDEF COMPILER10_UP}
      {$IFNDEF CLR}
      FastMM4,
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF FPC}
  FastCode,
  FastMove,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils
  {$IFDEF Borland}
  , TestFramework
  , TestExtensions
  {$IFDEF LINUX}
  , QGUITestRunner
  {$ELSE}
  , GUITestRunner
  {$ENDIF}
  , TextTestRunner
  {$ENDIF}
  {$IFDEF FPC}
    , custapp
    , fpcunit
    , testreport
    , testregistry
  {$ENDIF}
  , uMeObjectTest
  //, uMeStreamTest
  {$IFNDEF FPC}
  , uFastCompareTextExTest
  , uMeTokenizerTest

  , uTypInfoExTest
  , uMeYieldTest
  , uMeCoRoutineTest
  , uMeTypInfoTest
  , uMeInjectorTest

  , uMeTypesTest
  , uMeProcTypeTest

  , uMeInterceptorTest
  , uMeFeature
  {$ENDIF}
  , uMeScriptTest
  , uMeURITest
  , uMeSysUtilsTest
  , uMeThreadTest
  ;


{$APPTYPE CONSOLE}

{$IFDEF FPC}
const
  ShortOpts = 'alh';
  Longopts: Array[1..5] of String = (
    'all','list','format:','suite:','help');

type
  TTestRunner = Class(TCustomApplication)
  private
    FXMLResultsWriter: TXMLResultsWriter;
  protected
    procedure   DoRun ; Override;
    procedure   doTestRun(aTest: TTest); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


constructor TTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLResultsWriter := TXMLResultsWriter.Create;
end;


destructor TTestRunner.Destroy;
begin
  FXMLResultsWriter.Free;
end;


procedure TTestRunner.doTestRun(aTest: TTest);
var
  testResult: TTestResult;
begin
  testResult := TTestResult.Create;
  try
    testResult.AddListener(FXMLResultsWriter);
    aTest.Run(testResult);
    FXMLResultsWriter.WriteResult(testResult);
  finally
    testResult.Free;
  end;
end;


procedure TTestRunner.DoRun;
var
  I : Integer;
  S : String;
begin
  S:=CheckOptions(ShortOpts,LongOpts);
  If (S<>'') then
    Writeln(S);
  if HasOption('h', 'help') or (ParamCount = 0) then
  begin
    writeln(Title);
    writeln(Version);
    writeln('Usage: ');
    writeln('-l or --list to show a list of registered tests');
    writeln('default format is xml, add --format=latex to output the list as latex source');
    writeln('-a or --all to run all the tests and show the results in xml format');
    writeln('The results can be redirected to an xml file,');
    writeln('for example: ./testrunner --all > results.xml');
    writeln('use --suite=MyTestSuiteName to run only the tests in a single test suite class');
  end
  else;
    if HasOption('l', 'list') then
    begin
      if HasOption('format') then
      begin
        if GetOptionValue('format') = 'latex' then
          writeln(GetSuiteAsLatex(GetTestRegistry))
        else
          writeln(GetSuiteAsXML(GetTestRegistry));
      end
      else
        writeln(GetSuiteAsXML(GetTestRegistry));
    end;
  if HasOption('a', 'all') then
  begin
    doTestRun(GetTestRegistry)
  end
  else
    if HasOption('suite') then
    begin
      S := '';
      S:=GetOptionValue('suite');
      if S = '' then
        for I := 0 to GetTestRegistry.Tests.count - 1 do
          writeln(GetTestRegistry[i].TestName)
      else
      for I := 0 to GetTestRegistry.Tests.count - 1 do
        if GetTestRegistry[i].TestName = S then
        begin
          doTestRun(GetTestRegistry[i]);
        end;
    end;
  Terminate;
end;


var
  App: TTestRunner;

begin
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console Test Case runner.';
  App.Run;
  App.Free;
end.

{$ENDIF}

{$IFDEF Borland}
{ NOTE:
  This program uses the test registration system.
  Units containing test cases register their test suites calling one of:
    TestFramework.RegisterTest
    TestFramework.RegisterTests
    TestFramework.RegisterTestSuites
}

{.$R *.RES}

const
  rcs_id :string = '#(@)$Id: DTest.dpr,v 1.3 2008/02/08 16:31:06 riceball Exp $';
  SwitchChars = ['-','/'];

procedure RunInConsoleMode;
begin
  try
    {$IFDEF MSWINDOWS}
    if not IsConsole then
      Windows.AllocConsole;
    {$ENDIF}
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  except
    on e:Exception do
      Writeln(Format('%s: %s', [e.ClassName, e.Message]));
  end;
end;

begin
  if FindCmdLineSwitch('c', SwitchChars, true) then
    RunInConsoleMode
  else begin
    {$IFDEF LINUX}
    TGUITestRunner.RunRegisteredTests;
    {$ELSE}
     	{$IFDEF BORLAND}
    GUITestRunner.RunRegisteredTests;
      {$ENDIF}
    {$ENDIF}
  end;
end.
{$ENDIF}
