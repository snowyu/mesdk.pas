unit uRegExprTest;

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
  , uRegExpr
  , uMeSysUtils
  ;

type
  TTest_RegExpr = class(TTestCase)
  protected
    FRegEx: TRegExpr;
    procedure Setup;override;
    procedure TearDown;override;

    function GetExpression: string;
    procedure CheckExpr(const aSubExprMatchCount, aPos, aLen: integer);
    procedure CheckSubExpr(const aSubExprValues: array of string; const aSubExprNames: array of string);
  public
  published
    procedure Test_RegExpr;
  end;

implementation

{ TTest_RegExpr }
procedure TTest_RegExpr.Setup;
begin
  FRegEx := TRegExpr.Create;
end;

procedure TTest_RegExpr.TearDown;
begin
  FreeAndNil(FRegEx);
end;

function TTest_RegExpr.GetExpression: string;
begin
  Result := Format('Expression: %s;  Modifiers: %s; InputString: %s', [FRegEx.Expression, FRegEx.ModifierStr, FRegEx.InputString]);
end;

procedure TTest_RegExpr.CheckSubExpr(const aSubExprValues: array of string; const aSubExprNames: array of string);
var
  i: Integer;
begin
  for i := Low(aSubExprValues) to High(aSubExprValues) do
  begin
    CheckEquals(aSubExprValues[i], FRegEx.Match[i], ' the Match['+IntToStr(i)+'] Error. ' + GetExpression);
  end;
  {$IFDEF SubExprName_RegExpr}
  for i := Low(aSubExprNames) to High(aSubExprNames) do
  begin
    CheckEquals(aSubExprNames[i], FRegEx.SubExprNames[i], ' the SubExprNames['+IntToStr(i)+'] Error. ' + GetExpression);
  end;
  {$ENDIF}
end;

procedure TTest_RegExpr.CheckExpr(const aSubExprMatchCount, aPos, aLen : integer);
begin
  CheckEquals(aSubExprMatchCount, FRegEx.SubExprMatchCount, 'the SubExprMatchCount Error. '+GetExpression);
  CheckEquals(aPos, FRegEx.MatchPos[0], ' the MatchPos Error. '+GetExpression);
  CheckEquals(aLen, FRegEx.MatchLen[0], ' the MatchLen Error. '+GetExpression);
end;

procedure TTest_RegExpr.Test_RegExpr;
begin
   FRegEx.Expression := '[A-Z]';
   FRegEx.Exec ('234578923457823659GHJK38');
   CheckExpr (0, 19, 1);

   FRegEx.Expression := '[A-Z]*?';
   FRegEx.Exec ('234578923457823659ARTZU38');
   CheckExpr (0, 1, 0);

   FRegEx.Expression := '[A-Z]+';
   FRegEx.Exec ('234578923457823659ARTZU38');
   CheckExpr (0, 19, 5);

   FRegEx.Expression := '[A-Z][A-Z]*';
   FRegEx.Exec ('234578923457823659ARTZU38');
   CheckExpr (0, 19, 5);

   FRegEx.Expression := '[A-Z][A-Z]?';
   FRegEx.Exec ('234578923457823659ARTZU38');
   CheckExpr (0, 19, 2);

   FRegEx.Expression := '[^\d]+';
   FRegEx.Exec ('234578923457823659ARTZU38');
   CheckExpr (0, 19, 5);

   { test chaining }

   FRegEx.Expression := '[A-Z][A-Z]?[A-Z]';
   FRegEx.Exec ('234578923457823659ARTZU38');
   CheckExpr (0, 19, 3);

   FRegEx.Expression := '[A-Z][A-Z]*[0-9]';
   FRegEx.Exec ('234578923457823659ARTZU38');
   CheckExpr (0, 19, 6);

   FRegEx.Expression := '[A-Z]+[0-9]';
   FRegEx.Exec ('234578923457823659ARTZU38');
   CheckExpr (0, 19, 6);

   { case insensitive: }
   FRegEx.ModifierI := True;

   FRegEx.Expression := '[A-Z]';
   FRegEx.Exec ('234578923457823659a38');
   CheckExpr (0, 19, 1);

   { case insensitive: }
   FRegEx.Expression := '[a-z]';
   FRegEx.Exec ('234578923457823659A38');
   CheckExpr (0, 19, 1);

   FRegEx.ModifierI := False;

   { with parenthsis }
   FRegEx.Expression := '(foo)1234';
   FRegEx.Exec ('1234   foo1234XXXX');
   CheckExpr (1, 8, 7);
   CheckSubExpr(['foo1234','foo'], []);

   FRegEx.Expression := '(((foo)))1234';
   FRegEx.Exec ('1234   foo1234XXXX');
   CheckExpr (3, 8, 7);
   CheckSubExpr(['foo1234','foo','foo','foo'], []);

   FRegEx.Expression := '(foo)(1234)';
   FRegEx.Exec ('1234   foo1234XXXX');
   CheckExpr (2, 8, 7);
   CheckSubExpr(['foo1234','foo','1234'], []);

   { test real backtracking }

   FRegEx.Expression := 'nofoo|foo';
   FRegEx.Exec ('1234   foo1234XXXX');
   CheckExpr (0, 8, 3);

   FRegEx.Expression := '(nofoo|foo)1234';
   FRegEx.Exec ('1234   nofoo1234XXXX');
   CheckExpr (1, 8, 9);
   CheckSubExpr(['nofoo1234','nofoo'], []);

   FRegEx.Expression := '(nofoo|foo|anotherfoo)1234';
   FRegEx.Exec ('1234   nofoo1234XXXX');
   CheckExpr (1, 8, 9);
   CheckSubExpr(['nofoo1234','nofoo'], []);

   FRegEx.Expression := 'nofoo1234|foo1234';
   FRegEx.Exec ('1234   foo1234XXXX');
   CheckExpr (0, 8, 7);

   FRegEx.Expression := '<a\s+href=([''|"])(?<URI>.+?)\-2.?>(?<Name>.+?)</a>';
   FRegEx.Exec ('<td>New</td><a href="http://dd.com/hi">Hello</a>!@EER');
   CheckExpr (3, 13, 36);
   CheckSubExpr(['<a href="http://dd.com/hi">Hello</a>','"', 'http://dd.com/hi', 'Hello'], ['','','URI','Name']);

   FRegEx.Expression := '<a\s+href=([''|"])(.+?):URI:\-2.?>(.+?):Name:</a>';
   FRegEx.Exec ('<td>New</td><a href="http://dd.com/hi">Hello</a><>');
   CheckExpr (3, 13, 36);
   CheckSubExpr(['<a href="http://dd.com/hi">Hello</a>','"', 'http://dd.com/hi', 'Hello'], ['','','URI','Name']);

   FRegEx.Expression := '<a\s+href=([''|"])(?''URI''.+?)\-2.?>(?P<Name>.+?)</a>';
   FRegEx.Exec ('<td>New</td><a href="http://dd.com/hi">Hello</a>Nothing');
   CheckExpr (3, 13, 36);
   CheckSubExpr(['<a href="http://dd.com/hi">Hello</a>','"', 'http://dd.com/hi', 'Hello'], ['','','URI','Name']);

end;

Initialization

  RegisterTests('RegExpr suites',
                [
                 TTest_RegExpr.Suite
                 //, TTest_RegExpr.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
