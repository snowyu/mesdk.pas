{
  <(?:[^>'"]*|".*?"|'.*?')+>
  DIRegEx: test HTML.txt
    Run: Total Exectution time: 34,061 Time per match 10.
    found count:2759
  RegEx:
  : Compile Time: 40
    Run: 1,691,376
}
program TestRegEx;

{$I MeSetting.inc}

{$AppType Console}
uses
  //FastMM4,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils
  , uMeObject
  , uMeStrUtils
  , RegExpr
  , uMeRegExpr
  , uMeRegExprCoRoutine
  //, uMeEnumerator
  ;


var
  r: PMeRegExpr;
  r1 : TRegExpr;
  em: PMeRegExprEnumerator;
  vStrs: PMeStrings;
  vs: TStringList;
  c: Integer;
  t1,t2: Int64;
begin
  New(r, Create);
  New(em, Create(r));
  New(vStrs, Create);
  vs := TStringList.Create;
  r1 := TRegExpr.Create;
  try try 
    r.Pattern := '/<([^>''"]*|".*?"|''.*?'')>/:n';
    r1.Expression :=  '<([^>''"]*|".*?"|''.*?'')>';
    QueryPerformanceCounter(t1);
    r1.Compile;
    QueryPerformanceCounter(t2);
    writeln('Compile time: ', t2-t1);
    vs.LoadFromFile(ExtractFilePath(ParamStr(0))+'html.txt');
    r1.InputString := trim(vs.Text);
    r.Name := 'root';
    c := 1;
    QueryPerformanceCounter(t1);
    if r1.Exec then
      while r1.ExecNext do
        inc(c);
    QueryPerformanceCounter(t2);
    writeln('found count: ',c, ' time:', t2-t1);
    exit;
    
    while em.MoveNext() do
    begin
      Inc(c);
    end;
    QueryPerformanceCounter(t2);
    writeln('found count: ',c, ' time:', t2-t1);
    exit;

    {$IFDEF SubExprName_Support}
    r.AddExpr('ListBegin', '/<td>(.+?):field:</td>/', 1);
    r.Pattern := '/(Good|Better|Bad):thing:[[ListBegin]]/:n';
    {$ELSE}
    r.AddExpr('ListBegin', '/<td>(.+?)</td>/', 1);
    r.Pattern := '/(Good|Better|Bad)[[ListBegin]]/:n';
    {$ENDIF}
    r.InputString := 'this is not a Good <td>New</td>, but it is Better<td>New</td>. it is not Bad<td>New</td>.';
    r.Name := 'root';
    while em.MoveNext() do
    begin
      if Assigned(r.RegExpr) then
        writeln(em.Current.Name, ' found: ', r.RegExpr.Match[0]);
    end;
    r.GetMatchResult(vStrs);
    Writeln('---------------------------');
    Writeln('The SubExprs Result:');
    Writeln(vStrs.Text);
    Writeln('---------------------------');
    r.SavePatternToStrs(vStrs);
    Writeln(Trim(vStrs.Text));
    Writeln('---------------------------');
    r.LoadPatternFromStrs(vStrs);
    r.InputString := 'this is not a Good <td>New</td>, but it is Better<td>New</td>. it is not Bad<td>New</td>.';
    r.Name := 'root';
    em.Reset;
    while em.MoveNext() do
    begin
      if Assigned(r.RegExpr) then
        writeln(em.Current.Name, ' found: ', r.RegExpr.Match[0]);
    end;
  except
    on e: exception do writeln(e.ClassName, ' Exception:', e.message);
  end;
  finally
    r.free;
    em.free;
    vStrs.Free;
  end;
end.