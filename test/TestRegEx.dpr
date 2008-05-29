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
  , RegExpr
  , uMeRegExpr
  , uMeRegExprCoRoutine
  //, uMeEnumerator
  ;


var
  r: PMeCustomRegExpr;
  em: PMeRegExprEnumerator;
  vStrs: PMeStrings;
begin
  New(r, Create);
  New(em, Create(r));
  New(vStrs, Create);
  try try 
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
  except
    on e: exception do writeln(e.ClassName, ' Exception:', e.message);
  end;
  finally
    r.free;
    em.free;
    vStrs.Free;
  end;
end.