program TestRegEx;

{$I MeSetting.inc}

{$AppType Console}
uses
  FastMM4,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils
  {$IFNDEF YieldClass_Supports}
  , uMeObject
  {$ENDIF}
  , RegExpr
  , uMeRegExpr
  , uMeRegExprCoRoutine
  //, uMeEnumerator
  ;


var
  r: PMeCustomRegExpr;
  em: PMeRegExprEnumerator;
begin
  New(r, Create);
  New(em, Create(r));
  try try 
    r.InputString := 'this is not a Good New, but it is Better. it is not Bad.';
    r.Pattern := '/Good|Better|Bad/:n';
    while em.MoveNext() do
    begin
      if Assigned(r.RegExpr) then
        writeln('found: ', r.RegExpr.Match[0]);
    end;
  except
    on e: exception do writeln(e.ClassName, ' Exception:', e.message);
  end;
  finally
    r.free;
    em.free;
  end;
end.