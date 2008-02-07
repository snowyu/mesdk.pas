program TestY;

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
  , uMeYield
  , uMeEnumerator
  ;


var
  s: string;
  aStrs: TStringList;
begin
  aStrs := TStringList.Create;
  aStrs.Add('1');
  aStrs.Add('2');
  aStrs.Add('3');
  try 
    for s in EnumStrs(aStrs, @StrsReverse) do 
    begin
      writeln(s);
      try
        raise exception.Create('helllo exception'); //<-- exception stack protected miss.
      except
        on e: exception do writeln(e.message);
      end;
    end;
  finally
    astrs.Free;
  end;
end.