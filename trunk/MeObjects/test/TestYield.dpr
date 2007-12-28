program TestYield;

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
  ;

{.$Define YieldClass_Supports} //use the delphi class type instead.

procedure StringYieldProc(YieldObj: {$IFDEF YieldClass_Supports}TMeYieldObject{$ELSE} PMeYieldObject{$endif});
var  
  YieldValue: string;
  i: integer;
begin
  YieldValue:='None';
  YieldObj.Yield(YieldValue);
  for i := 1 to 10 do
  begin
    YieldValue := YieldValue + IntToStr(i);
    YieldObj.Yield(YieldValue);
  end;
end;

function GetEnumerator: {$IFDEF YieldClass_Supports}TYieldString {$ELSE}PYieldString{$ENDIF};
begin
  Result:= {$IFDEF YieldClass_Supports} TYieldString.Create(StringYieldProc){$ELSE}New(PYieldString, Create(StringYieldProc)){$ENDIF};
end;

{$IFDEF SUPPORTS_FOR_IN}
{$IFDEF YieldClass_Supports}
type
  TMyStrings = class
  public
    function GetEnumerator: TYieldString;
  end;

function TMyStrings.GetEnumerator: TYieldString;
begin
  Result:= TYieldString.Create(StringYieldProc);
end;

var
  s: string;
  vStrs: TMyStrings;
{$ENDIF}
{$ENDIF}
begin
    {$IFDEF SUPPORTS_FOR_IN}
      {$IFDEF YieldClass_Supports}
      writeln('Test For_In Yield tEnumerator:');
      vStrs := TMyStrings.Create;
      try
      for s in vStrs do
        Writeln(s);
      finally
       vStrs.Free;
      end;
     {$ENDIF}
   {$ENDIF}
    writeln(#13#10'Test Yield tEnumerator:');
    with GetEnumerator{$IFNDEF YieldClass_Supports}^{$endif} do
    try
      while MoveNext do
      begin
        Writeln(Current);
      end;
    finally
      Free;
    end;
{$IFDEF SUPPORTS_FOR_IN}
{$IFDEF YieldClass_Supports}
    s := '';
{$ENDIF}
{$ENDIF}
end.
