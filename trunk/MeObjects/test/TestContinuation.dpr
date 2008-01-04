program TestContinuation;

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

var
  vContinuationRec: TMeContinuationRec;


procedure YieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
var
  i: integer;
begin
    i := 0;
    writeln('white move: ', i);
    inc(i);
    YieldObj.MarkContinuation(vContinuationRec);
    writeln('white Markmove1: ', i);
    inc(i);
    YieldObj.Yield(i);
    inc(i);
    YieldObj.Yield(i);
end;

function GetMyEnumerator: {$IFDEF YieldClass_Supports}TYieldInteger {$ELSE}PYieldInteger{$ENDIF};
begin
  Result:= {$IFDEF YieldClass_Supports} TYieldInteger.Create(YieldProc){$ELSE}New(PYieldInteger, Create(YieldProc)){$ENDIF};
end;



begin
  //Wrong Usage:
  with GetMyEnumerator{$IFNDEF YieldClass_Supports}^{$endif} do
    try
      Reset;
      while MoveNext do
      begin
        //inc(i);
        Writeln(Current);
      end;
      Writeln('---CallCC---', Integer(vContinuationRec.StackFrameSize));

      CallCC(vContinuationRec);
      Writeln('resume:', Current);
      while MoveNext do
      begin
        Writeln('resume:', Current);
      end;

      Writeln('---CallCC Again---', Integer(vContinuationRec.StackFrameSize));
      if RestoreContinuation(vContinuationRec) then
      while MoveNext do
      begin
        Writeln('resume:', Current);
      end;
    finally
      Free;
    end;
  Writeln('---END');

end.