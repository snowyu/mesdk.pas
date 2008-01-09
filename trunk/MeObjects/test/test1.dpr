program Test1;

{.$I MeSetting.inc}

{$AppType Console}
uses
  //FastMM4,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils
  {$IFNDEF YieldClass_Supports}
  //, uMeObject
  {$ENDIF}
  , TestCorout
  ;

begin
  TestCoroutines;
end.