unit uFastCompareTextExTest;

{$I MeSetting.inc}

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
  TestFramework
  , uMeSystem
  , FastcodeCompareTextExUnit
  ;

type
  TTest_FastCompareTextEx = class(TTestCase)
  protected
    procedure TestFastCompareTextEx(const aFunc: FastcodeCompareTextExFunction; const aStr: string);
  published
    procedure Test_FastCompareTextEx;
  end;

implementation

uses
  FastcodeCPUID;

procedure TTest_FastCompareTextEx.TestFastCompareTextEx(const aFunc: FastcodeCompareTextExFunction; const aStr: string);
var
  s1, s2: string;
  i: integer;
begin
  s1 := '';
  s2 := '';
  i := aFunc(nil, nil, Min(Length(s1), Length(s2)));
  CheckEquals(0, i, 'the '+aStr+' result is error.');

  s1 := '123';
  s2 := '';
  i := aFunc(@s1[1], nil, 3);
  CheckEquals(3, i, 'the '+aStr+' result is error.');

  s1 := '';
  s2 := '123';
  i := aFunc(nil, @s2[1], 3);
  CheckEquals(-3, i, 'the '+aStr+' result is error.');

  s1 := '123';
  s2 := '123';
  i := aFunc(@s1[1], @s2[1], Min(Length(s1), Length(s2)));
  CheckEquals(0, i, 'compare the "'+ s1 + '" and "'+ s2 + '", the '+aStr+' result is error.');

  s1 := '23D中华人民共和国A';
  s2 := '23d中华人民共和国a';
  i := aFunc(@s1[1], @s2[1], Min(Length(s1), Length(s2)));
  CheckEquals(0, i, 'compare the "'+ s1 + '" and "'+ s2 + '", the '+aStr+' result is error.');

  s1 := '23DA';
  s2 := '23d ';
  i := aFunc(@s1[1], @s2[1], Min(Length(s1), Length(s2)));
  CheckTrue(i>0, 'compare the "'+ s1 + '" and "'+ s2 + '", the '+aStr+' result is error.');

  s1 := '23D ';
  s2 := '23dA';
  i := aFunc(@s1[1], @s2[1], Min(Length(s1), Length(s2)));
  CheckTrue(i<0, 'compare the "'+ s1 + '" and "'+ s2 + '", the '+aStr+' result is error.');

  s1 := '23D中华人民共和国A';
  s2 := '23d中华人民共和国 ';
  i := aFunc(@s1[1], @s2[1], Min(Length(s1), Length(s2)));
  CheckTrue(i>0, 'compare the "'+ s1 + '" and "'+ s2 + '", the '+aStr+' result is error.');

  s1 := '23D中华人民共和国 ';
  s2 := '23d中华人民共和国a';
  i := aFunc(@s1[1], @s2[1], Min(Length(s1), Length(s2)));
  CheckTrue(i<0, 'compare the "'+ s1 + '" and "'+ s2 + '", the '+aStr+' result is error.');
end;

procedure TTest_FastCompareTextEx.Test_FastCompareTextEx;
begin
  TestFastCompareTextEx(CompareTextEx_Sha_IA32_3, 'CompareTextEx_Sha_IA32_3');
  TestFastCompareTextEx(CompareTextEx_JOH_IA32_5, 'CompareTextEx_JOH_IA32_5');
  TestFastCompareTextEx(CompareTextEx_JOH_IA32_6, 'CompareTextEx_JOH_IA32_6');
  TestFastCompareTextEx(CompareTextEx_Sha_IA32_4, 'CompareTextEx_Sha_IA32_4');

  //TestFastCompareTextEx(CompareTextEx_Sha_Pas_5, 'CompareTextEx_Sha_Pas_5');
end;

Initialization

  RegisterTests('MeTypes suites',
                [
                 TTest_FastCompareTextEx.Suite
                 //, TTest_FastCompareTextEx.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
