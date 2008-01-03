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

//控制逻辑：枚举器
procedure StringYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
var  
  YieldValue: string;
  i: integer;
begin
  YieldValue:='None';
  YieldObj.Yield(YieldValue);//返回行为逻辑
  for i := 1 to 10 do
  begin
    YieldValue := YieldValue + IntToStr(i);
    YieldObj.Yield(YieldValue); //返回行为逻辑
  end;
end;

//-------------------------------
//交替控制（过于滥用的法子,禁止在一个控制中调用另一个控制，因为这里不是用编译器在状态机实现的，这样只会引起堆栈溢出和死锁。）
var
  i: integer;
  w,b: integer;
procedure BlackYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});forward;

procedure WhiteYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
begin
  while true do
  begin
    writeln('white move: ', w);
    inc(w);
    YieldObj.Yield(w);
    BlackYieldProc(YieldObj);  
    //if w > 10001 then exit;
  end;
end;

procedure BlackYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
begin
    //if b > 10 then exit;
    writeln('black move: ', b);
    inc(b);
    YieldObj.Yield(b);
    //WhiteYieldProc(YieldObj); //<--禁止在过程之间中交替执行.!否则可能会堆栈溢出。
end;

function GetWhiteEnumerator: {$IFDEF YieldClass_Supports}TYieldInteger {$ELSE}PYieldInteger{$ENDIF};
begin
  Result:= {$IFDEF YieldClass_Supports} TYieldInteger.Create(WhiteYieldProc){$ELSE}New(PYieldInteger, Create(WhiteYieldProc)){$ENDIF};
end;
//-------------------------------

//-------------------------------
//pipeline pattern
type
  PYieldIntegerRange = ^ TYieldIntegerRange;
  TYieldIntegerRange = {$IFDEF YieldClass_Supports}class {$ELSE}object{$ENDIF}(TYieldInteger)
  public
    Min, Max: Integer;
    constructor Create(const aMin, aMax: Integer);
  end;

  PYieldIntegerMul = ^ TYieldIntegerMul;
  TYieldIntegerMul = {$IFDEF YieldClass_Supports}class {$ELSE}object{$ENDIF}(TYieldInteger)
  public
    Factor: Integer;
    Input: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif};
    constructor Create(const aFact: Integer; const aInput: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif});
  end;

  PYieldIntegerMod = ^ TYieldIntegerMod;
  TYieldIntegerMod = {$IFDEF YieldClass_Supports}class {$ELSE}object{$ENDIF}(TYieldInteger)
  public
    Factor: Integer;
    Input: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif};
    constructor Create(const aFact: Integer; const aInput: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif});
  end;

  PYieldIntegerJoin = ^ TYieldIntegerJoin;
  TYieldIntegerJoin = {$IFDEF YieldClass_Supports}class {$ELSE}object{$ENDIF}(TYieldInteger)
  public
    Input1, Input2: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif};
    constructor Create(const aInput1, aInput2: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif});
  end;

procedure RangeYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
var
  i: integer;
begin
  with {$IFDEF YieldClass_Supports}TYieldIntegerRange {$ELSE}PYieldIntegerRange{$ENDIF}(YieldObj){$IFNDEF YieldClass_Supports}^{$endif} do
  begin
    for i := Min to Max do Yield(i);
  end;
end;

procedure MulYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
var
  i: integer;
begin
  with {$IFDEF YieldClass_Supports}TYieldIntegerMul {$ELSE}PYieldIntegerMul{$ENDIF}(YieldObj){$IFNDEF YieldClass_Supports}^{$endif} do
  begin
    if Assigned(Input) then
    while Input.MoveNext do
    begin
      i := Input.Current*Factor;
      YieldObj.Yield(i);
    end;
  end;
end;

procedure ModYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
var
  i: integer;
begin
  with {$IFDEF YieldClass_Supports}TYieldIntegerMod {$ELSE}PYieldIntegerMod{$ENDIF}(YieldObj){$IFNDEF YieldClass_Supports}^{$endif} do
  begin
    if Assigned(Input) then
    while Input.MoveNext do
    begin
      i := Input.Current;
      if i mod Factor = 0 then
      begin
        YieldObj.Yield(i);
      end;
    end;
  end;
end;

procedure JoinYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
var
  i: integer;
begin
  with {$IFDEF YieldClass_Supports}TYieldIntegerJoin {$ELSE}PYieldIntegerJoin{$ENDIF}(YieldObj){$IFNDEF YieldClass_Supports}^{$endif} do
  begin
    if Assigned(Input1) then while Input1.MoveNext do
    begin
      i := Input1.Current;
      YieldObj.Yield(i);
    end;

    if Assigned(Input2) then while Input2.MoveNext do
    begin
      i := Input2.Current;
      YieldObj.Yield(i);
    end;
  end;
end;

constructor TYieldIntegerRange.Create(const aMin, aMax: Integer);
begin
  Min := aMin; Max := aMax;
  inherited Create(RangeYieldProc);
end;

constructor TYieldIntegerMul.Create(const aFact: Integer; const aInput: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif});
begin
  Factor:= aFact; Input := aInput;
  inherited Create(MulYieldProc);
end;

constructor TYieldIntegerMod.Create(const aFact: Integer; const aInput: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif});
begin
  Factor:= aFact; Input := aInput;
  inherited Create(ModYieldProc);
end;

constructor TYieldIntegerJoin.Create(const aInput1, aInput2: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif});
begin
  Input1:= aInput1; Input2 := aInput2;
  inherited Create(JoinYieldProc);
end;

//-------------------------------


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
var
  RangeI,Range1I, MulI, JoinI, ModI: {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE} PYieldInteger{$endif};

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
  
  //Wrong Usage:
  w := 0; b:= 0; i := 0;
  with GetWhiteEnumerator{$IFNDEF YieldClass_Supports}^{$endif} do
    try
      while MoveNext do
      begin
        inc(i);
        if i > 21 then break;
        //Writeln(Current);
      end;
    finally
      Free;
    end;


    //Wrong Usage *)
RangeI := {$IFDEF YieldClass_Supports}TYieldIntegerRange.Create(-4,-2){$ELSE}New(PYieldIntegerRange, Create(-4,-2)){$ENDIF};
Range1I := {$IFDEF YieldClass_Supports}TYieldIntegerRange.Create(1,10){$ELSE}New(PYieldIntegerRange, Create(1,10)){$ENDIF};
MulI := {$IFDEF YieldClass_Supports}TYieldIntegerMul.Create(2, Range1I){$ELSE}New(PYieldIntegerMul, Create(2, Range1I)){$ENDIF};
ModI := {$IFDEF YieldClass_Supports}TYieldIntegerMod.Create(3, MulI){$ELSE}New(PYieldIntegerMod, Create(3, MulI)){$ENDIF};
JoinI := {$IFDEF YieldClass_Supports}TYieldIntegerJoin.Create(RangeI, ModI){$ELSE}New(PYieldIntegerJoin, Create(RangeI, ModI)){$ENDIF};
while JoinI.MoveNext do
  Writeln(JoinI.Current);
MulI.free;
RangeI.free;
JoinI.free;
ModI.free;
Range1I.free;
end.