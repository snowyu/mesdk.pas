unit uMeCoRoutineTest;

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
  , uMeCoRoutine
  ;

type
  {$IFNDEF YieldClass_Supports}
  PYieldString = ^TYieldString;
  PYieldInteger = ^TYieldInteger;
  {$ENDIF}

  TTest_CoRoutine = class(TTestCase)
  protected
    FCoRoutine: {$IFDEF YieldClass_Supports}TMeCoRoutineEnumerator {$ELSE}PMeCoRoutineEnumerator{$ENDIF};
    procedure Setup;override;
    procedure TearDown;override;
    function CreateObject: {$IFDEF YieldClass_Supports}TMeCoRoutineEnumerator {$ELSE}PMeCoRoutineEnumerator{$ENDIF};virtual; abstract;
  public
  end;

  TTest_CoRoutineYieldString = class(TTest_CoRoutine)
  protected
    function CreateObject: {$IFDEF YieldClass_Supports}TMeCoRoutineEnumerator {$ELSE}PMeCoRoutineEnumerator{$ENDIF};override;
  published
    procedure Test_Yield;
  end;

  TTest_CoRoutineYieldInteger = class(TTest_CoRoutine)
  protected
    function CreateObject: {$IFDEF YieldClass_Supports}TMeCoRoutineEnumerator {$ELSE}PMeCoRoutineEnumerator{$ENDIF};override;
  published
    procedure Test_Yield;
  end;

  TYieldString = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCoRoutineEnumerator)
  protected
    FValue:String;
    function GetCurrent:string;
    procedure SetNextValue(const aValue); {$IFDEF YieldClass_Supports}override{$ELSE} virtual{$endif}; 
  public
    {$IFNDEF YieldClass_Supports}
    destructor Destroy; virtual; {override}
    {$ENDIF}

    property Current:string read GetCurrent; //D2007 enumerable required
  end;

  TYieldInteger = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCoRoutineEnumerator)
  protected
    FValue: Integer;
    function GetCurrent: Integer;
    procedure SetNextValue(const aValue); {$IFDEF YieldClass_Supports}override{$ELSE} virtual{$endif}; 
  public

    //return the Current value
    property Current:Integer read GetCurrent; //D2007 enumerable required
  end;


implementation

const
  rsTestStringException = 'MyStringYieldException Test';

//Fibonacc
procedure IntegerYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoRoutineEnumerator {$ELSE}PMeCoRoutineEnumerator{$ENDIF});
var
  I, X,Y, t: Integer;
begin
  X := 0;
  Y := 1;
  I := 0;
  YieldObj.Yield(X);
  While I < 20 do
  begin
    YieldObj.Yield(Y);
    //writeln('X=',X);
    t := X;
    X := Y;
    //writeln('X=',X);
    Y := Y + t;
    Inc(I);
  end;
end;

procedure StringYieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoRoutineEnumerator {$ELSE}PMeCoRoutineEnumerator{$ENDIF});
var  
  YieldValue: string;
  i: integer;
begin
  YieldValue:='None';
  YieldObj.Yield(YieldValue);//返回行为逻辑
  try
    //writeln(3/0);
    raise Exception.Create(rsTestStringException);
  except
    On E:Exception do 
    begin
      YieldValue := E.Message;
      //YieldObj.Yield(YieldValue); //<-- DO NOT USE the Yield in here: Exception block or Finally block.
      //writeln('Exception:', E.Message);
      //raise;
    end;
  end;
  YieldObj.Yield(YieldValue);

  YieldValue := 'None';
  for i := 1 to 30 do
  begin
    YieldValue := YieldValue + IntToStr(i);
    YieldObj.Yield(YieldValue); //返回行为逻辑
  end;
end;

{ TTest_CoRoutine }
procedure TTest_CoRoutine.Setup;
begin
  inherited;
  FCoRoutine := CreateObject;
end;

procedure TTest_CoRoutine.TearDown;
begin
  if Assigned(FCoRoutine) then
  begin
    FCoRoutine.Free;
    FCoRoutine := nil;
  end;
end;

{ TTest_CoRoutineYieldString }
function TTest_CoRoutineYieldString.CreateObject: {$IFDEF YieldClass_Supports}TMeCoRoutineEnumerator {$ELSE}PMeCoRoutineEnumerator{$ENDIF};
begin
  Result := {$IFDEF YieldClass_Supports}TYieldString.Create(StringYieldProc) {$ELSE}New(PYieldString, Create(StringYieldProc)){$ENDIF};
end;

procedure TTest_CoRoutineYieldString.Test_Yield;
var
  i: integer;
  s: string;
  vRaised: boolean;
begin
  i:= 0;
  s := 'None';
  vRaised := false;
    with {$IFDEF YieldClass_Supports}TYieldString{$ELSE}PYieldString{$ENDIF}(FCoRoutine){$IFNDEF YieldClass_Supports}^{$endif} do
      while MoveNext do
      begin
        if (i = 1) and not vRaised then 
        begin
          vRaised := true;
          CheckEquals(rsTestStringException, Current,  IntToStr(i)+ ' except raised string error.');
          continue;
        end
        else begin
          CheckEquals(s, Current, IntToStr(i)+ ' yield string error.');
        end;
        inc(i);
        s := s + IntToStr(i);
      end;
end;

{ TTest_CoRoutineYieldInteger }
function TTest_CoRoutineYieldInteger.CreateObject: {$IFDEF YieldClass_Supports}TMeCoRoutineEnumerator {$ELSE}PMeCoRoutineEnumerator{$ENDIF};
begin
  Result := {$IFDEF YieldClass_Supports}TYieldInteger.Create(StringYieldProc) {$ELSE}New(PYieldInteger, Create(IntegerYieldProc)){$ENDIF};
end;

const
  cFibonacc : array[0..20] of integer = (0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765);

procedure TTest_CoRoutineYieldInteger.Test_Yield;
var
  i: integer;
  vRaised: boolean;
begin
  i:= 0;
  vRaised := false;
    with {$IFDEF YieldClass_Supports}TYieldInteger{$ELSE}PYieldInteger{$ENDIF}(FCoRoutine){$IFNDEF YieldClass_Supports}^{$endif} do
      while MoveNext do
      begin
        CheckEquals(cFibonacc[i], Current,  IntToStr(i)+ ' Fibonacc error.');
        inc(i);
      end;
end;

{ TYieldString }
{$IFNDEF YieldClass_Supports}
destructor TYieldString.Destroy;
begin
  FValue := '';
  inherited;
end;
{$ENDIF}

function TYieldString.GetCurrent: string;
begin
  Result := FValue;
end;

procedure TYieldString.SetNextValue(const aValue);
begin
  Self.FValue := string(aValue);
end;

{ TYieldInteger }
function TYieldInteger.GetCurrent: Integer;
begin
  Result := FValue;
end;

procedure TYieldInteger.SetNextValue(const aValue);
begin
  Self.FValue := Integer(aValue);
end;

Initialization

  RegisterTests('MeCoRoutine suites',
                [
                 TTest_CoRoutineYieldString.Suite
                 , TTest_CoRoutineYieldInteger.Suite
                 //, TTest_EnumerationType.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
