unit TestCorout;

interface

uses
  SysUtils, ScCoroutines;

type
  TCounter = class(TCoroutine)
  protected
    procedure Execute; override;
  end;

  TEnumCounter = class(TCoroutineEnumerator)
  protected
    FCurrent: Integer;

    procedure Execute; override;
    procedure SetNextValue(const Value); override;
  public
    function GetCurrent: Integer;

    property Current: Integer read GetCurrent;
  end;

  TTest = class
  public
    function GetEnumerator: TEnumCounter;
  end;

procedure TestCoroutines;

implementation

{ TCounter }

procedure TCounter.Execute;
var
  I: Integer;
begin
  for I := 0 to 4 do
  begin
    WriteLn(I);
    try
    raise Exception.Create('hi');
    Except
    on E: Exception do
      WriteLn('E:', E.Message);
    end;
    Yield;
    if Terminating then
      Exit;
  end;

  //raise Exception.Create('hi');
  WriteLn('-done-');
end;

{ TEnumCounter }

{procedure StackOverflow(Iter: Integer = 0);
var
  BigThing: array[0..1024-1] of Byte;
begin
  BigThing[0] := 0;
  WriteLn(BigThing[0], ' Stack overflow ', Iter);
  StackOverflow(Iter+1);
end;}

procedure TEnumCounter.Execute;
var
  I: Integer;
begin
  for I := 0 to 4 do
  begin
    Yield(I);
    if Terminating then
      Exit;
  end;

  //raise Exception.Create('hi');
  //StackOverflow;
  WriteLn('-done-');
end;

procedure TEnumCounter.SetNextValue(const Value);
begin
  FCurrent := Integer(Value);
end;

function TEnumCounter.GetCurrent: Integer;
begin
  Result := FCurrent;
end;

{ TTest }

function TTest.GetEnumerator: TEnumCounter;
begin
  Result := TEnumCounter.Create(clNoLoop);
end;

{ Test }

procedure TestCoroutines;
var
  Test: TTest;
  I: Integer;
  Answer: string;
begin
  Test := TTest.Create;
  try
    for I in Test do
      WriteLn(I);
  finally
    Test.Free;
  end;

  with TCounter.Create(clImmediate) do
  try
    repeat
      ReadLn(Answer);
      if Answer = 'y' then
        Invoke;
    until (Answer <> 'y') or Terminated;
  finally
    Free;
  end;
end;

end.

