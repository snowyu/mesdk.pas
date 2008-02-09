library dlltest;

var
  vResult: ShortString;

procedure TestDLLProc;
begin
  vResult := '@@Hello test return str@@!!';
end;

procedure ClearTestDLLProc;
begin
  vResult := '';
end;

function GetResultByDLL: PShortString;
begin
  Result := @vResult;
end;

function TestInt64: int64;register;
begin
  Result := $1122334455667788;
end;

exports
  GetResultByDLL,
  ClearTestDLLProc,
  TestDLLProc,
  TestInt64;

begin
end.
