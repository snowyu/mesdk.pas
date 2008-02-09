library dlltest;

{$I MeSetting.inc}

uses
    {$IFNDEF COMPILER10_UP}
      {$IFNDEF CLR}
      FastMM4,
      {$ENDIF}
    {$ENDIF}
  Windows
  ;

procedure ShowIt;register;
begin
asm
//the patched method must be greater than 5 bytes.
  Nop
  NOP
  NOP
  NOP
end;

  MessageBox(0, 'Hello From ShowIt!', 'ShowIt', MB_OK);
end;

exports
  ShowIt;

begin
end.
