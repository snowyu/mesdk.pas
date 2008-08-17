{## the VM instructions ## }
procedure iVMNext(const aGlobalFunction: PMeScriptGlobalFunction); forward;

procedure iVMNext(const aGlobalFunction: PMeScriptGlobalFunction);
var
  vInstruction: TMeVMInstruction; //the instruction.
  vProc: TMeVMInstructionProc;
begin
  with aGlobalFunction^ do
    {$IFDEF FPC}
    While (psRunning in TMeScriptProcessorStates(LongWord(States))) do
    {$ELSE Borland}
    While (psRunning in TMeScriptProcessorStates(States)) do
    {$ENDIF}
    begin
      vInstruction := PMeVMInstruction(aGlobalFunction._PC.Mem)^;
      Inc(aGlobalFunction._PC.Mem);
      vProc := GMeScriptCoreWords[vInstruction];
      if Assigned(vProc) then
      begin
        vProc(aGlobalFunction);
      end
      else begin
        //BadOpError
        //_iVMHalt(errBadInstruction);
        //break;
      end;
    end;
end;


procedure VMAssignment(const aGlobalFunction: PMeScriptGlobalFunction);
{代码体是在函数上的，如何处理？
  法1： 进入某函数的时候，压入原来的_Func和_PC,赋值给全局 _Func 和 _PC. 修改返回栈的内容为： _Func, _PC，退出函数则还原原来的。
}
begin
end;

procedure VMCall(const aGlobalFunction: PMeScriptGlobalFunction);
begin
end;

procedure InitMeScriptCoreWordList;
begin
end;

