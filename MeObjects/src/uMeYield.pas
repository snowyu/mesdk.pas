
{Summary: MeYield - implements the equivalent of the C# yield statement. }
(*
 * The contents of this file are released under a dual license, and
 * you may choose to use it under either the Mozilla Public License 
 * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html) 
 * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
 * http://www.opensource.org/licenses/lgpl-license.php).
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is $RCSfile: uMeYield.pas,v $.
 *
 * The Initial Developers of the Original Code are Sergey Antonov.
 * Portions created by Sergey Antonov<santonov.blogspot.com> is Copyright (C) 2007
 * Portions created by Riceball LEE<riceballl@hotmail.com> is Copyright (C) 2007
 * All rights reserved.
 *
 * Contributor(s):
 *  Sergey Antonov
 *  Riceball LEE (port to MeObject and little optimal)
 *
 * History
 *   Riceball LEE
 *     + little optimal
 *     + Continuation supports

 Usage:

Continuation Usage[this just a demo, but it's ugly coding]:

var
  vContinuationRec: TMeContinuationRec;

procedure YieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
var
  i: integer;
begin
    i := 0;
    writeln('white move: ', i);
    inc(i);
    YieldObj.MarkContinuation(vContinuationRec); //<-- remember this position
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
    finally
      Free;
    end;
  Writeln('---END');
end.

CoRoutine Usage:
//枚举器
procedure StringCoRoutineProc(const YieldObj: TaCoRoutine);
var  
  YieldValue: string;
  i: integer;
begin
  YieldValue:='None';
  YieldObj.Yield(YieldValue); //返回行为逻辑控制
  for i := 1 to 10 do
  begin
    YieldValue := YieldValue + IntToStr(i);
    YieldObj.Yield(YieldValue); //返回行为逻辑控制
  end;
end;

function TForm1.GetEnumerator: TYieldString;
begin
  Result:=TYieldString.Create(StringCoRoutineProc);
end;

procedure TForm1.Button1Click(Sender: TObject);
var 
  s:string;
begin
  for s in self do 
    Memo1.Lines.Add(s);
  {It means
    with GetEnumerator do
    try
      while Resume do //转到控制逻辑
      begin
        s := Current;
        Memo1.Lines.Add(s);
      end;
    finally
      Free;
    end;
  }
end;

 *)
unit uMeYield;

interface

{$I MeSetting.inc}
{.$Define YieldClass_Supports} //use the delphi class type instead.

uses
  {$IFNDEF YieldClass_Supports}
  uMeObject,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows
  {$ENDIF MSWINDOWS}
  ;

type
  {$IFDEF YieldClass_Supports}
  TMeCoRoutine = Class;
  {$ELSE}
  PMeCoRoutine = ^TMeCoRoutine;

  PMeYieldObject = ^TMeYieldObject;
  PYieldString = ^TYieldString;
  PYieldInteger = ^TYieldInteger;
  {$ENDIF}

  TMeCoRoutineProc = procedure (const aCoRoutine: {$IFDEF YieldClass_Supports}TMeCoRoutine{$ELSE} PMeCoRoutine{$endif});
  TMeCoRoutineMethod = procedure () of object;
  TMeCoRoutineStatus = (coSuspended, coRunning, coDead);

  TPreservedRegisters = packed record
    FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    FESP:pointer;
  end;
  //the control state of the rest of the computation, meaning the data structures and code needed to complete a computation. 
  //当前函数执行的位置现场
  //实现这个难点在于在函数中可能有动态分配内容，如字符串，当退出该函数后自然被清除，这时候从函数的中间进入就会出问题！
  TMeContinuationRec = packed record
    Registers: TPreservedRegisters;
    StackFrameSize:DWORD;
    StackFrame: array[1..128] of DWORD;
    NextIP:Pointer;
  end;

  TMeCoRoutine = {$IFDEF YieldClass_Supports}class{$ELSE}object(TMeDynamicObject){$ENDIF}
  protected
    FIsYield: Boolean;
    FStatus: TMeCoRoutineStatus;
    FNextIP:Pointer;
    FProc: TMeCoRoutineProc;
    FRegisters: TPreservedRegisters;
    //FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    //FESP:pointer;
    FStackFrameSize:DWORD;
    FStackFrame: array[1..128] of DWORD;
    procedure SaveYieldedValue(const aValue); virtual;
    //procedure iCallCC;
  public
    constructor Create(const CoRoutineProc: TMeCoRoutineProc);
    function Resume:boolean;
    function Reset:boolean;
    procedure Yield(const Value);
    {WARNING: MUST NOT USE the local string etc dynamic local variable after Mark postion!!
      Do not Mark the Continuation in the loop.
    }
    procedure MarkContinuation(var aContinuationRec: TMeContinuationRec);
    //Call with Current  Continuation
    function CallCC(const aContinuationRec: TMeContinuationRec): Boolean;
    function RestoreContinuation(const aContinuationRec: TMeContinuationRec): Boolean;

    property Status: TMeCoRoutineStatus read FStatus;
  end;

  TMeYieldObject = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCoRoutine)
  public
    function MoveNext:boolean; //D2007 enumerable required
  end;

  TYieldString = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeYieldObject)
  protected
    FValue:String;
    function GetCurrent:string;
    procedure SaveYieldedValue(const aValue); {$IFDEF YieldClass_Supports}override{$ELSE} virtual{$endif}; 
  public
    {$IFNDEF YieldClass_Supports}
    destructor Destroy; virtual; {override}
    {$ENDIF}

    property Current:string read GetCurrent; //D2007 enumerable required
  end;

  TYieldInteger = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeYieldObject)
  protected
    FValue: Integer;
    function GetCurrent: Integer;
    procedure SaveYieldedValue(const aValue); {$IFDEF YieldClass_Supports}override{$ELSE} virtual{$endif}; 
  public

    //return the Current value
    property Current:Integer read GetCurrent; //D2007 enumerable required
  end;

implementation

{ TMeCoRoutine }
constructor TMeCoRoutine.Create(const CoRoutineProc:TMeCoRoutineProc);
asm
  {$IFNDEF YieldClass_Supports}
  CALL TMeDynamicObject.Init
  {$ENDIF}
  mov eax.TMeCoRoutine.FNextIP,ecx;
  mov eax.TMeCoRoutine.FProc, ecx;
  mov eax.TMeCoRoutine.FRegisters.FEAX,EAX;
end;

function TMeCoRoutine.Reset:boolean;
begin
  Result := (FStatus = coDead) and Assigned(FProc);
  if Result then
  begin
    FNextIP := @FProc;
    FStatus := coSuspended;
    with FRegisters do
    begin
      FEAX:= {$IFNDEF YieldClass_Supports}@{$ENDIF}Self;
      FEBX:= nil;
      FECX:= nil;
      FEDX:= nil;
      FESI:= nil;
      FEDI:= nil;
      FEBP:= nil;
      FESP:= nil;
    end;
    FStackFrameSize := 0;
  end;
end;

procedure TMeCoRoutine.SaveYieldedValue(const aValue);
begin
end;

function TMeCoRoutine.Resume: boolean;
asm
  MOV EAX.TMeCoRoutine.FIsYield, 0

  CMP  EAX.TMeCoRoutine.FStatus, coSuspended
  JNE  @@CheckExit


@@DoResume:
  MOV EAX.TMeCoRoutine.FStatus, coRunning
  { Save the value of following registers.
    We must preserve EBP, EBX, EDI, ESI, EAX for some circumstances.
    Because there is no guarantee that the state of registers will 
    be the same after an iteration }
  push ebp;
  push ebx;
  push edi;
  push esi;
  push eax;

  push offset @@exit
  xor edx,edx;
  cmp eax.TMeCoRoutine.FRegisters.FESP,edx;
  jz @AfterEBPAdjust;

  { Here is the correction of EBP. Some need of optimization still exists. }
  mov edx,esp;
  sub edx,eax.TMeCoRoutine.FRegisters.FESP;
  add [eax.TMeCoRoutine.FRegisters.FEBP],edx

  @AfterEBPAdjust:
  mov eax.TMeCoRoutine.FRegisters.FESP,esp;

  { Is there any local frame? }
  cmp eax.TMeCoRoutine.FStackFrameSize,0
  jz @JumpIn;

  { Restore the local stack frame }
  mov ecx,eax.TMeCoRoutine.FStackFrameSize;
  sub esp,ecx;
  mov edi,esp;
  lea esi,eax.TMeCoRoutine.FStackFrame;

  shr ecx, 2
  rep movsd;
  @JumpIn:

  { Restore the content of processor registers }
  mov ebx,eax.TMeCoRoutine.FRegisters.FEBX;
  mov ecx,eax.TMeCoRoutine.FRegisters.FECX;
  mov edx,eax.TMeCoRoutine.FRegisters.FEDX;
  mov esi,eax.TMeCoRoutine.FRegisters.FESI;
  mov edi,eax.TMeCoRoutine.FRegisters.FEDI;
  mov ebp,eax.TMeCoRoutine.FRegisters.FEBP;
  push [eax.TMeCoRoutine.FNextIP];
  mov eax,eax.TMeCoRoutine.FRegisters.FEAX;

  { Here is the jump to next iteration }
  RET;

  { And we return here after next iteration in all cases, except exception of course. }
  @@exit:;

  { Restore the preserved EBP, EBX, EDI, ESI, EAX registers }
  pop eax;
  pop esi;
  pop edi;
  pop ebx;
  pop ebp;


@@CheckExit:
  CMP  EAX.TMeCoRoutine.FIsYield, 0
  JNE  @@skip
  MOV  EAX.TMeCoRoutine.FStatus, coDead
  MOV  EAX.TMeCoRoutine.FNextIP, 0

@@skip:
  MOV  AL, EAX.TMeCoRoutine.FIsYield
end;

(*
procedure TMeCoRoutine.iCallCC;
asm
  MOV EAX.TMeCoRoutine.FIsYield, 0

  CMP  EAX.TMeCoRoutine.FStatus, coSuspended
  JNE  @@CheckExit


@@DoResume:
  MOV EAX.TMeCoRoutine.FStatus, coRunning
  { Save the value of following registers.
    We must preserve EBP, EBX, EDI, ESI, EAX for some circumstances.
    Because there is no guarantee that the state of registers will 
    be the same after an iteration }
  push ebp;
  push ebx;
  push edi;
  push esi;
  push eax;

  push offset @@exit

  @AfterEBPAdjust:
  mov eax.TMeCoRoutine.FRegisters.FESP,esp;

  { Is there any local frame? }
  cmp eax.TMeCoRoutine.FStackFrameSize,0
  jz @JumpIn;

  { Restore the local stack frame }
  mov ecx,eax.TMeCoRoutine.FStackFrameSize;
  sub esp,ecx;
  mov edi,esp;
  lea esi,eax.TMeCoRoutine.FStackFrame;

  shr ecx, 2
  rep movsd;
  @JumpIn:

  { Restore the content of processor registers }
  mov ebx,eax.TMeCoRoutine.FRegisters.FEBX;
  mov ecx,eax.TMeCoRoutine.FRegisters.FECX;
  mov edx,eax.TMeCoRoutine.FRegisters.FEDX;
  mov esi,eax.TMeCoRoutine.FRegisters.FESI;
  mov edi,eax.TMeCoRoutine.FRegisters.FEDI;
  //mov ebp,eax.TMeCoRoutine.FRegisters.FEBP;
  push [eax.TMeCoRoutine.FNextIP];
  mov eax,eax.TMeCoRoutine.FRegisters.FEAX;

  { Here is the jump to next iteration }
  RET;

  { And we return here after next iteration in all cases, except exception of course. }
  @@exit:;

  { Restore the preserved EBP, EBX, EDI, ESI, EAX registers }
  pop eax;
  pop esi;
  pop edi;
  pop ebx;
  pop ebp;


@@CheckExit:
  CMP  EAX.TMeCoRoutine.FIsYield, 0
  JNE  @@skip
  MOV  EAX.TMeCoRoutine.FStatus, coDead
  MOV  EAX.TMeCoRoutine.FNextIP, 0

@@skip:
  MOV  AL, EAX.TMeCoRoutine.FIsYield
end;
*)

function TMeCoRoutine.RestoreContinuation(const aContinuationRec: TMeContinuationRec): Boolean;
var
  p: Pointer;
begin
  Result := Assigned(aContinuationRec.NextIP) and ((FStatus = coDead) or (not FIsYield and (FStatus = coSuspended))) ;
  if Result then
  begin
    P := FRegisters.FESP;
    FNextIP := aContinuationRec.NextIP;
    FRegisters := aContinuationRec.Registers;
    FRegisters.FESP := nil;
    FStatus := coSuspended;
    FStackFrameSize := aContinuationRec.StackFrameSize;
    Move(aContinuationRec.StackFrame, FStackFrame, FStackFrameSize);
  end;
end;

function TMeCoRoutine.CallCC(const aContinuationRec: TMeContinuationRec): boolean;
begin
  Result := RestoreContinuation(aContinuationRec);
  if Result then
    Result := Resume;
end;

procedure TMeCoRoutine.MarkContinuation(var aContinuationRec: TMeContinuationRec);
asm
  CMP  EAX.TMeCoRoutine.FStatus, coRunning
  JNE  @@Exit

  { Preserve EBP, EAX,EBX,ECX,EDX,ESI,EDI }
  MOV EDX.TMeContinuationRec.Registers.FEBP,EBP;
  MOV EDX.TMeContinuationRec.Registers.FEAX,EAX;
  MOV EDX.TMeContinuationRec.Registers.FEBX,EBX;
  MOV EDX.TMeContinuationRec.Registers.FECX,ECX;
  MOV EDX.TMeContinuationRec.Registers.FEDX,EDX;   // This is the Ref to aContinuationRec param
  MOV EDX.TMeContinuationRec.Registers.FESI,ESI;
  MOV EDX.TMeContinuationRec.Registers.FEDI,EDI;
  //MOV EDX.TMeContinuationRec.Registers.FESP,ESP;
  MOV ECX, [ESP]
  MOV EDX.TMeContinuationRec.NextIP, ECX; //store the next execution address

  
  { Calculate the current local stack frame size }
  mov ecx,eax.TMeCoRoutine.FRegisters.FESP;
  sub ecx,esp;
  //Sub ecx, 4
  DEC ECX
  DEC ECX
  DEC ECX
  DEC ECX
  mov edx.TMeContinuationRec.StackFrameSize,ecx;
  jz @AfterSaveStack;

  { Preserve the local stack frame }
  //LEA ESI, [ESP+4]
  MOV ESI,ESP
  INC ESI
  INC ESI
  INC ESI
  INC ESI
  LEA EDI,[EDX.TMeContinuationRec.StackFrame];

  SHR ECX, 2
  REP MOVSD;

  MOV ESI, EDX.TMeContinuationRec.Registers.FESI
  MOV EDI, EDX.TMeContinuationRec.Registers.FEDI
  //mov esp,eax.TMeCoRoutine.FRegisters.FESP;
  @AfterSaveStack:

@@Exit:
end;

procedure TMeCoRoutine.Yield(const Value);
asm
  CMP  EAX.TMeCoRoutine.FStatus, coRunning
  JNE  @@Exit

  { Preserve EBP, EAX,EBX,ECX,EDX,ESI,EDI }
  mov eax.TMeCoRoutine.FRegisters.FEBP,ebp;
  mov eax.TMeCoRoutine.FRegisters.FEAX,eax;
  mov eax.TMeCoRoutine.FRegisters.FEBX,ebx;
  mov eax.TMeCoRoutine.FRegisters.FECX,ecx;
  mov eax.TMeCoRoutine.FRegisters.FEDX,edx;   // This is the Ref to const param
  mov eax.TMeCoRoutine.FRegisters.FESI,ESI;
  mov eax.TMeCoRoutine.FRegisters.FEDI,EDI;
  pop ecx;
  mov eax.TMeCoRoutine.FNextIP,ecx; //store the next execution address

  //We must do it first for valid const reference
  push eax;
  mov ecx,[eax];
  CALL  DWORD PTR [ecx+VMTOFFSET TMeCoRoutine.SaveYieldedValue];
  pop eax;
  
  { Calculate the current local stack frame size }
  mov ecx,eax.TMeCoRoutine.FRegisters.FESP;
  sub ecx,esp;
  mov eax.TMeCoRoutine.FStackFrameSize,ecx;
  jz @AfterSaveStack;

  { Preserve the local stack frame }
  lea esi,[esp];
  lea edi,[eax.TMeCoRoutine.FStackFrame];
  
  shr ecx, 2
  rep movsd;
  mov esp,eax.TMeCoRoutine.FRegisters.FESP;
  @AfterSaveStack:

  {Set flag of Yield occurance }
  MOV EAX.TMeCoRoutine.FIsYield,1;
  MOV EAX.TMeCoRoutine.FStatus, coSuspended;
@@Exit:
end;

{ TMeYieldObject }
function TMeYieldObject.MoveNext:boolean;
begin
  Result := Resume();
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

procedure TYieldString.SaveYieldedValue(const aValue);
begin
  Self.FValue := string(aValue);
end;

{ TYieldInteger }
function TYieldInteger.GetCurrent: Integer;
begin
  Result := FValue;
end;

procedure TYieldInteger.SaveYieldedValue(const aValue);
begin
  Self.FValue := Integer(aValue);
end;

initialization
end.
