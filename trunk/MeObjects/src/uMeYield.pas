
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
 *     + TMeCustomCoRoutine
 *     + TMeCustomCoRoutine.Continuation supports
 *        Note: Continuations are the functional expression of the GOTO statement
 *              Re-invocable continuations must be simple enough and no local memory allocation in it, No Unwind SEH supports..

 Usage:


Continuation Usage[this just a demo, but it's ugly coding]:

var
  vContinuationRec: TMeContinuationRec;

procedure YieldProc(const YieldObj: {$IFDEF YieldClass_Supports}TMeCustomCoRoutine{$ELSE} PMeCoroutine{$endif});
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
{_$Define MarkContinuation_Supports}

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
  TMeCustomCoRoutine = Class;
  {$ELSE}
  PMeCoRoutine = ^TMeCustomCoRoutine;

  PMeYieldObject = ^TMeCoRoutineEnumerator;
  PYieldString = ^TYieldString;
  PYieldInteger = ^TYieldInteger;
  {$ENDIF}

  TMeCoRoutineProc = procedure (const aCoRoutine: {$IFDEF YieldClass_Supports}TMeCustomCoRoutine{$ELSE} PMeCoRoutine{$endif});
  TMeCoRoutineMethod = procedure () of object;
  TMeCoRoutineState = (coSuspended, coRunning, coDead);

  TPreservedRegisters = packed record
    FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    FESP:pointer;
  end;
  //the control state of the rest of the computation, meaning the data structures and code needed to complete a computation. 
  //当前函数执行的位置现场
  //不能用于在函数中可能有动态分配内容，如字符串，当退出该函数后自然被清除，这时候从函数的中间进入就会出问题！
  TMeContinuationRec = packed record
    Registers: TPreservedRegisters;
    StackFrameSize:DWORD;
    StackFrame: array[1..128] of DWORD;
    NextIP:Pointer;
    {_$IFNDEF MarkContinuation_Supports}
    InnerSEHCount:DWORD;
    InnerSEHOffsets:array[0..$F] of DWORD;
    {_$ENDIF}
  end;

  TMeCustomCoRoutine = {$IFDEF YieldClass_Supports}class{$ELSE}object(TMeDynamicObject){$ENDIF}
  protected
    FIsYield: Boolean; //when call yield break.
    FState: TMeCoRoutineState;
    FNextIP:Pointer;
    FProc: TMeCoRoutineProc;
    FRegisters: TPreservedRegisters;
    //FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    //FESP:pointer;
    FStackFrameSize:DWORD;
    FStackFrame: array[1..128] of DWORD;

    {_$IFNDEF MarkContinuation_Supports}
    InnerSEHCount:DWORD;
    InnerSEHOffsets:array[0..$F] of DWORD;
    {_$ENDIF}

    procedure SetNextValue(const aValue); virtual;

  public
    constructor Create(const CoRoutineProc: TMeCoRoutineProc);
    function Resume:boolean;
    function Reset:boolean;
    procedure Yield(const Value);
    {_$IFDEF MarkContinuation_Supports}
    {WARNING: MUST NOT USE the local string etc dynamic local variable after Mark postion!!
      Do not Mark the Continuation in the loop. no unwind SEH supports
    }
    procedure MarkContinuation(var aContinuationRec: TMeContinuationRec);
    //Call with Current  Continuation
    function CallCC(const aContinuationRec: TMeContinuationRec): Boolean;
    function RestoreContinuation(const aContinuationRec: TMeContinuationRec): Boolean;
    {_$ENDIF}

    property State: TMeCoRoutineState read FState;
  end;

  { the abstract enumerator running in a CoRoutine
    In order to obtain a concrete enumerator, you must override the SetNextValue 
    methods, and define a Current property. The Execute method can call Yield many 
    times with any value as a parameter. The SetNextValue must store this value, 
    and the Current property should read it.
  }
  TMeCoRoutineEnumerator = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCustomCoRoutine)
  public
    function MoveNext:boolean; //D2007 enumerable required
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

{ TMeCustomCoRoutine }
constructor TMeCustomCoRoutine.Create(const CoRoutineProc:TMeCoRoutineProc);
asm
  {$IFNDEF YieldClass_Supports}
  CALL TMeDynamicObject.Init
  {$ENDIF}
  mov eax.TMeCustomCoRoutine.FNextIP,ecx;
  mov eax.TMeCustomCoRoutine.FProc, ecx;
  mov eax.TMeCustomCoRoutine.FRegisters.FEAX,EAX;
end;

function TMeCustomCoRoutine.Reset:boolean;
begin
  Result := (FState = coDead) and Assigned(FProc);
  if Result then
  begin
    FNextIP := @FProc;
    FState := coSuspended;
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

procedure TMeCustomCoRoutine.SetNextValue(const aValue);
begin
end;

function TMeCustomCoRoutine.Resume: boolean;
asm
  MOV EAX.TMeCustomCoRoutine.FIsYield, 0

  CMP  EAX.TMeCustomCoRoutine.FState, coSuspended
  JNE  @@CheckExit


@@DoResume:
  MOV EAX.TMeCustomCoRoutine.FState, coRunning
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
  XOR EDX,EDX;
  {Is it the first call?}
  MOV ECX, EAX.TMeCustomCoRoutine.FRegisters.FESP
  CMP ECX,EDX
  JNZ @NotFirstCall

  MOV EAX.TMeCustomCoRoutine.FRegisters.FESP,ESP;
  //JMP @JustBeforeTheJump;

@NotFirstCall:
  CMP EAX.TMeCustomCoRoutine.FStackFrameSize,EDX
  JZ @RestoreRegisters;
  {Is need any correction?}

  MOV EDX,ESP;
  SUB EDX,ECX;
  JZ @RestoreStackFrame;
  {Correct ebp}
  add [eax.TMeCustomCoRoutine.FRegisters.FEBP],edx

{_$IFNDEF MarkContinuation_Supports}
  {Is any SEH frames}
  mov ecx,eax.TMeCustomCoRoutine.InnerSEHCount;
  jecxz @ChangeFESP;
  {correct SEH frames}
  mov ebx,eax.TMeCustomCoRoutine.FRegisters.FEBP;
  lea esi,eax.TMeCustomCoRoutine.FStackFrame;
  add esi,eax.TMeCustomCoRoutine.FStackFrameSize;
  dec ecx;
  mov edi,esi;
  sub edi,DWORD PTR eax.TMeCustomCoRoutine.InnerSEHOffsets+4*ecx;
  mov [edi+$08],ebx;
@SEHCorrection:
  dec ecx;
  jl @ChangeFESP
  mov edi,esi;
  sub edi,DWORD PTR eax.TMeCustomCoRoutine.InnerSEHOffsets+4*ecx;
  mov [edi+$08],ebx;
  add [edi],edx;
  jmp @SEHCorrection;
  {Change BESP}
@ChangeFESP:
  MOV EAX.TMeCustomCoRoutine.FRegisters.FESP,ESP;
{_$ENDIF}

@RestoreStackFrame:
  { Restore the local stack frame }
  mov ecx,eax.TMeCustomCoRoutine.FStackFrameSize;
  sub esp,ecx;
  mov edi,esp;
  lea esi,eax.TMeCustomCoRoutine.FStackFrame;
  shr ecx, 2
  rep movsd;

{_$IFNDEF MarkContinuation_Supports}
  {Connect Inner SEH frame. Are any inner SEH?}
  mov ecx,eax.TMeCustomCoRoutine.InnerSEHCount;
  jecxz @RestoreRegisters;

  { Connect Inner SEH frame }
  xor ecx,ecx;
  mov edi,eax.TMeCustomCoRoutine.FRegisters.FESP;
  sub edi,DWORD PTR eax.TMeCustomCoRoutine.InnerSEHOffsets+4*ecx;
  mov fs:[ecx],edi;
{_$ENDIF}

@RestoreRegisters:

  { Restore the content of processor registers }
  mov ebx,eax.TMeCustomCoRoutine.FRegisters.FEBX;
  mov ecx,eax.TMeCustomCoRoutine.FRegisters.FECX;
  mov edx,eax.TMeCustomCoRoutine.FRegisters.FEDX;
  mov esi,eax.TMeCustomCoRoutine.FRegisters.FESI;
  mov edi,eax.TMeCustomCoRoutine.FRegisters.FEDI;
  mov ebp,eax.TMeCustomCoRoutine.FRegisters.FEBP;

@JustBeforeTheJump:
  push [eax.TMeCustomCoRoutine.FNextIP];
  mov eax,eax.TMeCustomCoRoutine.FRegisters.FEAX;

  { Here is the jump to next iteration }
  RET;

  { And we return here after next iteration in all cases, except exception of course. }
@@exit:

  { Restore the preserved EBP, EBX, EDI, ESI, EAX registers }
  pop eax;
  pop esi;
  pop edi;
  pop ebx;
  pop ebp;


@@CheckExit:
  CMP  EAX.TMeCustomCoRoutine.FIsYield, 0
  JNE  @@skip
  MOV  EAX.TMeCustomCoRoutine.FState, coDead
  MOV  EAX.TMeCustomCoRoutine.FNextIP, 0

@@skip:
  MOV  AL, EAX.TMeCustomCoRoutine.FIsYield
end;

{_$IFDEF MarkContinuation_Supports}
function TMeCustomCoRoutine.RestoreContinuation(const aContinuationRec: TMeContinuationRec): Boolean;
var
  p: Pointer;
begin
  Result := Assigned(aContinuationRec.NextIP) and ((FState = coDead) or (not FIsYield and (FState = coSuspended))) ;
  if Result then
  begin
    P := FRegisters.FESP;
    FNextIP := aContinuationRec.NextIP;
    FRegisters := aContinuationRec.Registers;
    FRegisters.FESP := nil;
    FState := coSuspended;
    FStackFrameSize := aContinuationRec.StackFrameSize;
    Move(aContinuationRec.StackFrame, FStackFrame, FStackFrameSize);
    //InnerSEHCount := aContinuationRec.InnerSEHCount;
    //Move(aContinuationRec.InnerSEHOffsets, InnerSEHOffsets, SizeOf(InnerSEHOffsets));
  end;
end;

function TMeCustomCoRoutine.CallCC(const aContinuationRec: TMeContinuationRec): boolean;
begin
  Result := RestoreContinuation(aContinuationRec);
  if Result then
    Result := Resume;
end;

procedure TMeCustomCoRoutine.MarkContinuation(var aContinuationRec: TMeContinuationRec);
asm
  CMP  EAX.TMeCustomCoRoutine.FState, coRunning
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
  mov ecx,eax.TMeCustomCoRoutine.FRegisters.FESP;
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
  //mov esp,eax.TMeCustomCoRoutine.FRegisters.FESP;
  @AfterSaveStack:

@@Exit:
end;
{_$ENDIF}

procedure TMeCustomCoRoutine.Yield(const Value);
asm
  CMP  EAX.TMeCustomCoRoutine.FState, coRunning
  JNE  @@Exit

  { Preserve EBP, EAX,EBX,ECX,EDX,ESI,EDI }
  mov eax.TMeCustomCoRoutine.FRegisters.FEBP,ebp;
  mov eax.TMeCustomCoRoutine.FRegisters.FEAX,eax;
  mov eax.TMeCustomCoRoutine.FRegisters.FEBX,ebx;
  mov eax.TMeCustomCoRoutine.FRegisters.FECX,ecx;
  mov eax.TMeCustomCoRoutine.FRegisters.FEDX,edx;   // This is the Ref to const param
  mov eax.TMeCustomCoRoutine.FRegisters.FESI,ESI;
  mov eax.TMeCustomCoRoutine.FRegisters.FEDI,EDI;
  pop ecx;
  mov eax.TMeCustomCoRoutine.FNextIP,ecx; //store the next execution address

  //We must do it first for valid const reference
  push eax;
  mov ecx,[eax];
  CALL  DWORD PTR [ecx+VMTOFFSET TMeCustomCoRoutine.SetNextValue];
  pop eax;
  
{_$IFNDEF MarkContinuation_Supports}
  { Unwind SEH }
  xor ebx,ebx;
  mov ecx,fs:[ebx];
  @SEHUnwind:
  jecxz @JustAfterSEHUnwind;
  cmp ecx,eax.TMeCustomCoRoutine.FRegisters.FESP;
  jnl @JustAfterSEHUnwind
  mov esi,eax.TMeCustomCoRoutine.FRegisters.FESP;
  sub esi,ecx;
  mov DWORD PTR eax.TMeCustomCoRoutine.InnerSEHOffsets+4*ebx,esi;
  inc ebx;
  mov ecx,[ecx];
  jmp @SEHUnwind;
  @JustAfterSEHUnwind:
  mov eax.TMeCustomCoRoutine.InnerSEHCount,ebx;
  {
  Connect Outer SEH frame.
  If no local SEH frames next two commands are redundant
  }
  xor ebx,ebx;
  mov fs:[ebx],ecx;
{_$ENDIF}

  {Save local stack frame}
  { Calculate the current local stack frame size }
  mov ecx,eax.TMeCustomCoRoutine.FRegisters.FESP;
  sub ecx,esp;
  mov eax.TMeCustomCoRoutine.FStackFrameSize,ecx;
  jz @AfterSaveStack;

  { Preserve the local stack frame }
  lea esi,[esp];
  lea edi,[eax.TMeCustomCoRoutine.FStackFrame];
  
  shr ecx, 2
  rep movsd;
  mov esp,eax.TMeCustomCoRoutine.FRegisters.FESP;
  @AfterSaveStack:

  {Set flag of Yield occurance }
  MOV EAX.TMeCustomCoRoutine.FIsYield,1;
  MOV EAX.TMeCustomCoRoutine.FState, coSuspended;
@@Exit:
end;

{ TMeCoRoutineEnumerator }
function TMeCoRoutineEnumerator.MoveNext:boolean;
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

initialization
end.
