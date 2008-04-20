
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.12 $

  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeSystem.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Andreas Hausladen is Copyright (C) 2005
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.
    * Contributor(s):
    *  Andreas Hausladen
}
unit uMeSystem;

{$I MeSetting.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils
  , uMeConsts
  , uMeDisAsmEngine
  ;

const 
  cPointerSize = SizeOf(Pointer);
  //Near Jump Integer(OFFSET)
  cX86JumpDirective = $E9;
  //Near Call Integer(OFFSET)
  cX86CallDirective = $E8;
  cX86IndirectJump  = $25FF;
  cX86IndirectCall  = $15FF;
  //the NOOP(NO OPeration) directive
  cX86NoOpDirective = $90;
  cX86NoOpDirective4Bytes = $90909090;
  //RET Near:
  cX86RetNearDirective = $C3;
  //Ret Word(Param)
  cX86RetNearParamDirective = $C2;
  // $CA, $CB: Ret Far
  // $CF: IRET
  // $7X: (X=$0-$F) Short-displacement jump on condition (Jb)

  cNearJMPDirectiveSize = 1 + SizeOf(Pointer); //Near JMP or Near Call: = 5
  {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
  cRedirectCodeRecSize = 32;
  {$ENDIF}

type
  EMeError  = class(Exception);

type
  PShortJumpIfDirective = ^TShortJumpIfDirective;
  TShortJumpIfDirective = packed record
    Code: Byte;
    Offset: Byte;
  end;
  PNearJumpIfDirective = ^TNearJumpIfDirective;
  TNearJumpIfDirective = packed record
    Code: Word;
    Offset: Integer;
  end;

type
  PTIB = ^TTIB;
  {: Thread Information Block }
  { FS:[0]}
  TTIB = packed record
    SEH: Pointer;
    StackTop: Pointer;
    StackBottom: Pointer;
  end;

  { Summary the Near Jump directive }
  PRedirectCodeRec = ^TRedirectCodeRec;
  {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
  TRedirectCodeRec = packed record
    case Byte of
      0:(Jump: Byte; Offset: Integer);
      {$IFDEF FPC}
      2:(CodeJump: Byte; CodeOffset: Integer);
      {$ENDIF}
      1: (Code: array [0..cRedirectCodeRecSize - 1] of byte);
  end;
  {$ELSE}
  TRedirectCodeRec = packed record
    // $E9: the Near Jump directive 
    // $E8: the Near CALL directive 
    // $25FF: the Indirect Jump directive
    // $15FF: the Indirect Call directive
    Jump: Byte;
    //the relative offset: Cardinal(NewPtr)- Cardinal(OldPtr)- SizeOf(TRedirectCodeRec);
    Offset: Integer;
  end;
  {$ENDIF}

  {$IFDEF FPC}
  Largeint = Int64;
  {$ELSE}
    {$IFNDEF Compiler6_UP}
  PPointer = ^Pointer;
    {$ENDIF}
  {$ENDIF}



{ Summary Write to memory in the code segment.}
{
NOTE: the Size must is n*SizeOf(Pointer)
}
procedure WriteMem(const Location, Buffer: Pointer; const Size: Cardinal);
{ Summary Read from memory in the code segment.}
procedure ReadMem(const Location, Buffer: Pointer; const Size: Cardinal);

function WriteProtectedMemory(BaseAddress, Buffer: Pointer;
  Size: Cardinal; out WrittenBytes: Cardinal): Boolean;
function ReadProtectedMemory(BaseAddress, Buffer: Pointer; Size: Cardinal;
  out ReadBytes: Cardinal): Boolean;

procedure WriteJumpBuffer(Location: Pointer; const Buffer: TRedirectCodeRec);
procedure ReadJumpBuffer(Location: Pointer; var Buffer: TRedirectCodeRec);
function PatchJumpDirective(PatchLocation: Pointer; JumpTarget: Pointer;
                  var PatchLocationBackup: TRedirectCodeRec): Boolean;
function PatchCallDirective(PatchLocation: Pointer; CallTarget: Pointer;
                  var PatchLocationBackup: TRedirectCodeRec): Boolean;
function PatchDirective(PatchLocation: Pointer; Target: Pointer;
                  var PatchLocationBackup: TRedirectCodeRec; aDirective: Byte): Boolean;
//restore the PatchLocationBackup on the PatchLocation
function UnPatchDirective(PatchLocation: Pointer;  const PatchLocationBackup: TRedirectCodeRec): Boolean;
function GetActualAddress(AnAddr: Pointer): Pointer;    
//whether this address is already patched.
function IsPatchedDirective(PatchLocation: Pointer; Target: Pointer; aDirective: Byte): Boolean;
function IsPatchedJump(PatchLocation: Pointer; Target: Pointer): Boolean;
function IsPatchedCall(PatchLocation: Pointer; Target: Pointer): Boolean;

{$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
//##Internal functions for STATIC_METHOD_THREADSAFE_SUPPORT:
{ Summary move the hole to the procedure entry. }
procedure MoveProcEntryToHole(const aProc: Pointer; const aPatchHoleAddr: Pointer);
//nil means not found, else return the address of the hole
function GetPatchHoleAddress(const aProc: Pointer): Pointer;
{$ENDIF}


//return the multiple of the int value to upper nearest "Base" multiple
function  MultipleIntUp(const aValue: Integer; aRoundBase: Integer = SizeOf(Integer)): Integer;

//round an int value to upper nearest "Base" multiplier
function  RoundIntUp(const aValue: Integer; aRoundBase: Integer = SizeOf(Integer)): Integer;

{ Summary RedirectCode Is Noop directives or not }
{
EAX: <- TRedirectCodeRec
EAX: -> Result
affect registers: EAX, ECX
}
function IsRedirectCodeNoop(const aRedirectCode: TRedirectCodeRec): Boolean;

{: exchanging values }
procedure Swap(var X, Y: Integer);
{: minimum of two integers }
function Min(X, Y: Integer): Integer;
{: maximum of two integers }
function Max(X, Y: Integer): Integer;
{: absolute value }
function Abs(X: Integer): Integer;
{: sign of X: if X < 0, -1 is returned, if > 0, then +1, otherwise 0. }
function Sgn(X: Integer): Integer;
{: square root }
function iSqrt(X: DWORD): Integer;
{: cubic root }
function iCbrt(X: DWORD): Integer;

function ToMethod(const aProc: Pointer; const aData: Pointer {$IFDEF SUPPORTS_DEFAULTPARAMS}= nil{$ENDIF}): TMethod;

{ Summary:  Reset a clean running state for Delphi code, and find the TIB

  @return TIB linear address
}
function CleanUpAndGetTIB: PTIB;

implementation

uses
  RTLConsts;

{$IFDEF MSWINDOWS}
var
  FCurrentProcess: Cardinal;
{$ENDIF}

function CleanUpAndGetTIB: PTIB;
const
  TIBSelfPointer = $18;
asm
        // Clear Direction flag
        CLD

        // Reinitialize the FPU - see System._FpuInit
        FNINIT
        FWAIT
        //FLDCW means Set8087CW procedure
        FLDCW Default8087CW

        // Get TIB
        MOV     EAX,TIBSelfPointer
        MOV     EAX,FS:[EAX]
end;

function ToMethod(const aProc: Pointer; const aData: Pointer): TMethod;
begin
  with Result do
  begin
    Data := aData;
    Code := aProc;
  end;
end;

procedure Swap(var X, Y: Integer);
{$IFDEF FPC}
var Tmp: Integer;
begin
  Tmp := X;
  X := Y;
  Y := Tmp;
end;
{$ELSE DELPHI}
asm
  MOV  ECX, [EDX]
  XCHG ECX, [EAX]
  MOV  [EDX], ECX
end;
{$ENDIF FPC/DELPHI}

function Min(X, Y: Integer): Integer;
asm
  {$IFDEF FPC}
  MOV EAX, [X]
  MOV EDX, [Y]
  {$ENDIF FPC}
  {$IFDEF USE_CMOV}
  CMP   EAX, EDX
  CMOVG EAX, EDX
  {$ELSE}
  CMP EAX, EDX
  JLE @@exit
  MOV EAX, EDX
@@exit:
  {$ENDIF}
end {$IFDEF FPC} ['EAX', 'EDX'] {$ENDIF};
//[END Min]

function Max(X, Y: Integer): Integer;
asm
  {$IFDEF FPC}
  MOV EAX, [X]
  MOV EDX, [Y]
  {$ENDIF FPC}
  {$IFDEF USE_CMOV}
  CMP EAX, EDX
  CMOVL EAX, EDX
  {$ELSE}
  CMP EAX, EDX
  JGE @@exit
  MOV EAX, EDX
@@exit:
  {$ENDIF}
end {$IFDEF FPC} ['EAX', 'EDX'] {$ENDIF};

function Abs(X: Integer): Integer;
asm
  {$IFDEF FPC}
  MOV EAX, [X]
  {$ENDIF FPC}
  TEST EAX, EAX
  JGE @@1
  NEG EAX
@@1:
end {$IFDEF FPC} ['EAX'] {$ENDIF};

function Sgn(X: Integer): Integer;
asm
  CMP EAX, 0
  {$IFDEF USE_CMOV}
  MOV EDX, -1
  CMOVL EAX, EDX
  MOV EDX, 1
  CMOVG EAX, EDX
  {$ELSE}
  JZ  @@exit
  MOV EAX, 1
  JG  @@exit
  MOV EAX, -1
@@exit:
  {$ENDIF}
end;

function iSQRT(X: DWORD): Integer;
var m, y, b: DWORD;
begin
  m := $40000000;
  y := 0;
  while m <> 0 do // 16 times
  begin
    b := y or m;
    y := y shr 1;
    if x >= b then
    begin
      x := x - b;
      y := y or m;
    end;
    m := m shr 2;
  end;
  Result := y;
end;
{var I, N: Integer;
begin
  Result := 0;
  while Result < X do
  begin
    I := 1;
    while I > 0 do
    begin
      N := (Result + I) * (Result + I);
      if N > X then
      begin
        I := I shr 1;
        break;
      end
        else
      if N = X then
      begin
        Result := Result + I;
        Exit;
      end;
      I := I shl 1;
    end;
    if I <= 0 then Exit;
    Result := Result + I;
  end;
end;}

function iCbrt(X: DWORD): Integer;
var s: Integer;
    y, b: DWORD;
begin
  s := 30;
  y := 0;
  while s >= 0 do // 11 times
  begin
    y := 2 * y;
    b := (3 * y * (y+1) + 1) shl s;
    s := s - 3;
    if x >= b then
    begin
      x := x - b;
      y := y + 1;
    end;
  end;
  Result := y;
end;

function ReadProtectedMemory(BaseAddress, Buffer: Pointer; Size: Cardinal;
  out ReadBytes: Cardinal): Boolean;
begin
  Result := ReadProcessMemory(FCurrentProcess, BaseAddress, Buffer, Size, ReadBytes);
end;

function WriteProtectedMemory(BaseAddress, Buffer: Pointer;
  Size: Cardinal; out WrittenBytes: Cardinal): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := WriteProcessMemory(FCurrentProcess, BaseAddress, Buffer, Size, WrittenBytes);
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{ TODO -cHelp : Author: Andreas Hausladen }
{ TODO : Works so far, but causes app to hang on termination }
var
  AlignedAddress: Cardinal;
  PageSize, ProtectSize: Cardinal;
begin
  Result := False;
  WrittenBytes := 0;

  PageSize := Cardinal(getpagesize);
  AlignedAddress := Cardinal(BaseAddress) and not (PageSize - 1); // start memory page
  // get the number of needed memory pages
  ProtectSize := PageSize;
  while Cardinal(BaseAddress) + Size > AlignedAddress + ProtectSize do
    Inc(ProtectSize, PageSize);

  if mprotect(Pointer(AlignedAddress), ProtectSize,
       PROT_READ or PROT_WRITE or PROT_EXEC) = 0 then // obtain write access
  begin
    try
      Move(Buffer^, BaseAddress^, Size); // replace code
      Result := True;
      WrittenBytes := Size;
    finally
      // Is there any function that returns the current page protection?
//    mprotect(p, ProtectSize, PROT_READ or PROT_EXEC); // lock memory page
    end;
  end;
end;

procedure FlushInstructionCache;
{ TODO -cHelp : Author: Andreas Hausladen }
begin
  // do nothing
end;

{$ENDIF LINUX}

function  MultipleIntUp(const aValue: Integer; aRoundBase: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
    Result := (1 +  (aValue - 1) div aRoundBase);
end;
{$ELSE}
asm
   {EAX <- aValue
    EDX <- aRoundBase
   }
   MOV ECX, aRoundBase
   SUB  aValue, 1
   CDQ       //convert the EAX to EDX:EAX Quadword
   IDIV  ECX   //  EDX:EAX IDiv ECX -> EAX, mod -> EDX 
   INC  EAX
end;
{$ENDIF}

function  RoundIntUp(const aValue: Integer; aRoundBase: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
    Result := (1 +  (aValue - 1) div aRoundBase) * aRoundBase;
end;
{$ELSE}
asm
   {EAX <- aValue
    EDX <- aRoundBase
   }
   MOV ECX, aRoundBase
   SUB  aValue, 1
   CDQ       //convert the EAX to EDX:EAX Quadword
   IDIV  ECX   //  EDX:EAX IDiv ECX -> EAX, mod -> EDX 
   INC  EAX
   MUL  ECX
end;
{$ENDIF}

procedure ReadMem(const Location, Buffer: Pointer; const Size: Cardinal);
var
  ReadBytes: Cardinal;
begin
  if (not ReadProcessMemory(FCurrentProcess, Location,
    Buffer, Size, ReadBytes)) or (ReadBytes <> Size) then
      raise EMeError.CreateResFmt(@rsMemoryReadError, [SysErrorMessage(GetLastError)]);
end;

procedure WriteMem(const Location, Buffer: Pointer; const Size: Cardinal);
var
  WrittenBytes: DWORD;
  SaveFlag: DWORD;
begin
  //! StH: WriteProcessMemory IMO is not exactly the politically correct approach;
  // better VirtualProtect, direct patch, VirtualProtect
  if VirtualProtect(Location, Size, PAGE_EXECUTE_READWRITE, @SaveFlag) then
  try
    if not WriteProtectedMemory(Location, Buffer, Size, WrittenBytes) then
      raise EMeError.CreateResFmt(@rsMemoryWriteError, [SysErrorMessage(GetLastError)]);
  finally
    VirtualProtect(Location, Size, SaveFlag, @SaveFlag);
  end;

  if WrittenBytes <> Size then
    raise EMeError.CreateResFmt(@rsMemoryWriteError, [IntToStr(WrittenBytes)]);

  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache{$IFDEF MSWINDOWS}(FCurrentProcess, Location, Size){$ENDIF};
end;

procedure WriteJumpBuffer(Location: Pointer; const Buffer: TRedirectCodeRec);
begin
  WriteMem(Location, @Buffer, cNearJMPDirectiveSize);
end;

procedure ReadJumpBuffer(Location: Pointer; var Buffer: TRedirectCodeRec);
begin
  ReadMem(Location, @Buffer, cNearJMPDirectiveSize);
end;

function PatchCallDirective(PatchLocation: Pointer; CallTarget: Pointer;
                  var PatchLocationBackup: TRedirectCodeRec): Boolean;
begin
  Result := PatchDirective(PatchLocation, CallTarget, PatchLocationBackup, cX86CallDirective);
end;

function IsPatchedJump(PatchLocation: Pointer; Target: Pointer): Boolean;
begin
  Result := IsPatchedDirective(PatchLocation, Target, cX86JumpDirective);
end;

function IsPatchedCall(PatchLocation: Pointer; Target: Pointer): Boolean;
begin
  Result := IsPatchedDirective(PatchLocation, Target, cX86CallDirective);
end;

function IsPatchedDirective(PatchLocation: Pointer; Target: Pointer; aDirective: Byte): Boolean;
var
  Buffer: TRedirectCodeRec;
  PatchLocationBackup: TRedirectCodeRec;
begin
  ReadJumpBuffer(PatchLocation, PatchLocationBackup);

  Buffer.Jump := aDirective; //a directive
  Buffer.Offset := Integer(Target) - (Integer(PatchLocation) + cNearJMPDirectiveSize);
  Result := (Buffer.Offset =PatchLocationBackup.Offset) and (Buffer.Jump = PatchLocationBackup.Jump);
end;


function PatchDirective(PatchLocation: Pointer; Target: Pointer;
                  var PatchLocationBackup: TRedirectCodeRec; aDirective: Byte): Boolean;
var
  Buffer: TRedirectCodeRec;
  //n: Cardinal;
  {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
  vHoleAddr: Pointer;
  {$ENDIF}
  {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
  I: Integer;
  vOpCodeLen: Integer;
  SaveFlag: DWORD;
  {$IFNDEF STATIC_METHOD_THREADSAFE_SUPPORT}
  vHoleAddr: Pointer;
  {$ENDIF}
  {$ENDIF}
begin
  ReadJumpBuffer(PatchLocation, PatchLocationBackup);
  {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
  //vHoleAddr := nil;
  {$ENDIF}
  {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
  vHoleAddr := GetPatchHoleAddress(PatchLocation);
  if (vHoleAddr <> nil) and (vHoleAddr <> PatchLocation) then
  begin
    //adjust the hole position to the entry.
    MoveProcEntryToHole(PatchLocation, vHoleAddr);
    ReadJumpBuffer(PatchLocation, PatchLocationBackup);
  end;
  {$ENDIF}

  {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
    {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
    if vHoleAddr = nil then
    {$ENDIF}
    begin
      I := 0;
      vHoleAddr := PatchLocation;
      if VirtualProtect(PatchLocation, 64, PAGE_EXECUTE_READWRITE, @SaveFlag) then
      try
        while I < cNearJMPDirectiveSize do
        begin
          vOpCodeLen := GetX86OpCodeLength(vHoleAddr);
          if vOpCodeLen = -1 then
            raise EMeError.CreateResFmt(@rsInvalidOpCodeError, [PByte(vHoleAddr)^]);
          if (PByte(vHoleAddr)^ = cX86CallDirective) or (PByte(vHoleAddr)^ = cX86JumpDirective) then
          begin
            PRedirectCodeRec(vHoleAddr)^.Offset := PRedirectCodeRec(vHoleAddr)^.Offset + Integer(PatchLocation) - Integer(@PatchLocationBackup);
          end
          else if ((PWord(vHoleAddr)^ and $FFF0) = $0F80) then
          begin
            PNearJumpIfDirective(vHoleAddr)^.Offset := PNearJumpIfDirective(vHoleAddr)^.Offset + Integer(PatchLocation) - Integer(@PatchLocationBackup);
          end
          ;
          Inc(I, vOpCodeLen);
          Integer(vHoleAddr) := Integer(PatchLocation) + I;
        end; //while
      finally
        VirtualProtect(PatchLocation, 64, SaveFlag, @SaveFlag);
      end;
      ReadMem(PatchLocation, @PatchLocationBackup, I);
      Assert(I <= cRedirectCodeRecSize, 'the OpCode lenth should not be greater than ' + IntToStr(cRedirectCodeRecSize) + 'bytes');
      {//if Last OpCode is RET or JUMP the exit;
      if vOpCodeLen = -1 then exit; //}
      Pointer(vOpCodeLen) := @PatchLocationBackup.Code[I];
      with PRedirectCodeRec(vOpCodeLen)^ do
      begin
        Jump := cX86JumpDirective;
        Offset := Integer(vHoleAddr) - vOpCodeLen - cNearJMPDirectiveSize;
      end;
      //VirtualProtect(@PatchLocationBackup, SizeOf(PatchLocationBackup), PAGE_EXECUTE_READWRITE, @I);
    end;
  {$ENDIF}

  Buffer.Jump := aDirective; //a directive
  Buffer.Offset := Integer(Target) - Integer(PatchLocation) - cNearJMPDirectiveSize;
  Result := (Buffer.Offset <> PatchLocationBackup.Offset) or (Buffer.Jump <> PatchLocationBackup.Jump);
  if Result then
    WriteJumpBuffer(PatchLocation, Buffer);
    //WriteProtectedMemory(PatchLocation, @Buffer, SizeOf(Buffer), n)
end;

function UnPatchDirective(PatchLocation: Pointer;  const PatchLocationBackup: TRedirectCodeRec): Boolean;
var
  Buffer: TRedirectCodeRec;
  {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
  i: Integer;
  vOpCodeLen: Integer;
  SaveFlag: DWORD;
  p: Pointer;
  {$ENDIF}
begin
  ReadJumpBuffer(PatchLocation, Buffer);
  Result := (Buffer.Offset <> PatchLocationBackup.Offset) or (Buffer.Jump <> PatchLocationBackup.Jump);
  if Result then
  begin
    WriteJumpBuffer(PatchLocation, PatchLocationBackup);
    {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
    if VirtualProtect(PatchLocation, 64, PAGE_EXECUTE_READWRITE, @SaveFlag) then
    try
      I := 0;
      p := PatchLocation;
      while I < cNearJMPDirectiveSize do
      begin
        vOpCodeLen := GetX86OpCodeLength(p);
        Assert(vOpCodeLen <> -1, Format(rsInvalidOpCodeError, [PByte(p)^]));
        if (PByte(p)^ = cX86CallDirective) or (PByte(p)^ = cX86JumpDirective) then
        begin
          PRedirectCodeRec(p)^.Offset := PRedirectCodeRec(p)^.Offset + Integer(@PatchLocationBackup) - Integer(PatchLocation);
        end
        else if ((PWord(p)^ and $FFF0) = $0F80) then
        begin
          PNearJumpIfDirective(p)^.Offset := PNearJumpIfDirective(p)^.Offset + Integer(@PatchLocationBackup) - Integer(PatchLocation);
        end
        ;
        Inc(I, vOpCodeLen);
        Inc(Integer(p),  vOpCodeLen);
      end;
    finally
      VirtualProtect(PatchLocation, 64, SaveFlag, @SaveFlag);
    end;
    {$ENDIF}
  end;
end;

function PatchJumpDirective(PatchLocation: Pointer; JumpTarget: Pointer;
                  var PatchLocationBackup: TRedirectCodeRec): Boolean;
begin
  Result := PatchDirective(PatchLocation, JumpTarget, PatchLocationBackup, cX86JumpDirective);
end;

{$IFDEF INJECT_DLL_SUPPORT}
{ returns real address of proc/var if it is located in run-time package }
function GetDirectedAddr(AnAddr: Pointer): Pointer;

type
  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;   // $FF25(Jmp, FF /4), $FF15(Call, FF /2)
    Addr: ^Pointer;
  end;
  PRelativeJmpOrCall = ^TRelativeJmpOrCall;
  TRelativeJmpOrCall = packed record
    OpCode: Byte;   // $E9(Jmp),$E8(Call)
    Offset: LongInt;
  end;

begin
  if Assigned(AnAddr) then
  begin
    if (PAbsoluteIndirectJmp(AnAddr).OpCode = $25FF) or (PAbsoluteIndirectJmp(AnAddr).OpCode = $15FF) then
    begin
      Result := PAbsoluteIndirectJmp(AnAddr).Addr^;
    end
    else if (PRelativeJmpOrCall(AnAddr).OpCode = cX86JumpDirective){ or (PRelativeJmpOrCall(AnAddr).OpCode = cX86CallDirective)} then
    begin
      Result := Pointer(Integer(AnAddr) + SizeOf(TRelativeJmpOrCall) + PRelativeJmpOrCall(AnAddr).Offset);
    end
    else begin
      Result := AnAddr;
    end;
  end
  else begin
    Result := nil;
  end;
end;

type
  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: Pointer; // The actual address of the DLL routine
    Jump: Byte;     // JMP instruction opcode ($E9)
    Rel: Integer;  // Relative displacement (a Kernel32 address)
  end;

function IsWin9xDebugThunk(AnAddr: Pointer): Boolean;
{ -> EAX: AnAddr }
asm
  TEST EAX, EAX
  JZ  @@NoThunk
  CMP BYTE PTR [EAX+TWin9xDebugThunk.PUSH], $68
  JNE @@NoThunk
  {$IFDEF FPC}
  CMP BYTE PTR [EAX+TWin9xDebugThunk.Jump], cX86JumpDirective
  {$ELSE}
  CMP BYTE PTR [EAX].TWin9xDebugThunk.Jump, cX86JumpDirective
  {$ENDIF}
  JNE @@NoThunk
  XOR EAX, EAX
  MOV AL, 1
  JMP @@exit
@@NoThunk:
  XOR EAX, EAX
@@exit:
end;
{$ENDIF}

function GetActualAddress(AnAddr: Pointer): Pointer;
begin
  {$IFDEF INJECT_DLL_SUPPORT}
  if Assigned(AnAddr) then
  begin
    if (SysUtils.Win32Platform <> VER_PLATFORM_WIN32_NT) then
    begin
      if IsWin9xDebugThunk(AnAddr) then
        AnAddr := PWin9xDebugThunk(AnAddr).Addr;
    end
    else //Only IsWin32NT CAN write the system dll memory !!
      AnAddr := GetDirectedAddr(AnAddr); 
  end;
  {$ENDIF}
  Result := AnAddr;
end;

{$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
procedure MoveProcEntryToHole(const aProc: Pointer; const aPatchHoleAddr: Pointer);
var
  p: Pointer;
  i: Integer;
  vCodeLen: Integer;
  SaveFlag: DWORD;
begin
  Integer(p) := Integer(aProc) + cNearJMPDirectiveSize;
  if VirtualProtect(aProc, Integer(aPatchHoleAddr) - Integer(aProc), PAGE_EXECUTE_READWRITE, @SaveFlag) then
  try
    Move(aProc^, p^, Integer(aPatchHoleAddr) - Integer(aProc));
    FillChar(aProc^, cNearJMPDirectiveSize, cX86NoOpDirective);
    i := Integer(aPatchHoleAddr) - Integer(aProc);
    while (i > 0) do //seach the near call directive to update the offset!
    begin
      if (PByte(p)^ = cX86CallDirective) or (PByte(p)^ = cX86JumpDirective) then
      begin
        PRedirectCodeRec(p)^.Offset := PRedirectCodeRec(p)^.Offset - cNearJMPDirectiveSize;
      end
      else if (PByte(p)^ = $E3) or ((PByte(p)^ and $F0) = $70) then
      begin //jump short directive(offset is byte); $E3 = JECXZ
        PShortJumpIfDirective(p)^.Offset := PShortJumpIfDirective(p)^.Offset - cNearJMPDirectiveSize;
      end
      else if ((PWord(p)^ and $FFF0) = $0F80) then
      begin
        PNearJumpIfDirective(p)^.Offset := PNearJumpIfDirective(p)^.Offset - cNearJMPDirectiveSize;
      end
      ;
      vCodeLen := GetX86OpCodeLength(p);
      if vCodeLen = -1 then
        raise EMeError.CreateResFmt(@rsInvalidOpCodeError, [PByte(P)^]);
      Dec(i, vCodeLen);
      Inc(Integer(p),vCodeLen);
    end;
  finally
    VirtualProtect(aProc, Integer(aPatchHoleAddr) - Integer(aProc), SaveFlag, @SaveFlag);
  end;
end;

//nil means not found, else return the address of the hole
function GetPatchHoleAddress(const aProc: Pointer): Pointer;
var
  i: integer;
  p: Pointer;
begin
  p := aProc;
  Integer(Result) := 0;
  
  while (Integer(Result) <= 16) and (PByte(p)^ <> cX86RetNearDirective) and (PByte(p)^ <> cX86RetNearParamDirective) do
  begin
    if PByte(p)^ = cX86NoOpDirective then break;
    i := GetX86OpCodeLength(p);
    if i = -1 then
      raise EMeError.CreateResFmt(@rsInvalidOpCodeError, [PByte(P)^]);
    Inc(Integer(p),i);
    Inc(Integer(Result));
  end;
  if (PRedirectCodeRec(p)^.Offset = Integer(cX86NoOpDirective4Bytes)) and  (PRedirectCodeRec(p)^.Jump = cX86NoOpDirective) then
  begin
    Result := p;
  end
  else 
    Result := nil;
end;
{$ENDIF}

{ Summary RedirectCode Is Noop directives or not }
{
EAX: <- TRedirectCodeRec
EAX: -> Result
affect registers: EAX, ECX
}
function IsRedirectCodeNoop(const aRedirectCode: TRedirectCodeRec): Boolean;
{$IFDEF PUREPASCAL}
begin
  with aRedirectCode do 
    Result := (Offset = Integer(cX86NoOpDirective4Bytes)) and  (Jump = cX86NoOpDirective);
end;
{$ELSE}
asm
  MOV  ECX, aRedirectCode
  {$IFDEF FPC}
  MOV  EAX, [ECX].TRedirectCodeRec.CodeOffset
  {$ELSE}
  MOV  EAX, [ECX+TRedirectCodeRec.&Offset]
  {$ENDIF}
  CMP  EAX, cX86NoOpDirective4Bytes
  JNZ  @NotNoop
  MOV  AL, [ECX+TRedirectCodeRec.Jump]
  CMP  AL, cX86NoOpDirective
  JNZ  @NotNoop
  MOV  EAX, 1
  RET
@NotNoop:
  MOV EAX, 0   
end;
{$ENDIF}

initialization
{$IFDEF MSWINDOWS}
  FCurrentProcess := GetCurrentProcess;
  {以特权的方式来打开当前进程}
  //FCurrentProcess := OpenProcess(PROCESS_ALL_ACCESS,FALSE, GetCurrentProcessID);
{$ENDIF}

finalization
{$IFDEF MSWINDOWS}
  {关闭特权句柄}
  //CloseHandle(FCurrentProcess);
{$ENDIF}

end.
