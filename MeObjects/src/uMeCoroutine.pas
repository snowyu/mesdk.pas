
{Summary: MeCoroutine - implements the coroutine object. }
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
 * The Original Code is $RCSfile: uMeCoroutine.pas,v $.
 *
 * The Initial Developers of the Original Code are Riceball LEE.
 * Portions created by Riceball LEE<riceballl@hotmail.com> is Copyright (C) 2007
 * All rights reserved.
 *
 * Contributor(s):
 *
 *

 Usage:
 *)
unit uMeCoroutine;

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

const
  cStackSize = 1024 * 4;

type
  {$IFDEF YieldClass_Supports}
  TMeCoroutine = Class;
  {$ELSE}
  PMeCoroutine = ^TMeCoroutine;

  PMeYieldObject = ^TMeYieldObject;
  PYieldString = ^TYieldString;
  PYieldInteger = ^TYieldInteger;
  {$ENDIF}

  TMeCoroutineProc = procedure (const aCoroutine: {$IFDEF YieldClass_Supports}TMeCoroutine{$ELSE} PMeCoroutine{$endif});
  TMeCoroutineMethod = procedure () of object;

  TMeCoroutineStatus = (coSuspended, coRunning, coDead);
  TMeCoroutineErrorCode = (cecNone, cecAlreadyRunning, cecNotRunning, cecDead);

  TPreservedRegisters = packed record
    FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    FESP:pointer;
  end;
  TMeCoroutine = {$IFDEF YieldClass_Supports}class{$ELSE}object(TMeDynamicObject){$ENDIF}
  protected
    FLastErrorCode: Integer;
    FStatus: TMeCoroutineStatus;
    FInited: Byte;
    FNextIP: TMeCoroutineProc;
    //FNextItemEntryPoint:pointer;
    //Old backup
    //FSysRegs: TPreservedRegisters;
    //FRegs: TPreservedRegisters;
    FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    FESP:pointer;

    //FStackFrameSize:DWORD;
    //FStackFrame: array[1..128] of DWORD;
    FStack: Pointer;
    FStackPointer: Pointer; //the stack pointer.
    FStackTop: Pointer;
    FSTackFrame: Pointer;
    procedure SaveYieldedValue(const aValue); virtual; abstract;
    {$IFNDEF YieldClass_Supports}
    procedure Init(); virtual; {override;}
    {$ENDIF}
  public
    constructor Create(const CoroutineProc: TMeCoroutineProc);
    destructor Destroy; {$IFDEF YieldClass_Supports}override;{$ELSE}virtual;{$ENDIF}
    function Resume:boolean;
    //hang up the coroutine and restore the old register and return.
    procedure Yield(const Value);
  end;

  TMeYieldObject = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCoroutine)
  public
    function MoveNext:boolean;
  end;

  TYieldInteger = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeYieldObject)
  protected
    FValue: Integer;
    function GetCurrent: Integer;
    procedure SaveYieldedValue(const aValue); {$IFDEF YieldClass_Supports}override{$ELSE} virtual{$endif}; 
  public

    //return the Current value
    property Current:Integer read GetCurrent;
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

    property Current:string read GetCurrent;
  end;

implementation

{ TMeCoroutine }
constructor TMeCoroutine.Create(const CoroutineProc: TMeCoroutineProc);
begin
  inherited Create;
  {$IFDEF YieldClass_Supports}
  FStack := AllocMem(cStackSize*SizeOf(Pointer));
  FStackTop := Pointer(Integer(FStack) + (cStackSize)*SizeOf(Pointer));
  FStackPointer := FStackTop;
  FSTackFrame := FStackTop;
  FEAX := Self;
  {$ENDIF}
  FNextIP := CoroutineProc;
end;

{$IFNDEF YieldClass_Supports}
 procedure TMeCoroutine.Init();
{$ENDIF}
begin
  inherited;
  FStack := AllocMem(cStackSize*SizeOf(Pointer));
  FStackTop := Pointer(Integer(FStack) + (cStackSize)*SizeOf(Pointer));
  FStackPointer := FStackTop;
  FSTackFrame := FStackTop;
  FEAX := @Self;
end;

destructor TMeCoroutine.Destroy();
begin
  FreeMem(FStack);
  inherited;
end;

function TMeCoroutine.Resume: boolean;
asm
  CMP  EAX.TMeCoroutine.FStatus, coSuspended
  JNE  @@CheckExit

@@DoResume:
  { Preserve EBP, EAX,EBX,ECX,EDX,ESI,EDI }
  MOV EAX.TMeCoroutine.FEBP,EBP;
  MOV EAX.TMeCoroutine.FESP,ESP;

  PUSH EAX.TMeCoroutine.FEBX;
  PUSH EAX.TMeCoroutine.FECX;
  PUSH EAX.TMeCoroutine.FEDX;
  PUSH EAX.TMeCoroutine.FESI;
  PUSH EAX.TMeCoroutine.FEDI;

  MOV EAX.TMeCoroutine.FEBX,EBX;
  MOV EAX.TMeCoroutine.FECX,ECX;
  MOV EAX.TMeCoroutine.FEDX,EDX;
  MOV EAX.TMeCoroutine.FESI,ESI;
  MOV EAX.TMeCoroutine.FEDI,EDI;

  POP  EDI;
  POP  ESI;
  POP  EDX;
  POP  ECX;
  POP  EBX;

  MOV  ESP, EAX.TMeCoroutine.FStackPointer
  MOV  EBP, EAX.TMeCoroutine.FStackFrame
  MOV  EAX.TMeCoroutine.FStatus, coRunning
  CMP  EAX.TMeCoroutine.FInited, 0
  JNZ  @@DoNextIP

  //PUSH EAX.TMeCoroutine.FEBP
  MOV  EBP, EAX.TMeCoroutine.FESP
  PUSH EAX
  PUSH offset @@DeadExit
  MOV  EAX.TMeCoroutine.FInited, 1

@@DoNextIP: //TODO: add exception process here
  PUSH [EAX.TMeCoroutine.FNextIP]
  MOV  EAX, EAX.TMeCoroutine.FEAX
  RET

@@DeadExit: //the coroutine finished 
  POP EAX
  MOV EBP, EAX.TMeCoroutine.FEBP;
  MOV EBX, EAX.TMeCoroutine.FEBX;
  MOV ECX, EAX.TMeCoroutine.FECX;
  MOV EDX, EAX.TMeCoroutine.FEDX;
  MOV ESI, EAX.TMeCoroutine.FESI;
  MOV EDI, EAX.TMeCoroutine.FEDI;
  MOV ESP, EAX.TMeCoroutine.FESP;
  //mov EAX, eax.TMeCoroutine.FEAX;

  MOV EAX.TMeCoroutine.FStatus,coDead
  MOV EAX.TMeCoroutine.FLastErrorCode, Ord(cecNone)
  MOV EAX, 0
  RET

@@CheckExit:
  CMP  EAX.TMeCoroutine.FStatus, coRunning
  JNE  @@skip
  MOV  EAX.TMeCoroutine.FLastErrorCode, Ord(cecAlreadyRunning)
@@skip:
  MOV  EAX, 0
  RET

@@exit:

end;

procedure TMeCoroutine.Yield(const Value);
asm
  CMP  EAX.TMeCoroutine.FStatus, coRunning
  JNE  @@ErrorExit
  POP  ECX
  MOV  EAX.TMeCoroutine.FNextIP, ECX

  //We must do it first for valid const reference
  PUSH EAX;
  MOV ECX,[EAX];
  CALL  DWORD PTR [ecx+VMTOFFSET TMeCoroutine.SaveYieldedValue];
  POP EAX;
  

  {Set flag of Yield occurance }
  MOV EAX.TMeCoroutine.FStatus, coSuspended;


  { Preserve EBP, EAX,EBX,ECX,EDX,ESI,EDI }
  PUSH EAX.TMeCoroutine.FEBX;
  PUSH EAX.TMeCoroutine.FECX;
  PUSH EAX.TMeCoroutine.FEDX;
  PUSH EAX.TMeCoroutine.FESI;
  PUSH EAX.TMeCoroutine.FEDI;

  MOV EAX.TMeCoroutine.FEBX,EBX;
  MOV EAX.TMeCoroutine.FECX,ECX;
  MOV EAX.TMeCoroutine.FEDX,EDX;   // This is the Ref to const param
  MOV EAX.TMeCoroutine.FESI,ESI;
  MOV EAX.TMeCoroutine.FEDI,EDI;
  MOV EAX.TMeCoroutine.FEAX,EAX;

  POP  EDI;
  POP  ESI;
  POP  EDX;
  POP  ECX;
  POP  EBX;
  //POP  EBP;
  
  MOV  EAX.TMeCoroutine.FStackPointer, ESP;
  MOV  EAX.TMeCoroutine.FStackFrame, EBP;
{
  INC ESP
  INC ESP
  INC ESP
  INC ESP
}
  MOV  ESP, EAX.TMeCoroutine.FESP;
  MOV  EBP, EAX.TMeCoroutine.FEBP;
  RET

@@ErrorExit:
  MOV  EAX.TMeCoroutine.FLastErrorCode, Ord(cecNotRunning)

end;

function TMeYieldObject.MoveNext:boolean;
begin
  Result := Resume();
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

initialization
end.
