
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

 Usage:
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

  TMeCoRoutine = {$IFDEF YieldClass_Supports}class{$ELSE}object(TMeDynamicObject){$ENDIF}
  protected
    FIsYield: Boolean;
    FStatus: TMeCoRoutineStatus;
    FNextIP:TMeCoRoutineProc;
    FProc: TMeCoRoutineProc;
    FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    FESP:pointer;
    FStackFrameSize:DWORD;
    FStackFrame: array[1..128] of DWORD;
    procedure SaveYieldedValue(const aValue); virtual; abstract;
  public
    constructor Create(const CoRoutineProc: TMeCoRoutineProc);
    function Resume:boolean;
    function Reset:boolean;
    procedure Yield(const Value);

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
  mov eax.TMeCoRoutine.FEAX,EAX;
end;

function TMeCoRoutine.Reset:boolean;
begin
  Result := (FStatus = coDead) and Assigned(FProc);
  if Result then
  begin
    FNextIP := FProc;
    FStatus := coSuspended;
    FEAX:= {$IFNDEF YieldClass_Supports}@{$ENDIF}Self;
    FEBX:= nil;
    FECX:= nil;
    FEDX:= nil;
    FESI:= nil;
    FEDI:= nil;
    FEBP:= nil;
    FESP:= nil;
    FStackFrameSize := 0;
  end;
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
  cmp eax.TMeCoRoutine.FESP,edx;
  jz @AfterEBPAdjust;

  { Here is the correction of EBP. Some need of optimization still exists. }
  mov edx,esp;
  sub edx,eax.TMeCoRoutine.FESP;
  add [eax.TMeCoRoutine.FEBP],edx

  @AfterEBPAdjust:
  mov eax.TMeCoRoutine.FESP,esp;

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
  mov ebx,eax.TMeCoRoutine.FEBX;
  mov ecx,eax.TMeCoRoutine.FECX;
  mov edx,eax.TMeCoRoutine.FEDX;
  mov esi,eax.TMeCoRoutine.FESI;
  mov edi,eax.TMeCoRoutine.FEDI;
  mov ebp,eax.TMeCoRoutine.FEBP;
  push [eax.TMeCoRoutine.FNextIP];
  mov eax,eax.TMeCoRoutine.FEAX;

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

procedure TMeCoRoutine.Yield(const Value);
asm
  { Preserve EBP, EAX,EBX,ECX,EDX,ESI,EDI }
  mov eax.TMeCoRoutine.FEBP,ebp;
  mov eax.TMeCoRoutine.FEAX,eax;
  mov eax.TMeCoRoutine.FEBX,ebx;
  mov eax.TMeCoRoutine.FECX,ecx;
  mov eax.TMeCoRoutine.FEDX,edx;   // This is the Ref to const param
  mov eax.TMeCoRoutine.FESI,ESI;
  mov eax.TMeCoRoutine.FEDI,EDI;
  pop ecx;
  mov eax.TMeCoRoutine.FNextIP,ecx; //store the next execution address

  //We must do it first for valid const reference
  push eax;
  mov ecx,[eax];
  CALL  DWORD PTR [ecx+VMTOFFSET TMeCoRoutine.SaveYieldedValue];
  pop eax;
  
  { Calculate the current local stack frame size }
  mov ecx,eax.TMeCoRoutine.FESP;
  sub ecx,esp;
  mov eax.TMeCoRoutine.FStackFrameSize,ecx;
  jz @AfterSaveStack;

  { Preserve the local stack frame }
  lea esi,[esp];
  lea edi,[eax.TMeCoRoutine.FStackFrame];
  
  shr ecx, 2
  rep movsd;
  mov esp,eax.TMeCoRoutine.FESP;
  @AfterSaveStack:

  {Set flag of Yield occurance }
  MOV EAX.TMeCoRoutine.FIsYield,1;
  mov EAX.TMeCoRoutine.FStatus, coSuspended;
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
