
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
 *  Riceball LEE (port to MeObject)
 *

 Usage:
//枚举器
procedure StringCoroutineProc(const YieldObj: TaCoroutine);
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
  Result:=TYieldString.Create(StringCoroutineProc);
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

  TMeCoroutine = {$IFDEF YieldClass_Supports}class{$ELSE}object(TMeDynamicObject){$ENDIF}
  protected
    FIsYield:boolean;
    FNextIP:TMeCoroutineProc;
    BESP:pointer;
    FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    FStackFrameSize:DWORD;
    FStackFrame: array[1..128] of DWORD;
    procedure SaveYieldedValue(const aValue); virtual; abstract;
  public
    constructor Create(const CoroutineProc: TMeCoroutineProc);
    function Resume:boolean; //D2007 enumerable required
    procedure Yield(const Value);
  end;

  TMeYieldObject = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCoroutine)
  public
    function MoveNext:boolean;
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

  TYieldInteger = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeYieldObject)
  protected
    FValue: Integer;
    function GetCurrent: Integer;
    procedure SaveYieldedValue(const aValue); {$IFDEF YieldClass_Supports}override{$ELSE} virtual{$endif}; 
  public

    //return the Current value
    property Current:Integer read GetCurrent;
  end;

implementation

{ TMeCoroutine }
constructor TMeCoroutine.Create(const CoroutineProc:TMeCoroutineProc);
asm
  {$IFNDEF YieldClass_Supports}
  CALL TMeDynamicObject.Init
  {$ENDIF}
  mov eax.TMeCoroutine.FNextIP,ecx;
  mov eax.TMeCoroutine.FEAX,EAX;
end;

function TMeCoroutine.Resume: boolean;
asm
  { Save the value of following registers.
    We must preserve EBP, EBX, EDI, ESI, EAX for some circumstances.
    Because there is no guarantee that the state of registers will 
    be the same after an iteration }
  push ebp;
  push ebx;
  push edi;
  push esi;
  push eax;

  mov eax.TMeCoroutine.FIsYield,0
  push offset @@exit
  xor edx,edx;
  cmp eax.TMeCoroutine.BESP,edx;
  jz @AfterEBPAdjust;

  { Here is the correction of EBP. Some need of optimization still exists. }
  mov edx,esp;
  sub edx,eax.TMeCoroutine.BESP;
  add [eax.TMeCoroutine.FEBP],edx

  @AfterEBPAdjust:
  mov eax.TMeCoroutine.BESP,esp;

  { Is there any local frame? }
  cmp eax.TMeCoroutine.FStackFrameSize,0
  jz @JumpIn;

  { Restore the local stack frame }
  mov ecx,eax.TMeCoroutine.FStackFrameSize;
  sub esp,ecx;
  mov edi,esp;
  lea esi,eax.TMeCoroutine.FStackFrame;

  { Some need of optimization still exists. Like movsd}
  rep movsb;
  @JumpIn:

  { Restore the content of processor registers }
  mov ebx,eax.TMeCoroutine.FEBX;
  mov ecx,eax.TMeCoroutine.FECX;
  mov edx,eax.TMeCoroutine.FEDX;
  mov esi,eax.TMeCoroutine.FESI;
  mov edi,eax.TMeCoroutine.FEDI;
  mov ebp,eax.TMeCoroutine.FEBP;
  push [eax.TMeCoroutine.FNextIP];
  mov eax,eax.TMeCoroutine.FEAX;

  { Here is the jump to next iteration }
  ret;

  { And we return here after next iteration in all cases, except exception of course. }
  @@exit:;

  { Restore the preserved EBP, EBX, EDI, ESI, EAX registers }
  pop eax;
  pop esi;
  pop edi;
  pop ebx;
  pop ebp;
  { This Flag indicates the occurrence or no occurrence of Yield  }
  mov al,eax.TMeCoroutine.FIsYield;
end;

procedure TMeCoroutine.Yield(const Value);
asm
  { Preserve EBP, EAX,EBX,ECX,EDX,ESI,EDI }
  mov eax.TMeCoroutine.FEBP,ebp;
  mov eax.TMeCoroutine.FEAX,eax;
  mov eax.TMeCoroutine.FEBX,ebx;
  mov eax.TMeCoroutine.FECX,ecx;
  mov eax.TMeCoroutine.FEDX,edx;   // This is the Ref to const param
  mov eax.TMeCoroutine.FESI,ESI;
  mov eax.TMeCoroutine.FEDI,EDI;
  pop ecx;
  mov eax.TMeCoroutine.FNextIP,ecx;

  //We must do it first for valid const reference
  push eax;
  mov ecx,[eax];
  CALL  DWORD PTR [ecx+VMTOFFSET TMeCoroutine.SaveYieldedValue];
  pop eax;
  
  { Calculate the current local stack frame size }
  mov ecx,eax.TMeCoroutine.BESP;
  sub ecx,esp;
  mov eax.TMeCoroutine.FStackFrameSize,ecx;
  jz @AfterSaveStack;

  { Preserve the local stack frame }
  lea esi,[esp];
  lea edi,[eax.TMeCoroutine.FStackFrame];
  
  { Some need of optimization still exists. Like movsd }
  rep movsb;
  mov esp,eax.TMeCoroutine.BESP;
  @AfterSaveStack:

  {Set flag of Yield occurance }
  mov eax.TMeCoroutine.FIsYield,1;
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
