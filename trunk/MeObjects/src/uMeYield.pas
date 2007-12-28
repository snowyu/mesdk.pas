
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
procedure StringYieldProc(YieldObj: TYieldObject);
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
  Result:=TYieldString.Create(StringYieldProc);
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
      while MoveNext do //转到控制逻辑
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
  TMeYieldObject = Class;
  {$ELSE}
  PMeYieldObject = ^TMeYieldObject;
  PYieldString = ^TYieldString;
  PYieldInteger = ^TYieldInteger;
  {$ENDIF}

  TMeYieldProc = procedure (YieldObject: {$IFDEF YieldClass_Supports}TMeYieldObject{$ELSE} PMeYieldObject{$endif});

  TMeYieldObject = {$IFDEF YieldClass_Supports}class{$ELSE}object(TMeDynamicObject){$ENDIF}
  protected
    FIsYield:boolean;
    FNextItemEntryPoint:pointer;
    BESP:pointer;
    FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
    FStackFrameSize:DWORD;
    FStackFrame: array[1..128] of DWORD;
    procedure SaveYieldedValue(const aValue); virtual; abstract;
  public
    constructor Create(YieldProc: TMeYieldProc);
    function MoveNext:boolean; //D2007 enumerable required
    procedure Yield(const Value);
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

{ TMeYieldObject }
constructor TMeYieldObject.Create(YieldProc:TMeYieldProc);
asm
  {$IFNDEF YieldClass_Supports}
  CALL TMeDynamicObject.Init
  {$ENDIF}
  mov eax.TMeYieldObject.FNextItemEntryPoint,ecx;
  mov eax.TMeYieldObject.FEAX,EAX;
end;

function TMeYieldObject.MoveNext: boolean;
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

  mov eax.TMeYieldObject.FIsYield,0
  push offset @@exit
  xor edx,edx;
  cmp eax.TMeYieldObject.BESP,edx;
  jz @AfterEBPAdjust;

  { Here is the correction of EBP. Some need of optimization still exists. }
  mov edx,esp;
  sub edx,eax.TMeYieldObject.BESP;
  add [eax.TMeYieldObject.FEBP],edx

  @AfterEBPAdjust:
  mov eax.TMeYieldObject.BESP,esp;

  { Is there any local frame? }
  cmp eax.TMeYieldObject.FStackFrameSize,0
  jz @JumpIn;

  { Restore the local stack frame }
  mov ecx,eax.TMeYieldObject.FStackFrameSize;
  sub esp,ecx;
  mov edi,esp;
  lea esi,eax.TMeYieldObject.FStackFrame;

  { Some need of optimization still exists. Like movsd}
  rep movsb;
  @JumpIn:

  { Restore the content of processor registers }
  mov ebx,eax.TMeYieldObject.FEBX;
  mov ecx,eax.TMeYieldObject.FECX;
  mov edx,eax.TMeYieldObject.FEDX;
  mov esi,eax.TMeYieldObject.FESI;
  mov edi,eax.TMeYieldObject.FEDI;
  mov ebp,eax.TMeYieldObject.FEBP;
  push [eax.TMeYieldObject.FNextItemEntryPoint];
  mov eax,eax.TMeYieldObject.FEAX;

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
  mov al,eax.TMeYieldObject.FIsYield;
end;

procedure TMeYieldObject.Yield(const Value);
asm
  { Preserve EBP, EAX,EBX,ECX,EDX,ESI,EDI }
  mov eax.TMeYieldObject.FEBP,ebp;
  mov eax.TMeYieldObject.FEAX,eax;
  mov eax.TMeYieldObject.FEBX,ebx;
  mov eax.TMeYieldObject.FECX,ecx;
  mov eax.TMeYieldObject.FEDX,edx;   // This is the Ref to const param
  mov eax.TMeYieldObject.FESI,ESI;
  mov eax.TMeYieldObject.FEDI,EDI;
  pop ecx;
  mov eax.TMeYieldObject.FNextItemEntryPoint,ecx;

  //We must do it first for valid const reference
  push eax;
  mov ecx,[eax];
  CALL  DWORD PTR [ecx+VMTOFFSET TMeYieldObject.SaveYieldedValue];
  pop eax;
  
  { Calculate the current local stack frame size }
  mov ecx,eax.TMeYieldObject.BESP;
  sub ecx,esp;
  mov eax.TMeYieldObject.FStackFrameSize,ecx;
  jz @AfterSaveStack;

  { Preserve the local stack frame }
  lea esi,[esp];
  lea edi,[eax.TMeYieldObject.FStackFrame];
  
  { Some need of optimization still exists. Like movsd }
  rep movsb;
  mov esp,eax.TMeYieldObject.BESP;
  @AfterSaveStack:

  {Set flag of Yield occurance }
  mov eax.TMeYieldObject.FIsYield,1;
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
