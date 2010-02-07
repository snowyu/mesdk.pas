
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 362 $

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
    * The Original Code is $RCSfile: uMeException.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2007-2010
    * All rights reserved.
    * Contributor(s):
}
unit uMeException;

{$I MeSetting.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Types,
  {$IFDEF LINUX}
  Libc, //getpagesize
  {$ENDIF}
  {$ENDIF UNIX}
  SysUtils
  , uMeConsts
  ;


type
  EMeError  = class(Exception)
  public
    ErrorCode: Integer;

    constructor Create(const Msg: string; const aErrorCode: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = -1 {$ENDIF});
    constructor CreateFmt (const Msg: string; const Args: array of const; const aErrorCode: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = -1 {$ENDIF});
    constructor CreateRes(Ident: {$IFDEF FPC}PString{$ELSE}Longint{$ENDIF}; const aErrorCode: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = -1 {$ENDIF}); overload;
    constructor CreateRes(ResStringRec: PResStringRec; const aErrorCode: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = -1 {$ENDIF}); overload;
  end;


//return the multiple of the int value to upper nearest "Base" multiple
function  MultipleIntUp(const aValue: Integer; aRoundBase: Integer = SizeOf(Integer)): Integer;

//round an int value to upper nearest "Base" multiplier
function  RoundIntUp(const aValue: Integer; aRoundBase: Integer = SizeOf(Integer)): Integer;


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
function IsSameMethod(Var A,B): Boolean;


implementation

function ToMethod(const aProc: Pointer; const aData: Pointer): TMethod;
begin
  with Result do
  begin
    Data := aData;
    Code := aProc;
  end;
end;

function IsSameMethod(Var A,B): Boolean;
begin
  Result := (TMethod(A).Data=TMethod(b).Data) and (TMethod(A).Code=TMethod(B).Code);
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
{$IFDEF FPC}
begin
  Result := X;
  if Y < X then Result := Y;
end;
{$ENDIF FPC}
{$IFDEF BORLAND}
asm
  {$IFDEF FPC} 
//  MOV EAX, [X] //Now FPC Use the Delphi Call Conversation
//  MOV EDX, [Y]
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
{$ENDIF BORLAND}

function Max(X, Y: Integer): Integer;
asm
  {$IFDEF FPC}
//  MOV EAX, [X]
//  MOV EDX, [Y]
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
//  MOV EAX, [X]
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

{ EMeError }
constructor EMeError.Create(const Msg: string; const aErrorCode: Integer);
begin
  inherited Create(Msg);
  ErrorCode := aErrorCode;
end;

constructor EMeError.CreateFmt(const Msg: string; const Args: array of const; const aErrorCode: Integer);
begin
  inherited CreateFmt(Msg, Args);
  ErrorCode := aErrorCode;
end;

constructor EMeError.CreateRes(Ident: {$IFDEF FPC}PString{$ELSE}Longint{$ENDIF}; const aErrorCode: Integer);
begin
  inherited CreateRes(Ident);
  ErrorCode := aErrorCode;
end;

constructor EMeError.CreateRes(ResStringRec: PResStringRec; const aErrorCode: Integer);
begin
  inherited CreateRes(ResStringRec);
  ErrorCode := aErrorCode;
end;

initialization

finalization

end.
