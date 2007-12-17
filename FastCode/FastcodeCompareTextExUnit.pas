unit FastcodeCompareTextExUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Fastcode
 *
 * The Initial Developer of the Original Code is Fastcode
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Charalabos Michael <chmichael@creationpower.com>
 * John O'Harrow <john@elmcrest.demon.co.uk>
 * Aleksandr Sharahov
 * Riceball LEE <riceballl@hotmail.com>
 *
 * BV Version: 1.53
 * ***** END LICENSE BLOCK ***** *)

interface

{$I Fastcode.inc}

type
  FastcodeCompareTextExFunction = function(const S1, S2: PChar; const Len: Integer): Integer;

{Functions shared between Targets}
function CompareTextEx_Sha_IA32_3(const S1, S2: PChar; const Len: Integer): Integer;
function CompareTextEx_JOH_IA32_5(const S1, S2: PChar; const Len: Integer): Integer;
function CompareTextEx_JOH_IA32_6(const S1, S2: PChar; const Len: Integer): Integer;
function CompareTextEx_Sha_IA32_4(const S1, S2: PChar; const Len: Integer): integer;
function CompareTextEx_Sha_Pas_5(const S1, S2: PChar; const Len: Integer): integer;

{Functions not shared between Targets}

{Functions}

const
  FastcodeCompareTextExP4R: FastcodeCompareTextExFunction = CompareTextEx_JOH_IA32_6;
  FastcodeCompareTextExP4N: FastcodeCompareTextExFunction = CompareTextEx_JOH_IA32_6;
  FastcodeCompareTextExPMY: FastcodeCompareTextExFunction = CompareTextEx_Sha_IA32_3;
  FastcodeCompareTextExPMD: FastcodeCompareTextExFunction = CompareTextEx_Sha_IA32_3;
  FastcodeCompareTextExAMD64: FastcodeCompareTextExFunction = CompareTextEx_JOH_IA32_5;
  FastcodeCompareTextExAMD64_SSE3: FastcodeCompareTextExFunction = CompareTextEx_JOH_IA32_5;
  FastCodeCompareTextExIA32SizePenalty: FastCodeCompareTextExFunction = CompareTextEx_Sha_IA32_4;
  FastcodeCompareTextExIA32: FastcodeCompareTextExFunction = CompareTextEx_JOH_IA32_5;
  FastcodeCompareTextExMMX: FastcodeCompareTextExFunction = CompareTextEx_JOH_IA32_5;
  FastCodeCompareTextExSSESizePenalty: FastCodeCompareTextExFunction = CompareTextEx_Sha_IA32_4;
  FastcodeCompareTextExSSE: FastcodeCompareTextExFunction = CompareTextEx_JOH_IA32_5;
  FastcodeCompareTextExSSE2: FastcodeCompareTextExFunction = CompareTextEx_JOH_IA32_5;
  FastCodeCompareTextExPascalSizePenalty: FastCodeCompareTextExFunction = CompareTextEx_Sha_Pas_5;
  FastCodeCompareTextExPascal: FastCodeCompareTextExFunction = CompareTextEx_Sha_Pas_5;

var
  FastcodeCompareTextEx: FastCodeCompareTextExFunction = nil;

implementation

uses
  SysUtils
  , FastcodeCPUID;

function CompareTextEx_JOH_IA32_6(const S1, S2: PChar; const Len: Integer): Integer;
asm
  cmp     eax, edx
  je      @@Same             {S1 = S2}
  test    eax, edx
  jnz     @@Compare
  test    eax, eax
  jz      @FirstNil          {S1 = NIL}
  test    edx, edx
  jnz     @@Compare          {S1 <> NIL and S2 <> NIL}
  mov     eax, ecx           {S2 = NIL, Result = Length(S1)}
  ret
@@Same:
  xor     eax, eax
  ret
@FirstNil:
  sub     eax, ecx          {S1 = NIL, Result = -Length(S2)}
  ret
@@Compare:
  push    ebx
  push    ebp
  push    edi
  push    esi
  xor     ebx, ebx
  push    ebx                {Save Default Result}
  xor     ebp, ebp
  sub     ebp, ecx           {-Min(Length(S1),Length(S2))}
  sub     eax, ebp           {End of S1}
  sub     edx, ebp           {End of S2}
@@MainLoop:                  {Compare 4 Characters per Loop}
  mov     ebx, [eax+ebp]
  mov     ecx, [edx+ebp]
  cmp     ebx, ecx
  je      @@Next
  mov     esi, ebx           {Convert 4 Chars in EBX into Uppercase}
  or      ebx, $80808080
  mov     edi, ebx
  sub     ebx, $7B7B7B7B
  xor     edi, esi
  or      ebx, $80808080
  sub     ebx, $66666666
  and     ebx, edi
  shr     ebx, 2
  xor     ebx, esi
  mov     esi, ecx           {Convert 4 Chars in ECX into Uppercase}
  or      ecx, $80808080
  mov     edi, ecx
  sub     ecx, $7B7B7B7B
  xor     edi, esi
  or      ecx, $80808080
  sub     ecx, $66666666
  and     ecx, edi
  shr     ecx, 2
  xor     ecx, esi
  cmp     ebx, ecx
  jne     @@CheckDiff
@@Next:
  add     ebp, 4
  jl      @@MainLoop         {Loop until all required Characters Compared}
  pop     eax                {Default Result}
  jmp     @@Done
@@CheckDiff:
  pop     eax                {Default Result}
@@DiffLoop:
  cmp     bl, cl
  jne     @@SetResult
  add     ebp, 1
  jz      @@Done             {Difference after Compare Length}
  shr     ecx, 8
  shr     ebx, 8
  jmp     @@DiffLoop
@@SetResult:
  movzx   eax, bl            {Set Result from Character Difference}
  and     ecx, $ff
  sub     eax, ecx
@@Done:
  pop     esi
  pop     edi
  pop     ebp
  pop     ebx
end;

function CompareTextEx_JOH_IA32_5(const S1, S2: PChar; const Len: Integer): Integer;
asm
  cmp     eax, edx
  je      @@Same             {S1 = S2}
  test    eax, edx
  jnz     @@Compare
  test    eax, eax
  jz      @FirstNil          {S1 = NIL}
  test    edx, edx
  jnz     @@Compare          {S1 <> NIL and S2 <> NIL}
  mov     eax, ecx       {S2 = NIL, Result = Length(S1)}
  ret
@@Same:
  xor     eax, eax
  ret
@FirstNil:
  sub     eax, ecx       {S1 = NIL, Result = -Length(S2)}
  ret
@@Compare:
  push    ebx
  push    ebp
  push    edi
  push    esi
  xor     ebx, ebx
  push    ebx                {Save Default Result}
  mov     ebp, ecx
  add     eax, ecx           {End of S1}
  add     edx, ecx           {End of S2}
  neg     ebp                {Negate Compare Length}
@@MainLoop:                  {Compare 4 Characters per Loop}
  mov     ebx, [eax+ebp]
  mov     ecx, [edx+ebp]
  cmp     ebx, ecx
  je      @@Next
  mov     esi, ebx           {Convert 4 Chars in EBX into Uppercase}
  or      ebx, $80808080
  mov     edi, ebx
  sub     ebx, $7B7B7B7B
  xor     edi, esi
  or      ebx, $80808080
  sub     ebx, $66666666
  and     ebx, edi
  shr     ebx, 2
  xor     ebx, esi
  mov     esi, ecx           {Convert 4 Chars in ECX into Uppercase}
  or      ecx, $80808080
  mov     edi, ecx
  sub     ecx, $7B7B7B7B
  xor     edi, esi
  or      ecx, $80808080
  sub     ecx, $66666666
  and     ecx, edi
  shr     ecx, 2
  xor     ecx, esi
  cmp     ebx, ecx
  jne     @@CheckDiff
@@Next:
  add     ebp, 4
  jl      @@MainLoop         {Loop until all required Characters Compared}
  pop     eax                {Default Result}
  jmp     @@Done
@@CheckDiff:
  pop     eax                {Default Result}
@@DiffLoop:
  cmp     cl, bl
  jne     @@SetResult
  add     ebp, 1
  jz      @@Done             {Difference after Compare Length}
  shr     ecx, 8
  shr     ebx, 8
  jmp     @@DiffLoop
@@SetResult:
  movzx   eax, bl            {Set Result from Character Difference}
  and     ecx, $ff
  sub     eax, ecx
@@Done:
  pop     esi
  pop     edi
  pop     ebp
  pop     ebx
end;

function CompareTextEx_Sha_IA32_3(const S1, S2: PChar; const Len: Integer): Integer;
asm
         test  eax, eax
         jz    @nil1
         test  edx, edx
         jnz   @ptrok

@nil2:   mov   eax, ecx
         ret
@nil1:   test  edx, edx
         jz    @nil0
         sub   eax, ecx
@nil0:   ret

@ptrok:  push  edi
         push  ebx
         xor   edi, edi
         xor   ebx, ebx
         adc   edi, -1
         push  ebx
         and   ebx, edi
         mov   edi, eax
         sub   ebx, ecx

@lenok:  sub   edi, ebx
         sub   edx, ebx

@loop:   mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         cmp   eax, ecx
         jne   @byte0
@same:   add   ebx, 4
         jl    @loop

@len:    pop   eax
         pop   ebx
         pop   edi
         ret

@loop2:  mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         cmp   eax, ecx
         je    @same

@byte0:  cmp   al, cl
         je    @byte1

         and   eax, $FF
         and   ecx, $FF
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @up0a
         sub   eax, 'a'-'A'
@up0a:   cmp   cl, 'z'-'a'
         ja    @up0c
         sub   ecx, 'a'-'A'
@up0c:   sub   eax, ecx
         jnz   @done

         mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]

@byte1:  cmp   ah, ch
         je    @byte2

         and   eax, $FF00
         and   ecx, $FF00
         sub   eax, 'a'*256
         sub   ecx, 'a'*256
         cmp   ah, 'z'-'a'
         ja    @up1a
         sub   eax, ('a'-'A')*256
@up1a:   cmp   ch, 'z'-'a'
         ja    @up1c
         sub   ecx, ('a'-'A')*256
@up1c:   sub   eax, ecx
         jnz   @done

         mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]

@byte2:  add   ebx, 2
         jnl   @len2
         shr   eax, 16
         shr   ecx, 16
         cmp   al, cl
         je    @byte3

         and   eax, $FF
         and   ecx, $FF
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @up2a
         sub   eax, 'a'-'A'
@up2a:   cmp   cl, 'z'-'a'
         ja    @up2c
         sub   ecx, 'a'-'A'
@up2c:   sub   eax, ecx
         jnz   @done

         movzx eax, word ptr [ebx+edi]
         movzx ecx, word ptr [ebx+edx]

@byte3:  cmp   ah, ch
         je    @byte4

         and   eax, $FF00
         and   ecx, $FF00
         sub   eax, 'a'*256
         sub   ecx, 'a'*256
         cmp   ah, 'z'-'a'
         ja    @up3a
         sub   eax, ('a'-'A')*256
@up3a:   cmp   ch, 'z'-'a'
         ja    @up3c
         sub   ecx, ('a'-'A')*256
@up3c:   sub   eax, ecx
         jnz   @done

@byte4:  add   ebx, 2
         jl    @loop2
@len2:   pop   eax
         pop   ebx
         pop   edi
         ret

@done:   pop   ecx
         pop   ebx
         pop   edi
end;

function CompareTextEx_Sha_IA32_4(const S1, S2: PChar; const Len: Integer): integer;
asm
         test  eax, eax
         jz    @nil1
         test  edx, edx
         jnz   @ptrok

@nil2:   mov   eax, ecx
         ret
@nil1:   test  edx, edx
         jz    @nil0
         sub   eax, ecx
@nil0:   ret

@ptrok:  push  edi
         push  ebx
         xor   edi, edi
         xor   ebx, ebx
         adc   edi, -1
         push  ebx
         and   ebx, edi
         mov   edi, eax
         sub   ebx, ecx        //ebx := -min(Length(s1),Length(s2))

@lenok:  sub   edi, ebx
         sub   edx, ebx

@loop:   mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         xor   eax, ecx
         jne   @differ
@same:   add   ebx, 4
         jl    @loop

@len:    pop   eax
         pop   ebx
         pop   edi
         ret

@loop2:  mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         xor   eax, ecx
         je    @same
@differ: test  eax, $DFDFDFDF  //$00 or $20
         jnz   @find
         add   eax, eax        //$00 or $40
         add   eax, eax        //$00 or $80
         test  eax, ecx
         jnz   @find
         and   ecx, $5F5F5F5F  //$41..$5A
         add   ecx, $3F3F3F3F  //$80..$99
         and   ecx, $7F7F7F7F  //$00..$19
         add   ecx, $66666666  //$66..$7F
         test  ecx, eax
         jnz   @find
         add   ebx, 4
         jl    @loop2

@len2:   pop   eax
         pop   ebx
         pop   edi
         ret

@loop3:  add   ebx, 1
         jge   @len2
@find:   movzx eax, [ebx+edi]
         movzx ecx, [ebx+edx]
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @upa
         sub   eax, 'a'-'A'
@upa:    cmp   cl, 'z'-'a'
         ja    @upc
         sub   ecx, 'a'-'A'
@upc:    sub   eax, ecx
         jz    @loop3

@found:  pop   ecx
         pop   ebx
         pop   edi
end;

//TODO: DONT Use this :BUG in it!!
function CompareTextEx_Sha_Pas_5(const S1, S2: PChar; const Len: Integer): integer;
var
  c1, c2, d, q, save: integer;
  p: pIntegerArray;
label
  past, find;
type 
  TDWordArray = array [0..3] of byte;
begin;
  c1:=0;
  c2:=0;
  if S1 <> nil then c1:=Len;
  if S2 <> nil then c2:=Len;

  q := c1;
  c1:= c1-c2;
  if c1>0 then q:=c2;                      //q = min length
  if q<=0 then 
  begin
    Result:=c1;
    exit;
  end;
  q:=q+integer(S1);
  p:=Pointer(S2);
  save:=c1;                    //save result for equal data
  d:=(Integer(S1)-Integer(S2)) shr 2;                 //d = distance(s1-s2) div 4

  repeat;
    c1:=p[d];                            //dword from s1
    c2:=p[0];                              //dword from s2
    inc(integer(p),4);
    c1:=c1 xor c2;
    if c1<>0 then begin;                   //test the difference
      //all bits of each byte must be 0, except bit5 (weight $20)
      if (c1 and integer($DFDFDFDF))<>0 then goto find;

      //bit5 can be 1 for letters only
      c1:=c1 + c1;                         //$00 or $40
      c1:=c1 + c1;                         //$00 or $80
      if (c1 and c2)<>0 then goto find;    //if not letter
      c2:=c2 and $5F5F5F5F;                //$41..$5A
      c2:=c2   + $3F3F3F3F;                //$80..$99
      c2:=c2 and $7F7F7F7F;                //$00..$19
      c2:=c2   + $66666666;                //$66..$7F
      if (c1 and c2)<>0 then goto find;    //if not letter
      end;
    until cardinal(p)>=cardinal(q);
past:
  Result:=save;
  exit;

  repeat; //find mismatched characters
    if cardinal(p)>=cardinal(q+4) then goto past;
find:
    c1:=p[d];
    c2:=p[0];
    inc(integer(p));
    
    {
    cardinal(c1) := cardinal(c1) - $61616161;
    cardinal(c2) := cardinal(c2) - $61616161;
    if cardinal(c1) <= $7A7A7A7A - $61616161 then c1:=c1 -($61616161 - $41414141); //$41 = 'A';  $61 = 'a', $7A = 'z'
    if cardinal(c2) <= $7A7A7A7A - $61616161 then c2:=c2 -($61616161 - $41414141);
    //}
    
    TDWordArray(c1)[0]:=TDWordArray(c1)[0]-ord('a');
    TDWordArray(c2)[0]:=TDWordArray(c2)[0]-ord('a');
    if cardinal(TDWordArray(c1)[0])<=ord('z')-ord('a') then TDWordArray(c1)[0]:=TDWordArray(c1)[0]-(ord('a')-ord('A'));
    if cardinal(TDWordArray(c2)[0])<=ord('z')-ord('a') then TDWordArray(c2)[0]:=TDWordArray(c2)[0]-(ord('a')-ord('A'));
    

    TDWordArray(c1)[1]:=TDWordArray(c1)[1]-ord('a');
    TDWordArray(c2)[1]:=TDWordArray(c2)[1]-ord('a');
    if cardinal(TDWordArray(c1)[1])<=ord('z')-ord('a') then TDWordArray(c1)[1]:=TDWordArray(c1)[1]-(ord('a')-ord('A'));
    if cardinal(TDWordArray(c2)[1])<=ord('z')-ord('a') then TDWordArray(c2)[1]:=TDWordArray(c2)[1]-(ord('a')-ord('A'));

    TDWordArray(c1)[2]:=TDWordArray(c1)[2]-ord('a');
    TDWordArray(c2)[2]:=TDWordArray(c2)[2]-ord('a');
    if cardinal(TDWordArray(c1)[2])<=ord('z')-ord('a') then TDWordArray(c1)[2]:=TDWordArray(c1)[2]-(ord('a')-ord('A'));
    if cardinal(TDWordArray(c2)[2])<=ord('z')-ord('a') then TDWordArray(c2)[2]:=TDWordArray(c2)[2]-(ord('a')-ord('A'));

    TDWordArray(c1)[3]:=TDWordArray(c1)[3]-ord('a');
    TDWordArray(c2)[3]:=TDWordArray(c2)[3]-ord('a');
    if cardinal(TDWordArray(c1)[3])<=ord('z')-ord('a') then TDWordArray(c1)[3]:=TDWordArray(c1)[3]-(ord('a')-ord('A'));
    if cardinal(TDWordArray(c2)[3])<=ord('z')-ord('a') then TDWordArray(c2)[3]:=TDWordArray(c2)[3]-(ord('a')-ord('A'));
    //}
  until c1<>c2;
  Result:=cardinal(c1)-cardinal(c2);
end;

Initialization
{$IFDEF FastcodeCPUID}
  case FastcodeTarget of
           fctIA32: begin
                      {$IFDEF FastcodeSizePenalty}
                        FastcodeCompareTextEx := FastcodeCompareTextExIA32SizePenalty;
                      {$ELSE}
                        FastcodeCompareTextEx := FastcodeCompareTextExIA32;
                      {$ENDIF}
                    end;
            fctMMX: begin
                      FastcodeCompareTextEx := FastcodeCompareTextExMMX;
                    end;
            fctSSE: begin
                      {$IFDEF FastcodeSizePenalty}
                        FastcodeCompareTextEx := FastcodeCompareTextExSSESizePenalty;
                      {$ELSE}
                        FastcodeCompareTextEx := FastcodeCompareTextExSSE;
                      {$ENDIF}
                    end;
           fctSSE2: begin
                      FastcodeCompareTextEx := FastcodeCompareTextExSSE2;
                    end;
            fctPMD: begin
                      FastcodeCompareTextEx := FastcodeCompareTextExPMD;
                    end;
            fctPMY: begin
                      FastcodeCompareTextEx := FastcodeCompareTextExPMY;
                    end;
            fctP4N: begin
                      FastcodeCompareTextEx := FastcodeCompareTextExP4N;
                    end;
            fctP4R: begin
                      FastcodeCompareTextEx := FastcodeCompareTextExP4R;
                    end;
          fctAmd64: begin
                      FastcodeCompareTextEx := FastcodeCompareTextExAmd64;
                    end;
     fctAmd64_SSE3: begin
                      FastcodeCompareTextEx := FastcodeCompareTextExAmd64_SSE3;
                    end;

  end;

{$ENDIF}

{$IFDEF FastcodeIA32}
  {$IFDEF FastcodeSizePenalty}
    FastcodeCompareTextEx := FastcodeCompareTextExIA32SizePenalty;
  {$ELSE}
    FastcodeCompareTextEx := FastcodeCompareTextExIA32;
  {$ENDIF}
{$ENDIF}

{$IFDEF FastcodeMMX}
  FastcodeCompareTextEx := FastcodeCompareTextExMMX;
{$ENDIF}

{$IFDEF FastcodeSSE}
  {$IFDEF FastcodeSizePenalty}
    FastcodeCompareTextEx := FastcodeCompareTextExSSESizePenalty;
  {$ELSE}
    FastcodeCompareTextEx := FastcodeCompareTextExSSE;
  {$ENDIF}
{$ENDIF}

{$IFDEF FastcodeSSE2}
  FastcodeCompareTextEx := FastcodeCompareTextExSSE2;
{$ENDIF}

(* //Bug in pascal version
{$IFDEF FastcodePascal}
  {$IFDEF FastcodeSizePenalty}
    FastcodeCompareTextEx := FastcodeCompareTextExPascalSizePenalty;
  {$ELSE}
    FastcodeCompareTextEx := FastcodeCompareTextExPascal;
  {$ENDIF}
{$ENDIF}
*)
end.
