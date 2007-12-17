{
   @author  Riceball LEE<riceballl@hotmail.com>
   @version $Revision: 1.5 $
}

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
 * The Original Code is $RCSfile: uMeDisAsmEngine.pas,v $.
 *
 * The Initial Developers of the Original Code are sars [HI-TECH].
 * Portions created by sars [HI-TECH]  are Copyright (C) 2003.
 * Portions created by Riceball LEE<riceballl@hotmail.com> is Copyright (C) 2006-2007
 * All rights reserved.
 *
 * Contributor(s):
 *  sars [HI-TECH]
 *
 *)
unit uMeDisAsmEngine;

{.$I MeSetting.inc}

interface

// get the 32 bit X86 Op code length
//return the -1 means error!
function GetX86OpCodeLength(const aOpCode: Pointer): Integer;

{modified from Catchy32 v1.6 - Length Disassembler Engine 32bit by sars [HI-TECH] 2003}
{ Description
;in:   esi - pointer to opcode
;out:  eax - opcode length or 0ffffffffh if error

==Brief description==
This Length-Disassembler was created for processing the executable x86 code instruction's lengths in 32bit mode. When we are saying "executable code" it means the CPU can execute the code. That is Catchy32 will NOT distinguish between the executable code and the data. There are a lot of opcodes that are not used as for now, but the place for them is reserved therefore they will be disassembled as well.

==Example==
db 0C0h,030h,001h - the instruction does not exist, but Catchy32 will disassemble it using the general disassembly rules

We used the tables with the flags 4 bits each and it allowed us to reduce the size of the engine but the beauty of the code is suffering now :-) The engine does not contain any absolute offsets, variables or external function calls.

==How to use==
To use Catcy32 you have to include it into your codes and put an offset to a disassembled instruction into ESI. If the disassembly was successful the return value is the instruction length in EAX. Otherwise EAX contains 0FFFFFFFFh. the ecx, EBX, EDI, ESI, EAX, EDX registers are affected, the other registers are not affected.
}
procedure _GetX86OpCodeLength;

implementation

function GetX86OpCodeLength(const aOpCode: Pointer): Integer;assembler;
asm
  push EDI
  push ESI
  push EBX
  mov edi, aOpCode  //backup the aOpCode
  mov esi, aOpCode
  call _GetX86OpCodeLength
  pop EBX
  pop ESI
  pop EDI
end;

procedure _GetX86OpCodeLength;
const
//Lentable equ $-@Table
  LenTable = 8 * 16;
  pref66h = 1;
  pref67h = 2;
{
;in:   esi - pointer to opcode
;out:  eax - opcode length or 0ffffffffh if error
}
asm
  xor ecx, ecx
@ExtFlags:
	xor eax, eax
	xor ebx, ebx
	cdq
	lodsb				//al <- opcode
	mov 	cl, al			//cl <- opcode
	cmp al, 0fh 		//Test on prefix 0Fh
	je	@ExtdTable
	cmp 	word ptr [esi-1], 20CDh //Test on VXD call
	jne 	@NormTable
	inc esi 		//If VXD call (int 20h), then command length is 6 bytes
	lodsd
	jmp 	@CalcLen

@ExtdTable:				//Load flags from extended table
	lodsb
	inc 	ah			//EAX=al+100h (100h/2 - lenght first table)

@NormTable:				//Load flags from normal table
	shr 	eax, 1			//Elements tables on 4 bits
	mov 	al, byte ptr [@Table+eax]

@CheckC1:
	jc	@IFC1
	shr eax, 4			//Get high 4-bits block if offset is odd, otherwise...

@IFC1:
	and 	eax, 0Fh			//...low
	xchg	eax, ebx			//EAX will be needed for other purposes

//--------------Opcode type checking---------------
@CheckFlags:
	cmp 	bl, 0Eh 		//Test on ErrorFlag
	je	@Error
	cmp 	bl, 0Fh 		//Test on PrefixFlag
	je	@Prefix
	or	ebx, ebx			//One byte command
	jz	@CalcLen
	btr ebx, 0			//Command with ModRM byte
	jc	@ModRM
	btr 	ebx, 1			//Test on imm8,rel8 etc flag
	jc	@incr1
	btr ebx, 2			//Test on ptr16 etc flag
	jc	@incr2

//-----imm16/32,rel16/32, etc types processing-----
@16_32:
	and 	bl, 11110111b			//Reset 16/32 sign

	cmp 	cl, 0A0h			//Processing group 0A0h-0A3h
	jb	@Check66h
	cmp cl, 0A3h
	ja	@Check66h
	test	ch, pref67h
	jnz @incr2
	jmp @incr4

@Check66h: 			//Processing other groups
	test	ch, pref66h
	jz	@incr4
	jmp 	@incr2

//---------------Prefixes processing---------------
@Prefix:
	cmp 	cl, 66h
	je	@SetFlag66h
	cmp 	cl, 67h
	jne 	@ExtFlags

@SetFlag67h:
	or	ch, pref67h
	jmp 	@ExtFlags

@SetFlag66h:
	or	ch, pref66h
	jmp 	@ExtFlags

//--------------ModR/M byte processing-------------
@ModRM:
	lodsb

@Check_0F6h_0F7h:				//Check on 0F6h and 0F7h groups
	cmp 	cl, 0F7h
	je	@GroupF6F7
	cmp 	cl, 0F6h
	jne 	@ModXX

@GroupF6F7:				//Processing groups 0F6h and 0F7h
	test	al, 00111000b
	jnz 	@ModXX
	test	cl, 00000001b
	jz	@incbt1
	test	ch, 1
	jnz @incbt2
	inc 	esi
	inc 	esi
@incbt2:	inc 	esi
@incbt1:	inc 	esi

@ModXX:					//Processing MOD bits
	mov 	edx, eax
	and 	al, 00000111b		//al <- only R/M bits
	test	dl, 11000000b		//Check MOD bits
	jz		@Mod00
	jp		@CheckFlags		//Or @Mod11
	js		@Mod10

@Mod01:
	test	ch, pref67h
	jnz 	@incr1 			//16-bit addressing
	cmp 	al, 4			//Check SIB
	je	@incr2
	jmp 	@incr1

@Mod00:
	test	ch, pref67h
	jz	@Mod00_32		//32-bit addressing
	cmp 	al, 6
	je	@incr2
	jmp 	@CheckFlags
@Mod00_32:
	cmp 	al, 4			//Check SIB
	jne 	@disp32

@SIB:					//Processing SIB byte
	lodsb
	and 	al, 00000111b
	cmp 	al, 5
	je	@incr4
	jmp 	@CheckFlags

@disp32:
	cmp 	al, 5
	je	@incr4
	jmp 	@CheckFlags

@Mod10:
	test	ch, pref67h
	jnz 	@incr2 		//16-bit addressing
	cmp 	al, 4			//Check SIB
	je	@incr5
	jmp 	@incr4

@incr5:	inc 	esi
@incr4:	inc 	esi
	inc 	esi
@incr2:	inc 	esi
@incr1:	inc 	esi
	jmp 	@CheckFlags

//-----------Command length calculation------------
@CalcLen:
	sub 	esi, edi
	cmp esi, 15
	ja	@Error
	mov 	eax, esi
	jmp 	@Exit

//----------------Setting the error----------------
@Error:
	xor 	eax, eax
	dec 	eax

//---------Restore the registers and exit----------
@Exit:
	ret
//-------------------------------------------------


@Table:
  //;	 01  23    45	67	 89   AB   CD	EF
  db 011h,011h,028h,000h,011h,011h,028h,000h//0Fh
  db 011h,011h,028h,000h,011h,011h,028h,000h//1Fh
  db 011h,011h,028h,0F0h,011h,011h,028h,0F0h//2Fh
  db 011h,011h,028h,0F0h,011h,011h,028h,0F0h//3Fh
  db 000h,000h,000h,000h,000h,000h,000h,000h//4Fh
  db 000h,000h,000h,000h,000h,000h,000h,000h//5Fh
  db 000h,011h,0FFh,0FFh,089h,023h,000h,000h//6Fh
  db 022h,022h,022h,022h,022h,022h,022h,022h//7Fh
  db 039h,033h,011h,011h,011h,011h,011h,011h//8Fh
  db 000h,000h,000h,000h,000h,0C0h,000h,000h//9Fh
  db 088h,088h,000h,000h,028h,000h,000h,000h//AFh
  db 022h,022h,022h,022h,088h,088h,088h,088h//BFh
  db 033h,040h,011h,039h,060h,040h,002h,000h//CFh
  db 011h,011h,022h,000h,011h,011h,011h,011h//DFh
  db 022h,022h,022h,022h,088h,0C2h,000h,000h//EFh
  db 0F0h,0FFh,000h,011h,000h,000h,000h,011h//FFh
////==============================================

////===============EXTENDED OPCODES===============
@TableEXT:
  ////	 01  23    45	67	 89   AB   CD	EF
  db 011h,011h,0E0h,000h,000h,0EEh,0E1h,003h//0Fh
  db 011h,011h,011h,011h,01Eh,0EEh,0EEh,0EEh//1Fh
  db 011h,011h,01Eh,01Eh,011h,011h,011h,011h//2Fh
  db 000h,000h,000h,0EEh,0EEh,0EEh,0EEh,0EEh//3Fh
  db 011h,011h,011h,011h,011h,011h,011h,011h//4Fh
  db 011h,011h,011h,011h,011h,011h,011h,011h//5Fh
  db 011h,011h,011h,011h,011h,011h,011h,011h//6Fh
  db 033h,033h,011h,010h,011h,011h,011h,011h//7Fh
  db 088h,088h,088h,088h,088h,088h,088h,088h//8Fh
  db 011h,011h,011h,011h,011h,011h,011h,011h//9Fh
  db 000h,001h,031h,011h,000h,001h,031h,011h//AFh
  db 011h,011h,011h,011h,0EEh,031h,011h,011h//BFh
  db 011h,031h,033h,031h,000h,000h,000h,000h//CFh
  db 0E1h,011h,011h,011h,011h,011h,011h,011h//DFh
  db 011h,011h,011h,011h,011h,011h,011h,011h//EFh
  db 0E1h,011h,011h,011h,011h,011h,011h,01Eh//FFh
//;==============================================
end;

end.