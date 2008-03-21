
{ Summary Various character and string routines (searching, testing and transforming).}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.7 $

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
    * The Original Code is $RCSfile: uMeStrUtils.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Project JEDI Code Library (JCL), 
    * Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.

    * Contributor(s):
   Alexander Radchenko                                                                            
   Andreas Hausladen                                                                              
   Anthony Steele                                                                                 
   Azret Botash                                                                                   
   Barry Kelly                                                                                    
   Huanlin Tsai                                                                                   
   Jack N.A. Bakker                                                                               
   Jean-Fabien Connault                                                                           
   John C Molyneux                                                                                
   Leonard Wennekers                                                                              
   Martin Kimmings                                                                                
   Martin Kubecka                                                                                 
   Massimo Maria Ghisalberti                                                                      
   Matthias Thoma (mthoma)                                                                        
   Michael Winter                                                                                 
   Nick Hodges                                                                                    
   Olivier Sannier                                                                                
   Pelle F. S. Liljendal                                                                          
   Petr Vones                                                                                     
   Rik Barker (rikbarker)                                                                         
   Robert Lee                                                                                     
   Robert Marquardt                                                                               
   Robert Rossmair (rrossmair)                                                                    
}
unit uMeStrUtils;

{$I Setting.inc}
  
interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils
  //, Classes
  , uMeObject
  ;

type
  EStringError = Exception;
  //only result = true to continue.
  TOnDoFileEvent = function (const aFileName: string): Boolean;

resourcestring
  RsBlankSearchString       = 'Search string cannot be blank';
  RsInvalidEmptyStringItem  = 'String list passed to StringsToMultiSz cannot contain empty strings.';
  RsNumericConstantTooLarge = 'Numeric constant too large.';
  RsFormatException         = 'Format exception';
  RsDotNetFormatNullFormat  = 'Format string is null';
  RsArgumentIsNull          = 'Argument %d is null';
  RsDotNetFormatArgumentNotSupported = 'Argument type of %d is not supported';
  RsArgumentOutOfRange      = 'Argument out of range';

const
  {$IFDEF MSWINDOWS}
  DriveLetters     = ['a'..'z', 'A'..'Z'];
  PathDevicePrefix = '\\.\';
  PathUncPrefix    = '\\';
  {$ENDIF MSWINDOWS}

// Character constants and sets

const
  // Misc. often used character definitions
  AnsiNull           = Char(#0);
  AnsiSoh            = Char(#1);
  AnsiStx            = Char(#2);
  AnsiEtx            = Char(#3);
  AnsiEot            = Char(#4);
  AnsiEnq            = Char(#5);
  AnsiAck            = Char(#6);
  AnsiBell           = Char(#7);
  AnsiBackspace      = Char(#8);
  AnsiTab            = Char(#9);
  AnsiVerticalTab    = Char(#11);
  AnsiFormFeed       = Char(#12);
  AnsiSo             = Char(#14);
  AnsiSi             = Char(#15);
  AnsiDle            = Char(#16);
  AnsiDc1            = Char(#17);
  AnsiDc2            = Char(#18);
  AnsiDc3            = Char(#19);
  AnsiDc4            = Char(#20);
  AnsiNak            = Char(#21);
  AnsiSyn            = Char(#22);
  AnsiEtb            = Char(#23);
  AnsiCan            = Char(#24);
  AnsiEm             = Char(#25);
  AnsiEndOfFile      = Char(#26);
  AnsiEscape         = Char(#27);
  AnsiFs             = Char(#28);
  AnsiGs             = Char(#29);
  AnsiRs             = Char(#30);
  AnsiUs             = Char(#31);
  AnsiSpace          = Char(' ');
  AnsiComma          = Char(',');
  AnsiBackslash      = Char('\');
  AnsiForwardSlash   = Char('/');

  AnsiDoubleQuote = Char('"');
  AnsiSingleQuote = Char('''');
  AnsiLineFeed       = AnsiChar(#10);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);

// Misc. character sets

  AnsiWhiteSpace             = [AnsiTab, AnsiLineFeed, AnsiVerticalTab,
    AnsiFormFeed, AnsiCarriageReturn, AnsiSpace];
  AnsiSigns                  = ['-', '+'];
  // (rom) too basic for JclStrings
  {$IFDEF MSWINDOWS}
  AnsiLineBreak = AnsiCrLf;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  AnsiLineBreak = AnsiLineFeed;
  {$ENDIF UNIX}

  AnsiUppercaseLetters       = ['A'..'Z'];
  AnsiLowercaseLetters       = ['a'..'z'];
  AnsiLetters                = ['A'..'Z', 'a'..'z'];
  AnsiDecDigits              = ['0'..'9'];
  AnsiOctDigits              = ['0'..'7'];
  AnsiHexDigits              = ['0'..'9', 'A'..'F', 'a'..'f'];
  AnsiValidIdentifierLetters = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];

const
  // CharType return values
  C1_UPPER  = $0001; // Uppercase
  C1_LOWER  = $0002; // Lowercase
  C1_DIGIT  = $0004; // Decimal digits
  C1_SPACE  = $0008; // Space characters
  C1_PUNCT  = $0010; // Punctuation
  C1_CNTRL  = $0020; // Control characters
  C1_BLANK  = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA  = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic

  {$IFDEF MSWINDOWS}
  {$IFDEF SUPPORTS_EXTSYM}
  {$EXTERNALSYM C1_UPPER}
  {$EXTERNALSYM C1_LOWER}
  {$EXTERNALSYM C1_DIGIT}
  {$EXTERNALSYM C1_SPACE}
  {$EXTERNALSYM C1_PUNCT}
  {$EXTERNALSYM C1_CNTRL}
  {$EXTERNALSYM C1_BLANK}
  {$EXTERNALSYM C1_XDIGIT}
  {$EXTERNALSYM C1_ALPHA}
  {$ENDIF SUPPORTS_EXTSYM}
  {$ENDIF MSWINDOWS}


{可以递归调用处理子目录以及旗下的文件}
function ProcessFolder(const aFolder, aPatterns: string; const OnFile: TOnDoFileEvent): Boolean;
{
only process the files in the aPath
  @param aPatterns  the search patterns: *.dpr;*.dfm
}
function ProcessFiles(const aPath:string; aPatterns: string; const OnFile: TOnDoFileEvent): Boolean;

function ExtractFileBaseName(const aFileName: string): string;

//come from JCL
function PathIsDiskDevice(const Path: string): Boolean;
function PathIsUNC(const Path: string): Boolean;
function PathIsAbsolute(const Path: string): Boolean;

{ Summary Trim and lowercase the string.}
{ Description
  @Param aText the string to trim
  @Param IgnoreCase  Ignore this string Case states. the default is False.
  @Param IgnoreBlanks  Ignore the blanks in this string. the default is False.
  Return the result string.
}
function StrTrim(const aText: string; const IgnoreCase: boolean = False; const IgnoreBlanks: boolean = False): string;

function Fetch(var S: string; const aDelim: string; const aDelete: Boolean = True): string;

function StrToHex(const Source: string): string;
procedure StrToStrings(S, Sep: string; const List: PMeStrings; const AllowEmptyString: Boolean = True);

function StrFind(const Substr, S: string; const Index: Integer = 1): Integer;
function StrSearch(const Substr, S: string; const Index: Integer = 1): Integer;
//StrMatches: *, ? matches
function StrMatches(const Substr, S: AnsiString; const Index: Integer = 1): Boolean;
//Pos from right
function RPos(const SubStr, S: string): Integer;
{This searches an array of string for an occurance of SearchStr}
function PosInStrArray(const SearchStr: string; const Contents: array of string; const CaseSensitive: Boolean = True): Integer;

//convert the '#10' or '#$FF' to Chr(10) or Chr($FF);
function StrDelphiUnEscape(const S: string): string;

// String Extraction
function StrAfter(const SubStr, S: string): string;
function StrBefore(const SubStr, S: string): string;
function StrBetween(const S: string; const Start, Stop: Char): string;
function StrChopRight(const S: string; N: Integer): string;
function StrLeft(const S: string; Count: Integer): string;
function StrMid(const S: string; Start, Count: Integer): string;
function StrRestOf(const S: string; N: Integer): string;
function StrRight(const S: string; Count: Integer): string;

function TextIsSame(const A1, A2: string): Boolean;

// Character Transformation Routines
function CharHex(const C: Char): Byte;
function CharLower(const C: Char): Char; {$IFDEF CLR} inline; {$ENDIF}
function CharUpper(const C: Char): Char; {$IFDEF CLR} inline; {$ENDIF}

// Character Test Routines
function CharIsUpper(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsLower(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsDigit(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsNumberChar(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharPosInSet(const AString: string; const ACharPos: Integer; const ASet: String): Integer;{$IFDEF CLR} inline; {$ENDIF}
function CharIsInSet(const AString: string; const ACharPos: Integer; const ASet:  String): Boolean;{$IFDEF CLR} inline; {$ENDIF}
function CharIsInEOL(const AString: string; const ACharPos: Integer): Boolean;{$IFDEF CLR} inline; {$ENDIF}
function IsLeadChar(ACh : Char):Boolean;
function CharRange(const AMin, AMax : Char): String;

// String Test Routines
function StrIsInteger(const S: string): Boolean;

{$IFDEF COMPILER5} // missing Delphi 5 functions
function TryStrToInt(const S: string; out Value: Integer): Boolean;
function TryStrToInt64(const S: string; out Value: Int64): Boolean;
function TryStrToFloat(const S: string; out Value: Extended): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Double): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Single): Boolean; overload;
function TryStrToCurr(const S: string; out Value: Currency): Boolean;
{$ENDIF COMPILER5}

Var
  gAppPath: String;

implementation

{$IFNDEF CLR}
type
  TAnsiStrRec = packed record
    AllocSize: Longint;
    RefCount: Longint;
    Length: Longint;
  end;

const
  AnsiStrRecSize  = SizeOf(TAnsiStrRec);     // size of the string header rec
  AnsiCharCount   = Ord(High(Char)) + 1; // # of chars in one set
  AnsiLoOffset    = AnsiCharCount * 0;       // offset to lower case chars
  AnsiUpOffset    = AnsiCharCount * 1;       // offset to upper case chars
  AnsiReOffset    = AnsiCharCount * 2;       // offset to reverse case chars
  AnsiAlOffset    = 12;                      // offset to AllocSize in StrRec
  AnsiRfOffset    = 8;                       // offset to RefCount in StrRec
  AnsiLnOffset    = 4;                       // offset to Length in StrRec
  AnsiCaseMapSize = AnsiCharCount * 3;       // # of chars is a table

var
  AnsiCaseMap: array [0..AnsiCaseMapSize - 1] of Char; // case mappings
  AnsiCaseMapReady: Boolean = False;         // true if case map exists
  AnsiCharTypes: array [Char] of Word;

procedure LoadCharTypes;
var
  CurrChar: Char;
  CurrType: Word;
  {$IFDEF CLR}
  Category: System.Globalization.UnicodeCategory;
  {$ENDIF CLR}
begin
  for CurrChar := Low(Char) to High(Char) do
  begin
    {$IFDEF MSWINDOWS}
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(Char), CurrType);
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    CurrType := 0;
    if isupper(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_UPPER;
    if islower(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_LOWER;
    if isdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_DIGIT;
    if isspace(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_SPACE;
    if ispunct(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_PUNCT;
    if iscntrl(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_CNTRL;
    if isblank(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_BLANK;
    if isxdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_XDIGIT;
    if isalpha(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_ALPHA;
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF LINUX}
    AnsiCharTypes[CurrChar] := CurrType;
    {$IFNDEF CHAR_TYPES_INITIALIZED}
    Implement case map initialization here
    {$ENDIF ~CHAR_TYPES_INITIALIZED}
  end;
end;

procedure LoadCaseMap;
var
  CurrChar, UpCaseChar, LoCaseChar, ReCaseChar: Char;
begin
  if not AnsiCaseMapReady then
  begin
    for CurrChar := Low(Char) to High(Char) do
    begin
      {$IFDEF MSWINDOWS}
      LoCaseChar := CurrChar;
      UpCaseChar := CurrChar;
      Windows.CharLowerBuff(@LoCaseChar, 1);
      Windows.CharUpperBuff(@UpCaseChar, 1);
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      LoCaseChar := Char(tolower(Byte(CurrChar)));
      UpCaseChar := Char(toupper(Byte(CurrChar)));
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF LINUX}
      {$IFNDEF CASE_MAP_INITIALIZED}
      Implement case map initialization here
      {$ENDIF ~CASE_MAP_INITIALIZED}
      if CharIsUpper(CurrChar) then
        ReCaseChar := LoCaseChar
      else
        if CharIsLower(CurrChar) then
          ReCaseChar := UpCaseChar
        else
          ReCaseChar := CurrChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiLoOffset] := LoCaseChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiUpOffset] := UpCaseChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiReOffset] := ReCaseChar;
    end;
    AnsiCaseMapReady := True;
  end;
end;
{$ENDIF}

function RPos(const SubStr, S: string): Integer;
begin
  Result := Length(S) - Length(SubStr)+1;
  while Result > 0 do
  begin
    {$IFDEF MBCS_SUPPORT}
    if ByteType(s, Result) <> mbSingleByte then
      while (ByteType(s, Result) <> mbLeadByte) and (Result>1) do
        Dec(Result);
    if Result < 0 then exit; 
    {$ENDIF}
    if CompareMem(PChar(@s[Result]), PChar(SubStr), Length(SubStr)) then exit;
   
    Dec(Result);
  end;
  Result := -1;
end;

function StrDelphiUnEscape(const S: string): string;
var
  i: Integer;
begin
  i := 1;
  Result := '';
  while i <= Length(S) do
  begin
    {$IFDEF MBCS_SUPPORT}
      while (ByteType(s, i) <> mbSingleByte) do
      begin
        Result := Result + s[i];
        inc(i);
        if (i > Length(S)) then break;
      end;
    {$ENDIF}
    if (i+2 <= Length(s)) and (s[i] = '#') then
    begin
      Inc(i);
      {$IFDEF MBCS_SUPPORT}
      if (ByteType(s, i) = mbLeadByte) then
      begin
        continue;
      end;
      {$ENDIF}
      if (s[i] = '$') and (i+2 <= Length(s)) and (s[i+1] in AnsiHexDigits) and (s[i+2] in AnsiHexDigits) then
      begin
        Result := Result + Chr(StrToInt(Copy(s, i, 3)));
        Inc(i, 3);
      end
      else if (s[i] in AnsiDecDigits) and (s[i+1] in AnsiDecDigits) then
      begin
        Result := Result + Chr(StrToInt(Copy(s, i, 2)));
        Inc(i, 2);
      end;
    end
    else begin
      Result := Result + s[i];
      Inc(i);
    end;
  end;
end;

function Fetch(var S: string; const aDelim: string; const aDelete: Boolean): string;
var
  I: Integer;
begin
  I := AnsiPos(aDelim, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    if aDelete then
      Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    if aDelete then
      S := '';
  end;
end;

procedure StrToStrings(S, Sep: string; const List: PMeStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  Left: string;
begin
  Assert(List <> nil);
  //List.BeginUpdate;
  //try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if S <> '' then
      List.Add(S);  // Ignore empty strings at the end.
  //finally
    //List.EndUpdate;
  //end;
end;


function StrToHex(const Source: string): string;
var
  Index: Integer;
  C, L, N: Integer;
  BL, BH: Byte;
  S: string;
  {$IFDEF CLR}
  sb: StringBuilder;
  {$ENDIF CLR}
begin
  {$IFDEF CLR}
  sb := StringBuilder.Create;
  {$ELSE}
  Result := '';
  {$ENDIF CLR}
  if Source <> '' then
  begin
    S := Source;
    L := Length(S);
    if Odd(L) then
    begin
      S := '0' + S;
      Inc(L);
    end;
    Index := 1;
    {$IFDEF CLR}
    sb.Length := L div 2;
    {$ELSE}
    SetLength(Result, L div 2);
    {$ENDIF CLR}
    C := 1;
    N := 1;
    while C <= L do
    begin
      BH := CharHex(S[Index]);
      Inc(Index);
      BL := CharHex(S[Index]);
      Inc(Index);
      Inc(C, 2);
      if (BH = $FF) or (BL = $FF) then
      begin
        Result := '';
        Exit;
      end;
      {$IFDEF CLR}
      sb[N] :=
      {$ELSE}
      Result[N] :=
      {$ENDIF CLR}
        Char((BH shl 4) + BL);
      Inc(N);
    end;
  end;
  {$IFDEF CLR}
  Result := sb.ToString();
  {$ENDIF CLR}
end;

{$IFDEF CLR}
function StrFind(const Substr, S: string; const Index: Integer): Integer;
begin
  Result := System.String(S).ToLower().IndexOf(System.String(SubStr).ToLower(), Index - 1) + 1;
end;
{$ELSE}
function StrFind(const Substr, S: string; const Index: Integer): Integer; assembler;
const
   SearchChar: Byte = 0;
   NumberOfChars: Integer = 0;
asm
        // if SubStr = '' then  Return := 0;

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        // if Str = '' then  Return := 0;

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // Index := Index - 1; if Index < 0 then Return := 0;

        DEC     ECX
        JL      @@IndexIsSmall

        // EBX will hold the case table, ESI pointer to Str, EDI pointer
        // to Substr and - # of chars in Substr to compare

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // set the string pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the Index in EDX

        MOV     EDX, ECX

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        // save the address of Str to compute the result

        PUSH    ESI

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // #positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // # of chars in Substr to compare

        MOV     NumberOfChars, EBX

        // point Str to Index'th char

        ADD     ESI, EDX

        // load case map into EBX, and clear EAX

        LEA     EBX, AnsiCaseMap
        XOR     EAX, EAX
        XOR     EDX, EDX

        // bring the first char out of the Substr and point Substr to the next char

        MOV     DL, [EDI]
        INC     EDI

        // lower case it

        MOV     DL, [EBX + EDX]
        MOV     SearchChar, DL

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of string.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current char from the string, and point Str to the next one

        MOV     AL, [ESI]
        INC     ESI


        // lower case current char

        MOV     AL, [EBX + EAX]

        // does current char match primary search char? if not, go back to the main loop

        CMP     AL, SearchChar
        JNE     @@FindNext

@@Compare:

        // # of chars in Substr to compare

        MOV     EDX, NumberOfChars

@@CompareNext:

        // dec loop counter and check if we reached the end. If yes then we found it

        DEC     EDX
        JL      @@Found

        // get the chars from Str and Substr, if they are equal then continue comparing

        MOV     AL, [ESI + EDX]
        CMP     AL, [EDI + EDX]
        JE      @@CompareNext

        // otherwise try the reverse case. If they still don't match go back to the Find loop

        MOV     AL, [EBX + EAX + AnsiReOffset]
        CMP     AL, [EDI + EDX]
        JNE     @@FindNext

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        // not found it, clear the result

        XOR     EAX, EAX
        POP     ESI
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        // clear the result

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;
{$ENDIF CLR}

{$IFDEF CLR}
function StrSearch(const Substr, S: string; const Index: Integer): Integer;
begin
  Result := System.String(S).IndexOf(SubStr, Index - 1) + 1;
end;
{$ELSE}
function StrSearch(const Substr, S: string; const Index: Integer): Integer; assembler;
asm
        // make sure that strings are not null

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // limit index to satisfy 1 <= index, and dec it

        DEC     ECX
        JL      @@IndexIsSmall

        // ebp will hold # of chars in Substr to compare, esi pointer to Str,
        // edi pointer to Substr, ebx primary search char

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // set the string pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the (Index - 1) in edx

        MOV     EDX, ECX

        // save the address of Str to compute the result

        PUSH    ESI

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // # of positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // point Str to Index'th char

        ADD     ESI, EDX

        // # of chars in Substr to compare

        MOV     EBP, EBX

        // clear EAX & ECX (working regs)

        XOR     EAX, EAX
        XOR     EBX, EBX

        // bring the first char out of the Substr, and
        // point Substr to the next char

        MOV     BL, [EDI]
        INC     EDI

        // jump into the loop

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of string.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current char from the string, and /point Str to the next one.
        MOV     AL, [ESI]
        INC     ESI

        // does current char match primary search char? if not, go back to the main loop

        CMP     AL, BL
        JNE     @@FindNext

        // otherwise compare SubStr

@@Compare:

        // move # of char to compare into edx, edx will be our compare loop counter.

        MOV     EDX, EBP

@@CompareNext:

        // check if we reached the end of Substr. If yes we found it.

        DEC     EDX
        JL      @@Found

        // get last chars from Str and SubStr and compare them,
        // if they don't match go back to out main loop.

        MOV     AL, [EDI+EDX]
        CMP     AL, [ESI+EDX]
        JNE     @@FindNext

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result and exit.

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:
        // not found it, clear result and exit.

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:
        // clear result and exit.

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;
{$ENDIF CLR}

//=== String Extraction ======================================================

function StrAfter(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function StrBefore(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;


function StrBetween(const S: string; const Start, Stop: Char): string;
var
  PosStart, PosEnd: Integer;
  L: Integer;
begin
  PosStart := Pos(Start, S);
  PosEnd := StrSearch(Stop, S, PosStart+1);  // PosEnd has to be after PosStart.

  if (PosStart > 0) and (PosEnd > PosStart) then
  begin
    L := PosEnd - PosStart;
    Result := Copy(S, PosStart + 1, L - 1);
  end
  else
    Result := '';
end;

function StrChopRight(const S: string; N: Integer): string;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLeft(const S: string; Count: Integer): string;
begin
  Result := Copy(S, 1, Count);
end;

function StrMid(const S: string; Start, Count: Integer): string;
begin
  Result := Copy(S, Start, Count);
end;

function StrRestOf(const S: string; N: Integer ): string;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrRight(const S: string; Count: Integer): string;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

function StrIsInteger(const S: string): Boolean;
var
  i: Integer;
begin
  Result  := S <> '';
  if Result then
  begin
    if (s[1] = '$') then
    begin
      if Length(s) > 1 then
      begin
        i := 2;
        while (i <= Length(s)) and (s[i] in AnsiHexDigits) do inc(i);
        dec(i);
        Result := (i = Length(s)) and (s[i] in AnsiHexDigits);
      end
      else
      begin
        i := 1;
        while (i <= Length(s)) and (s[i] in AnsiDecDigits) do inc(i);
        dec(i);
        Result := (i = Length(s)) and (s[i] in AnsiDecDigits);
      end
    end
    else
      for i := 1 to Length(S) do
  end;
    
end;

//=== Character Transformation Routines ======================================

function CharHex(const C: Char): Byte;
begin
  Result := $FF;
  if C in AnsiDecDigits then
    Result := Ord(CharUpper(C)) - Ord('0')
  else
  begin
    if C in AnsiHexDigits then
      Result := Ord(CharUpper(C)) - (Ord('A')) + 10;
  end;
end;

function CharIsLower(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsLower(C);
  {$ELSE}
  Result := (AnsiCharTypes[C] and C1_LOWER) <> 0;
  {$ENDIF CLR}
end;

function CharIsUpper(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsUpper(C);
  {$ELSE}
  Result := (AnsiCharTypes[C] and C1_UPPER) <> 0;
  {$ENDIF CLR}
end;

function CharIsDigit(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsDigit(C);
  {$ELSE}
  Result := (AnsiCharTypes[C] and C1_DIGIT) <> 0;
  {$ENDIF CLR}
end;

function CharIsNumberChar(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsDigit(C) or
  {$ELSE}
  Result := ((AnsiCharTypes[C] and C1_DIGIT) <> 0) or
  {$ENDIF CLR}
    (C in AnsiSigns) or (C = DecimalSeparator);
end;

function CharUpper(const C: Char): Char;
begin
  {$IFDEF CLR}
  Result := System.Char.ToUpper(C);
  {$ELSE}
  Result := AnsiCaseMap[Ord(C) + AnsiUpOffset];
  {$ENDIF CLR}
end;

function CharLower(const C: Char): Char;
begin
  {$IFDEF CLR}
  Result := System.Char.ToLower(C);
  {$ELSE}
  Result := AnsiCaseMap[Ord(C) + AnsiLoOffset];
  {$ENDIF CLR}
end;

function StrMatches(const Substr, S: AnsiString; const Index: Integer): Boolean;
var
  StringPtr: PAnsiChar;
  PatternPtr: PAnsiChar;
  StringRes: PAnsiChar;
  PatternRes: PAnsiChar;
begin
  if SubStr = '' then
    raise EStringError.CreateRes(@RsBlankSearchString);

  Result := SubStr = '*';

  if Result or (S = '') then
    Exit;

  if (Index <= 0) or (Index > Length(S)) then
    raise EStringError.CreateRes(@RsArgumentOutOfRange);

  StringPtr := PAnsiChar(@S[Index]);
  PatternPtr := PAnsiChar(SubStr);
  StringRes := nil;
  PatternRes := nil;

  repeat
    repeat
      case PatternPtr^ of
        #0:
          begin
            Result := StringPtr^ = #0;
            if Result or (StringRes = nil) or (PatternRes = nil) then
              Exit;

            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
            Break;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ <> PatternPtr^ then
            begin
              if (StringRes = nil) or (PatternRes = nil) then
                Exit;
              StringPtr := StringRes;
              PatternPtr := PatternRes;
              Break;
            end
            else
            begin
              Inc(StringPtr);
              Inc(PatternPtr);
            end;
          end;
      end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
          begin
            Result := True;
            Exit;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            repeat
              if StringPtr^ = #0 then
                Exit;
              if StringPtr^ = PatternPtr^ then
                Break;
              Inc(StringPtr);
            until False;
            Inc(StringPtr);
            StringRes := StringPtr;
            Inc(PatternPtr);
            Break;
          end;
      end;
    until False;
  until False;
end;

function ExtractFileBaseName(const aFileName: string): string;
var
  i: integer;
begin
  i := RPos('.', aFileName);
  if i <= 0 then i := Length(aFileName);
  Result := Copy(aFileName, 1, i-1);
end;

function PathIsDiskDevice(const Path: string): Boolean;
{$IFDEF UNIX}
var
  FullPath: string;
  F: PIOFile;
  Buffer: array [0..255] of Char;
  MountEntry: TMountEntry;
  FsTypes: PMeStrings;

  procedure GetAvailableFileSystems(const List: PMeStrings);
  var
    F: TextFile;
    S: string;
  begin
    AssignFile(F, '/proc/filesystems');
    Reset(F);
    repeat
      Readln(F, S);
      if Pos('nodev', S) = 0 then // how portable is this ?
        List.Add(Trim(S));
    until Eof(F);
    List.Add('supermount');
    CloseFile(F);
  end;

begin
  Result := False;

  SetLength(FullPath, _POSIX_PATH_MAX);
  if realpath(PChar(Path), PChar(FullPath)) = nil then
    RaiseLastOSError;
  StrResetLength(FullPath);
  
  New(FsTypes, Create);
  try
    GetAvailableFileSystems(FsTypes);
    F := setmntent(_PATH_MOUNTED, 'r'); // PATH_MOUNTED is deprecated,
                                        // but PATH_MNTTAB is defective in Libc.pas
    try
      // get drives from mtab
      while not Result and (getmntent_r(F, MountEntry, Buffer, SizeOf(Buffer)) <> nil) do
        if FsTypes.IndexOf(MountEntry.mnt_type) <> -1 then
          Result := MountEntry.mnt_dir = FullPath;

    finally
      endmntent(F);
    end;
  finally
    FsTypes.Free;
  end;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
begin
  Result := Copy(Path, 1, Length(PathDevicePrefix)) = PathDevicePrefix;
end;
{$ENDIF MSWINDOWS}

function PathIsUNC(const Path: string): Boolean;

{$IFDEF MSWINDOWS}

const
  cUNCSuffix = '?\UNC';

var
  {$IFDEF CLR}
  Index, LenPath: Integer;
  {$ELSE}
  P: PChar;
  {$ENDIF}

  function AbsorbSeparator: Boolean;
  begin
    {$IFDEF CLR}
    Result := (Index <> 0) and (Path[Index] = PathDelim);
    if Result then
      Inc(Index);
    {$ELSE ~CLR}
    Result := (P <> nil) and (P^ = PathDelim);
    if Result then
      Inc(P);
    {$ENDIF ~CLR}
  end;

  function AbsorbMachineName: Boolean;
  var
    NonDigitFound: Boolean;
  begin
    // a valid machine name is a string composed of the set [a-z, A-Z, 0-9, -, _] but it may not
    // consist entirely out of numbers
    Result := True;
    NonDigitFound := False;
    {$IFDEF CLR}
    while (Index <= LenPath) and (Path[Index] <> PathDelim) do
    begin
      if AnsiChar(Path[Index]) in ['a'..'z', 'A'..'Z', '-', '_', '.'] then
      begin
        NonDigitFound := True;
        Inc(Index);
      end
      else
      if AnsiChar(Path[Index]) in AnsiDecDigits then
        Inc(Index)
      else
      begin
        Result := False;
        Break;
      end;
    end;
    {$ELSE ~CLR}
    while (P^ <> #0) and (P^ <> PathDelim) do
    begin
      if P^ in ['a'..'z', 'A'..'Z', '-', '_', '.'] then
      begin
        NonDigitFound := True;
        Inc(P);
      end
      else
      if P^ in AnsiDecDigits then
        Inc(P)
      else
      begin
        Result := False;
        Break;
      end;
    end;
    {$ENDIF ~CLR}
    Result := Result and NonDigitFound;
  end;

  function AbsorbShareName: Boolean;
  const
    InvalidCharacters =
      ['<', '>', '?', '/', ',', '*', '+', '=', '[', ']', '|', ':', ';', '"', '''']; //'
  begin
    // a valid share name is a string composed of a set the set !InvalidCharacters note that a
    // leading '$' is valid (indicates a hidden share)
    Result := True;
    {$IFDEF CLR}
    while (Index <= LenPath) and (Path[Index] <> '\') do
    begin
      if AnsiChar(Path[Index]) in InvalidCharacters then
      begin
        Result := False;
        Break;
      end;
      Inc(Index);
    end;
    {$ELSE ~CLR}
    while (P^ <> #0) and (P^ <> '\') do
    begin
      if P^ in InvalidCharacters then
      begin
        Result := False;
        Break;
      end;
      Inc(P);
    end;
    {$ENDIF ~CLR}
  end;

begin
  Result := Copy(Path, 1, Length(PathUncPrefix)) = PathUncPrefix;
  if Result then
  begin
    {$IFDEF CLR}
    Index := Length(PathUncPrefix);
    if Path.StartsWith(PathUncPrefix + cUNCSuffix) then
      Inc(Index, Length(cUNCSuffix))
    else
      Result := AbsorbSeparator and AbsorbMachineName;
    {$ELSE ~CLR}
    if Copy(Path, 1, Length(PathUncPrefix + cUNCSuffix)) = PathUncPrefix + cUNCSuffix then
      P := @Path[Length(PathUncPrefix + cUNCSuffix)]
    else
    begin
      P := @Path[Length(PathUncPrefix)];
      Result := AbsorbSeparator and AbsorbMachineName;
    end;
    {$ENDIF ~CLR}
    Result := Result and AbsorbSeparator;
    if Result then
    begin
      Result := AbsorbShareName;
      // remaining, if anything, is path and or filename (optional) check those?
    end;
  end;
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}

begin
  Result := False;
end;

{$ENDIF UNIX}

function PathIsAbsolute(const Path: string): Boolean;
{$IFDEF CLR}
begin
  Result := System.IO.Path.IsPathRooted(Path);
end;
{$ELSE ~CLR}
{$IFDEF MSWINDOWS}
var
  I: Integer;
{$ENDIF MSWINDOWS}
begin
  Result := False;
  if Path <> '' then
  begin
    {$IFDEF UNIX}
    Result := (Path[1] = PathDelim);
    {$ENDIF UNIX}
    {$IFDEF MSWINDOWS}
    if not PathIsUnc(Path) then
    begin
      I := 0;
      if PathIsDiskDevice(Path) then
        I := Length(PathDevicePrefix);
      Result := (Length(Path) > I + 2) and (Path[I + 1] in DriveLetters) and
        (Path[I + 2] = ':') and (Path[I + 3] = PathDelim);
    end
    else
      Result := True;
    {$ENDIF MSWINDOWS}
  end;
end;
{$ENDIF ~CLR}

function ProcessFiles(const aPath:string; aPatterns: string; const OnFile: TOnDoFileEvent): Boolean;
var 
  p: integer;
  SrchRec: TSearchRec;
  Pattern: string;
begin
  //WriteLn('ProcessFiles ' + aPath);
  Result := True;
  //first process files
  while aPatterns <> '' do
  begin
    P := Pos(';', aPatterns);
    if P > 0 then
    begin
      Pattern := Copy(aPatterns, 1, p-1);
      Delete(aPatterns, 1, p);
    end
    else begin
      Pattern := aPatterns;
      aPatterns := '';
    end;
    if FindFirst(aPath + Pattern, faAnyFile - faDirectory, SrchRec) = 0 then
    try
      Repeat
        //iStream := TFileStream.Create(SrchRec.Name, fmOpenRead);
        //WriteLn('ProcessFile ' + aPath + SrchRec.Name);
        if Assigned(OnFile) then Result := OnFile(aPath + SrchRec.Name);
        if not Result then break;
        //ProcessAFile(SrchRec.Name);
      until FindNext(SrchRec) <> 0;
    finally
      FindClose(SrchRec);
    end;//try
  end; //while
end;

//the aFolder must IncludeTrailingPathDelimiter!!
function ProcessFolder(const aFolder, aPatterns: string; const OnFile: TOnDoFileEvent): Boolean;
var
  DirSrchRec: TSearchRec;
begin
  Result := True;
  //aFolder := IncludeTrailingPathDelimiter(aFolder);
  if FindFirst(aFolder + '*.*', faDirectory, DirSrchRec) = 0 then
  try
    Repeat
      if (DirSrchRec.Name <> '.') and (DirSrchRec.Name <> '..') 
      and ((DirSrchRec.Attr and faDirectory) = faDirectory) then
      begin
        //WriteLn('Enter ' + aFolder + DirSrchRec.Name);
        //ChDir(DirSrchRec.Name);
        Result := ProcessFolder(aFolder + DirSrchRec.Name + PathDelim, aPatterns, OnFile);
        //if not Result then WriteLn('break on ' + aFolder);
        if not Result then Break;
        //ChDir('..');
      end;
    until FindNext(DirSrchRec) <> 0;
  finally
    FindClose(DirSrchRec);
  end;//try
  if Result then
    Result := ProcessFiles(aFolder, aPatterns, OnFile);
end;

function StrTrim(const aText: string; const IgnoreCase: boolean; const IgnoreBlanks: boolean): string;
var
  i, j, len: integer;
begin
  if IgnoreBlanks then
  begin
    i := 1;
    j := 1;
    len := length(aText);
    while i <= len do
    begin
      {$IFDEF MBCS_SUPPORTS}
      if ByteType(aText, i) = mbSingleByte then
      begin
      {$ENDIF}
        if not (aText[i] in [#9, #32]) then
        begin
          Result[j] := aText[i];
          inc(j);
        end;
      {$IFDEF MBCS_SUPPORTS}
      end 
      else begin
        Result[j] := aText[i];
        While ByteType(aText, i) <> mbTrailByte do
        begin
          inc(j);
          inc(i);
          Result[j] := aText[i];
        end;
      end;
      {$ENDIF}
      inc(i);
    end;
    SetLength(Result,j-1);
  end
  else
    Result := aText;
  if IgnoreCase then Result := AnsiLowerCase(Result);
end;

{$IFDEF COMPILER5} // missing Delphi 5 functions
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;

function TryStrToInt64(const S: string; out Value: Int64): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

function TryStrToFloat(const S: string; out Value: Double): Boolean;
var
  F: Extended;
begin
  Result := TryStrToFloat(S, F);
  if Result then
    Value := F;
end;

function TryStrToFloat(const S: string; out Value: Single): Boolean;
var
  F: Extended;
begin
  Result := TryStrToFloat(S, F);
  if Result then
    Value := F;
end;

function TryStrToCurr(const S: string; out Value: Currency): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvCurrency);
end;
{$ENDIF COMPILER5}

function TextIsSame(const A1, A2: string): Boolean;
begin
  {$IFDEF DOTNET}
  Result := System.String.Compare(A1, A2, True) = 0;
  {$ELSE}
  Result := AnsiCompareText(A1, A2) = 0;
  {$ENDIF}
end;

{This searches an array of string for an occurance of SearchStr}
function PosInStrArray(const SearchStr: string; const Contents: array of string; const CaseSensitive: Boolean = True): Integer;
begin
  for Result := Low(Contents) to High(Contents) do begin
    if CaseSensitive then begin
      if SearchStr = Contents[Result] then begin
        Exit;
      end;
    end else begin
      if TextIsSame(SearchStr, Contents[Result]) then begin
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

function CharPosInSet(const AString: string; const ACharPos: Integer; const ASet: String): Integer;
begin
  if ACharPos <= Length(AString) then begin
    Result := AnsiPos(AString[ACharPos], ASet);
  end else begin
    Result := 0;
  end;
end;

function CharIsInSet(const AString: string; const ACharPos: Integer; const ASet:  String): Boolean;
begin
  Result := CharPosInSet(AString, ACharPos, ASet) > 0;
end;

function CharIsInEOL(const AString: string; const ACharPos: Integer): Boolean;
begin
  Result := CharIsInSet(AString, ACharPos, AnsiCrLf);
end;

function IsLeadChar(ACh : Char):Boolean;
begin
  {$IFDEF DOTNET}
  Result := False;
  {$ELSE}
  Result := ACh in LeadBytes;
  {$ENDIF}
end;

function CharRange(const AMin, AMax : Char): String;
var
  i : Char;
{$IFDEF DOTNET}
  LSB : System.Text.StringBuilder;
begin
  LSB := System.Text.StringBuilder.Create;
  for i := AMin to AMax do begin
    LSB.Append(i);
  end;
  Result := LSB.ToString;
{$ELSE}
begin
  Result := '';
  for i := AMin to AMax do begin
    Result := Result + i;
  end;
{$ENDIF}
end;

initialization
  gAppPath := ExtractFilePath(ParamStr(0));
  LoadCharTypes;
  LoadCaseMap;
end.