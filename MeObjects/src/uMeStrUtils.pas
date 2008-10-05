
{ Summary Various character and AnsiString routines (searching, testing and transforming).}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision$

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

{$I MeSetting.inc}
  
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
  TOnDoFileEvent = function (const aFileName: AnsiString): Boolean;

resourcestring
  RsBlankSearchString       = 'Search AnsiString cannot be blank';
  RsInvalidEmptyStringItem  = 'AnsiString list passed to StringsToMultiSz cannot contain empty strings.';
  RsNumericConstantTooLarge = 'Numeric constant too large.';
  RsFormatException         = 'Format exception';
  RsDotNetFormatNullFormat  = 'Format AnsiString is null';
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
  AnsiNull           = AnsiChar(#0);
  AnsiSoh            = AnsiChar(#1);
  AnsiStx            = AnsiChar(#2);
  AnsiEtx            = AnsiChar(#3);
  AnsiEot            = AnsiChar(#4);
  AnsiEnq            = AnsiChar(#5);
  AnsiAck            = AnsiChar(#6);
  AnsiBell           = AnsiChar(#7);
  AnsiBackspace      = AnsiChar(#8);
  AnsiTab            = AnsiChar(#9);
  AnsiVerticalTab    = AnsiChar(#11);
  AnsiFormFeed       = AnsiChar(#12);
  AnsiSo             = AnsiChar(#14);
  AnsiSi             = AnsiChar(#15);
  AnsiDle            = AnsiChar(#16);
  AnsiDc1            = AnsiChar(#17);
  AnsiDc2            = AnsiChar(#18);
  AnsiDc3            = AnsiChar(#19);
  AnsiDc4            = AnsiChar(#20);
  AnsiNak            = AnsiChar(#21);
  AnsiSyn            = AnsiChar(#22);
  AnsiEtb            = AnsiChar(#23);
  AnsiCan            = AnsiChar(#24);
  AnsiEm             = AnsiChar(#25);
  AnsiEndOfFile      = AnsiChar(#26);
  AnsiEscape         = AnsiChar(#27);
  AnsiFs             = AnsiChar(#28);
  AnsiGs             = AnsiChar(#29);
  AnsiRs             = AnsiChar(#30);
  AnsiUs             = AnsiChar(#31);
  AnsiSpace          = AnsiChar(' ');
  AnsiComma          = AnsiChar(',');
  AnsiBackslash      = AnsiChar('\');
  AnsiForwardSlash   = AnsiChar('/');

  AnsiDoubleQuote = AnsiChar('"');
  AnsiSingleQuote = AnsiChar('''');
  AnsiLineFeed       = AnsiChar(#10);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  AnsiAsterisk = AnsiChar(#$2A);
  AnsiQuestionMark = AnsiChar(#$3F);
  WideAsterisk = WideChar(#$002A);
  WideQuestionMark = WideChar(#$003F);


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
function ProcessFolder(const aFolder, aPatterns: AnsiString; const OnFile: TOnDoFileEvent): Boolean;
{
only process the files in the aPath
  @param aPatterns  the search patterns: *.dpr;*.dfm
}
function ProcessFiles(const aPath:AnsiString; aPatterns: AnsiString; const OnFile: TOnDoFileEvent): Boolean;

function ExtractFileBaseName(const aFileName: AnsiString): AnsiString;

//come from JCL
function PathIsDiskDevice(const Path: AnsiString): Boolean;
function PathIsUNC(const Path: AnsiString): Boolean;
function PathIsAbsolute(const Path: AnsiString): Boolean;

{ Summary Trim and lowercase the AnsiString.}
{ Description
  @Param aText the AnsiString to trim
  @Param IgnoreCase  Ignore this AnsiString Case states. the default is False.
  @Param IgnoreBlanks  Ignore the blanks in this AnsiString. the default is False.
  Return the result AnsiString.
}
function StrTrim(const aText: AnsiString; const IgnoreCase: boolean = False; const IgnoreBlanks: boolean = False): AnsiString;

//Fetch the left AnsiString until aDelim found.
function StrFetch(var S: AnsiString; const aDelim: AnsiString; const aDelete: Boolean = True): AnsiString;
//Fetch the right AnsiString until aDelim found.
function StrRFetch(var S: AnsiString; const aDelim: AnsiString; const aDelete: Boolean = True): AnsiString;

function StrToHex(const Source: AnsiString): AnsiString;
procedure StrToStrings(S, Sep: AnsiString; const List: PMeStrings; const AllowEmptyString: Boolean = True);

function StrFind(const Substr, S: AnsiString; const Index: Integer = 1): Integer;
function StrSearch(const Substr, S: AnsiString; const Index: Integer = 1): Integer;
//StrMatches: *, ? matches
function StrMatches(const Substr, S: AnsiString; const Index: Integer = 1): Boolean;
function StrMatchWildA(const Source, Mask: AnsiString; 
  const WildChar: AnsiChar{$IFDEF SUPPORTS_DEFAULTPARAMS} = AnsiAsterisk{$ENDIF}; 
  const MaskChar: AnsiChar{$IFDEF SUPPORTS_DEFAULTPARAMS} = AnsiQuestionMark{$ENDIF}): Boolean;
function StrMatchWildIA(const Source, Mask: AnsiString; 
  const WildChar: AnsiChar{$IFDEF SUPPORTS_DEFAULTPARAMS} = AnsiAsterisk{$ENDIF}; 
  const MaskChar: AnsiChar{$IFDEF SUPPORTS_DEFAULTPARAMS} = AnsiQuestionMark{$ENDIF}): Boolean;
function StrMatchWildIW(const Source, Mask: WideString; 
  const WildChar: WideChar{$IFDEF SUPPORTS_DEFAULTPARAMS} = WideAsterisk{$ENDIF}; 
  const MaskChar: WideChar{$IFDEF SUPPORTS_DEFAULTPARAMS} = WideQuestionMark{$ENDIF}): Boolean;
//Pos from right
function RPos(const SubStr, S: AnsiString): Integer;
{This searches an array of AnsiString for an occurance of SearchStr}
function PosInStrArray(const SearchStr: AnsiString; const Contents: array of AnsiString; const CaseSensitive: Boolean = True): Integer;

//convert the '#10' or '#$FF' to Chr(10) or Chr($FF);
function StrDelphiUnEscape(const S: AnsiString): AnsiString;

// AnsiString Extraction
function StrAfter(const SubStr, S: AnsiString): AnsiString;
function StrBefore(const SubStr, S: AnsiString): AnsiString;
function StrBetween(const S: AnsiString; const Start, Stop: AnsiChar): AnsiString;
function StrChopRight(const S: AnsiString; N: Integer): AnsiString;
function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
function StrMid(const S: AnsiString; Start, Count: Integer): AnsiString;
function StrRestOf(const S: AnsiString; N: Integer): AnsiString;
function StrRight(const S: AnsiString; Count: Integer): AnsiString;

function TextIsSame(const A1, A2: AnsiString): Boolean;

// Character Transformation Routines
function CharHex(const C: AnsiChar): Byte;
function CharLower(const C: AnsiChar): AnsiChar; {$IFDEF CLR} inline; {$ENDIF}
function CharUpper(const C: AnsiChar): AnsiChar; {$IFDEF CLR} inline; {$ENDIF}

// Character Test Routines
function CharIsUpper(const C: AnsiChar): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsLower(const C: AnsiChar): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsDigit(const C: AnsiChar): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsNumberChar(const C: AnsiChar): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharPosInSet(const AString: AnsiString; const ACharPos: Integer; const ASet: AnsiString): Integer;{$IFDEF CLR} inline; {$ENDIF}
function CharIsInSet(const AString: AnsiString; const ACharPos: Integer; const ASet:  AnsiString): Boolean;{$IFDEF CLR} inline; {$ENDIF}
function CharIsInEOL(const AString: AnsiString; const ACharPos: Integer): Boolean;{$IFDEF CLR} inline; {$ENDIF}
function IsLeadChar(ACh : AnsiChar):Boolean;
function CharRange(const AMin, AMax : AnsiChar): AnsiString;

// AnsiString Test Routines
function StrIsInteger(const S: AnsiString): Boolean;

{$IFDEF COMPILER5} // missing Delphi 5 functions
function TryStrToInt(const S: AnsiString; out Value: Integer): Boolean;
function TryStrToInt64(const S: AnsiString; out Value: Int64): Boolean;
function TryStrToFloat(const S: AnsiString; out Value: Extended): Boolean; overload;
function TryStrToFloat(const S: AnsiString; out Value: Double): Boolean; overload;
function TryStrToFloat(const S: AnsiString; out Value: Single): Boolean; overload;
function TryStrToCurr(const S: AnsiString; out Value: Currency): Boolean;
{$ENDIF COMPILER5}

Var
  gAppPath: AnsiString;

implementation

{$IFNDEF CLR}
type
  TAnsiStrRec = packed record
    AllocSize: Longint;
    RefCount: Longint;
    Length: Longint;
  end;

const
  AnsiStrRecSize  = SizeOf(TAnsiStrRec);     // size of the AnsiString header rec
  AnsiCharCount   = Ord(High(AnsiChar)) + 1; // # of chars in one set
  AnsiLoOffset    = AnsiCharCount * 0;       // offset to lower case chars
  AnsiUpOffset    = AnsiCharCount * 1;       // offset to upper case chars
  AnsiReOffset    = AnsiCharCount * 2;       // offset to reverse case chars
  AnsiAlOffset    = 12;                      // offset to AllocSize in StrRec
  AnsiRfOffset    = 8;                       // offset to RefCount in StrRec
  AnsiLnOffset    = 4;                       // offset to Length in StrRec
  AnsiCaseMapSize = AnsiCharCount * 3;       // # of chars is a table

  ANSI_UPPER_CHAR_TABLE: array[#0..#255] of AnsiChar = (
    #000, #001, #002, #003, #004, #005, #006, #007, #008, #009, #010, #011, #012, #013, #014, #015,
    #016, #017, #018, #019, #020, #021, #022, #023, #024, #025, #026, #027, #028, #029, #030, #031,
    #032, #033, #034, #035, #036, #037, #038, #039, #040, #041, #042, #043, #044, #045, #046, #047,
    #048, #049, #050, #051, #052, #053, #054, #055, #056, #057, #058, #059, #060, #061, #062, #063,
    #064, #065, #066, #067, #068, #069, #070, #071, #072, #073, #074, #075, #076, #077, #078, #079,
    #080, #081, #082, #083, #084, #085, #086, #087, #088, #089, #090, #091, #092, #093, #094, #095,
    #096, #065, #066, #067, #068, #069, #070, #071, #072, #073, #074, #075, #076, #077, #078, #079,
    #080, #081, #082, #083, #084, #085, #086, #087, #088, #089, #090, #123, #124, #125, #126, #127,
    #128, #129, #130, #131, #132, #133, #134, #135, #136, #137, #138, #139, #140, #141, #142, #143,
    #144, #145, #146, #147, #148, #149, #150, #151, #152, #153, #138, #155, #140, #157, #142, #159,
    #160, #161, #162, #163, #164, #165, #166, #167, #168, #169, #170, #171, #172, #173, #174, #175,
    #176, #177, #178, #179, #180, #181, #182, #183, #184, #185, #186, #187, #188, #189, #190, #191,
    #192, #193, #194, #195, #196, #197, #198, #199, #200, #201, #202, #203, #204, #205, #206, #207,
    #208, #209, #210, #211, #212, #213, #214, #215, #216, #217, #218, #219, #220, #221, #222, #223,
    #192, #193, #194, #195, #196, #197, #198, #199, #200, #201, #202, #203, #204, #205, #206, #207,
    #208, #209, #210, #211, #212, #213, #214, #247, #216, #217, #218, #219, #220, #221, #222, #159);


var
  AnsiCaseMap: array [0..AnsiCaseMapSize - 1] of AnsiChar; // case mappings
  AnsiCaseMapReady: Boolean = False;         // true if case map exists
  AnsiCharTypes: array [AnsiChar] of Word;

procedure LoadCharTypes;
var
  CurrChar: AnsiChar;
  CurrType: Word;
  {$IFDEF CLR}
  Category: System.Globalization.UnicodeCategory;
  {$ENDIF CLR}
begin
  for CurrChar := Low(AnsiChar) to High(AnsiChar) do
  begin
    {$IFDEF MSWINDOWS}
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(AnsiChar), CurrType);
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
  CurrChar, UpCaseChar, LoCaseChar, ReCaseChar: AnsiChar;
begin
  if not AnsiCaseMapReady then
  begin
    for CurrChar := Low(AnsiChar) to High(AnsiChar) do
    begin
      {$IFDEF MSWINDOWS}
      LoCaseChar := CurrChar;
      UpCaseChar := CurrChar;
      Windows.CharLowerBuff(@LoCaseChar, 1);
      Windows.CharUpperBuff(@UpCaseChar, 1);
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      LoCaseChar := AnsiChar(tolower(Byte(CurrChar)));
      UpCaseChar := AnsiChar(toupper(Byte(CurrChar)));
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

function RPos(const SubStr, S: AnsiString): Integer;
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

function StrDelphiUnEscape(const S: AnsiString): AnsiString;
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

function StrFetch(var S: AnsiString; const aDelim: AnsiString; const aDelete: Boolean): AnsiString;
var
  I: Integer;
begin
  if ADelim = #0 then
      // AnsiPos does not work with #0
    I := Pos(aDelim, S)
  else
    I := AnsiPos(aDelim, S);

  if I > 0 then
  begin
    Result := Copy(S, 1, I - 1);
    if aDelete then
      //slower Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
      //remaining part is larger than the deleted
      //Delete(S, 1, I + Length(aDelim)-1);
      S := Copy(S, I + Length(aDelim), MaxInt);
  end
  else
  begin
    Result := S;
    if aDelete then
      S := '';
  end;
end;

function StrRFetch(var S: AnsiString; const aDelim: AnsiString; const aDelete: Boolean): AnsiString;
var
  I: Integer;
begin
  I := RPos(aDelim, S);

  if I > 0 then
  begin
    Result := Copy(S, 1, I - 1);
    if aDelete then
      //slower Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
      //remaining part is larger than the deleted
      //Delete(S, 1, I + Length(aDelim)-1);
      S := Copy(S, I + Length(aDelim), MaxInt);
  end
  else
  begin
    Result := S;
    if aDelete then
      S := '';
  end;
end;

procedure StrToStrings(S, Sep: AnsiString; const List: PMeStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  Left: AnsiString;
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


function StrToHex(const Source: AnsiString): AnsiString;
var
  Index: Integer;
  C, L, N: Integer;
  BL, BH: Byte;
  S: AnsiString;
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
        AnsiChar((BH shl 4) + BL);
      Inc(N);
    end;
  end;
  {$IFDEF CLR}
  Result := sb.ToString();
  {$ENDIF CLR}
end;

{$IFDEF CLR}
function StrFind(const Substr, S: AnsiString; const Index: Integer): Integer;
begin
  Result := System.AnsiString(S).ToLower().IndexOf(System.AnsiString(SubStr).ToLower(), Index - 1) + 1;
end;
{$ELSE}
function StrFind(const Substr, S: AnsiString; const Index: Integer): Integer; assembler;
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

        // set the AnsiString pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the Index in EDX

        MOV     EDX, ECX

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        // save the address of Str to compute the result

        PUSH    ESI

        // dec the length of Substr because the first AnsiChar is brought out of it

        DEC     EBX
        JS      @@NotFound

        // #positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // # of chars in Substr to compare

        MOV     NumberOfChars, EBX

        // point Str to Index'th AnsiChar

        ADD     ESI, EDX

        // load case map into EBX, and clear EAX

        LEA     EBX, AnsiCaseMap
        XOR     EAX, EAX
        XOR     EDX, EDX

        // bring the first AnsiChar out of the Substr and point Substr to the next AnsiChar

        MOV     DL, [EDI]
        INC     EDI

        // lower case it

        MOV     DL, [EBX + EDX]
        MOV     SearchChar, DL

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of AnsiString.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current AnsiChar from the AnsiString, and point Str to the next one

        MOV     AL, [ESI]
        INC     ESI


        // lower case current AnsiChar

        MOV     AL, [EBX + EAX]

        // does current AnsiChar match primary search AnsiChar? if not, go back to the main loop

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
function StrSearch(const Substr, S: AnsiString; const Index: Integer): Integer;
begin
  Result := System.AnsiString(S).IndexOf(SubStr, Index - 1) + 1;
end;
{$ELSE}
function StrSearch(const Substr, S: AnsiString; const Index: Integer): Integer; assembler;
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
        // edi pointer to Substr, ebx primary search AnsiChar

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // set the AnsiString pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the (Index - 1) in edx

        MOV     EDX, ECX

        // save the address of Str to compute the result

        PUSH    ESI

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length

        // dec the length of Substr because the first AnsiChar is brought out of it

        DEC     EBX
        JS      @@NotFound

        // # of positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // point Str to Index'th AnsiChar

        ADD     ESI, EDX

        // # of chars in Substr to compare

        MOV     EBP, EBX

        // clear EAX & ECX (working regs)

        XOR     EAX, EAX
        XOR     EBX, EBX

        // bring the first AnsiChar out of the Substr, and
        // point Substr to the next AnsiChar

        MOV     BL, [EDI]
        INC     EDI

        // jump into the loop

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of AnsiString.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current AnsiChar from the AnsiString, and /point Str to the next one.
        MOV     AL, [ESI]
        INC     ESI

        // does current AnsiChar match primary search AnsiChar? if not, go back to the main loop

        CMP     AL, BL
        JNE     @@FindNext

        // otherwise compare SubStr

@@Compare:

        // move # of AnsiChar to compare into edx, edx will be our compare loop counter.

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

//=== AnsiString Extraction ======================================================

function StrAfter(const SubStr, S: AnsiString): AnsiString;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function StrBefore(const SubStr, S: AnsiString): AnsiString;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;


function StrBetween(const S: AnsiString; const Start, Stop: AnsiChar): AnsiString;
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

function StrChopRight(const S: AnsiString; N: Integer): AnsiString;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, 1, Count);
end;

function StrMid(const S: AnsiString; Start, Count: Integer): AnsiString;
begin
  Result := Copy(S, Start, Count);
end;

function StrRestOf(const S: AnsiString; N: Integer ): AnsiString;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrRight(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

function StrIsInteger(const S: AnsiString): Boolean;
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

function CharHex(const C: AnsiChar): Byte;
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

function CharIsLower(const C: AnsiChar): Boolean;
begin
  {$IFDEF CLR}
  Result := System.AnsiChar.IsLower(C);
  {$ELSE}
  Result := (AnsiCharTypes[C] and C1_LOWER) <> 0;
  {$ENDIF CLR}
end;

function CharIsUpper(const C: AnsiChar): Boolean;
begin
  {$IFDEF CLR}
  Result := System.AnsiChar.IsUpper(C);
  {$ELSE}
  Result := (AnsiCharTypes[C] and C1_UPPER) <> 0;
  {$ENDIF CLR}
end;

function CharIsDigit(const C: AnsiChar): Boolean;
begin
  {$IFDEF CLR}
  Result := System.AnsiChar.IsDigit(C);
  {$ELSE}
  Result := (AnsiCharTypes[C] and C1_DIGIT) <> 0;
  {$ENDIF CLR}
end;

function CharIsNumberChar(const C: AnsiChar): Boolean;
begin
  {$IFDEF CLR}
  Result := System.AnsiChar.IsDigit(C) or
  {$ELSE}
  Result := ((AnsiCharTypes[C] and C1_DIGIT) <> 0) or
  {$ENDIF CLR}
    (C in AnsiSigns) or (C = DecimalSeparator);
end;

function CharUpper(const C: AnsiChar): AnsiChar;
begin
  {$IFDEF CLR}
  Result := System.AnsiChar.ToUpper(C);
  {$ELSE}
  Result := AnsiCaseMap[Ord(C) + AnsiUpOffset];
  {$ENDIF CLR}
end;

function CharLower(const C: AnsiChar): AnsiChar;
begin
  {$IFDEF CLR}
  Result := System.AnsiChar.ToLower(C);
  {$ELSE}
  Result := AnsiCaseMap[Ord(C) + AnsiLoOffset];
  {$ENDIF CLR}
end;

function CharToCaseFoldW(const AnsiChar: WideChar): WideChar;
const
  CHAR_TO_CASE_FOLD_1: array[$0000..$03FF] of Byte = (
    $00, $01, $02, $03, $04, $05, $06, $07,
    $08, $00, $00, $00, $00, $09, $0A, $0B,
    $0C, $0D, $0E, $0F, $10, $11, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $12, $13, $14, $15, $16, $17, $18, $19,

    $00, $00, $00, $00, $1A, $1B, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $1C, $1D, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,

    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $1E, $00, $00, $00);
  CHAR_TO_CASE_FOLD_2: array[$0000..$001D, $0000..$003F] of WideChar = (

    (#$0040, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
    #$0068, #$0069, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F,
    #$0070, #$0071, #$0072, #$0073, #$0074, #$0075, #$0076, #$0077,
    #$0078, #$0079, #$007A, #$005B, #$005C, #$005D, #$005E, #$005F,
    #$0060, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
    #$0068, #$0069, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F,
    #$0070, #$0071, #$0072, #$0073, #$0074, #$0075, #$0076, #$0077,
    #$0078, #$0079, #$007A, #$007B, #$007C, #$007D, #$007E, #$007F),

    (#$0080, #$0081, #$0082, #$0083, #$0084, #$0085, #$0086, #$0087,
    #$0088, #$0089, #$008A, #$008B, #$008C, #$008D, #$008E, #$008F,
    #$0090, #$0091, #$0092, #$0093, #$0094, #$0095, #$0096, #$0097,
    #$0098, #$0099, #$009A, #$009B, #$009C, #$009D, #$009E, #$009F,
    #$00A0, #$00A1, #$00A2, #$00A3, #$00A4, #$00A5, #$00A6, #$00A7,
    #$00A8, #$00A9, #$00AA, #$00AB, #$00AC, #$00AD, #$00AE, #$00AF,
    #$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$03BC, #$00B6, #$00B7,
    #$00B8, #$00B9, #$00BA, #$00BB, #$00BC, #$00BD, #$00BE, #$00BF),

    (#$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7,
    #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
    #$00F0, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$00D7,
    #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$00FD, #$00FE, #$00DF,
    #$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7,
    #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
    #$00F0, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$00F7,
    #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$00FD, #$00FE, #$00FF),

    (#$0101, #$0101, #$0103, #$0103, #$0105, #$0105, #$0107, #$0107,
    #$0109, #$0109, #$010B, #$010B, #$010D, #$010D, #$010F, #$010F,
    #$0111, #$0111, #$0113, #$0113, #$0115, #$0115, #$0117, #$0117,
    #$0119, #$0119, #$011B, #$011B, #$011D, #$011D, #$011F, #$011F,
    #$0121, #$0121, #$0123, #$0123, #$0125, #$0125, #$0127, #$0127,
    #$0129, #$0129, #$012B, #$012B, #$012D, #$012D, #$012F, #$012F,
    #$0130, #$0131, #$0133, #$0133, #$0135, #$0135, #$0137, #$0137,
    #$0138, #$013A, #$013A, #$013C, #$013C, #$013E, #$013E, #$0140),

    (#$0140, #$0142, #$0142, #$0144, #$0144, #$0146, #$0146, #$0148,
    #$0148, #$0149, #$014B, #$014B, #$014D, #$014D, #$014F, #$014F,
    #$0151, #$0151, #$0153, #$0153, #$0155, #$0155, #$0157, #$0157,
    #$0159, #$0159, #$015B, #$015B, #$015D, #$015D, #$015F, #$015F,
    #$0161, #$0161, #$0163, #$0163, #$0165, #$0165, #$0167, #$0167,
    #$0169, #$0169, #$016B, #$016B, #$016D, #$016D, #$016F, #$016F,
    #$0171, #$0171, #$0173, #$0173, #$0175, #$0175, #$0177, #$0177,
    #$00FF, #$017A, #$017A, #$017C, #$017C, #$017E, #$017E, #$0073),

    (#$0180, #$0253, #$0183, #$0183, #$0185, #$0185, #$0254, #$0188,
    #$0188, #$0256, #$0257, #$018C, #$018C, #$018D, #$01DD, #$0259,
    #$025B, #$0192, #$0192, #$0260, #$0263, #$0195, #$0269, #$0268,
    #$0199, #$0199, #$019A, #$019B, #$026F, #$0272, #$019E, #$0275,
    #$01A1, #$01A1, #$01A3, #$01A3, #$01A5, #$01A5, #$0280, #$01A8,
    #$01A8, #$0283, #$01AA, #$01AB, #$01AD, #$01AD, #$0288, #$01B0,
    #$01B0, #$028A, #$028B, #$01B4, #$01B4, #$01B6, #$01B6, #$0292,
    #$01B9, #$01B9, #$01BA, #$01BB, #$01BD, #$01BD, #$01BE, #$01BF),

    (#$01C0, #$01C1, #$01C2, #$01C3, #$01C6, #$01C6, #$01C6, #$01C9,
    #$01C9, #$01C9, #$01CC, #$01CC, #$01CC, #$01CE, #$01CE, #$01D0,
    #$01D0, #$01D2, #$01D2, #$01D4, #$01D4, #$01D6, #$01D6, #$01D8,
    #$01D8, #$01DA, #$01DA, #$01DC, #$01DC, #$01DD, #$01DF, #$01DF,
    #$01E1, #$01E1, #$01E3, #$01E3, #$01E5, #$01E5, #$01E7, #$01E7,
    #$01E9, #$01E9, #$01EB, #$01EB, #$01ED, #$01ED, #$01EF, #$01EF,
    #$01F0, #$01F3, #$01F3, #$01F3, #$01F5, #$01F5, #$0195, #$01BF,
    #$01F9, #$01F9, #$01FB, #$01FB, #$01FD, #$01FD, #$01FF, #$01FF),

    (#$0201, #$0201, #$0203, #$0203, #$0205, #$0205, #$0207, #$0207,
    #$0209, #$0209, #$020B, #$020B, #$020D, #$020D, #$020F, #$020F,
    #$0211, #$0211, #$0213, #$0213, #$0215, #$0215, #$0217, #$0217,
    #$0219, #$0219, #$021B, #$021B, #$021D, #$021D, #$021F, #$021F,
    #$019E, #$0221, #$0223, #$0223, #$0225, #$0225, #$0227, #$0227,
    #$0229, #$0229, #$022B, #$022B, #$022D, #$022D, #$022F, #$022F,
    #$0231, #$0231, #$0233, #$0233, #$0234, #$0235, #$0236, #$0237,
    #$0238, #$0239, #$023A, #$023B, #$023C, #$023D, #$023E, #$023F),

    (#$0340, #$0341, #$0342, #$0343, #$0344, #$03B9, #$0346, #$0347,
    #$0348, #$0349, #$034A, #$034B, #$034C, #$034D, #$034E, #$034F,
    #$0350, #$0351, #$0352, #$0353, #$0354, #$0355, #$0356, #$0357,
    #$0358, #$0359, #$035A, #$035B, #$035C, #$035D, #$035E, #$035F,
    #$0360, #$0361, #$0362, #$0363, #$0364, #$0365, #$0366, #$0367,
    #$0368, #$0369, #$036A, #$036B, #$036C, #$036D, #$036E, #$036F,
    #$0370, #$0371, #$0372, #$0373, #$0374, #$0375, #$0376, #$0377,
    #$0378, #$0379, #$037A, #$037B, #$037C, #$037D, #$037E, #$037F),

    (#$0380, #$0381, #$0382, #$0383, #$0384, #$0385, #$03AC, #$0387,
    #$03AD, #$03AE, #$03AF, #$038B, #$03CC, #$038D, #$03CD, #$03CE,
    #$0390, #$03B1, #$03B2, #$03B3, #$03B4, #$03B5, #$03B6, #$03B7,
    #$03B8, #$03B9, #$03BA, #$03BB, #$03BC, #$03BD, #$03BE, #$03BF,
    #$03C0, #$03C1, #$03A2, #$03C3, #$03C4, #$03C5, #$03C6, #$03C7,
    #$03C8, #$03C9, #$03CA, #$03CB, #$03AC, #$03AD, #$03AE, #$03AF,
    #$03B0, #$03B1, #$03B2, #$03B3, #$03B4, #$03B5, #$03B6, #$03B7,
    #$03B8, #$03B9, #$03BA, #$03BB, #$03BC, #$03BD, #$03BE, #$03BF),

    (#$03C0, #$03C1, #$03C3, #$03C3, #$03C4, #$03C5, #$03C6, #$03C7,
    #$03C8, #$03C9, #$03CA, #$03CB, #$03CC, #$03CD, #$03CE, #$03CF,
    #$03B2, #$03B8, #$03D2, #$03D3, #$03D4, #$03C6, #$03C0, #$03D7,
    #$03D9, #$03D9, #$03DB, #$03DB, #$03DD, #$03DD, #$03DF, #$03DF,
    #$03E1, #$03E1, #$03E3, #$03E3, #$03E5, #$03E5, #$03E7, #$03E7,
    #$03E9, #$03E9, #$03EB, #$03EB, #$03ED, #$03ED, #$03EF, #$03EF,
    #$03BA, #$03C1, #$03F2, #$03F3, #$03B8, #$03B5, #$03F6, #$03F8,
    #$03F8, #$03F2, #$03FB, #$03FB, #$03FC, #$03FD, #$03FE, #$03FF),

    (#$0450, #$0451, #$0452, #$0453, #$0454, #$0455, #$0456, #$0457,
    #$0458, #$0459, #$045A, #$045B, #$045C, #$045D, #$045E, #$045F,
    #$0430, #$0431, #$0432, #$0433, #$0434, #$0435, #$0436, #$0437,
    #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E, #$043F,
    #$0440, #$0441, #$0442, #$0443, #$0444, #$0445, #$0446, #$0447,
    #$0448, #$0449, #$044A, #$044B, #$044C, #$044D, #$044E, #$044F,
    #$0430, #$0431, #$0432, #$0433, #$0434, #$0435, #$0436, #$0437,
    #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E, #$043F),

    (#$0440, #$0441, #$0442, #$0443, #$0444, #$0445, #$0446, #$0447,
    #$0448, #$0449, #$044A, #$044B, #$044C, #$044D, #$044E, #$044F,
    #$0450, #$0451, #$0452, #$0453, #$0454, #$0455, #$0456, #$0457,
    #$0458, #$0459, #$045A, #$045B, #$045C, #$045D, #$045E, #$045F,
    #$0461, #$0461, #$0463, #$0463, #$0465, #$0465, #$0467, #$0467,
    #$0469, #$0469, #$046B, #$046B, #$046D, #$046D, #$046F, #$046F,
    #$0471, #$0471, #$0473, #$0473, #$0475, #$0475, #$0477, #$0477,
    #$0479, #$0479, #$047B, #$047B, #$047D, #$047D, #$047F, #$047F),

    (#$0481, #$0481, #$0482, #$0483, #$0484, #$0485, #$0486, #$0487,
    #$0488, #$0489, #$048B, #$048B, #$048D, #$048D, #$048F, #$048F,
    #$0491, #$0491, #$0493, #$0493, #$0495, #$0495, #$0497, #$0497,
    #$0499, #$0499, #$049B, #$049B, #$049D, #$049D, #$049F, #$049F,
    #$04A1, #$04A1, #$04A3, #$04A3, #$04A5, #$04A5, #$04A7, #$04A7,
    #$04A9, #$04A9, #$04AB, #$04AB, #$04AD, #$04AD, #$04AF, #$04AF,
    #$04B1, #$04B1, #$04B3, #$04B3, #$04B5, #$04B5, #$04B7, #$04B7,
    #$04B9, #$04B9, #$04BB, #$04BB, #$04BD, #$04BD, #$04BF, #$04BF),

    (#$04C0, #$04C2, #$04C2, #$04C4, #$04C4, #$04C6, #$04C6, #$04C8,
    #$04C8, #$04CA, #$04CA, #$04CC, #$04CC, #$04CE, #$04CE, #$04CF,
    #$04D1, #$04D1, #$04D3, #$04D3, #$04D5, #$04D5, #$04D7, #$04D7,
    #$04D9, #$04D9, #$04DB, #$04DB, #$04DD, #$04DD, #$04DF, #$04DF,
    #$04E1, #$04E1, #$04E3, #$04E3, #$04E5, #$04E5, #$04E7, #$04E7,
    #$04E9, #$04E9, #$04EB, #$04EB, #$04ED, #$04ED, #$04EF, #$04EF,
    #$04F1, #$04F1, #$04F3, #$04F3, #$04F5, #$04F5, #$04F6, #$04F7,
    #$04F9, #$04F9, #$04FA, #$04FB, #$04FC, #$04FD, #$04FE, #$04FF),

    (#$0501, #$0501, #$0503, #$0503, #$0505, #$0505, #$0507, #$0507,
    #$0509, #$0509, #$050B, #$050B, #$050D, #$050D, #$050F, #$050F,
    #$0510, #$0511, #$0512, #$0513, #$0514, #$0515, #$0516, #$0517,
    #$0518, #$0519, #$051A, #$051B, #$051C, #$051D, #$051E, #$051F,
    #$0520, #$0521, #$0522, #$0523, #$0524, #$0525, #$0526, #$0527,
    #$0528, #$0529, #$052A, #$052B, #$052C, #$052D, #$052E, #$052F,
    #$0530, #$0561, #$0562, #$0563, #$0564, #$0565, #$0566, #$0567,
    #$0568, #$0569, #$056A, #$056B, #$056C, #$056D, #$056E, #$056F),

    (#$0570, #$0571, #$0572, #$0573, #$0574, #$0575, #$0576, #$0577,
    #$0578, #$0579, #$057A, #$057B, #$057C, #$057D, #$057E, #$057F,
    #$0580, #$0581, #$0582, #$0583, #$0584, #$0585, #$0586, #$0557,
    #$0558, #$0559, #$055A, #$055B, #$055C, #$055D, #$055E, #$055F,
    #$0560, #$0561, #$0562, #$0563, #$0564, #$0565, #$0566, #$0567,
    #$0568, #$0569, #$056A, #$056B, #$056C, #$056D, #$056E, #$056F,
    #$0570, #$0571, #$0572, #$0573, #$0574, #$0575, #$0576, #$0577,
    #$0578, #$0579, #$057A, #$057B, #$057C, #$057D, #$057E, #$057F),

    (#$1E01, #$1E01, #$1E03, #$1E03, #$1E05, #$1E05, #$1E07, #$1E07,
    #$1E09, #$1E09, #$1E0B, #$1E0B, #$1E0D, #$1E0D, #$1E0F, #$1E0F,
    #$1E11, #$1E11, #$1E13, #$1E13, #$1E15, #$1E15, #$1E17, #$1E17,
    #$1E19, #$1E19, #$1E1B, #$1E1B, #$1E1D, #$1E1D, #$1E1F, #$1E1F,
    #$1E21, #$1E21, #$1E23, #$1E23, #$1E25, #$1E25, #$1E27, #$1E27,
    #$1E29, #$1E29, #$1E2B, #$1E2B, #$1E2D, #$1E2D, #$1E2F, #$1E2F,
    #$1E31, #$1E31, #$1E33, #$1E33, #$1E35, #$1E35, #$1E37, #$1E37,
    #$1E39, #$1E39, #$1E3B, #$1E3B, #$1E3D, #$1E3D, #$1E3F, #$1E3F),

    (#$1E41, #$1E41, #$1E43, #$1E43, #$1E45, #$1E45, #$1E47, #$1E47,
    #$1E49, #$1E49, #$1E4B, #$1E4B, #$1E4D, #$1E4D, #$1E4F, #$1E4F,
    #$1E51, #$1E51, #$1E53, #$1E53, #$1E55, #$1E55, #$1E57, #$1E57,
    #$1E59, #$1E59, #$1E5B, #$1E5B, #$1E5D, #$1E5D, #$1E5F, #$1E5F,
    #$1E61, #$1E61, #$1E63, #$1E63, #$1E65, #$1E65, #$1E67, #$1E67,
    #$1E69, #$1E69, #$1E6B, #$1E6B, #$1E6D, #$1E6D, #$1E6F, #$1E6F,
    #$1E71, #$1E71, #$1E73, #$1E73, #$1E75, #$1E75, #$1E77, #$1E77,
    #$1E79, #$1E79, #$1E7B, #$1E7B, #$1E7D, #$1E7D, #$1E7F, #$1E7F),

    (#$1E81, #$1E81, #$1E83, #$1E83, #$1E85, #$1E85, #$1E87, #$1E87,
    #$1E89, #$1E89, #$1E8B, #$1E8B, #$1E8D, #$1E8D, #$1E8F, #$1E8F,
    #$1E91, #$1E91, #$1E93, #$1E93, #$1E95, #$1E95, #$1E96, #$1E97,
    #$1E98, #$1E99, #$1E9A, #$1E61, #$1E9C, #$1E9D, #$1E9E, #$1E9F,
    #$1EA1, #$1EA1, #$1EA3, #$1EA3, #$1EA5, #$1EA5, #$1EA7, #$1EA7,
    #$1EA9, #$1EA9, #$1EAB, #$1EAB, #$1EAD, #$1EAD, #$1EAF, #$1EAF,
    #$1EB1, #$1EB1, #$1EB3, #$1EB3, #$1EB5, #$1EB5, #$1EB7, #$1EB7,
    #$1EB9, #$1EB9, #$1EBB, #$1EBB, #$1EBD, #$1EBD, #$1EBF, #$1EBF),

    (#$1EC1, #$1EC1, #$1EC3, #$1EC3, #$1EC5, #$1EC5, #$1EC7, #$1EC7,
    #$1EC9, #$1EC9, #$1ECB, #$1ECB, #$1ECD, #$1ECD, #$1ECF, #$1ECF,
    #$1ED1, #$1ED1, #$1ED3, #$1ED3, #$1ED5, #$1ED5, #$1ED7, #$1ED7,
    #$1ED9, #$1ED9, #$1EDB, #$1EDB, #$1EDD, #$1EDD, #$1EDF, #$1EDF,
    #$1EE1, #$1EE1, #$1EE3, #$1EE3, #$1EE5, #$1EE5, #$1EE7, #$1EE7,
    #$1EE9, #$1EE9, #$1EEB, #$1EEB, #$1EED, #$1EED, #$1EEF, #$1EEF,
    #$1EF1, #$1EF1, #$1EF3, #$1EF3, #$1EF5, #$1EF5, #$1EF7, #$1EF7,
    #$1EF9, #$1EF9, #$1EFA, #$1EFB, #$1EFC, #$1EFD, #$1EFE, #$1EFF),

    (#$1F00, #$1F01, #$1F02, #$1F03, #$1F04, #$1F05, #$1F06, #$1F07,
    #$1F00, #$1F01, #$1F02, #$1F03, #$1F04, #$1F05, #$1F06, #$1F07,
    #$1F10, #$1F11, #$1F12, #$1F13, #$1F14, #$1F15, #$1F16, #$1F17,
    #$1F10, #$1F11, #$1F12, #$1F13, #$1F14, #$1F15, #$1F1E, #$1F1F,
    #$1F20, #$1F21, #$1F22, #$1F23, #$1F24, #$1F25, #$1F26, #$1F27,
    #$1F20, #$1F21, #$1F22, #$1F23, #$1F24, #$1F25, #$1F26, #$1F27,
    #$1F30, #$1F31, #$1F32, #$1F33, #$1F34, #$1F35, #$1F36, #$1F37,
    #$1F30, #$1F31, #$1F32, #$1F33, #$1F34, #$1F35, #$1F36, #$1F37),

    (#$1F40, #$1F41, #$1F42, #$1F43, #$1F44, #$1F45, #$1F46, #$1F47,
    #$1F40, #$1F41, #$1F42, #$1F43, #$1F44, #$1F45, #$1F4E, #$1F4F,
    #$1F50, #$1F51, #$1F52, #$1F53, #$1F54, #$1F55, #$1F56, #$1F57,
    #$1F58, #$1F51, #$1F5A, #$1F53, #$1F5C, #$1F55, #$1F5E, #$1F57,
    #$1F60, #$1F61, #$1F62, #$1F63, #$1F64, #$1F65, #$1F66, #$1F67,
    #$1F60, #$1F61, #$1F62, #$1F63, #$1F64, #$1F65, #$1F66, #$1F67,
    #$1F70, #$1F71, #$1F72, #$1F73, #$1F74, #$1F75, #$1F76, #$1F77,
    #$1F78, #$1F79, #$1F7A, #$1F7B, #$1F7C, #$1F7D, #$1F7E, #$1F7F),

    (#$1F80, #$1F81, #$1F82, #$1F83, #$1F84, #$1F85, #$1F86, #$1F87,
    #$1F80, #$1F81, #$1F82, #$1F83, #$1F84, #$1F85, #$1F86, #$1F87,
    #$1F90, #$1F91, #$1F92, #$1F93, #$1F94, #$1F95, #$1F96, #$1F97,
    #$1F90, #$1F91, #$1F92, #$1F93, #$1F94, #$1F95, #$1F96, #$1F97,
    #$1FA0, #$1FA1, #$1FA2, #$1FA3, #$1FA4, #$1FA5, #$1FA6, #$1FA7,
    #$1FA0, #$1FA1, #$1FA2, #$1FA3, #$1FA4, #$1FA5, #$1FA6, #$1FA7,
    #$1FB0, #$1FB1, #$1FB2, #$1FB3, #$1FB4, #$1FB5, #$1FB6, #$1FB7,
    #$1FB0, #$1FB1, #$1F70, #$1F71, #$1FB3, #$1FBD, #$03B9, #$1FBF),

    (#$1FC0, #$1FC1, #$1FC2, #$1FC3, #$1FC4, #$1FC5, #$1FC6, #$1FC7,
    #$1F72, #$1F73, #$1F74, #$1F75, #$1FC3, #$1FCD, #$1FCE, #$1FCF,
    #$1FD0, #$1FD1, #$1FD2, #$1FD3, #$1FD4, #$1FD5, #$1FD6, #$1FD7,
    #$1FD0, #$1FD1, #$1F76, #$1F77, #$1FDC, #$1FDD, #$1FDE, #$1FDF,
    #$1FE0, #$1FE1, #$1FE2, #$1FE3, #$1FE4, #$1FE5, #$1FE6, #$1FE7,
    #$1FE0, #$1FE1, #$1F7A, #$1F7B, #$1FE5, #$1FED, #$1FEE, #$1FEF,
    #$1FF0, #$1FF1, #$1FF2, #$1FF3, #$1FF4, #$1FF5, #$1FF6, #$1FF7,
    #$1F78, #$1F79, #$1F7C, #$1F7D, #$1FF3, #$1FFD, #$1FFE, #$1FFF),

    (#$2100, #$2101, #$2102, #$2103, #$2104, #$2105, #$2106, #$2107,
    #$2108, #$2109, #$210A, #$210B, #$210C, #$210D, #$210E, #$210F,
    #$2110, #$2111, #$2112, #$2113, #$2114, #$2115, #$2116, #$2117,
    #$2118, #$2119, #$211A, #$211B, #$211C, #$211D, #$211E, #$211F,
    #$2120, #$2121, #$2122, #$2123, #$2124, #$2125, #$03C9, #$2127,
    #$2128, #$2129, #$006B, #$00E5, #$212C, #$212D, #$212E, #$212F,
    #$2130, #$2131, #$2132, #$2133, #$2134, #$2135, #$2136, #$2137,
    #$2138, #$2139, #$213A, #$213B, #$213C, #$213D, #$213E, #$213F),

    (#$2140, #$2141, #$2142, #$2143, #$2144, #$2145, #$2146, #$2147,
    #$2148, #$2149, #$214A, #$214B, #$214C, #$214D, #$214E, #$214F,
    #$2150, #$2151, #$2152, #$2153, #$2154, #$2155, #$2156, #$2157,
    #$2158, #$2159, #$215A, #$215B, #$215C, #$215D, #$215E, #$215F,
    #$2170, #$2171, #$2172, #$2173, #$2174, #$2175, #$2176, #$2177,
    #$2178, #$2179, #$217A, #$217B, #$217C, #$217D, #$217E, #$217F,
    #$2170, #$2171, #$2172, #$2173, #$2174, #$2175, #$2176, #$2177,
    #$2178, #$2179, #$217A, #$217B, #$217C, #$217D, #$217E, #$217F),

    (#$2480, #$2481, #$2482, #$2483, #$2484, #$2485, #$2486, #$2487,
    #$2488, #$2489, #$248A, #$248B, #$248C, #$248D, #$248E, #$248F,
    #$2490, #$2491, #$2492, #$2493, #$2494, #$2495, #$2496, #$2497,
    #$2498, #$2499, #$249A, #$249B, #$249C, #$249D, #$249E, #$249F,
    #$24A0, #$24A1, #$24A2, #$24A3, #$24A4, #$24A5, #$24A6, #$24A7,
    #$24A8, #$24A9, #$24AA, #$24AB, #$24AC, #$24AD, #$24AE, #$24AF,
    #$24B0, #$24B1, #$24B2, #$24B3, #$24B4, #$24B5, #$24D0, #$24D1,
    #$24D2, #$24D3, #$24D4, #$24D5, #$24D6, #$24D7, #$24D8, #$24D9),

    (#$24DA, #$24DB, #$24DC, #$24DD, #$24DE, #$24DF, #$24E0, #$24E1,
    #$24E2, #$24E3, #$24E4, #$24E5, #$24E6, #$24E7, #$24E8, #$24E9,
    #$24D0, #$24D1, #$24D2, #$24D3, #$24D4, #$24D5, #$24D6, #$24D7,
    #$24D8, #$24D9, #$24DA, #$24DB, #$24DC, #$24DD, #$24DE, #$24DF,
    #$24E0, #$24E1, #$24E2, #$24E3, #$24E4, #$24E5, #$24E6, #$24E7,
    #$24E8, #$24E9, #$24EA, #$24EB, #$24EC, #$24ED, #$24EE, #$24EF,
    #$24F0, #$24F1, #$24F2, #$24F3, #$24F4, #$24F5, #$24F6, #$24F7,
    #$24F8, #$24F9, #$24FA, #$24FB, #$24FC, #$24FD, #$24FE, #$24FF),

    (#$FF00, #$FF01, #$FF02, #$FF03, #$FF04, #$FF05, #$FF06, #$FF07,
    #$FF08, #$FF09, #$FF0A, #$FF0B, #$FF0C, #$FF0D, #$FF0E, #$FF0F,
    #$FF10, #$FF11, #$FF12, #$FF13, #$FF14, #$FF15, #$FF16, #$FF17,
    #$FF18, #$FF19, #$FF1A, #$FF1B, #$FF1C, #$FF1D, #$FF1E, #$FF1F,
    #$FF20, #$FF41, #$FF42, #$FF43, #$FF44, #$FF45, #$FF46, #$FF47,
    #$FF48, #$FF49, #$FF4A, #$FF4B, #$FF4C, #$FF4D, #$FF4E, #$FF4F,
    #$FF50, #$FF51, #$FF52, #$FF53, #$FF54, #$FF55, #$FF56, #$FF57,
    #$FF58, #$FF59, #$FF5A, #$FF3B, #$FF3C, #$FF3D, #$FF3E, #$FF3F));
  CHAR_TO_CASE_FOLD_SIZE = 64;
var
  i: Integer;
begin
  Result := AnsiChar;
  i := CHAR_TO_CASE_FOLD_1[Ord(Result) div CHAR_TO_CASE_FOLD_SIZE];
  if i <> 0 then
    begin
      Dec(i);
      Result := CHAR_TO_CASE_FOLD_2[i, Ord(Result) and (CHAR_TO_CASE_FOLD_SIZE - 1)];
    end;
end;

function StrMatchWildA(const Source, Mask: AnsiString; const WildChar: AnsiChar; const MaskChar: AnsiChar): Boolean;

label
  Failure, Success, BackTrack;
var
  c: AnsiChar;
  SourcePtr, MaskPtr, LastWild, LastSource: PAnsiChar;
  SourceLength, MaskLength: Cardinal;
begin
  SourcePtr := Pointer(Source);
  SourceLength := Cardinal(SourcePtr);
  if SourceLength <> 0 then SourceLength := PCardinal(SourceLength - 4)^;

  MaskPtr := Pointer(Mask);
  MaskLength := Cardinal(MaskPtr);
  if MaskLength <> 0 then MaskLength := PCardinal(MaskLength - 4)^;

  while (SourceLength > 0) and (MaskLength > 0) do
    begin
      c := MaskPtr^;
      if (c = WildChar) or ((c <> MaskChar) and (c <> SourcePtr^)) then Break;
      Inc(MaskPtr);
      Inc(SourcePtr);
      Dec(MaskLength);
      Dec(SourceLength);
    end;

  if MaskLength > 0 then
    begin
      if MaskPtr^ = WildChar then
        begin

          repeat

            while (MaskLength > 0) and (MaskPtr^ = WildChar) do
              begin
                Inc(MaskPtr);
                Dec(MaskLength);
              end;

            if MaskLength = 0 then goto Success;

            LastWild := MaskPtr;

            BackTrack:

            c := MaskPtr^;
            while (SourceLength > 0) and (c <> MaskChar) and (c <> SourcePtr^) do
              begin
                Inc(SourcePtr);
                Dec(SourceLength);
              end;

            if SourceLength = 0 then goto Failure;

            Inc(SourcePtr);
            Dec(SourceLength);

            LastSource := SourcePtr;

            Inc(MaskPtr);
            Dec(MaskLength);

            while (SourceLength > 0) and (MaskLength > 0) do
              begin
                c := MaskPtr^;
                if (c = WildChar) or ((c <> MaskChar) and (c <> SourcePtr^)) then Break;
                Inc(MaskPtr);
                Inc(SourcePtr);
                Dec(MaskLength);
                Dec(SourceLength);
              end;

            if (MaskLength > 0) and (MaskPtr^ <> WildChar) then
              begin
                Inc(MaskLength, MaskPtr - LastWild);
                MaskPtr := LastWild;

                Inc(SourceLength, SourcePtr - LastSource);
                SourcePtr := LastSource;

                goto BackTrack;
              end;

          until MaskLength = 0;

          if SourceLength = 0 then goto Success;

          MaskLength := MaskPtr - LastWild;

          MaskPtr := LastWild;

          Inc(SourcePtr, SourceLength); Dec(SourcePtr, MaskLength);

          while (MaskLength > 0) do
            begin
              c := MaskPtr^;
              if (c <> MaskChar) and (c <> SourcePtr^) then Break;
              Inc(MaskPtr);
              Inc(SourcePtr);
              Dec(MaskLength);
            end;

          if MaskLength = 0 then goto Success;
        end;
    end
  else
    if SourceLength = 0 then
      goto Success;

  Failure:
  Result := False;
  Exit;

  Success:
  Result := True;
end;

function StrMatchWildIA(const Source, Mask: AnsiString; const WildChar: AnsiChar; const MaskChar: AnsiChar): Boolean;

label
  Failure, Success, BackTrack;
var
  c: AnsiChar;
  SourcePtr, MaskPtr, LastWild, LastSource: PAnsiChar;
  SourceLength, MaskLength: Cardinal;
begin
  SourcePtr := Pointer(Source);
  SourceLength := Cardinal(SourcePtr);
  if SourceLength <> 0 then SourceLength := PCardinal(SourceLength - 4)^;

  MaskPtr := Pointer(Mask);
  MaskLength := Cardinal(MaskPtr);
  if MaskLength <> 0 then MaskLength := PCardinal(MaskLength - 4)^;

  while (SourceLength > 0) and (MaskLength > 0) do
    begin
      c := MaskPtr^;
      if (c = WildChar) or ((c <> MaskChar) and (ANSI_UPPER_CHAR_TABLE[c] <> ANSI_UPPER_CHAR_TABLE[SourcePtr^])) then Break;
      Inc(MaskPtr);
      Inc(SourcePtr);
      Dec(MaskLength);
      Dec(SourceLength);
    end;

  if MaskLength > 0 then
    begin
      if MaskPtr^ = WildChar then
        begin

          repeat

            while (MaskLength > 0) and (MaskPtr^ = WildChar) do
              begin
                Inc(MaskPtr);
                Dec(MaskLength);
              end;

            if MaskLength = 0 then goto Success;

            LastWild := MaskPtr;

            BackTrack:

            c := ANSI_UPPER_CHAR_TABLE[MaskPtr^];
            while (SourceLength > 0) and (c <> MaskChar) and (c <> ANSI_UPPER_CHAR_TABLE[SourcePtr^]) do
              begin
                Inc(SourcePtr);
                Dec(SourceLength);
              end;

            if SourceLength = 0 then goto Failure;

            Inc(SourcePtr);
            Dec(SourceLength);

            LastSource := SourcePtr;

            Inc(MaskPtr);
            Dec(MaskLength);

            while (SourceLength > 0) and (MaskLength > 0) do
              begin
                c := MaskPtr^;
                if (c = WildChar) or ((c <> MaskChar) and (ANSI_UPPER_CHAR_TABLE[c] <> ANSI_UPPER_CHAR_TABLE[SourcePtr^])) then Break;
                Inc(MaskPtr);
                Inc(SourcePtr);
                Dec(MaskLength);
                Dec(SourceLength);
              end;

            if (MaskLength > 0) and (MaskPtr^ <> WildChar) then
              begin
                Inc(MaskLength, MaskPtr - LastWild);
                MaskPtr := LastWild;

                Inc(SourceLength, SourcePtr - LastSource);
                SourcePtr := LastSource;

                goto BackTrack;
              end;

          until MaskLength = 0;

          if SourceLength = 0 then goto Success;

          MaskLength := MaskPtr - LastWild;

          MaskPtr := LastWild;

          Inc(SourcePtr, SourceLength); Dec(SourcePtr, MaskLength);

          while (MaskLength > 0) do
            begin
              c := MaskPtr^;
              if (c <> MaskChar) and (ANSI_UPPER_CHAR_TABLE[c] <> ANSI_UPPER_CHAR_TABLE[SourcePtr^]) then Break;
              Inc(MaskPtr);
              Inc(SourcePtr);
              Dec(MaskLength);
            end;

          if MaskLength = 0 then goto Success;
        end;
    end
  else
    if SourceLength = 0 then
      goto Success;

  Failure:
  Result := False;
  Exit;

  Success:
  Result := True;
end;

function StrMatchWildIW(const Source, Mask: WideString; const WildChar: WideChar; const MaskChar: WideChar): Boolean;

label
  Failure, Success, BackTrack;
var
  c: WideChar;
  SourcePtr, MaskPtr, LastWild, LastSource: PWideChar;
  SourceLength, MaskLength: Cardinal;
begin
  SourcePtr := Pointer(Source);
  SourceLength := Cardinal(SourcePtr);
  if SourceLength <> 0 then SourceLength := PCardinal(SourceLength - 4)^ shr 1;

  MaskPtr := Pointer(Mask);
  MaskLength := Cardinal(MaskPtr);
  if MaskLength <> 0 then MaskLength := PCardinal(MaskLength - 4)^ shr 1;

  while (SourceLength > 0) and (MaskLength > 0) do
    begin
      c := MaskPtr^;
      if (c = WildChar) or ((c <> MaskChar) and (CharToCaseFoldW(c) <> CharToCaseFoldW(SourcePtr^))) then Break;
      Inc(MaskPtr);
      Inc(SourcePtr);
      Dec(MaskLength);
      Dec(SourceLength);
    end;

  if MaskLength > 0 then
    begin
      if MaskPtr^ = WildChar then
        begin

          repeat

            while (MaskLength > 0) and (MaskPtr^ = WildChar) do
              begin
                Inc(MaskPtr);
                Dec(MaskLength);
              end;

            if MaskLength = 0 then goto Success;

            LastWild := MaskPtr;

            BackTrack:

            c := CharToCaseFoldW(MaskPtr^);
            while (SourceLength > 0) and (c <> MaskChar) and (c <> CharToCaseFoldW(SourcePtr^)) do
              begin
                Inc(SourcePtr);
                Dec(SourceLength);
              end;

            if SourceLength = 0 then goto Failure;

            Inc(SourcePtr);
            Dec(SourceLength);

            LastSource := SourcePtr;

            Inc(MaskPtr);
            Dec(MaskLength);

            while (SourceLength > 0) and (MaskLength > 0) do
              begin
                c := MaskPtr^;
                if (c = WildChar) or ((c <> MaskChar) and (CharToCaseFoldW(c) <> CharToCaseFoldW(SourcePtr^))) then Break;
                Inc(MaskPtr);
                Inc(SourcePtr);
                Dec(MaskLength);
                Dec(SourceLength);
              end;

            if (MaskLength > 0) and (MaskPtr^ <> WildChar) then
              begin
                Inc(MaskLength, MaskPtr - LastWild);
                MaskPtr := LastWild;

                Inc(SourceLength, SourcePtr - LastSource);
                SourcePtr := LastSource;

                goto BackTrack;
              end;

          until MaskLength = 0;

          if SourceLength = 0 then goto Success;

          MaskLength := MaskPtr - LastWild;

          MaskPtr := LastWild;

          Inc(SourcePtr, SourceLength); Dec(SourcePtr, MaskLength);

          while (MaskLength > 0) do
            begin
              c := MaskPtr^;
              if (c <> MaskChar) and (CharToCaseFoldW(c) <> CharToCaseFoldW(SourcePtr^)) then Break;
              Inc(MaskPtr);
              Inc(SourcePtr);
              Dec(MaskLength);
            end;

          if MaskLength = 0 then goto Success;
        end;
    end
  else
    if SourceLength = 0 then
      goto Success;

  Failure:
  Result := False;
  Exit;

  Success:
  Result := True;
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

function ExtractFileBaseName(const aFileName: AnsiString): AnsiString;
var
  i: integer;
begin
  i := RPos('.', aFileName);
  if i <= 0 then i := Length(aFileName);
  Result := Copy(aFileName, 1, i-1);
end;

function PathIsDiskDevice(const Path: AnsiString): Boolean;
{$IFDEF UNIX}
var
  FullPath: AnsiString;
  F: PIOFile;
  Buffer: array [0..255] of AnsiChar;
  MountEntry: TMountEntry;
  FsTypes: PMeStrings;

  procedure GetAvailableFileSystems(const List: PMeStrings);
  var
    F: TextFile;
    S: AnsiString;
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

function PathIsUNC(const Path: AnsiString): Boolean;

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
    // a valid machine name is a AnsiString composed of the set [a-z, A-Z, 0-9, -, _] but it may not
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
    // a valid share name is a AnsiString composed of a set the set !InvalidCharacters note that a
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

function PathIsAbsolute(const Path: AnsiString): Boolean;
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

function ProcessFiles(const aPath:AnsiString; aPatterns: AnsiString; const OnFile: TOnDoFileEvent): Boolean;
var 
  p: integer;
  SrchRec: TSearchRec;
  Pattern: AnsiString;
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
function ProcessFolder(const aFolder, aPatterns: AnsiString; const OnFile: TOnDoFileEvent): Boolean;
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

function StrTrim(const aText: AnsiString; const IgnoreCase: boolean; const IgnoreBlanks: boolean): AnsiString;
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
function TryStrToInt(const S: AnsiString; out Value: Integer): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;

function TryStrToInt64(const S: AnsiString; out Value: Int64): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;

function TryStrToFloat(const S: AnsiString; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

function TryStrToFloat(const S: AnsiString; out Value: Double): Boolean;
var
  F: Extended;
begin
  Result := TryStrToFloat(S, F);
  if Result then
    Value := F;
end;

function TryStrToFloat(const S: AnsiString; out Value: Single): Boolean;
var
  F: Extended;
begin
  Result := TryStrToFloat(S, F);
  if Result then
    Value := F;
end;

function TryStrToCurr(const S: AnsiString; out Value: Currency): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvCurrency);
end;
{$ENDIF COMPILER5}

function TextIsSame(const A1, A2: AnsiString): Boolean;
begin
  {$IFDEF DOTNET}
  Result := System.AnsiString.Compare(A1, A2, True) = 0;
  {$ELSE}
  Result := AnsiCompareText(A1, A2) = 0;
  {$ENDIF}
end;

{This searches an array of AnsiString for an occurance of SearchStr}
function PosInStrArray(const SearchStr: AnsiString; const Contents: array of AnsiString; const CaseSensitive: Boolean = True): Integer;
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

function CharPosInSet(const AString: AnsiString; const ACharPos: Integer; const ASet: AnsiString): Integer;
begin
  if ACharPos <= Length(AString) then begin
    Result := AnsiPos(AString[ACharPos], ASet);
  end else begin
    Result := 0;
  end;
end;

function CharIsInSet(const AString: AnsiString; const ACharPos: Integer; const ASet:  AnsiString): Boolean;
begin
  Result := CharPosInSet(AString, ACharPos, ASet) > 0;
end;

function CharIsInEOL(const AString: AnsiString; const ACharPos: Integer): Boolean;
begin
  Result := CharIsInSet(AString, ACharPos, AnsiCrLf);
end;

function IsLeadChar(ACh : AnsiChar):Boolean;
begin
  {$IFDEF DOTNET}
  Result := False;
  {$ELSE}
  Result := ACh in LeadBytes;
  {$ENDIF}
end;

function CharRange(const AMin, AMax : AnsiChar): AnsiString;
var
  i : AnsiChar;
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