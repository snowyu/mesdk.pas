{
   @author  Riceball LEE<riceballl@hotmail.com>
   @version $Revision: 1.17 $
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
 * The Original Code is $RCSfile: uMeTokenizer.pas,v $.
 *
 * The Initial Developers of the Original Code are Riceball LEE<riceballl@hotmail.com>.
 * Portions created by Riceball LEE<riceballl@hotmail.com> is Copyright (C) 2007
 * All rights reserved.
 *
 * Contributor(s):
 *
 *)
unit uMeTokenizer;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils
  , FastcodeCompareTextExUnit
  , uMeObject
  , uMeStream
  ;

(* in System.pas
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);

var   { Text output line break handling.  Default value for all text files }
  DefaultTextLineBreakStyle: TTextLineBreakStyle = {$IFDEF LINUX} tlbsLF {$ENDIF}
                                                 {$IFDEF MSWINDOWS} tlbsCRLF {$ENDIF};
const
  sLineBreak = {$IFDEF LINUX} #10 {$ENDIF} {$IFDEF MSWINDOWS} #13#10 {$ENDIF};
*)

ResourceString
  //1: %i: Line; 2: %i: Col; 3. %i: the ErrorCode; 4. %s: the ErrorToken; 
  rsMeTokenErrorMissedToken = '(%i:%i) Fatal: %i the %s is expected. ';
  rsMeTokenErrorUnknown =  '(%i:%i) Fatal: Unknown Token Error %i the error token is "%s" . ';

type
  TMeCharset = set of char;
const
  cCR = #$0D;
  cLF = #$0A;

  cMaxTokenLength  = 32;
  cMeControlCharset: TMeCharset = [#0..#31];

  cMeTokenErrorMissedToken  = 1;
  cMeTokenErrorUnknownToken = 2;

  cMeCustomTokenType = 32;

{
  ttToken      = 0;
  ttSpliter    = 1;
  ttBlockBegin = 2;
  ttBlockEnd   = 3;
  ttString     = 4;
  ttComment    = 5;
}

type
  PMeTokenString = ^ TMeTokenString;
  TMeTokenString = String[cMaxTokenLength];
  {
  @param ttToken  the general token. this general token not the include the control-chars or blankChars.
  @param ttSimpleToken the general Simple Token.
  @param ttComplexToken the general Complex Token.
  }
  TMeTokenType = (ttUnknown, ttSimpleToken, ttComplexToken);
  TMeCustomTokenType = (ttToken, ttSpliter, ttBlockBegin, ttBlockEnd, ttString, ttComment);

  {
    @param tfEscapeDuplicateToken  转义在CompexToken 序列中 双写"TokenEnd"表示一个TokenEnd，不会结束序列。
    @param tfEscapeChar            是否启用 EscapeChar.
    @param tfOneLine               在CompexToken 序列中遇到 CRLF则停止，只处理一行。
  }
  TMeTokenFlag = (tfEscapeDuplicateToken, tfOneLine, tfEscapeChar);
  TMeTokenFlags = set of TMeTokenFlag;

  PMeComplexTokenTypes = ^ TMeComplexTokenTypes;
  PMeTokenizer = ^ TMeTokenizer;
  PMeToken = ^ TMeToken;

  PMeSimpleTokenType = ^ TMeSimpleTokenType;
  TMeSimpleTokenType = Object
    TokenType: TMeTokenType;
    //note: the 0-31 is preserved for system
    TokenId: Integer;
    //the Token should be not empty.
    Token: TMeTokenString;
  end;

  //for String or Comment like.
  PMeComplexTokenType = ^ TMeComplexTokenType;
  TMeComplexTokenType = Object(TMeSimpleTokenType)
    //treate the duplicate tokenEnd as single symbol in it(the duplicate tokenEnd is not the Token to end.).
    //only used in ttString
    Flags: TMeTokenFlags;
    //TokenBegin: TMeTokenString; it is TMeSimpleTokenType.Token
    //if TokenEnd is '' and tfOneLine in Flags then it is one line !
    TokenEnd: TMeTokenString;
  end;

  PMeSimpleTokenTypes = ^ TMeSimpleTokenTypes;
  TMeSimpleTokenTypes = Object(TMeList)
  protected
    function GetItem(const Index: Integer): PMeSimpleTokenType;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
    procedure Add(const aTokenId: Integer; const aTokenName: string);{$IFDEF SUPPORTS_OVERLOAD}overload;{$ENDIF}
    {$IFDEF SUPPORTS_OVERLOAD}
    procedure Add(const aTokenId: TMeCustomTokenType; const aTokenName: string);overload;
    {$ENDIF}
    procedure AddStandardToken(const aTokenId: TMeCustomTokenType; const aTokenName: string);
  public
    property Items[const Index: Integer]: PMeSimpleTokenType read GetItem; default;
  end;

  TMeComplexTokenTypes = Object(TMeList)
  protected
    function GetItem(const Index: Integer): PMeComplexTokenType;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
    procedure Add(const aTokenId: Integer; const aTokenBegin, aTokenEnd: string; const aFlags: TMeTokenFlags = []);{$IFDEF SUPPORTS_OVERLOAD}overload;{$ENDIF}
    {$IFDEF SUPPORTS_OVERLOAD}
    procedure Add(const aTokenId: TMeCustomTokenType; const aTokenBegin, aTokenEnd: string; const aFlags: TMeTokenFlags = []);overload;
    {$ENDIF}
    procedure AddStandardToken(const aTokenId: TMeCustomTokenType; const aTokenBegin, aTokenEnd: string; const aFlags: TMeTokenFlags = []);
    function IndexOfId(const aId: Integer; const aBeginIndex: Integer = 0): Integer;
    //search the index of TokenBegin 
    function IndexOf(const aName: string; const aBeginIndex: Integer = 0; const aIgnoreCase: Boolean = false): Integer;
    //search the index of TokenEnd
    function IndexOfEnd(const aName: string; const aBeginIndex: Integer = 0; const aIgnoreCase: Boolean = false): Integer;
  public
    property Items[const Index: Integer]: PMeComplexTokenType read GetItem; default;
  end;

  TMeToken = Object //Note: DO NOT USE VIRTUAL OBJECT! Treat as Record.
  protected
    function GetToken: string;
  public
    procedure Reset;
    function IsEmpty: Boolean;
    procedure Assign(const aValue: PMeToken);
  public
    Pos: PChar; //pos = nil means empty token.
    TokenId: Integer; //if TokenId = ttToken then TokeType means nil.
    TokenType: PMeSimpleTokenType;
    Size: Integer;
    Line, Col: Integer;
    LineEnd, ColEnd: Integer; //this token end line No and end column No.
    
    property Token: String read GetToken;
  end;

  PMeTokens = ^TMeTokens;
  TMeTokens = Object(TMeList)
  protected
    function GetItem(const Index: Integer): PMeToken;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
  public
    property Items[const Index: Integer]: PMeToken read GetItem; default;
  end;

  PMeTokenErrorInfo = ^ TMeTokenErrorInfo;
  TMeTokenErrorInfo = Object(TMeToken)
  public
    function ErrorInfo: string;
  public
    ErrorCode: Integer;
    ErrorFmt: ShortString;
  end;

  PMeTokenErrors = ^ TMeTokenErrors;
  TMeTokenErrors = Object(TMeList)
  protected
    function GetItem(const Index: Integer): PMeTokenErrorInfo;
  public
    destructor Destroy; virtual;{override}
    procedure Add(const aToken: TMeToken; const aErrorCode: Integer; const aErrorFmt: ShortString='');
    procedure Clear;
  public
    property Items[const Index: Integer]: PMeTokenErrorInfo read GetItem; default;
  end;

  {the Me Tokenizer}
  {
   Init the system supported internal Tokens:
     SimpleTokens.Add(ttSpliter, ';');
     SimpleTokens.Add(ttSpliter, ',');
     Tokens.Add(aTokenId, aTokenBegin, aTokenEnd);
   Note: the token never be CRLF!! 只有在 BlankChars 中才可以包含CRLF，在定义的Token字符串中绝对不能有 CRLF.

   First LoadFromXXX, 
   then 
     call ReadToken to read next token. return nil means no more token to get.
     call NextToken to pre-read the next token. return nil means no more token to get.
   search order:
     BlockToken:
     
  }
  TMeEscapedCharEvent = procedure(const Sender: PMeTokenizer; var EscapedStr: TMeTokenString) of object;
  TMeOnCommentEvent = procedure(const Sender: PMeTokenizer; const Comment: string) of object;
  TMeTokenizer = Object(TMeDynamicObject)
  protected
    FIgnoreCase: Boolean;
    FBlankChars: TMeCharset;
    FEscapeChar: TMeTokenString;

    FSimpleTokens: PMeSimpleTokenTypes;
    FTokens: PMeComplexTokenTypes;

    //the Source text to tokenize
    FSource: PChar;
    FSourceEnd: PChar;
    FSourceSize: Integer;
    FCurrentToken: TMeToken;
    FNextToken: TMeToken;
    FErrors: PMeTokenErrors;
    FOnEscapedChar: TMeEscapedCharEvent;
    FOnComment: TMeOnCommentEvent;
  protected
    procedure Init;virtual; {override}
    procedure Reset;
    procedure SkipBlankChars(var aToken: TMeToken);
    function IsBlankChar(const aToken: TMeToken): Boolean;

    function GetErrors: PMeTokenErrors;
    procedure ConsumeToken(var aToken: TMeToken);
    function CompareTokenText(const S1, S2: PChar; const Len: Integer): Boolean;
    procedure DoEscapedChar(var EscapedStr: TMeTokenString);virtual;
    procedure DoComment(const Comment: string);virtual;

  public
    destructor Destroy; virtual;{override}

    function HasTokens: Boolean;
    function ReadToken: PMeToken;
    function NextToken: PMeToken;

    procedure LoadFromStream(const Stream: PMeStream);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromString(const aValue: string);
    procedure Clear;
    function DeQuotedString(const aToken: PMeToken): string;
  public
    property CurrentToken: TMeToken read FCurrentToken;
    {collect the Token Errors}
    property Errors: PMeTokenErrors read GetErrors;

    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;
    property BlankChars: TMeCharset read FBlankChars write FBlankChars;
    //this is EscapeChar for string. if empty means not used. process in the compiler. here only process to escape to the tokenEnd.
    property EscapeChar: TMeTokenString read FEscapeChar write FEscapeChar;

    //collect the simple token types
    property SimpleTokens: PMeSimpleTokenTypes read FSimpleTokens;
    //collect the string, comment token types like.
    property Tokens: PMeComplexTokenTypes read FTokens;
    property OnEscapedChar: TMeEscapedCharEvent read FOnEscapedChar write FOnEscapedChar;
    property OnComment: TMeOnCommentEvent read FOnComment write FOnComment;
  end;

implementation

const
  cMeSystemSimpleTokenTypes = [ttSpliter, ttBlockBegin, ttBlockEnd];
  cMeSystemComplexTokenTypes = [ttString, ttComment];

{ TMeToken }
procedure TMeToken.Assign(const aValue: PMeToken);
begin
  if Assigned(aValue) then
  begin
    Pos := aValue.Pos;
    TokenId := aValue.TokenId;
    TokenType:= aValue.TokenType;
    Size := aValue.Size;
    Line := aValue.Line;
    Col  := aValue.Col;
    LineEnd := aValue.LineEnd;
    ColEnd := aValue.ColEnd;
  end;
end;

function TMeToken.GetToken: string;
begin
  Result := '';
  if Assigned(Pos) and (Size > 0) then
  begin
    SetLength(Result, Size);
    Move(Pos^, Result[1], Size);
  end;
end;

function TMeToken.IsEmpty: Boolean;
begin
  Result := Pos = nil;
end;

procedure TMeToken.Reset;
begin
    Pos     := nil;
    Size    := 0;
    Line    := 1;
    Col     := 1;
    LineEnd := 1;
    ColEnd  := 1;
    TokenId := 0;
    TokenType := nil;
end;

{ TMeTokens }
destructor TMeTokens.Destroy;
begin
  FreePointers;
  inherited;
end;

procedure TMeTokens.Clear;
begin
  FreePointers;
  inherited;
end;

function TMeTokens.GetItem(const Index: Integer): PMeToken;
begin
  Result := Inherited Get(Index);
end;

{ TMeSimpleTokenTypes }
destructor TMeSimpleTokenTypes.Destroy;
begin
  FreePointers;
  inherited;
end;

procedure TMeSimpleTokenTypes.Clear;
begin
  FreePointers;
  inherited;
end;

procedure TMeSimpleTokenTypes.Add(const aTokenId: Integer; const aTokenName: string);
var
  vTokenItem: PMeSimpleTokenType;
begin
  New(vTokenItem);
  with vTokenItem^ do
  begin
    TokenType := ttSimpleToken;
    TokenId := aTokenId;
    Token := aTokenName;
  end;
  inherited Add(vTokenItem);
end;

{$IFDEF SUPPORTS_OVERLOAD}
procedure TMeSimpleTokenTypes.Add(const aTokenId: TMeCustomTokenType; const aTokenName: string);
begin
  Add(Ord(aTokenId), aTokenName);
end;

{$ENDIF}
procedure TMeSimpleTokenTypes.AddStandardToken(const aTokenId: TMeCustomTokenType; const aTokenName: string);
begin
  Add(Ord(aTokenId), aTokenName);
end;

function TMeSimpleTokenTypes.GetItem(const Index: Integer): PMeSimpleTokenType;
begin
  Result := Inherited Get(Index);
end;

{ TMeComplexTokenTypes }
destructor TMeComplexTokenTypes.Destroy;
begin
  FreePointers;
  inherited;
end;

procedure TMeComplexTokenTypes.Clear;
begin
  FreePointers;
  inherited;
end;

procedure TMeComplexTokenTypes.Add(const aTokenId: Integer; const aTokenBegin, aTokenEnd: string; const aFlags: TMeTokenFlags);
var
  vTokenItem: PMeComplexTokenType;
begin
  New(vTokenItem);
  with vTokenItem^ do
  begin
    TokenType  := ttComplexToken;
    TokenId    := aTokenId;
    Flags      := aFlags;
    Token      := aTokenBegin;
    TokenEnd   := aTokenEnd;
  end;
  inherited Add(vTokenItem);
end;

{$IFDEF SUPPORTS_OVERLOAD}
procedure TMeComplexTokenTypes.Add(const aTokenId: TMeCustomTokenType; const aTokenBegin, aTokenEnd: string; const aFlags: TMeTokenFlags);
begin
  Add(Ord(aTokenId), aTokenBegin, aTokenEnd, aFlags);
end;

{$ENDIF}
procedure TMeComplexTokenTypes.AddStandardToken(const aTokenId: TMeCustomTokenType; const aTokenBegin, aTokenEnd: string; const aFlags: TMeTokenFlags);
begin
  Add(Ord(aTokenId), aTokenBegin, aTokenEnd, aFlags);
end;

function TMeComplexTokenTypes.GetItem(const Index: Integer): PMeComplexTokenType;
begin
  Result := Inherited Get(Index);
end;

function TMeComplexTokenTypes.IndexOfId(const aId: Integer; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if aId = Items[Result].TokenId then exit;
  end;
  Result := -1;
end;

function TMeComplexTokenTypes.IndexOf(const aName: string; const aBeginIndex: Integer; const aIgnoreCase: Boolean): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if aIgnoreCase then
    begin
      if AnsiSameText(aName, Items[Result].Token) then exit;
    end
    else
      if aName = Items[Result].Token then exit;
  end;
  Result := -1;
end;

function TMeComplexTokenTypes.IndexOfEnd(const aName: string; const aBeginIndex: Integer; const aIgnoreCase: Boolean): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if aIgnoreCase then
    begin
      if AnsiSameText(aName, Items[Result].TokenEnd) then exit;
    end
    else
      if aName = Items[Result].TokenEnd then exit;
  end;
  Result := -1;
end;

{ TMeTokenErrorInfo }
function TMeTokenErrorInfo.ErrorInfo: string;
begin
  //1: %i: Line; 2: %i: Col; 3. %i: the ErrorCode; 4. %s: the ErrorToken;
  case ErrorCode of
    cMeTokenErrorMissedToken: 
    begin
      Result := Format(rsMeTokenErrorMissedToken, [Line, Col, ErrorCode, ErrorFmt]);
    end;
    cMeTokenErrorUnknownToken:
      Result := Format(rsMeTokenErrorUnknown, [Line, Col, ErrorCode, Token]);
    else if ErrorFmt <> '' then
      Result := Format(ErrorFmt, [Line, Col, ErrorCode, Token]);
  end;  //case
end;

{ TMeTokenErrors }
destructor TMeTokenErrors.Destroy;
begin
  FreePointers;
  inherited;
end;

procedure TMeTokenErrors.Clear;
begin
  FreePointers;
  inherited;
end;

procedure TMeTokenErrors.Add(const aToken: TMeToken; const aErrorCode: Integer; const aErrorFmt: ShortString);
var
  vItem: PMeTokenErrorInfo;
begin
  New(vItem);
  inherited Add(vItem);
  vItem.Assign(@aToken);
  vItem.ErrorCode := aErrorCode;
  vItem.ErrorFmt := aErrorFmt;
end;

function TMeTokenErrors.GetItem(const Index: Integer): PMeTokenErrorInfo;
begin
  Result := Inherited Get(Index);
end;

{ TMeTokenizer }
destructor TMeTokenizer.Destroy;
begin
  MeFreeAndNil(FSimpleTokens);
  MeFreeAndNil(FTokens);
  MeFreeAndNil(FErrors);
  if Assigned(FSource) then FreeMem(FSource);
  inherited;
end;

procedure TMeTokenizer.Init;
begin
  inherited;
  New(FSimpleTokens, Create);
  New(FTokens, Create);
end;

type
  TMeMemoryStreamAccess = Object(TMeMemoryStream)
  end;

function TMeTokenizer.CompareTokenText(const S1, S2: PChar; const Len: Integer): Boolean;
begin
  if IgnoreCase then
  begin
    Result := FastcodeCompareTextEx(S1, S2, Len) = 0;
        {
        GetMem(vPChar, Length(Token) + 1);
        GetMem(vPChar1, Length(Token) + 1);
        try
          Move(Token[1], vPChar^, Length(Token));
          Move(aToken.Pos^, vPChar1^, Length(Token));
          vPChar[Length(Token)] := #0;
          vPChar1[Length(Token)] := #0;
          vFound := AnsiStrComp(vPChar, vPChar1) = 0;
        finally
          FreeMem(vPChar);
          FreeMem(vPChar1);
        end;
        //}
  end
  else
    Result := CompareMem(S1, S2, Len);
end;

procedure TMeTokenizer.DoComment(const Comment: string);
begin
  if Assigned(FOnComment) then
    FOnComment(@Self, Comment);
end;

procedure TMeTokenizer.DoEscapedChar(var EscapedStr: TMeTokenString);
begin
  if Assigned(FOnEscapedChar) then
    FOnEscapedChar(@Self, EscapedStr);
end;

function TMeTokenizer.IsBlankChar(const aToken: TMeToken): Boolean;
begin
  with aToken do
    Result := {$IFDEF MBCS_SUPPORT} (StrByteType(Pos, 0) = mbSingleByte) and {$ENDIF}
      (Pos^ in BlankChars);
end;

procedure TMeTokenizer.SkipBlankChars(var aToken: TMeToken);
begin
  //try SkipChars
  with aToken do
    While (Integer(Pos) < Integer(FSourceEnd))
      {$IFDEF MBCS_SUPPORT} and (StrByteType(Pos, 0) = mbSingleByte) {$ENDIF}
      and ((Pos^ in BlankChars) or (Pos^ in cMeControlCharset)) do
    begin
      if (Pos^ = cLF) then
      begin
        Inc(Line);
        Col := 1;
      end
      else
      begin
        Inc(Col);
      end;
      LineEnd := Line;
      ColEnd := Col;
      Inc(Pos);
      //writeln('pos.char=', ord(pos^));
    end;
end;

procedure TMeTokenizer.Clear;
begin
  if Assigned(FSource) then FreeMem(FSource);
  FSource    := nil;
  FSourceEnd := nil;
  Reset;
end;

procedure TMeTokenizer.ConsumeToken(var aToken: TMeToken);
var
  i: Integer;
  vSize: Integer;
  vFound: Boolean;
  vPChar: PChar;
  {$IFDEF MBCS_SUPPORT} 
  j: Integer;
  {$ENDIF}
  label NextTk;
begin
  if Assigned(FSource) then
  begin
    with aToken do
    begin
      if not Assigned(Pos) then
      begin
        Pos := FSource;
      end
      else begin
NextTk:
        Inc(Pos, Size);
        if Integer(Pos) >= Integer(FSourceEnd) then 
        begin
          aToken.Pos := nil;
          exit;
        end;
        Col := ColEnd;
        Line := LineEnd;
      end;
    end; //with

    SkipBlankChars(aToken);
    vFound := False;

    //Ok, Now aToken.Pos is the non-blank PChar
    //check SimpleTokens
    for i := 0 to FSimpleTokens.Count - 1 do
    with FSimpleTokens.Items[i]^ do
      if Integer(FSourceEnd) - Integer(aToken.Pos) >= Length(Token) then
      begin
        if CompareTokenText(aToken.Pos, @Token[1], Length(Token)) then
        begin
          aToken.Size := Length(Token);
          aToken.TokenType := FSimpleTokens.Items[i];
          aToken.TokenId := TokenId;
          aToken.ColEnd  := aToken.Col + Length(Token);
          exit;
        end;
      end;

    //check the Complex Tokens now:
    for i := 0 to FTokens.Count - 1 do
      with FTokens.Items[i]^ do
      if Integer(FSourceEnd) - Integer(aToken.Pos) >= (Length(Token) + Length(TokenEnd)) then
      begin
        if CompareTokenText(aToken.Pos, @Token[1], Length(Token)) then
        begin //got the TokenBegin, now try to find the TokenEnd
          vPChar := aToken.Pos;
          Inc(vPChar, Length(Token));
          Inc(aToken.Size, Length(Token));
          Inc(aToken.ColEnd, Length(Token));

          //search the TokenEnd
          Repeat
            if (vPChar^ = cCR) or (vPChar^ = cLF) then
            begin
              if (tfOneLine in Flags) then
              begin
                vFound := TokenEnd = '';
                if not vFound then
                begin
                  aToken.Size := Integer(vPChar)- Integer(aToken.Pos);
                  aToken.TokenId := TokenId;
                  aToken.TokenType := FTokens.Items[i];
                  FErrors.Add(aToken, cMeTokenErrorMissedToken, TokenEnd);
                  exit;
                end;
              end;
              if not vFound then
              begin
                Inc(vPChar);
                if (vPChar^ = cLF) then 
                  Inc(vPChar);
                Inc(aToken.LineEnd);
                aToken.ColEnd := 1;
              end;
              continue;
            end;
            if (tfEscapeChar in Flags) and (EscapeChar <> '') and CompareMem(vPChar, @EscapeChar[1], Length(EscapeChar)) then
            begin
              //only Escape the EscapeChar and TokenEnd.
              Inc(vPChar, Length(EscapeChar));
              Inc(aToken.ColEnd, Length(EscapeChar));
              if (Integer(vPChar) + Length(EscapeChar) < FSourceSize + Integer(FSource)) and CompareMem(vPChar, @EscapeChar[1], Length(EscapeChar)) then
              begin
                Inc(vPChar, Length(EscapeChar));
                Inc(aToken.ColEnd, Length(EscapeChar));
                continue;
              end
              else if (TokenEnd <> '') and (Integer(vPChar) + Length(TokenEnd) < FSourceSize + Integer(FSource)) and CompareMem(vPChar, @TokenEnd[1], Length(TokenEnd)) then begin
                Inc(vPChar, Length(TokenEnd));
                Inc(aToken.ColEnd, Length(TokenEnd));
                continue;
              end;
            end;
            vFound := (TokenEnd <> '') and CompareTokenText(vPChar, @TokenEnd[1], Length(TokenEnd));
            if vFound then
            begin
              if (tfEscapeDuplicateToken in Flags) and (Integer(FSourceEnd) - Integer(@vPChar[Length(TokenEnd)]) >= Length(TokenEnd)) then
              begin
                if CompareTokenText(@vPChar[Length(TokenEnd)], @TokenEnd[1], Length(TokenEnd)) then
                begin
                  vFound := False;
                  Inc(vPChar, Length(TokenEnd));
                  Inc(aToken.ColEnd, Length(TokenEnd));
                end;
              end;
            end;
            if not vFound then
            begin
              //forward a char
              {$IFDEF MBCS_SUPPORT} 
              if StrByteType(vPChar, 0) = mbLeadByte then
              begin
                j := 0;
                while (Integer(vPChar) + j < Integer(FSourceEnd)) and (StrByteType(vPChar, j) <> mbTrailByte) do
                begin
                  Inc(j);
                end;
                Inc(vPChar, j);
                Inc(aToken.ColEnd, j);
              end;
              {$ENDIF}
              Inc(vPChar);
              Inc(aToken.ColEnd);
            end;
          Until vFound or (Integer(vPChar) >= FSourceSize + Integer(FSource));

          if vFound then
          begin
            aToken.Size := Integer(vPChar)- Integer(aToken.Pos) + Length(TokenEnd);
            aToken.TokenId := TokenId;
            aToken.TokenType := FTokens.Items[i];
            if Length(TokenEnd) > 1 then
              Inc(aToken.ColEnd, Length(TokenEnd)-1);
            if (TokenId = Ord(ttComment)) and ((@aToken = @FCurrentToken) or (@aToken = @FNextToken)) then
            begin
              DoComment(DeQuotedString(@aToken));
              Goto NextTk;
            end
            else
              exit;
          end;
        end;
      end; //FTokens

    //now treat this as the general token
    vPChar := aToken.Pos;
    Repeat
      {$IFDEF MBCS_SUPPORT} 
      if StrByteType(vPChar, 0) = mbLeadByte then
      begin
        j := 0;
        while (Integer(vPChar)+j < Integer(FSourceEnd)) and (StrByteType(vPChar, j) = mbSingleByte) do
        begin
          Inc(j);
        end;
        Inc(vPChar, j);
        Inc(aToken.ColEnd, j);
      end;
      {$ENDIF}
      Inc(vPChar);
      Inc(aToken.ColEnd);

      if (vPChar^ in cMeControlCharset) or (vPChar^ in FBlankChars) then
      begin
        vFound := True;
      end;
    Until vFound or (Integer(vPChar) >= FSourceSize + Integer(FSource));

    aToken.Size := Integer(vPChar)- Integer(aToken.Pos);
    aToken.TokenId := Ord(ttToken);
    aToken.TokenType := nil;

  end
  else
    aToken.Pos := nil;
end;

function TMeTokenizer.DeQuotedString(const aToken: PMeToken): string;
var
  vPChar: PChar;
  {$IFDEF MBCS_SUPPORT} 
  i: Integer;
  {$ENDIF}
  vEscapedChar: TMeTokenString;
begin
  Result := '';
  if Assigned(aToken.Pos) and Assigned(aToken.TokenType) and (aToken.TokenType.TokenType = ttComplexToken) then
  begin
    vPChar := aToken.Pos;
        with PMeComplexTokenType(aToken.TokenType)^ do
        begin
          Inc(vPChar, Length(Token));
          while (Integer(vPChar) < aToken.Size + Integer(aToken.Pos)) do
          begin
            if (vPChar^ = cCR) or (vPChar^ = cLF) then
            begin
                Result := Result + vPChar^;
                Inc(vPChar);
                if (vPChar^ = cLF) then 
                begin
                  Result := Result + vPChar^;
                  Inc(vPChar);
                end;
              continue;
            end;
            if (tfEscapeChar in Flags) and (EscapeChar <> '') and CompareMem(vPChar, @EscapeChar[1], Length(EscapeChar)) then
            begin
              //only Escape the EscapeChar and TokenEnd.
              Inc(vPChar, Length(EscapeChar));
              if (Integer(vPChar)  + Length(EscapeChar) < aToken.Size + Integer(aToken.Pos)) and CompareMem(vPChar, @EscapeChar[1], Length(EscapeChar)) then
              begin
                Result := Result + EscapeChar;
                Inc(vPChar, Length(EscapeChar));
              end
              else if (TokenEnd <> '') and (Integer(vPChar) + Length(TokenEnd) < aToken.Size + Integer(aToken.Pos)) and CompareMem(vPChar, @TokenEnd[1], Length(TokenEnd)) then begin
                Result := Result + TokenEnd;
                Inc(vPChar, Length(TokenEnd));
              end
              else begin
                vEscapedChar := '';
                {$IFDEF MBCS_SUPPORT}
                if StrByteType(vPChar, 0) = mbLeadByte then
                begin
                  i := 0;
                  while (Integer(vPChar)+i < aToken.Size + Integer(aToken.Pos)) and (StrByteType(vPChar, i) <> mbTrailByte) do
                  begin
                    vEscapedChar := vEscapedChar + vPChar[i];
                    Inc(i);
                  end;
                  Inc(vPChar, i);
                end;
                {$ENDIF}
                vEscapedChar := vEscapedChar + vPChar^;
                Inc(vPChar);
                DoEscapedChar(vEscapedChar);
                Result := Result + vEscapedChar;
              end;
              continue;
            end;
            if (TokenEnd <> '') and CompareTokenText(vPChar, @TokenEnd[1], Length(TokenEnd)) then
            begin
              Inc(vPChar, Length(TokenEnd));
              if (tfEscapeDuplicateToken in Flags) and (Integer(vPChar) + Length(TokenEnd) < aToken.Size + Integer(aToken.Pos)) and CompareTokenText(vPChar, @TokenEnd[1], Length(TokenEnd)) then
              begin
                Result := Result + TokenEnd;
                Inc(vPChar, Length(TokenEnd));
              end;
            end
            else
            begin
              //forward a char
              {$IFDEF MBCS_SUPPORT} 
              if StrByteType(vPChar, 0) = mbLeadByte then
              begin
                i := 0;
                while (Integer(vPChar)+i < aToken.Size + Integer(aToken.Pos)) and (StrByteType(vPChar, i) <> mbTrailByte) do
                begin
                  Result := Result + vPChar[i];
                  Inc(i);
                end;
                Inc(vPChar, i);
              end;
              {$ENDIF}
              Result := Result + vPChar^;
              Inc(vPChar);
            end;
          end; //while
        end;
  end
end;

function TMeTokenizer.GetErrors: PMeTokenErrors;
begin
  if not Assigned(FErrors) then New(FErrors, Create);
  Result := FErrors;
end;

function TMeTokenizer.HasTokens: Boolean;
begin
  Result := Assigned(FCurrentToken.Pos);
  if not Result then 
    Result := Assigned(ReadToken());
end;

procedure TMeTokenizer.LoadFromStream(const Stream: PMeStream);
var
  vStream: PMeMemoryStream;
begin
  New(vStream, Create);
  try
    vStream.LoadFromStream(Stream);
    FSourceSize := TMeMemoryStreamAccess(vStream^).FSize;
    if FSourceSize <> 0 then
    begin
      ReallocMem(FSource, FSourceSize+1);
      Move(vStream.Memory^, FSource^, FSourceSize);
      FSource[FSourceSize] := #0;
      FSourceEnd := @FSource[FSourceSize];
    end
    else
    begin
      FreeMem(FSource);
      FSource := nil;
      FSourceEnd := nil;
    end;
  finally
    vStream.Free;
  end;
  Reset;
end;

procedure TMeTokenizer.LoadFromFile(const FileName: string);
var
  vStream: PMeFileStream;
begin
  New(vStream, Create);
  try
    vStream.Open(FileName, fmOpenRead or fmShareDenyNone);
    FSourceSize := vStream.Size;
    if FSourceSize <> 0 then
    begin
      ReallocMem(FSource, FSourceSize+1);
      vStream.ReadBuffer(FSource^, FSourceSize);
      FSource[FSourceSize] := #0;
      FSourceEnd := @FSource[FSourceSize];
    end
    else
    begin
      FreeMem(FSource);
      FSource := nil;
      FSourceEnd := nil;
    end;
  finally
    vStream.Free;
  end;
  Reset;
end;

procedure TMeTokenizer.LoadFromString(const aValue: string);
begin
  FSourceSize := Length(aValue);
  if FSourceSize <> 0 then
  begin
    ReallocMem(FSource, FSourceSize+1);
    Move(aValue[1], FSource^, FSourceSize);
    FSource[FSourceSize] := #0;
    FSourceEnd := @FSource[FSourceSize];
  end
  else
  begin
    FreeMem(FSource);
    FSource := nil;
    FSourceEnd := nil;
  end;
  Reset;
end;

function TMeTokenizer.NextToken: PMeToken;
begin
  if not Assigned(FNextToken.Pos) then ConsumeToken(FNextToken);
  if Assigned(FNextToken.Pos) then
    Result := @FNextToken
  else
    Result := nil;
end;

function TMeTokenizer.ReadToken: PMeToken;
begin
  if Assigned(FNextToken.Pos) then
  begin
    FCurrentToken := FNextToken;
    FNextToken.Reset;
  end
  else
    ConsumeToken(FCurrentToken);
  if Assigned(FCurrentToken.Pos) then
    Result := @FCurrentToken
  else
    Result := nil;
end;
procedure TMeTokenizer.Reset;
begin
  FCurrentToken.Reset;
  FNextToken.Reset;
end;

initialization
end.
