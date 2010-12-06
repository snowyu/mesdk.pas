unit uMeTokenizerTest;

{$I MeSetting.inc}

{.$DEFINE Debug_WriteToConsole_Support}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, //QueryPerformanceCounter
  {$ENDIF}
  {$IFDEF DEBUG}
  DbugIntf,
  {$ENDIF}
  Classes,
  SysUtils,
  TypInfo,
  IniFiles,
  TestFramework
  , uMeSystem
  , uMeObject
  , uMeException
  , uMeStrUtils
  , uMeTokenizer
  ;

type
  TMeReadTokenEvent = procedure(const Sender: TTestCase; const aToken: TMeToken);
  TTest_MeTokenizer = class(TTestCase)
  protected
    FTokenizer: PMeTokenizer;
    //collect the fact tokens.
    FFactTokens: PMeTokens;
    FExpectComments: PMeStrings;
    FFactComments: PMeStrings;

    procedure Setup;override;
    procedure TearDown;override;

    //walk through and save tokens to FFactTokens.
    procedure WalkThroughTokens;
    procedure WalkThroughReadToken;
    procedure Test(const aSource: string; const aExpects: PMeTokens; const IgnoreComment: Boolean = False);
    procedure TestFromFile(const aIniFile: string; const IgnoreComment: Boolean = False);

    procedure DoComment(const Sender: PMeTokenizer; const Comment: string);
  public
    procedure CheckToken(const aExcept, aFact: TMeToken);
  published
    procedure Test_ConsumeToken;
    procedure Test_ReadToken;
  end;
  TTest_MeTokenErrors = class(TTestCase)
  protected
    FErrors: PMeTokenErrors;
  end;

implementation

uses
  FastcodeCPUID;

var
  AppPath: string;

{ TMeScriptTokenizer }
type
  PMeScriptTokenizer = ^ TMeScriptTokenizer;
  TMeScriptTokenizer = Object(TMeTokenizer)
  protected
    procedure Init;virtual; {override}
  public
    function TokenString(const aToken: TMeToken): string;
  end;
  PMeTokenEx = ^ TMeTokenEx;
  TMeTokenEx = Object(TMeToken)
    Dequoted: ShortString;
  end;

const
  tkMeTokenSpliter = ';';
  tkMeTokenBlockBegin = '{';
  tkMeTokenBlockEnd = '}';
  tkMeTokenString1Limiter = '''';
  tkMeTokenString2Limiter = '"';
  tkMeTokenLineComment = '//';
  tkMeTokenCommentBegin = '/*';
  tkMeTokenCommentEnd = '*/';
  tkMeTokenEscapeChar = '\';
  //default blank chars:to skip
  tkMeTokenBlankChars : TMeCharset = [' '];
  tkMeTokenAssignment = '=';
  tkMeTokenVar  = 'var';
  tkMeTokenFunc = 'function';
  tkMeTokenArgsBegin = '(';
  tkMeTokenArgsEnd = ')';
  tkMeTokenArgSpliter = ',';

const
  //赋值
  ttAssignment  = cMeCustomTokenType;
  //声明（局部）变量
  ttDeclareVar  = cMeCustomTokenType + 1;
  //声明函数
  ttDeclareFunc = cMeCustomTokenType + 2;
  ttArgsBegin   = cMeCustomTokenType + 3;
  ttArgsEnd     = cMeCustomTokenType + 4;
  ttArgSpliter  = cMeCustomTokenType + 5;

procedure TMeScriptTokenizer.Init;
begin
  Inherited;
  FSimpleTokens.AddStandardToken(ttSpliter, tkMeTokenSpliter);
  FSimpleTokens.AddStandardToken(ttBlockBegin, tkMeTokenBlockBegin);
  FSimpleTokens.AddStandardToken(ttBlockEnd, tkMeTokenBlockEnd);


  FSimpleTokens.Add(ttAssignment, tkMeTokenAssignment);
  FSimpleTokens.Add(ttDeclareVar, tkMeTokenVar);
  FSimpleTokens.Add(ttDeclareFunc, tkMeTokenFunc);
  FSimpleTokens.Add(ttArgsBegin, tkMeTokenArgsBegin);
  FSimpleTokens.Add(ttArgsEnd, tkMeTokenArgsEnd);
  FSimpleTokens.Add(ttArgSpliter, tkMeTokenArgSpliter);

  FTokens.AddStandardToken(ttString, tkMeTokenString1Limiter, tkMeTokenString1Limiter,[tfEscapeChar]);
  FTokens.AddStandardToken(ttString, tkMeTokenString2Limiter, tkMeTokenString2Limiter, [tfEscapeDuplicateToken]);
  FTokens.AddStandardToken(ttComment, tkMeTokenLineComment, '', [tfOneLine]);
  FTokens.AddStandardToken(ttComment, tkMeTokenCommentBegin, tkMeTokenCommentEnd);

  FBlankChars := tkMeTokenBlankChars;
  FEscapeChar := tkMeTokenEscapeChar;
end;

function TMeScriptTokenizer.TokenString(const aToken: TMeToken): string;
var
  vTokenType: string;
begin
  with aToken do
  begin
    if (TokenId > Ord(High(TMeCustomTokenType))) or (TokenId < Ord(Low(TMeCustomTokenType))) then
      vTokenType := 'CustomTokenType:'+IntToStr(TokenId)
    else
      vTokenType := GetEnumName(TypeInfo(TMeCustomTokenType), TokenId);
    Result := 'Token="'+ Token + '"; Type=' + vTokenType
      + '; Size=' + IntToStr(Size)
      + '; Line=' + IntToStr(Line) + '; Col=' + IntToStr(Col) + '; LineEnd=' + IntToStr(LineEnd) + '; ColEnd=' + IntToStr(ColEnd)
    ;
    if Assigned(TokenType) then
    begin
      Result := Result + ' TypeObj:' + GetEnumName(TypeInfo(TMeTokenType), Ord(TokenType.TokenType));
      if TokenType.TokenType = ttComplexToken then
        Result := Result + ' DeQuote:|'+ DeQuotedString(@aToken)+'|';
    end;
  end;
end;

{ TTest_MeTokenizer }
procedure TTest_MeTokenizer.Setup;
begin
  FTokenizer := New(PMeScriptTokenizer, Create);
  New(FFactTokens, Create);
  New(FExpectComments, Create);
  New(FFactComments, Create);
  FTokenizer.OnComment := DoComment;
end;

procedure TTest_MeTokenizer.TearDown;
begin
  MeFreeAndNil(FTokenizer);
  MeFreeAndNil(FFactTokens);
  MeFreeAndNil(FExpectComments);
  MeFreeAndNil(FFactComments);
end;

{
procedure TTest_MeTokenizer.DoReadToken(const aToken: TMeToken);
begin
  if Assigned(FOnReadToken) then
    FOnReadToken(aToken);
end;
}
procedure TTest_MeTokenizer.DoComment(const Sender: PMeTokenizer; const Comment: string);
begin
  FFactComments.Add(StringReplace(Comment, sLineBreak, '#13#10', [rfReplaceAll]));
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn('Comment=',Comment);
  {$ENDIF}
end;

procedure TTest_MeTokenizer.CheckToken(const aExcept, aFact: TMeToken);
begin
  CheckEquals(aExcept.Size, aFact.Size, aExcept.Token+ ' the Token Size is error.');
  CheckEquals(aExcept.Token, aFact.Token, 'the Token string is error.');
  CheckEquals(aExcept.TokenId, aFact.TokenId, 'the TokenId is error.');
  //CheckEquals(GetEnumName(TypeInfo(TMeTokenType), Ord(aExcept.TokenType)), GetEnumName(TypeInfo(TMeTokenType), Ord(aFact.TokenType)), aExcept.Token+ ' the Token Type is error.');
  CheckEquals(aExcept.Line, aFact.Line, aExcept.Token+' the Token Line is error.');
  CheckEquals(aExcept.Col, aFact.Col, aExcept.Token+ ' the Token Col is error.');
  CheckEquals(aExcept.LineEnd, aFact.LineEnd, aExcept.Token+' the Token LineEnd is error.');
  CheckEquals(aExcept.ColEnd, aFact.ColEnd, aExcept.Token+' the Token ColEnd is error.');
  if PMeTokenEx(@aExcept).DeQuoted <> '' then
  begin
    CheckEquals(PMeTokenEx(@aExcept).DeQuoted, FTokenizer.DeQuotedString(@aFact), aExcept.Token+' the Token DeQuotedString is error.');
  end;
end;

function StrToCharset(const aValue: string): TMeCharset;
var
  i: Integer;
begin
  Result := [];
  for i := 1 to Length(aValue) do
  begin
    Include(Result, aValue[i]);
  end;
end;

function CharsetToStr(const aValue: TMeCharset): string;
var
  c: char;
begin
  Result := '';
  for c := #0 to #$FF do
   if c in aValue then
     Result := Result + c;
end;

function StrToToken(aValue: string; const aSrc: string): PMeTokenEx;
var
  i: Integer;
  s: string;
begin
  New(Result);
  i := Pos(',', aValue);
  if i < 0 then Raise EMeError.Create('TokenPos Miss,');
  s := trim(Copy(aValue, 1, i-1));
  Delete(aValue, 1, i);
  Result.Pos := Pointer(StrToInt(s)+Integer(@aSrc[1]) -1);

  i := Pos(',', aValue);
  if i < 0 then Raise EMeError.Create('TokenSize Miss,');
  s := trim(Copy(aValue, 1, i-1));
  Delete(aValue, 1, i);
  Result.Size := StrToInt(s);

  i := Pos(',', aValue);
  if i < 0 then Raise EMeError.Create('TokenType Miss,');
  s := trim(Copy(aValue, 1, i-1));
  Delete(aValue, 1, i);
  if s[1] = 't' then
    Result.TokenId := GetEnumValue(TypeInfo(TMeCustomTokenType), s)
  else
    Result.TokenId := StrToInt(s);

  i := Pos(',', aValue);
  if i < 0 then Raise EMeError.Create('Token Line Miss,');
  s := trim(Copy(aValue, 1, i-1));
  Delete(aValue, 1, i);
  Result.Line := StrToInt(s);

  i := Pos(',', aValue);
  if i < 0 then Raise EMeError.Create('Token Col Miss,');
  s := trim(Copy(aValue, 1, i-1));
  Delete(aValue, 1, i);
  Result.Col := StrToInt(s);

  i := Pos(',', aValue);
  if i < 0 then Raise EMeError.Create('Token LineEnd Miss,');
  s := trim(Copy(aValue, 1, i-1));
  Delete(aValue, 1, i);
  Result.LineEnd := StrToInt(s);

  i := Pos(',', aValue);
  if i > 0 then
  begin
    s := trim(Copy(aValue, 1, i-1));
    Delete(aValue, 1, i);
  end
  else begin
    s := trim(aValue);
    aValue := '';
  end;
  Result.ColEnd := StrToInt(s);

  s := aValue;
  Result.DeQuoted := StrDelphiUnEscape(s);

end;

{
the INI file:
#this is a comments
[result]
#pos,size,type,line,col,lineEnd,colEnd
1,18,ttComment

[src]
//Coment 中话 2323
bbb 好的


}
procedure TTest_MeTokenizer.TestFromFile(const aIniFile: string; const IgnoreComment: Boolean);
var
  vIni: TMemIniFile;
  vSrc, s: string;
  vStrs: TStringList;
  vExpectTokens: PMeTokens;
  i: integer;
begin
  if not FileExists(aIniFile) then
    Raise EMeError.Create(aIniFile+ ' not found.');

  vIni := TMemIniFile.Create(aIniFile);
  vStrs := TStringList.Create();
  with vIni do
  try
    FTokenizer.IgnoreCase := ReadBool('config', 'IgnoreCase', FTokenizer.IgnoreCase);
    s := '"' + FTokenizer.EscapeChar + '"';
    FTokenizer.EscapeChar := AnsiDequotedStr(ReadString('config', 'EscapeChar', s), '"');
    s := '"'+CharsetToStr(FTokenizer.BlankChars)+'"';
    s := AnsiDequotedStr(ReadString('config', 'BlankChars', s), '"');
    FTokenizer.BlankChars := StrToCharset(s);
    //writeln('BlankChars:"', s, '"');
    ReadSectionValues('src', vStrs);
    vSrc := trim(vStrs.Text);
    if vSrc = '' then 
      raise EMeError.Create('the src is empty');
    ReadSectionValues('result', vStrs);
    New(vExpectTokens, Create);
    try
      for i := 0 to vStrs.Count - 1 do
      begin
        s := Trim(vStrs[i]);
        if (s <> '') and (s[1] <> '#') then
          vExpectTokens.Add(StrToToken(s, vSrc));
      end;
      ReadSectionValues('comments', vStrs);
      FExpectComments.Clear;
      for i := 0 to vStrs.Count - 1 do
      begin
        s := AnsiDeQuotedStr(vStrs[i], '"');
        if (s <> '') and (s[1] <> '#') then
          FExpectComments.Add(s);
      end;

      Test(vSrc, vExpectTokens, IgnoreComment);
    finally
      vExpectTokens.Free;
    end;
  finally
    vIni.Free;
    vStrs.Free;
  end;
end;

procedure TTest_MeTokenizer.WalkThroughTokens;
var
  vToken: TMeToken;
  vItem: PMeToken;
  i: Integer;
begin
  i := 0;
  FFactTokens.Clear;
  with PMeScriptTokenizer(FTokenizer)^ do
  begin
    vToken.Reset;
    repeat
      //vToken := ReadToken();
      ConsumeToken(vToken);
      if Assigned(vToken.Pos) then
      begin
        {$IFDEF Debug_WriteToConsole_Support}
        Status(TokenString(vToken));
        {$ENDIF}
        New(vItem);
        vItem.Assign(@vToken);
        FFactTokens.Add(vItem);
        
        //if Assigned(OnReadToken) then OnReadToken(Self, vToken^);
      end;
      inc(i);
      //if i > 5 then exit;
    until vToken.Pos = nil;
  end;
  //CheckEquals(0, i, 'the '+aStr+' result is error.');
end;

procedure TTest_MeTokenizer.WalkThroughReadToken;
var
  vToken: PMeToken;
  vItem: PMeToken;
  vNext: TMeToken;
  i: Integer;
begin
  i := 0;
  FFactTokens.Clear;
  with PMeScriptTokenizer(FTokenizer)^ do
  begin
    vNext.Pos := nil;
    repeat
      vToken := ReadToken();
      if Assigned(vToken) then
      begin
        if Assigned(vNext.Pos) then
          CheckToken(vToken^, vNext);
        vItem := NextToken;
        if Assigned(vItem) then
        begin
          vNext := vItem^;
        end
        else
          vNext.Pos := nil;
        {$IFDEF Debug_WriteToConsole_Support}
        Status(TokenString(vToken^));
        {$ENDIF}
        New(vItem);
        vItem.Assign(vToken);
        FFactTokens.Add(vItem);
        
        //if Assigned(OnReadToken) then OnReadToken(Self, vToken^);
      end;
      inc(i);
      //if i > 5 then exit;
    until vToken = nil;
  end;
  //CheckEquals(0, i, 'the '+aStr+' result is error.');
end;

procedure TTest_MeTokenizer.Test(const aSource: string; const aExpects: PMeTokens; const IgnoreComment: Boolean);
var
  i,j: Integer;
begin
  with FTokenizer^ do
  begin
    LoadFromString(aSource);
  end;
  if IgnoreComment then
    WalkThroughReadToken
  else
    WalkThroughTokens;
  
  //CheckEquals(aExpects.Count, FFactTokens.Count, 'the tokens count error');
  j := 0;
  for i := 0 to aExpects.Count - 1 do
  begin
    CheckTrue(j < FFactTokens.Count, 'the tokens count error');
    if not IgnoreComment or (aExpects.Items[i].TokenId <> Ord(ttComment)) then
    begin
      CheckToken(aExpects.Items[i]^, FFactTokens.Items[j]^);
      Inc(j);
    end;
  end;

  if IgnoreComment then
  begin
    CheckEquals(FExpectComments.Count, FFactComments.Count, 'the tokens count error');
    for i := 0 to FExpectComments.Count - 1 do
    begin
      CheckEquals(FExpectComments.Items[i], FFactComments.Items[i], 'the comment['+IntToStr(i)+'] error');
    end;
  end;
end;

procedure TTest_MeTokenizer.Test_ReadToken;
begin
  TestFromFile('dat\tokenizer\comment.txt', True);
end;

procedure TTest_MeTokenizer.Test_ConsumeToken;
var
  vSrc: string;
  vToken: TMeToken;
begin
  TestFromFile('dat\tokenizer\comment.txt', False);
(*
  vSrc := '//Coment 中话 2323'#13#10'bbb 好的';
  FTokenizer.LoadFromString(vSrc);
  WalkThroughTokens;
  CheckEquals(3, FTokens.Count, 'the tokens count error');
  
  with vToken do
  begin
    Pos := @vSrc[1];
    Size := 18;
    TokenType:= Ord(ttComment);
    Line := 1;
    Col := 1;
    LineEnd := 1;
    ColEnd := 19;
  end;
  CheckToken(vToken, FTokens.Items[0]^);

  with vToken do
  begin
    Pos := @vSrc[21];
    Size := 3;
    TokenType:= Ord(ttToken);
    Line := 2;
    Col := 1;
    LineEnd := 2;
    ColEnd := 4;
  end;
  CheckToken(vToken, FTokens.Items[1]^);

  with vToken do
  begin
    Pos := @vSrc[25];
    Size := 4;
    TokenType:= Ord(ttToken);
    Line := 2;
    Col := 5;
    LineEnd := 2;
    ColEnd := 9;
  end;
  CheckToken(vToken, FTokens.Items[2]^);
*)
end;

Initialization

  AppPath := ExtractFilePath(ParamStr(0));
  RegisterTests('MeScripts suites',
                [
                 TTest_MeTokenizer.Suite
                 //, TTest_MeTokenizer.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
