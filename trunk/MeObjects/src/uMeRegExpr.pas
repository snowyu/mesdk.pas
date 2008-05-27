
{Summary the MeRegExpr extension object .}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.00 $

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
    * The Original Code is $RCSfile: uMeRegExpr.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Andrey V. Sorokin, St.Petersburg, Russia is Copyright (C) 1999-2004
    * Portions created by Riceball LEE is Copyright (C) 2003-2008
    * All rights reserved.

    * Contributor(s):
        Andrey V. Sorokin(RegExpr)
                                http://RegExpStudio.com
                                mailto:anso@mail.ru
}
unit uMeRegExpr;

interface

{$I MeSetting.inc}

uses
{$IFDEF MSWINDOWS}
  Windows, 
{$ENDIF}
  //TypInfo,
  SysUtils
  , RegExpr
  , uMeConsts
  , uMeSystem
  , uMeObject
  //, uMeYield
  , uMeCoroutine
  {$IFDEF DEBUG}
  , DbugIntf
  {$ENDIF}
  ;

type
{
将所有的正则表达式集中在一起
  Content=/():Count: SearchListBegin ():SearchList: SearchListEnd ():NextPageURL:/:1
  SearchListBegin = //
  SearchList = /():Field1 ():Field2/:n

冒号后面的数字表示执行匹配搜索的次数，如果是n表示一直搜索直到没有匹配的内容。
如果省略最后的冒号和数字，表示此表达式只搜索1次。

支持嵌套匹配以及匹配次数。
等号分隔匹配定义名称以及匹配内容: Name=Content[:n]
如果Content为引号内容，表示为简单字符串匹配（支持*号和?号通配符号，转义符号为\）： Name='my*.*':1
如果Content为"/"斜杠扩起的，则表示为正则匹配，可以定义子字段
如： Name =/hello(.*):myfield/:n


举一个复杂的例子：
<table>
<tr><th>性别</th><th>年龄</th><th>名字</th></tr>
<tr><td>Male</td><td>13</td><td>Rose</td></tr>
<tr><td>Female</td><td>20</td><td>Jacky</td></tr>
</table>

Content=//[ListBegin]/ Good /[List]//
ListBegin=/<tr><th>(.*):Sex</th><th>(.*):Age</th><th>(.*):Name</th></tr>/:1
List=/<tr><td>(.*):$[ListBegin.Sex]</td><td>(.*):$[ListBegin.Age]</td><td>(.*):$[ListBegin.Name]</td></tr>/:n

/[ListBegin]/ 为子表达式


    property MatchStrPos [const aSubExprName : RegExprString] : integer read GetMatchStrPos;
    property MatchStrLen [const aSubExprName : RegExprString] : integer read GetMatchStrLen;
    property MatchStr [const aSubExprName : RegExprString] : RegExprString read GetMatchStr;
    property SubExprMatchCount : integer read GetSubExprMatchCount;
    property SubExprNames[const index: integer]: RegExprString read GetSubExprName;
    Function GetSubExprIndexByName(const aSubExprName: RegExprString) : Integer;

}
  PMeCustomSimpleRegExpr = ^ TMeCustomSimpleRegExpr;
  PMeCustomRegExpr = ^ TMeCustomRegExpr;
  PMeRegExprs = ^ TMeRegExprs;

  TMeRegExprFoundEvent = procedure(const Sender: PMeCustomSimpleRegExpr) of object;
  TMeCustomSimpleRegExpr = object(TMeDynamicObject)
  protected
    FRegExpr: TRegExpr; //only available on the execution time.
    FName: RegExprString;
    FPattern: RegExprString;
    FExecCount: Integer;
    FOwner: PMeCustomRegExpr;
    FParent: PMeCustomRegExpr;
    FRoot: PMeCustomRegExpr;
    FInputString: RegExprString;
    FOnFound: TMeRegExprFoundEvent;

    procedure DoFound(const Sender: PMeCustomSimpleRegExpr);
    //function GetRegExpr: TRegExpr;
    //function GetExpression: RegExprString; virtual;
    function GetRoot: PMeCustomRegExpr;
    procedure SetPattern(const Value: RegExprString);virtual;
    procedure Init; virtual; //override;

    function iExecute(const aRegExpr: TRegExpr; var aPos: Integer): Boolean; virtual;
    procedure ApplyExpression(const aRegExpr: TRegExpr);virtual;

    property Root: PMeCustomRegExpr read GetRoot;
  public
    destructor Destroy; virtual; //override;
    function Execute(const aInputString: RegExprString; const aPos: Integer = 1): Boolean;overload;
    function Execute(): Boolean;overload;

    property Pattern: RegExprString read FPattern write SetPattern;
    //property RegExpr: TRegExpr read GetRegExpr;
    //the exec count for the expression, -1 means for ever until end.
    //the default is 1.
    property ExecCount: Integer read FExecCount write FExecCount;
    property InputString: RegExprString read FInputString write FInputString;
    property Name: RegExprString read FName write FName;
    property RegExpr: TRegExpr read FRegExpr;
    property OnFound: TMeRegExprFoundEvent read FOnFound write FOnFound;
  end;

  TMeRegExprs = object(TMeList)
  protected
    function GetItem(Index: Integer): PMeCustomSimpleRegExpr;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
    //procedure Assign(const aObjs: PMeNamedObjects);
    function IndexOf(const aName: RegExprString; const aBeginIndex: Integer = 0): Integer;
    function Find(const aName: RegExprString): PMeCustomSimpleRegExpr;
  public
    property Items[Index: Integer]: PMeCustomSimpleRegExpr read GetItem; default;
  end;

  TMeCustomRegExpr = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCustomSimpleRegExpr)
  protected
    FOrgExpression : RegExprString;
    //this is Regular Expressions to run
    FSubRegExprs: PMeRegExprs; //of TMeCustomSimpleRegExpr or TMeCustomRegExpr(Ref from FExpressions)
    //this is added Expressions
    FExpressions: PMeNamedObjects; //of TMeCustomRegExpr

    function CreateSimpleRegExpr: PMeCustomSimpleRegExpr;
    procedure ApplyExpression(const aRegExpr: TRegExpr);virtual; //override
    procedure SetPattern(const Value: RegExprString); virtual; //override
    procedure Init; virtual; //override;
    function iExecuteList(const aRegExpr: TRegExpr; var aPos: Integer): Boolean;
    function iExecute(const aRegExpr: TRegExpr; var aPos: Integer): Boolean; virtual; //override

  public
    destructor Destroy; virtual; //override;
    //Add a New varaible Expression define into Exprssions List
    function AddExpr(const aName: RegExprString; const aExpression: RegExprString; const aExecCount: Integer = 1): Integer;
  end;

const
   // '\/(.+?):Expression:\/(\:(\d+|n):ExecCount:)?';
   {1: Expression; 3: ExecCount}
  cMeExpressionPattern = '\/(.+?)\/(\:(\d+|n))?';
  //'\/\[(.+?):SubRegEx:\]\/';
  {1: SubExpressionName}
  cMeSubExpressionNamePattern = '\/\[(.+?)\]\/';

implementation

uses 
  RTLConsts, SysConst;

{ TMeCustomSimpleRegExpr }
procedure TMeCustomSimpleRegExpr.Init;
begin
  inherited;
  FExecCount := 1;
end;

destructor TMeCustomSimpleRegExpr.Destroy;
begin
  FPattern := '';
  //FreeAndNil(FRegExpr);
  inherited;
end;

procedure TMeCustomSimpleRegExpr.ApplyExpression(const aRegExpr: TRegExpr);
begin
  with aRegExpr do
  begin
    Expression := FPattern;
    Compile;
  end;
end;

procedure TMeCustomSimpleRegExpr.DoFound(const Sender: PMeCustomSimpleRegExpr);
begin
  if Assigned(FOnFound) then
    FOnFound(Sender)
  else if Assigned(FRoot) and Assigned(FRoot.FOnFound) then
    FRoot.FOnFound(Sender);
end;

function TMeCustomSimpleRegExpr.Execute(): Boolean;
begin
  Execute(FInputString);
end;

function TMeCustomSimpleRegExpr.Execute(const aInputString : RegExprString; const aPos: Integer): Boolean;
var
  vPos: Integer;
begin
  FRegExpr := TRegExpr.Create;
  try
    ApplyExpression(FRegExpr);
    FRegExpr.InputString := aInputString;
    vPos := aPos;
    Result := iExecute(FRegExpr, vPos);
  finally
    FRegExpr.Free;
    FRegExpr := nil;
  end;
end;

function TMeCustomSimpleRegExpr.iExecute(const aRegExpr: TRegExpr; var aPos: Integer): Boolean;
var
  vExecCount: Integer;
begin
  with aRegExpr do
  begin
    Result := ExecPos(aPos);
    if Result then
    begin
      //Yield(FRegExpr);
      //FParent.Yield;
      DoFound(@Self);
      vExecCount := FExecCount - 1;
      aPos := MatchPos[0] + MatchLen[0];
      while ((vExecCount > 0) or (FExecCount < 0)) and ExecNext do
      begin
        //Yield(FRegExpr);
        //FParent.Yield;
        DoFound(@Self);
        Dec(vExecCount);
        aPos := MatchPos[0] + MatchLen[0];
      end;
      Result := vExecCount <= 0;
      if not Result then
      begin
        Raise EMeError.Create('TMeCustomSimpleRegExpr.iExecute: the ExecCount is not enough left:' + IntToStr(vExecCount));
      end;
    end;
  end;
end;

{
function TMeCustomSimpleRegExpr.GetRegExpr: TRegExpr;
begin
  if not Assigned(FRegExpr) then
    FRegExpr := TRegExpr.Create;
  Result := FRegExpr;
end;
}

function TMeCustomSimpleRegExpr.GetRoot: PMeCustomRegExpr;
begin
  if Assigned(FRoot) then
    Result := FRoot
  else
    Result := @Self;
end;

procedure TMeCustomSimpleRegExpr.SetPattern(const Value: RegExprString);
begin  
  if FPattern <> Value then
    FPattern := Value;
end;

{ TMeCustomRegExpr }
procedure TMeCustomRegExpr.Init;
begin
  inherited;
  New(FSubRegExprs, Create);
  New(FExpressions, Create);
end;

destructor TMeCustomRegExpr.Destroy;
begin
  MeFreeAndNil(FSubRegExprs);
  MeFreeAndNil(FExpressions);
  inherited;
end;

function TMeCustomRegExpr.AddExpr(const aName: RegExprString; const aExpression: RegExprString; const aExecCount: Integer = 1): Integer;
var
  vExpr: PMeCustomRegExpr;
begin
  Result := FExpressions.IndexOf(aName);
  if Result < 0 then
  begin
    New(vExpr, Create);
    with vExpr^ do
    begin
      FOwner  := @Self;
      FParent := @Self;
      Name := aName;
      Pattern := aExpression;
      FExecCount := aExecCount;
    end;
    FExpressions.Add(vExpr);
  end
  else
    Result := -1;
end;

procedure TMeCustomRegExpr.ApplyExpression(const aRegExpr: TRegExpr);
begin
end;

function TMeCustomRegExpr.CreateSimpleRegExpr: PMeCustomSimpleRegExpr;
begin
  New(Result, Create);
  with Result^ do
  begin
    FParent := @Self;
    FRoot := Self.Root;
  end;
end;

function TMeCustomRegExpr.iExecuteList(const aRegExpr: TRegExpr; var aPos: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FSubRegExprs.Count - 1 do with FSubRegExprs.Items[i]^ do
  begin
    FRegExpr := aRegExpr;
    ApplyExpression(aRegExpr);
    Result := iExecute(aRegExpr, aPos);
    FRegExpr := nil;
    if not Result then
      Raise EMeError.Create('TMeCustomRegExpr.Exec: the SubExpr['+Name+'] is not match:'+Pattern);
  end;
end;

function TMeCustomRegExpr.iExecute(const aRegExpr: TRegExpr; var aPos: Integer): Boolean;
var
  vExecCount: Integer;
begin
  Result := FSubRegExprs.Count > 0;
  if Result then
  begin
    Result := iExecuteList(aRegExpr, aPos);
    if Result and (FExecCount > 1) then
    begin
      vExecCount := FExecCount - 1;
      while ((vExecCount > 0) or (FExecCount < 0)) and iExecuteList(aRegExpr, aPos) do
      begin
        //Yield(FRegExpr);
        Dec(vExecCount);
        //aPos := MatchPos[0] + MatchLen[0];
      end;
      Result := vExecCount <= 0;
      if not Result then
      begin
        Raise EMeError.Create('TMeCustomRegExpr.iExecute['+Name+']: the ExecCount is not enough left:' + IntToStr(vExecCount));
      end;
    end;
  end
  else
    Raise EMeError.Create('TMeCustomRegExpr.Exec: No Expression to execute!');
end;

procedure TMeCustomRegExpr.SetPattern(const Value: RegExprString);
var
  s: RegExprString;
  vRegExpr: PMeCustomSimpleRegExpr;
  vExpr: PMeCustomRegExpr;
  vExecCount: Integer;
  vPrevPos: Integer;
begin  
  if FPattern <> Value then
  begin
    FSubRegExprs.Clear;
  //1. AnsiExtractQuotedStr(var s: PChar; Quote: Char = '/'): string;
    with TRegExpr.Create do
    try
      //Expression := '\/(.+?):Expression:\/((\:(\d+|n):ExecCount:)|)';
      Expression := cMeExpressionPattern;
      if Exec(Value) then
      begin
        s := Match[1];
        vExecCount := StrToIntDef(Match[3], -1);
      end
      else
        Raise EMeError.Create('TMeCustomRegExpr.SetExpression: the  RegExprString format is error!');
      //s := AnsiExtractQuotedStr(PChar(Value), '/');
    //2. Search all the sub RegExpressions: 
      //RegExpr.Expression := '\/\[(.+?):SubRegEx:\]\/';
      Expression := cMeSubExpressionNamePattern;
      vPrevPos := 1;
      if Exec(s) then
        repeat
          s := Trim(System.Copy(InputString, vPrevPos, MatchPos[0] - vPrevPos));
          vPrevPos := MatchPos [0] + MatchLen [0];
          if s <> '' then
          begin
            vRegExpr := CreateSimpleRegExpr;
            vRegExpr.Pattern := s;
            FSubRegExprs.Add(vRegExpr);
          end;
          s := Trim(Match[1]);
          if s <>  '' then
          begin
            vExpr := PMeCustomRegExpr(FExpressions.Find(s));
            if not Assigned(vExpr) then
              Raise EMeError.Create('TMeCustomRegExpr.SetExpression: No Such SubRegExpression found:' + s);
            FSubRegExprs.Add(vExpr);
          end
          else
            Raise EMeError.Create('TMeCustomRegExpr.SetExpression: The SubRegExpression is empty!');
        until not ExecNext
      else
      begin
        vRegExpr := CreateSimpleRegExpr;
        vRegExpr.Pattern := s;
        vRegExpr.FExecCount := vExecCount;
        FSubRegExprs.Add(vRegExpr);
      end; //if
    finally
      Free;
    end;

    FPattern := Value;
  end;
end;

{ TMeRegExprs }
destructor TMeRegExprs.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMeRegExprs.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Assigned(FItems[I]) then with PMeCustomSimpleRegExpr(FItems[I])^ do
    begin
      if  not Assigned(FOwner) then
        Free;
    end;
    FItems[I] := nil;
  end;

  inherited Clear;
end;

function TMeRegExprs.Find(const aName: RegExprString): PMeCustomSimpleRegExpr;
var
  i: integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

function TMeRegExprs.IndexOf(const aName: RegExprString; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if {$IFDEF UniCode}WideSameText{$ELSE}AnsiSameText{$ENDIF}(aName, Items[Result].Name) then
      exit;
  end;
  Result := -1;
end;

function TMeRegExprs.GetItem(Index: Integer): PMeCustomSimpleRegExpr;
begin
  Result := Inherited Get(Index);
end;

initialization
  SetMeVirtualMethod(TypeOf(TMeCustomSimpleRegExpr), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeCustomRegExpr), ovtVmtParent, TypeOf(TMeCustomSimpleRegExpr));
  SetMeVirtualMethod(TypeOf(TMeRegExprs), ovtVmtParent, TypeOf(TMeNamedObjects));

  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeCustomSimpleRegExpr), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeCustomRegExpr), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeRegExprs), ovtVmtClassName, nil);
  {$ENDIF}
end.


