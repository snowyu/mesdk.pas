
{Summary MeScript - the MeSDK Script Engine Core.}
{
   @author  Riceball LEE<riceballl@hotmail.com>
   @version $Revision: 1.40 $



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
 * The Original Code is $RCSfile: uMeScript.pas,v $.
 *
 * The Initial Developers of the Original Code are Riceball LEE<riceballl@hotmail.com>.
 * Portions created by Riceball LEE<riceballl@hotmail.com> is Copyright (C) 2007
 * All rights reserved.
 *
 * Contributor(s):
 *
 *)
unit uMeScript;

interface

{$I Setting.inc}
{$Define PUREPASCAL}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeSystem
  , uMeObject
  , uMeScriptConsts
  , uMeTokenizer
  ;

const
  cSymbolErrorOk = 0;
  cSymbolErrorExpectScriptBlockType = 300;
  cSymbolErrorTokenizer = 200;
  //cSymbolErrorExpectArgsEnd = 301;
  //cSymbolErrorExpectArgSpliter = 302;
  cSymbolErrorFunctionNotFound      = 310;
  cSymbolErrorNameNotFound          = 311;
  cSymbolErrorAttributeNotFound     = 312;

type
  PMeScriptValue          = ^ TMeScriptValue;
  PMeScriptAttribute      = ^ TMeScriptAttribute;
  PMeScriptAttributes     = ^ TMeScriptAttributes;
  PMeScriptValues         = ^ TMeScriptValues;
  //PMeScriptCustomExecutor = ^ TMeScriptCustomExecutor;

  PMeScriptElement        = ^ TMeScriptElement;
  PMeScriptCustomBlock    = ^ TMeScriptCustomBlock;
  PMeScriptBlock          = ^ TMeScriptBlock;
  PMeScriptCustomFunction = ^ TMeScriptCustomFunction;
  PMeScriptCustomObject   = ^ TMeScriptCustomObject;
  PMeScriptObject         = ^ TMeScriptObject;
  PMeScriptFunction       = ^ TMeScriptFunction;
  PMeScriptArguments      = ^ TMeScriptObject;
  //PMeScriptArray          = ^ TMeScriptArray;
  PMeScriptGlobalFunction = ^ TMeScriptGlobalFunction;

  TMeScriptValueRecord = record
    case Integer of
      0: (
        case TMeScriptTypeKind of
          mtkString:     (VAnsiString: Pointer);
          mtkBoolean:    (VBool: Boolean);
          mtkNumber:     (VNumber: Double);
          mtkFunction:   (VFunc: PMeScriptCustomFunction);
          mtkObject:     (VObject: PMeScriptCustomObject);
      );
  end;

  TMeScriptElement = Object(TMeNamedObject)
  protected
    //procedure CompileError(const aErrCode: Integer);
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aObject: PMeNamedObject); virtual; {override}
  public
    {Summary the Parent owns this symbol. }
    Parent: PMeScriptElement;

    {Summary the element defined position in the src file}
    //Line: Integer;
    {<COMBINE Line>}
    //Column: Integer;

    {Summary the element name}
    property Name: String read FName write FName;
  end;

  TMeScriptValue = Object
  protected
    TypeKind: TMeScriptTypeKind;
  public
    procedure Clear;
    procedure Assign(const aValue: PMeScriptValue);
  public
    Ctor: PMeScriptCustomFunction;
    Value: TMeScriptValueRecord;
  end;

  TMeScriptDynaValues  = array of TMeScriptValue;
  TMeScriptDynaObjects = array of PMeScriptObject;

  TMeScriptAttribute = packed record
    Name: string;
    Value: TMeScriptValue;
    Kinds: byte; //TMeScriptAttributeKinds;
  end;

  TMeScriptAttributes = Object(TMeList)
  protected
    function GetItem(const Index: Integer): PMeScriptAttribute;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
    procedure Assign(const aSymbols: PMeScriptAttributes);
    function IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer; overload;
    function Find(const aName: string): PMeScriptAttribute;
  public
    property Items[const Index: Integer]: PMeScriptAttribute read GetItem; default;
  end;

  TMeScriptValues = Object(TMeList)
  protected
    function GetItem(const Index: Integer): PMeScriptValue;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
    procedure Assign(const aSymbols: PMeScriptValues);
  public
    property Items[const Index: Integer]: PMeScriptValue read GetItem; default;
  end;

  {: the abstract script execution block. }
  TMeScriptCustomBlock = Object(TMeScriptElement)
  protected
    FGlobalFunction: PMeScriptGlobalFunction;
  protected

    {## Compiler: }
    function IsCompileAllowed: Boolean; virtual;
    function iCompile: Integer; virtual;

    destructor Destroy; virtual; {override}

    {Summary Init before the Execution. }
    procedure InitExecution(const aParams: PMeScriptArguments); virtual;
    {Summary Execution}
    procedure iExecute();virtual;
    {Summary Finalize after the execution. }
    procedure FinalizeExecution; virtual;
  public
    procedure Assign(const aObject: PMeNamedObject); virtual; {override}
    procedure Execute();
    function Eval(const aParams: PMeScriptArguments): PMeScriptValue;
    //function Eval(const aParams: array of const): PMeScriptValue;

    property GlobalFunction: PMeScriptGlobalFunction read FGlobalFunction;
  end;

  {: 最小的脚本函数块}
  (*
  Add(2, Add(2,4))
    编译流程：
      如果我能将所有的都压入堆栈，就好处理了, 代码流如下:
      PushNum 2 : 
      
      PushNum 2
      PushNum 4
      Push Count //实际压入的参数个数
      Call Add
      Push Count  //实际压入的参数个数
      Call Add
    编译过程：
      aFunc := FindFunc('Add');
      if Assigned(aFunc) then
      begin
        vParams := ParserParams;
        for i := 0 to vParams.Count - 1 Double
        begin
          vParams.Items[i].ReferenceTo(Self);
        end;
        FBody.AddOpCode(opPush, vParams.Count);
        aFunc.ReferenceTo(Self);
      end;
    只编译函数序列：
      ff();
      ff();
      function xxx() {};

  *)
  TMeScriptBlock = Object(TMeScriptCustomBlock)
  protected
    {局部变量初始值, 不采用预先集中的方式，而是分散在代码流中，以 opNewVar 的形式，出现一个定义一个。
     由于局部变量仅当执行某函数块的时候才存在。所以在
    }
    //FVariables: array of TMeScriptValue;
    // the local variable count.
    //FVariableCount: Integer;
    //FConstants: TMeScriptDynaValues;
    FFunctions: PMeNamedObjects;
    FBody: PMeScriptCodeMemory;
    //for compile-time only, the index means the variable index.
    FVariablesName: array of string;
  protected
    function GetBody: PMeScriptCodeMemory;
    //function GetVariables: PTurboSymbols;
    function GetFunctions: PMeNamedObjects;
    procedure DoTokenError(const Sender: PMeTokenErrors; const aError: PMeTokenErrorInfo);

    procedure InitExecution(const aParams: PMeScriptArguments); virtual; {override}
    procedure iExecute();virtual; {override}
    procedure FinalizeExecution; virtual; {override}

    //以下是Parser.
    {}
    function iParser(const aTokenizer: PMeTokenizer): Boolean; virtual;
    //eg, aFunc(a,b);
    function iParserCallFunctionRef(const aTokenizer: PMeTokenizer): Boolean;
    //it's a localVar or attribute or constant
    function iParserValueRef(const aTokenizer: PMeTokenizer): Boolean;
    //eg, var var1, var2;
    function iParserDeclareVar(const aTokenizer: PMeTokenizer): Boolean;
    //eg, function aFunc(a,b) {};
    function iParserDeclareFunc(const aTokenizer: PMeTokenizer): Boolean;
    //function iParserNewBlock(const aTokenizer: PMeTokenizer): Boolean;


    //1. search Functions; 2. search Parent.Functions until Parent = nil.
    //only find function in attributes or Functions
    function FindFunction(const aName: string; const SearchAttr: Boolean = True): PMeScriptCustomFunction;
    function FindVariable(const aName: string; var aStackIndex: Integer): Integer;
  public
    destructor Destroy; virtual; {override}

    function Parser(const aSrc: string): Boolean; virtual;
  public
    //the local variables and initilization value if any.
    //property Variables: PTurboSymbols read GetVariables;
    property Functions: PMeNamedObjects read GetFunctions;
    property Body: PMeScriptCodeMemory read GetBody;
  end;

  { Summary this is an abstract class for internal function. }
  TMeScriptCustomFunction = Object(TMeScriptCustomBlock)
  protected
    //the defined arguments at compile-time. store the argument name.
    FArguments: PMeStrings;
  protected
    function GetArguments: PMeStrings;

    procedure InitExecution(const aParams: PMeScriptArguments); virtual; {override}
  public
    destructor Destroy; virtual; {override}
  public
    Prototype: PMeScriptCustomObject;
    property Arguments: PMeStrings read GetArguments;
  end;

  PMeScriptInternalFunction = ^ TMeScriptInternalFunction;
  {Summary 约定如果没有返回值则返回nil， nil表示undefined.}
  TMeScriptInternalFunctionType = function (const GlobalFunction: PMeScriptGlobalFunction): PMeScriptValue;
  TMeScriptInternalFunction = Object(TMeScriptCustomFunction)
  protected
    procedure iExecute();virtual; {override}
  public
    Func: TMeScriptInternalFunctionType;
  end;

  { Summary the user defined script function.}
  TMeScriptFunction = Object(TMeScriptBlock)
  protected
    //the defined arguments at compile-time. store the argument name.
    FArguments: PMeStrings;
  protected
    function GetArguments: PMeStrings;

    procedure InitExecution(const aParams: PMeScriptArguments); virtual; {override}
  public
    destructor Destroy; virtual; {override}
    { Summary Register Internal Function}
    procedure RegisterFunction(const aName: string; const aFunc: TMeScriptInternalFunctionType);
  public
    Prototype: PMeScriptCustomObject;
    property Arguments: PMeStrings read GetArguments;
  end;

  TMeScriptCustomObject = Object(TMeInterfacedObject)
  protected
    FGlobalFunction: PMeScriptGlobalFunction;

    function GetValue(const Name: string): PMeScriptValue;
    function GetItem(const Index: Integer): PMeScriptValue; virtual;abstract;

    procedure SetValue(const Name: string; const Value: PMeScriptValue);
    //it will search the Ctor.Prototype
    //Cloned it to attributes if it not found in attributes but found in it's Prototype.
    function FindAttribute(const Name: string; const Cloned: Boolean): PMeScriptAttribute;virtual;
  public
    //Ctor = nil means system Object.
    Ctor: PMeScriptCustomFunction; //constructor

    property GlobalFunction: PMeScriptGlobalFunction read FGlobalFunction;
    property Values[const Name: string]: PMeScriptValue read GetValue write SetValue;
    property Items[const Index: Integer]: PMeScriptValue read GetItem;
  end;

  TMeScriptObject = Object(TMeScriptCustomObject)
  protected
    FAttributes: PMeScriptAttributes;
  protected
    function GetAttributes: PMeScriptAttributes;
    function GetItem(const Index: Integer): PMeScriptValue; virtual; {override}

    function FindAttribute(const Name: string; const Cloned: Boolean): PMeScriptAttribute;virtual;{override}

  public
    destructor Destroy; virtual;{override}
  public
    property Attributes: PMeScriptAttributes read GetAttributes;
  end;

  //Return Stack Frame
  TMeScriptPC = record
    Mem: tsUInt; //it is the current FBody.Memory of the execute function
    //the current execute function.
    Func: PMeScriptBlock;
    //脚本函数参数,当进入某脚本函数将转换栈上的所有参数形成一个参数对象，退出函数前，注意释放该参数对象。
    Arguments: PMeScriptArguments;
    //当前函数运行实例的局部变量,退出时候注意释放！
    //局部变量编译后没有名字，没有后期绑定！
    Variables: PMeScriptValues;
    //SP: tsInt;
  end;
  TMeScriptGlobalFunction = Object(TMeScriptFunction)
  protected
    {所有数据栈的数据都是PMeScriptValue指针，运算是通过指针运算。}
    FDataStack: PMeScriptValues;
    {返回栈}
    FReturnStack: array of TMeScriptPC;
    //this 栈。this 指针的处理在进入函数前，后退出函数后，如果函数是对象调用的话！
    FThisPtrStack: PMeList;
    //_Func: PMeScriptBlock;
    _PC: TMeScriptPC;
    //the ReturnStack Pointer. it is the index of FReturnStack
    _RP: tsInt;
    //the DataStack Pointer. it is the index of FDataStack
    _SP: tsInt;
    //the DataStack Base Pointer. it is the index of FDataStack
    _BP: tsInt;
    //the current this pointer.
    _this: PMeScriptCustomObject;
  protected
    function GetIsRunning(): Boolean;
    procedure SetIsRunning(const Value: Boolean);
  protected
    procedure Init;virtual; {override}
    procedure iExecute();virtual; {override}
    procedure PushPC;
    procedure PopupPC;
  public
    destructor Destroy; virtual;{override}

    procedure Reset; 
    //function Execute(const aParams: PMeScriptArguments): PMeScriptValue;virtual; {override}
    procedure RaiseMeScriptError(const Sender: PMeScriptCustomFunction; const aErrorCode: Integer; const aMsg: string);
  public
    States: Byte; //TMeScriptProcessorStates; modified for FPC
    BindingMode: TMeScriptCompilerBindingMode;
    CompilerOptions: TMeScriptCompilerOptions;
    LastErrorCode: TMeScriptProcessorErrorCode;
    property DataStack: PMeScriptValues read FDataStack;
    //property Strings: PMeStrings read FStrings;
    {: the States in [psRunning, psStepping]}
    property IsRunning: Boolean read GetIsRunning write SetIsRunning;   
  end;

procedure ClearMeScriptDynaValues(aList: TMeScriptDynaValues);
procedure ClearMeScriptObjects(aList: TMeScriptDynaObjects);

implementation

type
  PMeScriptTokenizer = ^ TMeScriptTokenizer;
  TMeScriptTokenizer = Object(TMeTokenizer)
  protected
    procedure Init;virtual; {override}
    procedure DoEscapedChar(var EscapedStr: TMeTokenString);virtual; {override}
  public
  end;

  TMeVMInstructionProc = procedure(const aGlobalFunction: PMeScriptGlobalFunction);
  //the Core VM instructions List
  {: 核心虚拟指令表 } 
  TMeScriptCoreWords = array [TMeVMInstruction] of TMeVMInstructionProc;

var
  GMeScriptCoreWords: TMeScriptCoreWords;

{$IFDEF PUREPASCAL}
  {$I MeScript_Interpreter_inc.pas}
{$ELSE}
  {$I MeScript_X86Interpreter_inc.pas}
{$ENDIF}

//get dynamic Arguments from DataStack and Generate Arguments Object
function GenerateArgumentsObject(const aGlobalFunction: PMeScriptGlobalFunction): PMeScriptArguments;
var
  vCount: Integer;
begin
  New(Result, Create);
  with aGlobalFunction^ do
  begin
    Dec(_SP);
    vCount := Integer(FDataStack.Items[_SP]);
    with Result.Attributes^ do 
      while vCount > 0 do
      begin
        Dec(vCount);
        Dec(_SP);
        Add(FDataStack.Items[_SP]);
      end;
  end;
end;

{ TMeScriptTokenizer }
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
  tkMeTokenMemberBegin = '[';
  tkMeTokenMemberEnd = ']';

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
  ttMemberBegin   = cMeCustomTokenType + 6; //后期绑定的属性成员，或数组定义
  ttMemberEnd     = cMeCustomTokenType + 7;

  cMeTokenErrorExpectArgsEnd = 3;

ResourceString
  rsMeTokenErrorExpectArgsEnd = '(%i:%i) Fatal: %i the %s is expected. ';
  rsMeScriptAlreayRunningError = 'MeScript Alreay Running Error';

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
  FSimpleTokens.Add(ttMemberBegin, tkMeTokenMemberBegin);
  FSimpleTokens.Add(ttMemberEnd, tkMeTokenMemberEnd);

  FTokens.AddStandardToken(ttString, tkMeTokenString1Limiter, tkMeTokenString1Limiter, [tfEscapeChar]);
  FTokens.AddStandardToken(ttString, tkMeTokenString2Limiter, tkMeTokenString2Limiter, [tfEscapeChar]);
  FTokens.AddStandardToken(ttComment, tkMeTokenLineComment, '', [tfOneLine]);
  FTokens.AddStandardToken(ttComment, tkMeTokenCommentBegin, tkMeTokenCommentEnd);

  FBlankChars := tkMeTokenBlankChars;
  FEscapeChar := tkMeTokenEscapeChar;
end;

procedure TMeScriptTokenizer.DoEscapedChar(var EscapedStr: TMeTokenString);
begin
  if EscapedStr = 'r' then
    EscapedStr := sLineBreak;
end;

procedure ClearMeScriptDynaValues(aList: TMeScriptDynaValues);
var
  i: Integer;
begin
  for i := 0 to High(aList) do
  begin
    aList[i].Clear;
  end;
end;

procedure ClearMeScriptObjects(aList: TMeScriptDynaObjects);
var
  i: Integer;
begin
  for i := 0 to High(aList) do
  begin
    MeFreeAndNil(aList[i]);
  end;
end;

{ TMeScriptElement }
destructor TMeScriptElement.Destroy;
begin
  //Name := '';
  Inherited;
end;

procedure TMeScriptElement.Assign(const aObject: PMeNamedObject);
begin
  if Assigned(aObject) and aObject.InheritsFrom(TypeOf(TMeScriptElement)) then
    with PMeScriptElement(aObject)^ do
    begin
      Self.Name   := Name;
      Self.Parent := Parent;
      //Self.Line   := Line;
      //Self.Column := Column;
    end;
  //Inherited;
end;

{ TMeScriptValue }
procedure TMeScriptValue.Assign(const aValue: PMeScriptValue);
begin
  if Assigned(aValue) then
  begin
    TypeKind := aValue.TypeKind;
    case TypeKind of
      mtkString: string(Value.VAnsiString) := string(aValue.Value.VAnsiString);
      mtkObject: 
      begin
        if Assigned(Value.VObject) then Value.VObject.Free;
        Value.VObject := aValue.Value.VObject;
        if Assigned(Value.VObject) then Value.VObject.AddRef;
      end;
    end;
  end
  else
    Clear;
end;

procedure TMeScriptValue.Clear;
begin
  case TypeKind of
    mtkString: string(Value.VAnsiString) := '';
    mtkObject: MeFreeAndNil(Value.VObject);
  end;
  TypeKind := mtkUndefined;
end;

{ TMeScriptCustomBlock }
destructor TMeScriptCustomBlock.Destroy;
begin
  Inherited;
end;

procedure TMeScriptCustomBlock.Assign(const aObject: PMeNamedObject);
begin
  if Assigned(aObject) and aObject.InheritsFrom(TypeOf(TMeScriptCustomBlock)) then
    with PMeScriptCustomBlock(aObject)^ do
    begin
      Self.FGlobalFunction   := FGlobalFunction;
    end;
  Inherited;
end;

procedure TMeScriptCustomBlock.Execute();
begin
  //if not IsCompiled then exit;
  InitExecution(nil);
  try
    iExecute();
  finally
    FinalizeExecution();
  end;
end;

function TMeScriptCustomBlock.Eval(const aParams: PMeScriptArguments): PMeScriptValue;
begin
  //Result := nil;
  //if not IsCompiled then exit;
  InitExecution(aParams);
  try
    iExecute();
  finally
    FinalizeExecution();
  end;
  with FGlobalFunction^ do if _SP > 0 then
  begin
    Result := FDataStack.Items[_SP-1];
  end
  else
    Result := nil;
end;

procedure TMeScriptCustomBlock.FinalizeExecution;
begin
end;

procedure TMeScriptCustomBlock.iExecute();
begin
end;

procedure TMeScriptCustomBlock.InitExecution(const aParams: PMeScriptArguments);
begin
end;

function TMeScriptCustomBlock.iCompile: Integer;
begin
  Result := cSymbolErrorOk;
end;

function TMeScriptCustomBlock.IsCompileAllowed: Boolean;
begin
  //Result := (FRefCount > 0) or (not Assigned(Parent)) or (Assigned(Parent) and IsPublic(Parent.Module));
  Result := True;
end;

{ TMeScriptBlock }
destructor TMeScriptBlock.Destroy;
begin
  //MeFreeAndNil(FVariables);
  //FConstants
  SetLength(FVariablesName, 0);
  MeFreeAndNil(FFunctions);
  MeFreeAndNil(FBody);
  Inherited;
end;

function TMeScriptBlock.FindFunction(const aName: string; const SearchAttr: Boolean): PMeScriptCustomFunction;
var
  vParent: PMeScriptElement;
begin
  if SearchAttr then
  begin
    PMeScriptValue(vParent) := FGlobalFunction._this.Values[aName];
    if Assigned(vParent) then
    begin
      if (PMeScriptValue(vParent).TypeKind = mtkFunction) then
        Result := PMeScriptValue(vParent).Value.VFunc
      else begin
        Result := nil;
        FGlobalFunction.RaiseMeScriptError(@Self, cSymbolErrorFunctionNotFound, aName);
      end;
      Exit;
    end;
  end;

  Result := PMeScriptCustomFunction(FFunctions.Find(aName));
  vParent := Parent;
  while (Result = nil) and (vParent <> nil) do
  begin
    if vParent.InheritsFrom(TypeOf(TMeScriptBlock)) then
      Result := PMeScriptCustomFunction(PMeScriptBlock(vParent).FFunctions.Find(aName));
    vParent := vParent.Parent;
  end;
end;

function TMeScriptBlock.FindVariable(const aName: string; var aStackIndex: Integer): Integer;
var
  vParent: PMeScriptElement;
begin
	//search local variables
  for Result := 0 to Length(FVariablesName) - 1 do
  begin
    if FVariablesName[Result] = aName then exit;
  end;
  //search parent local variables
  Result := -1;
  vParent := Parent;
  while (Result = -1) and (vParent <> nil) do
  begin
    if vParent.InheritsFrom(TypeOf(TMeScriptBlock)) then with TMeScriptBlock(vParent^) do
    begin
      Inc(aStackIndex);
      for Result := 0 to Length(FVariablesName) do
      begin
        if FVariablesName[Result] = aName then exit;
      end;
      Result := -1;
    end;
    vParent := vParent.Parent;
  end; //while
end;

function TMeScriptBlock.GetBody: PMeScriptCodeMemory;
begin
  if FBody = nil then
    New(FBody, Create);
  Result := FBody;
end;

//Pushstring param1, ..., PushInt32 ParamCount opCall xxx
function TMeScriptBlock.iParserCallFunctionRef(const aTokenizer: PMeTokenizer): Boolean;
var
  vToken, vNextToken: PMeToken;
  vFuncName: string;
  i, vParamCount: Integer;
begin
  with aTokenizer^ do
  begin
    vFuncName := CurrentToken.Token;

    vToken := ReadToken();
    Result := Assigned(vToken) and (vToken.TokenId = ttArgsBegin);

    if Result then
    begin 
      vParamCount := 0;
        while Assigned(vToken) and Result do
        begin
          vToken := ReadToken();
          case vToken.TokenId of
            Ord(ttToken):
              begin
                try
                  Body.AddOpPushDouble(StrToFloat(vToken.Token));
                except
                  Result := iParserValueRef(aTokenizer);
                end;
              end;
            Ord(ttString):
              begin
                Body.AddOpPushString(DeQuotedString(vToken));
              end;
            ttArgsEnd:
              begin
                break;
              end;
            else begin
            end;
            //ttArgSpliter:
          end; //case
          Inc(vParamCount);
          vNextToken := NextToken();
          Result := Result and Assigned(vNextToken) and ((vNextToken.TokenId = ttArgSpliter) or (vNextToken.TokenId = ttArgsEnd));
          if Result then
          begin
            if vNextToken.TokenId = ttArgSpliter then
              vToken := ReadToken();
          end
          else
            Errors.Add(vToken^, cMeTokenErrorMissedToken, tkMeTokenArgsEnd);
        end; //while

        if Result then
        begin
          Body.AddOpCode(opPush, vParamCount);
          //reuse vParamCount as stackIndex
          i := FindVariable(vFuncName, vParamCount);
          if i >= 0 then
          begin
          	//vParamCount is stackIndex
            if vParamCount = 0 then
              Body.AddOpCode(opLoadVar, i)
            else begin
              Body.AddOpCode(opLoadVarFar, vParamCount);
              Body.AddInt(i);
            end;
            Body.AddOpCode(opCall);
          end
          else if (FGlobalFunction.BindingMode = cbmLaterBinding) then
          begin
            Body.AddOpBind(opCallBind, vFuncName);
          end
          else begin
            PMeScriptCustomFunction(vNextToken) := FindFunction(vFuncName);
            if Assigned(vNextToken) then 
              Body.AddOpCode(opCallFunc, Integer(vNextToken))
            else if (FGlobalFunction.BindingMode = cbmAutoBinding) then
            begin
              Body.AddOpBind(opCallBind, vFuncName);
            end
            else 
            begin
              Result := False;
              Errors.Add(vToken^, cMeTokenErrorMissedToken, 'function '+vFuncName);
            end;
          end;
        end;
    end
    else begin
      Errors.Add(vToken^, cMeTokenErrorMissedToken, tkMeTokenArgsBegin);
      Result := False;
    end; //if
  end; //with
end;

function TMeScriptBlock.iParserDeclareVar(const aTokenizer: PMeTokenizer): Boolean;
begin
end;

{
  1、查属性对象
  2、查局部变量
  3、查函数，返回的是函数地址。
}
function TMeScriptBlock.iParserValueRef(const aTokenizer: PMeTokenizer): Boolean;
var
  vToken: PMeToken;
  vP: PMeScriptValue;
  vName: string;
  I: Integer;
begin
  with aTokenizer^ do
  begin
    vName := CurrentToken.Token;
    PMeToken(vP) := NextToken();
    Result := Assigned(vP) and (PMeToken(vP).TokenId = ttArgsBegin);
    if Result then
    begin
      //是函数调用
      Result := iParserCallFunctionRef(aTokenizer);
    end
    else if FGlobalFunction.BindingMode = cbmLaterBinding then
    begin
      Body.AddOpBind(opLoadBind, vName);
      Result := True;
    end
    else begin
      //1、查属性对象
      vP := FGlobalFunction._this.Values[vName];
      Result := Assigned(vP);
      if Result then
      begin
        Body.AddOpCode(opPush, Integer(vP));
        exit;
      end;
      //2、查局部变量
      I := FindVariable(vName, Integer(vP));
      Result := I >= 0;
      if Result then
      begin
        if Integer(vP) = 0 then
          Body.AddOpCode(opLoadVar, I)
        else begin
          Body.AddOpCode(opLoadVarFar, Integer(vP));
          Body.AddInt(I);
        end;
        exit;
      end;
      //3.查函数地址
      vP := PMeScriptValue(FindFunction(vName, False));
      Result := Assigned(vP);
      if Result then
        Body.AddOpCode(opPush, Integer(vP))
      else if FGlobalFunction.BindingMode = cbmAutoBinding then
      begin
        Body.AddOpBind(opLoadBind, vName);
        Result := True;
      end
      else begin
        FGlobalFunction.RaiseMeScriptError(@Self, cSymbolErrorNameNotFound, vName);
      end;
    end;
  end;
end;

function TMeScriptBlock.iParserDeclareFunc(const aTokenizer: PMeTokenizer): Boolean;
begin
end;

{function TMeScriptBlock.iParserNewBlock(const aTokenizer: PMeTokenizer): Boolean;
var
  vToken, vNextToken: PMeToken;
  vBlock: PMeScriptBlock;
  i, vParamCount: Integer;
begin
  with aTokenizer^ do
  begin
    Result := CurrentToken.TokenId = Ord(ttBlockBegin);
    if Result then
    begin
      vToken := ReadToken();
      while vToken
    end
    else begin
      Errors.Add(CurrentToken, cMeTokenErrorMissedToken, tkMeTokenBlockBegin);
    end;
  end;
end;
}

(*
 parser:
   Function();
   value;
   or block: {}
   
*)
function TMeScriptBlock.iParser(const aTokenizer: PMeTokenizer): Boolean;
var
  vToken, vNextToken: PMeToken;
  vBlock: PMeScriptBlock;
begin
  with aTokenizer^ do
  begin
    Result := HasTokens() and (CurrentToken.TokenId = Ord(ttBlockBegin));
    if Result then
    repeat
      vToken := ReadToken();
      if Assigned(vToken) then
      case vToken.TokenId of
        Ord(ttToken): //it should be a function name or var|attribute name.
          begin
            vNextToken := NextToken;
            case vNextToken.TokenId of
              ttArgsBegin:  //it's a Function-Ref
                begin
                  Result := iParserCallFunctionRef(aTokenizer);
                end;
              else begin
                Result := iParserValueRef(aTokenizer);
              end;
            end;
          end;
        Ord(ttSpliter):
          begin
            //SKip
          end;
        Ord(ttBlockBegin):
          begin
            New(vBlock, Create);
            try
              vBlock.FGlobalFunction := FGlobalFunction;
              Result := vBlock.iParser(aTokenizer);
            except
              MeFreeAndNil(vBlock);
              Result := False;
            end;
            if Result then
            begin
              vBlock.Parent := @Self;
              Functions.Add(vBlock);
              FBody.AddOpCode(opCallBlock, Integer(vBlock));
            end;
          end;
        Ord(ttBlockEnd): break;
        ttDeclareVar:
          begin
            Result := iParserDeclareVar(aTokenizer);
          end;
        ttDeclareFunc:
          begin
            Result := iParserDeclareFunc(aTokenizer);
          end;
        else begin
          Errors.Add(vToken^, cMeTokenErrorUnknownToken, vToken^.Token);
          Result := False;
        end;
      end; //case
      if not Result then exit;
    until vToken = nil
    else begin
      Errors.Add(CurrentToken, cMeTokenErrorMissedToken, tkMeTokenBlockBegin);
    end;
  end; //if
  if Result then
  begin
    Result := Assigned(vToken) and (vToken.TokenId = Ord(ttBlockEnd) );
    if not Result then with aTokenizer^ do
      Errors.Add(CurrentToken, cMeTokenErrorMissedToken, tkMeTokenBlockEnd);
  end;
end;

procedure TMeScriptBlock.DoTokenError(const Sender: PMeTokenErrors; const aError: PMeTokenErrorInfo);
begin
  //if Assigned(aError) then
  FGlobalFunction.RaiseMeScriptError(@Self, cSymbolErrorTokenizer, aError.ErrorInfo);
end;

function TMeScriptBlock.Parser(const aSrc: string): Boolean;
var
  vTokenizer: PMeScriptTokenizer;
begin
  New(vTokenizer, Create);
  try
    vTokenizer.Errors.OnError := DoTokenError;
    vTokenizer.LoadFromString(aSrc);
    Result := iParser(vTokenizer);
  finally
    vTokenizer.Free;
  end;
end;

procedure TMeScriptBlock.FinalizeExecution;
begin
  with FGlobalFunction._PC do 
  begin
    MeFreeAndNil(Variables);
    MeFreeAndNil(Arguments);
    //FGlobalFunction.
  end;
  FGlobalFunction.PopupPC;
end;

procedure TMeScriptBlock.iExecute();
begin
  iVMNext(FGlobalFunction);
end;

procedure TMeScriptBlock.InitExecution(const aParams: PMeScriptArguments);
begin
  FGlobalFunction.PushPC;
  with FGlobalFunction._PC do 
  begin
    Func := @Self;
    Mem  := tsUInt(Body.Memory);
    Arguments := aParams;
    New(Variables, Create);
    //SP := FGlobalFunction._SP;
  end;
end;

function TMeScriptBlock.GetFunctions: PMeNamedObjects;
begin
  if not Assigned(FFunctions) then
    New(FFunctions, Create);
  Result := FFunctions;
end;

{ TMeScriptCustomFunction }
destructor TMeScriptCustomFunction.Destroy;
begin
  MeFreeAndNil(FArguments);
  Inherited;
end;

function TMeScriptCustomFunction.GetArguments: PMeStrings;
begin
  if not Assigned(FArguments) then
    New(FArguments, Create);
  Result := FArguments;
end;

procedure TMeScriptCustomFunction.InitExecution(const aParams: PMeScriptArguments);
begin
  Inherited InitExecution(aParams);
  if not Assigned(aParams) then //the internal calling
  begin
    FGlobalFunction._PC.Arguments := GenerateArgumentsObject(FGlobalFunction);
  end;
end;

{ TMeScriptInternalFunction }
procedure TMeScriptInternalFunction.iExecute();
var
  vResult: PMeScriptValue;
begin
  vResult := Func(FGlobalFunction);
  FGlobalFunction.DataStack.Add(vResult);
end;

{ TMeScriptFunction }
destructor TMeScriptFunction.Destroy;
begin
  MeFreeAndNil(FArguments);
  Inherited;
end;

function TMeScriptFunction.GetArguments: PMeStrings;
begin
  if not Assigned(FArguments) then
    New(FArguments, Create);
  Result := FArguments;
end;

procedure TMeScriptFunction.InitExecution(const aParams: PMeScriptArguments);
begin
  Inherited InitExecution(aParams);
  if not Assigned(aParams) then //the internal calling
  begin
    FGlobalFunction._PC.Arguments := GenerateArgumentsObject(FGlobalFunction);
  end;
end;

procedure TMeScriptFunction.RegisterFunction(const aName: string; const aFunc: TMeScriptInternalFunctionType);
var
  vFunc: PMeScriptInternalFunction;
begin
  if (aName <> '') and Assigned(aFunc) then
  begin
    New(vFunc, Create);
    vFunc.Name := aName;
    vFunc.Func := aFunc;
    Functions.Add(vFunc);
  end;
end;

{ TMeScriptCustomObject }
function TMeScriptCustomObject.FindAttribute(const Name: string; const Cloned: Boolean): PMeScriptAttribute;
begin
  Result := nil;
end;

function TMeScriptCustomObject.GetValue(const Name: string): PMeScriptValue;
begin
  PMeScriptAttribute(Result) := FindAttribute(Name, coAttributeReadClone in FGlobalFunction.CompilerOptions);
  if Assigned(Result) then
    with PMeScriptAttribute(Result)^ do
      Result := @Value;
end;

procedure TMeScriptCustomObject.SetValue(const Name: string; const Value: PMeScriptValue);
var
  vItem: PMeScriptAttribute;
begin
  vItem := FindAttribute(Name, True);
  if Assigned(vItem) then
    vItem.Value.Assign(Value)
  else begin
    FGlobalFunction.RaiseMeScriptError(Ctor, cSymbolErrorAttributeNotFound, Name);
  end;
end;

{ TMeScriptObject }
destructor TMeScriptObject.Destroy;
begin
  MeFreeAndNil(FAttributes);
  Inherited;
end;

function TMeScriptObject.FindAttribute(const Name: string; const Cloned: Boolean): PMeScriptAttribute;
var
  vCtor: PMeScriptCustomFunction;
begin
  Result := FAttributes.Find(Name);
  vCtor := Ctor;
  if not Assigned(Result) then
  begin
    while not Assigned(Result) and Assigned(vCtor) and Assigned(vCtor.Prototype) do
    begin
      //search Prototype
      Result := vCtor.Prototype.FindAttribute(Name, Cloned);
      PMeScriptCustomObject(vCtor) := vCtor.Prototype;
      if Assigned(vCtor) then
        vCtor := PMeScriptCustomObject(vCtor).Ctor;
    end;
    if Assigned(Result) and Cloned then
    begin
      PMeScriptAttribute(vCtor) := New(PMeScriptAttribute);
      PMeScriptAttribute(vCtor).Name := Result.Name;
      PMeScriptAttribute(vCtor).Value.Assign(@Result.Value);
      PMeScriptAttribute(vCtor).Kinds := Result.Kinds;
      Attributes.Add(vCtor);
      Result := PMeScriptAttribute(vCtor);
    end;
  end;

end;

function TMeScriptObject.GetAttributes: PMeScriptAttributes;
begin
  if not Assigned(FAttributes) then
    New(FAttributes, Create);
  Result := FAttributes;
end;

function TMeScriptObject.GetItem(const Index: Integer): PMeScriptValue;
begin
  with FAttributes.Items[Index]^ do
    Result := @Value;
end;

{ TMeScriptGlobalFunction }
destructor TMeScriptGlobalFunction.Destroy;
begin
  MeFreeAndNil(FThisPtrStack);
  MeFreeAndNil(FDataStack);
  //MeFreeAndNil(FStrings);
  SetLength(FReturnStack, 0);
  //SetLength(FArgumentsStack, 0);
  Inherited;
end;

procedure TMeScriptGlobalFunction.Init;
begin
  Inherited;
  New(FDataStack, Create);
  //New(FStrings, Create);
  New(FThisPtrStack, Create);
  SetLength(FReturnStack, cMeScriptMaxReturnStackSize);
  //SetLength(FArgumentsStack, cMeScriptMaxReturnStackSize);
  FDataStack.Count := cMeScriptMaxDataStackSize;
end;

procedure TMeScriptGlobalFunction.iExecute();
begin
  if IsRunning then
    raise EMeError.CreateRes(@rsMeScriptAlreayRunningError);
  Reset;
  iVMNext(@Self);
end;

function TMeScriptGlobalFunction.GetIsRunning(): Boolean;
begin
  {$IFDEF FPC}
  Result := TMeScriptProcessorStates(LongWord(States)) * [psRunning, psStepping] <> [] ;
  {$ELSE Borland}
  Result := TMeScriptProcessorStates(States) * [psRunning, psStepping] <> [] ;
  {$ENDIF}
end;

procedure TMeScriptGlobalFunction.PushPC;
begin
  FReturnStack[_RP] := _PC;
  Assert(_RP< Length(FReturnStack), '_RP is exceed the ReturnStack MaxSize');
  Inc(_RP);
end;

procedure TMeScriptGlobalFunction.PopupPC;
begin
  Assert(_RP > 0, '_RP is exceed the ReturnStack MinSize');
  Dec(_RP);
  _PC := FReturnStack[_RP];
end;

procedure TMeScriptGlobalFunction.RaiseMeScriptError(const Sender: PMeScriptCustomFunction; const aErrorCode: Integer; const aMsg: string);
begin
end;

procedure TMeScriptGlobalFunction.Reset; 
begin
  if not IsRunning then
  begin
    SetLength(FReturnStack, cMeScriptMaxReturnStackSize);
    //SetLength(FArgumentsStack, cMeScriptMaxReturnStackSize);
    FDataStack.Clear;
    FDataStack.Count := cMeScriptMaxDataStackSize;
    _SP := 0; //Integer(ParamStackTop) + ParamStackSize * SizeOf(tsInt);
    _RP := 0; //Integer(ReturnStackTop) + ReturnStackSize * SizeOf(Pointer);
    _BP := 0;
    _PC.Func   := nil;
    _PC.Mem := tsUInt(FBody.Memory);
    LastErrorCode := errNone;
  end;
end;

procedure TMeScriptGlobalFunction.SetIsRunning(const Value: Boolean);
{$IFDEF FPC}
var
  vStates: TMeScriptProcessorStates;
{$ENDIF}

begin
  {$IFDEF FPC}
  vStates := TMeScriptProcessorStates(LongWord(States));
  {$ENDIF}

  if Value then
    {$IFDEF FPC}
    Include(vStates, psRunning)
    {$ELSE Borland}
    Include(TMeScriptProcessorStates(States), psRunning)
    {$ENDIF}
  else
    {$IFDEF FPC}
    Exclude(vStates, psRunning);
    {$ELSE Borland}
    Exclude(TMeScriptProcessorStates(States), psRunning);
    {$ENDIF}

  {$IFDEF FPC}
  States := Byte(LongWord(vStates));
  {$ENDIF}
end;

{ TMeScriptValues }
destructor TMeScriptValues.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMeScriptValues.Clear;
var
  i: Integer;
  vItem: PMeScriptValue;
begin
  for i := 0 to Count - 1 do
  begin
    vItem := Items[i];
    if Assigned(vItem) then vItem.Clear;
  end;
  FreePointers;
  inherited;
end;

procedure TMeScriptValues.Assign(const aSymbols: PMeScriptValues);
var
  i: integer;
  vItem: PMeScriptValue;
begin
  if Assigned(aSymbols) then
  begin
    Clear;
    Count := aSymbols.Count;
    for i := 0 to Count - 1 do
    begin
      New(vItem);
      vItem.Assign(aSymbols.Items[i]);
      Add(vItem);
    end;
  end;
end;

function TMeScriptValues.GetItem(const Index: Integer): PMeScriptValue;
begin
  Result := Inherited Get(Index);
end;

{ TMeScriptAttributes }
destructor TMeScriptAttributes.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMeScriptAttributes.Clear;
var
  i: Integer;
  vItem: PMeScriptAttribute;
begin
  for i := 0 to Count - 1 do
  begin
    vItem := Items[i];
    vItem^.Value.Clear;
  end;
  FreePointers;
  inherited;
end;

procedure TMeScriptAttributes.Assign(const aSymbols: PMeScriptAttributes);
var
  i: integer;
  vItem: PMeScriptAttribute;
begin
  if Assigned(aSymbols) then
  begin
    Clear;
    Count := aSymbols.Count;
    for i := 0 to Count - 1 do
    begin
      New(vItem);
      vItem^.Name := aSymbols.Items[i]^.Name;
      vItem.Value.Assign(@aSymbols.Items[i]^.Value);
      Add(vItem);
    end;
  end;
end;

function TMeScriptAttributes.GetItem(const Index: Integer): PMeScriptAttribute;
begin
  Result := Inherited Get(Index);
end;

function TMeScriptAttributes.Find(const aName: string): PMeScriptAttribute;
var
  i: integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

function TMeScriptAttributes.IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if AnsiSameText(aName, Items[Result].Name) then
      exit;
  end;
  Result := -1;
end;

initialization
  SetMeVirtualMethod(TypeOf(TMeScriptElement), ovtVmtParent, TypeOf(TMeNamedObject));
  SetMeVirtualMethod(TypeOf(TMeScriptCustomBlock), ovtVmtParent, TypeOf(TMeScriptElement));
  SetMeVirtualMethod(TypeOf(TMeScriptBlock), ovtVmtParent, TypeOf(TMeScriptCustomBlock));
  SetMeVirtualMethod(TypeOf(TMeScriptCustomFunction), ovtVmtParent, TypeOf(TMeScriptCustomBlock));
  SetMeVirtualMethod(TypeOf(TMeScriptInternalFunction), ovtVmtParent, TypeOf(TMeScriptCustomFunction));
  SetMeVirtualMethod(TypeOf(TMeScriptFunction), ovtVmtParent, TypeOf(TMeScriptCustomFunction));
  SetMeVirtualMethod(TypeOf(TMeScriptCustomObject), ovtVmtParent, TypeOf(TMeInterfacedObject));
  SetMeVirtualMethod(TypeOf(TMeScriptObject), ovtVmtParent, TypeOf(TMeScriptCustomObject));
  
  //SetMeVirtualMethod(TypeOf(TMeScriptCustomExecutor), ovtVmtParent, TypeOf(TMeDynamicObject));

  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeScriptElement), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeScriptBlock), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeScriptCustomFunction), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeScriptInternalFunction), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeScriptFunction), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeScriptCustomObject), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeScriptObject), ovtVmtClassName, nil);
  //SetMeVirtualMethod(TypeOf(TMeScriptCustomExecutor), ovtVmtClassName, nil);
  {$ENDIF}

  InitMeScriptCoreWordList;
end.
