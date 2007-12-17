unit uMeScriptConsts;

interface

{$I Setting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeObject
  ;

const
  cMeScriptMaxReturnStackSize = 512;
  cMeScriptMaxDataStackSize   = 4096;

type
  tsInt = Integer;
  tsUInt = Longword;
  {
    @param cbmAutoBinding    设置该参数则为自动决策是否为前期还是后期绑定：当在编译时刻能找到attr的定义则该符号前期绑定，否则该符号作为后期绑定处理。
    @param cbmManualBinding  only xxx['attr'] means later-binding
                            设置该参数则仅当xxx['attr']的形式方为后期绑定，xxx.attr总是前期绑定（在编译时刻找不到attr则报错）；
    @param cbmLaterBinding   设置该参数则为所有的符号全部作为后期绑定。
  }
  TMeScriptCompilerBindingMode = (cbmAutoBinding, cbmManualBinding, cbmLaterBinding);
  {
  @param coHintFunctionArgCountError 当函数的参数个数不匹配的时候，提示而不是终止。
  @param coAttributeReadClone        当读对象的属性时，如果属性只存在于ProtoType，则clone复制到属性中，否则仅当写的时候发生复制。
  }
  TMeScriptCompilerOption = (coHintFunctionArgCountError, coAttributeReadClone);
  TMeScriptCompilerOptions = set of TMeScriptCompilerOption;
  TMeScriptTypeKind = (mtkUndefined, mtkNumber, mtkBoolean, mtkString, mtkFunction, mtkObject);
  TMeScriptAttributeKind = (akReadOnly, akDontEnum, akDontDelete);
  TMeScriptAttributeKinds = set of TMeScriptAttributeKind;
  TMeScriptProcessorState = (psHalt, psRunning, psStepping, psCompiling
     , psHaltError
  );
  TMeScriptProcessorStates = set of TMeScriptProcessorState;
  PTMeScriptProcessorErrorCode = ^ TMeScriptProcessorErrorCode;
  {
    @param errOutMem 代码区内存无可用的空间
    @param errOutOfMetaData MetaData区已无可用的空间
  }
  TMeScriptProcessorErrorCode = (errNone, errBadInstruction, errInstructionBadParam, errDizZero
    , errModuleNotFound, errMethodNotFound, errTypeInfoNotFound
    , errStaticFieldNotFound, errFieldNotFound
    , errOutOfMem
    , errOutOfMetaData
    , errOutOfDataStack, errOutOfReturnStack
    , errAssertionFailed  
  );

  PMeVMInstruction = ^ TMeVMInstruction;
  TMeVMInstruction = (
    opNoop
    , opHalt       // ( -- )
    , opCallBlock   // opCallBlock pBlock (  --  )
    , opCallFunc    // opCallFunc pFunction ( Arguments -- pResultValue )
    , opCall        // opCall ( Arguments pFuncValue -- pResultValue )
    , opCallBind    // opCallBind <Len:byte>FuncNameStr ( Arguments -- pResultValue ) the runtime error raised if function is not exists at runtime
    , opObjectBind  // opObjectBind <Len:byte>ObjNameStr ( -- pObject)
    , opLoadAttrById   // opLoadAttrById ( pObject <Id:Int32> -- pValue)  if not found return nil
    , opLoadAttr // opLoadAttr ( pObject <Len:Byte>AttrNameStr -- pValue) if not found return nil
    , opAssign     // opAssign pVar ( pValue -- )
    , opPush       // opPush Int32 ( -- Int32)
    , opPushDouble // opPushDouble Double ( -- pValue)
    , opPushString // opPushString <cnt:Longword>string ( -- pValue)
    , opPushObject // opPushObject pObject ( -- pValue)
    , opPushFunction // opPushFunction pFunc ( -- pValue)
    , opPop        // opPop         (Int32 -- )
    , opLoadArg    // opLoadArg <index> ( -- pValue)  load local argument
    , opLoadArgFar // opLoadArgFar <stackIndex> <index> ( -- pValue)  load parent argument which argument in return stack:  FReturnStack[_RP - stackIndex].Arguments.Attributes[index]
    , opLoadVar    // opLoadVar <index> (-- pValue)  load loal varaible
    , opLoadVarFar // opLoadVarFar <stackIndex> <index> ( -- pValue)  load parent varaible which varaible in return stack:  FReturnStack[_RP - stackIndex].Varaibles.Items[index]
    , opLoadBind   // opCallBind <Len:byte>NameStr ( -- pValue ) the runtime error raised if function is not exists at runtime
  );

  PMeScriptCodeMemory = ^TMeScriptCodeMemory;
  TMeScriptCodeMemory = object(TMeDynamicMemory)
  public
    procedure AddOpCode(const aOpCode: TMeVMInstruction);overload;
    procedure AddOpCode(const aOpCode: TMeVMInstruction; const aValue: Integer);overload;
    procedure AddOpPushDouble(const aValue: Double);
    procedure AddOpBind(const aOpCode: TMeVMInstruction; const aName: string);
    procedure AddOpPushString(const aStr: string);
  end;

implementation

{TMeScriptCodeMemory}

procedure TMeScriptCodeMemory.AddOpPushString(const aStr: string);
begin
  AddOpCode(opPushString);
  AddInt(Length(aStr));
  if Length(aStr) > 0 then
  begin
  AddBuffer(aStr[1], Length(aStr));
  end;
end;

procedure TMeScriptCodeMemory.AddOpBind(const aOpCode: TMeVMInstruction; const aName: string);
var
  vLen: Byte;
begin
  AddOpCode(aOpCode);
  vLen := Length(aName);
  AddByte(vLen);
  AddBuffer(aName[1], vLen);
end;

procedure TMeScriptCodeMemory.AddOpCode(const aOpCode: TMeVMInstruction);
var
  p: pointer;
begin
  if (FUsedSize + SizeOf(aOpCode)) >= FSize then
    Grow(SizeOf(aOpCode));
  Assert(Assigned(FMemory), 'Err:FMemory is nil!!'+IntToStr(FSize));

  //writeln(FUsedSize, ':', FSize);
  p := Pointer(Integer(FMemory) + FUsedSize);
  PMeVMInstruction(P)^ := aOpCode;
  Inc(FUsedSize, SizeOf(TMeVMInstruction)); //}
end;

procedure TMeScriptCodeMemory.AddOpCode(const aOpCode: TMeVMInstruction; const aValue: Integer);
begin
  AddOpCode(aOpCode);
  AddInt(aValue);
end;

procedure TMeScriptCodeMemory.AddOpPushDouble(const aValue: Double);
begin
  AddOpCode(opPush);
  AddDouble(aValue);
end;

initialization
end.
