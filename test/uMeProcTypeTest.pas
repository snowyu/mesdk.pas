unit uMeProcTypeTest;

{$I MeSetting.inc}

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
  Math,
  TypInfo,
  TestFramework
  //uStringListEx,
  , uMeObject
  , uMeTypInfo //the TCallingConvention
  , uMeTypes
  , uMeProcType
  , uMeTypesTest
  ;

type
  TTest_MeProcType = class(TTest_MeType)
  protected
    function MeTypeClass: TMeClass;override;
    procedure TestAssignFromTypeData(aTypeData: PTypeData);override;
    function CheckProcParamInfo(const aProcParamInfo: TMeParamType; const aParamData: TMeParamData): PMeParamData;
  published
    procedure Test_AssignTypeInfo;
    procedure Test_ParamStream;
  end;

implementation

type
  TTestProc1 = function(var aIntVar: integer; const aByte: Byte; out aOut: String): Boolean of object;
  TTestProc2 = procedure(const aInt: integer; const aStr: string; a: Boolean; c: shortstring) of object;
  TTestProc3 = procedure(const aInt: LongInt; const aStr: WideString; out a: Boolean; var c: shortstring);
 
function TTest_MeProcType.MeTypeClass: TMeClass;
begin
  Result := TypeOf(TMeProcType);
end;

function TTest_MeProcType.CheckProcParamInfo(const aProcParamInfo: TMeParamType; const aParamData: TMeParamData): PMeParamData;
var
  vTypeStr: PShortString;
  vParamType: PMeType;
begin
  with aProcParamInfo do
  begin
    CheckEquals(Byte(aParamData.Flags), Byte(Flags), 'the Param Flags is mismatch.');
    vTypeStr := Pointer(Integer(@aParamData.ParamName) +
      Length(aParamData.ParamName) + 1);
    {$IFDEF MeRTTI_EXT_SUPPORT}
    CheckEquals(aParamData.ParamName, Name, 'the Param Name is mismatch.');
    {$ENDIF}
    vParamType := nil;
    if Assigned(FType.Owner) then
    begin
      vParamType := FType.Owner.GetRegisteredTypeByName(vTypeStr^);
    end;
    if vParamType = nil then
    begin
      vParamType := uMeTypes.GetRegisteredTypeByName(vTypeStr^);
    end;
    CheckEquals(Integer(vParamType), Integer(ParamType), aProcParamInfo.ProcType.Name + ' the Param Type is mismatch.');
  end;
  Result := PMeParamData(Integer(@aParamData) + SizeOf(TParamFlags) +
    Length(aParamData.ParamName) + Length(vTypeStr^) + 2);
end;

procedure TTest_MeProcType.TestAssignFromTypeData(aTypeData: PTypeData);
var
  I: Integer;
  vParamData: PMeParamData;
  vStr: PShortString;
  vParamType: PMeParamType;
  vResultType : PMeType;
begin
  inherited;
  CheckEquals(Ord(aTypeData.MethodKind), Ord(PMeProcType(FType).MethodKind), 'the MethodKind is mismatch.');
  CheckEquals(aTypeData^.ParamCount, PMeProcType(FType).ParamList.Count, 'the ParamList.Count is mismatch.');
  vParamData := PMeParamData(@aTypeData^.ParamList);
  for I := 0 to aTypeData^.ParamCount - 1 do
  with PMeProcType(FType)^ do
  begin
    vParamType := ParamList.Items[I];
    vParamData := CheckProcParamInfo(vParamType^, vParamData^);
  end;

  vResultType := nil;
  with PMeProcType(FType)^ do
  begin
    if (MethodKind = mkFunction) or (MethodKind = mkClassFunction) then
    begin
      vStr := PShortString(vParamData);
      if Assigned(Owner) then
        vResultType := Owner.GetRegisteredTypeByName(vStr^);
      if not Assigned(vResultType) then
        vResultType:= uMeTypes.GetRegisteredTypeByName(vStr^);
      vResultType := GetRegisteredTypeByName(vStr^);
      CheckEquals(True, ResultType = vResultType, 'the ResultType is mismatch.');
      if RetOnStack then
      begin //the caller passes an additional 32-bit pointer that points to a variable in which to return the function result.
        vParamType := ParamList.Items[ParamList.Count-1];
        CheckEquals(True, vParamType.ProcType = PMeProcType(FType), 'the Result ProcType is mismatch.');
        CheckEquals(True, vParamType.Flags = [pfVar], 'the Result Flags is mismatch.');
        CheckEquals(True, vParamType.ParamType = vResultType, 'the Result ParamType is mismatch.');
      end;
      vParamData := PMeParamData(Integer(vParamData) 
        + Length(vStr^) + 1 + 4);
    end
    else begin
      CheckEquals(True, ResultType = nil, 'the ResultType is not nil.');
    end;
    CheckEquals(Ord(TCallingConvention(PByte(vParamData)^)), Ord(CallingConvention), 'the CallingConvention is mismatch.');
  end;
end;

procedure TTest_MeProcType.Test_AssignTypeInfo;
begin
  TestAssignFromTypeInfo(TypeInfo(TTestProc1));
  TestAssignFromTypeInfo(TypeInfo(TTestProc2));
  TestAssignFromTypeInfo(TypeInfo(TTestProc3));
end;

Type
  TTestFuncParams = procedure(const i: integer; s: string; b: Boolean):

procedure TTest_MeProcType.Test_ParamStream;
var
  vParams: PMeProcParams;
begin
  New(vParams, Create);
  vParams.InitFromType(RegisterProcTypeInfo(TypeInfo(TTestFuncParams)));
  vParams.
end;

Initialization

  RegisterTests('MeTypes suites',
                [
                 TTest_MeProcType.Suite
                 //, TTest_MeProcType.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
