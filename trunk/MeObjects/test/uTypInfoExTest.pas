unit uTypInfoExTest;

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
  , TypInfoEx
  ;

type
  TTestEnumNumber = (One, Two, Three, Four);
  TTestSetNumbers = set of TTestEnumNumber;

  TTest_TypeEx = class(TTestCase)
  protected
    FTypeInfo: PTypeInfo;
    procedure TestTypeData;virtual;
  public
  published
    procedure Test_TypeEx;
  end;

  TTest_CustomOrdinalType = class(TTest_TypeEx)
  protected
    procedure TestTypeData;override;
  public
  published
  end;

  TTest_SetType = class(TTest_CustomOrdinalType)
  protected
    procedure Setup;override;
    procedure TestTypeData;override;
  public
  published
  end;

  TTest_OrdinalType = class(TTest_CustomOrdinalType)
  protected
    MinValue, MaxValue: Longint;
    procedure TestTypeData;override;
  public
  published
  end;

  TTest_EnumerationType = class(TTest_OrdinalType)
  protected
    procedure Setup;override;
    procedure TestTypeData;override;
  public
  published
  end;

implementation


var
  AppPath: string;


{ TTest_TypeEx }
procedure TTest_TypeEx.TestTypeData;
begin
  Check(Assigned(FTypeInfo), 'not assigned TypeInfo');
  Check(Assigned(PTypeEx(FTypeInfo).TypeData()), ' not Assigned TypeData.');
  Check(PTypeEx(FTypeInfo).Kind >= Low(TTypeKind), ' FTypeInfo).Kind < Low(TTypeKind).');
  Check(PTypeEx(FTypeInfo).Kind <= High(TTypeKind), ' FTypeInfo).Kind > High(TTypeKind).');
end;

procedure TTest_TypeEx.Test_TypeEx;
begin
  TestTypeData;
end;

procedure TTest_CustomOrdinalType.TestTypeData;
begin
  inherited;
  Check(PCustomOrdinalType(FTypeInfo).OrdType >= Low(TOrdType), ' FTypeInfo).OrdType < Low(TOrdType).');
  Check(PCustomOrdinalType(FTypeInfo).OrdType <= High(TOrdType), ' FTypeInfo).OrdType > High(TOrdType).');
end;

procedure TTest_SetType.Setup;
begin
  FTypeInfo := TypeInfo(TTestSetNumbers);
end;

procedure TTest_SetType.TestTypeData;
var
  vCompType: PEnumerationType;
begin
  inherited;
  CheckEquals(Ord(tkSet), Ord(PTypeEx(FTypeInfo).Kind) , ' Kind should be tkSet.');
  CheckEquals('TTestSetNumbers', PTypeEx(FTypeInfo).Name , ' Name Error.');

  vCompType := PEnumerationType(PSetType(FTypeInfo).CompType);
  CheckEquals(Integer(TypeInfo(TTestEnumNumber)), Integer(vCompType) , ' CompType Error.');

  CheckEquals(Ord(tkEnumeration), Ord(vCompType.Kind) , ' Kind should be tkEnumeration.');
  CheckEquals('TTestEnumNumber', vCompType.Name , ' CompType.Name Error.');
end;

procedure TTest_OrdinalType.TestTypeData;
begin
  inherited;
  CheckEquals(MinValue, POrdinalType(FTypeInfo).MinValue , ' MinValue Error.');
  CheckEquals(MaxValue, POrdinalType(FTypeInfo).MaxValue , ' MaxValue Error.');
end;

procedure TTest_EnumerationType.Setup;
begin
  FTypeInfo := TypeInfo(TTestEnumNumber);
  MinValue  := Ord(Low(TTestEnumNumber));
  MaxValue  := Ord(High(TTestEnumNumber));
end;

procedure TTest_EnumerationType.TestTypeData;
var
  i: Integer;
begin
  inherited;
  CheckEquals(Ord(tkEnumeration), Ord(PTypeEx(FTypeInfo).Kind) , ' Kind should be tkEnumeration.');
  CheckEquals('TTestEnumNumber', PTypeEx(FTypeInfo).Name , ' Name Error.');
  
  CheckEquals('uTypInfoExTest', PEnumerationType(FTypeInfo).EnumUnitName , ' EnumUnitName Error.');
  for i := 0 to PEnumerationType(FTypeInfo).Count -1 do
    CheckEquals(GetEnumName(FTypeInfo, i), PEnumerationType(FTypeInfo).EnumName[i] , ' EnumName Error.');

  for i := 0 to PEnumerationType(FTypeInfo).Count -1 do
    CheckEquals(i, PEnumerationType(FTypeInfo).EnumValue[GetEnumName(FTypeInfo, i)], ' EnumName Error.');
end;

Initialization

  AppPath := ExtractFilePath(ParamStr(0));
  RegisterTests('MeTypes suites',
                [
                 TTest_SetType.Suite
                 , TTest_EnumerationType.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
