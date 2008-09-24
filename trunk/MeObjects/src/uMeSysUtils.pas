
{ Summary: MeSysUtils - Some helper functions and classes. }
{
  License
   * The contents of this file are released under the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
     * The Original Code is $RCSfile: uMeSysUtils.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.
    * Contributor(s):
}
unit uMeSysUtils;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  //, uMeConsts
  , uMeTypInfo
  , uMeObject
  , uMeSyncObjs
  , uMeInjector
  ;

type
  PMeThreadSafeList = ^ TMeThreadSafeList;

  { Summary: represents a thread-safe list.}
  TMeThreadSafeList = object(TMeDynamicObject)
  protected
    FList: PMeList;
    FLock: PMeCriticalSection;
    FDuplicates: TDuplicates;
    //FMaxCount: Integer;

    procedure Init; virtual; //override
  public
    destructor Destroy; virtual;
    function Add(const Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(const Index: Integer);
    function  LockList: PMeList;
    function Count: Integer;
    function Last: Pointer;
    function Popup: Pointer;
    function Get(const Index: Integer): Pointer;
    function Remove(const Item: Pointer): Integer;
    procedure UnlockList;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property List: PMeList read FList;
    //##if MaxCount > 0 then limits the list count by Add method 
    //##property MaxCount: Integer read FMaxCount write FMaxCount;
  end;

  TFreeNotifyProc = procedure(Instance : Pointer) of object;

{ Summary: Ensures that aProc is notified that the aInstance is going to be destroyed.}
{
  Desccription
Use AddFreeNotification to register aProc that should be notified when the aInstance is about to be destroyed. 
}
procedure AddFreeNotification(const aInstance : Pointer; const aProc : TFreeNotifyProc);
{ Summary: Disables destruction notification that was enabled by AddFreeNotification.}
{
Description
RemoveFreeNotification removes the NotificationProc specified by the aProc parameter from the internal list of procedures to be notified that the aInstance is about to be destroyed. aProc is added to this list by a previous call to the AddFreeNotification function.
}
procedure RemoveFreeNotification(const aInstance : Pointer; const aProc : TFreeNotifyProc);

//the thread safe version:
function FormatDateTimeS(const Format: string; aDateTime: TDateTime): string;
function FormatS(const aFormat: string; const Args: array of const): string;
function TimeToStrS(const aTime: TDateTime): string;
function DateToStrS(const aDate: TDateTime): string;
function DateTimeToStrS(const aDateTime: TDateTime): string;

procedure GetDefaultFormatSettings(var Result: TFormatSettings);

//return GMT now.
function GMTNow: TDateTime;

implementation

uses
  RTLConsts;

{$IFDEF LINUX}
var
  // For linux the user needs to set these variables to be accurate where used (mail, etc)
  GOffsetFromUTC: TDateTime = 0;

function OffsetFromUTC: TDateTime;
begin
  //TODO: Fix OffsetFromUTC for Linux to be automatic from OS
  Result := GOffsetFromUTC;
end;
{$ENDIF}
{$IFDEF DOTNET}
function OffsetFromUTC: TDateTime;
begin
  Result := System.Timezone.CurrentTimezone.GetUTCOffset(now).TotalDays;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
function OffsetFromUTC: TDateTime;
var
  iBias: Integer;
  tmez: TTimeZoneInformation;
begin
  Case GetTimeZoneInformation(tmez) of
    //TIME_ZONE_ID_INVALID:
      //raise EIdFailedToRetreiveTimeZoneInfo.Create(RSFailedTimeZoneInfo);
    TIME_ZONE_ID_UNKNOWN  :
       iBias := tmez.Bias;
    TIME_ZONE_ID_DAYLIGHT :
      iBias := tmez.Bias + tmez.DaylightBias;
    TIME_ZONE_ID_STANDARD :
      iBias := tmez.Bias + tmez.StandardBias;
    else begin
      Result := 0;
      Exit;
    end
      //raise EIdFailedToRetreiveTimeZoneInfo.Create(RSFailedTimeZoneInfo);
  end;
  {We use ABS because EncodeTime will only accept positve values}
  Result := EncodeTime(Abs(iBias) div 60, Abs(iBias) mod 60, 0, 0);
  {The GetTimeZone function returns values oriented towards convertin
   a GMT time into a local time.  We wish to do the do the opposit by returning
   the difference between the local time and GMT.  So I just make a positive
   value negative and leave a negative value as positive}
  if iBias > 0 then begin
    Result := 0 - Result;
  end;
end;
{$ENDIF}

function GMTNow: TDateTime;
begin
  Result := Now - OffsetFromUTC;
end;

function FormatS(const aFormat: string; const Args: array of const): string;
var
  vFormatSettings:  TFormatSettings;
begin
  GetDefaultFormatSettings(vFormatSettings);
  Result := SysUtils.Format(aFormat, Args, vFormatSettings);
end;

function FormatDateTimeS(const Format: string; aDateTime: TDateTime): string;
var
  vFormatSettings: TFormatSettings;
begin
  vFormatSettings.DateSeparator := '-';
  vFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  vFormatSettings.ShortDateFormat := LongDateFormat;
  vFormatSettings.TimeSeparator := ':';
  vFormatSettings.LongTimeFormat := 'hh:nn:ss';
  vFormatSettings.ShortTimeFormat := LongTimeFormat;
  Result := FormatDateTime(Format, aDateTime, vFormatSettings);
end;

function DateTimeToStrS(const aDateTime: TDateTime): string;
var
  vFormatSettings: TFormatSettings;
begin
  vFormatSettings.DateSeparator := '-';
  vFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  vFormatSettings.ShortDateFormat := LongDateFormat;
  vFormatSettings.TimeSeparator := ':';
  vFormatSettings.LongTimeFormat := 'hh:nn:ss';
  vFormatSettings.ShortTimeFormat := LongTimeFormat;
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', aDateTime, vFormatSettings);
end;

function DateToStrS(const aDate: TDateTime): string;
var
  vFormatSettings: TFormatSettings;
begin
  vFormatSettings.DateSeparator := '-';
  vFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  vFormatSettings.ShortDateFormat := LongDateFormat;
  Result := SysUtils.DateToStr(aDate, vFormatSettings);
end;

function TimeToStrS(const aTime: TDateTime): string;
var
  vFormatSettings: TFormatSettings;
begin
  vFormatSettings.TimeSeparator := ':';
  vFormatSettings.LongTimeFormat := 'hh:nn:ss';
  vFormatSettings.ShortTimeFormat := LongTimeFormat;
  Result := SysUtils.TimeToStr(aTime, vFormatSettings);
end;

type
  TDestructorProc = procedure(const aInstance: Pointer; const aFlag: Boolean);
  {
  this very simplest to implement the FreeNotify On object.Free.
  it inject the TObject.FreeInstance method and check it here!!
  the good way should be inject the virtual method on the proper class, not on the TObject!
  }
  TFreeNotificationObjects = class;
  PFreeNotificationInfo = ^ TFreeNotificationInfo;
  TFreeNotificationInfo = object
  protected
    FFreeNotifies: PMeList;
  public
    Instance: Pointer;
    //Owner: TFreeNotificationObjects;
    function IndexOfProc(const aProc : TFreeNotifyProc): Integer;
    procedure AddFreeNotification(const aProc : TFreeNotifyProc);
    procedure RemoveFreeNotification(const aProc : TFreeNotifyProc);
    procedure NotifyObjectFree;
  end;

  //the objects need watch to FreeNotification
  TFreeNotificationObjects = class(TList)
  protected
    {$IFDEF THREADSAFE_SUPPORT}
    FLock: PMeCriticalSection;
    {$ENDIF}
    FFreeInstanceInjector: TMeInjector;
    FFreeMeInstanceInjector: TMeInjector;
    FFreeInstance: PProcedure;//TDestructorProc;
    FFreeMeInstance: PProcedure;

    procedure Inject(const aInstance : Pointer);

    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItem(const Index: Integer): PFreeNotificationInfo;
  public
    {$IFDEF THREADSAFE_SUPPORT}
    constructor Create;
    {$ENDIF}
    destructor Destroy; override;
    function Add(const aInstance : Pointer): PFreeNotificationInfo;
    function FindByInstance(const aInstance : Pointer): PFreeNotificationInfo;
    function IndexOfInstance(const aInstance : Pointer): Integer;
    property Items[const Index: Integer]: PFreeNotificationInfo read GetItem;
  end;

//################################################################
var
  //the objects need monitor to FreeNotification
  FFreeNotificationObjects: TFreeNotificationObjects;

function GFreeNotificationObjects: TFreeNotificationObjects;
begin
  if not Assigned(FFreeNotificationObjects) then
    FFreeNotificationObjects := TFreeNotificationObjects.Create;
  Result := FFreeNotificationObjects;
end;


{ TFreeNotificationInfo }
procedure TFreeNotificationInfo.AddFreeNotification(const aProc : TFreeNotifyProc);
{$IFDEF PUREPASCAL}
{$IFDEF FPC}
var Ptr1, Ptr2: Pointer;
{$ENDIF FPC}
begin
  if FFreeNotifies = nil then
    FFreeNotifies := NewList;
  {$IFDEF FPC}
  asm
    MOV  EAX, [aProc]
    MOV  [Ptr1], EAX
    MOV  EAX, [aProc+4]
    MOV  [Ptr2], EAX
  end ['EAX'];
  FFreeNotifies.Insert(0, Ptr2);
  FFreeNotifies.Insert(0, Ptr1);
  {$ELSE DELPHI}
  FFreeNotifies.Insert(0, Pointer(TMethod(aProc).Data));
  FFreeNotifies.Insert(0, Pointer(TMethod(aProc).Code));
  {$ENDIF}
end;
{$ELSE PUREPASCAL} 
asm
        PUSH     EBX
        XCHG     EAX, EBX
        MOV      EAX, [EBX].FFreeNotifies
        TEST     EAX, EAX
        JNZ      @@1
        CALL     NewList
        MOV      [EBX].FFreeNotifies, EAX
@@1:    XOR      EDX, EDX
        MOV      ECX, [EBP+12] // Data
        MOV      EBX, EAX
        CALL     TMeList.Insert
        XCHG     EAX, EBX
        XOR      EDX, EDX
        MOV      ECX, [EBP+8] // Code
        CALL     TMeList.Insert
        POP      EBX
end;
{$ENDIF PUREPASCAL}

procedure TFreeNotificationInfo.NotifyObjectFree;
var
  I: Integer;
    ProcMethod: TMethod;
    Proc: TFreeNotifyProc Absolute ProcMethod;
begin
  if Assigned(FFreeNotifies) then
  begin
    for I := 0 to FFreeNotifies.Count div 2 - 1 do
    begin
      ProcMethod.Code := FFreeNotifies.List[I * 2];
      ProcMethod.Data := FFreeNotifies.List[I * 2 + 1];
      Proc(Instance);
    end;
  end;
end;

procedure TFreeNotificationInfo.RemoveFreeNotification(const aProc : TFreeNotifyProc);
var
  i: Integer;
{$IFDEF FPC}
  Ptr1, Ptr2: Pointer;
{$ENDIF FPC}
begin
  if Assigned(FFreeNotifies) then
    for i := 0 to FFreeNotifies.Count div 2 - 1 do
    begin
      {$IFDEF FPC}
      asm
        MOV  EAX, [aProc]
        MOV  [Ptr1], EAX
        MOV  EAX, [aProc+4]
        MOV  [Ptr2], EAX
      end ['EAX'];
      if (FFreeNotifies.Items[i*2] = Ptr1) and (FFreeNotifies.Items[i*2] = Ptr2) then
      begin
      	FFreeNotifies.DeleteRange(i*2, 2);
      	break;
      end;
      {$ELSE DELPHI}
      if (FFreeNotifies.Items[i*2] = TMethod(aProc).Code) and (FFreeNotifies.Items[i*2+1] = TMethod(aProc).Data) then
      begin
      	FFreeNotifies.DeleteRange(i*2, 2);
      	break;
      end;
      {$ENDIF}
    end;
end;

function TFreeNotificationInfo.IndexOfProc(const aProc : TFreeNotifyProc): Integer;
begin
  for Result := 0 to FFreeNotifies.Count div 2 - 1 do
  begin
    if FFreeNotifies.List[Result * 2] = TMethod(aProc).Code then exit;
  end;
  Result := -1;
end;

{ TFreeNotificationObjects }
{$IFDEF THREADSAFE_SUPPORT}
constructor TFreeNotificationObjects.Create;
begin
  inherited;
  New(FLock, Create);
end;
{$ENDIF}
destructor TFreeNotificationObjects.Destroy;
begin
  FFreeInstanceInjector.Enabled := False;
  FFreeMeInstanceInjector.Enabled := False;
  {$IFDEF THREADSAFE_SUPPORT}
  FLock.Free;
  {$ENDIF}
  inherited;
end;

procedure DoObjectFreeInstance(aSelf: TObject);
var
  vInfo: PFreeNotificationInfo;
begin
  with FFreeNotificationObjects do
  begin
    if FFreeNotificationObjects <> aSelf then
    begin
      vInfo := FindByInstance(aSelf);
      if Assigned(vInfo) then
      begin
        vInfo.NotifyObjectFree();
        Remove(vInfo);
      end;
    end;
    FFreeInstance(aSelf);
  end;
end;

procedure DoMeObjectFreeInstance(aSelf: Pointer);
var
  vInfo: PFreeNotificationInfo;
begin
  with FFreeNotificationObjects do
  begin
    //if FFreeNotificationObjects <> aSelf then
    begin
      vInfo := FindByInstance(aSelf);
      if Assigned(vInfo) then
      begin
        vInfo.NotifyObjectFree();
        Remove(vInfo);
      end;
    end;
    FFreeMeInstance(aSelf);
  end;
end;

procedure TFreeNotificationObjects.Inject(const aInstance : Pointer);
var
  vMethodIndex: Integer;
begin
  if Assigned(aInstance) then
  begin
    if IsObject(aInstance) then
    begin
      if not FFreeInstanceInjector.Enabled then
      begin
        vMethodIndex := Integer(FFreeInstanceInjector.InjectStaticMethod(TObject, @TObject.FreeInstance, @DoObjectFreeInstance));
        Assert(vMethodIndex <> 0, 'Can not Inject the TObject.Destroy method.');
    
        @FFreeInstance := FFreeInstanceInjector.OriginalProc;
      end;
    end
    else if not FFreeMeInstanceInjector.Enabled then
    begin
        vMethodIndex := Integer(FFreeMeInstanceInjector.InjectProcedure(@TMeDynamicObject.DestroyMem, @DoMeObjectFreeInstance));
        Assert(vMethodIndex <> 0, 'Can not Inject the TMeObject.Destroy method.');
    
        @FFreeMeInstance := FFreeMeInstanceInjector.OriginalProc;
    end;
  end;
end;

procedure TFreeNotificationObjects.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and Assigned(Ptr) then
  begin
    PFreeNotificationInfo(Ptr).FFreeNotifies.Free;
    FreeMem(Ptr);
  end;
end;

function TFreeNotificationObjects.Add(const aInstance : Pointer): PFreeNotificationInfo;
begin
  Result := FindByinstance(aInstance);
  if not Assigned(Result) then
  begin
    {$IFDEF THREADSAFE_SUPPORT}
    FLock.Enter;
    try
    {$ENDIF}
    New(Result);
    Result.Instance := aInstance;
    New(Result.FFreeNotifies, Create);
    Inject(aInstance);
    //Result.Owner := Self;
    inherited Add(Result);
    {$IFDEF THREADSAFE_SUPPORT}
    finally
      FLock.Leave;
    end;
    {$ENDIF}
  end;
end;

function TFreeNotificationObjects.IndexOfInstance(const aInstance : Pointer): Integer;
begin
  {$IFDEF THREADSAFE_SUPPORT}
  FLock.Enter;
  try
  {$ENDIF}
  for Result := 0 to Count - 1 do
  begin
    if Items[Result].Instance = aInstance then
      exit;
  end;
  {$IFDEF THREADSAFE_SUPPORT}
  finally
    FLock.Leave;
  end;
  {$ENDIF}
  Result := -1;
end;

function TFreeNotificationObjects.FindByInstance(const aInstance : Pointer): PFreeNotificationInfo;
var
  i: Integer;
begin
  i := IndexOfInstance(aInstance);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

function TFreeNotificationObjects.GetItem(const Index: Integer): PFreeNotificationInfo;
begin
  Result := inherited Get(Index);
end;

procedure AddFreeNotification(const aInstance : Pointer; const aProc : TFreeNotifyProc);
var
  vInfo: PFreeNotificationInfo;
begin
  with GFreeNotificationObjects do
  begin
    vInfo := FindByInstance(aInstance);
    if not Assigned(vInfo) then
    begin
      vInfo := Add(aInstance);
    end
    else if vInfo.IndexOfProc(aProc) >= 0 then 
    begin
      //already exits.
      exit;
    end;
    vInfo.AddFreeNotification(aProc);
  end;
end;

procedure RemoveFreeNotification(const aInstance : Pointer; const aProc : TFreeNotifyProc);
var
  vInfo: PFreeNotificationInfo;
begin
  if Assigned(FFreeNotificationObjects) then
    with FFreeNotificationObjects do
    begin
      vInfo := FindByInstance(aInstance);
      if Assigned(vInfo) then
      begin
        vInfo.RemoveFreeNotification(aProc);
      end;
    end;
end;

{ TMeThreadSafeList }
procedure TMeThreadSafeList.Init;
begin
  inherited;
  New(FList, Create);
  New(FLock, Create);
  FDuplicates := dupIgnore;
end;

destructor TMeThreadSafeList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

function TMeThreadSafeList.Add(const Item: Pointer): Integer;
begin
  Result := -1;
  LockList;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(Item) = -1) then
      Result := FList.Add(Item)
    else if Duplicates = dupError then
      FList.Error(@SDuplicateItem, Integer(Item));
  finally
    UnlockList;
  end;
end;

procedure TMeThreadSafeList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TMeThreadSafeList.Count: Integer;
begin
  LockList;
  try
    Result := FList.Count;
  finally
    UnlockList;
  end;
end;

procedure TMeThreadSafeList.Delete(const Index: Integer);
begin
  LockList;
  try
    FList.Delete(Index);
  finally
    UnlockList;
  end;
end;

function TMeThreadSafeList.Get(const Index: Integer): Pointer;
begin
  with LockList^ do
  try
    Result := FList.List[Index];
  finally
    UnlockList;
  end;
end;

function  TMeThreadSafeList.LockList: PMeList;
begin
  FLock.Enter;
  Result := FList;
end;

function TMeThreadSafeList.Last: Pointer;
begin
  with LockList^ do
  try
    Result := FList.Last;
  finally
    UnlockList;
  end;
end;

function TMeThreadSafeList.Popup: Pointer;
begin
  with LockList^ do
  try
    Result := FList.Popup;
  finally
    UnlockList;
  end;
end;

function TMeThreadSafeList.Remove(const Item: Pointer): Integer;
begin
  LockList;
  try
    Result := FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TMeThreadSafeList.UnlockList;
begin
  FLock.Leave;
end;

procedure GetDefaultFormatSettings(var Result: TFormatSettings);
var
  i: Integer;
begin
  //GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, Result);
  
  with Result do
  begin
    CurrencyFormat:= SysUtils.CurrencyFormat;
    NegCurrFormat := SysUtils.NegCurrFormat;;
    ThousandSeparator := SysUtils.ThousandSeparator;
    DecimalSeparator := SysUtils.DecimalSeparator;
    CurrencyDecimals := SysUtils.CurrencyDecimals;
    DateSeparator := SysUtils.DateSeparator;
    TimeSeparator := SysUtils.TimeSeparator;
    ListSeparator :=  SysUtils.ListSeparator;
    CurrencyString := SysUtils.CurrencyString;
    ShortDateFormat := SysUtils.ShortDateFormat;
    LongDateFormat := SysUtils.LongDateFormat;
    TimeAMString :=  SysUtils.TimeAMString;
    TimePMString := SysUtils.TimePMString;
    ShortTimeFormat:= SysUtils.ShortTimeFormat;
    LongTimeFormat:= SysUtils.LongTimeFormat;

    for i:= 1 to 12 do
    begin
      ShortMonthNames[i] := SysUtils.ShortMonthNames[i];
      LongMonthNames[i] := SysUtils.LongMonthNames[i];
    end;
    for i := 1 to 7 do
    begin
      ShortDayNames[i]:= SysUtils.ShortDayNames[i];
      LongDayNames[i]:= SysUtils.LongDayNames[i];
    end;
    TwoDigitYearCenturyWindow:= SysUtils.TwoDigitYearCenturyWindow;
  end; //}
end;

initialization
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeThreadSafeList), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeThreadSafeList), ovtVmtParent, TypeOf(TMeDynamicObject));

finalization
  FreeAndNil(FFreeNotificationObjects);
end.
