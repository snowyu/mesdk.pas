
{ Summary: MeSysUtils - Helper functions and classes. }
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

  TMeThreadSafeList = object(TMeDynamicObject)
  protected
    FList: PMeList;
    FLock: PMeCriticalSection;
    FDuplicates: TDuplicates;

    procedure Init; virtual; //override
  public
    destructor Destroy; virtual;
    procedure Add(const Item: Pointer);
    procedure Clear;
    function  LockList: PMeList;
    function Count: Integer;
    function Last: Pointer;
    function Popup: Pointer;
    function Get(const Index: Integer): Pointer;
    function Remove(const Item: Pointer): Integer;
    procedure UnlockList;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property List: PMeList read FList;
  end;

  TFreeNotifyProc = procedure(Instance : TObject) of object;

{ Summary: Ensures that aProc is notified that the aInstance is going to be destroyed.}
{
  Desccription
Use AddFreeNotification to register aProc that should be notified when the aInstance is about to be destroyed. 
}
procedure AddFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);
{ Summary: Disables destruction notification that was enabled by AddFreeNotification.}
{
Description
RemoveFreeNotification removes the NotificationProc specified by the aProc parameter from the internal list of procedures to be notified that the aInstance is about to be destroyed. aProc is added to this list by a previous call to the AddFreeNotification function.
}
procedure RemoveFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);

implementation

uses
  RTLConsts;

type
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
    Instance: TObject;
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
    FFreeInstance: PProcedure;

    procedure Inject(const aInstance : TObject);

    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItem(const Index: Integer): PFreeNotificationInfo;
  public
    {$IFDEF THREADSAFE_SUPPORT}
    constructor Create;
    {$ENDIF}
    destructor Destroy; override;
    function Add(const aInstance : TObject): PFreeNotificationInfo;
    function FindByInstance(const aInstance : TObject): PFreeNotificationInfo;
    function IndexOfInstance(const aInstance : TObject): Integer;
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

procedure TFreeNotificationObjects.Inject(const aInstance : TObject);
var
  vMethodIndex: Integer;
begin
  if not FFreeInstanceInjector.Enabled and Assigned(aInstance) then
  begin
    vMethodIndex := Integer(FFreeInstanceInjector.InjectStaticMethod(TObject, @TObject.FreeInstance, @DoObjectFreeInstance));
    Assert(vMethodIndex <> 0, 'Can not Inject the FreeInstance virtual method.');
    {
    vMethodIndex := FindVirtualMethodIndex(TObject, @TObject.FreeInstance);
    Assert(vMethodIndex >= 0, 'not found the TObject.FreeInstance virtual method');
    vMethodIndex := Integer(FFreeInstanceInjector.InjectVirtualMethod(aInstance.ClassType, vMethodIndex, @TFreeNotificationObjects.DoObjectFreeInstance));
    Assert(vMethodIndex <> 0, 'Can not Inject the FreeInstance virtual method.')
    }

    @FFreeInstance := FFreeInstanceInjector.OriginalProc;
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

function TFreeNotificationObjects.Add(const aInstance : TObject): PFreeNotificationInfo;
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

function TFreeNotificationObjects.IndexOfInstance(const aInstance : TObject): Integer;
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

function TFreeNotificationObjects.FindByInstance(const aInstance : TObject): PFreeNotificationInfo;
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

procedure AddFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);
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

procedure RemoveFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);
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

procedure TMeThreadSafeList.Add(const Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
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

initialization
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeThreadSafeList), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeThreadSafeList), ovtVmtParent, TypeOf(TMeDynamicObject));

finalization
  FreeAndNil(FFreeNotificationObjects);
end.
