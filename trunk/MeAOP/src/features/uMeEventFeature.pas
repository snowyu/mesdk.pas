
{ Summary: the MultiCast-Event - implements the AOP -- feature-oriented programming class.}
{
  License
   * The contents of this file are released under the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
     * The Original Code is $RCSfile: uMeEventFeature.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEEis Copyright (C) 2008
    * All rights reserved.
    * Contributor(s):
}
unit uMeEventFeature;

interface

{$I MeSetting.inc}

uses
  {$IFDEF LINUX}
  Types,
  Libc,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes
  , uMeObject
  , uMeSysUtils
  {$IFDEF MeRTTI_SUPPORT}
  , uMeTypes
  , uMeProcType
  {$ENDIF}
  , uMeInterceptor
  , uMeFeature
  ;

type
  TEventId = Cardinal;
  PMeEventInfo = ^ TMeEventInfo;
  PMePublisherInfo = ^ TMePublisherInfo;
  TMeCustomEventFeature = class;

  TMeEventInfo = object(TMeDynamicObject)
  protected
    procedure Init; virtual; //override
    procedure ListenerFreeNotify(Instance : Pointer);

  public
    destructor Destroy; virtual; //override;
    procedure Dispatch(const Sender: TObject; var aEvent);
    procedure AddListener(const aListener: TObject);
  public
    EventId: TEventId;
    Subscribers: PMeThreadSafeList;
  end;

  {: the abstract Publisher Info object.}
  TMePublisherInfo = object(TMeDynamicObject)
  protected
    FOwner: TMeCustomEventFeature;
    procedure Init; virtual; //override
  public
    destructor Destroy; virtual; //override;
    {: Is the event exists }
    function IsExists(aEventId: TEventId): Boolean; virtual;
    function IndexOfEvent(aEventId: TEventId): Integer;
    function FindEvent(const aEvent): PMeEventInfo; virtual;
    function RegisterEvent(const aEventId: TEventId): Integer;
  public
    Publisher: TObject;
    Events: PMeList; //List of PMeEventInfo
    //Subscribers: PMeThreadSafeList;
  end;
  
  { Summary: the abstract multicast event feature class. }
  TMeCustomEventFeature = class(TMeCustomFeature)
  protected
    FPublisherInfoList: PMeThreadSafeList;
  protected
    procedure PublisherFreeNotify(Instance : Pointer);
    function IndexOfPublisher(const Instance : TObject): Integer;
    function FindPublisher(const Instance : TObject): PMePublisherInfo;
    procedure ClearPublishers;
    procedure DeletePublisher(const Instance : TObject);
    {: must override }
    function GetPublisherInfoClass: TMeClass; virtual; abstract;
    function RetrieveEventId(const aEvent): TEventId; virtual; abstract;

    procedure BeforeExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
            override;
    //function IsValidEvent(const aPublisher: TObject; const aEvent: Pointer): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    function RegisterEvent(const aPublisher: TObject; const aEventId: TEventId): PMeEventInfo;
    procedure UnRegisterEvent(const aPublisher: TObject; const aEventId: TEventId);
  end;

  { Summary: 基于Window 消息的多投事件的功能。 }
  TMeWindowMessageFeature = class(TMeCustomEventFeature)
  protected
    function GetPublisherInfoClass: TMeClass; override;
    function RetrieveEventId(const aEvent): TEventId; override;
  end;

function GMeWindowMessageFeature: TMeWindowMessageFeature;

implementation

var
  FMeWindowMessageFeature: TMeWindowMessageFeature  = nil;

type
  TDispatchProc = procedure (const Message: Integer) of object;
  PDispatchMessage = ^ TDispatchMessage;

function GMeWindowMessageFeature: TMeWindowMessageFeature;
begin
  if not Assigned(FMeWindowMessageFeature) then
    FMeWindowMessageFeature := TMeWindowMessageFeature(TMeWindowMessageFeature.AddTo(TObject, @TObject.Dispatch, 'Dispatch', TypeInfo(TDispatchProc)));

  Result := FMeWindowMessageFeature;
end;

{ TMeEventInfo }
procedure TMeEventInfo.Init;
begin
  inherited;
  New(Subscribers, Create);
end;

destructor TMeEventInfo.Destroy; 
var
  i: Integer;
begin
  with Subscribers.LockList^ do
  try
    for i := 0 to Count - 1 do
    begin
      RemoveFreeNotification(Items[i], ListenerFreeNotify);
    end;
  finally
    Subscribers.UnLockList;
  end;

  MeFreeAndNil(Subscribers);
  inherited;
end;

procedure TMeEventInfo.AddListener(const aListener: TObject);
begin
  if Assigned(aListener) then
    with Subscribers.LockList^ do
    try
      if IndexOf(aListener) < 0 then
      begin
        AddFreeNotification(aListener, ListenerFreeNotify);
        Add(aListener);
      end;
    finally
      Subscribers.UnLockList;
    end;
end;

procedure TMeEventInfo.Dispatch(const Sender: TObject; var aEvent);
var
  i: Integer;
begin
  with Subscribers.LockList^ do
  try
    //writeln('Subscribers.conunt:',Count);
    //writeln('EventId:',PDispatchMessage(aEvent).MsgId);
    for i := 0 to Count - 1 do
    begin
      TObject(Items[i]).Dispatch(PDispatchMessage(aEvent)^);
    end;
  finally
    Subscribers.UnLockList;
  end;
end;

procedure TMeEventInfo.ListenerFreeNotify(Instance : Pointer);
begin
  Subscribers.Remove(Instance);
end;

{ TMePublisherInfo }
procedure TMePublisherInfo.Init;
begin
  inherited;
  New(Events, Create);
end;

destructor TMePublisherInfo.Destroy; 
begin
  Events.FreeMeObjects;
  MeFreeAndNil(Events);
  inherited;
end;

function TMePublisherInfo.FindEvent(const aEvent): PMeEventInfo;
var
  i: Integer;
begin
  with Events^ do
    for i := 0 to Count - 1 do
    begin
      Result := PMeEventInfo(Items[i]);
      if Assigned(Result) and (Result.EventId = FOwner.RetrieveEventId(aEvent)) then
        exit;
    end;
  Result := nil;
end;

function TMePublisherInfo.IndexOfEvent(aEventId: TEventId): Integer;
var
  vItem: PMeEventInfo;
begin
  with Events^ do
    for Result := 0 to Count - 1 do
    begin
      vItem := PMeEventInfo(Items[Result]);
      if Assigned(vItem) and (vItem.EventId = aEventId) then
        exit;
    end;
  Result := -1;
end;

function TMePublisherInfo.IsExists(aEventId: TEventId): Boolean; 
begin
  Result := IndexOfEvent(aEventId) >= 0;
end;

function TMePublisherInfo.RegisterEvent(const aEventId: TEventId): Integer;
var
  vItem: PMeEventInfo;
begin
  Result := IndexOfEvent(aEventId);
  if Result < 0 then
  begin
    New(vItem, Create);
    vItem.EventId := aEventId;
    Result := Events.Add(vItem);
  end
  else 
    Result := -1;
end;

{
function TMePublisherInfo.RetrieveEventId(var aEvent): TEventId;
begin
end;
}

{ TMeCustomEventFeature }
constructor TMeCustomEventFeature.Create;
begin
  inherited;
  New(FPublisherInfoList, Create);
end;

destructor TMeCustomEventFeature.Destroy;
begin
  ClearPublishers;
  //FPublisherInfoList.FreeMeObjects;
  MeFreeAndNil(FPublisherInfoList);
  inherited;
end;

Type
  PMessage = ^TMessage;
  TMessage = packed record

    Msg: Cardinal;
    case Integer of
      0: (
        WParam: Longint;
        LParam: Longint;
        Result: Longint);
      1: (
        WParamLo: Word;
        WParamHi: Word;
        LParamLo: Word;
        LParamHi: Word;
        ResultLo: Word;
        ResultHi: Word);

  end;
procedure TMeCustomEventFeature.BeforeExecute(Sender: TObject; MethodItem:
  TMeInterceptedMethodItem; const Params: PMeProcParams);
var
  vItem: PMePublisherInfo;
  v: PDispatchMessage;
  vEventInfo: PMeEventInfo;
begin
  //if Assigned(Sender) and (Sender.ClassName= 'TTestPublisher') then write('Dispatch.BeforeExecute:'+Sender.ClassName);
  vItem := FindPublisher(Sender);
  if Assigned(vItem) then
  begin
    v := PDispatchMessage(Params.Items[0].AsPointer);
    //writeln(' found(', TWMKey(v^).Charcode, ')');
    vEventInfo := vItem.FindEvent(v);
    if Assigned(vEventInfo) then
      vEventInfo.Dispatch(Sender, v);
  end;
end;

procedure TMeCustomEventFeature.ClearPublishers;
var
  i: Integer;
  vItem: PMePublisherInfo;
begin
  with FPublisherInfoList.LockList^ do
  try
    for i := Count -1 downto 0 do
    begin
      vItem := PMePublisherInfo(Items[i]);
      if Assigned(vItem) then
      begin
        RemoveFreeNotification(vItem, PublisherFreeNotify);
        vItem.Free;
        Delete(i);
      end;
    end; //for
  finally
    FPublisherInfoList.UnLockList;
  end;
end;

procedure TMeCustomEventFeature.DeletePublisher(const Instance : TObject);
var
  i: Integer;
  vItem: PMePublisherInfo;
begin
  with FPublisherInfoList.LockList^ do
  try
    for i := Count -1 downto 0 do
    begin
      vItem := PMePublisherInfo(Items[i]);
      if Assigned(vItem) and (vItem.Publisher  = Instance) then
      begin
        RemoveFreeNotification(vItem, PublisherFreeNotify);
        vItem.Free;
        Delete(i);
        Exit;
      end;
    end; //for
  finally
    FPublisherInfoList.UnLockList;
  end;
end;

function TMeCustomEventFeature.FindPublisher(const Instance : TObject): PMePublisherInfo;
var
  i: Integer;
begin
  with FPublisherInfoList.LockList^ do
  try
    for i := 0 to Count -1 do
    begin
      Result := PMePublisherInfo(Items[i]);
      if Assigned(Result) and (Result.Publisher  = Instance) then
        exit;
    end;
  finally
    FPublisherInfoList.UnLockList;
  end;
  Result := nil;
end;

function TMeCustomEventFeature.IndexOfPublisher(const Instance : TObject): Integer;
var
  vItem: PMePublisherInfo;
begin
  with FPublisherInfoList.LockList^ do
  try
    for Result := 0 to Count -1 do
    begin
      vItem := PMePublisherInfo(Items[Result]);
      if Assigned(vItem) and (vItem.Publisher  = Instance) then
        exit;
    end;
  finally
    FPublisherInfoList.UnLockList;
  end;
  Result := -1;
end;

procedure TMeCustomEventFeature.PublisherFreeNotify(Instance : Pointer);
var
  i: Integer;
begin
  i := IndexOfPublisher(Instance);
  if i >= 0 then
    FPublisherInfoList.Delete(i);
  //DeletePublisher(Instance);
end;

function TMeCustomEventFeature.RegisterEvent(const aPublisher: TObject; const aEventId: TEventId): PMeEventInfo;
var
  i: Integer;
  vPublisherInfo: PMePublisherInfo;
begin
  Result := nil;
  i := IndexOfPublisher(aPublisher);
  if i < 0 then
  begin
    vPublisherInfo := PMePublisherInfo(NewMeObject(GetPublisherInfoClass));
    vPublisherInfo.FOwner := Self;
    vPublisherInfo.Publisher := aPublisher;
    i := FPublisherInfoList.Add(vPublisherInfo);
    if i < 0 then
    begin
      MeFreeAndNil(vPublisherInfo);
    end;
  end
  else
    vPublisherInfo := PMePublisherInfo(FPublisherInfoList.Get(i));
  if Assigned(vPublisherInfo) then
  begin
    i := vPublisherInfo.RegisterEvent(aEventId);
    if i >= 0 then
      Result := PMeEventInfo(vPublisherInfo.Events.Items[i]);
  end;
end;

procedure TMeCustomEventFeature.UnRegisterEvent(const aPublisher: TObject; const aEventId: TEventId);
var
  i: Integer;
  vPublisherInfo: PMePublisherInfo;
begin
  i := IndexOfPublisher(aPublisher);
  if i >= 0 then
  begin
    vPublisherInfo := FPublisherInfoList.Get(i);
    i := vPublisherInfo.IndexOfEvent(aEventId);
    if i >= 0 then
    begin
      PMeEventInfo(vPublisherInfo.Events.Items[i]).Free;
      vPublisherInfo.Events.Delete(i);
    end;
  end;
end;

{ TMeWindowMessageFeature }
function TMeWindowMessageFeature.GetPublisherInfoClass: TMeClass;
begin
  Result := TypeOf(TMePublisherInfo);
end;

{ TObject.Dispatch accepts any data type as its Message parameter.  The
  first 2 bytes of the data are taken as the message id to search for
  in the object's message methods.  TDispatchMessage is an example of
  such a structure with a word field for the message id.
  TDispatchMessage = record
    MsgID: Word;
  end;
}
function TMeWindowMessageFeature.RetrieveEventId(const aEvent): TEventId; 
begin
  Result :=  PDispatchMessage(aEvent)^.MsgId;
  //writeln('MsgId:', Result);
end;

initialization
  //FMeWindowMessageFeature := nil;
finalization
end.
