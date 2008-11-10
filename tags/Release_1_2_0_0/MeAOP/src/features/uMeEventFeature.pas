
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
  , uMeSystem
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
  {: this exception prevents the event object from moving on to the next node, but only after any other event listeners on the current node are allowed to execute. }
  EMeEventStopPropagation = class(EMeError);
  {: this exception prevents prevents the event object from moving on to the next node, but does not allow any other event listeners on the current node to execute. }
  EMeEventStopImmediatePropagation = class(EMeError);

  TEventId = Cardinal;
  PMeCustomEventInfo = ^ TMeCustomEventInfo;
  PMeCustomPublisherInfo = ^ TMeCustomPublisherInfo;
  TMeCustomEventFeature = class;

  TMeEventPhase = (epUnknown, epCapturing, epTarget, epBubbling);

  PMeMessage = ^TMeMessage;
  TMeMessage = packed record
    Msg: Cardinal;
    WParam: Longint;
    LParam: Longint;
    Result: Longint;

    //the extend:
    Target: TObject;
    CurrentTarget: TObject;
  end;

  TMeCustomEventInfo = object(TMeDynamicObject)
  protected
    procedure Init; virtual; //override
    procedure ListenerFreeNotify(Instance : Pointer);
  public
    destructor Destroy; virtual; //override;
    {: dispatch the event to Subscribers(listeners)} 
    procedure Dispatch(const Sender: TObject; var aEvent);
    procedure AddListener(const aListener: TObject);
  public
    //Read-only property
    EventId: TEventId;
    //Read-only property
    Subscribers: PMeThreadSafeList;
    //this event is cancelable or not
    Cancelable: Boolean;
  end;

  TMeEventInfo = object(TMeCustomEventInfo)
  public
    EventPhase: TMeEventPhase;
  end;

  {: the Publisher Info object.}
  TMeCustomPublisherInfo = object(TMeDynamicObject)
  protected
    FOwner: TMeCustomEventFeature;
    procedure Init; virtual; //override
    function GetEventInfoClass: TMeClass; virtual; 
  public
    destructor Destroy; virtual; //override;
    {: Is the event exists }
    function IsEventExists(aEventId: TEventId): Boolean;
    function IndexOfEvent(aEventId: TEventId): Integer;
    function FindEvent(const aEvent): PMeCustomEventInfo;
    function FindEventById(const aEventId: TEventId): PMeCustomEventInfo;
    function RegisterEventById(const aEventId: TEventId): Integer;
    function RegisterEvent(const aEvent: PMeCustomEventInfo): Integer;
  public
    Publisher: TObject;
    Events: PMeThreadSafeList; //List of PMeCustomEventInfo
    //Subscribers: PMeThreadSafeList;
  end;
  
  { Summary: the abstract multicast event feature class. }
  TMeCustomEventFeature = class(TMeCustomFeature)
  protected
    FPublisherInfoList: PMeThreadSafeList;
  protected
    procedure PublisherFreeNotify(Instance : Pointer);
    function IndexOfPublisher(const Instance : TObject): Integer;
    function FindPublisher(const Instance : TObject): PMeCustomPublisherInfo;
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

    function GetEvent(const aPublisher: TObject; const aEventId: TEventId): PMeCustomEventInfo;
    function RegisterEvent(const aPublisher: TObject; const aEventId: TEventId): PMeCustomEventInfo;
    procedure UnRegisterEvent(const aPublisher: TObject; const aEventId: TEventId);
  end;

  { Summary: the multicast event feaure for windows message  }
  {
    the return value of the TMessage should be 0 if someone processes this message.
    
  }
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

{ TMeCustomEventInfo }
procedure TMeCustomEventInfo.Init;
begin
  inherited;
  New(Subscribers, Create);
end;

destructor TMeCustomEventInfo.Destroy; 
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

procedure TMeCustomEventInfo.AddListener(const aListener: TObject);
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

procedure TMeCustomEventInfo.Dispatch(const Sender: TObject; var aEvent);
var
  i: Integer;
begin
  with Subscribers.LockList^ do
  try
    //writeln('Subscribers.conunt:',Count);
    //writeln('EventId:',PDispatchMessage(aEvent).MsgId);
    for i := 0 to Count - 1 do
    try
      TObject(Items[i]).Dispatch(PDispatchMessage(aEvent)^);
    except
      On EMeEventStopImmediatePropagation do break;
      else
        Raise;
    end;
  finally
    Subscribers.UnLockList;
  end;
end;

procedure TMeCustomEventInfo.ListenerFreeNotify(Instance : Pointer);
begin
  Subscribers.Remove(Instance);
end;

{ TMeCustomPublisherInfo }
procedure TMeCustomPublisherInfo.Init;
begin
  inherited;
  New(Events, Create);
end;

destructor TMeCustomPublisherInfo.Destroy; 
begin
  with Events.LockList^ do 
  try 
    FreeMeObjects;
  finally
    Events.UnLockList;
  end;
  MeFreeAndNil(Events);
  inherited;
end;

function TMeCustomPublisherInfo.FindEvent(const aEvent): PMeCustomEventInfo;
begin
  Result := FindEventById(FOwner.RetrieveEventId(aEvent));
end;

function TMeCustomPublisherInfo.FindEventById(const aEventId: TEventId): PMeCustomEventInfo;
var
  i: Integer;
begin
  with Events.LockList^ do
  try
    for i := 0 to Count - 1 do
    begin
      Result := PMeCustomEventInfo(Items[i]);
      if Assigned(Result) and (Result.EventId = aEventId) then
        exit;
    end;
  finally
    Events.UnLockList;
  end;
  Result := nil;
end;


function TMeCustomPublisherInfo.GetEventInfoClass: TMeClass;
begin
  Result := TypeOf(TMeCustomEventInfo);
end;

function TMeCustomPublisherInfo.IndexOfEvent(aEventId: TEventId): Integer;
var
  vItem: PMeCustomEventInfo;
begin
  with Events.LockList^ do
  try
    for Result := 0 to Count - 1 do
    begin
      vItem := PMeCustomEventInfo(Items[Result]);
      if Assigned(vItem) and (vItem.EventId = aEventId) then
        exit;
    end;
  finally
    Events.UnLockList;
  end;
  Result := -1;
end;

function TMeCustomPublisherInfo.IsEventExists(aEventId: TEventId): Boolean; 
begin
  Result := IndexOfEvent(aEventId) >= 0;
end;

function TMeCustomPublisherInfo.RegisterEvent(const aEvent: PMeCustomEventInfo): Integer;
begin
  if Assigned(aEvent) and (IndexOfEvent(aEvent.EventId) < 0) then
  begin
    Result := Events.Add(aEvent);
  end
  else 
    Result := -1;
end;

function TMeCustomPublisherInfo.RegisterEventById(const aEventId: TEventId): Integer;
var
  vItem: PMeCustomEventInfo;
begin
  Result := IndexOfEvent(aEventId);
  if Result < 0 then
  begin
    vItem := PMeCustomEventInfo(NewMeObject(GetEventInfoClass));
    //New(vItem, Create);
    vItem.EventId := aEventId;
    Result := Events.Add(vItem);
  end
  else 
    Result := -1;
end;

{
function TMeCustomPublisherInfo.RetrieveEventId(var aEvent): TEventId;
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

procedure TMeCustomEventFeature.BeforeExecute(Sender: TObject; MethodItem:
  TMeInterceptedMethodItem; const Params: PMeProcParams);
var
  vItem: PMeCustomPublisherInfo;
  v: PDispatchMessage;
  vEventInfo: PMeCustomEventInfo;
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
  vItem: PMeCustomPublisherInfo;
begin
  with FPublisherInfoList.LockList^ do
  try
    for i := Count -1 downto 0 do
    begin
      vItem := PMeCustomPublisherInfo(Items[i]);
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
  vItem: PMeCustomPublisherInfo;
begin
  with FPublisherInfoList.LockList^ do
  try
    for i := Count -1 downto 0 do
    begin
      vItem := PMeCustomPublisherInfo(Items[i]);
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

function TMeCustomEventFeature.FindPublisher(const Instance : TObject): PMeCustomPublisherInfo;
var
  i: Integer;
begin
  with FPublisherInfoList.LockList^ do
  try
    for i := 0 to Count -1 do
    begin
      Result := PMeCustomPublisherInfo(Items[i]);
      if Assigned(Result) and (Result.Publisher  = Instance) then
        exit;
    end;
  finally
    FPublisherInfoList.UnLockList;
  end;
  Result := nil;
end;

function TMeCustomEventFeature.GetEvent(const aPublisher: TObject; const aEventId: TEventId): PMeCustomEventInfo;
var
  vPublisherInfo: PMeCustomPublisherInfo;
begin
  Result := nil;
  vPublisherInfo := FindPublisher(aPublisher);
  if Assigned(vPublisherInfo) then
  begin
    Result := vPublisherInfo.FindEventById(aEventId);
  end;
end;

function TMeCustomEventFeature.IndexOfPublisher(const Instance : TObject): Integer;
var
  vItem: PMeCustomPublisherInfo;
begin
  with FPublisherInfoList.LockList^ do
  try
    for Result := 0 to Count -1 do
    begin
      vItem := PMeCustomPublisherInfo(Items[Result]);
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

function TMeCustomEventFeature.RegisterEvent(const aPublisher: TObject; const aEventId: TEventId): PMeCustomEventInfo;
var
  i: Integer;
  vPublisherInfo: PMeCustomPublisherInfo;
begin
  Result := nil;
  i := IndexOfPublisher(aPublisher);
  if i < 0 then
  begin
    vPublisherInfo := PMeCustomPublisherInfo(NewMeObject(GetPublisherInfoClass));
    vPublisherInfo.FOwner := Self;
    vPublisherInfo.Publisher := aPublisher;
    i := FPublisherInfoList.Add(vPublisherInfo);
    if i < 0 then
    begin
      MeFreeAndNil(vPublisherInfo);
    end;
  end
  else
    vPublisherInfo := PMeCustomPublisherInfo(FPublisherInfoList.Get(i));
  if Assigned(vPublisherInfo) then
  begin
    i := vPublisherInfo.RegisterEventById(aEventId);
    if i >= 0 then
      Result := PMeCustomEventInfo(vPublisherInfo.Events.Get(i));
  end;
end;

procedure TMeCustomEventFeature.UnRegisterEvent(const aPublisher: TObject; const aEventId: TEventId);
var
  i: Integer;
  vPublisherInfo: PMeCustomPublisherInfo;
begin
  i := IndexOfPublisher(aPublisher);
  if i >= 0 then
  begin
    vPublisherInfo := FPublisherInfoList.Get(i);
    i := vPublisherInfo.IndexOfEvent(aEventId);
    if i >= 0 then
    begin
      PMeCustomEventInfo(vPublisherInfo.Events.Get(i)).Free;
      vPublisherInfo.Events.Delete(i);
    end;
  end;
end;

{ TMeWindowMessageFeature }
function TMeWindowMessageFeature.GetPublisherInfoClass: TMeClass;
begin
  Result := TypeOf(TMeCustomPublisherInfo);
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
