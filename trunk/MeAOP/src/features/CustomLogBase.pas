{ Summary : Custom Log Component Class. }
{ Description
  ##$Id: CustomLogBase.pas,v 1.1 2008/02/08 15:48:50 riceball Exp $
  ##Initial Date: 2003

  See Also
    参阅

  Bugs
    已知问题。

  Internal
    内部开发人员参阅，不会对外。

  TODO
    待作事项。

  Author
    Riceball LEE(riceball@cq118.com)
    Riceball LEE(riceballl@hotmail.com)

  Copyright
    Copyright(C) 2003 - 2008 by Riceball LEE

  Current Version
    $Revision: 1.1 $
    <Pre>
       Last Modified by $Author: riceball $ 
       $Date: 2008/02/08 15:48:50 $ 
    </Pre>

  History
    <Pre>
    $Log: CustomLogBase.pas,v $
    Revision 1.1  2008/02/08 15:48:50  riceball
    *** empty log message ***

    Revision 1.5  2006/05/14 11:58:38  riceball
    *** empty log message ***

    </Pre>
}
unit CustomLogBase;

{$I MeSetting.inc}
{.$define Debug}

interface

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif}
  //JclStrings,
  {$IFDEF LINUX}
  Types,
  Libc,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF COMPILER5_UP}
  SyncObjs, // critical sections
  {$ENDIF}
  SysUtils, 
  Classes;

type
  TLogLevel = 
  ( 
    vlNone,
    vlErrors,
    vlAudit,
    vlWarnings,
    vlNormal,
    vlVerbose,
    vlDebug     
  );

const
  vlVeryQuiet = vlErrors;
  vlQuiet     = vlWarnings;

const
  LF = #10;
  CR = #13;
  EOL = CR + LF;
  //EOL = sLineBreak;

const
  LogLevelStrings: array[TLogLevel] of string =
  (
    '',
    'Errors',
    'Audits',
    'Warnings',
    'Normal',
    'Verbose',
    'Debug'     
  );
  
resourcestring
  RSLogEOL = '<EOL>'; //End of Line
  RSLogCR  = '<CR>'; //Carriage Return
  RSLogLF  = '<LF>'; //Line feed

type
  { Summary : Defines a component logging Framework. }
  { Description
  TCustomLogBase is an abstract class that
  defines a framework for logging general 
  purposed information.
  
  TCustomLogBase is a descendant of TComponent. 
  TCustomLogBase is abstract component for log.
  
  Messages written to the log may optionally 
  transform all EOL characters, (
  Carriage Return + Line Feed) by default, 
  to the token '<EOL>'. A log message can 
  optionally generate the date and time 
  it was written to the log.
  
  Note: TCustomLogBase does not specify the 
  destination for log messages. TCustomLogBase
  descendants must implement the virtual 
  methods in the class to resolve where log 
  messages are stored, or written.
  
  TCustomLogBase is the ancestor class for the 
  TLogDebug component. TLogDebug demonstrates 
  an implementation that can store messages 
  in a file stream or write messages to the 
  standard Debugger Output.
  }
  TCustomLogBase = class(TComponent)
  private
    FActive: Boolean;
    FLevel: TLogLevel;
    FLogDate: Boolean;
    FLogFields: string;
    FLogTime: Boolean;
    FReplaceCRLF: Boolean;
    FSoftware: string;
    FStreamedActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetLogFields(const Value: string);
    procedure SetSoftware(const Value: string);
  protected
    FIsUpdateLogHead: Boolean;
    FLock: {$IFDEF COMPILER5_UP} TCriticalSection {$ELSE} TRTLCriticalSection
            {$ENDIF};
    { Summary : format the log message. }
    function FormatLog(const aLevel: TLogLevel; const aMsg: string; aIndent:
            Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}; aDateTime:
            TDateTime {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
            virtual;
    { Summary : record the log message of the specified level. }
    procedure InternalLog(aLevel: TLogLevel; aMsg: string); virtual;
    { Summary : record the log message of the specified level. }
    procedure InternalLogEx(aLevel: TLogLevel; aSubject, aMsg: string); virtual;
    procedure Loaded; override;
    procedure Lock;
    procedure UnLock;
    procedure WriteLog(aMsg: string); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Summary : Close to stop recording log. }
    procedure Close; virtual;
    { Summary : record the log message of the specified level. }
    procedure Log(const aLevel: TLogLevel; const aMsg: string);
    { Summary : record the log message of the specified level. }
    procedure LogEx(const aLevel: TLogLevel; const aSubject, aMsg: string);
    { Summary : Open to record Log. }
    procedure Open; virtual;
    { Summary : Start or stop logging of messages. }
    { Description
    Active is used to start or stop logging of messages. 
    Set Active to True to begin message logging. 
    The logging destination will be Opened if it is 
    currently Closed. Set Active to False to stop message 
    logging. The logging System will be Closed if it 
    is currently Open.
    }
    property Active: Boolean read FActive write SetActive;
    { Summary : the recorded Log Level. }
    property Level: TLogLevel read FLevel write FLevel default vlErrors;
    { Summary : Specifies if log messages are date-stamped. }
    { Description
    LogDate is a Boolean property that indicates if the log 
    message should automatically include the current date 
    and time when the message is created. The default value 
    for LogDate is True, as assigned in the Create 
    constructor.
    
    
    If LogDate is True, a message will have the current date 
    prepended to the message contents. Date
    is formatted using the value given in the global 
    ShortDateFormat variable.
    
    
    LogDate is called from virtual methods in the class as 
    a part of message formatting routines.
    }
    property LogDate: Boolean read FLogDate write FLogDate default True;
    { Summary you must define your logfields }
    { Description
    the first fields is always: " x-level [date [time]]"
    字段之间用空格分隔
    }
    property LogFields: string read FLogFields write SetLogFields;
    { Summary : Specifies if log messages are time-stamped. }
    { Description
    LogTime is a Boolean property that indicates if the log 
    message should automatically include the current 
    time when the message is created. The default value 
    for LogTime is True, as assigned in the Create 
    constructor.
    
    
    If LogTime is True, a message will have the current
    time prepended to the message contents. time
    is formatted using the value given in the global 
    ShortDateFormat variable.
    
    
    LogTime is called from virtual methods in the class as 
    a part of message formatting routines.
    }
    property LogTime: Boolean read FLogTime write FLogTime default True;
    { Summary : Indicates that line-end characters in a message should be
            replace with tags. }
    { Description
    ReplaceCRLF is a Boolean property that indicates if the 
    line-end characters CR, LF, and EOL in a log message 
    should be replace with their tag counterparts "<CR>", 
    "<LF>", and "<EOL>". 
    }
    property ReplaceCRLF: Boolean read FReplaceCRLF write FReplaceCRLF default
            True;
    { Summary the software name and version for log }
    property Software: string read FSoftware write SetSoftware;
  end;
  

//the thread safe FormatDateTime 
function FormatDateTimeS(const Format: string; aDateTime: TDateTime): string;

//return GMT now.
function GMTNow: TDateTime;

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);

implementation

function StrLeft(const S: string; Count: Integer): string;
begin
  Result := Copy(S, 1, Count);
end;

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  Left: string;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if S <> '' then
      List.Add(S);  // Ignore empty strings at the end.
  finally
    List.EndUpdate;
  end;
end;

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

function DateTimeToStr(const aDateTime: TDateTime): string;
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

function DateToStr(const aDate: TDateTime): string;
var
  vFormatSettings: TFormatSettings;
begin
  vFormatSettings.DateSeparator := '-';
  vFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  vFormatSettings.ShortDateFormat := LongDateFormat;
  Result := SysUtils.DateToStr(aDate, vFormatSettings);
end;

function TimeToStr(const aTime: TDateTime): string;
var
  vFormatSettings: TFormatSettings;
begin
  vFormatSettings.TimeSeparator := ':';
  vFormatSettings.LongTimeFormat := 'hh:nn:ss';
  vFormatSettings.ShortTimeFormat := LongTimeFormat;
  Result := SysUtils.TimeToStr(aTime, vFormatSettings);
end;

{
******************************** TCustomLogBase ********************************
}
constructor TCustomLogBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF COMPILER5_UP}
    FLock:=TCriticalSection.Create;
  {$ELSE}
    InitializeCriticalSection(FLock);
  {$ENDIF}
  FLogTime := True;
  FReplaceCRLF := True;
  FLevel := vlErrors;
  FLogDate := True;
end;

destructor TCustomLogBase.Destroy;
begin
  Active := False;
  {$IFDEF COMPILER5_UP}
    FLock.Free;
  {$ELSE}
    DeleteCriticalSection(FLock);
  {$ENDIF}
  inherited Destroy;
end;

procedure TCustomLogBase.Close;
begin
  FActive := False;
end;

function TCustomLogBase.FormatLog(const aLevel: TLogLevel; const aMsg: string;
        aIndent: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF};
        aDateTime: TDateTime {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}):
        string;
var
  s: string;
begin
  s := '';
  if aLevel <> vlNone then
    s :=  '[' + LogLevelStrings[aLevel] + '] ';
  
  if aDateTime <> 0 then
  begin
    if LogDate then
      s := s + DateToStr(aDateTime) + ' ';
    if LogTime then
      s := s + TimeToStr(aDateTime) + ' ';
  end;
  Result := StringOfChar(' ', aIndent) + s + aMsg;
end;

procedure TCustomLogBase.InternalLog(aLevel: TLogLevel; aMsg: string);
var
  Lines: TStringList;
  I: Integer;
  dt: TDateTime;
  s: string;
begin
  if FIsUpdateLogHead then
  begin
    if FSoftware <> '' then
      WriteLog('#Software: ' + FSoftware);
    WriteLog('#Version: 1.0');
    WriteLog('#Date: ' + DateTimeToStr(GMTNow));
    s := '#Fields: x-level ';
    if FLogDate then
      s := s + 'date ';
    if FLogTime then
      s := s + 'time ';
    if FLogFields <> '' then
      s := s + FLogFields;
    WriteLog(s);
    FIsUpdateLogHead := False;
  end;
  
  if FLogTime or FLogDate then
    dt := GMTNow
  else
    dt := 0;
  if FReplaceCRLF then
  begin
    aMsg := StringReplace(aMsg, EOL, RSLogEOL, [rfReplaceAll]);
    aMsg := StringReplace(aMsg, CR, RSLogCR, [rfReplaceAll]);
    aMsg := StringReplace(aMsg, LF, RSLogLF, [rfReplaceAll]);
    aMsg := FormatLog(aLevel,aMsg, 0, dt);
    WriteLog(aMsg);
  end
  else
  begin
    //Seperate MultiLine
    Lines := TStringList.Create;
    try
      aMsg := StringReplace(aMsg, CR, '', [rfReplaceAll]);
      aMsg := aMsg + ' ';
      StrToStrings(aMsg, #10, Lines);
      aMsg := FormatLog(aLevel, Lines[0], 0, dt);
      WriteLog(aMsg);
      I := 1;
      while I < Lines.Count do
      begin
        //other lines
        aMsg := FormatLog(vlNone, '|'+Lines[i], 2, 0);
        WriteLog(aMsg);
        Inc(I);
      end;
    finally
      Lines.Free;
    end;
  end;
end;

procedure TCustomLogBase.InternalLogEx(aLevel: TLogLevel; aSubject, aMsg:
        string);
begin
  aMsg := aSubject + ':'+ aMsg;
  InternalLog(aLevel, aMsg);
end;

procedure TCustomLogBase.Loaded;
begin
  Active := FStreamedActive;
end;

procedure TCustomLogBase.Lock;
begin
  {$IFDEF COMPILER5_UP}
    FLock.Enter;
  {$ELSE}
    EnterCriticalSection(FLock);
  {$ENDIF}
end;

procedure TCustomLogBase.Log(const aLevel: TLogLevel; const aMsg: string);
var
  Lines: TStringList;
  I: Integer;
  dt: TDateTime;
begin
  if FActive and (FLevel >= aLevel) then
  try
    Lock;
    try
      InternalLog(aLevel, aMsg);
    finally
      UnLock;
    end;
  except
    FActive := False;
    raise;
  end;
end;

procedure TCustomLogBase.LogEx(const aLevel: TLogLevel; const aSubject, aMsg:
        string);
var
  Lines: TStringList;
  I: Integer;
  dt: TDateTime;
begin
  if FActive and (FLevel >= aLevel) then
  try
    Lock;
    try
      InternalLogEx(aLevel, aSubject, aMsg);
    finally
      UnLock;
    end;
  except
    FActive := False;
    raise;
  end;
end;

procedure TCustomLogBase.Open;
begin
  FActive := True;
end;

procedure TCustomLogBase.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
    FStreamedActive := Value
  else
    if FActive <> Value then
    begin
      //FActive := Value;
      if Value then
        Open
      else
        Close;
    end;
end;

procedure TCustomLogBase.SetLogFields(const Value: string);
begin
  if FLogFields <> Value then
  begin
    FLogFields := Value;
    Lock;
    try
      FIsUpdateLogHead := True;
    finally
      Unlock;
    end;
  end;
end;

procedure TCustomLogBase.SetSoftware(const Value: string);
begin
  if FSoftware <> Value then
  begin
    FSoftware := Value;
    Lock;
    try
      FIsUpdateLogHead := True;
    finally
      Unlock;
    end;
  end;
end;

procedure TCustomLogBase.UnLock;
begin
  {$IFDEF COMPILER5_UP}
    FLock.Leave;
  {$ELSE}
    LeaveCriticalSection(FLock);
  {$ENDIF}
end;


initialization
finalization
end.
