{ Summary : MeObject Logging of Application Class. }
{ Description
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision$


  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
      or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeLog.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2003-2008
    * All rights reserved.

    * Contributor(s):
}
unit uMeLog;

{$I MeSetting.inc}
{.$define Debug}

interface

uses
  {$IFDEF DEBUG}
  DbugIntf,
  {$ENDIF}
  {$IFDEF LINUX}
  Types,
  Libc,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils
  , uMeObject
  , uMeSyncObjs // critical sections
  ;

type
  TMeLogLevel = Longint;

const
  { Levels of logging as integer values. }
  vlOff   = High(TMeLogLevel);
  vlFatal = 50000;
  vlError = 40000;
  vlAudit = 35000;
  vlWarning  = 30000;
  vlInfo  = 20000;
  vlVerbose  = 15000;
  vlDebug = 10000;
  vlAll   = Low(TMeLogLevel);

ResourceString
  rsFatal  =  'Fatal';
  rsError  =  'Error';
  rsAudit  =  'Audit';
  rsWarning =  'Warning';
  rsInfo   =   'Info';
  rsVerbose =   'Verbose';
  rsDebug =  'Debug';

const
  vlNormal    = vlInfo;
  vlVeryQuiet = vlError;
  vlQuiet     = vlWarning;

const
  LF = #10;
  CR = #13;
  EOL = CR + LF;
  //EOL = sLineBreak;

const
  cLoggerRevision = '$Revision$';

resourcestring
  RSLogEOL = '<EOL>'; //End of Line
  RSLogCR  = '<CR>'; //Carriage Return
  RSLogLF  = '<LF>'; //Line feed

type
  PMeCustomLogger = ^ TMeCustomLogger;
  PMeRootLogger = ^ TMeRootLogger;
  PMeStringsLogger = ^ TMeStringsLogger;
  PMeStreamLogger = ^ TMeStreamLogger;
  PMeDebugLogger = ^ TMeDebugLogger;
  

  { Summary : the abstract logger class for logging . }
  { Description
  TMeCustomLogger is an abstract class that
  defines a framework for logging general
  purposed information.
  }
  TMeCustomLogger = object(TMeDynamicObject)
  protected
    FRootLogger: PMeRootLogger;
    procedure WriteLog(aMsg: string); virtual; abstract;
  public
    destructor Destroy; virtual;//override;
    { Summary : Close to stop recording log. }
    procedure Close; virtual;
    { Summary : Open to record Log. }
    procedure Open; virtual;
    property RootLogger: PMeRootLogger read FRootLogger;
  end;

  { Summary : the singleton root logger class for logging . }
  { Description
    AddLogger the logger object here. the logging message can be sent to the AddLoggered logger.
  }
  TMeRootLogger = object(TMeCustomLogger)
  protected
    FActive: Boolean;
    FLoggers: PMeList;
    FLogDate: Boolean;
    FLogTime: Boolean;
    FLogFields: string;
    FLevel: TMeLogLevel;
    FReplaceCRLF: Boolean;
    FSoftware: string;
    procedure SetActive(Value: Boolean);
    procedure SetLogFields(const Value: string);
    procedure SetSoftware(const Value: string);
  protected
    FIsUpdateLogHead: Boolean;
    FLock: PMeCriticalSection;

    { Summary : format the log message. }
    function FormatLog(const aLevel: TMeLogLevel; const aMsg: string; aIndent:
            Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}; aDateTime:
            TDateTime {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
            virtual;
    { Summary : record the log message of the specified level. }
    procedure InternalLog(const aLevel: TMeLogLevel; aMsg: string); virtual;
    { Summary : record the log message of the specified level. }
    procedure InternalLogEx(const aLevel: TMeLogLevel; const aSubject, aMsg: string); virtual;
    procedure WriteLog(aMsg: string); virtual; //override;
    procedure Init; virtual; //override;
    class function GetLogLevelString(const aLevel: TMeLogLevel): string; virtual;
  public
    destructor Destroy; virtual;//override;
    { Summary : Close to stop recording log. }
    procedure Close; virtual; //override;
    { Summary : record the log message of the specified level. }
    procedure Lock;
    procedure UnLock;
    procedure Log(const aLevel: TMeLogLevel; const aMsg: string);{$IFDEF SUPPORTS_OVERLOAD} overload;
    procedure Log(const aLevel: TMeLogLevel; const aFmt: string; const Args: array of const);overload;{$ENDIF}
    procedure LogFmt(const aLevel: TMeLogLevel; const aFmt: string; const Args: array of const);
    procedure Fatal(const aMsg: string);{$IFDEF SUPPORTS_OVERLOAD} overload;
    procedure Fatal(const aMsg: string; const Args: array of const);overload;{$ENDIF}
    procedure FatalFmt(const aMsg: string; const Args: array of const);
    procedure Error(const aMsg: string);{$IFDEF SUPPORTS_OVERLOAD} overload;
    procedure Error(const aMsg: string; const Args: array of const);overload;{$ENDIF}
    procedure ErrorFmt(const aMsg: string; const Args: array of const);
    procedure Audit(const aMsg: string);{$IFDEF SUPPORTS_OVERLOAD} overload;
    procedure Audit(const aMsg: string; const Args: array of const);overload;{$ENDIF}
    procedure AuditFmt(const aMsg: string; const Args: array of const);
    procedure Warn(const aMsg: string);{$IFDEF SUPPORTS_OVERLOAD} overload;
    procedure Warn(const aMsg: string; const Args: array of const);overload;{$ENDIF}
    procedure WarnFmt(const aMsg: string; const Args: array of const);
    procedure Info(const aMsg: string);{$IFDEF SUPPORTS_OVERLOAD} overload;
    procedure Info(const aMsg: string; const Args: array of const);overload;{$ENDIF}
    procedure InfoFmt(const aMsg: string; const Args: array of const);
    procedure Verbose(const aMsg: string);{$IFDEF SUPPORTS_OVERLOAD} overload;
    procedure Verbose(const aMsg: string; const Args: array of const);overload;{$ENDIF}
    procedure VerboseFmt(const aMsg: string; const Args: array of const);
    procedure Debug(const aMsg: string);{$IFDEF SUPPORTS_OVERLOAD} overload;
    procedure Debug(const aMsg: string; const Args: array of const);overload;{$ENDIF}
    procedure DebugFmt(const aMsg: string; const Args: array of const);
    { Summary : record the log message of the specified level. }
    procedure LogEx(const aLevel: TMeLogLevel; const aSubject, aMsg: string);
    { Summary : Open to record Log. }
    procedure Open; virtual; //override;
    function AddLogger(const aLogger: PMeCustomLogger): Boolean;

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
    property Level: TMeLogLevel read FLevel write FLevel default vlError;
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

  { Summary: for logging information to the MeStrings. }
  TMeStringsLogger = object(TMeCustomLogger)
  protected
    FStrings: PMeStrings;
    procedure SetStrings(const Value: PMeStrings);
  protected
    procedure WriteLog(aMsg: string); virtual; //override;
  public
    constructor Create(const aStrings: PMeStrings);
    procedure Open; virtual; //override;
    property Strings: PMeStrings read FStrings write SetStrings;
  end;

  { Summary: abstract stream logger for logging information to the MeStream. }
  TMeStreamLogger = object(TMeCustomLogger)
  protected
    FStream: PMeStream;
    procedure SetStream(const Value: PMeStream);
  protected
    procedure WriteLog(aMsg: string); virtual; //override;
  public
    constructor Create(const aStream: PMeStream);
    procedure Open; virtual; //override;
    property Stream: PMeStream read FStream write SetStream;
  end;

  { Summary : for logging information to the DebugOutput stream. }
  { Description
  extends the framework for logging information to the DebugOutput stream.
  
  TMeDebugLogger provides a flexible means of selecting 
  the destination of log messages. Messages can be 
  written to either a file or the WIN32 API Debug 
  Output stream.
  
  TMeDebugLogger is very useful for capturing information 
  for debugging, trouble-shooting, and general 
  feedback purposes.
  }
  TMeDebugLogger = object(TMeCustomLogger)
  protected
    procedure WriteLog(aMsg: string); virtual; //override;
  end;

function GLogger: PMeRootLogger;

{ Summary : Sends a string to the system debugger.}
{ Description
  DebugOutput is a procedure used to send the string in AText to either the active debugger for a process, or the system debugger. If there is no active debugger for the current application, DebugOutput has no effect.
  DebugOutput encapsulates the platform-specific calls needed to send the string to the debugger for the current application. For the Windows platform, the WIN32 API procedure is OutputDebugString. On the Linux platform, DebugOutput writes the message followed by CRLF to the standard error handle (StdErr).

  Parameters:
   AText: Value to be sent to the debugger.
}
procedure DebugOutput(const AText: string);

implementation

uses
  uMeSysUtils, uMeStrUtils;

var
  FLogger: PMeRootLogger = nil;

function GLogger: PMeRootLogger;
begin
  if not Assigned(FLogger) then
    New(FLogger, Create);
  Result := FLogger;
end;

{ TMeCustomLogger }
destructor TMeCustomLogger.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TMeCustomLogger.Close;
begin
end;

procedure TMeCustomLogger.Open;
begin
end;

{ TMeRootLogger }
procedure TMeRootLogger.Init;
begin
  inherited;
  New(FLock, Create);
  New(FLoggers, Create);
  FLogTime := True;
  FReplaceCRLF := True;
  FLevel := vlError;
  FLogDate := True;
end;

destructor TMeRootLogger.Destroy;
begin
  //Active := False;
  FLoggers.FreeMeObjects;
  FLoggers.Free;
  inherited Destroy;
  FLock.Free;
  FLogFields := '';
  FSoftware := '';
end;

procedure TMeRootLogger.Close;
var
  i: Integer;
begin
  if FActive then
  begin
    Lock;
    try
      FActive := False;
      for i := 0 to FLoggers.Count - 1 do
      begin
        PMeCustomLogger(FLoggers.Items[i]).Close;
      end;
    finally
      UnLock;
    end;
  end;
end;

function TMeRootLogger.FormatLog(const aLevel: TMeLogLevel; const aMsg: string;
        aIndent: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF};
        aDateTime: TDateTime {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}):
        string;
var
  s: string;
begin
  s := '';
  if aLevel <> vlOff then
    s :=  '[' + GetLogLevelString(aLevel) + '] ';

  if aDateTime <> 0 then
  begin
    if LogDate then
      s := s + DateToStrS(aDateTime) + ' ';
    if LogTime then
      s := s + TimeToStrS(aDateTime) + ' ';
  end;
  Result := StringOfChar(' ', aIndent) + s + aMsg;
end;

procedure TMeRootLogger.InternalLog(const aLevel: TMeLogLevel; aMsg: string);
var
  vLines: PMeStrings;
  I: Integer;
  dt: TDateTime;
  s: string;
begin
  if FIsUpdateLogHead then
  begin
    if FSoftware <> '' then
      WriteLog('#Software: ' + FSoftware);
    WriteLog(cLoggerRevision);
    WriteLog('#Date: ' + DateTimeToStrS(GMTNow));
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
    New(vLines, Create);
    try
      aMsg := StringReplace(aMsg, CR, '', [rfReplaceAll]);
      aMsg := aMsg + ' ';
      StrToStrings(aMsg, #10, vLines);
      aMsg := FormatLog(aLevel, vLines.Items[0], 0, dt);
      WriteLog(aMsg);
      I := 1;
      while I < vLines.Count do
      begin
        //other lines
        aMsg := FormatLog(vlOff, '|'+vLines.Items[i], 2, 0);
        WriteLog(aMsg);
        Inc(I);
      end;
    finally
      vLines.Free;
    end;
  end;
end;

procedure TMeRootLogger.InternalLogEx(const aLevel: TMeLogLevel; const aSubject, aMsg: string);
begin
  InternalLog(aLevel, aSubject + ':'+ aMsg);
end;

class function TMeRootLogger.GetLogLevelString(const aLevel: TMeLogLevel): string;
begin
  Result := '';
  case aLevel of
    vlFatal..vlOff-1: Result := rsFatal;
    vlError..vlFatal-1: Result := rsError;
    vlAudit..vlError-1: Result := rsAudit;
    vlWarning..vlAudit-1: Result := rsWarning;
    vlInfo..vlWarning-1: Result := rsInfo;
    vlVerbose..vlInfo-1: Result := rsVerbose;
    vlDebug..vlVerbose-1: Result := rsDebug;
  end;
end;

procedure TMeRootLogger.Lock;
begin
  FLock.Enter;
end;

procedure TMeRootLogger.Fatal(const aMsg: string);
begin
  Log(vlFatal, aMsg);
end;

{$IFDEF SUPPORTS_OVERLOAD}
procedure TMeRootLogger.Fatal(const aMsg: string; const Args: array of const);
begin
  FatalFmt(aMsg, Args);
end;
{$ENDIF}

procedure TMeRootLogger.FatalFmt(const aMsg: string; const Args: array of const);
begin
  LogFmt(vlFatal, aMsg, Args);
end;

{$IFDEF SUPPORTS_OVERLOAD}
procedure TMeRootLogger.Error(const aMsg: string; const Args: array of const);
begin
  ErrorFmt(aMsg, Args);
end;
{$ENDIF}

procedure TMeRootLogger.Error(const aMsg: string);
begin
  Log(vlError, aMsg);
end;

procedure TMeRootLogger.ErrorFmt(const aMsg: string; const Args: array of const);
begin
  LogFmt(vlError, aMsg, Args);
end;

{$IFDEF SUPPORTS_OVERLOAD}
procedure TMeRootLogger.Audit(const aMsg: string; const Args: array of const);
begin
  AuditFmt(aMsg, Args);
end;
{$ENDIF}

procedure TMeRootLogger.Audit(const aMsg: string);
begin
  Log(vlAudit, aMsg);
end;

procedure TMeRootLogger.AuditFmt(const aMsg: string; const Args: array of const);
begin
  LogFmt(vlAudit, aMsg, Args);
end;

{$IFDEF SUPPORTS_OVERLOAD}
procedure TMeRootLogger.Warn(const aMsg: string; const Args: array of const);
begin
  WarnFmt(aMsg, Args);
end;
{$ENDIF}

procedure TMeRootLogger.Warn(const aMsg: string);
begin
  Log(vlWarning, aMsg);
end;

procedure TMeRootLogger.WarnFmt(const aMsg: string; const Args: array of const);
begin
  LogFmt(vlWarning, aMsg, Args);
end;

procedure TMeRootLogger.Info(const aMsg: string);
begin
  Log(vlInfo, aMsg);
end;

{$IFDEF SUPPORTS_OVERLOAD} 
procedure TMeRootLogger.Info(const aMsg: string; const Args: array of const);
begin
  InfoFmt(aMsg, Args);
end;
{$ENDIF}

procedure TMeRootLogger.InfoFmt(const aMsg: string; const Args: array of const);
begin
  LogFmt(vlInfo, aMsg, Args);
end;

{$IFDEF SUPPORTS_OVERLOAD} 
procedure TMeRootLogger.Verbose(const aMsg: string; const Args: array of const);
begin
  VerboseFmt(aMsg, Args);
end;
{$ENDIF}

procedure TMeRootLogger.Verbose(const aMsg: string);
begin
  Log(vlVerbose, aMsg);
end;

procedure TMeRootLogger.VerboseFmt(const aMsg: string; const Args: array of const);
begin
  LogFmt(vlVerbose, aMsg, Args);
end;

{$IFDEF SUPPORTS_OVERLOAD} 
procedure TMeRootLogger.Debug(const aMsg: string; const Args: array of const);
begin
  LogFmt(vlDebug, aMsg, Args);
end;
{$ENDIF}

procedure TMeRootLogger.Debug(const aMsg: string);
begin
  Log(vlDebug, aMsg);
end;

procedure TMeRootLogger.DebugFmt(const aMsg: string; const Args: array of const);
begin
  LogFmt(vlDebug, aMsg, Args);
end;

procedure TMeRootLogger.Log(const aLevel: TMeLogLevel; const aMsg: string);
begin
  if FActive and (aLevel >= FLevel) and (aLevel <> vlOff) then
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

{$IFDEF SUPPORTS_OVERLOAD}
procedure TMeRootLogger.Log(const aLevel: TMeLogLevel; const aFmt: string; const Args: array of const);
begin
  Log(aLevel, FormatS(aFmt, Args));
end;
{$ENDIF}

procedure TMeRootLogger.LogFmt(const aLevel: TMeLogLevel; const aFmt: string; const Args: array of const);
begin
  Log(aLevel, FormatS(aFmt, Args));
end;

procedure TMeRootLogger.LogEx(const aLevel: TMeLogLevel; const aSubject, aMsg: string);
begin
  if FActive and (aLevel >= FLevel) and (aLevel <> vlOff) then
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

procedure TMeRootLogger.Open;
var
  i: Integer;
begin
  if not FActive then
  begin
    Lock;
    try
      FActive := True;
      for i := 0 to FLoggers.Count - 1 do
      begin
        PMeCustomLogger(FLoggers.Items[i]).Open;
      end;
    finally
      Unlock;
    end;
  end;
end;

function TMeRootLogger.AddLogger(const aLogger: PMeCustomLogger): Boolean;
begin
  Result := FLoggers.IndexOf(aLogger) < 0;
  if Result then
  begin
    FLoggers.Add(aLogger);
    aLogger.FRootLogger := @Self;
    Lock;
    try
      if FActive then
        aLogger.Open;
    finally
      Unlock;
    end;
  end;
end;

procedure TMeRootLogger.SetActive(Value: Boolean);
begin
    if FActive <> Value then
    begin
      //FActive := Value;
      if Value then
        Open
      else
        Close;
    end;
end;

procedure TMeRootLogger.SetLogFields(const Value: string);
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

procedure TMeRootLogger.SetSoftware(const Value: string);
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

procedure TMeRootLogger.UnLock;
begin
  FLock.Leave;
end;

procedure TMeRootLogger.WriteLog(aMsg: string);
var
  i: Integer;
begin
  for i := 0 to FLoggers.Count - 1 do
  begin
    PMeCustomLogger(FLoggers.Items[i]).WriteLog(aMsg);
  end;
end;

{ TMeStringsLogger }
constructor TMeStringsLogger.Create(const aStrings: PMeStrings);
begin
  inherited Create;
  FStrings := aStrings;
end;

procedure TMeStringsLogger.Open;
begin
  FStrings.Clear;
end;

procedure TMeStringsLogger.SetStrings(const Value: PMeStrings);
begin
  if Value <> FStrings then
  begin
    if Assigned(FRootLogger) then
      FRootLogger.Lock;
    try
      FStrings := Value;
    finally
      if Assigned(FRootLogger) then
        FRootLogger.Unlock;
    end;
  end;
end;

procedure TMeStringsLogger.WriteLog(aMsg: string);
begin
  FStrings.Add(aMsg);
end;

{ TMeStreamLogger }
constructor TMeStreamLogger.Create(const aStream: PMeStream);
begin
  inherited Create;
  FStream := aStream;
end;

procedure TMeStreamLogger.Open;
begin
  //FStream.Seek(0, soEnd);
end;

procedure TMeStreamLogger.SetStream(const Value: PMeStream);
begin
  if Value <> FStream then
  begin
    if Assigned(FRootLogger) then
      FRootLogger.Lock;
    try
      FStream := Value;
    finally
      if Assigned(FRootLogger) then
        FRootLogger.Unlock;
    end;
  end;
end;

procedure TMeStreamLogger.WriteLog(aMsg: string);
begin
  FStream.WriteBuffer(aMsg[1], Length(aMsg));
end;

procedure DebugOutput(const AText: string);
begin
  {$IFDEF LINUX}
  __write(stderr, AText, Length(AText));
  __write(stderr, EOL, Length(EOL));
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AText));
  {$ENDIF}
end;

procedure TMeDebugLogger.WriteLog(aMsg: string);
begin
  aMsg := aMsg + EOL;
  DebugOutput(aMsg);
end;

initialization
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeCustomLogger), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeRootLogger), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeStringsLogger), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeDebugLogger), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeStreamLogger), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeCustomLogger), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeRootLogger), ovtVmtParent, TypeOf(TMeCustomLogger));
  SetMeVirtualMethod(TypeOf(TMeStringsLogger), ovtVmtParent, TypeOf(TMeCustomLogger));
  SetMeVirtualMethod(TypeOf(TMeDebugLogger), ovtVmtParent, TypeOf(TMeCustomLogger));
  SetMeVirtualMethod(TypeOf(TMeStreamLogger), ovtVmtParent, TypeOf(TMeCustomLogger));
finalization
  MeFreeAndNil(FLogger);
end.
