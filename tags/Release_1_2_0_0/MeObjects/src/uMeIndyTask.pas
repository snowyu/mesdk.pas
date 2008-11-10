
{Summary the Indy Net Task class ..}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision$

  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeIndyTask.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):
}
unit uMeIndyTask;

interface

{$I MeSetting.inc}

uses
  SysUtils, Classes //TStream
  , uMeObject
  , uMeStream
  , uMeSysUtils
  , uMeThread
  , uMeURI

  //, IdGlobal
  //, IdGlobalProtocols //GMTXXX
  , IdComponent
  , IdException
  , IdExceptionCore
  , IdHeaderList
  , IdAuthenticationDigest //MD5-Digest authentication
  , IdURI, IdCookie, IdCookieManager
  , IdHTTPHeaderInfo    //for HTTP request and response info.
  , IdHTTP
  , IdCompressorZLib
  {$ifdef UseOpenSsl}
  ,  IdSSLOpenSSL  //ssl
  ,  IdAuthenticationNTLM //NTLM - uses OpenSSL libraries
  {$endif}
  ;

const
  cDefaultTimeout = 30 * 60 * 1000; //30 min
  cDefaultUserAgent = 'Mozilla/4.0 (Windows; zh-CN) Gecko';

  //Global
  IdTimeoutDefault = -1;
  IdTimeoutInfinite = -2;

type
  PMeDownloadTask = ^ TMeDownloadTask;
  PMeDownloadPartTask = ^ TMeCustomDownloadPartTask;
  PMeDownloadInfo = ^ TMeDownloadInfo;
  PMeHttpDownloadSimpleTask = ^ TMeHttpDownloadSimpleTask;
  PMeHttpDownloadSimpleThreadMgrTask = ^ TMeHttpDownloadSimpleThreadMgrTask;

  TMeDownloadInfo = object(TMeDynamicObject)
  protected
    // the header properties is inited or not.
    FHeaderInited: Boolean;
    FURI: string;
    FContentLength: Int64;
    FHasContentLength: Boolean;
    //the response to tell the client it can be resume by unit:
    // Accept-Ranges: bytes
    FAcceptRanges: string;
    (*
    lValue := Values['Expires']; {do not localize}
    if IsNumeric(lValue) then
    begin
      // This is happening when expires is an integer number in seconds
      LSecs := IndyStrToInt(lValue);
      // RLebeau 01/23/2005 - IIS sometimes sends an 'Expires: -1' header
      if LSecs >= 0 then begin
        FExpires := Now +  (LSecs / SecsPerDay);
      end else begin
        FExpires := 0.0;
      end;
    end else
    begin
      FExpires := GMTToLocalDateTime(lValue);
    end;
    *)
    FExpires: TDateTime;
    //GMTToLocalDateTime(Values['Date'])
    FDate: TDateTime;
    //GMTToLocalDateTime(Values['Last-Modified'])
    FLastModified: TDateTime;
    //'Content-Version'
    FRevision: string; 
    FCanResume: Boolean;
    FConnectTimeout: Integer;
    FReadTimeout: Integer;

    procedure Init; virtual; //override
  public
    destructor Destroy; virtual; //override
    procedure Assign(const aSource: PMeDownloadInfo);

    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
  end;
{
Client send this request to server:
http.Request.Range := '0-'; //取所有的数据。
Range头域可以请求实体的一个或者多个子范围。例如，   
　　表示头500个字节：bytes=0-499   
　　表示第二个500字节：bytes=500-999   
　　表示最后500个字节：bytes=-500
　　表示500字节以后的范围：bytes=500-   
　　第一个和最后一个字节：bytes=0-0,-1   
　　同时指定几个范围：bytes=500-600,601-999   

　　但是服务器可以忽略此请求头，如果无条件GET包含Range请求头，响应会以状态码206（PartialContent）返回而不是以200   （OK）
    如果支持，那么服务器将返回： /后面的数字为Instance长度
     content-range: bytes 1-65536/102400
     content-range: bytes */102400
     content-range: bytes 1-65536/*
  when read or connection timeout, this should re-connection it.
}
  TMeDownloadTask = object(TMeTask)
  protected
    FMaxParts: Integer;
    //collect the TMeCustomDownloadPartTask.
    FParts: PMeThreadSafeList;
    FDownInfo: PMeDownloadInfo;

    //procedure DoPartTaskDone(const aPart: PMeDownloadPartTask);
    procedure BeforeRun; virtual; //override
    procedure Init; virtual; //override
  public
    destructor Destroy; virtual; //override

    //the MaxParts default is 1.
    property MaxParts: Integer read FMaxParts write FMaxParts;
    property Parts: PMeThreadSafeList read FParts;
  end;

  //abstract DownloadPart task
  TMeCustomDownloadPartTask = object(TMeTask)
  protected
    FURL: string;
    FStream: TStream;
    //owner
    FDownInfo: PMeDownloadInfo;
    FContentRangeEnd: Int64;
    FContentRangeStart: Int64;
    //the real downloaded size.
    FContentRangeInstanceLength: Int64;


    procedure SetStream(const aStream: TStream);
    //procedure AfterRun; virtual; //override
    //procedure BeforeRun; virtual; //override
    //function Run: Boolean; virtual; //override
    procedure Init; virtual; //override
    procedure BeforeRun; virtual; //override
  public
    constructor Create(const aDownInfo: PMeDownloadInfo; const aStream: TStream);
    destructor Destroy; virtual; //override

    property Stream: TStream read FStream write SetStream;
    //property URL: string read FURL write FURL;
    property ContentRangeEnd: Int64 read FContentRangeEnd;
    property ContentRangeStart: Int64 read FContentRangeStart;
    property ContentRangeInstanceLength: Int64 read FContentRangeInstanceLength;
  end;

  TMeHttpDownloadPartTask = object(TMeCustomDownloadPartTask)
  protected
    FHttp: TIdHTTP;
   {$ifdef UseOpenSsl}
    FIOSSL : TIdSSLIOHandlerSocketOpenSSL;
   {$endif}
    //the exception occur when execute the task, this will be true.
    FHasException: Boolean;

    FOnStatus: TIdStatusEvent;
    FOnWork: TWorkEvent;
    FOnWorkBegin: TWorkBeginEvent;
    FOnWorkEnd: TWorkEndEvent;

    procedure DoHeadersAvailable(Sender: TObject; aHeaders: TIdHeaderList; var vContinue: Boolean);
    procedure DoStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);

    procedure BeforeRun; virtual; //override
    function Run: Boolean; virtual; //override
    procedure HandleRunException(const Sender: PMeCustomThread; const aException: Exception; var aProcessed: Boolean); virtual;//override
    procedure Init; virtual; //override
  public
    destructor Destroy; virtual; //override
    property HasException: Boolean read FHasException;
    property OnStatus: TIdStatusEvent read FOnStatus write FOnStatus;
    property OnWork: TWorkEvent read FOnWork write FOnWork;
    property OnWorkBegin: TWorkBeginEvent read FOnWorkBegin write FOnWorkBegin;
    property OnWorkEnd: TWorkEndEvent read FOnWorkEnd write FOnWorkEnd;
  end;

  TMeHttpDownloadSimpleTask = object(TMeHttpDownloadPartTask)
  protected
    //procedure Init; virtual; //override
    procedure SetURL(const Value: string);
    procedure BeforeRun; virtual; //override
    function Run: Boolean; virtual; //override
  public
    constructor Create(const aURL: string; const aStream: TStream);
    destructor Destroy; virtual; //override
    property URL: string read FURL write SetURL;
  end;

  //TMeTaskDoneEvent = procedure(const aTask: PMeHttpDownloadSimpleTask) of object;
  TMeHttpDownloadSimpleThreadMgrTask = object(TMeThreadMgrTask)
  protected
    FIdleTasks: PMeThreadSafeList;
    //FOnTaskDone: TMeTaskDoneEvent;
    FProxyParameters: TIdProxyConnectionInfo;

    procedure DoThreadStopped(const aThread: PMeCustomThread); virtual; //override
    procedure Init; virtual; //override
  public
    destructor Destroy; virtual; //override
    procedure Download(const aURL: string; const aStream: TStream; const aTag: Integer = 0);

    property ProxyParameters: TIdProxyConnectionInfo read FProxyParameters;
    //this event must support thread-safe.
    //property OnTaskDone: TMeTaskDoneEvent read FOnTaskDone write FOnTaskDone;
  end;

implementation

uses
  uMeStrUtils;

type
  TIdHTTPAccess = class(TIdHTTP);

var
  LCompressor: TIdCompressorZLib;
  LCookieManager: TIdCookieManager;

{ TMeDownloadInfo }
procedure TMeDownloadInfo.Init;
begin
  inherited;
  //New(FURI, Create);
  FContentLength := -1;
  FReadTimeout := IdTimeoutDefault;
  FConnectTimeout := IdTimeoutDefault;
end;

destructor TMeDownloadInfo.Destroy;
begin
  //MeFreeAndNil(FURI);
  FURI := '';
  FRevision := '';
  FAcceptRanges := '';
  inherited;
end;

procedure TMeDownloadInfo.Assign(const aSource: PMeDownloadInfo);
begin
  if Assigned(aSource) then
  begin
    FURI := aSource.FURI;
    FRevision := aSource.FRevision;
    FAcceptRanges := aSource.FAcceptRanges;
    FConnectTimeout := aSource.FConnectTimeout;
    FReadTimeout := aSource.FReadTimeout;
    FContentLength := aSource.FContentLength;
    FExpires := aSource.FExpires;
    FLastModified := aSource.FLastModified;
    FCanResume := aSource.FCanResume;
    FDate := aSource.FDate;
    FHeaderInited := aSource.FHeaderInited;
    FHasContentLength := aSource.FHasContentLength;
  end;
end;

{ TMeDownloadTask }
procedure TMeDownloadTask.Init;
begin
  inherited;
  New(FParts, Create);
  New(FDownInfo, Create);
end;

destructor TMeDownloadTask.Destroy;
begin
  with FParts.LockList^ do
  try
    FreeMeObjects;
  finally
    FParts.UnlockList;
  end;

  MeFreeAndNil(FParts);
  MeFreeAndNil(FDownInfo);
  inherited;
end;

procedure TMeDownloadTask.BeforeRun;
begin
end;

{ TMeCustomDownloadPartTask }
constructor TMeCustomDownloadPartTask.Create(const aDownInfo: PMeDownloadInfo; const aStream: TStream);
Begin
  inherited Create;
  FDownInfo := aDownInfo;
  FStream := aStream;
  Assert(FStream <> nil, 'no stream assigned error!');
  if (FURL = '') and Assigned(FDownInfo) and (FDownInfo.FURI <> '') then
  begin
    FURL := FDownInfo.FURI;
  end;
end;

procedure TMeCustomDownloadPartTask.Init;
begin
  inherited;
  //FStream := TMemoryStream.Create;
end;

destructor TMeCustomDownloadPartTask.Destroy;
begin
  //FreeAndNil(FStream);
  FURL := '';
  inherited;
end;

procedure TMeCustomDownloadPartTask.BeforeRun;
begin
  FStream.Size := 0;
end;

procedure TMeCustomDownloadPartTask.SetStream(const aStream: TStream);
begin
  if not IsRunning and (FStream <> aStream) then
    FStream := aStream;
end;

{ TMeHttpDownloadPartTask }
procedure TMeHttpDownloadPartTask.Init;
begin
  inherited;
  FHttp := TIdHTTP.Create(nil);
  {$ifdef UseOpenSsl}
  FIOSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  FHttp.IOHandler := FIOSSL;
  {$endif}
  //MUST decode manually!!
  //FHttp.Compressor := LCompressor;
  FHttp.CookieManager := LCookieManager;
  FHttp.RedirectMaximum := 5;
  FHttp.HandleRedirects := True;
  FHTTP.OnHeadersAvailable := DoHeadersAvailable;
  FHTTP.Request.UserAgent := cDefaultUserAgent;
  FHTTP.OnStatus := DoStatus;
  FHTTP.OnWorkBegin := DoWorkBegin;
  FHTTP.OnWork := DoWork;
  FHTTP.OnWorkEnd := DoWorkEnd;
end;

destructor TMeHttpDownloadPartTask.Destroy;
begin
  {$ifdef UseOpenSsl}
   FreeAndNil(FIOSSL);
  {$endif}
  FreeAndNil(FHttp);
  inherited;
end;

procedure TMeHttpDownloadPartTask.BeforeRun;
begin
  inherited;
  FHasException := False;
  if Assigned(FDownInfo) and FDownInfo.FCanResume and (FContentRangeEnd > 0) then
  begin
    FHTTP.Request.Range := 'bytes=';
    if FContentRangeStart > 0 then
      FHTTP.Request.Range := FHTTP.Request.Range + IntToStr(FContentRangeStart);
    FHTTP.Request.Range := FHTTP.Request.Range + '-' + IntToStr(FContentRangeEnd);
    FHTTP.ConnectTimeout := FDownInfo.FConnectTimeout;
    FHTTP.ReadTimeout := FDownInfo.FReadTimeout;
  end;
end;

procedure TMeHttpDownloadPartTask.DoHeadersAvailable(Sender: TObject; aHeaders: TIdHeaderList; var vContinue: Boolean);
begin
  if Assigned(FDownInfo) and not FDownInfo.FHeaderInited then 
  with FDownInfo^ do
  begin
    FContentLength := (Sender as TIdCustomHTTP).Response.ContentLength;
    FHasContentLength := FContentLength > 0;
    FDate := (Sender as TIdCustomHTTP).Response.Date;
    FLastModified := (Sender as TIdCustomHTTP).Response.LastModified;
    FHttp.Request.LastModified := FLastModified;
    FExpires := (Sender as TIdCustomHTTP).Response.Expires;

    FAcceptRanges := (Sender as TIdCustomHTTP).Response.AcceptRanges;
    FCanResume := FAcceptRanges <> '';

    FHeaderInited := True;
  end;
  { tell the client this connection what the download range 
   handle content-range headers, like:

   content-range: bytes 1-65536/102400
   content-range: bytes */102400
   content-range: bytes 1-65536/*
  }
  FContentRangeStart := (Sender as TIdCustomHTTP).Response.ContentRangeStart;
  FContentRangeEnd := (Sender as TIdCustomHTTP).Response.ContentRangeEnd;
  FContentRangeInstanceLength := (Sender as TIdCustomHTTP).Response.ContentRangeInstanceLength;
  with TIdHTTPAccess(Sender) do
    DoStatus(hsStatusText, ['Response='+Response.RawHeaders.Text]);
end;

procedure TMeHttpDownloadPartTask.DoStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  //IdStati[AStatus] + ':' + AStatusText 
  if Assigned(FOnStatus) then
    FOnStatus(ASender, AStatus, AStatusText);
end;

procedure TMeHttpDownloadPartTask.DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  if Assigned(FOnWorkBegin) then
    FOnWorkBegin(ASender, AWorkMode, AWorkCountMax);
end;

procedure TMeHttpDownloadPartTask.DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  if Assigned(FOnWorkEnd) then
    FOnWorkEnd(ASender, AWorkMode);
end;

procedure TMeHttpDownloadPartTask.DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if Assigned(FOnWork) then
    FOnWork(ASender, AWorkMode, AWorkCount);
end;

procedure TMeHttpDownloadPartTask.HandleRunException(const Sender: PMeCustomThread; const aException: Exception; var aProcessed: Boolean);
begin
  inherited;
  ///for re-use EIdConnClosedGracefully, EIdReadTimeout, EIdConnectTimeout, EIdReadLnMaxLineLengthExceeded, EIdReadLnWaitMaxAttemptsExceeded, 
  //EIdSocketError
  FHasException := True;
end;

function TMeHttpDownloadPartTask.Run: Boolean;
begin
  FHttp.Get(FURL, FStream);
  Result := False; //stop.
end;

{ TMeHttpDownloadSimpleTask }
constructor TMeHttpDownloadSimpleTask.Create(const aURL: string; const aStream: TStream);
begin
  inherited Create(New(PMeDownloadInfo, Create), aStream);
  FURL := aURL;
  FDownInfo.FURI := aURL;
  FHttp.Compressor := LCompressor;
end;

destructor TMeHttpDownloadSimpleTask.Destroy;
begin
  MeFreeAndNil(FDownInfo);
  inherited;
end;

procedure TMeHttpDownloadSimpleTask.BeforeRun;
begin
  if FStream.Position > 0 then
  begin
    if Assigned(FDownInfo) and not FDownInfo.FHeaderInited then
      FHttp.Head(FURL);
    if Assigned(FDownInfo) and FDownInfo.FHeaderInited and FDownInfo.FCanResume then
    begin
      if FStream.Position < FDownInfo.FContentLength then
      begin
        FContentRangeStart := FStream.Position;
        FContentRangeEnd := FDownInfo.FContentLength;
      end
      else
        FStream.Size := 0;
    end
  end;
  inherited;
end;

function TMeHttpDownloadSimpleTask.Run: Boolean;
var
  //vStream: TStream;
  vBackup: TStream;
begin
  if (FStream.Position > 0) and Assigned(FDownInfo) and FDownInfo.FHeaderInited and FDownInfo.FCanResume then
  begin
    vBackup := FStream;
    FStream := TMemoryStream.Create;
  end
  else
    vBackup := nil;
  Result := inherited Run;
  if Assigned(vBackup) then
  begin
    vBackup.CopyFrom(FStream, FStream.Size);
    FStream.Free;
    FStream := vBackup;
  end;
end;

procedure TMeHttpDownloadSimpleTask.SetURL(const Value: string);
begin
  if (FURL <> Value) and not FIsRunning then
  begin
    FURL := Value;
    FContentRangeEnd := 0;
    if Assigned(FDownInfo) then
      FDownInfo.FHeaderInited := False;
  end;
end;

{ TMeHttpDownloadSimpleThreadMgrTask }
procedure TMeHttpDownloadSimpleThreadMgrTask.Init;
begin
  inherited;
  New(FIdleTasks, Create);
  FProxyParameters := TIdProxyConnectionInfo.Create();
  FFreeTask := False;
end;

destructor TMeHttpDownloadSimpleThreadMgrTask.Destroy;
begin
  with FIdleTasks.LockList^ do
  try
    FreeMeObjects;
  finally
    FIdleTasks.UnLockList;
  end;
  FIdleTasks.Free;
  FProxyParameters.Free;
  FFreeTask := True;
  inherited;
end;

procedure TMeHttpDownloadSimpleThreadMgrTask.DoThreadStopped(const aThread: PMeCustomThread);
var
  vTask: PMeHttpDownloadSimpleTask;
begin
  vTask := PMeHttpDownloadSimpleTask(PMeThread(aThread).Task);
  //if Assigned(FOnTaskDone) then
    //FOnTaskDone(vTask);
  if (MaxThreads <= 0) or (FIdleTasks.Count < MaxThreads) then
    FIdleTasks.Add(vTask)
  else begin
    MeFreeAndNil(vTask);
    PMeThread(aThread).Task := nil;
  end;
  inherited;
end;

procedure TMeHttpDownloadSimpleThreadMgrTask.Download(const aURL: string; const aStream: TStream; const aTag: Integer);
var
  vTask: PMeHttpDownloadSimpleTask;
begin
  with FIdleTasks.LockList^ do
  try
    vTask := Popup;
    if not Assigned(vTask) then
    begin
      New(vTask, Create(aURL, aStream));
    end
    else
      vTask.FStream := aStream;
    vTask.Tag := aTag;
    vTask.FHttp.ProxyParams.Assign(FProxyParameters);
  finally
    FIdleTasks.UnLockList;
  end;
  Add(vTask);
end;

initialization
  {$IFDEF MeRTTI_SUPPORT}
  //Make the ovtVmtClassName point to PShortString class name
  SetMeVirtualMethod(TypeOf(TMeDownloadInfo), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeDownloadTask), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeCustomDownloadPartTask), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeHttpDownloadPartTask), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeHttpDownloadSimpleTask), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeHttpDownloadSimpleThreadMgrTask), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeDownloadInfo), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeDownloadTask), ovtVmtParent, TypeOf(TMeTask));
  SetMeVirtualMethod(TypeOf(TMeCustomDownloadPartTask), ovtVmtParent, TypeOf(TMeTask));
  SetMeVirtualMethod(TypeOf(TMeHttpDownloadPartTask), ovtVmtParent, TypeOf(TMeCustomDownloadPartTask));
  SetMeVirtualMethod(TypeOf(TMeHttpDownloadSimpleTask), ovtVmtParent, TypeOf(TMeHttpDownloadPartTask));
  SetMeVirtualMethod(TypeOf(TMeHttpDownloadSimpleThreadMgrTask), ovtVmtParent, TypeOf(TMeThreadMgrTask));


   LCompressor:= TIdCompressorZLib.Create(nil);
   LCookieManager:= TIdCookieManager.Create(nil);
finalization
   FreeAndNil(LCompressor);
   FreeAndNil(LCookieManager);
end.
