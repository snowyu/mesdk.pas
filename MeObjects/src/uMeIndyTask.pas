
{Summary the Indy Net Task class ..}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.10 $

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
  SysUtils,
  uMeObject
  , uMeStream
  , uMeThread
  , uMeURI

  //, IdGlobal
  //, IdGlobalProtocols //GMTXXX
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

type
  PMeDownloadTask = ^ TMeDownloadTask;
  PMeDownloadPartTask = ^ TMeDownloadPartTask;

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
}
  TMeDownloadTask = object(TMeTask)
  protected
    // the header properties is inited or not.
    FHeaderInited: Boolean;
    FURL: PMeURI;
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
    FTimeout: Integer;
    //collect the TMeDownloadPartTask.
    FParts: PMeThreadSafeList;

    procedure Init; virtual; //override
  public
    destructor Destroy; virtual; //override
  end;

  TMeDownloadPartTask = object(TMeTask)
  protected
    //owner
    FOwner: PMeDownloadTask;
    FContentRangeEnd: Int64;
    FContentRangeStart: Int64;
    FContentRangeInstanceLength: Int64;

    procedure DoHeadersAvailable(Sender: TObject; aHeaders: TIdHeaderList; var vContinue: Boolean);

    //procedure AfterRun; virtual; //override
    //procedure BeforeRun; virtual; //override
    //function Run: Boolean; virtual; //override
    procedure HandleException(const aException: Exception); virtual;//override
  public
    constructor Create(const Owner: PMeDownloadTask);
  end;

  TMeHttpDownloadPartTask = object(TMeDownloadPartTask)
  protected
    FHttp: TIdHTTP;
   {$ifdef UseOpenSsl}
    FIOSSL : TIdSSLIOHandlerSocketOpenSSL;
   {$endif}
    function Run: Boolean; virtual; //override
    procedure Init; virtual; //override
  public
    destructor Destroy; virtual; //override
  end;


implementation

uses
  uMeStrUtils;

var
  LCompressor: TIdCompressorZLib;
  LCookieManager: TIdCookieManager;

{ TMeDownloadTask }
procedure TMeDownloadTask.Init;
begin
  inherited;
  FContentLength := -1;
end;

destructor TMeDownloadTask.Destroy;
begin
  FRevision := '';
  inherited;
end;

{ TMeDownloadPartTask }
constructor TMeDownloadPartTask.Create(const Owner: PMeDownloadTask);
Begin
  inherited Create;
  FOwner := Owner;
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
  FHttp.Compressor := LCompressor;
  FHttp.CookieManager := LCookieManager;
  FHttp.RedirectMaximum := 5;
  FHttp.HandleRedirects := True;
  FHTTP.Request.UserAgent := cDefaultUserAgent;
  FHTTP.OnHeadersAvailable := DoHeadersAvailable;
end;

destructor TMeHttpDownloadPartTask.Destroy;
begin
  {$ifdef UseOpenSsl}
   FreeAndNil(FIOSSL);
  {$endif}
  FreeAndNil(FHttp);
end;

procedure TMeHttpDownloadPartTask.DoHeadersAvailable(Sender: TObject; aHeaders: TIdHeaderList; var vContinue: Boolean);
begin
  if Assigned(FOwner) and not FOwner.FHeaderInited then 
  with FOwner^ do
  begin
    FContentLength := (Sender as TIdCustomHTTP).Response.ContentLength;
    FHasContentLength := FContentLength > 0;
    FDate := (Sender as TIdCustomHTTP).Response.Date;
    FLastModified := (Sender as TIdCustomHTTP).Response.LastModified;
    FHttp.Request.LastModified := LastModified;
    FExpires := (Sender as TIdCustomHTTP).Response.Expires;

    FAcceptRanges := (Sender as TIdCustomHTTP).Response.AcceptRanges;
    FCanResume := FAcceptRanges <> '';

    { tell the client this connection what the download range 
     handle content-range headers, like:

     content-range: bytes 1-65536/102400
     content-range: bytes */102400
     content-range: bytes 1-65536/*
    }
    FContentRangeStart := (Sender as TIdCustomHTTP).Response.ContentRangeStart;
    FContentRangeEnd := (Sender as TIdCustomHTTP).Response.ContentRangeEnd;
    FContentRangeInstanceLength := (Sender as TIdCustomHTTP).Response.ContentRangeInstanceLength;

    FHeaderInited := True;
  end;
end;

function TMeHttpDownloadPartTask.Run: Boolean;
begin
  
end;

initialization
  {$IFDEF MeRTTI_SUPPORT}
  //Make the ovtVmtClassName point to PShortString class name
  SetMeVirtualMethod(TypeOf(TMeDownloadTask), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeDownloadPartTask), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeHttpDownloadPartTask), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeDownloadTask), ovtVmtParent, TypeOf(TMeTask));
  SetMeVirtualMethod(TypeOf(TMeDownloadPartTask), ovtVmtParent, TypeOf(TMeTask));
  SetMeVirtualMethod(TypeOf(TMeHttpDownloadPartTask), ovtVmtParent, TypeOf(TMeDownloadPartTask));


   LCompressor:= TIdCompressorZLib.Create(nil);
   LCookieManager:= TIdCookieManager.Create(nil);
finalization
   FreeAndNil(LCompressor);
   FreeAndNil(LCookieManager);
end.
