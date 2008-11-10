
{Summary the abstract Uniform/Universal Resource Accessor class and factory..}
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
    * The Original Code is $RCSfile: uMeURL.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):
}
unit uMeURL;

interface

{$I MeSetting.inc}

uses
  SysUtils,
  uMeObject
  , uMeStream
  , uMeThread
  //, uMeURI
  ;

const
  cDefaultTimeout = 30 * 60 * 1000; //30 min

type
  TMeResourceResult = (rrOk, rrUnkownError, rrNoSuchProtocol, rrNoSupports, rrNoSuchResource, rrNoPermission);
  TMeResourceSupport = (rsfGet, rsfPut, rsfDelete, rsfHead);
  TMeResourceSupports = set of TMeResourceSupport;

  TMeResourceRequestDone = procedure (const Sender : TObject;
                              const RqType : TMeResourceSupport;
                              const Error  : TMeResourceResult) of object;

  PMeResourceAccessor = ^ TMeResourceAccessor;
  TMeResourceAccessor = object(TMeDynamicObject)
  protected
    //the current protocol.
    FProtocol: string;
    FURL: string;
    FLocalBaseDir: string;

    procedure SetURL(const aURL: string);

    //返回该 Accessor 支持的操作集合
    class function Supports: TMeResourceSupports; virtual; abstract;
    //list this connection supports protocols, seperate by ";"
    //eg, http;https
    class function Protocols: string; virtual; abstract;

    //分析更新URL，当URL被改变赋值的时候被调用，如果返回为假，表示分析失败，那么URL将不会被改变。
    //update the FURL property after the URL changed, return false means update failed, the URL is not chagned.
    function UpdateURL(Var Value: string): Boolean; virtual;
    //内部取文件资源的头信息，返回为0表示成功，后代必须重载
    function iGetHeader(const aInfo: PMeStrings): TMeResourceResult; virtual; abstract;
    function iGet(var aStream: PMeStream): TMeResourceResult; virtual; abstract;
    function iPut(const aStream: PMeStream): TMeResourceResult; virtual; abstract;
    function iDelete(): TMeResourceResult; virtual; abstract;

    //测试该 Accessor 是否支持 aProtocol 协议。
    function CanProcessed(const aProtocol: string): Boolean;
  public
    destructor Destroy; virtual; {override}

    { Summary: 首先检查该Accessor是否支持该操作，如果支持才去调用 iGetHeader. }
    {
      Return the ResourceInfo in aInfo: AttributeName=value
        Such as:
        Author=XX
        Size=XX
        Date=XX
        Revision=
    }
    function GetResourceHeader(const aInfo: PMeStrings): TMeResourceResult;
    //首先检查该Accessor是否支持该操作，如果支持才去调用 iGet.
    function GetResource(var aStream: PMeStream): TMeResourceResult;
    //首先检查该Accessor是否支持该操作，如果支持才去调用 iPut.
    function PutResource(const aStream: PMeStream): TMeResourceResult;
    //首先检查该Accessor是否支持该操作，如果支持才去调用 iDelete.
    function DeleteResource(): TMeResourceResult;


    //当设置新的URL值的时候，如果协议被改变为是不被当前Accessor支持的，那么改变不会成功。
    property URL: string read FURL write SetURL;
    property Protocol: string read FProtocol;
    property LocalBaseDir: string read FLocalBaseDir write FLocalBaseDir;
  end;

  {TMeResourceAccessorTask = object(TMeTask)
  protected
    procedure AfterRun; virtual; //override
    procedure BeforeRun; virtual; //override
    function Run: Boolean; virtual; //override
    procedure HandleException(const aException: Exception); virtual;//override
  public
  end;//}

  PMeResourceAccessorAsyn = ^ TMeResourceAccessorAsyn;
  TMeResourceAccessorAsyn = object(TMeResourceAccessor)
  protected
    FTimeout: Integer;
  public
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  { Summary: the abstract remote resource. }
  TMeCustomRemoteAccessor = object(TMeResourceAccessor)
  protected
  public
  end;

  { Summary: the abstract local resource(file). }
  {
    the file resource MUST like this: "Protocol://[user:passwd@][/folder/../]filename:/folder/../rsource.txt"
    pack://user:pwd@/test/aFile.pak:/folder/test.txt
    
    FFileName: /test/aFile.pak
    FResourceName:  /folder/test.txt
    Note: 只支持相对目录。
  }
  TMeCustomFileAccessor = object(TMeResourceAccessor)
  protected
    FFileName: string;
    FResourceName: string;
    FUserName: string;
    FPassword: string;

    function UpdateURL(Var Value: string): Boolean; virtual; {override;}
    function GetFileName: string;
  public
    destructor Destroy; virtual; {override}

    //the data(packed) file name.
    property FileName: string read GetFileName;
  end;

{ the registered resource Accessor classes }
function GResourceAccessorClasses: PMeList;


{ Summary: find the index of the protocol class in the GResourceAccessorClasses, if not found return -1}
{ 根据URL中的协议查找Class在列表中的位置，没有找到返回为-1，否则为在列表中的索引号。 }
function IndexOfAccessorClass(const aURL: string): Integer;
//分析aURL类型，查找 Resource Accessor 类工厂，如果发现就创建该 Accessor，否则返回nil.
function CreateResourceAccessor(const aURL: string; const aLocalBaseDir: string = ''): PMeResourceAccessor;

function GetResourceHeader(const aURL: string; const aInfo: PMeStrings; const aLocalBaseDir: string = ''): TMeResourceResult;
function GetResource(const aURL: string; var aStream: PMeStream; const aLocalBaseDir: string = ''): TMeResourceResult;
function PutResource(const aURL: string; const aStream: PMeStream; const aLocalBaseDir: string = ''): TMeResourceResult;
function DeleteResource(const aURL: string; const aLocalBaseDir: string = ''): TMeResourceResult; 

//将Accessor类注册到Accessor类工厂。
procedure Register(const aAccessorClass: TMeClass);

//get the aAccessorClass supports protocols.
function GetAccessorProtocols(const aAccessorClass: TMeClass): string;
//返回该 Accessor 支持的操作集合
function GetAccessorSupports(const aAccessorClass: TMeClass): TMeResourceSupports;
//test whether this Protocol can be processed.
//测试该 Accessor 是否支持 aProtocol 协议。
function ProtocolCanProcessed(const aAccessorClass: TMeClass; const aProtocol: string): Boolean;

implementation

uses
  uMeStrUtils;

type
  //get the method pointer from the VMT directly.
  PMeVMTResourceAccessor = ^ TMeVMTResourceAccessor;
  TMeVMTResourceAccessor = object(TMeVMT)
  public
    Supports: Pointer;
    Protocols: Pointer;
  end;

var
  FResourceAccessorClasses: PMeList;

function GResourceAccessorClasses: PMeList;
begin
  if not Assigned(FResourceAccessorClasses) then
    New(FResourceAccessorClasses, Create);
  Result := FResourceAccessorClasses;
end;


function GetAccessorSupports(const aAccessorClass: TMeClass): TMeResourceSupports;
var
  vFunc: function(Self: Pointer): TMeResourceSupports;
begin
  if MeInheritsFrom(aAccessorClass, TypeOf(TMeResourceAccessor)) then
  if Assigned(aAccessorClass) then
  begin
    @vFunc := PMeVMTResourceAccessor(aAccessorClass).Supports;
    Result := vFunc(aAccessorClass);
  end
  else
    Result := [];
end;

function GetAccessorProtocols(const aAccessorClass: TMeClass): string;
var
  vFunc: function(Self: Pointer): string;
begin
  if MeInheritsFrom(aAccessorClass, TypeOf(TMeResourceAccessor)) then
  begin
    @vFunc := PMeVMTResourceAccessor(aAccessorClass).Protocols;
    Result := vFunc(aAccessorClass);
  end
  else
    Result := '';
end;

function ProtocolCanProcessed(const aAccessorClass: TMeClass; const aProtocol: string): Boolean;
var
  s: string;
begin
  s := GetAccessorProtocols(aAccessorClass);
  Result := False;
  while (s <> '') and not Result do
  begin
    Result := AnsiCompareText(StrFetch(s, ';', True), aProtocol) = 0;
  end;
end;

procedure Register(const aAccessorClass: TMeClass);
begin
  with GResourceAccessorClasses^ do
    if IndexOf(aAccessorClass) < 0 then
      Add(aAccessorClass);
end;

function IndexOfAccessorClass(const aURL: string): Integer;
var
  vProtocol: string;
begin
  Result := AnsiPos(':', aURL);
  if Result > 0 then
  begin
    vProtocol := Copy(aURL, 1, Result - 1);
    with GResourceAccessorClasses^ do
      for Result := 0 to Count -1 do
        if AnsiCompareText(vProtocol, GetAccessorProtocols(Items[Result])) = 0 then
          exit;
  end;
  Result := -1;
end;

function CreateResourceAccessor(const aURL: string; const aLocalBaseDir: string = ''): PMeResourceAccessor;
var
  i: Integer;
begin
  i := IndexOfAccessorClass(aURL);
  if i >= 0 then
  begin
    Result := PMeResourceAccessor(NewMeObject(GResourceAccessorClasses.Items[i]));
    Result.URL := aURL;
    Result.LocalBaseDir := aLocalBaseDir;
  end
  else
    Result := nil;
end;

function GetResourceHeader(const aURL: string; const aInfo: PMeStrings; const aLocalBaseDir: string = ''): TMeResourceResult;
var
  vAccessor: PMeResourceAccessor;
begin
  vAccessor := CreateResourceAccessor(aURL, aLocalBaseDir);
  if Assigned(vAccessor) then
  try
    Result := vAccessor.GetResourceHeader(aInfo);
  finally
    vAccessor.Free;
  end
  else
    Result := rrNoSuchProtocol;  
end;

function GetResource(const aURL: string; var aStream: PMeStream; const aLocalBaseDir: string = ''): TMeResourceResult;
var
  vAccessor: PMeResourceAccessor;
begin
  vAccessor := CreateResourceAccessor(aURL, aLocalBaseDir);
  if Assigned(vAccessor) then
  try
    Result := vAccessor.GetResource(aStream);
  finally
    vAccessor.Free;
  end
  else
    Result := rrNoSuchProtocol;  
end;

function PutResource(const aURL: string; const aStream: PMeStream; const aLocalBaseDir: string = ''): TMeResourceResult;
var
  vAccessor: PMeResourceAccessor;
begin
  vAccessor := CreateResourceAccessor(aURL, aLocalBaseDir);
  if Assigned(vAccessor) then
  try
    Result := vAccessor.PutResource(aStream);
  finally
    vAccessor.Free;
  end
  else
    Result := rrNoSuchProtocol;  
end;

function DeleteResource(const aURL: string; const aLocalBaseDir: string = ''): TMeResourceResult;
var
  vAccessor: PMeResourceAccessor;
begin
  vAccessor := CreateResourceAccessor(aURL, aLocalBaseDir);
  if Assigned(vAccessor) then
  try
    Result := vAccessor.DeleteResource();
  finally
    vAccessor.Free;
  end
  else
    Result := rrNoSuchProtocol;  
end;

{ TMeResourceAccessor }
destructor TMeResourceAccessor.Destroy;
begin
  FURL := '';
  FLocalBaseDir := '';
  FProtocol := '';
  inherited;
end;

function TMeResourceAccessor.CanProcessed(const aProtocol: string): Boolean;
var
  s: string;
begin
  s := Protocols;
  Result := False;
  while (s <> '') and not Result do
  begin
    Result := AnsiCompareText(StrFetch(s, ';', True), aProtocol) = 0;
  end;
end;

procedure TMeResourceAccessor.SetURL(const aURL: string);
var
  s: string;
begin
  if FURL <> aURL then
  begin
    s := aURL;
    if UpdateURL(s) then
      FURL := aURL;
  end;
end;

function TMeResourceAccessor.GetResourceHeader(const aInfo: PMeStrings): TMeResourceResult;
begin
  if rsfHead in Supports then
    Result := iGetHeader(aInfo)
  else
    Result := rrNoSupports;
end;

function TMeResourceAccessor.GetResource(var aStream: PMeStream): TMeResourceResult;
begin
  if rsfGet in Supports then
    Result := iGet(aStream)
  else
    Result := rrNoSupports;
end;

function TMeResourceAccessor.PutResource(const aStream: PMeStream): TMeResourceResult;
begin
  if rsfPut in Supports then
    Result := iPut(aStream)
  else
    Result := rrNoSupports;
end;

function TMeResourceAccessor.DeleteResource(): TMeResourceResult;
begin
  if rsfDelete in Supports then
    Result := iDelete()
  else
    Result := rrNoSupports;
end;

function TMeResourceAccessor.UpdateURL(Var Value: string): Boolean;
begin
  FProtocol := StrFetch(Value, ':', True);
  Result := (FProtocol <> '') and CanProcessed(FProtocol);
  //if not Result then FProtocol := '';
end;

{ TMeCustomFileAccessor }
destructor TMeCustomFileAccessor.Destroy;
begin
  FFileName:= '';
  FResourceName:='';
  FUserName:='';
  FPassword:='';
  inherited;
end;

function TMeCustomFileAccessor.GetFileName: string;
begin
  if FLocalBaseDir <> '' then
    Result := IncludeTrailingPathDelimiter(FLocalBaseDir)
  else
    Result := ExtractFilePath(ParamStr(0));
  Result := Result + FFileName;
end;

function TMeCustomFileAccessor.UpdateURL(Var Value: string): Boolean;
Var
  s: string;
begin
  Result := inherited UpdateURL(Value);
  if Result then
  begin
    s := StrRFetch(Value, ':', True);
    Result := s <> '';
    if Result then
    begin
      FFileName := s;
      FResourceName := Value;
      if AnsiPos( '@', s) > 0 then
      begin
        s := StrFetch(FFileName, '@', True);
        FUserName := s;
        if AnsiPos( ':', s) > 0 then
          FPassword := StrFetch(FUserName, ':', True)
        else
          FPassword := '';
      end;
    end
    else
      Result := False;
  end;
end;

initialization
  {$IFDEF MeRTTI_SUPPORT}
  //Make the ovtVmtClassName point to PShortString class name
  SetMeVirtualMethod(TypeOf(TMeResourceAccessor), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeCustomFileAccessor), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeResourceAccessor), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeCustomFileAccessor), ovtVmtParent, TypeOf(TMeResourceAccessor));
finalization
  MeFreeAndNil(FResourceAccessorClasses);
end.
