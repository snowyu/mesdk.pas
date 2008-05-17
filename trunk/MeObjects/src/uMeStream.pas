
{Summary the stream objects .}
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
    * The Original Code is $RCSfile: uMeStream.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.

    * Contributor(s):
}
unit uMeStream;

interface

{$I MeSetting.inc}

uses
{$IFDEF MSWINDOWS}
  Windows, 
{$ENDIF}
  //TypInfo,
  SysUtils
  , uMeConsts
  , uMeSystem
  , uMeObject
  {$IFDEF DEBUG}
  , DbugIntf
  {$ENDIF}
  ;

//const
{ TMeFileStream create mode }
  {$IFDEF MSWINDOWS}
  const
    fmCreate         = $FFFF;
    fmOpenRead       = $0000;
    fmOpenWrite      = $0001;
    fmOpenReadWrite  = $0002;

    fmShareCompat    = $0000 platform;
    fmShareExclusive = $0010;
    fmShareDenyWrite = $0020;
    fmShareDenyRead  = $0030 platform;
    fmShareDenyNone  = $0040;
  {$ENDIF}

  {$IFDEF LINUX}
  const
    fmOpenRead       = O_RDONLY;
    fmOpenWrite      = O_WRONLY;
    fmOpenReadWrite  = O_RDWR;
    fmShareExclusive = $0010;
    fmShareDenyWrite = $0020;
    fmShareDenyNone  = $0030;
  {$ENDIF}

type
  PMeHandleStream = ^ TMeHandleStream;
  PMeFileStream = ^ TMeFileStream;
  PMeCustomMemoryStream = ^ TMeCustomMemoryStream;
  PMeMemoryStream = ^ TMeMemoryStream;

  TMeHandleStream = Object(TMeStream)
  protected
    FHandle: Integer;
    procedure SetSize(const NewSize: Int64); virtual;{override}
  public
    procedure Open(AHandle: Integer);
    function Read(var Buffer; Count: Longint): Longint; virtual; {override}
    function Write(const Buffer; Count: Longint): Longint; virtual;{override}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual;{override}
    property Handle: Integer read FHandle;
  end;

  TMeFileStream = Object(TMeHandleStream)
  private
    FFileName: string;
  public
    procedure Open(const AFileName: string; Mode: Word); {$IFDEF SUPPORTS_OVERLOAD}overload;{$ENDIF}
    {$IFDEF SUPPORTS_OVERLOAD}
    procedure Open(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    {$ENDIF}
    procedure OpenEx(const AFileName: string; Mode: Word; Rights: Cardinal);
    procedure Close();
    destructor Destroy; virtual; {override}
    property FileName: string read FFileName;
  end;

{ TMeCustomMemoryStream abstract class }

  TMeCustomMemoryStream = Object(TMeStream)
  protected
    FMemory: Pointer;
    FSize, FPosition: Longint;
  protected
    procedure SetPointer(Ptr: Pointer; aSize: Longint);
  public
    function EOF: Boolean;
    function Read(var Buffer; Count: Longint): Longint; virtual; {override}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual;{override}
    procedure SaveToStream(const Stream: PMeStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToString(var aValue: string);

    property Memory: Pointer read FMemory;
  end;

  TMeMemoryStream = Object(TMeCustomMemoryStream)
  private
    FCapacity: Longint;
    procedure SetCapacity(NewCapacity: Longint);
  protected
    function Realloc(var NewCapacity: Longint): Pointer; virtual;
    property Capacity: Longint read FCapacity write SetCapacity;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
    procedure LoadFromStream(const Stream: PMeStream);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromString(const aValue: string);
    procedure SetSize(const NewSize: Int64); virtual; {override}
    function Write(const Buffer; Count: Longint): Longint; virtual; {override}
  end;

implementation

uses 
  RTLConsts, SysConst;

{ TMeHandleStream }

procedure TMeHandleStream.Open(AHandle: Integer);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TMeHandleStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function TMeHandleStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function TMeHandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FileSeek(FHandle, Offset, Ord(Origin));
end;

procedure TMeHandleStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
{$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
  Win32Check(SetEndOfFile(FHandle));
  {$WARN SYMBOL_PLATFORM ON}
{$ELSE}
  if ftruncate(FHandle, Position) = -1 then
    raise EMeError(sStreamSetSize);
{$ENDIF}
end;

{ TMeFileStream }

destructor TMeFileStream.Destroy;
begin
  FFileName := '';
  if FHandle >= 0 then FileClose(FHandle);
  inherited Destroy;
end;

procedure TMeFileStream.Close();
begin
  if FHandle >= 0 then FileClose(FHandle);
  FHandle := -1;
end;

procedure TMeFileStream.Open(const AFileName: string; Mode: Word);
begin
{$IFDEF MSWINDOWS}
  Open(AFilename, Mode, 0);
{$ELSE}
  OpenEx(AFilename, Mode, FileAccessRights);
{$ENDIF}
end;

{$IFDEF SUPPORTS_OVERLOAD}
procedure TMeFileStream.Open(const AFileName: string; Mode: Word; Rights: Cardinal);
begin
  OpenEx(AFileName, Mode, Rights);
end;
{$ENDIF}

procedure TMeFileStream.OpenEx(const AFileName: string; Mode: Word; Rights: Cardinal);
begin
  if Mode = fmCreate then
  begin
    inherited Open(FileCreate(AFileName, Rights));
    if FHandle < 0 then
      raise EMeError.CreateResFmt(@SFCreateErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
    inherited Open(FileOpen(AFileName, Mode));
    if FHandle < 0 then
      raise EMeError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end;
  FFileName := AFileName;
end;

{ TMeCustomMemoryStream }
function TMeCustomMemoryStream.EOF: Boolean;
begin
  Result := FPosition >= FSize;
end;

procedure TMeCustomMemoryStream.SetPointer(Ptr: Pointer; aSize: Longint);
begin
  FMemory := Ptr;
  FSize := aSize;
end;

function TMeCustomMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      Move(Pointer(Longint(FMemory) + FPosition)^, Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TMeCustomMemoryStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TMeCustomMemoryStream.SaveToStream(const Stream: PMeStream);
begin
  if FSize <> 0 then Stream.WriteBuffer(FMemory^, FSize);
end;

procedure TMeCustomMemoryStream.SaveToFile(const FileName: string);
var
  Stream: PMeFileStream;
begin
  New(Stream, Create);
  try
    Stream.Open(FileName, fmCreate);
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMeCustomMemoryStream.SaveToString(var aValue: string);
begin
  SetLength(aValue, FSize);
  if FSize <> 0 then
    Move(FMemory^, aValue[1], FSize);
end;

{ TMeMemoryStream }

const
  MemoryDelta = $2000; { Must be a power of 2 }

destructor TMeMemoryStream.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMeMemoryStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TMeMemoryStream.LoadFromStream(const Stream: PMeStream);
var
  Count: Longint;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if Count <> 0 then Stream.ReadBuffer(FMemory^, Count);
  FPosition := 0;
end;

procedure TMeMemoryStream.LoadFromFile(const FileName: string);
var
  Stream: PMeFileStream;
begin
  New(Stream, Create);
  try
    Stream.Open(FileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMeMemoryStream.LoadFromString(const aValue: string);
begin
  SetSize(Length(aValue));
  if FSize <> 0 then
    Move(aValue[1], FMemory^, FSize);
  FPosition := 0;
end;

procedure TMeMemoryStream.SetCapacity(NewCapacity: Longint);
begin
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
end;

procedure TMeMemoryStream.SetSize(const NewSize: Int64);
var
  OldPosition: Longint;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then Seek(0, soEnd);
end;

function TMeMemoryStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  if (NewCapacity > 0) and (NewCapacity <> FSize) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Memory;
  if NewCapacity <> FCapacity then
  begin
    if NewCapacity = 0 then
    begin
      FreeMem(Memory);
      Result := nil;
    end else
    begin
      if Capacity = 0 then
        GetMem(Result, NewCapacity)
      else
        ReallocMem(Result, NewCapacity);
      if Result = nil then raise EMeError.CreateRes(@SMemoryStreamError);
    end;
  end;
end;

function TMeMemoryStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
      begin
        if Pos > FCapacity then
          SetCapacity(Pos);
        FSize := Pos;
      end;
      System.Move(Buffer, Pointer(Longint(FMemory) + FPosition)^, Count);
      FPosition := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

initialization
  SetMeVirtualMethod(TypeOf(TMeHandleStream), ovtVmtParent, TypeOf(TMeStream));
  SetMeVirtualMethod(TypeOf(TMeFileStream), ovtVmtParent, TypeOf(TMeHandleStream));
  SetMeVirtualMethod(TypeOf(TMeCustomMemoryStream), ovtVmtParent, TypeOf(TMeStream));
  SetMeVirtualMethod(TypeOf(TMeMemoryStream), ovtVmtParent, TypeOf(TMeCustomMemoryStream));

  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeHandleStream), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeFileStream), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeCustomMemoryStream), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeMemoryStream), ovtVmtClassName, nil);
  {$ENDIF}
end.
