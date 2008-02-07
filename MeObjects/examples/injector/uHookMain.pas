Unit uHookMain;

interface

{$I MeSetting.inc}

{$IFNDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
  {$Message Fatal 'This demo need the scrambling code supports'}
{$ENDIF}

uses
  Windows, SysUtils, Messages
  , uMeInjector
  ;


procedure StartHook(); stdcall;
procedure StopHook; stdcall;

implementation

type
  TShareFileInfo = packed record
    FileName:array[0..255] of char;
    Handle: Longword;
  end;
  PShareFileInfo = ^TShareFileInfo;

const
   MappingFileName = 'MeInMappingFileCreate DLL';

var
  FirstEnter:boolean;
  pFileInfo: PShareFileInfo;
  hMappingFile : THandle;
  OldMessageHook:Thandle;
  OldCreateFileAFunc: function (lpFileName: PChar;dwDesiredAccess, dwShareMode: DWORD;
   lpSecurityAttributes: PSecurityAttributes;dwCreationDisposition,dwFlagsAndAttributes: DWORD;
   hTemplateFile: THandle): THandle; stdcall  = nil;

function GetMsgProc(code: integer; wPar: integer; lPar: integer): Integer; stdcall;
begin
  Result := CallNextHookEx(OldMessageHook, Code, wPar, lPar);
end;

procedure StartHook(); stdcall;
begin
   if FirstEnter and (OldMessageHook = 0) then
   begin
      OldMessageHook := SetWindowsHookEx(WH_GetMessage, GetMsgProc, HInstance, 0);
   end;
end;

procedure StopHook(); stdcall;
begin
   if OldMessageHook <> 0 then
   begin
     UnhookWindowsHookEx(OldMessageHook);
     OldMessageHook := 0;
     //SendMessage(HWND_BROADCAST,WM_SETTINGCHANGE,0,0);
   end;
end;

function NewCreateFileA(lpFileName: PChar;dwDesiredAccess, dwShareMode: DWORD;
   lpSecurityAttributes: PSecurityAttributes;dwCreationDisposition,dwFlagsAndAttributes: DWORD;
   hTemplateFile: THandle): THandle;stdcall;
begin
  try
    if Assigned(OldCreateFileAFunc) then
      result:=OldCreateFileAFunc(lpFileName,dwDesiredAccess,dwShareMode,
        lpSecurityAttributes,dwCreationDisposition,dwFlagsAndAttributes,
        hTemplateFile)
    else
      Result := 0;
  except
    Result := 0;
  end;
  strlcopy(pFileInfo^.FileName,lpFileName,255);
  pFileInfo^.Handle := Result;
  //pFileInfo^.FileName[0] := PChar(@OldCreateFileAFunc)^;

end;

var
  vCreateFileAInjector: TMeInjector;

initialization
  hMappingFile := OpenFileMapping(FILE_MAP_WRITE,False,MappingFileName);
  if hMappingFile=0 then
  begin
    hMappingFile := CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,SizeOf(TShareFileInfo),MappingFileName);
    FirstEnter   := true;
  end
  else 
    FirstEnter:= False;
  if hMappingFile = 0 then Exception.Create('Can not CreateFileMapping!');
  pFileInfo :=  MapViewOfFile(hMappingFile,FILE_MAP_WRITE or FILE_MAP_READ,0,0,0);
  if pFileInfo = nil then
  begin
    CloseHandle(hMappingFile);
    Exception.Create('Can not Create Share Memory!');
  end;

  if vCreateFileAInjector.InjectProcedure(@CreateFileA, @NewCreateFileA) then
  begin
    @OldCreateFileAFunc := vCreateFileAInjector.OriginalProc;
    OldMessageHook := 0;
  end
  else begin
    Exception.Create('ERROR::CAN NOT inject!');
  end;

finalization
  vCreateFileAInjector.Enabled := False; //uninject
  UnMapViewOfFile(pFileInfo);
  CloseHandle(hMappingFile);
end.
