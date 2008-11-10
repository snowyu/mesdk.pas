{: hook FileCreate api to monitor.}
program ApiHook;

{$I MeSetting.inc}

{$APPTYPE Console}
	
uses
  Windows, SysUtils
  ;

procedure StartHook(); stdcall;external 'DllHook';
procedure StopHook; stdcall;external 'DllHook';
  
type
  TShareFileInfo = packed record
    FileName:array[0..255] of char;
    Handle: Longword;
  end;
  PShareFileInfo = ^TShareFileInfo;

const
   MappingFileName = 'MeInMappingFileCreate DLL';

var
  pFileInfo : PShareFileInfo;
  hMappingFile : THandle;
  s: string;

begin
  hMappingFile := OpenFileMapping(FILE_MAP_WRITE,False,MappingFileName);
  if hMappingFile = 0 then Exception.Create('没有共享内存!');
  pFileInfo :=  MapViewOfFile(hMappingFile,FILE_MAP_WRITE or FILE_MAP_READ,0,0,0);
  if pFileInfo = nil then
  begin
    CloseHandle(hMappingFile);
    Exception.Create('不能映射共享内存!');
  end;
  StartHook();
  try
    s := '';
    while true do
    begin
      if pFileInfo.FileName <> s then 
      begin
        writeln(pFileInfo.FileName, '[', pFileInfo.Handle, ']');
        s := pFileInfo.FileName;
      end;
      sleep(1000);
    end;
  finally
    StopHook();
    UnMapViewOfFile(pFileInfo);
    CloseHandle(hMappingFile);
  end;
end.
