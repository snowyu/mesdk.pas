program demo2;

{$I MeSetting.inc}

{$IFNDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
  {$Message Fatal 'This demo need the scrambling code supports'}
{$ENDIF}
uses
  Windows, SysUtils
  , uMeInjector
  ;

var
  OldMessageBoxFunc: function (hWnd: HWND; lpText, lpCaption: PChar;
    uType: UINT): Integer; stdcall = nil;

function NewMessageBoxFunc(hWnd: HWND; lpText, lpCaption: PChar;
  uType: UINT): Integer; stdcall;
var
  S: String;
begin
  S := UpperCase(lpText);
  if Assigned(OldMessageBoxFunc) then
    Result := OldMessageBoxFunc(hWnd, PChar(S), PChar('MeInjector:'+lpCaption), uType)
  else 
    Result := -1;
end;

var
  vMsgBoxInjector: TMeInjector;

begin
  MessageBox(0, 'the origianl message box','Demo2', 0);
  if vMsgBoxInjector.InjectProcedure(@MessageBox, @NewMessageBoxFunc) then
    @OldMessageBoxFunc := vMsgBoxInjector.OriginalProc
  else begin
    MessageBox(0, 'ERROR::CAN NOT inject!','Demo2', 0);
    halt;
  end;

  //the string 'the injected message box' should be UpperCase now.
  MessageBox(0, 'the injected message box','Demo2', 0);

  vMsgBoxInjector.Enabled := False;
end.
