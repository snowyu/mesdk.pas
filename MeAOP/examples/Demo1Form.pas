unit Demo1Form;

interface

{$I MeSetting.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons
  , uMeSystem
  , uMeTypInfo
  , uMeInterceptor
  , uMeFeature
  , uLogFeature
  , CustomLogBase
  , LogStrings
  , uLoginFeature
  , uLoginManager
  ;

type
  TForm1 = class(TForm)
    btnFunction: TBitBtn;
    btnRunProc: TButton;
    btnLogin: TButton;
    btnLogout: TButton;
    btnRemove: TButton;
    btnDLLProc: TButton;
    mmoLog: TMemo;
    procedure btnFunctionClick(Sender: TObject);
    procedure btnRunProcClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDLLProcClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FLogStrs: TLogStringList;
    procedure DoLoginStatus(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

var
  GLogFeature: TMeLogFeature;
//function DoSth: real;

procedure ShowIt; external 'dlltest.dll';

implementation

{$R *.dfm}
const
  cTheResult = $1234567810000;

type
  TProc = procedure of object;
  TDoSthProc = function(a,b: integer; var c:integer; d,e,f: Integer; var g: string): Int64  of object;
  TTestProc = function(a,b: integer): integer of object;

function _test(a,b:integer):integer;
asm
  add eax, edx
end;

function test(a,b:integer):integer;
asm
  CALL _test
  NOP
  NOP
end;

function DoSth(a,b: integer; var c:integer; d,e,f: Integer; var g: string): Int64 ;
begin
{$I uMeMakeHole.inc} //TODO: make hole BUG
  Result := a+b+c+d+e+f+StrToIntDef(g,0);
  //Result := 'Hello from DoSth!!';
  ShowMessage('the procedure is running now');
end;

procedure TForm1.DoLoginStatus(Sender: TObject);
begin
  if Sender is TLoginManager then
    if TLoginManager(Sender).IsLogined then
      Caption := 'Logined'
    else
      Caption := 'not Logined';
end;

procedure TForm1.btnFunctionClick(Sender: TObject);
begin
  ShowMessage('OK! Now Function Run');
end;

procedure TForm1.btnRunProcClick(Sender: TObject);
var
  a,b,c,d,e,f:Integer;
  i: Int64;
  s: string;
begin
  {s:= DoSth;
  if s <> '' then
    ShowMessage(s);//}
  a := $11;
  b := $22;
  c := $33;
  d := $2;
  e := 1;
  f := 3;
  s := '1';
  mmoLog.Lines.Add('TForm1.btnRunProcClick.OrgVar='+ IntToHex(c,8));
  i := DoSth(a,b,c, d, e,f, s);
  mmoLog.Lines.Add('TForm1.btnRunProcClick.OrgResult='+ IntToHex(a+b+c+d+e+f+StrToIntDef(s,0),8));
  mmoLog.Lines.Add('TForm1.btnRunProcClick.NewResult='+ IntToHex(i,8));
  mmoLog.Lines.Add('TForm1.btnRunProcClick.NewVarC='+ IntToHex(c,8));
  
  mmoLog.Lines.Add('TForm1.btnRunProcClick.NewResult should be changed to $3344556600');
  mmoLog.Lines.Add('TForm1.btnRunProcClick.NewVarC should is Inc(Old c)');
  //i := test($11, $22);
  //if i = cTheResult then
    ShowMessage('the result is '#13#10 +'M='+IntToHex(i,8) + #10'the c=$'+ inttoHex(c,8))
  //else
    //ShowMessage('ERROR: no function RUN!!');
  //}
end;

procedure TForm1.btnLoginClick(Sender: TObject);
begin
  GLoginManager.Login;
end;

procedure TForm1.btnLogoutClick(Sender: TObject);
begin
  GLoginManager.Logout;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLogStrs := TLogStringList.Create(nil);
  FLogStrs.Strings := mmoLog.Lines;
  FLogStrs.Level := vlDebug;
  FLogStrs.Active := True;

  GLogFeature := TMeLogFeature(TMeLogFeature.AddTo(TForm1, @TForm1.btnFunctionClick, 'btnFunctionClick'));
  GLogFeature.LogBase := FLogStrs;
  TMeLogFeature.AddTo(@DoSth, 'DoSth', TypeInfo(TDoSthProc));
  TMeLogFeature.AddTo(@ShowIt, 'ShowIt', TypeInfo(TProc));
  TMeLogFeature.AddTo(@Test, 'Test', TypeInfo(TtestProc));
  btnRemove.Tag := 0;

  GLoginManager.OnStatus := DoLoginStatus;
  ShowHint := True;
end;

procedure TForm1.btnRemoveClick(Sender: TObject);
begin
  if btnRemove.Tag = 0 then
  begin
    TMeLogFeature.RemoveFrom(@ShowIt);
    btnRemove.Tag := 1;
    btnRemove.Caption := 'AddLogFeatureToDllProc';
    btnRemove.Hint := 'add log feature to DLLProc';
  end
  else
  begin
    TMeLogFeature.AddTo(@ShowIt, 'ShowIt', TypeInfo(TProc));
    btnRemove.Tag := 0;
    btnRemove.Caption := 'RemoveLogFeatureFromDllProc';
    btnRemove.Hint := 'remove log feature from DLLProc';
  end;
end;

procedure TForm1.btnDLLProcClick(Sender: TObject);
begin
  ShowIt;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLogStrs);
end;

initialization
  TMeLoginFeature.AddTo(TForm1, @TForm1.btnFunctionClick, 'btnFunctionClick', TypeInfo(TNotifyEvent));
  TMeLoginFeature.AddTo(@DoSth, 'DoSth', TypeInfo(TDoSthProc));
  TMeLoginFeature.AddTo(@ShowIt, 'ShowIt', TypeInfo(TProc));
  //TMeLoginFeature.AddTo(@MessageBox, 'MessageBox');// do not support this function with parameters.
finalization
  TMeLoginFeature.RemoveFrom(@ShowIt);
end.
