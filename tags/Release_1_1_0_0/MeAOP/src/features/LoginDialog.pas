unit LoginDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TdlgLogin = class(TForm)
    lblUserName: TLabel;
    lblPasswd: TLabel;
    edtUser: TEdit;
    edtPasswd: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ShowLoginDialog(var user, pwd: string): TModalResult;

implementation

{$R *.dfm}

function ShowLoginDialog(var user, pwd: string): TModalResult;
begin
  with TdlgLogin.Create(nil) do
  try
    Result := ShowModal;
    if Result = mrOK then
    begin
      user := edtUser.Text;
      pwd  := edtPasswd.Text;
    end;
  finally
    Free;
  end;
end;

end.
