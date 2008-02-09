program simple;

{$I MeSetting.inc}

uses
    {$IFNDEF COMPILER10_UP}
      {$IFNDEF CLR}
      FastMM4,
      {$ENDIF}
    {$ENDIF}
  Forms,
  LoginDialog {dlgLogin},
  Demo1Form in 'Demo1Form.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
