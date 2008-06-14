object dlgLogin: TdlgLogin
  Left = 211
  Top = 225
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 124
  ClientWidth = 207
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblUserName: TLabel
    Left = 8
    Top = 16
    Width = 53
    Height = 13
    Caption = 'User Name'
  end
  object lblPasswd: TLabel
    Left = 9
    Top = 48
    Width = 37
    Height = 13
    Caption = 'Passwd'
  end
  object edtUser: TEdit
    Left = 80
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'admin'
  end
  object edtPasswd: TEdit
    Left = 80
    Top = 40
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 112
    Top = 80
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
end
