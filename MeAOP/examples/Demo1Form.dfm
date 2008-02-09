object Form1: TForm1
  Left = 260
  Top = 201
  Width = 408
  Height = 247
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnFunction: TBitBtn
    Left = 8
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Run Method'
    TabOrder = 0
    OnClick = btnFunctionClick
  end
  object btnRunProc: TButton
    Left = 88
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Run Proc'
    TabOrder = 1
    OnClick = btnRunProcClick
  end
  object btnLogin: TButton
    Left = 32
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Login'
    TabOrder = 2
    OnClick = btnLoginClick
  end
  object btnLogout: TButton
    Left = 128
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Logout'
    TabOrder = 3
    OnClick = btnLogoutClick
  end
  object btnRemove: TButton
    Left = 216
    Top = 8
    Width = 168
    Height = 25
    Caption = 'RemoveLogFeatureFromDLLProc'
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object btnDLLProc: TButton
    Left = 176
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Run DLLProc'
    TabOrder = 4
    OnClick = btnDLLProcClick
  end
  object mmoLog: TMemo
    Left = 0
    Top = 75
    Width = 360
    Height = 138
    Align = alBottom
    Lines.Strings = (
      'mmoLog')
    TabOrder = 5
  end
end
