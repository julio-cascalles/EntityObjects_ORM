object frmPrincipal: TfrmPrincipal
  Left = 270
  Top = 150
  BorderStyle = bsDialog
  ClientHeight = 228
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    584
    228)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMedico: TLabel
    Left = 279
    Top = 91
    Width = 38
    Height = 13
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Caption = '&Medico:'
    Color = clBtnFace
    FocusControl = edtMedico
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = lblMedicoClick
  end
  object lblPaciente: TLabel
    Left = 279
    Top = 147
    Width = 45
    Height = 13
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Caption = '&Paciente:'
    Color = clBtnFace
    FocusControl = edtPaciente
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = lblPacienteClick
  end
  object lblHorario: TLabel
    Left = 287
    Top = 27
    Width = 42
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '&Horarios:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object lbl_Disponivel: TLabel
    Left = 335
    Top = 8
    Width = 58
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '(disponiveis)'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label1: TLabel
    Left = 479
    Top = 8
    Width = 52
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '(marcados)'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object edtMedico: TEdit
    Left = 335
    Top = 88
    Width = 241
    Height = 21
    Anchors = [akRight, akBottom]
    ReadOnly = True
    TabOrder = 2
  end
  object edtPaciente: TEdit
    Left = 335
    Top = 147
    Width = 241
    Height = 21
    Anchors = [akRight, akBottom]
    ReadOnly = True
    TabOrder = 3
  end
  object cbxHorario: TComboBox
    Left = 335
    Top = 27
    Width = 97
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    Enabled = False
    TabOrder = 0
  end
  object btGravar: TButton
    Left = 501
    Top = 188
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Gravar'
    TabOrder = 5
    OnClick = btGravarClick
  end
  object btVerificar: TButton
    Left = 8
    Top = 188
    Width = 177
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Verifica Horarios...'
    TabOrder = 4
    OnClick = btVerificarClick
  end
  object cbxMarcados: TComboBox
    Left = 479
    Top = 27
    Width = 97
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    Enabled = False
    Sorted = True
    TabOrder = 1
    OnSelect = exibeConsultaAgendada
  end
end
