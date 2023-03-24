unit Principal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons,
  {$ifdef FPC}
    Calendar,
  {$else}
    DB,
  {$endif}
  Dados, EntityObjects
  ;


type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    lblMedico: TLabel;
    lblPaciente: TLabel;
    edtMedico: TEdit;
    edtPaciente: TEdit;
    cbxHorario: TComboBox;
    lblHorario: TLabel;
    btGravar: TButton;
    btVerificar: TButton;
    lbl_Disponivel: TLabel;
    cbxMarcados: TComboBox;
    Label1: TLabel;
    procedure lblMedicoClick(Sender: TObject);
    procedure lblPacienteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btGravarClick(Sender: TObject);
    procedure btVerificarClick(Sender: TObject);
    procedure calDataClick(Sender: TObject);
    procedure exibeConsultaAgendada(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    agenda: TAgenda;
    procedure criaCalendario();
    function ajustaHorario(Sender: TEntityField): Boolean;
    procedure preencheTodosHorarios;
    function podeGravar: Boolean;
  public
    {$ifdef FPC}
    calData: TCalendar;
    {$else}
    calData: TMonthCalendar;
    {$endif}
    function DataSelecionada: TDate;
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses CadastroMedicos, CadastroPacientes;

{$R *.dfm}

procedure TfrmPrincipal.criaCalendario();
begin
  {$ifdef FPC}
    calData := TCalendar.Create(self);
    calData.onClick  := @calDataClick;
    calData.DateTime := Now;
  {$else}
    calData := TMonthCalendar.Create(self);
    calData.onClick  := calDataClick;
    calData.Date     := Now;
  {$endif}
  calData.Parent := self;
  calData.Left     :=  16;
  calData.Top      :=  16;
  calData.Height   := 161;
  calData.Width    := 176;
  calData.TabOrder := 0;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  agenda := TAgenda.Create;
end;

procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  agenda.Free;
end;

procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  Caption := 'Exemplo de Agenda medica - usando EntityObjects';
  criaCalendario();
  Top  := 150;
  Left := 200;
end;

procedure TfrmPrincipal.lblMedicoClick(Sender: TObject);
var
  medico: TMedico;
  Form: TfrmCadastroMedicos;
begin
  medico := TMedico(agenda.Medico.ForeignEntity);
  Form := TfrmCadastroMedicos.Create(self);
  Form.AgendaOriginal := self.agenda;
  if Form.ShowModal = mrOK then
  begin
    medico.Assign(Form.Pessoa);
    edtMedico.Text := medico.nome.Value;
  end;
  Form.Free;
end;

procedure TfrmPrincipal.lblPacienteClick(Sender: TObject);
var
  paciente: TPaciente;
  Form: TfrmCadastroPacientes;
begin
  paciente := TPaciente(agenda.Paciente.ForeignEntity);
  Form := TfrmCadastroPacientes.Create(self);
  Form.AgendaOriginal := self.agenda;
  if Form.ShowModal = mrOK then
  begin
    paciente.Assign(Form.Pessoa);
    edtPaciente.Text := paciente.nome.Value;
  end;
  Form.Free;
end;

procedure TfrmPrincipal.preencheTodosHorarios;
begin
  cbxHorario.Items.CommaText := '08:00,08:30,09:00,09:30,10:00,10:30,11:00,11:30'
  +',13:00,13:30,14:00,14:30,15:00,15:30,16:00,16:30,17:00,17:30,18:00,18:30';
  cbxMarcados.Items.Clear;
end;

function TfrmPrincipal.ajustaHorario(Sender: TEntityField): Boolean;
var
  idx_hora: integer;
begin
  Result := true; // se result False, interrompe o evento
  idx_hora := cbxHorario.Items.IndexOf(Sender.AsString);
  {---- Se o horário já existe, remove do ComboBox ---}
  if idx_hora > -1 then
  begin
    cbxHorario.Items.Delete(idx_hora);
  end;
  {---------------------------------------------------}
  cbxMarcados.items.Add(Sender.AsString);
end;

function TfrmPrincipal.podeGravar: Boolean;
begin
  Result := false;
  if cbxHorario.ItemIndex = -1 then
  begin
    MessageBox(0, 'Selecione um Horário!', 'Aviso', MB_ICONWARNING or MB_OK);
    Exit;
  end;
  if edtMedico.Text = '' then
  begin
    MessageBox(0, 'Selecione um Médico!', 'Aviso', MB_ICONWARNING or MB_OK);
    Exit;
  end;
  if edtPaciente.Text = '' then
  begin
    MessageBox(0, 'Selecione um Paciente!', 'Aviso', MB_ICONWARNING or MB_OK);
    Exit;
  end;
  Result := true;
end;

procedure TfrmPrincipal.btGravarClick(Sender: TObject);
begin
  if not podeGravar then Exit;
  {---- Grava o registro na Agenda médica: ----}
  agenda.NewRecord;
  with agenda do
  begin
     dia.Value      := DataSelecionada;
     hora.Value     := cbxHorario.Text;
     if not SaveChanges then Exit;
  end;
  ajustaHorario(agenda['hora']);
  cbxHorario.Text := '';
  {--------------------------------------------}
  MessageBox(0, 'Registro gravado com sucesso!', 'Sucesso', MB_ICONINFORMATION or MB_OK);
end;

procedure TfrmPrincipal.btVerificarClick(Sender: TObject);
var
  Combo: TComboBox;
begin
  preencheTodosHorarios();
  {
    agenda['hora'] é o mesmo que agenda.hora...
  }
  if agenda.Find(DataSelecionada,[agenda.hora]) then
    begin
      {$ifdef FPC}
        agenda.List(agenda['hora'],@ajustaHorario);
      {$else}
        agenda.List(agenda['hora'],ajustaHorario);
      {$endif}
      cbxMarcados.Enabled := true;
      cbxMarcados.SetFocus;
      cbxMarcados.DroppedDown := true;
    end
  Else
    begin
      MessageDlg('Ainda não foram marcados horários para este dia.', mtWarning, [mbOK], 0);
    end;
  cbxHorario.Enabled := true;
end;

procedure TfrmPrincipal.calDataClick(Sender: TObject);
begin
  cbxHorario.Enabled := false;
  cbxMarcados.Enabled := false;
end;

procedure TfrmPrincipal.exibeConsultaAgendada(Sender: TObject);
var
  nome_med, nome_pac: TEntityField;
begin
  nome_med := TMedico(agenda.Medico.ForeignEntity).nome;
  nome_pac := TPaciente(agenda.Paciente.ForeignEntity).nome;
  {***--- Sintaxe de consulta parecida com LINQ -----}
  with agenda do
    Where([
      dia.Equals(DataSelecionada),
      hora.Equals(cbxMarcados.Text)
    ]).Select([
      medico,
      paciente,
      nome_med,
      nome_pac
    ]);
  {-----------------------------------------------***}
  edtMedico.Text   := nome_med.AsString;
  edtPaciente.Text := nome_pac.AsString;
end;

function TfrmPrincipal.DataSelecionada: TDate;
begin
  {$ifdef FPC}
  Result := calData.DateTime;
  {$else}
  Result := calData.Date;
  {$endif}
end;

end.
