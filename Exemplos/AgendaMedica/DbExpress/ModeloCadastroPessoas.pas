unit ModeloCadastroPessoas;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls,
  EntityObjects, Dados,
  ComCtrls, Buttons, DBGrids,
  {$ifdef FPC}
    db, DBClient
  {$else}
    DB
  {$endif}
  ,Midas, DBClient
  ;

type

  { TfrmModeloCadastroPessoas }

  TfrmModeloCadastroPessoas = class(TForm)
    pan_botoes: TPanel;
    btnNovo: TBitBtn;
    btnEdita: TBitBtn;
    btnApaga: TBitBtn;
    btnGrava: TBitBtn;
    btnCancela: TBitBtn;
    areaEdicao: TPanel;
    edtId: TEdit;
    lblId: TLabel;
    edtNome: TEdit;
    lblNome: TLabel;
    pan_Pesquisa: TPanel;
    btPesquisa: TSpeedButton;
    lbl_Pesquisa: TLabel;
    edtPesquisa: TEdit;
    Grid: TDBGrid;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    procedure btnCancelaClick(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
    procedure btnGravaClick(Sender: TObject);
    procedure btnApagaClick(Sender: TObject);
    procedure btPesquisaClick(Sender: TObject);
    procedure retornaSelecao(Sender: TObject);
    procedure edtPesquisaChange(Sender: TObject);
    procedure edtPesquisaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
  private
    f_Editando, inclusao: Boolean;
    fPessoa: TPessoa;
    fCopia: TAgenda;
    fOriginal: TAgenda;
    procedure SetEditando(const Value: Boolean);
    procedure CarregaCampos(com_dados: Boolean);
    procedure setPessoa(const Value: TPessoa);
  protected
    procedure setOriginal(const Value: TAgenda); virtual;
    property Editando: Boolean read f_Editando write SetEditando;
  public
    property Pessoa: TPessoa read fPessoa write setPessoa;
    property AgendaOriginal: TAgenda read fOriginal write setOriginal;
    property AgendaCopia: TAgenda read fCopia;
  end;


implementation

{$I EntityObjects.inc}

{$R *.dfm}

procedure TfrmModeloCadastroPessoas.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then retornaSelecao(nil);
end;

procedure TfrmModeloCadastroPessoas.SetEditando(const Value: Boolean);
begin
  f_Editando := Value;
  btnNovo.Enabled     := not f_Editando;
  btnEdita.Enabled    := not f_Editando AND (not Pessoa.IsEmpty);
  btnGrava.Enabled    := f_Editando;
  btnCancela.Enabled  := f_Editando;
  btnApaga.Enabled    := not f_Editando;
  edtNome.Enabled     := f_Editando;
end;

procedure TfrmModeloCadastroPessoas.setOriginal(const Value: TAgenda);
begin
  fOriginal := Value;
  fCopia := TAgenda(fOriginal.Clone);
end;

procedure TfrmModeloCadastroPessoas.setPessoa(const Value: TPessoa);
begin
  fPessoa := Value;
  fPessoa.setDataSource(DataSource1);
  SetEditando(false);
end;

procedure TfrmModeloCadastroPessoas.btnCancelaClick(Sender: TObject);
begin
  Pessoa.Cancel;
  Editando := false;
end;

procedure TfrmModeloCadastroPessoas.btnNovoClick(Sender: TObject);
begin
  inclusao := (Sender = btnNovo);
  if inclusao then
    Pessoa.NewRecord
  else
    Pessoa.Edit();
  CarregaCampos(Sender = btnEdita);
  Editando := true;
  edtNome.SetFocus;
end;

procedure TfrmModeloCadastroPessoas.btnGravaClick(Sender: TObject);
begin
  Pessoa.nome.Value := edtNome.Text;
  Pessoa.SaveChanges;
  Editando := false;
end;


procedure TfrmModeloCadastroPessoas.btnApagaClick(Sender: TObject);
begin
  if (MessageBox(0, 'Deseja mesmo apagar o registro?', 'Confirmação', MB_ICONQUESTION or MB_YESNO) = idYES) then
    Pessoa.Delete();
end;

procedure TfrmModeloCadastroPessoas.btPesquisaClick(Sender: TObject);
begin
  {------ Monta o "Select": ------}
  with Pessoa do
    Where([
      nome.Contains(edtPesquisa.Text)
    ])
    .OrderBy([
      nome
    ])
    .Select([]);
  {--------------------------------}
  ClientDataSet1.Open;
  if Pessoa.IsEmpty then
  begin
    MessageBox(0, 'Nenhum registro encontrado!', 'Aviso', MB_ICONWARNING or MB_OK);
  end;
  edtPesquisa.Text := '';
  Editando := false;
  grid.SetFocus;
end;


procedure TfrmModeloCadastroPessoas.CarregaCampos(com_dados: Boolean);
begin
  if com_dados then
    begin
      edtId.Text   := Pessoa.id.Value;
      edtNome.Text := Pessoa.nome.Value;
    end
  Else
    begin
      edtId.Text   := '';
      edtNome.Text := '';
    end; // if..else
end;

procedure TfrmModeloCadastroPessoas.retornaSelecao(Sender: TObject);
begin
  if Pessoa.IsEmpty then Exit;
  {}
  Pessoa.LoadRecord(ClientDataSet1);
  ModalResult := mrOK;
end;

procedure TfrmModeloCadastroPessoas.edtPesquisaChange(Sender: TObject);
begin
  btPesquisa.Enabled := (edtPesquisa.Text <> '');
end;

procedure TfrmModeloCadastroPessoas.edtPesquisaKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case key of
    VK_RETURN:
      btPesquisa.Click;
    VK_ESCAPE:
      ModalResult := mrCancel;
  end;
end;

procedure TfrmModeloCadastroPessoas.FormDestroy(Sender: TObject);
begin
  fCopia.Free
end;

end.

