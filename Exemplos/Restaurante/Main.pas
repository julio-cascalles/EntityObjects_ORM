unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Dados, EntityObjects,
  System.ImageList,  Vcl.ImgList, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.SQLite;

type
  TMainForm = class(TForm)
    TreeView1: TTreeView;
    ButtonsPanel: TPanel;
    PizzaButton: TButton;
    BurgerButton: TButton;
    RefreshButton: TButton;
    imgLstTree: TImageList;
    SummaryButton: TButton;
    DeleteButton: TButton;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    procedure PizzaButtonClick(Sender: TObject);
    procedure BurgerButtonClick(Sender: TObject);
    procedure DoRefresh(Sender: TObject);
    procedure SummaryButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function CarregaIngredientes(receita_id: integer; Node: TTreeNode): Boolean;
    function MostraResumo(Sender: TEntityField): Boolean;
    function IndexOfNode(const Search: string): integer;
    procedure HabilitaBotoes(const ativar: Boolean);
  public
    function FindNode(const Search: string): TTreeNode;
  end;


var
  MainForm: TMainForm;


implementation


{$R *.dfm}



procedure TMainForm.PizzaButtonClick(Sender: TObject);
var
  pizza: TReceita;
  Node: TTreeNode;
begin
  pizza := TReceita.AddReceita('Pizza',nil);
  {--- Adiciona na pizza a receita da Massa, contendo Farinha, Ovo e Oleo: ---}
  pizza.AddReceita('Massa',4)
       .AddItem('Farinha',  3,  0.30)
       .AddItem('Ovo',      2,  0.25)
       .AddItem('Oleo',     1,  0.60)
     ;
  {--- Adiciona na pizza a receita de Molho, contendo Tomate, Agua e Tempero: ---}
  pizza.AddReceita('Molho',2)
       .AddItem('Tomate',   3,  1.00)
       .AddItem('Agua',     2,  0.25)
       .AddItem('Tempero',  1,  0.10)
     ;
  {------------ Adiciona Queijo à pizza: ---------------------}
  pizza.AddItem('Queijo',  5, 1.25);

  MessageDlg('O valor da pizza é $'+FloatToStr(pizza.Total), mtInformation, [mbOK], 0);
  pizza.Free;
  {------------------}
  DoRefresh(nil);
  Node := FindNode('Pizza');
  if Node <> nil then
  begin
    Node.Selected := true;
    Node.Expand(true);
  end;
  {------------------}
  TreeView1.SetFocus;
end;

procedure TMainForm.HabilitaBotoes(const ativar: Boolean);
begin
  SummaryButton.Enabled := ativar;
  DeleteButton.Enabled := ativar;
end;

procedure TMainForm.DoRefresh(Sender: TObject);
var
  R: TReceita;
  G: TGrupo;
  hasData: Boolean;
  NRoot: TTreeNode;
begin
  R := TReceita.Create();
  G := TGrupo.Create(R);
  G.Receita.ForeignEntity := R;
  {===============================}
  try
   {....................}
    try
      hasData := G.Find([
        G.Ingrediente.Equals(0)
      ],[
        R.id,
        R.nome
      ]);
      HabilitaBotoes(true);
    except
      HabilitaBotoes(false);
    end;
   {....................}
    TreeView1.Items.BeginUpdate;
    TreeView1.Items.Clear;
    while hasData do
    begin
      NRoot := TreeView1.Items.AddChild(nil,R.nome.AsString);
      NRoot.ImageIndex := 0;
      NRoot.SelectedIndex := 0;
      CarregaIngredientes(R.id.AsInteger,NRoot);
      hasData := G.More();
    end;
   {....................}
    TreeView1.Items.EndUpdate;
  finally
    R.Free;
  end;
  {===============================}
end;

procedure TMainForm.SummaryButtonClick(Sender: TObject);
var
  R: TReceita;
  G: TGrupo;
  I: TIngrediente;
begin
  R := TReceita.Create();
  G := TGrupo.Create(R);
  G.Receita.ForeignEntity := R;
  G.OnList := MostraResumo;
  I := TIngrediente.Create(G);
  G.Ingrediente.ForeignEntity := I;
  {--- Quantos ingredientes "caros" tem por receita? ---}
  I.Where([
    I.valor.Greater(1.7)
  ])
  .InTo(
    G
  )
  .GroupBy([
    R.nome
  ])
  .Select([
    R.nome.Count
  ]);
  {-------------------------------------------------------}
  R.Free;
end;

function TMainForm.MostraResumo(Sender: TEntityField): Boolean;
var
  G: TGrupo;
  R: TReceita;
  str_msg: string;
begin
  G := TGrupo(Sender.Parent);
  R := TReceita(G.Receita.ForeignEntity);
  str_msg := 'Ingredientes caros em '+R.nome.AsString+' = '+IntToStr(R.nome.countValue)
            +#13#10'Deseja ver mais?';
  Result := ( MessageDlg(str_msg, mtConfirmation, [mbYes, mbNo], 0) = mrYES );
end;

function TMainForm.CarregaIngredientes(receita_id: integer; Node: TTreeNode): Boolean;
var
  G: TGrupo;
  hasData: Boolean;
  I: TIngrediente;
  NItem: TTreeNode;
begin
  G := TGrupo.Create();
  TReceita.setConnection(G);
  I := TIngrediente.Create(G);
  G.Ingrediente.ForeignEntity := I;
  hasData := G.Find([
    G.Receita.Equals(receita_id),
    G.Ingrediente.Greater(0)
  ],[
    I.id,
    I.nome
  ]);
  Result := hasData;
  while hasData do
  begin
    NItem :=  TreeView1.Items.AddChild(Node,I.nome.AsString);
    if CarregaIngredientes(I.id.AsInteger,NItem) then
      NItem.ImageIndex := 2
    else
      NItem.ImageIndex := 1
    ;
    NItem.SelectedIndex := NItem.ImageIndex;
    hasData := G.More();
  end;
  G.Free;
end;

procedure TMainForm.DeleteButtonClick(Sender: TObject);
var
  MaisUtilizados: TStringList;
  G: TGrupo;
  I: TIngrediente;
begin
  {-----------------}
  G := TGrupo.Create();
  TReceita.setConnection(G);
  I := TIngrediente.Create(G);
  {-----------------}
  MaisUtilizados := TStringList.Create;
  MaisUtilizados.Add('Ovo');
  MaisUtilizados.Add('Tomate');
  MaisUtilizados.Add('Tempero');
  MaisUtilizados.Add('Queijo');
  {-----------------}
  G.Ingrediente.ForeignEntity := I;
  I.Delete([
    I.nome.InList(MaisUtilizados)
  ]);
  {-----------------}
  MaisUtilizados.Free;
  DoRefresh(nil);
  G.Free;
end;

function TMainForm.FindNode(const Search: string): TTreeNode;
var
  idx: Integer;
begin
  idx := IndexOfNode(Search);
  if idx > -1 then
    Result := TreeView1.Items[idx]
  else
    Result := nil
  ;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  DoRefresh(nil);
end;

function TMainForm.IndexOfNode(const Search: string): integer;
begin
  for Result := 0 to TreeView1.Items.Count-1 do
  begin
    if SameText(TreeView1.Items[Result].Text,Search) then
      Exit
    ;
  end; // for
  Result := -1;
end;

procedure TMainForm.BurgerButtonClick(Sender: TObject);
var
  hamburger: TReceita;
  Node: TTreeNode;
begin
  hamburger := TReceita.AddReceita('Hamburguer',nil);
  hamburger.AddItem('Pao',     1,  0.50);
  hamburger.AddReceita('Maionese',2)
           .AddItem('Ovo',     3, 0.25)
           .AddItem('Leite',   1, 0.75)
           .AddItem('Tempero', 1,  0.10)
  ;
  hamburger.AddItem('Carne moida', 1,  2.50);
  hamburger.AddItem('Queijo',      1,  1.25);
  hamburger.AddItem('Tomate',      1,  1.00);
  hamburger.AddItem('Alface',      1,  0.20);
  MessageDlg('O valor do hamburguer é $'+FloatToStr(hamburger.Total), mtInformation, [mbOK], 0);
  hamburger.Free;
  {------------------}
  DoRefresh(nil);
  Node := FindNode('Hamburguer');
  if Node <> nil then
  begin
    Node.Selected := true;
    Node.Expand(true);
  end;
  {------------------}
  TreeView1.SetFocus;
end;

end.
