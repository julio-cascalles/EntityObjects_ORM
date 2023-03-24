unit Principal;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, Grids, ValEdit, ExtCtrls, ImgList, DB, Buttons, CreateTableParser,
  System.Actions, Vcl.ActnList, Vcl.XPStyleActnCtrls, Vcl.ActnMan,
  System.ImageList;

type
    TfrmPrincipal = class(TForm)
        PageControl: TPageControl;
        tabConexao: TTabSheet;
        grpComponente: TGroupBox;
        radFireDac: TRadioButton;
        radZeos: TRadioButton;
        radDbExpress: TRadioButton;
        radADO: TRadioButton;
        radSqlLib: TRadioButton;
        cbxVersaoDelphi: TComboBox;
        lblVersaoDelphi: TLabel;
        cbxTipoBancoDados: TComboBox;
        lblTipoBancoDados: TLabel;
        btProximo: TButton;
        btAnterior: TButton;
        tabTabelas: TTabSheet;
        propriedadesConexao: TPanel;
        edtHost: TEdit;
        lblHost: TLabel;
        edtUser: TEdit;
        lblUser: TLabel;
        edtPassword: TEdit;
        lblPassword: TLabel;
        edtDatabase: TEdit;
        lblDatabase: TLabel;
        chkStarter: TCheckBox;
        edtStringConexao: TEdit;
        lblStringConexao: TLabel;
        ImageList1: TImageList;
        treeTabelas: TTreeView;
        tabArquivos: TTabSheet;
        SaveDialog1: TSaveDialog;
        areaBotoes: TPanel;
        btNovaTabela: TSpeedButton;
        btCampoData: TSpeedButton;
        btCampoNumero: TSpeedButton;
        btCampoTexto: TSpeedButton;
        btCampoMoeda: TSpeedButton;
        SpeedButton1: TSpeedButton;
        memoUnit: TMemo;
        btGravar: TBitBtn;
        edtTamanho: TEdit;
        upTamanho: TUpDown;
        OpenDialog1: TOpenDialog;
        Panel1: TPanel;
        btImporta: TBitBtn;
        chkCriaFields: TCheckBox;
        chkJoin: TCheckBox;
        ActionManager1: TActionManager;
        actNovaTabela: TAction;
        actCampoData: TAction;
        actCampoTexto: TAction;
        actCampoNumero: TAction;
        actCampoMoeda: TAction;
        actExcluir: TAction;
        SpeedButton2: TSpeedButton;
        radInterbase: TRadioButton;
    btArquivoGDB: TSpeedButton;
        procedure chkCriaFieldsClick(Sender: TObject);
        procedure MontaStringConexao(Sender: TObject);
        procedure HabilitaComponentes(Sender: TObject);
        procedure MudaPagina(Sender: TObject);
        procedure HabilitaProximoAnterior(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure actExcluirExecute(Sender: TObject);
        procedure edtStringConexaoChange(Sender: TObject);
        procedure btGravarClick(Sender: TObject);
        procedure edtTamanhoExit(Sender: TObject);
        procedure btImportaClick(Sender: TObject);
        procedure NovoCampoAction(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btArquivoGDBClick(Sender: TObject);
    private
        fModificado: Boolean;
        function IntToFieldType(const Value: integer): TFieldType;
        function FieldTypeToInt(const Value: TFieldType): integer;
        function AddNode(const FieldType: TFieldType; Size: integer; ItemName: string): TTreeNode;
        procedure UpdateMemo;
        function EntityConnectionCommand: string;
        procedure SaveIncFile;
        function ExtractFieldSize(var Str: string): string;
        function FindNode(const Search: string; FieldType: TFieldType): TTreeNode; overload;
        function FindNode(const Search: string; NRoot: TTreeNode): TTreeNode; overload;
        function ConfirmaSaida: Boolean;
        procedure setModificado(const Value: Boolean);
        function TipoBancoGDB: Boolean;
        function podeUsarIBX: Boolean;
    protected
        property modificado: Boolean read fModificado write setModificado;
    public
        function UnitText(const UnitName: string): string;
    end;

function fieldTypeToStr(fieldType: TFieldType): string;

var
    frmPrincipal: TfrmPrincipal;

implementation

{$R *.dfm}

procedure TfrmPrincipal.MontaStringConexao(Sender: TObject);
var
    FmtStrConexao: string;
begin
    radInterbase.Enabled := podeUsarIBX;
    btArquivoGDB.Enabled := TipoBancoGDB;
    if radInterbase.Checked then
    begin
      edtUser.Text := 'SYSDBA';
      edtPassword.Text := 'masterkey';
    end;
    if radADO.Checked then begin
        edtStringConexao.Text := 'Provider=MSDASQL.1;Persist Security Info=False;Data Source=' + edtDatabase.Text;
        if Sender = radADO then
            edtDatabase.SetFocus;
        Exit;
    end;
    if radZeos.Checked then begin
        FmtStrConexao := 'HostName'
    end
    else if radFireDac.Checked then begin
        FmtStrConexao := 'Server'
    end
    else begin
        FmtStrConexao := 'Host';
    end; // if..else
    FmtStrConexao := FmtStrConexao + '=%s,Database=%s,';
    if radDbExpress.Checked then
        FmtStrConexao := FmtStrConexao + 'User_Name'
    else
        FmtStrConexao := FmtStrConexao + 'User';
    FmtStrConexao := FmtStrConexao + '=%s,Password=%s,Port=,';
    if radSqlLib.Checked then begin
        FmtStrConexao := FmtStrConexao + 'Type=%s';
    end
    else if radDbExpress.Checked then begin
        FmtStrConexao := FmtStrConexao + 'DriverName=%s';
    end
    else if radZeos.Checked then begin
        FmtStrConexao := FmtStrConexao + 'Protocol=%s';
    end
    else if radFireDac.Checked then begin
        FmtStrConexao := FmtStrConexao + 'DriverID=%s';
    end
    else begin
        FmtStrConexao := FmtStrConexao + 'Driver=%s';
    end; // if..else
    edtStringConexao.Text := Format(FmtStrConexao, [edtHost.Text, edtDatabase.Text, edtUser.Text, edtPassword.Text, cbxTipoBancoDados.Text]);
end;

procedure TfrmPrincipal.HabilitaComponentes(Sender: TObject);
begin
    radInterbase.Enabled := podeUsarIBX;
    case cbxVersaoDelphi.ItemIndex of
        0: // Delphi 5-7
            begin
                radFireDac.Enabled := false;
                radZeos.Enabled := true;
                radDbExpress.Enabled := false;
                radSqlLib.Enabled := false;
                radADO.Enabled := true;
            end;
        1: // Delphi XE...
            begin
                radFireDac.Enabled := (not chkStarter.Checked);
                radZeos.Enabled := (chkStarter.Checked);
                radDbExpress.Enabled := (not chkStarter.Checked);
                radSqlLib.Enabled := false;
                radADO.Enabled := false;
            end;
        2: // Lazarus
            begin
                radFireDac.Enabled := false;
                radZeos.Enabled := false;
                radDbExpress.Enabled := false;
                radSqlLib.Enabled := true;
                radADO.Enabled := false;
            end;
    end; // case
    radFireDac.Checked    := false;
    radZeos.Checked       := false;
    radDbExpress.Checked  := false;
    radSqlLib.Checked     := false;
    radADO.Checked        := false;
    radInterbase.Checked  := false;
end;

procedure TfrmPrincipal.MudaPagina(Sender: TObject);
var
    idxPage: integer;
begin
    idxPage := PageControl.ActivePageIndex;
    if Sender = btProximo then begin
        Inc(idxPage);
    end
    else begin
        Dec(idxPage);
    end; // if..else
    PageControl.ActivePageIndex := idxPage;
    HabilitaProximoAnterior(nil);
end;

procedure TfrmPrincipal.HabilitaProximoAnterior(Sender: TObject);
var
    idxPage: integer;
begin
    idxPage := PageControl.ActivePageIndex;
    btAnterior.Enabled := (idxPage > 0);
    btProximo.Enabled := (idxPage < PageControl.PageCount - 1);
end;

function fieldTypeToStr(fieldType: TFieldType): string;
begin
    case fieldType of
        ftString, ftFixedChar:
            Result := 'ftString';
        ftDataSet,ftInteger:
            Result := 'ftInteger';
        ftFloat:
            Result := 'ftFloat';
        ftDate:
            Result := 'ftDate';
    end; // case
end;

function TfrmPrincipal.ExtractFieldSize(var Str: string): string;
var
    p: Integer;
begin
    p := Pos('(', Str);
    if (p = 0) then begin
        Result := '';
        Exit;
    end;
    Inc(p);
    Result := Copy(Str, p, Length(Str) - p);
    Str := Copy(Str, 1, p - 2);
end;

function TfrmPrincipal.UnitText(const UnitName: string): string;
var
    tables: TList;
    i, j: integer;
    InterfaceList, ImplementList: TStringList;
    currentTable, ForeignTable, command, ItemName,
    FieldSize, baseClass, mainTable: string;
    NRoot, NItem, NPrimary: TTreeNode;
    FieldType: TFieldType;
    isPrimaryKey: Boolean;
  {### Local functions ###}

    function removeStringSize(const Str: string): string;
    var
      p: Integer;
    begin
      p := Pos('(',Str);
      if p = 0 then
        Result := Str 
      else
        Result := Copy(Str,1,p-1);
    end;

    procedure addInterface;
    var 
      w: Integer;
    begin
        if InterfaceList.Count = 0 then begin
            InterfaceList.Add('unit ' + UnitName + ';');
            InterfaceList.Add('');
            InterfaceList.Add('interface');
            InterfaceList.Add('');
            InterfaceList.Add('uses');
            InterfaceList.Add('  Windows, Messages, SysUtils, Variants,');
            InterfaceList.Add('  Classes, EntityObjects, DB, StdCtrls;');
            InterfaceList.Add('');
            InterfaceList.Add('type');
            InterfaceList.Add('');
        end;
        InterfaceList.Add('  T' + currentTable + ' = class('+baseClass+')');
        if chkCriaFields.Checked and (NRoot.Count > 0) then
        begin
          InterfaceList.Add('  private');
          for w := 0 to NRoot.Count - 1 do begin
              NItem := TTreeNode(NRoot.Item[w]);
              ItemName := 'f_'+removeStringSize(NItem.Text);
              InterfaceList.Add('    '+ItemName+': TEntityField;');
          end; // for
        end;
        InterfaceList.Add('  public');
        if chkCriaFields.Checked then
        begin
          for w := 0 to NRoot.Count - 1 do begin
              NItem := TTreeNode(NRoot.Item[w]);
              ItemName := removeStringSize(NItem.Text);
              InterfaceList.Add('    property '+ItemName+': TEntityField read f_'+ItemName+';');
          end; // for
        end;
        InterfaceList.Add('    constructor Create(_Master: TEntity = nil); override;');
        InterfaceList.Add('  end;');
        InterfaceList.Add('');
    end;

    procedure closeFunction;
    begin
        ImplementList.Add('end;');
        ImplementList.Add('');
    end;

    procedure addImplement;
    var
        functionName: string;
        firstTime: Boolean;
    begin
        firstTime := (ImplementList.Count = 0);
        if firstTime then begin
            ImplementList.Add('');
            ImplementList.Add('implementation');
            ImplementList.Add('');
        end;
        functionName := 'constructor T' + currentTable + '.Create(_Master: TEntity);';
        if ImplementList.IndexOf(functionName) = -1 then begin
            if not firstTime then
                closeFunction;
            ImplementList.Add(functionName);
            ImplementList.Add('begin');
            ImplementList.Add('  inherited;');
            if NRoot.StateIndex <> -2 then
              ImplementList.Add('  TableName := ' + QuotedStr(currentTable) + ';');
        end;
        if command <> '' then ImplementList.Add(command);
    end;
  {#######################}

begin
    tables := TList.Create;
    InterfaceList := TStringList.Create;
    ImplementList := TStringList.Create;
    mainTable := '';
    for i := 0 to treeTabelas.Items.Count - 1 do begin
        NItem := treeTabelas.Items[i];
        if NItem.ImageIndex = 0 then
            tables.Add(NItem)
    end; // for
    for i := 0 to tables.Count - 1 do begin
        NRoot := TTreeNode(tables[i]);
        currentTable := NRoot.Text;
        if NRoot.Data = nil then
          baseClass := 'TEntity'
        else
        begin
          baseClass := 'T'+TTreeNode(NRoot.Data).Text;
          command := '';
          if NRoot.Count = 0 then addImplement();
        end;
        addInterface();
        for j := 0 to NRoot.Count - 1 do begin
            NItem := TTreeNode(NRoot.Item[j]);
            ForeignTable := '';
            ItemName := NItem.Text;
            FieldType := IntToFieldType(NItem.ImageIndex);
            if FieldType = ftDataSet then begin
        {------ Uma tabela dentro de outra tabela: ----}
                ForeignTable := ItemName;
                NPrimary := NItem.getFirstChild();
                if NPrimary <> nil then begin
                    FieldType := IntToFieldType(NPrimary.ImageIndex);
                end;
        {----------------------------------------------}
            end;
            if (FieldType = ftString) then begin
                FieldSize := ExtractFieldSize(ItemName)
            end;
            command := 'AddField(' + QuotedStr(ItemName) + ',' + fieldTypeToStr(FieldType);
            isPrimaryKey := (NItem.StateIndex = 1);
            if isPrimaryKey then
                command := command + ',true';
            if (FieldType = ftString) then begin
                if not isPrimaryKey then
                    command := command + ',false';
                command := command + ',' + FieldSize;
            end;
            command := command + ')';
            if chkCriaFields.Checked then
            begin
              command := '  f_'+ItemName + ' := '+command+';';
              addImplement();
              command := 'f_'+ItemName;
            end;
            if ForeignTable <> '' then begin
                command := command + '.ForeignEntity := T' + ForeignTable + '.Create(self)';
            end
            else if isPrimaryKey then begin
                command := command + '.isAutoInc := true';
            end; // if..else
            if command <> 'f_'+ItemName then
            begin
              command := '  '+command + ';';
              addImplement();
            end;
        end; // for j
        if mainTable = '' then begin
          if (NRoot.StateIndex > -2) then
          begin
            mainTable := currentTable;
            ImplementList.Add('  ' + EntityConnectionCommand);
            if chkJoin.Checked then
              ImplementList.Add('  EntityConnection.Settings.JoinTables := true;');
            ImplementList.Add('  RunDatabase(); // Cria as tabelas, se necessário');
          end;
        end
    end; // for i
    closeFunction;
    Result := interfaceList.text + implementList.text + 'end.';
    interfaceList.Free;
    implementList.Free;
    tables.Free;
end;

function TfrmPrincipal.EntityConnectionCommand: string;
const
    cECONECTION_FORMAT = 'EntityConnection := %s.Create(';
begin
    Result := '';
    if radSqlLib.Checked then
        Result := Format(cECONECTION_FORMAT, ['TEntityConnectionSqlDb']);
    if radZeos.Checked then
        Result := Format(cECONECTION_FORMAT, ['TEntityConnectionZeosLib']);
    if radFiredac.Checked then
        Result := Format(cECONECTION_FORMAT, ['TEntityConnectionFiredac']);
    if radDbExpress.Checked then
        Result := Format(cECONECTION_FORMAT, ['TEntityConnectionDbExpress']);
    if radADO.Checked then
        Result := Format(cECONECTION_FORMAT, ['TEntityConnectionADO']);
    if radInterbase.Checked then
        Result := Format(cECONECTION_FORMAT, ['TEntityConnectionInterbase']);
    if (Result = '') then begin
        Result := '//** Falta criar EntityConnection!';
    end
    else begin
        Result := Result + QuotedStr(edtStringConexao.Text) + ');';
    end; // if..else
end;

procedure TfrmPrincipal.btArquivoGDBClick(Sender: TObject);
begin
    OpenDialog1.Filter := 'Arquivo Firebird|*.fdb|Arquivo Interbase|*.gdb';
    if not OpenDialog1.Execute then
      Exit
    ;
    edtDatabase.Text := OpenDialog1.FileName;
end;

procedure TfrmPrincipal.chkCriaFieldsClick(Sender: TObject);
begin
  UpdateMemo();
end;

procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
    PageControl.ActivePageIndex := 0;
end;

function TfrmPrincipal.AddNode(const FieldType: TFieldType; Size: integer; ItemName: string): TTreeNode;
const
    cNODES_NAMES: array[0..5] of string = ('Tabela', '', 'Campo Data', 'Campo Texto', 'Campo Numérico', 'Campo Moeda');
var
    NRoot, NItem: TTreeNode;
    index: integer;

    function IsInvalidRoot: Boolean;
    begin
        Result := false;
        if (FieldType = ftDataSet) then
            Exit
        else if NRoot = nil then
            Result := true
        else if NRoot.ImageIndex > 0 then
            NRoot := NRoot.Parent;
    end;

    function EmptyName: Boolean;
    begin
        Result := (ItemName = '') and not InputQuery('Novo item...', cNODES_NAMES[index], ItemName)
    end;

begin
    NRoot := treeTabelas.Selected;
    index := FieldTypeToInt(FieldType);
    if IsInvalidRoot or EmptyName() then
        Exit;
  {}
    if (FieldType = ftString) then begin
        ItemName := Format('%s(%d)', [ItemName, Size]);
    end;
    NItem := treeTabelas.Items.AddChild(NRoot, ItemName);
    if (NRoot <> nil)
    and ( IntToFieldType(NRoot.ImageIndex) = ftDataSet)
    and (NRoot.Count = 1) and (FieldType = ftInteger) then begin
        NItem.StateIndex := 1; // Primary Key
    end;
    NItem.ImageIndex := index;
    NItem.SelectedIndex := index;
    if FieldType = ftDataSet then begin
        NItem.Selected := true;
        treeTabelas.SetFocus;
    end
    else begin
        treeTabelas.FullExpand;
    end; // if..else
    Result := NItem;
end;

function TfrmPrincipal.FieldTypeToInt(const Value: TFieldType): integer;
begin
    case Value of
        ftDataSet:
            Result := 0;
        ftDate:
            Result := 2;
        ftString:
            Result := 3;
        ftInteger:
            Result := 4;
        ftFloat:
            Result := 5;
    else
        Result := 1;
    end; // case
end;

function TfrmPrincipal.IntToFieldType(const Value: integer): TFieldType;
begin
    case Value of
        0:
            Result := ftDataSet;
        2:
            Result := ftDate;
        3:
            Result := ftString;
        4:
            Result := ftInteger;
        5:
            Result := ftFloat;
    else
        Result := ftUnknown;
    end; // case
end;

procedure TfrmPrincipal.actExcluirExecute(Sender: TObject);
var
    NItem: TTreeNode;
begin
    NItem := treeTabelas.Selected;
    if NItem = nil then
        Exit;
  {}
    if (MessageBox(0, 'Tem certeza que deseja excluir esse item?', 'Confirmação', MB_ICONQUESTION or MB_YESNO) = idYes) then begin
        NItem.Delete;
        UpdateMemo();
    end;
end;

procedure TfrmPrincipal.UpdateMemo;
begin
    memoUnit.Text := UnitText(ExtractFileName(ChangeFileExt(SaveDialog1.FileName, '')));
    modificado := True;
end;

procedure TfrmPrincipal.edtStringConexaoChange(Sender: TObject);
begin
    UpdateMemo();
end;

procedure TfrmPrincipal.btGravarClick(Sender: TObject);
begin
    if not SaveDialog1.Execute then
      Exit
    ;
    UpdateMemo();
    {------------}
    memoUnit.Lines.SaveToFile(SaveDialog1.FileName);
    SaveIncFile();
    modificado := False;
    MessageDlg('Arquivos gravados!', mtInformation, [mbOK], 0);
    {------------}
end;

procedure TfrmPrincipal.SaveIncFile;
var
    IncLines: TStringList;
    command: string;
begin
    IncLines := TStringList.Create;
    command := '{$define ZEOS_LIB_DB}';
    if not radZeos.Checked then
        command := '//' + command;
    IncLines.Add(command);
    command := '{$define DB_EXPRESS_LIB}';
    if not radDbExpress.Checked then
        command := '//' + command;
    IncLines.Add(command);
    command := '{$define FIRE_DAC_DB}';
    if not radFireDac.Checked then
        command := '//' + command;
    IncLines.Add(command);
    command := '{$define INTERBASE_XE}';
    if not radInterbase.Checked then
        command := '//' + command;
    IncLines.Add(command);
    command := '//{$define ENTITY_LOG_SQL} // uncomment to save SQL commands for debug';
    IncLines.Add(command);
    IncLines.SaveToFile(ExtractFilePath(Application.ExeName) + '\lib\EntityObjects.inc');
    IncLines.Free;
end;

procedure TfrmPrincipal.setModificado(const Value: Boolean);
begin
  fModificado := Value;
  btGravar.Enabled := fModificado;
end;

function TfrmPrincipal.TipoBancoGDB: Boolean;
begin
  Result := ( SameText(cbxTipoBancoDados.Text,'interbase')
           or SameText(cbxTipoBancoDados.Text,'firebird') )
  ;
end;

procedure TfrmPrincipal.edtTamanhoExit(Sender: TObject);
var
    novo_valor: integer;
begin
    if TryStrToInt(edtTamanho.Text, novo_valor) then
        upTamanho.Position := novo_valor;
    edtTamanho.Text := IntToStr(upTamanho.Position);
end;

procedure TfrmPrincipal.btImportaClick(Sender: TObject);
var
    parser: TCreateTableParser;
    LastTable: string;
    NForeign, NItem, NRoot: TTreeNode;
begin
    OpenDialog1.Filter := 'Script de Banco de dados|*.sql';
    if not OpenDialog1.Execute then
        Exit;
  {}
    treeTabelas.OnChange := nil;
//    treeTabelas.Items.BeginUpdate;
    treeTabelas.Items.Clear;
    parser := TCreateTableParser.Create(OpenDialog1.FileName);
    while parser.FindNextField do begin
        if parser.Table <> LastTable then begin
            LastTable := parser.Table;
            treeTabelas.Selected := nil;
            AddNode(ftDataSet, 0, parser.Table);
            NRoot := treeTabelas.Selected;
        end;
        if parser.classList <> '' then
          with TStringList.Create do
          begin
            CommaText := parser.classList;
            while Count > 0 do
            begin
               NItem := AddNode(ftDataSet, 0, Strings[0]);
               NItem.Data := NRoot;
               NRoot.StateIndex := -2;
               Delete(0);
               treeTabelas.Selected := NRoot;
            end;
            Destroy;
          end; // with
        if (parser.FieldType <> ftUnknown) then begin
            AddNode(parser.FieldType, parser.Size, parser.FieldName);
        end Else if (parser.ForeignTable <> '') then begin
           NForeign := FindNode(parser.ForeignTable,ftDataSet);
           if (NForeign <> nil) then begin
               NItem := FindNode(parser.FieldName,treeTabelas.Selected);
               if NItem <> nil then
                 NItem.Delete;
               NForeign.MoveTo(treeTabelas.Selected,naAddChild);
           end;
        end; // if..else
    end; // while
//    treeTabelas.Items.EndUpdate;
    if (cbxTipoBancoDados.ItemIndex = -1) then
        case parser.DatabaseStyle of
            dsMySql:
                cbxTipoBancoDados.ItemIndex := 3;
            dsSqlServer:
                cbxTipoBancoDados.ItemIndex := 7;
        end; // case
    parser.Free;
    UpdateMemo();
end;

function TfrmPrincipal.FindNode(const Search: string; FieldType: TFieldType): TTreeNode;
var
    i: Integer;
begin
    for i := 0 to treeTabelas.Items.Count-1 do begin
        Result := treeTabelas.Items[i];
        if SameText(Result.Text,Search)
        and (IntToFieldType(Result.ImageIndex) = FieldType) then begin
            Exit;
        end;
    end; // for
    Result := nil;
end;

procedure TfrmPrincipal.NovoCampoAction(Sender: TObject);
begin
    AddNode( IntToFieldType(TAction(Sender).ImageIndex), upTamanho.Position, '' );
    UpdateMemo();
end;

function TfrmPrincipal.podeUsarIBX: Boolean;
begin
  Result := TipoBancoGDB
         and (cbxVersaoDelphi.ItemIndex = 1)
         and not chkStarter.Checked
  ;
end;

function TfrmPrincipal.FindNode(const Search: string;
  NRoot: TTreeNode): TTreeNode;
begin
   Result := NRoot.getFirstChild;
   while (Result <> nil) do begin
       if SameText(Result.Text,Search) then Exit;
       {}
       Result := Result.GetNext;
   end; // while
end;

function TfrmPrincipal.ConfirmaSaida: Boolean;
begin
    Result := ( MessageBox(
                            0,
                            'O arquivo ainda não foi salvo. Sair mesmo assim?',
                            'Aviso',
                            MB_ICONWARNING or MB_YESNO
                          )
               = idYes );
end;

procedure TfrmPrincipal.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
   if (modificado) and not ConfirmaSaida then begin
       Action := caNone;
   end;
end;

end.


