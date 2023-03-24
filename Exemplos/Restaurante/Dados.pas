unit Dados;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, EntityObjects, DB, StdCtrls;

type

  TIngrediente = class(TCachedEntity)
  private
    f_id: TEntityField;
    f_nome: TEntityField;
    f_valor: TEntityField;
  protected
    function getTotal: Double; virtual;
  public
    quantidade: integer;
    property id: TEntityField read f_id;
    property nome: TEntityField read f_nome;
    property valor: TEntityField read f_valor;
    property Total: Double read getTotal;
    constructor Create(_Master: TEntity = nil); override;
  end;

  TReceita = class;

  TGrupo = class(TCachedEntity)
  private
    f_id: TEntityField;
    f_Ingrediente: TEntityField;
    f_Receita: TEntityField;
    f_quantidade: TEntityField;
  public
    property id: TEntityField read f_id;
    property Ingrediente: TEntityField read f_Ingrediente;
    property Receita: TEntityField read f_Receita;
    property quantidade: TEntityField read f_quantidade;
    constructor Create(_Master: TEntity = nil); override;
  end;

  TReceita = class(TIngrediente)
  private
    List: TList;
  protected
    function getTotal: Double; override;
  public
    function AddItem(AItem: TIngrediente; const AQtd: integer = 1): TReceita; overload;
    function AddItem(const ANome: string; const AQtd: integer; AValor: double): TReceita; overload;
    class function AddReceita(const ANome: string; _Parent: TReceita; add_emptyItem: Boolean = true): TReceita; overload;
    class procedure setConnection(C: TCachedEntity);
    function AddReceita(const ANome: string; const AQtd: integer): TReceita; overload;
    constructor Create(_Master: TEntity = nil); override;
    destructor Destroy; override;
end;


implementation

var
   DatabaseReceita, DatabaseGrupo: Boolean;


constructor TIngrediente.Create(_Master: TEntity);
begin
  inherited;
  TableName := 'Ingrediente';
  quantidade := 1;
  f_id := AddField('id',ftInteger,true);
  f_id.isAutoInc := true;
  f_nome := AddField('nome',ftString,false,30);
  f_valor := AddField('valor',ftFloat);
end;

constructor TGrupo.Create(_Master: TEntity);
begin
  inherited;
  TableName := 'Grupo';
  f_id := AddField('id',ftInteger,true);
  f_id.isAutoInc := true;
  f_Ingrediente := AddField('Ingrediente',ftInteger);
  f_Receita := AddField('Receita',ftInteger);
  f_quantidade := AddField('quantidade',ftInteger);
  if not DatabaseGrupo then
  begin
    RunDatabase(); // Cria as tabelas, se necessário
    DatabaseGrupo := true;
  end;
end;


{ TReceita }

function TReceita.AddItem(const ANome: string; const AQtd: integer; AValor: double): TReceita;
var
  AItem: TIngrediente;
begin
  Result := self;
  AItem := TIngrediente.Create(self);
  if not AItem.Find([AItem.nome.Equals(ANome)],[]) then
  begin
    AItem.NewRecord();
    AItem.nome.Value  := ANome;
  end;
  AItem.valor.Value := AValor;
  AItem.SaveChanges();
  AddItem(AItem,AQtd);
end;

class function TReceita.AddReceita(const ANome: string; _Parent: TReceita; add_emptyItem: Boolean = true): TReceita;
begin
   Result := TReceita.Create(_Parent);
   if not Result.Find([Result.nome.Equals(ANome)],[]) then
   begin
     Result.NewRecord();
     Result.nome.Value := ANome;
   end;
   Result.SaveChanges();
   if add_emptyItem and (_Parent = nil) then
     Result.AddItem(nil)
   ;
end;

function TReceita.AddItem(AItem: TIngrediente; const AQtd: integer): TReceita;
var
  Grupo: TGrupo;
  id_ingrediente: Integer;
  found: Boolean;
begin
  Result := self;
  Grupo := TGrupo.Create(self);
  if AItem <> nil then
    begin
      AItem.quantidade := AQtd;
      List.Add(AItem);
      id_ingrediente := AItem.id.Value;
    end
  Else
    begin
      id_ingrediente := 0;
    end;
  found := Grupo.Find([
     Grupo.Ingrediente.Equals(id_ingrediente),
     Grupo.Receita.Equals(self.id.Value)
  ],[]);
  if not found then
    Grupo.NewRecord()
  ;
  Grupo.Receita.Value     := self.id.Value;
  Grupo.Ingrediente.Value := id_ingrediente;
  Grupo.quantidade.Value  := AQtd;
  Grupo.SaveChanges();
  Grupo.Free;
end;

function TReceita.AddReceita(const ANome: string; const AQtd: integer): TReceita;
begin
  Result := TReceita.AddReceita(ANome,self);
  AddItem(Result,AQtd);
end;

constructor TReceita.Create(_Master: TEntity);
begin
  inherited;
  List := TList.Create;
  if _Master = nil then
  begin
    setConnection(self);
    if not DatabaseReceita then
      RunDatabase(); // Cria as tabelas, se necessário
    DatabaseReceita := true;
  end;
end;

class procedure TReceita.setConnection(C: TCachedEntity);
begin
  C.EntityConnection := TEntityConnectionInterbase.Create('Host=localhost,Database=C:\VM_Shared\db\FOOD.FDB,User=SYSDBA,Password=123,Port=,Driver=Firebird');
  C.LocalConnection  := TEntityConnectionFiredac.Create('Database=./shadow.db,DriverID=SQLite');
  C.EntityConnection.Settings.JoinTables  := true;
  C.EntityConnection.Settings.AutoIncType := aitAuxTable;
end;

destructor TReceita.Destroy;
var
  AItem: TIngrediente;
begin
  while List.Count > 0 do
  begin
    AItem := TIngrediente(List[0]);
    AItem.Free;
    List.Delete(0);
  end; // while
  List.Free;
  inherited;
end;

function TReceita.getTotal: Double;
var
  i: Integer;
  ingreditente: TIngrediente;
begin
  Result := 0.00;
  for i := 0 to List.Count-1 do
  begin
    ingreditente := TIngrediente(List[i]);
    Result := Result + ingreditente.Total;
  end;
  if valor.AsFloat = 0.00 then
  begin
    valor.Value := Result;
    SaveChanges(); // Atualiza o valor calculado da receita
  end;
  Result := Result * quantidade;
end;

function TIngrediente.getTotal: Double;
begin
  Result := (f_valor.AsFloat * quantidade);
end;

end.