{*******************************
    Entity Framework for Delphi/Lazarus - version 1.46
	(codinome "Finta")
     by Julio Cascalles - 2015/2019 - Brasil
********************************}
unit EntityObjects;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I EntityObjects.inc}



{$ifdef FPC}
  {$define EQUALS_EXISTS}
{$else}
  {$IF CompilerVersion >= 25}
    {$define DELPHI_XE_SUP}
  {$ifend}
{$endif}
{$ifdef DELPHI_XE_SUP}
  {$define EQUALS_EXISTS}
{$endif}

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils,
  {$ifndef VER130}
  Variants,
  {$endif}
  Classes, TypInfo,
  {$ifdef ZEOS_LIB_DB}
    {$ifdef FPC}
      ZConnection, ZDataset,
    {$else}
      ZAbstractRODataset,
      ZDataset, ZAbstractConnection, ZConnection,
    {$endif}
  {$endif}
  DB
  {$ifdef FPC}
  ,mysql50conn, mssqlconn, pqconnection,
  oracleconnection,IBConnection,odbcconn,
  sqldb
  {$else}
      {$IFDEF DB_EXPRESS_LIB}
          ,Provider, DBClient
          {$ifdef  DELPHI_XE_SUP}
            ,Data.SqlExpr,
            Data.DBXFirebird,Data.DbxMySql,Data.DbxMSSQL,Data.DbxOracle
            //,Data.DbxODBC
          {$else}
            ,DBXpress, SqlExpr
          {$endif}
      {$ENDIF}

      {$IFDEF FIRE_DAC_DB}
        ,Firedac.Stan.Intf, Firedac.Stan.Option,
        Firedac.Stan.Param, Firedac.Stan.Error, Firedac.DatS, Firedac.Phys.Intf,
        Firedac.DApt.Intf, Firedac.Stan.Async, Firedac.DApt, Firedac.UI.Intf,
        Firedac.Stan.Def, Firedac.Stan.Pool, Firedac.Phys, Firedac.VCLUI.Wait,
        Firedac.Comp.Client, Firedac.Comp.DataSet
      {$ENDIF}

      {$ifdef INTERBASE_XE}
        ,IBX.IBDatabase, IBX.IBCustomDataSet, IBX.IBQuery
      {$endif}
      ,ADODB    {$define OLD_VERSION}
  {$endif}
  ;


var
  LINE_BREAK: string = #13#10;
  TAB_CHAR: string = #9;

const
  COMMENT_TO_NOT_SAVE = '/* Dont save */';


type

  TEntity = class;

  TSearchCompare = (scGreater,scLess,scContains,scEquals);
  TLogicalOperator = (_AND_, _OR_);

  TConditionList = class;

  TConditionItem = class
  private
    fOwner: TConditionList;
    procedure SetOwner(const Value: TConditionList);
  protected
    property Owner: TConditionList read fOwner write SetOwner;
    function LOName: string;
    function isValid: Boolean; virtual;
  public
    prefix, separator, suffix: string;
    LogicalOperator: TLogicalOperator;
    property FieldName:string read prefix write prefix;
    property ConditionValue:string read suffix write suffix;
    function Expression: string; virtual;
    function _OR: TConditionItem;
  end;

  TEntityField = class;

  TConditionList = class(TConditionItem)
  private
    fList: TList;
    fGrouped: Boolean;
    function getItem(Index: integer): TConditionItem;
    function getCount: integer;
  protected
    fEntity: TEntity;
    function isValid: Boolean; override;
  public
    property Item[Index: integer]: TConditionItem read getItem; default;
    property Count: integer read getCount;
    property Grouped: Boolean read fGrouped;
    procedure Add(_item: TConditionItem);
    function InTo(Target: TEntity): TConditionList;
    function Remove(_item: TConditionItem):Boolean;
    function Expression: string; override;
    constructor Create(AEntity: TEntity);
    destructor Destroy; override;
    procedure Clear;
    function Where(conditions: array of TConditionItem): TConditionList;
    function _OR(conditions: array of TConditionItem): TConditionList;
    function _AND(conditions: array of TConditionItem): TConditionList;
    function OrderBy(FieldList: array of TEntityField): TConditionList;
    function GroupBy(FieldList: array of TEntityField): TConditionList;
    procedure Select(FieldList: array of TEntityField);
  end;

  TFieldAttribute = (faNotNull,faAutoIncrement,faUnique,faPrimarykey);
  TFieldAttributes = set of TFieldAttribute;

  TSqlQueryAttrib = set of (qa_ORDER,qa_GROUP);

  TSqlElement = (se_SELECT,se_FROM,se_WHERE,se_GROUPBY,se_ORDERBY);
  TSqlArray = array[TSqlElement] of string;


    TSqlFieldBuilder = class
    private
      fItem: TEntityField;
    public
      //property item: TEntityField read fItem;
      constructor Create(AItem: TEntityField);
      function Text: string; virtual;
    end;
    TAggregateBuilder = class(TSqlFieldBuilder)
    protected
      function Alias: string; virtual;
      function functionName: string; virtual; abstract;
    public
      function Text: string; override;
    end;
    TCountBuilder = class(TAggregateBuilder)
    protected
      function Alias: string; override;
      function functionName: string; override;
    end;
    TMaxBuilder = class(TAggregateBuilder)
    protected
      function functionName: string; override;
    end;
    TMinBuilder = class(TAggregateBuilder)
    protected
      function functionName: string; override;
    end;
    TAvgBuilder = class(TAggregateBuilder)
    protected
      function functionName: string; override;
    end;
    TSumBuilder = class(TAggregateBuilder)
    protected
      function functionName: string; override;
    end;

  TEntityField = class
  private
    fParent: TEntity;
    FName: string;
    FFieldType: TFieldType;
    fForeignEntity: TEntity;
    FSize: integer;
    fBuilder: TSqlFieldBuilder;
    FieldAttributes: TFieldAttributes;
    procedure SetFieldType(const _Value: TFieldType);
    procedure SetName(const _Value: string);
    procedure SetForeignEntity(const _Value: TEntity);
    procedure SetSize(const _Value: integer);
    procedure SetAttribute(_Value: string);
    function getAttribute: string;
    function NewCondition(SearchCompare: TSearchCompare): TConditionItem;
    procedure SetAutoInc(_Value: Boolean);
    function getAutoInc: Boolean;
    function getBuilder: TSqlFieldBuilder;
    procedure setBuilder(const Value: TSqlFieldBuilder);
    function getGrouped: Boolean;
    function getSorted: Boolean;
  protected
    FValue: Variant;
    isModified: Boolean;
    QueryAttributes: TSqlQueryAttrib;
    property Builder: TSqlFieldBuilder read getBuilder write setBuilder;
    procedure SetValue(const _Value: Variant); virtual;
    function getValue: Variant; virtual;
    function DataSetField(DataSet: TDataSet = nil): TField;
    procedure Clear;
  public
    ignore: Boolean;
    countValue: integer;
    property ForeignEntity: TEntity read fForeignEntity write SetForeignEntity;
    property Value: Variant read getValue write SetValue;
    property Name: string read FName write SetName;
    property FieldType: TFieldType read FFieldType write SetFieldType;
    property Size: integer read FSize write SetSize;
    property Parent: TEntity read fParent;
    property isAutoInc: Boolean read getAutoInc write SetAutoInc; { DONE -c1.06 : Auto increment control }
    property Attribute: string read getAttribute write SetAttribute;
    {}
    { DONE -c1.34 : GroupBy / OrderBy }
    property Sorted: Boolean read getSorted;
    property Grouped: Boolean read getGrouped;
    {}
    function AsInteger: Integer;
    function AsFloat: Double;
    function AsDate: TDateTime;
    function AsString: string;
    function SqlFormat: string;
    function SqlFunction: string;
    function FullName: string;
    constructor Create(_Parent: TEntity);
    destructor Destroy; override;
    {}
    function Greater(Search: Variant): TConditionItem;
    function Less(Search: Variant): TConditionItem;
    function Contains(Search: Variant): TConditionItem;
    function InList(List: TStrings): TConditionItem;
    function Equals(Search: Variant): TConditionItem; {$ifdef EQUALS_EXISTS} overload; {$endif}
    {}
    { DONE -c1.33 : Aggregations }
    function Count: TEntityField;
    function Max: TEntityField;
    function Min: TEntityField;
    function Avg: TEntityField;
    function Sum: TEntityField;
  end;


  TAutoIncrementType = (aitNone, aitMySql,aitSqlServer,aitPostgre, aitAuxTable);

  TJoinState = (jsCHANGED, jsENABLED,jsDISABLED);

  TEntityConnectionType = (ect_Normal, ect_DbExpress);

  TEntitySettings = class
  private
    fAutoInc: TAutoIncrementType;
    fJoinTables: Boolean;
    fJoinState: TJoinState;
    fUsingDbExpress: Boolean;
    procedure SetAutoIncType(const Value: TAutoIncrementType);
    procedure setJoinTables(const Value: Boolean);
    procedure setJoinState(const Value: TJoinState);
  protected
    AuxTable: TEntity;
    property JoinState: TJoinState read fJoinState write setJoinState;
  public
    DateFormat,
    FloatFormat,
    StartLikeExpr,
    EndLikeExpr: string;
    property AutoIncType: TAutoIncrementType read fAutoInc write SetAutoIncType;
    property JoinTables: Boolean read fJoinTables write setJoinTables;
    property UsingDbExpress: Boolean read fUsingDbExpress;
    constructor Create(ConnectionType: TEntityConnectionType);
    destructor Destroy; override;
  end;

  { DONE -c1.1 : Use EntityConnection as Interface }

  TConListAction = (claTables,claFields,claClear);

  IEntityConnection = interface
    function Settings: TEntitySettings;
    function ExecuteCommand(const _command: string; returnData: Boolean): TDataSet;
    function GetList(Action: TConListAction; const Params: string = ''): TStringList;
  end;


  TSqlDbType = (sdtSqlServer,sdtSybase,sdtPostGre,sdtMySql,sdtOracle,sdtFireBird, sdtUnknown);

  { TCustomEntityConnection }

  TCustomEntityConnection = class(TInterfacedObject,IEntityConnection)
    private
      f_Settings: TEntitySettings;
      fTempDataSets: TList;
      {$ifdef ENTITY_LOG_SQL}
      Log_id: integer;
      procedure SaveLog(Strings: TStrings);
      {$endif}
      procedure ClearDataSets;
      procedure AddTempDataSet(_dataSet: TDataSet);
    protected
      Connection: TComponent; //TCustomConnection;
      function ConnectionType: TEntityConnectionType; virtual;
      function TypeOfDb(Str: string): TSqlDbType;
      function ExecuteCommand(const _command: string; returnData: Boolean): TDataSet; virtual; abstract;
      { DONE -c1.24b : ExecuteCommand move to protected }
    public
      destructor Destroy; override;
      function Settings: TEntitySettings; virtual;
      function GetList(Action: TConListAction; const Params: string = ''): TStringList; virtual;
  end;

  {$ifdef FPC}

  { DONE -c1.2 : Lazarus support }


  { TEntityConnectionSqlDb }

  TEntityConnectionSqlDb = class(TCustomEntityConnection)
    private
      Transaction: TSQLTransaction;
    protected
      function ExecuteCommand(const _command: string; returnData: Boolean): TDataSet; override;
    public
      constructor Create(const _ConnectionString: string);
      destructor Destroy; override;
      function GetList(Action: TConListAction; const Params: string = ''): TStringList; override;
  end;

  {$endif}

  {$ifdef INTERBASE_XE}

    { DONE -c1.39 : IBX connection for Interbase or Firebird }

    TEntityConnectionInterbase =  class(TCustomEntityConnection)
    private
      Transaction: TIBTransaction;
      Database: TIBDatabase;
    protected
      function ExecuteCommand(const _command: string; returnData: Boolean): TDataSet; override;
    public
      constructor Create(const _ConnectionString: string);
      destructor Destroy; override;
      function GetList(Action: TConListAction; const Params: string = ''): TStringList; override;
    end;
  {$endif}

 {$ifdef DB_EXPRESS_LIB}
    { DONE -c1.23 : Connection for Delphi XE... }
    TEntityConnectionDbExpress =  class(TCustomEntityConnection)
    protected
      function ExecuteCommand(const _command: string; returnData: Boolean): TDataSet; override;
      function ConnectionType: TEntityConnectionType; override;
    public
      constructor Create(const _ConnectionString: string);
      function GetList(Action: TConListAction; const Params: string = ''): TStringList; override;
    end;
 {$endif}

 {$ifdef OLD_VERSION}
    TEntityConnectionADO = class(TCustomEntityConnection)
      protected
        function ExecuteCommand(const _command: string; returnData: Boolean): TDataSet; override;
      public
        constructor Create(const _ConnectionString: string);
        function GetList(Action: TConListAction; const Params: string = ''): TStringList; override;
      end;
  {$endif}

  {$ifdef ZEOS_LIB_DB}
    TEntityConnectionZeosLib = class(TCustomEntityConnection)
    protected
      function ExecuteCommand(const _command: string; returnData: Boolean): TDataSet; override;
    public
      constructor Create(const _ConnectionString: string);
      function GetList(Action: TConListAction; const Params: string = ''): TStringList; override;
    end;
  {$endif}

  {$IFDEF FIRE_DAC_DB}
  //DONE -oAmarildo -c1.25 : Firedac Support
  TEntityConnectionFiredac = class(TCustomEntityConnection)
    protected
      function ExecuteCommand(const _command: string; returnData: Boolean): TDataSet; override;
    public
      constructor Create(const _ConnectionString: string);
      function GetList(Action: TConListAction; const Params: string = ''): TStringList; override;
  end;
  {$ENDIF}



  TCommaTextOptions = set of (ctoNames,ctoValues);

  TEntityState = (estReady, estLoading, estEditing);

  TEntityEvent = function(Sender: TEntityField): Boolean of object;

  TEntity = class
  private
    fItems, fRelations: TList;
    FPrimaryKey: TEntityField;
    isNewRecord,keepDataSet: Boolean;
    fTableName: string;
    fState: TEntityState;
    f_IsEmpty,fCreated: Boolean;
    fConditionList: TConditionList;
    fEntityConnection: IEntityConnection;  { DONE -c1.1 : Allow to use many connections }
    f_DataSource: TDataSource;
    f_DataSet: TDataSet;
    fMaster: TEntity;
    {$ifdef DB_EXPRESS_LIB}
    objProvider: TDataSetProvider;
    procedure setProviderName(const Value: string);
    function getProviderName: string;
    {$ENDIF}
    procedure SetPrimaryKey(const _Value: TEntityField);
    function getPrimaryKey: TEntityField;
    function NothingToDo: Boolean;
    procedure SetTableName(const _Value: string);
    function getTableName: string;
    function getPKValueFromDB: Variant;
    procedure SetEntityConnection(const Value: IEntityConnection);
    function CalcNextPK(Sender: TEntityField): Boolean;
    procedure SetDataSet(const Value: TDataSet);
    procedure InternalDelete(_field: TEntityField = nil);
    procedure CheckRelations;
  protected
    needReload: Boolean;
    property DataSet: TDataSet read f_DataSet write SetDataSet;
    function getCount: integer;
    function getItem(const Index: integer): TEntityField;
    function getField(const Name:string): TEntityField;
    function CommaText(options: TCommaTextOptions; ignorePK: Boolean = true): string;
    function IndexOf(const Search: string):integer;
    procedure AddRelation(field: TEntityField);
    procedure CheckConnection;
    function selectCommand: string; virtual;
    procedure LoadFromTable; virtual;
    procedure FillSqlElements(var Elements: TSqlArray; JoinCondition: TConditionItem = nil); virtual;
    procedure SetReady(recursive: Boolean = false);
    function enableFields(FieldList: array of TEntityField; checkJoin: Boolean = true): Boolean;
    {$ifdef DB_EXPRESS_LIB}
    property ProviderName: string read getProviderName write setProviderName;
    {$ENDIF}
    procedure TransferDataSet(Target: TEntity);
  public
    BeforeInsert,OnList: TEntityEvent;
    AutoIncType: TAutoIncrementType;
    property IsEmpty: Boolean read f_IsEmpty;
    property TableName: string read getTableName write SetTableName;
    property PrimaryKey: TEntityField read getPrimaryKey write SetPrimaryKey;
    property Field[const Name: string]: TEntityField read getField; default;
    property State: TEntityState read fState;
    property ConditionList: TConditionList read fConditionList;
    property Created: Boolean read fCreated;
    property EntityConnection: IEntityConnection read fEntityConnection write SetEntityConnection;
             { DONE -c1.24 : property EntityConnection moved to public }
    property Master: TEntity read fMaster;
    constructor Create(_Master: TEntity = nil); virtual;
    destructor Destroy; override;
    function AddField(const Name: string; FieldType: TFieldType;
      isPrimaryKey:Boolean = False; Size: integer = 0): TEntityField;
    procedure Assign(obj: TPersistent; const keyField: string = ''); overload;
    procedure Assign(Source: TEntity); overload;
    procedure NewRecord;
    procedure Edit(load_field: Boolean = true);
    procedure Cancel;
    function SaveChanges: Boolean; virtual;
    procedure ClearFields;
    procedure RunDatabase(tables: TStrings = nil);
    function Clone: TEntity;
    function Copy: TEntity;
    procedure Delete; overload;
    procedure Delete(conditions: array of TConditionItem); overload;
    procedure List(item: TEntityField; Event: TEntityEvent);
    function FieldForEntity(Search: TEntity): string;
    class function From(Target: TEntity): TConditionList;
    function Where(conditions: array of TConditionItem): TConditionList;
    function Find(const Search: Variant; FieldList: array of TEntityField): Boolean; overload;
    function Find(conditions: array of TConditionItem; FieldList: array of TEntityField): Boolean; overload; virtual;
    procedure setDataSource(ADataSource: TDataSource);
    // DONE -c1.25b : Delphi 5 bug fix
    function MaxPKValue: integer;
    procedure LoadRecord(Source: TDataSet = nil);
    function indexOfCountField: integer;
    function More: Boolean;
    function ValuesOf(conditions: array of TConditionItem): TStringList;
  end;

  TCachedEntity = class(TEntity)
  private
    Cache: TEntity;
    function getLocalConnection: IEntityConnection;
    procedure setLocalConnection(const Value: IEntityConnection);
  protected
    function selectCommand: string; override;
  public
    property LocalConnection: IEntityConnection read getLocalConnection write setLocalConnection;
    function Find(conditions: array of TConditionItem;
      FieldList: array of TEntityField): Boolean; override;
    function SaveChanges: Boolean; override;
    destructor Destroy; override;
  end;


implementation


{ TCustomEntityConnection }

function TCustomEntityConnection.TypeOfDb(Str: string): TSqlDbType;
const
  dbTypes: array[TSqlDbType] of string = ('sqlserver','sybase','postgre','mysql','oracle','firebird',   '');
                                    // sdtSqlServer,sdtSybase,sdtPostGre,sdtMySql,sdtOracle,sdtFireBird, sdtUnknown
begin
  Str := LowerCase(Str);
  for Result := sdtSqlServer to sdtFireBird do
  begin
    if (Pos(Str,dbTypes[Result]) > 0)
    or (Pos(dbTypes[Result],Str) > 0) then Exit;
    {}
    if (Result = sdtSqlServer) and SameText(Str,'mssql')
    or (Result = sdtFireBird) and SameText(Str,'interbase') then Exit;
  end; // for
  Result := sdtUnknown;
end;

procedure TCustomEntityConnection.AddTempDataSet(_dataSet: TDataSet);
begin
  if fTempDataSets = nil then
    fTempDataSets := TList.Create;
  fTempDataSets.Add(_dataSet);
end;

procedure TCustomEntityConnection.ClearDataSets;
var
  _dataSet: TDataSet;
begin
  if fTempDataSets = nil then Exit;
 { DONE -c1.25 : DataSet list independent from Connection }
  while fTempDataSets.Count > 0 do
  begin
    _dataSet := TDataSet(fTempDataSets[0]);
    _dataSet.Free;
    fTempDataSets.Delete(0);
  end; // while
  FreeAndNil(fTempDataSets)
end;

function TCustomEntityConnection.GetList(Action: TConListAction;
  const Params: string): TStringList;
var
  _dataSet: TDataSet;
begin
  Result := nil;
  { DONE -c1.25 : Base method GetList for inheritance }
  case Action of
    {$ifndef FPC}
    claFields:
      begin
        _dataSet := ExecuteCommand(Params,true);
        Result := _dataSet.FieldList;
        AddTempDataSet(_dataSet);
      end;
    {$endif}
    claClear: ClearDataSets;
  end; // case
end;

{$ifdef ENTITY_LOG_SQL}
procedure TCustomEntityConnection.SaveLog(Strings: TStrings);
var
  LogFile: string;
begin
  if Pos(COMMENT_TO_NOT_SAVE,Strings.Text) > 0 then
    Exit
  ;
  Inc(Log_id);
  LogFile := '.\'+FormatDateTime('yyyyddmmhhnn',Now)
            +'_'+Copy(Strings.Text,1,3)
            +'_'+IntToStr(Log_id)+'.sql';
  Strings.SaveToFile(LogFile);
end;
{$endif}


function TCustomEntityConnection.Settings: TEntitySettings;
begin
  if f_Settings = nil then
    f_Settings := TEntitySettings.Create(Self.ConnectionType);
  Result := f_Settings;
end;


{$ifdef FPC}

constructor TEntityConnectionSqlDb.Create(const _ConnectionString: string);
var
  LTemp: TStringList;
  myConnection: TSQLConnection;
begin
  LTemp := TStringList.Create;
  LTemp.CommaText := _ConnectionString;
  case TypeOfDb(LTemp.Values['Type']) of
    sdtSqlServer:
      begin
        Connection := TMsSqlConnection.Create(nil);
        Settings.AutoIncType := aitSqlServer;
      end;
    sdtSybase:
      Connection := TSybaseConnection.Create(nil);
    sdtPostGre:
      begin
        Connection := TPQConnection.Create(nil);
        Settings.AutoIncType := aitPostgre;
      end;
    sdtMySql:
      begin
        Connection := TMySQLConnectionDef.ConnectionClass.Create(nil);
        TConnectionName(Connection).Port:=StrToIntDef(LTemp.Values['Port'],3306);
        Settings.AutoIncType := aitMySql;
      end;
    sdtOracle:
      Connection := TOracleConnection.Create(nil);
    sdtFireBird:
      Connection := TIBConnection.Create(nil);
    sdtUnknown:
      //raise Exception.Create('Unknown Database type');
      Connection := TODBCConnection.Create(nil);
  end;
  myConnection := TSQLConnection(Connection);
  myConnection.HostName     := LTemp.Values['Host'];
  myConnection.DatabaseName := LTemp.Values['Database'];
  myConnection.Password     := LTemp.Values['Password'];
  myConnection.UserName     := LTemp.Values['User'];
  Transaction := TSQLTransaction.Create(nil);
  Transaction.DataBase := myConnection;
  LTemp.Free;
end;

destructor TEntityConnectionSqlDb.Destroy;
begin
  Transaction.Free;
  inherited Destroy;
end;


function TEntityConnectionSqlDb.ExecuteCommand(const _command: string;
  returnData: Boolean): TDataSet;
begin
  Result := TSqlQuery.Create(nil);
  with TSQLQuery(Result) do
  begin
    ReadOnly := true;
    AutoCalcFields := false;
    ParamCheck := false;
    UniDirectional := true;
    DataBase := TSQLConnection(Connection);
    Options := [sqoAutoCommit];
    Transaction := self.Transaction;
    SQL.Text := _command;
    {$ifdef ENTITY_LOG_SQL}
    SaveLog(Sql);
    {$endif}
    if returnData then
      Open
    else
      begin
        ExecSQL;
        Result.Free
      end;
  end; // with
end;

function TEntityConnectionSqlDb.GetList(Action: TConListAction; const Params: string
  ): TStringList;
begin
  Result := nil;
  if Action = claClear then Exit;
  Result := TStringList.Create;
  case Action of
    claTables: TSQLConnection(Connection).GetTableNames(Result);
    claFields: TSQLConnection(Connection).GetFieldNames(Params,Result);
  end;
end;
{$endif}


function TEntity.AddField(const Name: string; FieldType: TFieldType;
  isPrimaryKey:Boolean = False; Size: integer = 0): TEntityField;
var
  i: integer;
begin
  i := IndexOf(Name);
  if i > -1 then
  begin
    Result := TEntityField(fItems[i]);
    Exit;
  end;
  Result := TEntityField.Create(Self);
  Result.Name := Name;
  if isPrimaryKey then
    PrimaryKey := Result;
  Result.FieldType := FieldType;
  if Size > 0 then
    Result.Size := Size;
  fItems.Add(Result);
end;

constructor TEntity.Create(_Master: TEntity = nil);
begin
  fItems := TList.Create();
  fConditionList := TConditionList.Create(self);
  f_IsEmpty := true;
  if (EntityConnection = nil) and (_Master <> nil) then
  begin
    EntityConnection := _Master.EntityConnection;
  end;
  fMaster := _Master;
  SetReady();
end;

destructor TEntity.Destroy;
var
  i: Integer;
  item: TEntityField;
begin
  for i := fItems.Count-1 downto 0 do
  begin
    item := TEntityField(fItems[i]);
    { DONE -c1.29 : Destroy Foreign Entities }
    if (item.ForeignEntity <> nil) and (item.ForeignEntity.Master = self) then
      item.ForeignEntity.Free
    ;
    item.Free;
  end; // for
  fItems.Free;
  if fRelations <> nil then
    fRelations.Free;
  {}
  {$ifdef DB_EXPRESS_LIB}
      if objProvider <> nil then
        objProvider.Free;
  {$ENDIF}
  fConditionList.Free;
  DataSet := nil;
  inherited;
end;

function TEntity.getItem(const Index: integer): TEntityField;
begin
  if (index < 0) or (index >= fItems.Count) then
    Result := nil
  else
    Result := TEntityField(fItems[index]);
end;

function TEntity.getField(const Name: string): TEntityField;
begin
  Result := getItem( IndexOf(Name) );
  if Result = nil then
    raise Exception.Create('Field "'+Name+'" not found in Table "'+fTableName+'"')
end;

procedure TEntityField.Clear;
begin
  case FieldType of
    ftString:
      Value := '';
    ftFloat:
      Value := 0.00;
    ftInteger:
      Value := 0;
    ftDate:
      Value := Now;
  end; // if..case
end;

procedure TEntity.ClearFields;
var
  i: Integer;
  item: TEntityField;
begin
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    if (item.ForeignEntity = nil) and not item.ignore then
    begin
      item.Clear;
      item.isModified := false;
    end;
  end; // for
  needReload := false;
end;


procedure TEntity.NewRecord;
begin
  Edit(false);
  isNewRecord := True;
end;

function TEntity.SaveChanges: Boolean;
var
  _command: string;
  attr_pk: boolean;
  AutoIncTable: TEntity;
begin
  Result := true;
  CheckConnection();
  AutoIncTable := EntityConnection.Settings.AuxTable;
  if isNewRecord then
    begin
    if (PrimaryKey.isAutoInc) then
    begin
      if (AutoIncTable <> self) and  (AutoIncTable <> nil) then
        CalcNextPK(PrimaryKey) { DONE -c1.05 : Get PK value if current entity is ForeignEntity }
      ;
    end;
      if Assigned(BeforeInsert) then
        if not BeforeInsert(PrimaryKey) then
        begin
          Result := false;
          Exit;
        end;
      attr_pk := (PrimaryKey.isAutoInc) and (EntityConnection.Settings.AutoIncType <> aitAuxTable);
      _command := 'INSERT INTO '+TableName+'('+CommaText([ctoNames],attr_pk)
              +') VALUES ('+CommaText([ctoValues],attr_pk)+')';
    end
  else if NothingToDo then
    Exit
  Else
    begin
      _command := 'UPDATE '+TableName+' SET '+CommaText([ctoNames,ctoValues]);
      if (AutoIncTable <> self) then
        _command := _command +' WHERE '+PrimaryKey.Name+'='+PrimaryKey.SqlFormat;
    end;
  try
    EntityConnection.ExecuteCommand(_command,false);
    fState := estLoading;
    if isNewRecord and (PrimaryKey.isAutoInc) then
    begin
      if AutoIncTable = nil then
        PrimaryKey.Value := getPKValueFromDB()
      ;
      CheckRelations;
    end;
  except
    Result := false;
  end;
  SetReady();
  isNewRecord := False;
  f_IsEmpty := false;
end;

procedure TEntity.SetPrimaryKey(const _Value: TEntityField);
begin
  if FPrimaryKey <> nil then
    with FPrimaryKey do
    begin
      FieldAttributes := FieldAttributes - [faPrimaryKey];
    end; // if..with
  FPrimaryKey := _Value;
  with FPrimaryKey do
  begin
    FieldAttributes := FieldAttributes + [faPrimaryKey]
  end; // with
end;

function TEntity.CommaText(options: TCommaTextOptions; ignorePK: Boolean): string;
var
  i: Integer;
  item: TEntityField;
  hasFields, hasValues: Boolean;
  separator: string;
{### local function ###}
  function AppendField(const suffix: string): string;
  begin
    if hasFields then
      Result := separator
    else
      Result := '';
    Result := Result + item.Name + suffix;
    hasFields := true;
  end;
  function AppendValue: string;
  begin
    if hasValues then
      Result := separator
    else
      Result := '';
    Result := Result + item.SqlFormat;
    hasValues := (options = [ctoValues]);
  end;
{######################}
begin
  hasFields := false; hasValues := false;
  if options = [] then
    begin
      separator := ' AND ';
    end
  Else
    begin
      separator := ',';
    end; // if..else
  Result := '';
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    if (item = fPrimaryKey) and ignorePK then
      Continue;
    {}
    if ctoNames in options then
    begin
      if not (ctoValues in options) then
        Result := Result + AppendField('')
      else if not item.isModified then
        Continue
      else
        Result := Result+AppendField('=');  // name AND value
    end;
    if ctoValues in options then
    begin
      Result := Result+AppendValue();
    end;
    if options = [] then
    begin
      Result := Result+AppendField('=')+AppendValue();
    end;
  end; // for
end;

function TEntity.Copy: TEntity;
var
  i: Integer;
  item: TEntityField;
begin
  Result := TEntity.Create(self);
  for i := 0 to fItems.Count - 1 do
  begin
    item := TEntityField(fItems[i]);
    Result.AddField(item.Name,item.FieldType,false,item.Size);
  end;
  Result.TableName := fTableName;
end;

function TEntity.getPrimaryKey: TEntityField;
begin
  if fPrimaryKey <> nil then
    Result := fPrimaryKey
  else if fItems.Count = 0 then
    raise Exception.Create('TEntity - Primary Key missing!')
  else
    Result := TEntityField(fItems[0]);
end;

function TEntity.NothingToDo: Boolean;
var
  i: Integer;
  item: TEntityField;
begin
  Result := true;
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    if item.isModified then
    begin
      Result := False;
      Exit;
    end;
  end; // for
end;

procedure TEntity.SetTableName(const _Value: string);
begin
  fTableName := _Value;
end;

function TEntity.getTableName: string;
begin
  if Trim(fTableName) = '' then
    raise Exception.Create('TEntity - TableName empty');
  Result := fTableName;
end;

function TEntity.indexOfCountField: integer;
var
  item: TEntityField;
begin
  for Result := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[Result]);
    if item.Builder is TCountBuilder then
      Exit;
  end; // for
  Result := -1;
end;

function TEntity.selectCommand: string;
const
  statment: array[TSqlElement] of string = (
            'SELECT ','FROM ','WHERE ','GROUP BY ','ORDER BY '
  );
var
  E: TSqlArray;
  i: TSqlElement;
begin
  for i := Low(E) to High(E) do
    E[i] := ''
  ;
  FillSqlElements(E);  { DONE -c0.81 : SELECT .. JOIN ... }
  if E[se_SELECT] = '' then
    E[se_SELECT] := '*'
  ;
  E[se_WHERE] := E[se_WHERE] + ConditionList.Expression;
  Result := '';
  for i := Low(E) to High(E) do
  begin
    if (E[i] <> '') then
      Result := Result + statment[i] + E[i] + LINE_BREAK;
  end; // for
end;

procedure TEntity.LoadFromTable;
begin
  CheckConnection();
  if (EntityConnection.Settings.JoinTables) and (fRelations <> nil) then
  begin
    Exit;
  end;
  fState := estLoading;
  needReload := False;
  DataSet := EntityConnection.ExecuteCommand(selectCommand,true);
  f_IsEmpty := DataSet.IsEmpty;
  while not DataSet.Eof do
  begin
    LoadRecord;
    if not Assigned(OnList) then
      Break
    else
    begin
      if not OnList(PrimaryKey) then
        Break;
    end;
    DataSet.Next;
  end; // while
  if not keepDataSet then DataSet := nil;
  SetReady();
end;

function TEntity.IndexOf(const Search: string): integer;
var
  item: TEntityField;
begin
  for Result := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[Result]);
    if SameText(item.Name,Search) then Exit;
  end; // for
  Result := -1;
end;

procedure TEntity.SetDataSet(const Value: TDataSet);
begin
  if (f_DataSet <> nil) then
    f_DataSet.Free
  ;
  f_DataSet := Value;
  if f_DataSource <> nil then
    f_DataSource.DataSet := f_DataSet;
  { DONE -c1.28 : Datasource support }
  {$ifdef DB_EXPRESS_LIB}
  if objProvider <> nil then
    objProvider.DataSet := f_DataSet
  ;
  {$ENDIF}
end;

procedure TEntity.setDataSource(ADataSource: TDataSource);
begin
  keepDataSet := true;
  {$ifdef DB_EXPRESS_LIB}
  if EntityConnection.Settings.UsingDbExpress then
//  if EntityConnection is TEntityConnectionDbExpress then
  begin
    objProvider := TDataSetProvider.Create(ADataSource.Owner);
    ProviderName := self.ClassName;
    if ADataSource.DataSet is TClientDataSet then
      TClientDataSet(ADataSource.DataSet).ProviderName := self.ProviderName
    ;
    Exit;
  end;
  {$ENDIF}
  f_DataSource := ADataSource;
end;

function TEntity.CalcNextPK(Sender: TEntityField): Boolean;
var
  AutoIncTable: TEntity;
  fieldName: string;
  _Value: integer;
  _field: TEntityField;
begin
  AutoIncTable := EntityConnection.Settings.AuxTable;
  fieldName := Sender.Parent.TableName;
  _field := AutoIncTable.getItem(AutoIncTable.IndexOf(fieldName));
  if _field = nil then
  begin
    if AutoIncTable.EntityConnection = nil then
      AutoIncTable.EntityConnection := Sender.Parent.EntityConnection
    ;
    _field := AutoIncTable.AddField(TableName, Sender.FieldType, false, Sender.Size);
    AutoIncTable.needReload := true;
  end;
  if AutoIncTable.needReload then
      TEntity.From(
        AutoIncTable
      )
      .Select([])
      ;
  _Value := _field.AsInteger + 1;
  if AutoIncTable.IsEmpty then
    begin
      AutoIncTable.NewRecord;
    end
  else
    begin
      AutoIncTable.Edit(false);
    end;
  Sender.Value := _Value;
  _field.Value := _Value;
  Result := AutoIncTable.SaveChanges;
end;

procedure TEntity.RunDatabase(tables: TStrings = nil);
var
  AutoIncTable: TEntity;
  i: Integer;
  item: TEntityField;
  _command: string;
  firstTime,alterTable: Boolean;
  FieldList: TStringList;
begin
  CheckConnection();
  AutoIncTable := EntityConnection.Settings.AuxTable;
  if (AutoIncTable <> self) and (AutoIncTable <> nil) and (PrimaryKey.isAutoInc) then
  begin
    if AutoIncTable.EntityConnection = nil then
      AutoIncTable.EntityConnection := self.EntityConnection
    ;
    if AutoIncTable.IndexOf(TableName) > -1 then
      Exit;
    AutoIncTable.AddField(TableName, PrimaryKey.FieldType, false, PrimaryKey.Size);
    AutoIncTable.RunDatabase(tables);
  end;
  if tables = nil then
    begin
      tables := EntityConnection.GetList(claTables);
      firstTime := true;
    end
  Else
    firstTime := false;
  {}
  if tables.IndexOf(self.TableName) = -1 then
    begin
      _command := 'CREATE TABLE '+TableName+'(';
      tables.Add(TableName);
      fCreated := true;  { DONE -c1.04 : identify tables that were recently created }
      FieldList := nil;
    end
  Else
    begin
      {$ifdef FPC}
        FieldList := EntityConnection.GetList(claFields,TableName);
      {$else}
        FieldList := EntityConnection.GetList(
                  claFields,
                  Format(
                  'SELECT * FROM %s WHERE %s = NULL %s',[
                    TableName,
                    PrimaryKey.Name,
                    COMMENT_TO_NOT_SAVE
                  ])
        );
      {$endif}
    end; // if..else
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    alterTable := false;
    if (FieldList = nil) then
      begin
        if i > 0 then
          _command := _command + ','
        ;
        _command := _command + LINE_BREAK+TAB_CHAR;
      end
    Else if FieldList.IndexOf(item.Name) = -1 then
      begin
        _command := 'ALTER TABLE '+TableName+' ADD ';
        alterTable := true;
      end
    Else
      begin
        if item.ForeignEntity <> nil then
          item.ForeignEntity.RunDatabase(tables);
        Continue;
      end; // if..else..else
    _command := _command + item.Name+' '+item.Attribute;
    if (item.ForeignEntity <> nil) and (AutoIncType <> aitPostgre) then
    begin
      _command := _command + ' REFERENCES '+item.ForeignEntity.TableName+'('
                + item.ForeignEntity.PrimaryKey.Name+')';
      item.ForeignEntity.RunDatabase(tables);
    end;
    if alterTable then
    begin
      EntityConnection.ExecuteCommand(_command,false);
      _command := '';
    end;
  end; // for
  if firstTime then
  begin
    {-----------------}
    if (FieldList <> nil) then
      EntityConnection.GetList(claClear);
    tables.Free;
    {-----------------}
  end;
  if _command <> '' then
  begin
    _command := _command + ')';
    EntityConnection.ExecuteCommand(_command,false);
  end;
end;

procedure TEntity.AddRelation(field: TEntityField);
begin
  if fRelations = nil then
    fRelations := TList.Create;
  fRelations.Add(field);
end;

procedure TEntity.Assign(Source: TEntity);
var
  i: Integer;
  f1,f2: TEntityField;
begin
  for i := 0 to fItems.Count - 1 do
  begin
    f2 := TEntityField(fItems[i]);
    f1 := Source.getItem( Source.IndexOf(f2.Name) );
    if f1 = nil then Continue;
    f2.Value := f1.Value;
  end; // for
  CheckRelations;
  needReload := false;
end;

procedure TEntity.Delete;
begin
  LoadRecord();
  CheckConnection();
  InternalDelete;
end;

function TEntity.ValuesOf(conditions: array of TConditionItem): TStringList;
var
  hasData: Boolean;
begin
  Result := TStringList.Create;
  Result.Sorted := true;
  Result.Duplicates := dupIgnore;
  {---}
  hasData := Find(conditions,[PrimaryKey]);
  while hasData do
  begin
    Result.Add(PrimaryKey.AsString);
    hasData := More;
  end; // while
  {---}
end;

procedure TEntity.CheckRelations;
var
  i: Integer;
  _item: TEntityField;
begin
  if (fRelations <> nil) then
    for i := 0 to fRelations.Count - 1 do
    begin
      _item := TEntityField(fRelations[i]);
      _item.Value := PrimaryKey.Value;
    end;  // for
end;

procedure TEntity.Delete(conditions: array of TConditionItem);
var
  ToDelete: TStringList;
begin
  { DONE -c1.4 : Conditions for DELETE }
  ToDelete := ValuesOf(conditions);
  while ToDelete.Count > 0 do
  begin
    PrimaryKey.Value := StrToIntDef(ToDelete[0],0);
    InternalDelete();
    ToDelete.Delete(0);
  end; // while
  ToDelete.Free;
end;

procedure TEntity.InternalDelete(_field: TEntityField = nil);
var
  i: Integer;
  item: TEntityField;
  _command: string;
begin
  if _field = nil then
    _field := PrimaryKey
  ;
  if fRelations <> nil then
    for i := 0 to fRelations.Count-1 do
    begin
      item := TEntityField(fRelations[i]);
      item.Value := _field.Value;
      item.Parent.InternalDelete(item);
    end; // for
  _command := 'DELETE FROM '+TableName+' WHERE '+_field.Name+'='+_field.SqlFormat;
  EntityConnection.ExecuteCommand(_command,false);
end;

function TEntity.FieldForEntity(Search: TEntity): string;
var
  i: Integer;
  item: TEntityField;
begin
  for i := 0 to fItems.Count - 1 do
  begin
    item := TEntityField(fItems[i]);
    if (item.ForeignEntity <> nil)
    and (item.ForeignEntity.TableName = Search.TableName) then
    begin
      Result := item.Name;
      Exit;
    end;
  end; // for
  Result := '';
end;

procedure TEntity.FillSqlElements(var Elements: TSqlArray; JoinCondition: TConditionItem = nil);
var
  i: Integer;
  item: TEntityField;
  ForeignConditions: TConditionList;
  JoinEntity: TEntity;
  procedure AddStr(var str: string; const Value: string; const comma_separator: Boolean = true);
  BEGIN
    if (str <> '') and comma_separator then
      str := str + ','
    ;
    str := str + LINE_BREAK+TAB_CHAR+ Value;
  END;
begin
//  {$define JOIN_AS_WHERE}
  if JoinCondition <> nil then
    begin
      AddStr(
        Elements[se_FROM],  { DONE -c1.30 : LEFT JOIN }
        'LEFT JOIN '+TableName+' ON ('+JoinCondition.Expression+')',
        false
      );
    end
  Else
    begin
      AddStr(Elements[se_FROM],self.TableName);
    end;
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    JoinEntity := nil;
    if (item.ForeignEntity <> nil) and not item.ignore then
      JoinEntity := item.ForeignEntity
    ;
    if item.Grouped then
      begin
        if item.Parent <> self then
          JoinEntity := item.Parent;
        if EntityConnection.Settings.JoinTables then
          AddStr(Elements[se_GROUPBY],item.FullName)
        else
          AddStr(Elements[se_GROUPBY],item.Name);
      end
    else if ConditionList.Grouped and not (item.Builder is TAggregateBuilder) then
      item.ignore := true
    ;
    if item.Sorted then
      AddStr(Elements[se_ORDERBY],item.Name)
    ;
    if (JoinEntity <> nil)  and (EntityConnection.Settings.JoinTables) then
      begin
        {$ifdef JOIN_AS_WHERE}
        ForeignConditions := fConditionList;
        {$else}
        ForeignConditions := TConditionList.Create(nil);
        {$endif}
        JoinCondition := TConditionItem.Create;
        with JoinCondition  do
        begin
          FieldName := self.TableName+'.'+item.Name;
          separator := ' = ';
          ConditionValue := JoinEntity.TableName
                         +'.'+JoinEntity.PrimaryKey.Name;
        end; // with
        ForeignConditions.Add(JoinCondition);
        {$ifdef JOIN_AS_WHERE}
        JoinEntity.FillSqlElements(Elements);
        {$else}
        JoinEntity.FillSqlElements(Elements,JoinCondition);
        ForeignConditions.Free;
        {$endif}
      end
    Else if not item.ignore then         
      begin
        AddStr(
          Elements[se_SELECT],
          item.SqlFunction()
        );
      end; // if..else
  end; // for i
end;

function TEntity.Find(conditions: array of TConditionItem;
  FieldList: array of TEntityField): Boolean;
begin
  keepDataSet := true;
  Where(
    conditions
  ).Select(
    FieldList
  );
  Result := not IsEmpty;
end;

procedure TEntity.LoadRecord(Source: TDataSet);
var
  i: Integer;
  item: TEntityField;
  dField: TField;
begin
  fState := estLoading;
  if Source = nil then
  begin
    Source := self.DataSet;
  end;
  {}
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    if (item.ForeignEntity <> nil) and (EntityConnection.Settings.JoinTables) then
    begin
      item.ForeignEntity.LoadRecord(Source);
    end;
    if item.Builder is TCountBuilder then
    begin
      dField := Source.FindField('countValue');
      if dField <> nil then
        item.countValue := dField.AsInteger;  { DONE -c1.35 : Count Value -- for Count(*) function }
    end;
    dField := item.DataSetField(Source);
    if dField = nil then
      Continue
    ;
    if dField.IsNull then
      item.Clear  { DONE -c1.38 : Null fields }
    else
      item.Value := dField.AsVariant
    ;
    item.isModified := False;
  end; // for
  needReload := False;
end;

function TEntity.Where(conditions: array of TConditionItem): TConditionList;
begin
  Result := fConditionList;
  Result.Clear;
  Result.Where(conditions);
  { DONE -c0.97 : combinations of conditions such as
                          (x AND y) OR (z AND w)
                          (x OR y) AND (z OR w) ... }
  SetReady();
end;

procedure TEntity.Cancel;
begin
  SetReady();
  isNewRecord := false;
end;

function TEntity.Find(const Search: Variant; FieldList: array of TEntityField): Boolean;
begin
  Result := Find([PrimaryKey.Equals(Search)],FieldList);
end;

procedure TEntity.Assign(obj: TPersistent; const keyField: string);
var
  PropCount, i: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  propType: TFieldType;
  _field: TEntityField;
  function StrToFieldType(const Str: string): TFieldType;
  BEGIN
    if SameText(Str,'integer') then
      Result := ftInteger
    else if SameText(Str,'double') then
      Result := ftFloat
    else if SameText(Str,'TDate') then
      Result := ftDate
    else if SameText(Str,'Boolean') then
      Result := ftBoolean
    else if SameText(Str,'String') then
      Result := ftString
    else
      Result := ftUnknown;
  END;
begin
  PropList := nil;
  if TableName = '' then TableName := obj.ClassName;
  { DONE -c1.03 : Run-time type information (RTTI) }
  try
    PropCount:= GetTypeData(obj.ClassInfo)^.PropCount;
    GetMem(PropList, PropCount * SizeOf(Pointer));
    GetPropInfos(obj.ClassInfo, PropList);
    for i := 0 to PropCount-1 do
    begin
      PropInfo := PropList^[i];
      propType := StrToFieldType(PropInfo^.PropType^.Name);
      _field := AddField(PropInfo^.Name,propType,false,30);
      if SameText(_field.Name,keyField) then
      begin
        PrimaryKey := _field;
      end;
      _field.Value := GetPropValue(obj, PropInfo^.Name);
    end; // for
  finally
    FreeMem(PropList);
  end;
  needReload := false;
end;

procedure TEntity.SetReady(recursive: Boolean = false);
var
  i: Integer;
  item: TEntityField;
begin
  fState := estReady;
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    if recursive and (item.ForeignEntity <> nil) then
    begin
      item.ForeignEntity.SetReady();
      item.ForeignEntity.ConditionList.Clear;
    end;
    item.ignore := false;
    if item.Builder is TAggregateBuilder then
      item.Builder := nil
    ;    
    item.QueryAttributes := [];
  end; // for
  if (EntityConnection <> nil) and (EntityConnection.Settings <> nil) then
    with EntityConnection.Settings do
    begin
      JoinState := jsENABLED;
    end
  ;
end;

function TEntity.enableFields(FieldList: array of TEntityField; checkJoin: Boolean = true): Boolean;
var
  i,j, ForeignCount: Integer;
  item: TEntityField;
  arrayEmpty: Boolean;
  function FindInArray: Boolean;
  BEGIN
    Result := false;
    j := Low(FieldList);
    while (not Result) and (j <= High((FieldList))) do
    begin
      Result := (item = FieldList[j]);
      Inc(j);
    end; // while
  END;
begin
  arrayEmpty := (Length(FieldList) = 0);
  ForeignCount := 0;
  Result := false;
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    if (not arrayEmpty) and (not FindInArray) then
      item.ignore := true
    else
      begin
        item.ignore := false;
        Result := true;
      end;
    if (item.ForeignEntity <> nil)
    and (item.ForeignEntity.enableFields(FieldList,false) ) then
    begin
      item.ignore := false;
      Result := true;
      Inc(ForeignCount);
    end;
  end; // for
  if checkJoin then
    with EntityConnection.Settings do
    begin
      if ForeignCount = 0 then
        JoinState := jsDISABLED
      else
        JoinState := jsENABLED;
    end; // if..with
end;

procedure TEntity.Edit(load_field: Boolean = true);
begin
  fState := estEditing;
  isNewRecord := false;
  if load_field then LoadRecord();
end;

function TConditionList.InTo(Target: TEntity): TConditionList;
var
  lookupField: string;
  cond_item: TConditionItem;
begin
  lookupField := Target.FieldForEntity(fEntity);
  Result := Target.ConditionList;
  if lookupField = '' then Exit;
  { DONE -c1.09 : Filter by ForeignEntity }
  cond_item := TConditionItem.Create;
  cond_item.FieldName := lookupField;
  cond_item.separator := ' IN ';
  LINE_BREAK := ' '; TAB_CHAR := '';
  fEntity.enableFields([fEntity.PrimaryKey]);
  cond_item.ConditionValue := '('+fEntity.selectCommand+')';
  LINE_BREAK := #13#10; TAB_CHAR := #9;
  {-- Example: ---

   with Customer,Order do
     .Where([
        Country.Equals("BR")
     ])
     .Into(
        Order
     ).And(
        Order.Total.Greater(1500)
     ).Select(
        [customer_id,Total,SaleDate]
   );
   --- Result: ---
     SELECT customer_id,Total,SaleDate
     FROM Order WHERE customer_id IN
     (SELECT id FROM Customer WHERE Country = "BR")
     AND Total > 1500
  ----------------}
  Result.Add(cond_item);
end;

function TConditionList.isValid: Boolean;
begin
  Result := (fList.Count > 0);
end;

function TConditionList.OrderBy(
  FieldList: array of TEntityField): TConditionList;
var
  i: Integer;
  _item: TEntityField;
begin
  for i := Low(FieldList) to High(FieldList) do
  begin
    _item := FieldList[i];
    _item.QueryAttributes := _item.QueryAttributes + [qa_ORDER];
  end; // for
  Result := self;
end;

procedure TEntity.CheckConnection;
begin
  if EntityConnection = nil then
    raise Exception.Create('EntityConnection missing for '+TableName);
end;


function TEntity.getCount: integer;
begin
  Result := fItems.Count;
end;

procedure TEntity.SetEntityConnection(const Value: IEntityConnection);
var
  i: Integer;
  item: TEntityField;
begin
  fEntityConnection := Value;
  if (Value = nil) then Exit;
  {}
  for i := 0 to fItems.Count-1 do
  begin
    item := TEntityField(fItems[i]);
    if (item.ForeignEntity <> nil)
    and (item.ForeignEntity.EntityConnection = nil) then
       item.ForeignEntity.EntityConnection := fEntityConnection;
  end; // for
  if (fEntityConnection.Settings.AutoIncType = aitNone) then
    fEntityConnection.Settings.AutoIncType := self.AutoIncType;
end;

destructor TCustomEntityConnection.Destroy;
begin
  Connection.Free;
  f_Settings.Free;
  inherited;
end;

function TCustomEntityConnection.ConnectionType: TEntityConnectionType;
begin
  Result := ect_Normal;
end;

{ TEntityField }

function TEntityField.AsDate: TDateTime;
begin
  {$ifdef VER130}
  Result := VarToDateTime(Value);
  {$else}
  Result := Value;
  {$endif}
end;

function TEntityField.AsFloat: Double;
{$ifdef VER130}
var
  var2: Variant;
begin
  VarCast(var2,Value, varDouble);
  Result := var2;
{$else}
begin
  Result := Value;
{$endif}
end;

function TEntityField.AsInteger: Integer;
{$ifdef VER130}
var
  var2: Variant;
begin
  VarCast(var2,Value, varInteger);
  Result := var2;
{$else}
begin
  Result := Value;
{$endif}
end;

function TEntityField.AsString: string;
begin
{$ifdef VER130}
  Result := VarToStr(Value);
{$else}
  Result := Value;
{$endif}
end;

function TEntityField.Avg: TEntityField;
begin
  Result := self;
  Builder := TAvgBuilder.Create(Result);
end;

constructor TEntityField.Create(_Parent: TEntity);
begin
  fParent := _Parent;
end;

function TEntityField.DataSetField(DataSet: TDataSet): TField;
var
  Names: TStringList;
  L: Integer;
begin
  Result := nil;
  if DataSet = nil then
  begin
    if Parent.DataSet = nil then Exit;
    DataSet := Parent.DataSet;
  end;
  Names := TStringList.Create;
  if Parent.EntityConnection.Settings.JoinTables then
    Names.Add(FullName());
  Names.Add(Name);
  L := Names.Count-1;
  while (Result = nil) and (L >= 0) do
  begin
    Result := DataSet.FindField(Names[L]);
    Dec(L);
  end;
  Names.Free;
end;

destructor TEntityField.Destroy;
begin
  Builder := nil;
  inherited;
end;

function TEntity.getPKValueFromDB: Variant;
var
  str: string;
begin
  str := 'SELECT '+PrimaryKey.Name
   +' FROM '+TableName+' WHERE '+CommaText([],true);
  { DONE -c0.99 : Auto_inc values }   { DONE -c1.05 : Dont use MAX to get last/current record}
  with EntityConnection.ExecuteCommand(str,true) do
  begin
    Result := Fields[0].AsVariant;
    Free;
  end; // with
end;

function TEntityField.getValue: Variant;
begin
  if fParent.needReload and (fParent.State = estReady) then
     fParent.LoadFromTable;
  Result := FValue;
end;

procedure TEntityField.SetFieldType(const _Value: TFieldType);
begin
  FFieldType := _Value;
end;

procedure TEntityField.SetForeignEntity(const _Value: TEntity);
begin
  fForeignEntity := _Value;
  if fForeignEntity <> nil then
    fForeignEntity.AddRelation(self);
  if (fParent.EntityConnection <> nil)
  and (fForeignEntity.EntityConnection = nil) then
  begin
    fForeignEntity.EntityConnection := fParent.EntityConnection;
  end;
end;

procedure TEntityField.SetSize(const _Value: integer);
begin
  FSize := _Value;
end;

function isAutoIncType(const Str: string; out _autoIncType: TAutoIncrementType): Boolean;
const
  AutoIncExpr: array[TAutoIncrementType] of string = ('','AUTO_INCREMENT','IDENTITY(','SERIAL','');
var
  i: TAutoIncrementType;
begin
  for i := aitMySql to aitPostgre do
  begin
    if Pos(AutoIncExpr[i],Str) > 0 then
    begin
      Result := true;
      _autoIncType := i;
      Exit;
    end;
  end; // for
  Result := false;
end;

procedure TEntityField.SetAttribute(_Value: string);
const
  AttrID: array[TFieldAttribute] of string = (
  'NOT NULL','','UNIQUE','PRIMARY KEY'
  );
var
  i: TFieldAttribute;
  _AutoIncType: TAutoIncrementType;
begin
  _Value := UpperCase(_Value);
  for i := Low(TFieldAttribute) to High(TFieldAttribute)  do
  begin
    if i <> faAutoIncrement then
      begin
        if Pos(AttrID[i],_Value) = 0 then Continue;
        if i = faPrimarykey then fParent.PrimaryKey := self;
      end
    Else if isAutoIncType(_Value,_AutoIncType) then
      begin
        if (fParent.EntityConnection = nil) then
          fParent.AutoIncType := _AutoIncType
        else
          fParent.EntityConnection.Settings.AutoIncType := _AutoIncType;
      end
    Else
      begin
        Continue;
      end; // if..else
    FieldAttributes := FieldAttributes + [i];
  end; // for
end;

function TEntityField.getAttribute: string;
const
  AttrID: array[TFieldAttribute] of string = (
  ' NOT NULL','',' UNIQUE',' PRIMARY KEY'
  );
var
  i: TFieldAttribute;
begin
  case FieldType of
    ftString,ftFixedChar:
      begin
        if Size < 1 then
          Size := 50;
        if fieldtype = ftFixedChar then
          Result := 'Char('
        else
          Result := 'varChar(';
        Result := Result+IntToStr(Size)+')';
      end;
    ftInteger,ftAutoInc:
      Result := 'integer';
    ftFloat:
      Result := 'float';
    ftDate:
      Result := 'date';
    ftBoolean:
      Result := 'char(1)';
    else
      Result := '';
  end; // case
  for i := Low(TFieldAttribute) to High(TFieldAttribute)  do
    if i in FieldAttributes then
    begin
      if i <> faAutoIncrement then
        begin
          Result := Result + AttrID[i];
        end
      Else if (fParent.EntityConnection <> nil) then
        case fParent.EntityConnection.Settings.AutoIncType of
          aitMySql:
            Result := Result + ' AUTO_INCREMENT';
          aitPostgre:  {DONE -c1.25a : Auto increment for PostgreSql }
            begin
              Result := ' SERIAL';
              Exit;
            end;
          aitSqlServer:
            Result := Result + ' IDENTITY(1,1)';
        end; // else..case
    end; // for..if
end;

procedure TEntityField.SetAutoInc(_Value: Boolean);
begin
  if _Value then
    begin
      FieldAttributes := FieldAttributes + [faAutoIncrement];
    end
  Else
    begin
      FieldAttributes := FieldAttributes - [faAutoIncrement];
    end; // if..else
end;

procedure TEntityField.setBuilder(const Value: TSqlFieldBuilder);
begin
  if fBuilder <> nil then
    fBuilder.Free
  ;
  fBuilder := Value;
end;

function TEntityField.getAutoInc: Boolean;
begin
  Result := ( faAutoIncrement in FieldAttributes );
end;

function TEntityField.getBuilder: TSqlFieldBuilder;
begin
  if fBuilder = nil then
    fBuilder := TSqlFieldBuilder.Create(self)
  ;
  Result := fBuilder;
end;

function TEntityField.getGrouped: Boolean;
begin
  Result := (qa_GROUP in QueryAttributes);
end;

function TEntityField.getSorted: Boolean;
begin
  Result := (qa_ORDER in QueryAttributes);
end;

procedure TEntityField.SetName(const _Value: string);
begin
  FName := _Value;
end;

procedure TEntityField.SetValue(const _Value: Variant);
begin
  {$ifdef VER130}
  if not VarIsNull(FValue) then
  begin
    if VarToStr(FValue) = VarToStr(_Value) then
      Exit;
  end;
  {$else}
  if FValue <> Unassigned then
  begin
    if FValue = _Value then
      Exit;
  end;
  {$endif}
  FValue := _Value;
  if ignore then Exit; { DONE -c1.07 : Dont perform validations on ignored fields }
  //--------------------
  if ForeignEntity <> nil then
    begin
      ForeignEntity.PrimaryKey.Equals(_Value);
      ForeignEntity.SetReady();
    end
  Else if (fParent.PrimaryKey = self) and (fParent.State <> estEditing) then
    begin
      fParent.needReload := (fParent.State = estReady);
    end;
  //--------------------
  isModified := True;
end;

function TEntityField.SqlFormat: string;
var
  p: integer;
begin
  case FieldType of
    ftString, ftDate:
      begin
        if FieldType = ftDate then
          Result := FormatDateTime(fParent.EntityConnection.Settings.DateFormat,AsDate)
        else
          Result := AsString;
        Result := QuotedStr(Result);
      end;
    ftFloat:
      begin
        Result := FormatFloat(fParent.EntityConnection.Settings.FloatFormat,AsFloat);
        p := Pos(',',Result);
        if p > 0 then Result[p] := '.';
      end;
    ftInteger:
      Result := IntToStr(AsInteger);
  end; // case
end;

function TEntityField.SqlFunction: string;
begin
  Result := Builder.Text;
end;

function TEntityField.Sum: TEntityField;
begin
  Result := self;
  Builder := TSumBuilder.Create(Result);
end;

function TEntityField.Contains(Search: Variant): TConditionItem;
begin
  fValue := Search;
  Result := NewCondition(scContains);
end;

function TEntityField.Count: TEntityField;
begin
  Result := self;
  if Parent.indexOfCountField > -1 then
    raise Exception.Create(Parent.TableName+' already has a Count function'
  );
  Builder := TCountBuilder.Create(Result);
end;

function TEntityField.Equals(Search: Variant): TConditionItem;
begin
  fValue := Search;
  Result := NewCondition(scEquals);
end;

function TEntityField.FullName: string;
begin
  Result := fParent.TableName+'_'+self.Name;
end;

function TEntityField.Greater(Search: Variant): TConditionItem;
begin
  fValue := Search;
  Result := NewCondition(scGreater);
end;

function TEntityField.InList(List: TStrings): TConditionItem;
var
  ListValues: string;
  i: Integer;
begin
  { DONE -c1.4 : SELECT IN (value1, value2...valueN) }
  Result := TConditionItem.Create;
  Result.FieldName := self.Name;
  Result.separator := ' IN ';
  ListValues := '';
  for i := 0 to List.Count - 1 do
  begin
    if ListValues <> '' then
      ListValues := ListValues + ','
    ;
    if FieldType in [ftString,ftDate] then
      ListValues := ListValues + QuotedStr(List[i])
    else
      ListValues := ListValues + List[i]
    ;
  end; // for
  Result.ConditionValue := '('+ListValues+')';
end;

function TEntityField.Less(Search: Variant): TConditionItem;
begin
  fValue := Search;
  Result := NewCondition(scLess);
end;

function TEntityField.Max: TEntityField;
begin
  Result := self;
  Builder := TMaxBuilder.Create(Result);
end;

function TEntityField.Min: TEntityField;
begin
  Result := self;
  Builder := TMinBuilder.Create(Result);
end;

function TEntityField.NewCondition(SearchCompare: TSearchCompare): TConditionItem;
const
  comparisons: array[TSearchCompare] of string = (
     ' > ',    // scGreater
     ' < ',    // scLess
     ' LIKE ', // scContains
     ' = '     // scEquals
  );
var
  alias: string;
begin
  fParent.Edit(false);
  Result := TConditionItem.Create;
  { TODO -c1.46 : restructuring of the conditions mechanism }
  Result.separator := comparisons[SearchCompare];
  if fParent.EntityConnection.Settings.JoinTables then
    alias := Parent.TableName+'.'
  else
    alias := '';
  Result.FieldName := alias+self.Name;
  if SearchCompare = scContains then
    with fParent.EntityConnection.Settings do
    begin
      fValue := StartLikeExpr+fValue+EndLikeExpr;
    end; // with
  Result.ConditionValue := self.SqlFormat;
  fParent.needReload := true;
end;

{$ifdef OLD_VERSION}

constructor TEntityConnectionADO.Create(const _ConnectionString: string);
begin
  Connection := TADOConnection.Create(nil);
  with TADOConnection(Connection) do
  begin
    LoginPrompt := false;
    ConnectionString := _ConnectionString;
  end; // with
end;

function TEntityConnectionADO.ExecuteCommand(const _command: string;
  returnData: Boolean): TDataSet;
begin
  Result := TADOQuery.Create(nil);
  with TADOQuery(Result) do
  begin
    AutoCalcFields := False;
    CursorType := ctStatic;
    LockType   := ltReadOnly;
    EnableBCD  := False;
    ParamCheck := False;
    Connection := TADOConnection(self.Connection);
    MaxRecords := 200;
    SQL.Text := _command;
    {$ifdef ENTITY_LOG_SQL}
    SaveLog(Sql);
    {$endif}
    if returnData then
      Open
    else
      begin
        ExecSQL;
        Result.Free
      end;
  end; //  with
end;

function TEntityConnectionADO.GetList(Action: TConListAction;
  const Params: string): TStringList;
begin
  Result := nil;
  case Action of
    claTables:
      begin
        Result := TStringList.Create;
        TADOConnection(Connection).GetTableNames(Result);
      end;
    claFields,claClear:
      Result := inherited GetList(Action,Params);
  end; // case
end;


{$endif}

function TEntity.Clone: TEntity;
begin
  Result := TEntity(ClassType.NewInstance).Create(self);
end;


{ TConditionList }

procedure TConditionList.Add(_item: TConditionItem);
begin
  fList.Add(_item);
  _item.Owner := self;
end;

constructor TConditionList.Create(AEntity: TEntity);
begin
  fList := TList.Create;
  fEntity := AEntity;
end;


destructor TConditionList.Destroy;
begin
  Clear;
  fList.Free;
  inherited;
end;

function TConditionList.Expression: string;
var
  i,validConditions: integer;
  cond_item: TConditionItem;
begin
  Result := prefix;
  validConditions := 0;
  for i := 0 to fList.Count-1 do
  begin
    cond_item := getItem(i);
    if not cond_item.isValid then
      Continue
    ;
    if validConditions > 0 then
      Result := Result + getItem(i-1).LOName;
    Result := Result +LINE_BREAK+TAB_CHAR+ cond_item.Expression;
    Inc(validConditions);
  end; // for
  Result := Result + suffix;
end;

function TConditionList.getCount: integer;
begin
  Result := fList.Count;
end;

function TConditionList.getItem(Index: integer): TConditionItem;
begin
  Result := TConditionItem(fList[Index]);
end;

function TConditionList.GroupBy(
  FieldList: array of TEntityField): TConditionList;
var
  i: Integer;
  _item: TEntityField;
begin
  for i := Low(FieldList) to High(FieldList) do
  begin
    _item := FieldList[i];
    _item.QueryAttributes := _item.QueryAttributes + [qa_GROUP];
  end; // for
  fGrouped := true;
  Result := self;
end;

function TConditionList._AND(conditions: array of TConditionItem): TConditionList;
begin
  TConditionItem(fList.Last).LogicalOperator := _AND_;
  Result := Where(conditions);
end;

function TConditionList.Where(conditions: array of TConditionItem): TConditionList;
var
  i: integer;
begin
  Result := TConditionList.Create(fEntity);
  if Length(conditions) = 0 then Exit
  ;
  Result.prefix := '(';
  Result.suffix := ')';
  for i := Low(conditions) to High(conditions) do
  begin
    Result.Add(conditions[i]);
  end; // for
  self.Add(Result);
end;

function TConditionList._OR(conditions: array of TConditionItem): TConditionList;
begin
  TConditionItem(fList.Last).LogicalOperator := _OR_;
  Result := Where(conditions);
end;

procedure TConditionList.Clear;
var
  condition: TConditionItem;
begin
  while fList.Count > 0 do
  begin
    condition := getItem(0);
    condition.Free;
    fList.Delete(0);
  end; // while
  fGrouped := false;
end;

function TConditionList.Remove(_item: TConditionItem): Boolean;
var
  p: integer;
begin
  p := fList.IndexOf(_item);
  Result := (p > -1);
  if Result then
  begin
    fList.Delete(p);
  end;
end;

procedure TConditionList.Select(FieldList: array of TEntityField);
begin
  if fEntity = nil then Exit
  ;
  fEntity.enableFields(FieldList);
  fEntity.LoadFromTable;
  fEntity.CheckRelations;
end;

{ TConditionItem }


function TConditionItem.Expression: string;
begin
  Result := FieldName + separator + ConditionValue
end;

function TConditionItem.isValid: Boolean;
begin
  Result := (ConditionValue <> '');
end;

function TConditionItem.LOName: string;
const
  LONames: array[TLogicalOperator] of string = (' AND ',' OR ');
begin
  Result := LONames[self.LogicalOperator];
end;


procedure TConditionItem.SetOwner(const Value: TConditionList);
begin
  if fOwner <> nil then
  begin
    fOwner.Remove(self);
  end;
  fOwner := Value;
end;

function TConditionItem._OR: TConditionItem;
begin
  LogicalOperator := _OR_;
  Result := self;
end;

procedure TEntity.List(item: TEntityField; Event: TEntityEvent);
begin
  { DONE -c1.22 : List only one field }
  repeat
    if not Event(item) then Break;
  until (not More);
  keepDataSet := false;
  DataSet := nil;
end;

class function TEntity.From(Target: TEntity): TConditionList;
begin
  Target.keepDataSet := true;
  Result := Target.ConditionList;
end;

function TEntity.MaxPKValue: integer;
var
  _command: string;
begin
  _command := 'SELECT Max('+PrimaryKey.Name+') FROM '+TableName;
  with EntityConnection.ExecuteCommand(_command,true) do
  begin
    Result := Fields[0].AsInteger;
    Free;
  end; // with
end;

function TEntity.More: Boolean;
begin
  { DONE -c1.38 : Loading next record from database }
  if DataSet = nil then
  begin
    Result := false;
    Exit;
  end;
  DataSet.Next;
  Result := not DataSet.Eof;
  if Result then
    LoadRecord;
end;

{$ifdef INTERBASE_XE}
constructor TEntityConnectionInterbase.Create(const _ConnectionString: string);
var
  LTemp: TStringList;
begin
  Transaction := TIBTransaction.Create(nil);
  Database := TIBDatabase.Create(nil);
  Database.DefaultTransaction := Transaction;
  LTemp := TStringList.Create;
  LTemp.CommaText := _ConnectionString;
  if LTemp.Values['Host'] <> '' then
    Database.DatabaseName := LTemp.Values['Host']+':'+LTemp.Values['Database']
  else
    Database.DatabaseName := LTemp.Values['Database']
  ;
  Database.Params.Values['user_name'] := LTemp.Values['User'];
  Database.Params.Values['password']  := LTemp.Values['Password'];
  Database.LoginPrompt := False;
  Database.ServerType := 'IBServer';
  LTemp.Free;
end;

function TEntityConnectionInterbase.ExecuteCommand(const _command: string;
  returnData: Boolean): TDataSet;
begin
  Result := TIBQuery.Create(nil);
  with TIBQuery(Result) do
  begin
    Database := self.Database;
    AutoCalcFields := False;
    ParamCheck := False;
    UniDirectional := True;
    SQL.Text := _command;
    {$ifdef ENTITY_LOG_SQL}
    SaveLog(Sql);
    {$endif}
    if returnData then
      Open
    else
      begin
        if not  self.Transaction.Active then
          self.Transaction.StartTransaction
        ;
        try
          ExecSQL;
          self.Transaction.Commit;
        except
          self.Transaction.Rollback;
        end;
        Result.Free
      end;
  end; // with
end;

function TEntityConnectionInterbase.GetList(Action: TConListAction;
  const Params: string): TStringList;
begin
  Result := nil;
  case Action of
    claTables:
      begin
        Result := TStringList.Create;
        Database.GetTableNames(Result);
      end;
    claClear, claFields:
      Result := inherited GetList(Action,Params);
  end;
end;

destructor TEntityConnectionInterbase.Destroy;
begin
  Database.Free;
  Transaction.Free;
  inherited Destroy;
end;

{$endif}

{ TEntityConnectionDbExpress }
{$ifdef DB_EXPRESS_LIB}
function TEntityConnectionDbExpress.ConnectionType: TEntityConnectionType;
begin
  Result := ect_DbExpress;
end;

constructor TEntityConnectionDbExpress.Create(const _ConnectionString: string);
var
  LTemp: TStringList;
begin
  LTemp := TStringList.Create;
  LTemp.CommaText := _ConnectionString;
  Connection := TSQLConnection.Create(nil);
  with TSQLConnection(Connection) do
  begin
    DriverName := LTemp.Values['DriverName'];
    LoginPrompt := false;
    case TypeOfDb(LTemp.Values['DriverName']) of
      sdtSqlServer:
        Settings.AutoIncType := aitSqlServer;
      sdtMySql:
        begin
          Settings.AutoIncType := aitMySql;
          DriverName := 'MySQL50';
          GetDriverFunc := 'getSQLDriverMYSQL50';
          LibraryName := 'dbxopenmysql50.dll';
          VendorLib := 'libmysql.dll';
        end;
      sdtPostGre:
        Settings.AutoIncType := aitPostgre;
    end; // case
    Params.Values['DriverName'] := LTemp.Values['DriverName'];
    Params.Values['HostName']   := LTemp.Values['Host'];
    Params.Values['Database']   := LTemp.Values['Database'];
    Params.Values['User_Name']  := LTemp.Values['User_Name'];
    Params.Values['Password']   := LTemp.Values['Password'];
    ConnectionName := LTemp.Values['ConnectionName'];
    Connected := true;
  end; // with
  LTemp.Free;
end;

function TEntityConnectionDbExpress.ExecuteCommand(const _command: string;
  returnData: Boolean): TDataSet;
begin
  Result := TSqlQuery.Create(nil);
  with TSQLQuery(Result) do
  begin
    AutoCalcFields := false;
    ParamCheck := false;
    SQLConnection := TSQLConnection(self.Connection);
    {--------------------------------------------}
    SQL.Text := _command;
    {$ifdef ENTITY_LOG_SQL}
    SaveLog(Sql);
    {$endif}
    if returnData then
      Open
    else
      begin
        ExecSQL;
        Result.Free
      end;
  end; // with
end;

function TEntityConnectionDbExpress.GetList(Action: TConListAction;
  const Params: string): TStringList;
begin
  Result := nil;
  case Action of
    claTables:
      begin
        Result := TStringList.Create;
        TSQLConnection(Connection).GetTableNames(Result);
      end;
    claClear, claFields:
      Result := inherited GetList(Action,Params);
  end;
end;
{$endif}

{$ifdef ZEOS_LIB_DB}
constructor TEntityConnectionZeosLib.Create(const _ConnectionString: string);
var
  LTemp: TStringList;
begin
  Connection := TZConnection.Create(nil);
  LTemp := TStringList.Create;
  LTemp.CommaText := _ConnectionString;
  with TZConnection(Connection) do
  begin
    HostName := LTemp.Values['HostName'];
    Port     := StrToIntDef(LTemp.Values['Port'],0);
    Database := LTemp.Values['Database'];
    User     := LTemp.Values['User'];
    Password := LTemp.Values['Password'];
    { DONE -c1.44 : ZeosLib bug fix }
    Protocol := LowerCase( LTemp.Values['Protocol'] );
    Connected := true;
  end; // with
  case TypeOfDb(LTemp.Values['Protocol']) of
    sdtSqlServer:
      Settings.AutoIncType := aitSqlServer;
    sdtPostGre:
      Settings.AutoIncType := aitPostgre;
    sdtMySql:
      Settings.AutoIncType := aitMySql;
  end; // case
  LTemp.Free;
end;

function TEntityConnectionZeosLib.ExecuteCommand(const _command: string;
  returnData: Boolean): TDataSet;
begin
  Result := TZReadOnlyQuery.Create(nil);
  with TZReadOnlyQuery(Result) do
  begin
    AutoCalcFields := false;
    Connection := TZConnection(self.Connection);
    SQL.Text := _command;
    {$ifdef ENTITY_LOG_SQL}
    SaveLog(Sql);
    {$endif}
    if returnData then
      Open
    else
      ExecSQL;
  end; // with
  if not returnData then FreeAndNil(Result);
end;

function TEntityConnectionZeosLib.GetList(Action: TConListAction;
  const Params: string): TStringList;
begin
  Result := nil;
  case Action of
    claTables:
      begin
        Result := TStringList.Create;
        TZConnection(Connection).GetTableNames('',Result);
      end;
    claClear, claFields:
      Result := inherited GetList(Action,Params);
  end;
end;
{$endif}

{$ifdef FIRE_DAC_DB}
constructor TEntityConnectionFiredac.Create(const _ConnectionString: string);
begin
  Connection := TFDConnection.Create(nil);
  with TFDConnection(Connection) do
  begin
    Params.CommaText := _ConnectionString;
    case TypeOfDb(Params.Values['DriverID']) of
      sdtSqlServer:
        Settings.AutoIncType := aitSqlServer;
      sdtPostGre:
        Settings.AutoIncType := aitPostgre;
      sdtMySql:
        Settings.AutoIncType := aitMySql;
    end; // case
    LoginPrompt := false;
    Connected := true;
  end; // with
end;

function TEntityConnectionFiredac.ExecuteCommand(const _command: string;
  returnData: Boolean): TDataSet;
begin
  Result := TFDQuery.Create(nil);
  with TFDQuery(Result) do
  begin
    AutoCalcFields := false;
    ResourceOptions.ParamExpand := false;
    ResourceOptions.ParamCreate := false;
    Connection := TFDConnection(self.Connection);
    SQL.Text := _command;
    {$ifdef ENTITY_LOG_SQL}
    SaveLog(Sql);
    {$endif}
    if returnData then
      Open
    else
      begin
        ExecSQL;
        Result.Free
      end;
  end; // with
end;

function TEntityConnectionFiredac.GetList(Action: TConListAction;
  const Params: string): TStringList;
begin
  Result := nil;
  case Action of
    claTables:
      begin
        Result := TStringList.Create;
        TFDConnection(Connection).GetTableNames('', '', '', Result,[osMy],[tkTable],false);
      end;
    claClear, claFields:
      Result := inherited GetList(Action,Params);
  end;
end;
{$ENDIF}

{ TEntitySettings }

constructor TEntitySettings.Create(ConnectionType: TEntityConnectionType);
begin
  JoinTables    := false;
  DateFormat    := 'yyyy/mm/dd';
  FloatFormat   := '###0.00';
  StartLikeExpr := '%';
  EndLikeExpr   := '%';
  AutoIncType   := aitNone;
  if ConnectionType = ect_DbExpress then
    fUsingDbExpress := true;
end;

destructor TEntitySettings.Destroy;
begin
  if AuxTable <> nil then
    AuxTable.Free;
  inherited;
end;

procedure TEntitySettings.SetAutoIncType(const Value: TAutoIncrementType);
const
  cAUX_TABLE_NAME = 'AuxTable';
begin
  fAutoInc := Value;
  if fAutoInc = aitAuxTable then
  begin
    AuxTable := TEntity.Create;
    AuxTable.needReload := true;
    AuxTable.TableName := cAUX_TABLE_NAME;
  end;
end;

procedure TEntitySettings.setJoinState(const Value: TJoinState);
begin
  case Value of
    jsENABLED:
      if fJoinState = jsDISABLED then
        fJoinTables := true
      ;
    jsDISABLED:
      if not fJoinTables then
        Exit
      else
        fJoinTables := false;
  end;
  fJoinState := Value;
end;

procedure TEntitySettings.setJoinTables(const Value: Boolean);
begin
  fJoinState := jsCHANGED;
  fJoinTables := Value;
end;

{ TSqlBuilder }

constructor TSqlFieldBuilder.Create(AItem: TEntityField);
begin
  fItem := AItem;
end;

function TSqlFieldBuilder.Text: string;
var
  EntityConnection: IEntityConnection;
begin
  EntityConnection := fItem.Parent.EntityConnection;
  if EntityConnection.Settings.JoinTables then
     Result := Format(
                '%s.%s AS %s',[
                       fItem.Parent.TableName,
                       fItem.Name,
                       fItem.FullName()
                ]
              )
  else
    Result := fItem.Name
  ;
end;

{ TCountBuilder }

function TCountBuilder.Alias: string;
begin
  Result := 'countValue';
end;

function TCountBuilder.functionName: string;
var
  B: TSqlFieldBuilder;
begin
  Result := 'Count';
  B := TSqlFieldBuilder.Create(fItem);
  if fItem.Grouped then
    Result := B.Text+','+Result
  ;
  B.Free;
end;


{ TAggregateBuilder }

function TAggregateBuilder.Alias: string;
begin
  Result := fItem.Name;
end;

function TAggregateBuilder.Text: string;
begin
     Result := Format(
                '%s(%s) AS %s',[
                       functionName(),
                       fItem.Name,
                       Alias
                ]
              )
     ;
end;

{ TMaxBuilder }

function TMaxBuilder.functionName: string;
begin
  Result := 'Max';
end;

{ TMinBuilder }

function TMinBuilder.functionName: string;
begin
  Result := 'Min';
end;

{ TAvgBuilder }

function TAvgBuilder.functionName: string;
begin
  Result := 'Avg';
end;

{ TSumBuilder }

function TSumBuilder.functionName: string;
begin
  Result := 'Sum';
end;

{$ifdef DB_EXPRESS_LIB}
procedure TEntity.setProviderName(const Value: string);
begin
  { DONE -c1.43 : ProviderName for DbExpress }
  if Value <> '' then
    begin
      if objProvider = nil then Exit;
      objProvider.Name := Value;
    end
  else if objProvider <> nil then
    begin
      FreeAndNil(objProvider);
    end;
end;


function TEntity.getProviderName: string;
begin
    if objProvider <> nil then
      Result := objProvider.Name
    else
      Result := ''
    ;
end;
{$ENDIF}

{ TCachedEntity }

function TCachedEntity.selectCommand: string;
begin
  Result := inherited selectCommand + COMMENT_TO_NOT_SAVE;
end;

procedure TCachedEntity.setLocalConnection(const Value: IEntityConnection);
begin
  { TODO -c1.46 : Cache with LocalConnection }
  Cache := self.Copy();
  Cache.EntityConnection := Value;
  Cache.RunDatabase();
end;

function TCachedEntity.getLocalConnection: IEntityConnection;
begin
  if Cache <> nil then
    Result := Cache.EntityConnection
  else
    Result := nil
  ;
end;

function TCachedEntity.Find(conditions: array of TConditionItem;
  FieldList: array of TEntityField): Boolean;
begin
  Result := True;
  if (Cache <> nil) and (Cache.Find(conditions,FieldList)) then
  begin
    Assign(Cache);
    Cache.TransferDataSet(self);
    Exit;
  end;
  Result := inherited Find(conditions,FieldList);
  if Result and (Cache <> nil) then
  begin
    Cache.NewRecord();
    Cache.Assign(self);
    Cache.SaveChanges;
  end;
end;

function TCachedEntity.SaveChanges: Boolean;
begin
  if isNewRecord then
    Cache.NewRecord()
  ;
  Cache.Assign(self);
  Cache.SaveChanges;
  Result := inherited SaveChanges;
end;

destructor TCachedEntity.Destroy;
begin
  if Cache <> nil then
    Cache.Free
  ;
  inherited;
end;

procedure TEntity.TransferDataSet(Target: TEntity);
begin
  Target.DataSet := f_DataSet;
  f_DataSet := nil;
  SetDataSet(nil);
end;


end.
