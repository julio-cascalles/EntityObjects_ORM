unit Dados;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, EntityObjects, DB, StdCtrls;

type

  TPessoa = class(TEntity)
  private
    f_Nome: TEntityField;
    f_Id: TEntityField;
  public
    property id: TEntityField read f_Id;
    property nome: TEntityField read f_Nome;
    constructor Create(_Master: TEntity = nil); override;
  end;

  TAgenda = class(TEntity)
  private
    fDia: TEntityField;
    fHora: TEntityField;
    fMedico: TEntityField;
    fPaciente: TEntityField;
  public
    property dia: TEntityField read fDia;
    property hora: TEntityField read fHora;
    property medico: TEntityField read fMedico;
    property paciente: TEntityField read fPaciente;
    constructor Create(_Master: TEntity = nil); override;
  end;

  TMedico = class(TPessoa)
  public
    constructor Create(_Master: TEntity = nil); override;
  end;

  TPaciente = class(TPessoa)
  public
    constructor Create(_Master: TEntity = nil); override;
  end;


implementation

{ TPessoa }

constructor TPessoa.Create(_Master: TEntity = nil);
begin
  inherited;
  f_Id := AddField('id',ftInteger,true);
  f_Id.isAutoInc := true;
  f_Nome := AddField('nome',ftString,false,30);
end;

constructor TAgenda.Create(_Master: TEntity = nil);
begin
  inherited;
  TableName := 'Agenda';
  fDia := AddField('dia',ftDate);
  fHora := AddField('hora',ftString,false,5);
  fMedico := AddField('Medico',ftInteger);
  fMedico.ForeignEntity := TMedico.Create(self);
  fPaciente := AddField('Paciente',ftInteger);
  fPaciente.ForeignEntity := TPaciente.Create(self);;
  {$ifdef FPC}
  EntityConnection := TEntityConnectionSqlDb.Create('Database=ODBC_agenda,User=root');
  {$else}
//  EntityConnection := TEntityConnectionADO.Create('Provider=MSDASQL.1;Persist Security Info=False;User ID=root;Data Source=ODBC_agenda;');
   EntityConnection := TEntityConnectionDbExpress.create('DriverName=Firebird,Database=C:\VM_Shared\db\AGENDAMED.FDB,User_Name=SYSDBA,Password=123');
//
  {$endif}
//  EntityConnection.Settings.JoinTables  := True;
  EntityConnection.Settings.AutoIncType := aitAuxTable;
  RunDatabase();
end;

constructor TMedico.Create(_Master: TEntity = nil);
begin
  inherited;
  TableName := 'Medico';
end;

constructor TPaciente.Create(_Master: TEntity = nil);
begin
  inherited;
  TableName := 'Paciente';
end;


end.
