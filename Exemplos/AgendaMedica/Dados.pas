unit Dados;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, EntityObjects, DB, StdCtrls;

type

  TPessoa = class(TEntity)
  private
    f_id: TEntityField;
    f_nome: TEntityField;
  public
    property id: TEntityField read f_id;
    property nome: TEntityField read f_nome;
    constructor Create(_Master: TEntity = nil); override;
  end;

  TAgenda = class(TEntity)
  private
    f_dia: TEntityField;
    f_hora: TEntityField;
    f_Medico: TEntityField;
    f_Paciente: TEntityField;
  public
    property dia: TEntityField read f_dia;
    property hora: TEntityField read f_hora;
    property Medico: TEntityField read f_Medico;
    property Paciente: TEntityField read f_Paciente;
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

constructor TPessoa.Create(_Master: TEntity);
begin
  inherited;
  f_id := AddField('id',ftInteger,true);
  f_id.isAutoInc := true;
  f_nome := AddField('nome',ftString,false,30);
end;

constructor TAgenda.Create(_Master: TEntity);
begin
  inherited;
  TableName := 'Agenda';
  f_dia := AddField('dia',ftDate);
  f_hora := AddField('hora',ftString,false,5);
  f_Medico := AddField('Medico',ftInteger);
  f_Medico.ForeignEntity := TMedico.Create(self);
  f_Paciente := AddField('Paciente',ftInteger);
  f_Paciente.ForeignEntity := TPaciente.Create(self);
  EntityConnection := TEntityConnectionInterbase.Create('Host=,Database=C:\VM_Shared\db\AGENDAMED.FDB,User=SYSDBA,Password=123,Port=,Driver=Firebird');
  EntityConnection.Settings.JoinTables := true;
  RunDatabase(); // Cria as tabelas, se necessário
end;

constructor TMedico.Create(_Master: TEntity);
begin
  inherited;
  TableName := 'Medico';
end;

constructor TPaciente.Create(_Master: TEntity);
begin
  inherited;
  TableName := 'Paciente';
end;

end.