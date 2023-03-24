unit CadastroPacientes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ModeloCadastroPessoas, Grids, Buttons, StdCtrls, ExtCtrls, Dados,
  DB, DBGrids;

type
  TfrmCadastroPacientes = class(TfrmModeloCadastroPessoas)
  protected
    procedure setOriginal(const Value: TAgenda); override;
  end;


implementation

{$R *.dfm}


{ TfrmCadastroPacientes }

procedure TfrmCadastroPacientes.setOriginal(const Value: TAgenda);
begin
  inherited;
  Pessoa := TPaciente(AgendaCopia.Paciente.ForeignEntity);
end;

end.
