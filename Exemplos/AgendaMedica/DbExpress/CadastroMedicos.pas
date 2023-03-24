unit CadastroMedicos;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ModeloCadastroPessoas, Grids, Buttons, StdCtrls, ExtCtrls, Dados,
  DB, DBGrids;

type
  TfrmCadastroMedicos = class(TfrmModeloCadastroPessoas)
  protected
    procedure setOriginal(const Value: TAgenda); override;
  end;


implementation

{$R *.dfm}


{ TfrmCadastroMedicos }

procedure TfrmCadastroMedicos.setOriginal(const Value: TAgenda);
begin
  inherited;
  Pessoa := TMedico(AgendaCopia.Medico.ForeignEntity);
end;

end.
