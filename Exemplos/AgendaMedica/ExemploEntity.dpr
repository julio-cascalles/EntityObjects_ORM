program ExemploEntity;

uses
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  ModeloCadastroPessoas in 'ModeloCadastroPessoas.pas' {frmModeloCadastroPessoas},
  CadastroMedicos in 'CadastroMedicos.pas' {frmCadastroMedicos},
  CadastroPacientes in 'CadastroPacientes.pas' {frmCadastroPacientes},
  Dados in 'Dados.pas',
  Principal in 'Principal.pas' {frmPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
