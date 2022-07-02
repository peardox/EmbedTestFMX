program EmbedTestFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {frmMain},
  Modules in 'Modules.pas',
  Unit2 in 'Unit2.pas' {frmProgress},
  Unit3 in 'Unit3.pas' {frmTraining};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmTraining, frmTraining);
  Application.Run;
end.
