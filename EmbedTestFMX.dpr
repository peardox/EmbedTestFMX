program EmbedTestFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Modules in 'Modules.pas',
  Unit2 in 'Unit2.pas' {frmProgress};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.Run;
end.
