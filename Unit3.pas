unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, PythonEngine,
  PyEnvironment, PyEnvironment.Embeddable, PyCommon, PyModule, PyPackage;

type
  TfrmTraining = class(TForm)
    Text1: TText;
    Text2: TText;
    btnAbort: TButton;
    btnSample: TButton;
    procedure btnAbortClick(Sender: TObject);
    procedure btnSampleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTraining: TfrmTraining;

implementation

{$R *.fmx}
Uses unit1, Modules;

procedure TfrmTraining.btnAbortClick(Sender: TObject);
begin
  frmMain.AbortTraining;
end;

procedure TfrmTraining.btnSampleClick(Sender: TObject);
begin
  frmMain.AbortTraining;
end;

end.
