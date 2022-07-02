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
    procedure btnAbortClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTraining: TfrmTraining;

implementation

{$R *.fmx}
Uses unit1;

procedure TfrmTraining.btnAbortClick(Sender: TObject);
begin
  ModalResult := mrAbort;
end;

end.
