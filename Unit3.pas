unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, PythonEngine,
  PyEnvironment, PyEnvironment.Embeddable, PyCommon, PyModule, PyPackage,
  System.DateUtils, Modules, FMX.TabControl;

type
  TfrmTraining = class(TForm)
    Text1: TText;
    btnAbort: TButton;
    btnSample: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Text2: TText;
    procedure btnAbortClick(Sender: TObject);
    procedure btnSampleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateProgress(const log: TTrainLog);
    { Public declarations }
  end;

var
  frmTraining: TfrmTraining;

implementation

{$R *.fmx}
Uses unit1;

procedure TfrmTraining.UpdateProgress(const log: TTrainLog);
var
  left : TDateTime;
  eta: TDateTime;
begin
  eta := IncSecond(Now, log.train_left);
  Text1.Text := Format('%2.2d:%2.2d:%2.2d',[
    log.train_left div 3600,
    (log.train_left div 60) mod 60,
    log.train_left mod 60]);

  if log.train_left < 60 then
    Text2.Text := 'Almost' + sLineBreak + 'Done'
  else
    Text2.Text := 'ETA' + sLineBreak + FormatDateTime('hh:mm', eta);
  Text1.RePaint;
end;
procedure TfrmTraining.btnAbortClick(Sender: TObject);
begin
  btnAbort.Enabled := False;
  frmMain.AbortTraining;
end;

procedure TfrmTraining.btnSampleClick(Sender: TObject);
begin
  btnSample.Enabled := False;
  frmMain.SampleTraining;
end;

procedure TfrmTraining.FormShow(Sender: TObject);
begin
  Text1.Text := 'Starting Training';
  Text2.Text := 'Please Wait';
  Text1.SetFocus;
end;

end.
