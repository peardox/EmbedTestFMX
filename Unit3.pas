unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, PythonEngine,
  PyEnvironment, PyEnvironment.Embeddable, PyCommon, PyModule, PyPackage,
  System.DateUtils, Modules, FMX.TabControl, FMXTee.Engine, FMXTee.Procs,
  FMXTee.Chart;

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
    imgSample: TImage;
    ProgressBar1: TProgressBar;
    Chart1: TChart;
    Button1: TButton;
    procedure btnAbortClick(Sender: TObject);
    procedure btnSampleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateSample(const AFilename: String);
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
  calibration: Boolean;
begin
  if log.reporting_line = 1 then
    begin
      btnAbort.Enabled := True;
      btnSample.Enabled := True;
    end;

  eta := IncSecond(Now, log.train_left);
  if btnSample.Enabled then
    begin
      if (log.reporting_line = 1) or (log.train_delta > 0.01) then
        Text1.Text := 'Calibrating'
      else
        Text1.Text := Format('%2.2d:%2.2d:%2.2d Remaining',[
          log.train_left div 3600,
          (log.train_left div 60) mod 60,
          log.train_left mod 60]);
    end
  else
    Text1.Text := 'Creating Sample';

  if log.train_left < 60 then
    Text2.Text := 'Almost' + sLineBreak + 'Done'
  else
    Text2.Text := 'ETA' + sLineBreak + FormatDateTime('hh:mm', eta);

  ProgressBar1.Value := log.train_completion;

  Application.ProcessMessages;
end;

procedure TfrmTraining.UpdateSample(const AFilename: String);
begin
  if FileExists(AFilename) then
    begin
      TabControl1.ActiveTab := TabItem2;
      imgSample.Bitmap.LoadFromFile(AFilename);
      btnSample.Enabled := True;
    end;
  Application.ProcessMessages;
end;

procedure TfrmTraining.btnAbortClick(Sender: TObject);
begin
  btnAbort.Enabled := False;
  frmMain.AbortTraining;
end;

procedure TfrmTraining.btnSampleClick(Sender: TObject);
begin
  Text1.Text := 'Creating Sample';
  btnSample.Enabled := False;
  frmMain.SampleTraining;
end;

procedure TfrmTraining.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmTraining.FormShow(Sender: TObject);
begin
  Text1.Text := 'Starting Training';
  Text2.Text := 'Please Wait';
  TabControl1.ActiveTab := TabItem1;
  btnAbort.Enabled := False;
  btnSample.Enabled := False;
  ProgressBar1.Value := 0;
  frmMain.TrainModel;
end;

end.
