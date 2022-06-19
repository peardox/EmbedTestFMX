unit Unit1;

interface

uses
  System.SysUtils, System.IOUtils, System.Threading, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Zip,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, PyEnvironment.Embeddable.Res, PyEnvironment.Embeddable.Res.Python39,
  PythonEngine, PyEnvironment, PyEnvironment.Embeddable, TorchVision, PyTorch,
  NumPy, PyCommon, PyModule, PyPackage, SciPy, FMX.PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    TabControl1: TTabControl;
    LogMemo: TMemo;
    TabItem1: TTabItem;
    Splitter1: TSplitter;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    PyEngine: TPythonEngine;
    SciPy1: TSciPy;
    NumPy1: TNumPy;
    PyTorch1: TPyTorch;
    TorchVision1: TTorchVision;
    CodeMemo: TMemo;
    PyEmbedEnv: TPyEmbeddedResEnvironment39;
    PyIO: TPythonGUIInputOutput;
    cbCleanOnExit: TCheckBox;
    lblStatus: TLabel;
    Panel1: TPanel;
    btnRunCode: TButton;
    btnReLoad: TButton;
    procedure PyIOSendUniData(Sender: TObject; const Data: string);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure PackageConfigureInstall(Sender: TObject);
    procedure PackageAfterInstall(Sender: TObject);
    procedure PackageInstallError(Sender: TObject; AErrorMessage: string);
    procedure PackageBeforeImport(Sender: TObject);
    procedure PyEngineBeforeUnload(Sender: TObject);
    procedure PyEngineSysPathInit(Sender: TObject; PathList: PPyObject);
    procedure PyEmbedEnvAfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbedEnvAfterDeactivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbedEnvZipProgress(Sender: TObject;
      ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
      Header: TZipHeader; Position: Int64);
    procedure btnReLoadClick(Sender: TObject);
    procedure btnRunCodeClick(Sender: TObject);
  private
    { Private declarations }
    AppRoot: String;
    FTask: ITask;
    SystemAvailable: Boolean;
    SystemActivated: Boolean;
    procedure EnableForm(Enable: Boolean);
    procedure Log(AMsg: String);
    procedure UpdateStatus(const AStatus: String);
    function IsTaskRunning: boolean;
    procedure CreateSystem;
    procedure RunCode();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Log('Trying to close');
  if IsTaskRunning() then begin
    Log('Waiting for operations...');
    FTask.Cancel();
    while IsTaskRunning() do begin
      FTask.Wait(100);
      //Avoid synchronization deadlock
      Application.ProcessMessages();
    end;
  end;
  PyEmbedEnv.Deactivate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AppRoot := TPath.GetLibraryPath;
  SystemAvailable := False;
  SystemActivated := False;
  LogMemo.ReadOnly := True;
  LogMemo.Lines.Clear;
  Log('Getting Ready');
  CodeMemo.Lines.LoadFromFile(AppRoot + 'testcode.py');
  CreateSystem;
  EnableForm(False);
end;

function TForm1.IsTaskRunning: boolean;
begin
  if FTask = Nil then
    Result := False
  else
    Result := not (FTask.Status in [TTaskStatus.Completed, TTaskStatus.Exception]);
end;

procedure TForm1.UpdateStatus(const AStatus: String);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, procedure() begin
      lblStatus.Text := AStatus;
      StatusBar1.Repaint;
    end)
  else
    begin
      lblStatus.Text := AStatus;
      StatusBar1.Repaint;
    end;
end;


procedure TForm1.Log(AMsg: String);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, procedure() begin
     LogMemo.Lines.Add(AMsg);
     LogMemo.Repaint;
    end)
  else
    LogMemo.Lines.Add(AMsg);
end;

procedure TForm1.EnableForm(Enable: Boolean);
begin
  btnRunCode.Enabled := Enable;
  btnReLoad.Enabled := Enable;
  cbCleanOnExit.Enabled := Enable;
  CodeMemo.Enabled := Enable;
end;

procedure TForm1.PackageConfigureInstall(Sender: TObject);
begin
//  TPyManagedPackage(Sender).PythonEngine := PyEngine;
//  TPyManagedPackage(Sender).PyEnvironment := PyEmbedEnv;
  TPyManagedPackage(Sender).AfterInstall := PackageAfterInstall;
  TPyManagedPackage(Sender).OnInstallError := PackageInstallError;
  TPyManagedPackage(Sender).BeforeImport := PackageBeforeImport;

  MaskFPUExceptions(True);
  Log('Installing ' + TPyPackage(Sender).PyModuleName);
  UpdateStatus('Installing ' + TPyPackage(Sender).PyModuleName);
end;

procedure TForm1.PackageAfterInstall(Sender: TObject);
begin
  Log('Installed ' + TPyPackage(Sender).PyModuleName);
end;

procedure TForm1.PackageBeforeImport(Sender: TObject);
begin
  Log('Importing ' + TPyPackage(Sender).PyModuleName);
  UpdateStatus('Importing ' + TPyPackage(Sender).PyModuleName);
  MaskFPUExceptions(True);
end;

procedure TForm1.PackageInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log(TPyPackage(Sender).PyModuleName + ' : ' + AErrorMessage);
end;

procedure TForm1.PyIOSendUniData(Sender: TObject; const Data: string);
begin
  Log(Data);
  Application.ProcessMessages;
end;

procedure TForm1.CreateSystem;
begin
  if not SystemAvailable then
    begin
      Log('Creating System');
      Log('===============');
      Log('Home : ' + TPath.GetHomePath);
      Log('Temp : ' + TPath.GetTempPath);
      Log('Libs : ' + TPath.GetLibraryPath);
      Log('Docs : ' + TPath.GetDocumentsPath);
      Log('Shared Docs : ' + TPath.GetSharedDocumentsPath);
      Log('Cache : ' + TPath.GetCachePath);
      Log('Pics : ' + TPath.GetPicturesPath);
      Log('Shared Pics : ' + TPath.GetSharedPicturesPath);
      Log('Public : ' + TPath.GetPublicPath);
      Log('Cams : ' + TPath.GetCameraPath);
      Log('Shared Cams : ' + TPath.GetSharedCameraPath);
      Log('Movies : ' + TPath.GetMoviesPath);
      Log('Shared Movies : ' + TPath.GetSharedMoviesPath);
      Log('Downloads : ' + TPath.GetDownloadsPath);
      Log('Shared Downloads : ' + TPath.GetSharedDownloadsPath);
      Log('===============');
      MaskFPUExceptions(True);
      FTask := TTask.Run(procedure() begin
        PyEmbedEnv.Setup(PyEmbedEnv.PythonVersion);
        FTask.CheckCanceled();
        TThread.Synchronize(nil, procedure() begin
          PyEmbedEnv.Activate(PyEmbedEnv.PythonVersion);
        end);
        FTask.CheckCanceled();

        Numpy1.Install();
        FTask.CheckCanceled();

        PyTorch1.Install();
        FTask.CheckCanceled();

        TorchVision1.Install();
        FTask.CheckCanceled();

        SciPy1.Install();
        FTask.CheckCanceled();

        TThread.Queue(nil, procedure() begin
{
          Numpy1.Import();
          PyTorch1.Import();
          TorchVision1.Import();
          SciPy1.Import();
}
          Log('Ready');
          UpdateStatus('Ready');
          SystemAvailable := True;
          EnableForm(True);
        end);
      end);
    end;
end;

///// Startup Code Execution /////

procedure TForm1.btnRunCodeClick(Sender: TObject);
begin
  RunCode();
end;

procedure TForm1.btnReLoadClick(Sender: TObject);
begin
  CodeMemo.Lines.LoadFromFile(AppRoot + 'testcode.py');
end;

procedure TForm1.RunCode();
begin
  if SystemAvailable then
    begin
      LogMemo.Lines.Clear;
      try
        MaskFPUExceptions(True);
        try
          PyEngine.ExecStrings(CodeMemo.Lines);
        except
          on E: EPyIndentationError do
            begin
              Log('Indentation Exception : Line = ' + IntToStr(E.ELineNumber) +
                  ', Offset = ' + IntToStr(E.EOffset));
            end;
          on E: EPyImportError do
            begin
              Log('Import Exception : ' + E.EValue + ' : ' + E.EName);
            end;
          on E: Exception do
            begin
              Log('Unhandled Exception');
              Log('Class : ' + E.ClassName);
              Log('Error : ' + E.Message);
            end;
        end;
      finally
          MaskFPUExceptions(False);
      end;
    end;
end;

///// Python Setup /////

procedure TForm1.PyEmbedEnvZipProgress(Sender: TObject;
  ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  UpdateStatus(Filename + ' : ' + IntToStr(Position));
end;

///// Python Startup /////

procedure TForm1.PyEmbedEnvAfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  if AActivated then
    Log('Python ' + APythonVersion + ' Active');
end;

procedure TForm1.PyEmbedEnvAfterDeactivate(Sender: TObject;
  const APythonVersion: string);
begin
  SystemActivated := False;
  if cbCleanOnExit.IsChecked then
    begin
      TDirectory.Delete(PyEmbedEnv.EnvironmentPath, True);
    end;
end;

procedure TForm1.PyEngineBeforeUnload(Sender: TObject);
begin
  Log('PyEngineBeforeUnload'); // SBDbg
end;

procedure TForm1.PyEngineSysPathInit(Sender: TObject; PathList: PPyObject);
begin
  Log('PyEngineSysPathInit'); // SBDbg
end;



end.
