unit Unit1;

interface

uses
  System.SysUtils, System.IOUtils, System.Threading, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Zip,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, PyEnvironment.Embeddable.Res, PyEnvironment.Embeddable.Res.Python39,
  PythonEngine, PyEnvironment, PyEnvironment.Embeddable, TorchVision, PyTorch,
  NumPy, PyCommon, PyModule, PyPackage, SciPy, FMX.PythonGUIInputOutput,
  FMX.Menus, Skia.FMX, FMX.Objects;

type
  TTrainingOptions = record
    dataset: String;
    style_image: String;
    model_name: String;
    model_dir: String;
    model_ext: String;
    checkpoint_model_dir: String;
    net: String;
    logfile: String;
    epochs: Integer;
    limit: Integer;
    batch_size: Integer;
    log_interval: Integer;
    checkpoint_interval: Integer;
    image_size: Integer;
    seed: Integer;
    content_weight: Single;
    style_weight: Single;
    lr: Single;
    style_scale: Single;
    force_size: Boolean;
    ignore_gpu: Boolean;
    cuda: Boolean;
  end;

  TStylizeOptions = record
    content_image: String;
    content_image_raw: String;
    output_image: String;
    model: String;
    model_dir: String;
    model_ext: String;
    logfile: String;
    content_scale: Single;
    cuda: Boolean;
    ignore_gpu: Boolean;
    export_onnx: Boolean;
    add_model_ext: Boolean;
  end;

  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
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
    CodeBtnPanel: TPanel;
    dlgOpenCode: TOpenDialog;
    Panel2: TPanel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    dlgOpenImage: TOpenDialog;
    pnlCodeView: TPanel;
    pnlLogView: TPanel;
    LogMemo: TMemo;
    Splitter1: TSplitter;
    pnlCodeEdit: TPanel;
    Panel1: TPanel;
    btnRunCode: TSpeedButton;
    btnReLoad: TSpeedButton;
    btnLoadCode: TSpeedButton;
    PythonModule1: TPythonModule;
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
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure cbCleanOnExitChange(Sender: TObject);
    procedure PythonModule1Initialization(Sender: TObject);
  private
    { Private declarations }
    AppRoot: String;
    CodeRoot: String;
    PythonCode: String;
    ImageRoot: String;
    ContentImageFileName: String;
    ContentBitmap: TBitMap;
    FirstRun: Boolean;
    FTask: ITask;
    LastShimPath: String;
    SystemAvailable: Boolean;
    SystemActivated: Boolean;
    StylizeOptions: TStylizeOptions;
    TrainingOptions: TTrainingOptions;

    procedure EnableForm(Enable: Boolean);
    procedure Log(AMsg: String);
    procedure UpdateStatus(const AStatus: String);
    function IsTaskRunning: boolean;
    procedure CreateSystem;
    procedure ShimSysPath(const ShimPath: String);
    procedure RunCode();
    procedure ExecutePython(const PythonCode: TStrings);
    procedure LoadCode;
    procedure ReLoadCode;
    procedure LoadImage;

    function GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

function CreateDefaultStylizeOptions: TStylizeOptions;
function CreateDefaultTrainingOptions: TTrainingOptions;

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
//  PyEngine.UnloadDll;
  if not(ContentBitmap = Nil) then
  begin
    ContentBitmap.Free;
    ContentBitmap := Nil;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if (DebugHook <> 0) then
    AppRoot := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetFullPath(IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath + '..' + PathDelim + '..' + PathDelim)))
  else
    AppRoot := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath);
  CodeRoot := AppRoot;
  ImageRoot := AppRoot;
  ContentImageFileName := String.Empty;
  ContentBitmap := Nil;
  LastShimPath := String.Empty;
  FirstRun := True;
  PythonCode := AppRoot + 'testcode.py';
  SystemAvailable := False;
  SystemActivated := False;
  LogMemo.ReadOnly := True;
  LogMemo.Lines.Clear;
  UpdateStatus('Initializing');
  Log('Initializing');

  StylizeOptions := CreateDefaultStylizeOptions;
  TrainingOptions := CreateDefaultTrainingOptions;

  if not FileExists(PythonCode) then
  begin
    CodeMemo.Lines.Clear;
    ShowMessage('Code not found : ' + PythonCode);
  end
  else
    CodeMemo.Lines.LoadFromFile(PythonCode);
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
  begin
    TThread.Synchronize(nil, procedure() begin
     LogMemo.Lines.Add(AMsg);
     LogMemo.GoToTextEnd;
     LogMemo.Repaint;
    end);
  end
  else
  begin
    LogMemo.Lines.Add(AMsg);
    LogMemo.GoToTextEnd;
  end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  LoadImage;
end;

procedure TForm1.LoadImage;
begin
  dlgOpenImage.InitialDir := ImageRoot;
  if dlgOpenImage.Execute then
  begin
{
    ContentImageFileName := dlgOpenImage.FileName;
    ImageRoot := IncludeTrailingPathDelimiter(ExtractFileDir(dlgOpenImage.FileName));
    ContentImage.Bitmap.LoadFromFile(ContentImageFileName);
}
  end;
end;


procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  LoadCode;
end;

procedure TForm1.EnableForm(Enable: Boolean);
begin
  btnRunCode.Enabled := Enable;
  btnReLoad.Enabled := Enable;
  btnLoadCode.Enabled := Enable;
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
      Log('Home : ' + System.IOUtils.TPath.GetHomePath);
      Log('Temp : ' + System.IOUtils.TPath.GetTempPath);
      Log('Libs : ' + System.IOUtils.TPath.GetLibraryPath);
      Log('Docs : ' + System.IOUtils.TPath.GetDocumentsPath);
      Log('Shared Docs : ' + System.IOUtils.TPath.GetSharedDocumentsPath);
      Log('Cache : ' + System.IOUtils.TPath.GetCachePath);
      Log('Pics : ' + System.IOUtils.TPath.GetPicturesPath);
      Log('Shared Pics : ' + System.IOUtils.TPath.GetSharedPicturesPath);
      Log('Public : ' + System.IOUtils.TPath.GetPublicPath);
      Log('Cams : ' + System.IOUtils.TPath.GetCameraPath);
      Log('Shared Cams : ' + System.IOUtils.TPath.GetSharedCameraPath);
      Log('Movies : ' + System.IOUtils.TPath.GetMoviesPath);
      Log('Shared Movies : ' + System.IOUtils.TPath.GetSharedMoviesPath);
      Log('Downloads : ' + System.IOUtils.TPath.GetDownloadsPath);
      Log('Shared Downloads : ' + System.IOUtils.TPath.GetSharedDownloadsPath);
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

procedure TForm1.LoadCode;
begin
  dlgOpenCode.InitialDir := CodeRoot;
  if dlgOpenCode.Execute then
  begin
    PythonCode := dlgOpenCode.FileName;
    CodeRoot := IncludeTrailingPathDelimiter(ExtractFileDir(dlgOpenCode.FileName));
    CodeMemo.Lines.LoadFromFile(PythonCode);
    FirstRun := True;
  end;
end;

procedure TForm1.ReLoadCode;
begin
  if not FileExists(PythonCode) then
  begin
    dlgOpenCode.InitialDir := CodeRoot;
    if dlgOpenCode.Execute then
    begin
      PythonCode := dlgOpenCode.FileName;
      CodeRoot := IncludeTrailingPathDelimiter(ExtractFileDir(dlgOpenCode.FileName));
      CodeMemo.Lines.LoadFromFile(PythonCode);
      FirstRun := True;
    end
  else
    CodeMemo.Lines.Clear;
  end
  else
    CodeMemo.Lines.LoadFromFile(PythonCode);
end;

///// Startup Code Execution /////

procedure TForm1.btnRunCodeClick(Sender: TObject);
begin
  RunCode();
end;

procedure TForm1.cbCleanOnExitChange(Sender: TObject);
begin
  if cbCleanOnExit.IsChecked then
    pnlCodeView.Visible := False
  else
    pnlCodeView.Visible := True;
end;

procedure TForm1.btnReLoadClick(Sender: TObject);
begin
  ReLoadCode;
end;

procedure TForm1.ShimSysPath(const ShimPath: String);
var
  Shim: TStringList;
begin
  Shim := Nil;
  try
    Shim := TStringList.Create;
    Shim.Add('import os');
    Shim.Add('import sys');
    if not(LastShimPath = String.Empty) then
    begin
      Shim.Add('for p in reversed(sys.path):');
      Shim.Add('  if p == "' + LastShimPath + '":');
      Shim.Add('    sys.path.remove(p)');
    end;
    Shim.Add('sys.path.append("' + CodeRoot + ShimPath + '")');
    Shim.Add('os.chdir("' + ExcludeTrailingPathDelimiter(CodeRoot) + '")');

    Log('Shim');
    for var i := 0 to Shim.Count - 1 do
      Log(Shim[i]);

    PyEngine.ExecStrings(Shim);
    LastShimPath := CodeRoot + ShimPath;
  finally
    if not(Shim = Nil) then
      Shim.Free;
  end;
end;

procedure TForm1.ExecutePython(const PythonCode: TStrings);
begin
  try
    if FirstRun then
      begin
        ShimSysPath('pysrc');
        FirstRun := False;
      end;
    PyEngine.ExecStrings(PythonCode);
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
end;


procedure TForm1.RunCode();
begin
  if SystemAvailable then
    begin
      LogMemo.Lines.Clear;
      try
        MaskFPUExceptions(True);
        ExecutePython(CodeMemo.Lines);
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

///// Style Module Definitions /////
function CreateDefaultStylizeOptions: TStylizeOptions;
begin
  Result.content_image := 'input-images/haywain.jpg';
  Result.content_image_raw := String.Empty;
  Result.output_image := 'output-images/delphi-test.jpg';
  Result.model := 'test-dae-gothic-256';
  Result.model_dir := 'models';
  Result.model_ext := '.pth';
  Result.logfile := String.Empty;
  Result.content_scale := 1;
  Result.cuda := True;
  Result.ignore_gpu := False;
  Result.export_onnx := False;
  Result.add_model_ext := True;
end;

procedure TForm1.PythonModule1Initialization(Sender: TObject);
begin
  with Sender as TPythonModule do
    begin
      AddDelphiMethod( 'GetProperty', GetProperty, 'GetProperty(PropName) -> PropValue' );
      AddDelphiMethod( 'SetProperty', SetProperty, 'SetProperty(PropName, PropValue) -> None' );
      AddDelphiMethod( 'GetPropertyList', GetPropertyList, 'GetPropertyList() -> List of property names' );
    end;
end;

function TForm1.GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's:GetProperty',@key ) <> 0 then
      begin
        if key = 'content_image' then
          Result := VariantAsPyObject(StylizeOptions.content_image)
        else if key = 'content_image_raw' then
          Result := VariantAsPyObject(StylizeOptions.content_image_raw)
        else if key = 'output_image' then
          Result := VariantAsPyObject(StylizeOptions.output_image)
        else if key = 'model' then
          Result := VariantAsPyObject(StylizeOptions.model)
        else if key = 'model_dir' then
          Result := VariantAsPyObject(StylizeOptions.model_dir)
        else if key = 'model_ext' then
          Result := VariantAsPyObject(StylizeOptions.model_ext)
        else if key = 'logfile' then
          Result := VariantAsPyObject(StylizeOptions.logfile)
        else if key = 'content_scale' then
          Result := VariantAsPyObject(StylizeOptions.content_scale)
        else if key = 'cuda' then
          Result := VariantAsPyObject(StylizeOptions.cuda)
        else if key = 'ignore_gpu' then
          Result := VariantAsPyObject(StylizeOptions.ignore_gpu)
        else if key = 'export_onnx' then
          Result := VariantAsPyObject(StylizeOptions.export_onnx)
        else if key = 'add_model_ext' then
          Result := VariantAsPyObject(StylizeOptions.add_model_ext)
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TForm1.SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
  value : PPyObject;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'sO:SetProperty',@key, @value ) <> 0 then
      begin
        if key = 'content_image' then
          begin
            StylizeOptions.content_image := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'content_image_raw' then
          begin
            StylizeOptions.content_image_raw := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'output_image' then
          begin
            StylizeOptions.output_image := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model' then
          begin
            StylizeOptions.model := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model_dir' then
          begin
            StylizeOptions.model_dir := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model_ext' then
          begin
            StylizeOptions.model_ext := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'logfile' then
          begin
            StylizeOptions.logfile := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'content_scale' then
          begin
            StylizeOptions.content_scale := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'cuda' then
          begin
            StylizeOptions.cuda := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'ignore_gpu' then
          begin
            StylizeOptions.ignore_gpu := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'export_onnx' then
          begin
            StylizeOptions.export_onnx := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'add_model_ext' then
          begin
            StylizeOptions.add_model_ext := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TForm1.GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      Result := PyList_New(12);
      PyList_SetItem(Result, 0, PyUnicodeFromString('content_image'));
      PyList_SetItem(Result, 1, PyUnicodeFromString('content_image_raw'));
      PyList_SetItem(Result, 2, PyUnicodeFromString('output_image'));
      PyList_SetItem(Result, 3, PyUnicodeFromString('model'));
      PyList_SetItem(Result, 4, PyUnicodeFromString('model_dir'));
      PyList_SetItem(Result, 5, PyUnicodeFromString('model_ext'));
      PyList_SetItem(Result, 6, PyUnicodeFromString('logfile'));
      PyList_SetItem(Result, 7, PyUnicodeFromString('content_scale'));
      PyList_SetItem(Result, 8, PyUnicodeFromString('cuda'));
      PyList_SetItem(Result, 9, PyUnicodeFromString('ignore_gpu'));
      PyList_SetItem(Result, 10, PyUnicodeFromString('export_onnx'));
      PyList_SetItem(Result, 11, PyUnicodeFromString('add_model_ext'));
    end;
end;

///// Training Module Definitions /////
function CreateDefaultTrainingOptions: TTrainingOptions;
begin
  Result.dataset := '/train/unsplash/256';
  Result.style_image := 'style-images/flowers.jpg';
  Result.model_name := 'gui-flowers-256-4';
  Result.model_dir := 'models';
  Result.model_ext := '.pth';
  Result.checkpoint_model_dir := '';
  Result.net := 'vgg16';
  Result.logfile := '';
  Result.epochs := 4;
  Result.limit := 0;
  Result.batch_size := 1;
  Result.log_interval := 500;
  Result.checkpoint_interval := 1000;
  Result.image_size := 256;
  Result.seed := 42;
  Result.content_weight := 1e5;
  Result.style_weight := 1e10;
  Result.lr := 1e-3;
  Result.style_scale := 1.0;
  Result.force_size := True;
  Result.ignore_gpu := False;
  Result.cuda := True
end;


end.
