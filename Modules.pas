unit Modules;

interface
uses
  System.SysUtils, System.IOUtils, System.Threading, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Zip;

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


function CreateDefaultStylizeOptions: TStylizeOptions;
function CreateDefaultTrainingOptions: TTrainingOptions;

implementation

///// Style Module Definitions /////
function CreateDefaultStylizeOptions: TStylizeOptions;
begin
  Result.content_image := 'input-images/fermin-rembg.png';
  Result.content_image_raw := String.Empty;
  Result.output_image := 'output-images/fermin-flowers-256-4.jpg';
  Result.model := 'flowers-256-4';
  Result.model_dir := 'models';
  Result.model_ext := '.pth';
  Result.logfile := String.Empty;
  Result.content_scale := 1;
  Result.cuda := True;
  Result.ignore_gpu := False;
  Result.export_onnx := False;
  Result.add_model_ext := True;
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