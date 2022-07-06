unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TfrmProgress = class(TForm)
    txtProgress: TText;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowProgress(const ADesc: String);
  end;

var
  frmProgress: TfrmProgress;

implementation

{$R *.fmx}
Uses Unit1;

procedure TfrmProgress.ShowProgress(const ADesc: String);
begin
  txtProgress.Text := ADesc;
  txtProgress.Repaint;
  frmProgress.BringToFront;
end;

end.
