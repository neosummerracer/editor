unit uProperty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Spin, StdCtrls, Types, Math;

type

  TPenStyleItem = record
    mName: String;
    PenStyle: TPenStyle
  end;

  TBrushStyleItem = record
    mName: String;
    BrushStyle: TBrushStyle;
  end;

  TProperty = class
  private
  public
  end;

  TPenColor = class(TProperty)
    sPenColor: TColor; static;
    class procedure CreatePenColorButton(panel: TPanel);
    class procedure PenColorChange(Sender: TObject);
  end;

  TPenWidth = class(TProperty)
    sPenWidth: Integer; static;
    class procedure CreateWidthSpinEdit(panel: TPanel);
    Class procedure PenWidthChange(Sender: TObject);
  end;

  TBrushColor = class(TProperty)
    sBrushColor: TColor; static;
    class procedure CreateBrushColorButton(panel: TPanel);
    class procedure BrushColorChange(Sender: TObject);
  end;

  TPenStyle = class(TProperty)
    sPenStyle: Integer; static;
    class procedure CreatePenStyleComboBox(panel: TPanel);
    class procedure PenStyleChange(Sender: TObject);
    const
      PEN_STYLES: array[0..5] of TPenStyleItem =
        (
          (mName: 'Solid';         PenStyle: psSolid),
          (mName: 'No line';       PenStyle: psClear),
          (mName: 'Dots';          PenStyle: psDot),
          (mName: 'Dashes';        PenStyle: psDash),
          (mName: 'Dash dots';     PenStyle: psDashDot),
          (mName: 'Dash dot dots'; PenStyle: psDashDotDot)
        );
  end;

  TBrushStyle = class(TProperty)
    sBrushStyle: Integer; static;
    class procedure CreateBrushStyleComboBox(panel: TPanel);
    class procedure BrushStyleChange(Sender: TObject);
    const
      BRUSH_STYLES: array[0..7] of TBrushStyleItem =
          (
            (mName: 'Solid';              BrushStyle: bsSolid),
            (mName: 'Hollow';             BrushStyle: bsClear),
            (mName: 'Horizontal stripes'; BrushStyle: bsHorizontal),
            (mName: 'Vertical stripes';   BrushStyle: bsVertical),
            (mName: 'Left diagonal';      BrushStyle: bsFDiagonal),
            (mName: 'Right diagonal';     BrushStyle: bsBDiagonal),
            (mName: 'Cross';              BrushStyle: bsCross),
            (mName: 'Diagonal cross';     BrushStyle: bsDiagCross)
          );
  end;

  TRoundRect = class(TProperty)
    sRX, sRY: Integer; static;
    class procedure CreateRXSpinEdit(panel: TPanel);
    class procedure ChangeRX(Sender: TObject);
    class procedure CreateRYSpinEdit(panel: TPanel);
    Class procedure ChangeRY(Sender: TObject);
  end;

implementation

{Create buttons}
class procedure TPenColor.CreatePenColorButton(panel: TPanel);
var
  container: TPanel;
  name: TLabel;
  ColorButton: TColorButton;
begin
  container := TPanel.Create(panel);
  container.Parent := panel;
  container.Align := alTop;
  name := TLabel.Create(container);
  name.Parent := container;
  name.Align := alTop;
  name.Height := 10;
  name.Caption := 'Pen color';
  ColorButton := TColorButton.Create(container);
  ColorButton.Parent := container;
  ColorButton.Align := alBottom;
  ColorButton.ButtonColor := sPenColor;
  ColorButton.OnColorChanged := @PenColorChange;
end;

class procedure TPenWidth.CreateWidthSpinEdit(panel: TPanel);
var
  container: TPanel;
  name: TLabel;
  Edit: TSpinEdit;
begin
  container := TPanel.Create(panel);
  container.Parent := panel;
  container.Align := alTop;
  name := TLabel.Create(container);
  name.Parent := container;
  name.Align := alTop;
  name.Caption := 'Line width';
  Edit := TSpinEdit.Create(container);
  Edit.Parent := container;
  Edit.Align:= alBottom;
  Edit.AutoSelect := false;
  Edit.Value:= sPenWidth;
  Edit.OnChange := @PenWidthChange;
end;

class procedure TBrushColor.CreateBrushColorButton(panel: TPanel);
var
  Container: TPanel;
  name: TLabel;
  ColorButton: TColorButton;
begin
  container := TPanel.Create(panel);
  container.Parent := panel;
  container.Align := alTop;
  name := TLabel.Create(container);
  name.Parent := container;
  name.Align := alTop;
  name.Caption := 'Brush color';
  ColorButton := TColorButton.Create(container);
  ColorButton.Parent := container;
  ColorButton.Align := alBottom;
  ColorButton.ButtonColor := sBrushColor;
  ColorButton.OnColorChanged := @BrushColorChange;
end;

class procedure TPenStyle.CreatePenStyleComboBox(panel: TPanel);
var
  container: TPanel;
  name: TLabel;
  ComboBox: TComboBox;
  i: Integer;
begin
  container := TPanel.Create(panel);
  container.Parent := panel;
  container.Align := alTop;
  name := TLabel.Create(container);
  name.Parent := container;
  name.Align := alTop;
  name.Caption := 'Pen style';
  ComboBox := TComboBox.Create(container);
  ComboBox.Parent := container;
  ComboBox.Align:= alBottom;
  ComboBox.ReadOnly := true;
  for i := 0 to high(PEN_STYLES) do
    comboBox.Items.Add(PEN_STYLES[i].mName);
  ComboBox.ItemIndex := sPenStyle;
  ComboBox.OnChange := @PenStyleChange;
end;

class procedure TBrushStyle.CreateBrushStyleComboBox(panel: TPanel);
var
  container: TPanel;
  name: TLabel;
  ComboBox: TComboBox;
  i: Integer;
begin
  container := TPanel.Create(panel);
  container.Parent := panel;
  container.Align := alTop;
  name := TLabel.Create(container);
  name.Parent := container;
  name.Align := alTop;
  name.Caption := 'Brush style';
  ComboBox := TComboBox.Create(container);
  ComboBox.Parent := container;
  ComboBox.Align:= alBottom;
  ComboBox.ReadOnly := true;
  for i := 0 to high(BRUSH_STYLES) do
    comboBox.Items.Add(BRUSH_STYLES[i].mName);
  ComboBox.ItemIndex := sBrushStyle;
  ComboBox.OnChange := @BrushStyleChange;
end;

class procedure TRoundRect.CreateRXSpinEdit(panel: TPanel);
var
  container: TPanel;
  name: TLabel;
  Edit: TSpinEdit;
begin
  container := TPanel.Create(panel);
  container.Parent := panel;
  container.Align := alTop;
  name := TLabel.Create(container);
  name.Parent := container;
  name.Align := alTop;
  name.Caption := 'RX';
  Edit := TSpinEdit.Create(container);
  Edit.Parent := container;
  Edit.Align:= alBottom;
  Edit.AutoSelect := false;
  Edit.Value:= sRX;
  Edit.OnChange := @ChangeRX;
end;

class procedure TRoundRect.CreateRYSpinEdit(panel: TPanel);
var
  container: TPanel;
  name: TLabel;
  Edit: TSpinEdit;
begin
  container := TPanel.Create(panel);
  container.Parent := panel;
  container.Align := alTop;
  name := TLabel.Create(container);
  name.Parent := container;
  name.Align := alTop;
  name.Caption := 'RY';
  Edit := TSpinEdit.Create(container);
  Edit.Parent := container;
  Edit.Align:= alBottom;
  Edit.AutoSelect := false;
  Edit.Value:= sRY;
  Edit.OnChange := @ChangeRY;
end;

{Button's handlers}
class procedure TPenStyle.PenStyleChange(Sender: TObject);
begin
  sPenStyle := (Sender as TComboBox).ItemIndex;
end;

class procedure TBrushStyle.BrushStyleChange(Sender: TObject);
begin
  sBrushStyle :=  (Sender as TComboBox).ItemIndex;
end;

class procedure TBrushColor.BrushColorChange(Sender: TObject);
begin
  sBrushColor := (Sender as TColorButton).ButtonColor;
end;

class procedure TPenColor.PenColorChange(Sender: TObject);
begin
  sPenColor := (Sender as TColorButton).ButtonColor;
end;

Class procedure TPenWidth.PenWidthChange(Sender: TObject);
begin
  sPenWidth := (Sender as TSpinEdit).Value;
end;

class procedure TRoundRect.ChangeRX(Sender: TObject);
begin
  sRX := (Sender as TSpinEdit).Value;
end;

Class procedure TRoundRect.ChangeRY(Sender: TObject);
begin
  sRY := (Sender as TSpinEdit).Value;
end;
end.

