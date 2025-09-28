unit View.EmployeeDetailPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, Clipbrd, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, DateTimePicker, Model.Declarations, LCLType, LCLIntf;

type
  TDetailState = (dsNormalState, dsChangeState);
  TPropertyChangeNotifyEvent =
    procedure(Sender: TObject; AProperty:String) of object;
  { TEmployeeDetailPanel }

  TEmployeeDetailPanel = class(TPanel)
  private
    FPropertyDetailData:TPropertyDetailData;
    FButtonIconsList:TImageList;
    //Панель отображения поля:
    FShowPropertyPanel:TPanel;
    FShowPropertyBack:TImage;
    FShowPropertyName:TLabel;
    FShowPropertyValue:TLabel;
    FShowPropertyCopySB:TSpeedButton;
    FShowPropertyChangeSB:TSpeedButton;
    //Панель редактирования поля:
    FChangePropertyPanel:TPanel;
    FChangePropertyBack:TImage;
    FChangePropertyHint:TLabel;
    FChangePropertyInputEdit:TEdit;
    FChangePropertyDatePicker:TDateTimePicker;
    FChangePropertyClosePanelSB:TSpeedButton;
    FChangePropertySaveChangesSB:TSpeedButton;

    FPropertyChangedEvent:TPropertyChangeNotifyEvent;
    FOnClickEdit: TNotifyEvent;

    procedure CreateEmployeeDetailItem;
    procedure OnKeyboardKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnPropertyEditButtonClick(Sender: TObject);
    procedure OnPropertyCopyButtonClick(Sender: TObject);
    procedure OnSaveButtonClick(Sender: TObject);
    procedure OnCancelChangesButtonClick(Sender: TObject);
  public
    procedure UpdatePropertyDetailData(APropertyDetailData: TPropertyDetailData);
    procedure SetPropertyChangedCallback(const APropertyChangedCallback:TPropertyChangeNotifyEvent);
    procedure SetEditPropertyClickCallback(const AEditClickCallback:TNotifyEvent);
    procedure SetEmplyeeDetailState(const AState:TDetailState);
    constructor CreateNew(AOwner:TComponent;
                APropertyDetailData:TPropertyDetailData;
                AButtonIconsList:TImageList);
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;


implementation

{ TEmployeeDetailPanel }

procedure TEmployeeDetailPanel.CreateEmployeeDetailItem;
var
  FFont:TFont;
  FUIPath,
  FUIAssetsPath:String;
  dt:TDate;
begin
  FFont:=TFont.Create;
  FFont.Name:='Arial';
  //FFont.Style:=[fsBold];
  FFont.Size:=10;
  FFont.Color:=clDefault;
  FUIPath:=GetCurrentDir + '\Resources\UI\';
  FUIAssetsPath:=FUIPath + 'UIAssets\';
  ///-------------------------------------------------------------///
  //Панель отображения поля:--------------------------------------///
  ///-------------------------------------------------------------///
  FShowPropertyPanel:=TPanel.Create(Self);
  With FShowPropertyPanel do
  begin
    Parent:=Self;
    Caption:='';
    Visible:=False;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    Width:=420;
    Height:=70;
    Color:=$0089BBE0;
  end;
  //Подложка панели отображения информации свойства:
  FShowPropertyBack:=TImage.Create(FShowPropertyPanel);
  With FShowPropertyBack do
  begin
    Parent:=FShowPropertyPanel; // Задаем родителя
    AutoSize:=false;//Запрещаем автомасштабирование
    Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
    Stretch:=true;//Разрешаем растягивание
    if(FileExists(FUIAssetsPath + 'string_item_back.png')) then
       Picture.LoadFromFile(FUIAssetsPath + 'string_item_back.png');
    Width:=Picture.Width;  // Ширина
    Height:=Picture.Height; // Высота
  end;
  //Название характеристики:
  FShowPropertyName:=TLabel.Create(FShowPropertyPanel);
  with FShowPropertyName do
  begin
     Name:='lblTitle';
     Parent:=FShowPropertyPanel; // Задаем родителя
     Font:=FFont;
     Caption:=FPropertyDetailData.PropertyName;
     Left:=16;// Позиция по горизонтали
     Top:=8;  // Позиция по вертикали
     AutoSize:=true;
     Alignment:=taLeftJustify;
  end;
  FFont.Size:=16;
  FFont.Color:=$00FF0099;
  //Значение характеристики:
  FShowPropertyValue:=TLabel.Create(FShowPropertyPanel);
  with FShowPropertyValue do
  begin
     Name:='lblValue';
     Parent:=FShowPropertyPanel; // Задаем родителя
     Font:=FFont;
     Caption:=FPropertyDetailData.PropertyValue;
     Left:=16;// Позиция по горизонтали
     Top:=28;  // Позиция по вертикали
     AutoSize:=true;
     Alignment:=taLeftJustify;
  end;

  //Кнопка копирования значения характеристики в буфер обмена:
  FShowPropertyCopySB:=TSpeedButton.Create(FShowPropertyPanel);
  With FShowPropertyCopySB do
  begin
    Parent:=FShowPropertyPanel;
    Caption:='';
    ShowCaption:=False;
    Flat:=True;
    Cursor:=crHandPoint;
    Images:=FButtonIconsList;
    ImageWidth:=24;
    ImageIndex:=3;
    HotImageIndex:=4;
    PressedImageIndex:=5;
    Left:=344;// Позиция по горизонтали
    Top:=4;  // Позиция по вертикали
    OnClick:=@OnPropertyCopyButtonClick;
  end;
  //Кнопка редактирования значения характеристики:
  FShowPropertyChangeSB:=TSpeedButton.Create(FShowPropertyPanel);
  With FShowPropertyChangeSB do
  begin
    Parent:=FShowPropertyPanel;
    Caption:='';
    ShowCaption:=False;
    Flat:=True;
    Cursor:=crHandPoint;
    Images:=FButtonIconsList;
    ImageWidth:=24;
    ImageIndex:=0;
    HotImageIndex:=1;
    PressedImageIndex:=2;
    Left:=376;// Позиция по горизонтали
    Top:=4;  // Позиция по вертикали
    OnClick:=@OnPropertyEditButtonClick;
  end;
  
  ///-------------------------------------------------------------///
  //--Панель редактирования поля:---------------------------------///
  ///-------------------------------------------------------------///
  FFont.Size:=10;
  FFont.Color:=clDefault;
  FChangePropertyPanel:=TPanel.Create(Self);
  With FChangePropertyPanel do
  begin
    Parent:=Self;
    Caption:='';
    Visible:=False;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    Width:=420;
    Height:=70;
    Color:=$0089BBE0;
  end;
  //Подложка панели редактирования информации свойства:
  FChangePropertyBack:=TImage.Create(FChangePropertyPanel);
  With FChangePropertyBack do
  begin
    Parent:=FChangePropertyPanel; // Задаем родителя
    AutoSize:=false;//Запрещаем автомасштабирование
    Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
    Stretch:=true;//Разрешаем растягивание
    if(FileExists(FUIAssetsPath + 'string_item_back.png')) then
       Picture.LoadFromFile(FUIAssetsPath + 'string_item_back.png');
    Width:=Picture.Width;  // Ширина
    Height:=Picture.Height; // Высота
  end;

  //Подсказка/ошибка редактирования:
  FFont.Size:=10;
  FFont.Color:=$00666666;

  FChangePropertyHint:=TLabel.Create(FChangePropertyPanel);
  with FChangePropertyHint do
  begin
     Name:='lblHint';
     Parent:=FChangePropertyPanel; // Задаем родителя
     Font:=FFont;
     Caption:=FPropertyDetailData.PropertyHint;
     Left:=16;// Позиция по горизонтали
     Top:=8;  // Позиция по вертикали
     AutoSize:=true;
     Alignment:=taLeftJustify;
  end;
  //Если тип панели - строковой, добавляем поле ввода, в противном случае,
  //добавляем компонент выбора даты:
  FFont.Size:=14;
  FFont.Color:=clDefault;
  if FPropertyDetailData.PropertyType = TDetailPropertyType.dptStringType then
  begin
    FChangePropertyInputEdit:=TEdit.Create(FChangePropertyPanel);
    with FChangePropertyInputEdit do
    begin
      Name:='LabelEditPropertyValue';
      Parent:=FChangePropertyPanel; // Задаем родителя
      Font:=FFont;
      Text:=FPropertyDetailData.PropertyValue;
      MaxLength:=28;
      AutoSize:=true;
      Alignment:=taLeftJustify;
      Width:=300;
      Left:=16;// Позиция по горизонтали
      Top:=24;  // Позиция по вертикали
      OnKeyDown:=@OnKeyboardKeyDown;
    end;
  end else//Компонент выбора даты:
  begin
    FChangePropertyDatePicker:=TDateTimePicker.Create(FChangePropertyPanel);
    With FChangePropertyDatePicker do
    begin
      Name:='DateTimePicker';
      Parent:=FChangePropertyPanel;
      Font:=FFont;
      DateSeparator:='.';

      if TryStrToDate(FPropertyDetailData.PropertyValue, dt) then
         Date:=dt
      else
          Date:=Now;
      Left:=16;// Позиция по горизонтали
      Top:=32;  // Позиция по вертикали
      OnKeyDown:=@OnKeyboardKeyDown;
    end;
  end;

  //Кнопка сохранения изменений:
  FChangePropertySaveChangesSB:=TSpeedButton.Create(FChangePropertyPanel);
  With FChangePropertySaveChangesSB do
  begin
    Parent:=FChangePropertyPanel;
    Caption:='';
    ShowCaption:=False;
    Flat:=True;
    Cursor:=crHandPoint;
    Images:=FButtonIconsList;
    ImageWidth:=24;
    ImageIndex:=9;
    HotImageIndex:=10;
    PressedImageIndex:=11;
    Left:=344;// Позиция по горизонтали
    Top:=40;  // Позиция по вертикали
    OnClick:=@OnSaveButtonClick;
  end;
  //Кнопка закрытия панели редактирования:
  FChangePropertyClosePanelSB:=TSpeedButton.Create(FChangePropertyPanel);
  With FChangePropertyClosePanelSB do
  begin
    Parent:=FChangePropertyPanel;
    Caption:='';
    ShowCaption:=False;
    Flat:=True;
    Cursor:=crHandPoint;
    Images:=FButtonIconsList;
    ImageWidth:=24;
    ImageIndex:=6;
    HotImageIndex:=7;
    PressedImageIndex:=8;
    Left:=384;// Позиция по горизонтали
    Top:=40;  // Позиция по вертикали
    OnClick:=@OnCancelChangesButtonClick;
  end;

  FFont.Free;
end;

{-------------------------------------------------------------------------}
{------------------Show panel buttons listeners---------------------------}
{-------------------------------------------------------------------------}
procedure TEmployeeDetailPanel.SetEditPropertyClickCallback(
  const AEditClickCallback: TNotifyEvent);
begin
  if Assigned(AEditClickCallback) then
     FOnClickEdit:=AEditClickCallback;
end;

procedure TEmployeeDetailPanel.OnPropertyEditButtonClick(Sender: TObject);
begin
  if Assigned(FOnClickEdit) then
    FOnClickEdit(Self);
  SetEmplyeeDetailState(TDetailState.dsChangeState);
end;

procedure TEmployeeDetailPanel.OnPropertyCopyButtonClick(Sender: TObject);
begin
  Clipboard.AsText:=FPropertyDetailData.PropertyValue;
end;

{------------------------------------------------------------------------}
{-----------------------Change panel buttons listeners-------------------}
{------------------------------------------------------------------------}

procedure TEmployeeDetailPanel.SetPropertyChangedCallback(
  const APropertyChangedCallback: TPropertyChangeNotifyEvent);
begin
  if Assigned(APropertyChangedCallback) then
     FPropertyChangedEvent:=APropertyChangedCallback;
end;

procedure TEmployeeDetailPanel.OnSaveButtonClick(Sender: TObject);
var propertyValue:String;
begin
  if Assigned(FPropertyChangedEvent) then
  begin
    if FPropertyDetailData.PropertyType=TDetailPropertyType.dptStringType then
       propertyValue:=FChangePropertyInputEdit.Text
    else
      propertyValue:=DateToStr(FChangePropertyDatePicker.Date);
    FPropertyChangedEvent(Self, propertyValue);
  end;
end;

procedure TEmployeeDetailPanel.OnCancelChangesButtonClick(Sender: TObject);
begin
  SetEmplyeeDetailState(TDetailState.dsNormalState);
end;

procedure TEmployeeDetailPanel.OnKeyboardKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
      OnSaveButtonClick(Sender);
  if Key = VK_ESCAPE then
      OnCancelChangesButtonClick(Sender);
end;


procedure TEmployeeDetailPanel.UpdatePropertyDetailData(
  APropertyDetailData:TPropertyDetailData);
begin
  if APropertyDetailData.PropertyIsChanged then
  begin
    FShowPropertyName.Caption:=APropertyDetailData.PropertyName +
      APropertyDetailData.PropertyChangedCaption;
    FPropertyDetailData:=APropertyDetailData;
    FShowPropertyValue.Caption:=FPropertyDetailData.PropertyValue;
  end;

  SetEmplyeeDetailState(TDetailState.dsNormalState);
end;

procedure TEmployeeDetailPanel.SetEmplyeeDetailState(
  const AState: TDetailState);
var dt:TDate;
begin
  if AState = TDetailState.dsNormalState then
  begin
    FShowPropertyPanel.Show;
    FShowPropertyPanel.Enabled:=True;
    FChangePropertyPanel.Hide;
    FChangePropertyPanel.Enabled:=False;
  end else
  begin
    FChangePropertyPanel.Show;
    FChangePropertyPanel.Enabled:=True;
    FShowPropertyPanel.Hide;
    FShowPropertyPanel.Enabled:=False;
    if FPropertyDetailData.PropertyType=TDetailPropertyType.dptStringType then
    begin
      FChangePropertyInputEdit.Text:=FPropertyDetailData.PropertyValue;
      FChangePropertyInputEdit.SetFocus;
    end else
    begin
      FChangePropertyDatePicker.Date:=Now;
      if TryStrToDate(FPropertyDetailData.PropertyValue, dt) then
         FChangePropertyDatePicker.Date:=dt
    end;
  end;
end;

constructor TEmployeeDetailPanel.CreateNew(AOwner: TComponent;
  APropertyDetailData: TPropertyDetailData; AButtonIconsList: TImageList);
begin
  Create(AOwner);
  //Self.Parent:=AOwner;
  Self.Width:=420;  // Ширина
  Self.Height:=70; // Высота
  Self.BevelInner:=bvNone;
  Self.BevelOuter:=bvNone;
  Self.Caption:='';  // Тескт убираем
  FPropertyDetailData:=APropertyDetailData;
  FButtonIconsList:=AButtonIconsList;
  CreateEmployeeDetailItem;
  SetEmplyeeDetailState(TDetailState.dsNormalState);
end;

constructor TEmployeeDetailPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TEmployeeDetailPanel.Destroy;
begin
  {if Assigned(FPanelOutImage) then FreeAndNil(FPanelOutImage);
  if Assigned(FPanelOverImage) then FreeAndNil(FPanelOverImage);
  if Assigned(FPanelBackImage) then FreeAndNil(FPanelBackImage);
  if Assigned(FAvatara) then FreeAndNil(FAvatara);
  if Assigned(FFullNameLbl) then FreeAndNil(FFullNameLbl);
  if Assigned(FJobPositionLbl) then FreeAndNil(FJobPositionLbl);
  if Assigned(FPanelTransparenCoverImage) then FreeAndNil(FPanelTransparenCoverImage);}
  inherited Destroy;
end;

end.

