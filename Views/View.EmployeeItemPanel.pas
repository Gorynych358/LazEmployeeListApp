unit View.EmployeeItemPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, SysUtils,
  Model.Declarations;

type
  TEmployeeItemState = (OverState, OutState);
  { TEmployeeItemPanel }

  TEmployeeItemPanel = class(TPanel)
  private
    FResourcesPath:TDefaultResourcesPath;
    FItemID:String;
    FIsSelected:Boolean;
    FNextButtonIconImage:TImage;
    FNextButtonBackImage:TImage;
    FPanelTransparenCoverImage:TImage;
    FSelectedIconImage:TImage;
    FPanelOutImage:TImage;
    FPanelOverImage:TImage;
    FAvatara:TImage;
    FPanelBackImage:TImage;
    FFullNameLbl,
    FJobPositionLbl:TLabel;
    FItemMouseDownEvent:TMouseEvent;
    FNextButtonMouseDownEvent:TMouseEvent;


    procedure CreateEmployeeItem;
    procedure OnEmployeeItemMouseOut(Sender: TObject);
    procedure OnEmployeeItemMouseOver(Sender: TObject);
    procedure OnNextButtonMouseOut(Sender: TObject);
    procedure OnNextButtonMouseOver(Sender: TObject);
    procedure SetIsSelected(AValue: Boolean);
  public
    Property ItemID:String read FItemID;
    Property IsSelected:Boolean read FIsSelected write SetIsSelected;
    procedure Initialize(AEmployeeDTO:TMainEmployeeDTO);
    procedure AddItemTo(AOwner:TWinControl);
    procedure RemoveItem;
    procedure SetItemMouseDown(const AMouseEvent:TMouseEvent);
    procedure SetNextButtonMouseDown(const AMouseEvent:TMouseEvent);
    procedure SetEmplyeeItemState(const AState:TEmployeeItemState);
    constructor CreateNew(APath: TDefaultResourcesPath);
    destructor Destroy; override;
  end;


implementation

{ TEmployeeItemPanel }

procedure TEmployeeItemPanel.CreateEmployeeItem;
var
  FFont:TFont;
  FUIPath,
  FUIAssetsPath:String;
begin
  FFont:=TFont.Create;
  FFont.Name:='Arial Narrow';
  FFont.Style:=[fsBold];
  FFont.Size:=16;
  FFont.Color:=clDefault;
  FUIPath:=FResourcesPath.UIFolder;
  FUIAssetsPath:=FResourcesPath.UIAssetsFolder;
  //ShowMessage('ui path: ' + FUIPath + ', ui assets path: ' + FUIAssetsPath);
  //Иконка выделения:
  FSelectedIconImage:=TImage.Create(Self);
  with FSelectedIconImage do
  begin
     Name:='ItemSelIcon';
     Parent:=Self; // Задаем родителя
     AutoSize:=false;//Запрещаем автомасштабирование
     Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
     Stretch:=true;//Разрешаем растягивание
     if(FileExists(FUIAssetsPath + 'employee_item_selected_icon.png')) then
        Picture.LoadFromFile(FUIAssetsPath + 'employee_item_selected_icon.png');
     Width:=Picture.Width;  // Ширина
     Height:=Picture.Height; // Высота
     Left:=0; // Позиция по горизонтали
     Top:=Self.Height div 2 - Width div 2;  // Позиция по вертикали
  end;

  //Голубая подложка под аватару:
  FPanelBackImage:=TImage.Create(Self);
  with FPanelBackImage do
  begin 
     Name:='PanelBackImage';
     Parent:=Self; // Задаем родителя
     AutoSize:=false;//Запрещаем автомасштабирование
     Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
     Stretch:=true;//Разрешаем растягивание
     if(FileExists(FUIAssetsPath + 'employee_item_ava_back.png')) then
        Picture.LoadFromFile(FUIAssetsPath + 'employee_item_ava_back.png');
     Width:=Picture.Width;  // Ширина
     Height:=Picture.Height; // Высота
     Left:=0; // Позиция по горизонтали
     Top:=2;  // Позиция по вертикали
  end;
  //Аватарка.
  FAvatara:=TImage.Create(Self);
  with FAvatara do
  begin
     Name:='AvaImage';
     Parent:=Self; // Задаем родителя
     AntialiasingMode:=amOn;
     Left:=6; // Позиция по горизонтали
     Top:=8;  // Позиция по вертикали
     Width:=64;  // Ширина
     Height:=64; // Высота
     AutoSize:=false;//Запрещаем автомасштабирование
     Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
     Stretch:=true;//Разрешаем растягивание
  end;

  //Голубая плашка которая отображается при наведении:
  FPanelOverImage:=TImage.Create(Self);
  with FPanelOverImage do
  begin
     Name:='PanelOverImage';
     Parent:=Self; // Задаем родителя
     AutoSize:=false;//Запрещаем автомасштабирование
     Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
     Stretch:=true;//Разрешаем растягивание
     if(FileExists(FUIAssetsPath + 'employee_item_solid_back.png')) then
        Picture.LoadFromFile(FUIAssetsPath + 'employee_item_solid_back.png');
     Width:=Picture.Width;  // Ширина
     Height:=Picture.Height; // Высота
     Left:=0; // Позиция по горизонтали
     Top:=0;  // Позиция по вертикали
  end;

  //Градиентная плашка которая отображается при отваде курсора:
  FPanelOutImage:=TImage.Create(Self);
  with FPanelOutImage do
  begin
     Name:='PanelOutImage';
     Parent:=Self; // Задаем родителя
     AutoSize:=false;//Запрещаем автомасштабирование
     Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
     Stretch:=true;//Разрешаем растягивание
     if(FileExists(FUIAssetsPath + 'employee_item_gradient_back.png')) then
        Picture.LoadFromFile(FUIAssetsPath + 'employee_item_gradient_back.png');
     Width:=Picture.Width;
     Height:=Picture.Height;
     Left:=0; // Позиция по горизонтали
     Top:=0;  // Позиция по вертикали
  end;
  //Label ФИО.
  FFullNameLbl:=TLabel.Create(Self);
  with FFullNameLbl do
  begin
     Name:='lblFullName';
     Parent:=Self; // Задаем родителя
     Font:=FFont;
     Caption:='';
     Width:=Self.Width - 50;  // Ширина
     Height:=30; // Высота
     Left:=30 + ((Self.Width - 30) div 2 - Width div 2); // Позиция по горизонтали
     Top:=6;  // Позиция по вертикали
     AutoSize:=false;
     Alignment:=taCenter;
  end;

  //Label должность.
  FFont.Name:='Arial';
  FFont.Color:=$009933CC;
  FFont.Size:=12;
  FJobPositionLbl:=TLabel.Create(Self);
  with FJobPositionLbl do
  begin
     Name:='lblJobPosition';
     Parent:=Self; // Задаем родителя
     Font:=FFont;
     Caption:='';
     Width:=Self.Width - 50;  // Ширина
     Height:=30; // Высота
     Left:= 30 + ((Self.Width - 30) div 2 - Width div 2); // Позиция по горизонтали
     Top:=44;  // Позиция по вертикали
     AutoSize:=false;
     Alignment:=taCenter;
  end;

  //Image для привязки событий мыши
  FPanelTransparenCoverImage:=TImage.Create(Self);
  with FPanelTransparenCoverImage do
  begin
     Name:='PanelTransparentImage';
     Parent:=Self;
     AutoSize:=false;//Запрещаем автомасштабирование
     Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
     Stretch:=true;//Разрешаем растягивание
     if(FileExists(FUIPath + 'UIAssets\employee_item_transparent_cover.png')) then
        Picture.LoadFromFile(FUIPath + 'UIAssets\employee_item_transparent_cover.png');
     Cursor:=crHandPoint;
     Width:=Picture.Width;  // Ширина
     Height:=Picture.Height; // Высота
     Left:=0; // Позиция по горизонтали
     Top:=0;  // Позиция по вертикали
  end;

  //Image подложка кнопки "Далее"
  FNextButtonBackImage:=TImage.Create(Self);
  with FNextButtonBackImage do
  begin
     Visible:=False;
     Parent:=Self;
     AutoSize:=false;//Запрещаем автомасштабирование
     Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
     Stretch:=true;//Разрешаем растягивание
     if(FileExists(FUIPath + 'UIAssets\button_next_back.png')) then
        Picture.LoadFromFile(FUIPath + 'UIAssets\button_next_back.png');
     Cursor:=crHandPoint;
     Width:=Picture.Width;  // Ширина
     Height:=Picture.Height; // Высота
     Left:=420 - Width div 2; // Позиция по горизонтали
     Top:=36 - Height div 2;  // Позиция по вертикали
  end;

  //Image иконка кнопки "Далее"
  FNextButtonIconImage:=TImage.Create(Self);
  with FNextButtonIconImage do
  begin
     Parent:=Self;
     AutoSize:=false;//Запрещаем автомасштабирование
     Proportional:=true;//Устанавливаем пропорциональное растяжение картинки
     Stretch:=true;//Разрешаем растягивание
     if(FileExists(FUIPath + 'UIAssets\button_next_icon.png')) then
        Picture.LoadFromFile(FUIPath + 'UIAssets\button_next_icon.png');
     Cursor:=crHandPoint;
     Width:=Picture.Width;  // Ширина
     Height:=Picture.Height; // Высота
     Left:=420 - Width div 2; // Позиция по горизонтали
     Top:=36 - Height div 2;  // Позиция по вертикали
  end;
  FFont.Free;
end;

procedure TEmployeeItemPanel.Initialize(AEmployeeDTO: TMainEmployeeDTO);
var
  i:Integer;
  FDefaultAvatarPath,
  FUserAvatarPath:String;
begin
  FItemID:=AEmployeeDTO.ID;
  if AEmployeeDTO.AvatarName='' then
    begin
       //каталог с дефолтными аватарками
       FDefaultAvatarPath:=FResourcesPath.DefaultAvatarsFolder;
       if AEmployeeDTO.Gender = 'Мужской' then
       begin
         i:=1+random(12);
         FUserAvatarPath:=FDefaultAvatarPath +
                     'DefaultMan64_' + IntToStr(i) + '.png';
       end;
       if AEmployeeDTO.Gender = 'Женский' then
       begin
          i:=1+random(12);
          FUserAvatarPath:=FDefaultAvatarPath +
                      'DefaultWoman64_' + IntToStr(i) + '.png';
       end;
    end
  else
  begin
     FUserAvatarPath:=FResourcesPath.EmployeeAvatarsFolder +
                                               AEmployeeDTO.AvatarName;
  end;
  //Добавляем аватару:
  if(FileExists(FUserAvatarPath)) then
        FAvatara.Picture.LoadFromFile(FUserAvatarPath)
  else
      ShowMessage('Ava not exists');

  // Сокращённое ФИО:
  FFullNameLbl.Caption:=AEmployeeDTO.ShortFullName;

  //Должность:
  FJobPositionLbl.Caption:=AEmployeeDTO.JobPosition;
end;

procedure TEmployeeItemPanel.AddItemTo(AOwner: TWinControl);
begin
  Self.Parent:=AOwner;
  FPanelTransparenCoverImage.OnMouseEnter:=@OnEmployeeItemMouseOver;
  FPanelTransparenCoverImage.OnMouseLeave:=@OnEmployeeItemMouseOut;
  if Assigned(FItemMouseDownEvent) then
     FPanelTransparenCoverImage.OnMouseDown:=FItemMouseDownEvent;

  FNextButtonIconImage.OnMouseEnter:=@OnNextButtonMouseOver;
  FNextButtonIconImage.OnMouseLeave:=@OnNextButtonMouseOut;
  if Assigned(FNextButtonMouseDownEvent) then
     FNextButtonIconImage.OnMouseDown:=FNextButtonMouseDownEvent;
end;

procedure TEmployeeItemPanel.RemoveItem();
begin
  Self.Parent:=Nil;
  //Убираем выделение, если было:
  SetIsSelected(false);
  //Удаляем аватару:
  FAvatara.Picture.Clear;
  // Сокращённое ФИО:
  FFullNameLbl.Caption:='';
  //Должность:
  FJobPositionLbl.Caption:='';
  FPanelTransparenCoverImage.OnMouseEnter:=Nil;
  FPanelTransparenCoverImage.OnMouseLeave:=Nil;
  FItemMouseDownEvent:=Nil;
  FNextButtonIconImage.OnMouseEnter:=Nil;
  FNextButtonIconImage.OnMouseLeave:=Nil;
  FNextButtonMouseDownEvent:=Nil;
end;
{-------------------------------------------------------------------------}
{----------------------Item panel listeners-------------------------------}
{-------------------------------------------------------------------------}
procedure TEmployeeItemPanel.SetItemMouseDown(const AMouseEvent: TMouseEvent);
begin
  if Assigned(AMouseEvent) then
     FItemMouseDownEvent:=AMouseEvent;
end;

procedure TEmployeeItemPanel.OnEmployeeItemMouseOver(Sender: TObject);
begin
  //if IsSelected=False then
     SetEmplyeeItemState(TEmployeeItemState.OverState);
end;

procedure TEmployeeItemPanel.OnEmployeeItemMouseOut(Sender: TObject);
begin
  //if IsSelected=False then
     SetEmplyeeItemState(TEmployeeItemState.OutState);
end;

{------------------------------------------------------------------------}
{-----------------------Next button listeners----------------------------}
{------------------------------------------------------------------------}

procedure TEmployeeItemPanel.SetNextButtonMouseDown(
  const AMouseEvent: TMouseEvent);
begin
  if Assigned(AMouseEvent) then
     FNextButtonMouseDownEvent:=AMouseEvent;
end;

procedure TEmployeeItemPanel.OnNextButtonMouseOver(Sender: TObject);
begin
  FNextButtonBackImage.Visible:=True;
  SetEmplyeeItemState(TEmployeeItemState.OverState);
end;

procedure TEmployeeItemPanel.OnNextButtonMouseOut(Sender: TObject);
begin
  FNextButtonBackImage.Visible:=False;
  FPanelOutImage.Visible:=False;
end;

procedure TEmployeeItemPanel.SetIsSelected(AValue: Boolean);
var offsetX:Integer;
begin
  if FIsSelected=AValue then Exit;
  FIsSelected:=AValue;

  offsetX:=30;
  if FIsSelected then
  begin

     FPanelTransparenCoverImage.Left:=FPanelTransparenCoverImage.Left + offsetX;
     FPanelOutImage.Left:=FPanelOutImage.Left + offsetX;
     FPanelOverImage.Left:=FPanelOverImage.Left + offsetX;
     FAvatara.Left:=FAvatara.Left + offsetX;
     FPanelBackImage.Left:=FPanelBackImage.Left + offsetX;
  end
  else
  begin
     FPanelTransparenCoverImage.Left:=FPanelTransparenCoverImage.Left - offsetX;
     FPanelOutImage.Left:=FPanelOutImage.Left - offsetX;
     FPanelOverImage.Left:=FPanelOverImage.Left - offsetX;
     FAvatara.Left:=FAvatara.Left - offsetX;
     FPanelBackImage.Left:=FPanelBackImage.Left - offsetX;
  end;
end;

procedure TEmployeeItemPanel.SetEmplyeeItemState(
  const AState: TEmployeeItemState);
begin
  if AState = OverState then
  begin
     FPanelOutImage.Visible:=False;
  end
  else
      FPanelOutImage.Visible:=True;
end;

constructor TEmployeeItemPanel.CreateNew(APath: TDefaultResourcesPath);
begin
  inherited Create(Nil);
  FResourcesPath:=APath;
  //Панель-контейнер для контроллов с инофрмацией о сотруднике
  //Self.Name:='EmployeeItem';
  Self.Left:=0; // Позиция по горизонтали
  Self.Top:=0;  // Позиция по вертикали
  Self.Width:=454;  // Ширина
  Self.Height:=73; // Высота
  Self.BevelInner:=bvNone;
  Self.BevelOuter:=bvNone;
  Self.Caption:='';  // Тескт убираем
  CreateEmployeeItem;
end;

destructor TEmployeeItemPanel.Destroy;
begin
  if Assigned(FPanelOutImage) then FreeAndNil(FPanelOutImage);
  if Assigned(FPanelOverImage) then FreeAndNil(FPanelOverImage);
  if Assigned(FPanelBackImage) then FreeAndNil(FPanelBackImage);
  if Assigned(FAvatara) then FreeAndNil(FAvatara);
  if Assigned(FFullNameLbl) then FreeAndNil(FFullNameLbl);
  if Assigned(FJobPositionLbl) then FreeAndNil(FJobPositionLbl);
  if Assigned(FPanelTransparenCoverImage) then
     FreeAndNil(FPanelTransparenCoverImage);
  inherited Destroy;
end;

end.

