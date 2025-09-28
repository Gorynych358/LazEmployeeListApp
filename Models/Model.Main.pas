unit Model.Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, FileUtil, Model.Interfaces, Model.Declarations,
  Model.EmployeeRepository, Dialogs;

function CreateMainModelClass:IMainModelInterface;
implementation
type

  { TModelMain }

  TModelMain= class(TInterfacedObject, IMainModelInterface)
  private const
         EmployeeListSection = 'ElmployeeLists';
         LastUsedList = 'LastUsedListName';
  private
    FINIFullPath,
    FCurrentListName:String;
    FResourcesPath:TDefaultResourcesPath;
    FMainPanelDefaultText:TMainPanelDefaultCaptions;
    FExportExcelTemplateDialogText:TExportExcelTemplateDialogText;
    FListSettingsMenuText:TListSettingsMenuText;
    FNoListExistsDlgText:TNoListExistsDlgText;
    FErrorMessageText:TErrorMessages;
    FEmployeeRepository:IEmployeeRepository;
    FFilterSortingCaptions:TStringList;

    function GetLastUsedListName: String;
    procedure SaveStateOnExit;
  public
    procedure SetEmployeeRepository(const AEmployeeRepository:IEmployeeRepository);
    procedure SetCurrentList(const AListName:String);
    procedure GetExistingListsProperties(var AExistingListsProperties:
              TListPropertiesArray);
    procedure GetEmployeeList(var AEmployeeList: TEmployeeList;
              const AListSortingIndex:Byte);
    procedure DoExportExcelTemplate(const APathToSave:String);
    procedure AddNewEmployee(const ANewEmployee:TEmployee;
              const AListName:String);
    procedure CreateNewEmployeeList(const AListName:String);
    procedure RenameEmployeeList(const ACurrentListName, ANewListName:String);
    procedure RemoveEmployeeList(const AListName:String);
    procedure RemoveEmployeeFromID(const AID: String);
    function GetResourcesPath:TDefaultResourcesPath;
    function GetMainPanelText: TMainPanelDefaultCaptions;
    function GetListSettingsMenuText: TListSettingsMenuText;
    function GetNoListExistsDialogText: TNoListExistsDlgText;
    function GetExportExcelTemplateText: TExportExcelTemplateDialogText;
    function GetErrorMessageText: TErrorMessages;
    function GetCurrentListProperties: TListProperties;
    function GetSortingFileterCaptions:TStringList;
    property EmployeeRepository:IEmployeeRepository write SetEmployeeRepository;
    constructor Create;
    destructor Destroy; override;
  end;

function CreateMainModelClass: IMainModelInterface;
begin
  Result:=TModelMain.Create;
end;

{ TModelMain }

constructor TModelMain.Create;
var
  resRoot,
  UIFolder:String;
begin
  resRoot:=GetCurrentDir + '\Resources';
  UIFolder:=resRoot + '\UI\';
  FResourcesPath.ResoucesFolder:=resRoot;
  FResourcesPath.EmployeeListsFolder:=resRoot + '\EmployeeLists\';
  FResourcesPath.EmployeeAvatarsFolder:=resRoot + '\EmployeeAvatars\';
  FResourcesPath.DefaultAvatarsFolder:=resRoot +
                                        '\EmployeeAvatars\DefaultAvatars\';
  FResourcesPath.UIFolder:=UIFolder;
  FResourcesPath.UIAssetsFolder:=UIFolder + '\UIAssets\';

  FINIFullPath:=GetCurrentDir + 'EmployeeListAppData.data';
  FFilterSortingCaptions:=TStringList.Create;
  FFilterSortingCaptions.Add('Без сортировки');
  FFilterSortingCaptions.Add('Фамилия');
  FFilterSortingCaptions.Add('Дата рождения');
  FFilterSortingCaptions.Add('Дата трудоустройства');

  With FMainPanelDefaultText do
  begin
    AddButtonText:='Добавить';
    RemoveButtonText:='Удалить';
    CurrentListText:='';
    ListFilterText:='Без сортировки';
  end;

  With FExportExcelTemplateDialogText do
  begin
    DlgTitle:='Куда сохранить шаблон списка?';
    DlgDefaultExt:='.xlsx';
    DlgFilter:='Файл Excel|*.xlsx';
    DlgDefaultFileName:='Список сотрудников.xlsx';
  end;

  With FListSettingsMenuText do
  begin
    MenuPanelTitle:='Управление списками';
    BigYourListsNoteText:='Ваши списки';
    BigCreateListNoteText:='Создание нового списка';
    BigRenameListNoteText:='Переименование списка';
    HintCreateListText:='Введите название списка(не более 20 символов)';
    HintRenameListText:='Введите новое название списка(не более 20 символов)';
    ButRemoveListCaption:='Удалить';
    ButRenameListCaption:='Переименовать';
    ButCreateListCaption:='Создать новый' + #10 + 'список';
    ButCancelSetNameCaption:='Отмена';
    ButSaveListNameCaption:='Сохранить';
    //Диалог удаления списка:
    RemoveListDlgCaption:='Удаление списка';
    RemoveListDlgMessage:='Выбранный список содержит записи.' +
    'Вы действительно хотите его удалить?';
    RemoveListDlgYesCaption:='Да';
    RemoveListDlgNoCaption:='Нет';
  end;

  With FNoListExistsDlgText do
  begin
    //MenuPanel
    MenuPanelTitle:='Добро пожаловать';
    //DialogNoListExistsPanel
    BigNoListNoteText:='У вас ещё нет списков.';
    StringButCreateListCaption:='+ Создать новый список';
    //MenuInputListNamePanel
    BigCreateListNoteText:='Давайте создадим новый список';
    HintCreateListText:='Введите название списка';
    ButCancelSetNameCaption:='Отмена';
    ButSaveListNameCaption:='Сохранить';
  end;

  With FErrorMessageText do
  begin
    ListNameEmptyErrorText:= 'Ошибка! Имя списка не может быть пустым.';
    ListNameAlreadyExistsText:= 'Ошибка! Список с таким именем уже существует.';
  end;
end;

procedure TModelMain.SetEmployeeRepository(
  const AEmployeeRepository: IEmployeeRepository);
var
  i:Integer;
  existingListsProperties:TListPropertiesArray;
  listName:string;
  isListNameExistsInStorage:Boolean;
begin
  if Assigned(AEmployeeRepository) then
  begin
    FEmployeeRepository:=AEmployeeRepository;
    listName:=GetLastUsedListName;
    SetLength(existingListsProperties, 0);
    GetExistingListsProperties(existingListsProperties);
    if Length(existingListsProperties) = 0 then Exit;

    //Проверяем содержится ли в хранилище списков название последнего
    //используемого списка.
    isListNameExistsInStorage:=False;

    if listName <> '' then
    begin
      for i:=Low(existingListsProperties) to High(existingListsProperties) do
      begin
        if existingListsProperties[i].ListTitle = listName then
        begin
          isListNameExistsInStorage:=True;
          break;
        end;
      end;
    end;
    //Если нет, то устанавливаем первое имя списка в качестве текущего.
    if not isListNameExistsInStorage then
      listName:=existingListsProperties[0].ListTitle;

    SetCurrentList(listName);
    SetLength(existingListsProperties, 0);
  end;
end;

procedure TModelMain.SetCurrentList(const AListName: String);
begin
  FCurrentListName:=AListName;
end;

function TModelMain.GetCurrentListProperties: TListProperties;
var
  existingListsProperties: TListPropertiesArray;
  listProps:TListProperties;
begin
  Result:=Default(TListProperties);
  GetExistingListsProperties(existingListsProperties);
  if Assigned(existingListsProperties) and (Length(existingListsProperties) > 0)
  then begin
    for listProps in existingListsProperties do
    begin
      if listProps.ListTitle=FCurrentListName then
      begin
        Result:=ListProps;
        break;
      end;
    end;
  end;
  SetLength(existingListsProperties, 0);
end;

procedure TModelMain.GetExistingListsProperties(
  var AExistingListsProperties: TListPropertiesArray);
begin
  FEmployeeRepository.GetExistingListsProperties(AExistingListsProperties);
end;

procedure TModelMain.GetEmployeeList(var AEmployeeList: TEmployeeList;
  const AListSortingIndex:Byte);
begin
  if FCurrentListName = '' then Exit;
  FEmployeeRepository.GetEmployeeList(AEmployeeList, AListSortingIndex,
  FCurrentListName);
end;

procedure TModelMain.DoExportExcelTemplate(const APathToSave: String);
var
   pathToExcelTemplate:String;
begin
  pathToExcelTemplate:=FResourcesPath.EmployeeListsFolder +
    'ExprotEmployeeListTemplate.xlsx';
  CopyFile(pathToExcelTemplate, APathToSave);
end;

procedure TModelMain.AddNewEmployee(const ANewEmployee: TEmployee;
  const AListName: String);
begin

end;

procedure TModelMain.CreateNewEmployeeList(const AListName: String);
begin
  FEmployeeRepository.CreateNewEmployeeList(AListName);
  if FCurrentListName = '' then
     FCurrentListName:=AListName;
end;

procedure TModelMain.RenameEmployeeList(const ACurrentListName,
  ANewListName: String);
begin
  if ACurrentListName=FCurrentListName then FCurrentListName:=ANewListName;
  FEmployeeRepository.RenameEmployeeList(ACurrentListName, ANewListName);
end;

procedure TModelMain.RemoveEmployeeList(const AListName: String);
var
  existingListsProperties: TListPropertiesArray;
begin
  FEmployeeRepository.RemoveEmployeeList(AListName);
  if AListName=FCurrentListName then
  begin
    GetExistingListsProperties(existingListsProperties);
    if Assigned(existingListsProperties) and
       (Length(existingListsProperties) > 1) then
    begin
      FCurrentListName:=existingListsProperties[0].ListTitle;
    end else
        FCurrentListName:='';
  end;

  SetLength(existingListsProperties, 0);
end;

procedure TModelMain.RemoveEmployeeFromID(const AID: String);
begin
  FEmployeeRepository.RemoveEmployeeFromID(AID, FCurrentListName);
end;

function TModelMain.GetResourcesPath: TDefaultResourcesPath;
begin
  Result:=FResourcesPath;
end;

function TModelMain.GetExportExcelTemplateText: TExportExcelTemplateDialogText;
begin
  Result:=FExportExcelTemplateDialogText;
end;

function TModelMain.GetErrorMessageText: TErrorMessages;
begin
  Result:=FErrorMessageText;
end;

function TModelMain.GetMainPanelText: TMainPanelDefaultCaptions;
begin
  Result:=FMainPanelDefaultText;
end;

function TModelMain.GetListSettingsMenuText: TListSettingsMenuText;
begin
  Result:=FListSettingsMenuText;
end;

function TModelMain.GetNoListExistsDialogText: TNoListExistsDlgText;
begin
  Result:=FNoListExistsDlgText;
end;

function TModelMain.GetSortingFileterCaptions: TStringList;
begin
  Result:=FFilterSortingCaptions;
end;

function TModelMain.GetLastUsedListName:String;
var
 tmpINIFile:TIniFile;
 lastUsedListName:String;
begin
 lastUsedListName:='';
 tmpINIFile:=TIniFile.Create(FINIFullPath);
 try
  lastUsedListName:=tmpINIFile.ReadString(EmployeeListSection, LastUsedList, '');
 finally
   tmpINIFile.Free;
   Result:=lastUsedListName;
 end;
end;

procedure TModelMain.SaveStateOnExit;
var
 currentListName: TListProperties;
 tmpINIFile:TIniFile;
begin
  currentListName:=GetCurrentListProperties;
  tmpINIFile:=TIniFile.Create(FINIFullPath);
  try
   tmpINIFile.WriteString(EmployeeListSection, LastUsedList,
     currentListName.ListTitle);
  finally
   tmpINIFile.Free;
  end;
end;

destructor TModelMain.Destroy;
begin
  SaveStateOnExit;
  FreeAndNil(FFilterSortingCaptions);
  inherited;
end;

end.

