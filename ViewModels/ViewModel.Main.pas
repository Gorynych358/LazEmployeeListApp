unit ViewModel.Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, Model.Interfaces, Model.Declarations, Dialogs,
  Model.ProSu.Interfaces, Model.ProSu.Provider, Model.ProSu.InterfaceActions;



function CreateMainViewModelClass: IMainViewModelInterface;

implementation

  { TMainViewModel }
type
  TMainViewModel = class(TInterfacedObject, IMainViewModelInterface)
  private
    FMainModel:IMainModelInterface;
    FProvider:IProviderInterface;
    FCurrentViewState:TViewStates;
    //Меню управления списками
    FIsListSettingsPanelVisible:Boolean;
    FIsInputListNamePanelVisible:Boolean;
    FIsNoListExistsPanelVisible:Boolean;
    FIsCloseMenuButtonVisible:Boolean;
    FIsAddNewListButtonEnabled: Boolean;
    FIsEditListCancelButtonEnabled: Boolean;
    FIsEditListOkButtonEnabled: Boolean;
    FIsRemoveListButtonEnabled: Boolean;
    FIsRenameListButtonEnabled: Boolean;
    //UI главной страницы
    FIsSortListGroupEnabled: Boolean;
    FIsAddUserButtonEnabled: Boolean;
    FIsRemoveUserButtonEnabled: Boolean;
    FIsSelectListButtonEnabled: Boolean;
    FSelectedListIndex:Integer;
    FListSortingIndex:Byte;
    FSelectedEmployeesID:TStringList;
    procedure CloseListSettingsMenu;
    function GetIsCloseMenuButtonVisible: Boolean;
    function GetIsNoListExistsPanelVisible: Boolean;
    procedure SendErrorNotification(const AErrorType: TInterfaceErrors;
      const AErrorMsg:String);
    procedure SendChangeStateNotification;
    procedure SendNotification(const AEventType: TInterfaceActions);
    procedure SetMainModel (const AModel: IMainModelInterface);
    procedure SetListSortingTypeIndex (const AListSortingTypeIndex:Byte);
    //MenuListSettingsPanel
    function GetSelectedListIndex: Integer;
    function GetIsListSettingsPanelVisible: Boolean;
    function GetIsAddNewListButtonEnabled: Boolean;
    function GetIsEditListCancelButtonEnabled: Boolean;
    function GetIsEditListOkButtonEnabled: Boolean;
    function GetIsRemoveListButtonEnabled: Boolean;
    function GetIsRenameListButtonEnabled: Boolean;
    
    function CheckListExisting: boolean;
    function GetCurrentListItemsCount: Integer;
    function GetResourcesPath: TDefaultResourcesPath;
    function GetIsAddUserButtonEnabled: Boolean;
    function GetIsSelectListButtonEnabled: Boolean;
    function GetIsInputListNamePanelVisible: Boolean;
    function GetIsRemoveUserButtonEnabled: Boolean;
    function GetIsSortListGroupEnabled: Boolean;
    function GetProvider: IProviderInterface;
    function GetCurrentListTitle:String;
    function GetListSortingTypeIndex:Byte;
    function GetExportExcelTemplateText: TExportExcelTemplateDialogText;
    function ValidateListName(const AListName: String): Boolean;
  public
    procedure InitMainScreen;
    procedure InitListSettingsMenu;
    procedure SetCurrentList(const AListIndex:Integer);
    procedure SetSelectedEmployeesID(const AIDList:TStringList=Nil);
    procedure SetSelectedListIndex(const AListIndex:Integer);
    procedure GetEmployeeList(var AEmployeeDTOList:TMainEmployeeDTOList);
    procedure ExportExcelTemplate(const APathToSave:String);
    procedure AddNewEmployee(const ANewEmployee:TEmployee);
    procedure AddNewEmployeeTo(const ANewEmployee:TEmployee;
              const AListName:String);
    procedure GetExistingListsTitles(var AListTitles:TStringList;
              const AWithCount:Boolean);
    procedure RemoveEmployee;
    procedure CreateNewEmployeeList(const AListName:String);
    procedure RenameEmployeeList(const ANewListName: String);
    procedure RemoveSelectedEmployeeList;
    function GetMainPanelText: TMainPanelDefaultCaptions;
    function GetListSettingsMenuText: TListSettingsMenuText;
    function GetNoListExistsDialogText: TNoListExistsDlgText;
    function GetSortingFileterCaptions:TStringList;
    property ResourcesPath:TDefaultResourcesPath read GetResourcesPath;
    //Menu
    property IsListSettingsPanelVisible:Boolean read GetIsListSettingsPanelVisible;
    property IsInputListNamePanelVisible:Boolean read GetIsInputListNamePanelVisible;
    property IsNoListExistsPanelVisible:Boolean read GetIsNoListExistsPanelVisible;
    property IsCloseMenuButtonVisible:Boolean read GetIsCloseMenuButtonVisible;
    property IsRemoveListButtonEnabled:Boolean read GetIsRemoveListButtonEnabled;
    property IsRenameListButtonEnabled:Boolean read GetIsRenameListButtonEnabled;
    property IsAddNewListButtonEnabled:Boolean read GetIsAddNewListButtonEnabled;
    property IsEditListOkButtonEnabled:Boolean read GetIsEditListOkButtonEnabled;
    property IsEditListCancelButtonEnabled:Boolean read GetIsEditListCancelButtonEnabled;
    //Main
    property IsSelectListButtonEnabled:Boolean read GetIsSelectListButtonEnabled;
    property IsAddUserButtonEnabled:Boolean read GetIsAddUserButtonEnabled;
    property IsRemoveUserButtonEnabled:Boolean read GetIsRemoveUserButtonEnabled;
    property IsSortListGroupEnabled:Boolean read GetIsSortListGroupEnabled;
    property CurrentListTitle:String read GetCurrentListTitle;
    property CurrentListCount:Integer read GetCurrentListItemsCount;
    property ListSortingTypeIndex:Byte read GetListSortingTypeIndex write
             SetListSortingTypeIndex;
    property ExportExcelTemplateText:TExportExcelTemplateDialogText read
             GetExportExcelTemplateText;
    property SelectedListIndex:Integer read GetSelectedListIndex  write
             SetSelectedListIndex;
    property MainModel: IMainModelInterface write SetMainModel;
    property Provider: IProviderInterface read GetProvider;
    constructor Create;
    destructor Destroy; override;
  end;

{ TMainViewModel }
function CreateMainViewModelClass: IMainViewModelInterface;
begin
  Result:=TMainViewModel.Create;
end;

constructor TMainViewModel.Create;
begin
  FProvider:=CreateProSuProviderClass;
  FIsSortListGroupEnabled:=False;
  FListSortingIndex:=0;
  FSelectedListIndex:=-1;
end;

///-----------------------------------------------------------///
///------------------Отправка событий во View-----------------///
///-----------------------------------------------------------///
function TMainViewModel.GetProvider: IProviderInterface;
begin
  if Assigned(FProvider) then Result:=FProvider;
end;

procedure TMainViewModel.SendNotification(const AEventType: TInterfaceActions);
var
  tmpNotificationClass: TNotificationClass;
begin
  tmpNotificationClass:=TNotificationClass.Create;
  tmpNotificationClass.Actions:=AEventType;
  if Assigned(fProvider) then
   fProvider.NotifySubscribers(tmpNotificationClass);
  tmpNotificationClass.Free;
end;

procedure TMainViewModel.SendChangeStateNotification;
var
  tmpNotificationClass: TNotificationClass;
begin
  tmpNotificationClass:=TNotificationClass.Create;
  tmpNotificationClass.Actions:=[changeViewStateAction];
  tmpNotificationClass.ViewStateValue:=FCurrentViewState;
  if Assigned(fProvider) then
   fProvider.NotifySubscribers(tmpNotificationClass);
  tmpNotificationClass.Free;
end;

procedure TMainViewModel.SendErrorNotification(
  const AErrorType: TInterfaceErrors; const AErrorMsg: String);
var
  tmpErrorNotificationClass: TErrorNotificationClass;
begin
  tmpErrorNotificationClass:=TErrorNotificationClass.Create;
  try
    tmpErrorNotificationClass.Actions:=AErrorType;
    tmpErrorNotificationClass.ActionMessage:=AErrorMsg;
    fProvider.NotifySubscribers(tmpErrorNotificationClass);
  finally
    tmpErrorNotificationClass.Free;
  end;
end;

procedure TMainViewModel.InitMainScreen;
begin
  if CheckListExisting then
  begin
    //ShowMessage('VMCInitialize');
    FIsSelectListButtonEnabled:=True;
    FIsSortListGroupEnabled:=True;
    FIsAddUserButtonEnabled:=True;
    FIsRemoveUserButtonEnabled:=False;
    FCurrentViewState:=TViewStates.vsMainState;
    SendChangeStateNotification;
  end else
  begin
    FIsCloseMenuButtonVisible:=False;
    FIsListSettingsPanelVisible:=False;
    FIsInputListNamePanelVisible:=True;
    FIsNoListExistsPanelVisible:=True;
    FIsEditListOkButtonEnabled:=True;
    FIsEditListCancelButtonEnabled:=True;
    FCurrentViewState:=TViewStates.vsNoListExistsState;
    SendChangeStateNotification;
  end;

end;

function TMainViewModel.CheckListExisting:boolean;
var
   existingListsProperties:TListPropertiesArray;
begin
  Result:=False;
  FMainModel.GetExistingListsProperties(existingListsProperties);
  if Length(existingListsProperties) > 0 then Result:=True;
  SetLength(existingListsProperties, 0);
end;

procedure TMainViewModel.SetMainModel(const AModel: IMainModelInterface);
begin
  FMainModel:=AModel;
end;
///-----------------------------------------------------------///
///----------------------Сортировка списка--------------------///
///-----------------------------------------------------------///
function TMainViewModel.GetSortingFileterCaptions: TStringList;
begin
  Result:=FMainModel.GetSortingFileterCaptions;
end;

function TMainViewModel.GetListSortingTypeIndex: Byte;
begin
  Result:=FListSortingIndex;
end;

procedure TMainViewModel.SetListSortingTypeIndex(
  const AListSortingTypeIndex: Byte);
begin
  if AListSortingTypeIndex <> FListSortingIndex then
  begin
    FListSortingIndex:=AListSortingTypeIndex;
    SendNotification([employeeListChangeAction]);
  end;
end;

function TMainViewModel.GetExportExcelTemplateText: TExportExcelTemplateDialogText;
begin
  Result:=FMainModel.GetExportExcelTemplateText;
end;

//----------------------------------------------------------///
//--------------------Меню управления списками--------------///
//----------------------------------------------------------///


procedure TMainViewModel.InitListSettingsMenu;
begin
  FSelectedListIndex:=-1;
  FIsListSettingsPanelVisible:=True;
  FIsInputListNamePanelVisible:=True;
  FIsNoListExistsPanelVisible:=False;
  FIsCloseMenuButtonVisible:=True;
  FIsAddNewListButtonEnabled:=True;
  FIsEditListCancelButtonEnabled:=True;
  FIsEditListOkButtonEnabled:=True;
  FIsRemoveListButtonEnabled:=False;
  FIsRenameListButtonEnabled:=False;
  FCurrentViewState:=TViewStates.vsListSettingState;
  SendChangeStateNotification;
end;

procedure TMainViewModel.CloseListSettingsMenu;
begin
  InitMainScreen;
end;

function TMainViewModel.GetSelectedListIndex: Integer;
begin
  Result:=FSelectedListIndex;
end;

procedure TMainViewModel.SetSelectedListIndex(const AListIndex: Integer);
begin
  if FSelectedListIndex = -1 then
  begin
    FIsRemoveListButtonEnabled:=True;
    FIsRenameListButtonEnabled:=True;
    FSelectedListIndex:=AListIndex;
    SendNotification([updateListSettingsMenuAction]);
  end
  else begin
    FSelectedListIndex:=AListIndex;
    if FSelectedListIndex = -1 then
    begin
      FIsRemoveListButtonEnabled:=False;
      FIsRenameListButtonEnabled:=False;
    end;
  end;
end;

function TMainViewModel.ValidateListName(const AListName: String):Boolean;
var
  listTitles:TStringList;
begin
  Result:=True;
  if trim(AListName) = '' then
  begin
    Result:=False;
    SendErrorNotification([errListNameEmpty],
       FMainModel.GetErrorMessageText.ListNameEmptyErrorText);
  end;

  listTitles:=TStringList.Create;
  GetExistingListsTitles(listTitles, False);
  if (listTitles.Count > 0) and (listTitles.IndexOf(AListName) > -1) then
  begin
    Result:=False;
    SendErrorNotification([errListNameAlreadyExists],
       FMainModel.GetErrorMessageText.ListNameAlreadyExistsText);
  end;
  listTitles.Free;
end;
procedure TMainViewModel.CreateNewEmployeeList(const AListName: String);
begin
  if not ValidateListName(AListName) then Exit;

  FMainModel.CreateNewEmployeeList(AListName);
  if FCurrentViewState=TViewStates.vsListSettingState then
  begin
    FSelectedListIndex:=-1;
    FIsListSettingsPanelVisible:=True;
    FIsAddNewListButtonEnabled:=True;
    FIsEditListCancelButtonEnabled:=True;
    FIsEditListOkButtonEnabled:=True;
    FIsRemoveListButtonEnabled:=False;
    FIsRenameListButtonEnabled:=False;
    SendNotification([updateListSettingsMenuAction]);
  end else
  if FCurrentViewState=TViewStates.vsNoListExistsState then
  begin
    InitMainScreen;
  end;
end;

procedure TMainViewModel.RenameEmployeeList(const ANewListName:String);
var
   existingListsProperties:TListPropertiesArray;
begin
  //Не выбран список для переименования.
  if FSelectedListIndex = -1 then Exit;
  //Имя списка не прошло валидацию.
  if not ValidateListName(ANewListName) then Exit;

  FMainModel.GetExistingListsProperties(existingListsProperties);
  FMainModel.RenameEmployeeList(existingListsProperties[FSelectedListIndex]
    .ListTitle, trim(ANewListName));
  SetLength(existingListsProperties, 0);

  FSelectedListIndex:=-1;
  FIsListSettingsPanelVisible:=True;
  FIsAddNewListButtonEnabled:=True;
  FIsEditListCancelButtonEnabled:=True;
  FIsEditListOkButtonEnabled:=True;
  FIsRemoveListButtonEnabled:=False;
  FIsRenameListButtonEnabled:=False;
  SendNotification([updateListSettingsMenuAction]);
end;

procedure TMainViewModel.RemoveSelectedEmployeeList;
var
   existingListsProperties:TListPropertiesArray;
begin
  if FSelectedListIndex = -1 then Exit;
  FMainModel.GetExistingListsProperties(existingListsProperties);
  FMainModel.RemoveEmployeeList(existingListsProperties[FSelectedListIndex]
    .ListTitle);

  FSelectedListIndex:=-1;
  FIsListSettingsPanelVisible:=True;
  FIsAddNewListButtonEnabled:=True;
  FIsEditListCancelButtonEnabled:=True;
  FIsEditListOkButtonEnabled:=True;
  FIsRemoveListButtonEnabled:=False;
  FIsRenameListButtonEnabled:=False;
  SendNotification([updateListSettingsMenuAction]);
end;


function TMainViewModel.GetIsListSettingsPanelVisible: Boolean;
begin
  Result:=FIsListSettingsPanelVisible;
end;

function TMainViewModel.GetIsInputListNamePanelVisible: Boolean;
begin
  Result:=FIsInputListNamePanelVisible;
end;

function TMainViewModel.GetIsNoListExistsPanelVisible: Boolean;
begin
  Result:=FIsNoListExistsPanelVisible;
end;

function TMainViewModel.GetIsCloseMenuButtonVisible: Boolean;
begin
  Result:=FIsCloseMenuButtonVisible;
end;

function TMainViewModel.GetIsAddNewListButtonEnabled: Boolean;
begin
  Result:=FIsAddNewListButtonEnabled;
end;

function TMainViewModel.GetIsEditListCancelButtonEnabled: Boolean;
begin
  Result:=FIsEditListCancelButtonEnabled;
end;

function TMainViewModel.GetIsEditListOkButtonEnabled: Boolean;
begin
  Result:=FIsEditListOkButtonEnabled;
end;

function TMainViewModel.GetIsRemoveListButtonEnabled: Boolean;
begin
  Result:=FIsRemoveListButtonEnabled;
end;

function TMainViewModel.GetIsRenameListButtonEnabled: Boolean;
begin
  Result:=FIsRenameListButtonEnabled;
end;
//CurrentListPanel
function TMainViewModel.GetIsAddUserButtonEnabled: Boolean;
begin
  Result:=FIsAddUserButtonEnabled;
end;

function TMainViewModel.GetIsRemoveUserButtonEnabled: Boolean;
begin
  Result:=FIsRemoveUserButtonEnabled;
end;

function TMainViewModel.GetIsSortListGroupEnabled: Boolean;
begin
  Result:=FIsSortListGroupEnabled;
end;

function TMainViewModel.GetIsSelectListButtonEnabled: Boolean;
begin
  Result:=FIsSelectListButtonEnabled;
end;

procedure TMainViewModel.SetCurrentList(const AListIndex: Integer);
var
  listTitles:TStringList;
  itemIndex:Byte;
  newListTitle:string;
begin
  listTitles:=TStringList.Create;
  GetExistingListsTitles(listTitles, False);
  newListTitle:=listTitles[AListIndex];
  listTitles.Free;
  //ShowMessage('VMChangeList');
  if CurrentListTitle <> newListTitle then
  begin
    FListSortingIndex:=0;
    FMainModel.SetCurrentList(newListTitle);
    SendNotification([employeeListChangeAction]);
  end;

end;

function TMainViewModel.GetCurrentListTitle: String;
begin
  Result:=FMainModel.GetCurrentListProperties.ListTitle;
end;

function TMainViewModel.GetCurrentListItemsCount: Integer;
begin
  Result:=FMainModel.GetCurrentListProperties.ListItemsCount;
end;

procedure TMainViewModel.SetSelectedEmployeesID(const AIDList:TStringList=Nil);
begin
  if (AIDList = Nil) or (AIDList.Count = 0) then
  begin
    FIsAddUserButtonEnabled:=True;
    FIsRemoveUserButtonEnabled:=False;
  end else
  begin
    FIsAddUserButtonEnabled:=True;
    FIsRemoveUserButtonEnabled:=True;
  end;
  FSelectedEmployeesID:=AIDList;
end;

procedure TMainViewModel.GetEmployeeList(
  var AEmployeeDTOList:TMainEmployeeDTOList);
var
  tmpEmployeeList:TEmployeeList;
  tmpEmployee:TEmployee;
  tmpEmployeeDTO:TMainEmployeeDTO;
  translatedSortingIndex:Byte;
  jobPosStr,
  shortFullNameStr:String;
begin

  Case FListSortingIndex of
    0 : translatedSortingIndex:=0;
    1 : translatedSortingIndex:=1;
    2 : translatedSortingIndex:=6;
    3 : translatedSortingIndex:=7;
  end;

  tmpEmployeeList:=TEmployeeList.Create;
  FMainModel.GetEmployeeList(tmpEmployeeList, translatedSortingIndex);
  FIsSortListGroupEnabled:=False;
  if tmpEmployeeList.Count = 0 then
  begin
    FIsRemoveUserButtonEnabled:=False;
    FreeAndNil(tmpEmployeeList);
    Exit;
  end;

  for tmpEmployee in tmpEmployeeList do
  begin
    tmpEmployeeDTO:=TMainEmployeeDTO.Create;
    With tmpEmployee do
    begin
      //ID сотрудника:
      tmpEmployeeDTO.ID:=ID;
      //Имя аватары(если была добавлена):
      tmpEmployeeDTO.AvatarName:=AvatarName;
      //Сокращённое ФИО:
      shortFullNameStr:=LastName + ' ' +
             UTF8Copy(FirstName, 1, 1) + '. ' +
             UTF8Copy(Patronymic, 1, 1) + '.';
      tmpEmployeeDTO.ShortFullName:=shortFullNameStr;
      //Должность:
      jobPosStr:=JobPosition;
      if UTF8Length(jobPosStr) > 28 then
        jobPosStr:=UTF8Copy(jobPosStr, 1, 28) + '...';
      tmpEmployeeDTO.JobPosition:=jobPosStr;
      //Пол:
      tmpEmployeeDTO.Gender:=Gender;
    end;
    AEmployeeDTOList.Add(tmpEmployeeDTO);
  end;

  if(tmpEmployeeList.Count > 1) then
    FIsSortListGroupEnabled:=True;

  FreeAndNil(tmpEmployeeList);
end;

procedure TMainViewModel.ExportExcelTemplate(const APathToSave: String);
begin
  if APathToSave <> '' then
     FMainModel.DoExportExcelTemplate(APathToSave);
end;

procedure TMainViewModel.AddNewEmployee(const ANewEmployee: TEmployee);
begin

end;

procedure TMainViewModel.AddNewEmployeeTo(const ANewEmployee: TEmployee;
  const AListName: String);
begin

end;

procedure TMainViewModel.GetExistingListsTitles(var AListTitles: TStringList;
  const AWithCount: Boolean);
var
  i:Integer;
  listTitle,
  listCountStr:string;
  existingListsProperties:TListPropertiesArray;
  listProperties:TListProperties;
begin
  FMainModel.GetExistingListsProperties(existingListsProperties);
  for i:=Low(existingListsProperties) to High(existingListsProperties) do
  begin
    listProperties:=existingListsProperties[i];
    listTitle:=listProperties.ListTitle;
    listCountStr:=' (' + IntToStr(listProperties.ListItemsCount) + ')';
    if AWithCount then
       listTitle:=listTitle+listCountStr;
    AListTitles.Add(listTitle);
  end;
  SetLength(existingListsProperties, 0);
end;

procedure TMainViewModel.RemoveEmployee;
var employeeId:String;
begin
  if not Assigned(FSelectedEmployeesID) or (FSelectedEmployeesID.Count = 0) then
     raise Exception.Create('Error: Emploee item/s not selected!');

  for employeeId in FSelectedEmployeesID do
  begin
    FMainModel.RemoveEmployeeFromID(employeeId);
  end;
  FSelectedEmployeesID.Clear;
  FIsRemoveUserButtonEnabled:=False;

  SendNotification([employeeListChangeAction]);
end;

function TMainViewModel.GetMainPanelText: TMainPanelDefaultCaptions;
begin
  Result:=FMainModel.GetMainPanelText;
end;

function TMainViewModel.GetListSettingsMenuText: TListSettingsMenuText;
begin
  Result:=FMainModel.GetListSettingsMenuText;
end;

function TMainViewModel.GetNoListExistsDialogText: TNoListExistsDlgText;
begin
  Result:=FMainModel.GetNoListExistsDialogText;
end;

function TMainViewModel.GetResourcesPath: TDefaultResourcesPath;
begin
  Result:=FMainModel.GetResourcesPath;
end;

destructor TMainViewModel.Destroy;
begin
  inherited;
end;

end.

