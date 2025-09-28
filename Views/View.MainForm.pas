unit View.MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ExtDlgs, typinfo, Menus, ActnList, ComCtrls, FGL, DateTimePicker,
  View.EmployeeItemPanel, View.EmployeeDetailPanel, Model.Declarations,
  Model.Interfaces, Model.ProSu.Subscriber, Model.ProSu.Interfaces,
  Model.ProSu.InterfaceActions;

type
  TEmployeeItemPanelList = specialize TFPGObjectList<TEmployeeItemPanel>;
  TEmployeeDetailPanelList = specialize TFPGObjectList<TEmployeeDetailPanel>;
  { TAppMainForm }

  TAppMainForm = class(TForm)
    DetailPanelButtonIconsIMGList: TImageList;
    EditSaveChangesSB: TSpeedButton;
    EditCopyAllDataButtonLbl: TLabel;
    MenuErrorTextLbl: TLabel;
    MenuBackImg: TImage;
    DialogNoListTextButtonLbl: TLabel;
    MenuTitlePanel: TPanel;
    MenuInputListNameHintLbl: TLabel;
    MenuInputListNameNoteLbl: TLabel;
    MenuInputListNameEdit: TEdit;
    EditAvaImg: TImage;
    EditAvaCoverImg: TImage;
    EditAvaChangeButImg: TImage;
    EditAvaBackgImg: TImage;
    MainButtonIconsIMGList: TImageList;
    ListMenuAddNewListSB: TSpeedButton;
    MenuInputListNameCancelSB: TSpeedButton;
    MenuCloseSB: TSpeedButton;
    MenuInputListNameSaveSB: TSpeedButton;
    ListMenuRenameListSB: TSpeedButton;
    ListMenuRemoveListSB: TSpeedButton;
    ExportMenuItem: TMenuItem;
    ExportCurrentListMenuItem: TMenuItem;
    ExportListTemplateMenuItem: TMenuItem;
    EditBigFIOLbl: TLabel;
    MainAddUserSB: TSpeedButton;
    MenuTitleLbl: TLabel;
    DialogNoListNoteLbl: TLabel;
    MainRemoveUserSB: TSpeedButton;
    MainFilterTextBackImg: TImage;
    MainListFilterCaptionLbl: TLabel;
    MainListFilterSB: TSpeedButton;
    MainListTitleLbl: TLabel;
    CurrentListUIPanel: TPanel;
    MenuListSettingsPanel: TPanel;
    ListMenuYourListTitleLbl: TLabel;
    ListMenuExistingListsListBox: TListBox;
    MenuPanel: TPanel;
    MenuInputListNamePanel: TPanel;
    DialogNoListExistsPanel: TPanel;
    sbSelectList: TComboBox;
    EditLineWithPointImg: TImage;
    AppMainMenu: TMainMenu;
    MainScreenHeaderImg: TImage;
    MainScreenPanel: TPanel;
    EditScreenHeaderImg: TImage;
    EditScreenFooterImg: TImage;
    EditScreenPanel: TPanel;
    FileMenuItem: TMenuItem;
    ListsSettingsMenuItem: TMenuItem;
    OpenExistingListMenuItem: TMenuItem;
    ImportListFromExcelMenuItem: TMenuItem;
    ScreensContainerPanel: TPanel;
    EditBackButtonSB: TSpeedButton;
    MainListSelectListSB: TSpeedButton;
    MainUserItemsScrollBox: TScrollBox;
    EditUserInfoItemsScrollBox: TScrollBox;
    procedure EditSaveChangesSBClick(Sender: TObject);
    procedure MainAddUserSBClick(Sender: TObject);
    procedure MainRemoveUserSBClick(Sender: TObject);
    procedure MainListFilterSBClick(Sender: TObject);
    procedure MainListSelectListSBClick(Sender: TObject);
    procedure MainEmployeeItemMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DialogNoListTextButtonClick(Sender: TObject);

    procedure MenuSettingsListBoxClick(Sender: TObject);
    procedure MenuCloseSBClick(Sender: TObject);
    procedure MenuInputListNameCancelSBClick(Sender: TObject);
    procedure MenuInputListNameEditChange(Sender: TObject);
    procedure ListMenuAddNewListSBClick(Sender: TObject);
    procedure ListMenuRemoveListSBClick(Sender: TObject);
    procedure ListMenuRenameListSBClick(Sender: TObject);
    procedure ListMenuCreateNewListClick(Sender: TObject);
    procedure ListsSettingsMenuItemClick(Sender: TObject);
    procedure OnAvatarMouseEnter(Sender: TObject);
    procedure OnAvatarMouseLeave(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure ImportListFromExcelMenuItemClick(Sender: TObject);
    procedure ExportListTemplateMenuItemClick(Sender: TObject);
    procedure OpenExistingListMenuItemClick(Sender: TObject);
    procedure EditBackButtonSBClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    //Edit Employee Panel
    procedure AvataraEditButtonImageClick(Sender: TObject);procedure OnItemNextButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    //Change Employee Properties callbacks
    procedure OnFirstNameChanged(Sender:TObject; AProperty:String);
    procedure OnLastNameChanged(Sender:TObject; AProperty:String);
    procedure OnPatronymicChanged(Sender:TObject; AProperty:String);
    procedure OnGenderChanged(Sender:TObject; AProperty:String);
    procedure OnJobPositionChanged(Sender:TObject; AProperty:String);
    procedure OnDateOfBirthChanged(Sender:TObject; AProperty:String);
    procedure OnDateOfEmploymentChanged(Sender:TObject; AProperty:String);
    procedure OnPhoneNumberChanged(Sender:TObject; AProperty:String);
    procedure OnEmailChanged(Sender:TObject; AProperty:String);
  private
    FAppResourcesPath:TDefaultResourcesPath;
    FTestCounter:Integer;
    FMainViewModel: IMainViewModelInterface;
    FEditViewModel: IEditViewModelInterface;
    FSubscriber:ISubscriberInterface;
    FEmployeeItemsPool:TEmployeeItemPanelList;
    FSelectedItemsList:TEmployeeItemPanelList;
    FSelectedItemsIDList:TStringList;
    FSelectedDetailPanel:TEmployeeDetailPanel;
    FEmployeeDetailList:TEmployeeDetailPanelList;
    FPopupFilterMenu:TPopupMenu;
    FPopupSelectListMenu:TPopupMenu;
    procedure AddItemsSetToItemsPool(const AItemsQuantity:Integer);
    procedure ClearSelectedItemsList;
    procedure CurrentDetailPanelEditButtonClick(Sender: TObject);
    procedure FilterPopupMenuItemClick(Sender: TObject);
    procedure ListMenuRenameListClick(Sender: TObject);
    procedure ListSettingPopupMenuItemClick(Sender: TObject);
    procedure SetMainViewModel (const AViewModel: IMainViewModelInterface);
    procedure SetEditViewModel (const AViewModel: IEditViewModelInterface);
    procedure NotificationFromProvider (const notifyClass: INotificationClass);
    procedure SetupEditPanel;
    procedure SetupMainPanel;
    procedure ChangeViewState(const ANewState: TViewStates);
    procedure SetupMenuListSettings;
    procedure SetupNoListExistsDialog;
    procedure DiscardEmployeeDetailChanges;
    procedure ShowMainScreen;
    procedure ShowEditScreen;
    procedure UpdateMenuPanel;
    procedure UpdateEmployeeItemsList;
    procedure UpdateEditEmployeePanel;
    procedure UpdateMainButtonsState;
    procedure UpdateMainPanelUI;
  public
    property MainViewModel: IMainViewModelInterface read FMainViewModel write SetMainViewModel;
    property EditViewModel: IEditViewModelInterface read FEditViewModel write SetEditViewModel;
  end;

var
  AppMainForm: TAppMainForm;

implementation

{$R *.lfm}

{ TAppMainForm }

procedure TAppMainForm.FormCreate(Sender: TObject);
var
  butImageList:TImageList;
begin
  FTestCounter:= 0;
  FSelectedItemsIDList:=TStringList.Create;
  FEmployeeItemsPool:=TEmployeeItemPanelList.Create;
  FSelectedItemsList:=TEmployeeItemPanelList.Create(False);
  FEmployeeDetailList:=TEmployeeDetailPanelList.Create;
  FSubscriber:=CreateProSuSubscriberClass;
  FSubscriber.SetUpdateSubscriberMethod(@NotificationFromProvider);
  FPopupFilterMenu:=TPopupMenu.Create(Self);
  FPopupFilterMenu.Parent:=MainScreenPanel;
  FPopupSelectListMenu:=TPopupMenu.Create(Self);
  FPopupSelectListMenu.Parent:=MainScreenPanel;
end;

///---------------------------------------------------------------///
///---------------Уведомления от ViewModel------------------------///
///---------------------------------------------------------------///
procedure TAppMainForm.NotificationFromProvider(
  const notifyClass: INotificationClass);
var
  tmpNotifClass: TNotificationClass;
  tmpErrorNotifClass: TErrorNotificationClass;
begin
  if notifyClass is TNotificationClass then
  begin
    //ShowMessage('VReceive message: ' + FTestCounter.ToString);
    tmpNotifClass:=notifyClass as TNotificationClass;
    if changeViewStateAction in tmpNotifClass.Actions then
       ChangeViewState(tmpNotifClass.ViewStateValue)
    else
    if employeeListChangeAction in tmpNotifClass.Actions then
    begin
      UpdateEmployeeItemsList;
    end else
    if updateListSettingsMenuAction in tmpNotifClass.Actions then
    begin
      UpdateMenuPanel;
    end else
    if updateEmployeeDetailPanel in tmpNotifClass.Actions then
    begin
      if Assigned(FSelectedDetailPanel) then
      begin
        FSelectedDetailPanel.UpdatePropertyDetailData(
          tmpNotifClass.PropertyDetailData);
        EditBigFIOLbl.Caption:=FEditViewModel.FullFIO;
      end;

    end;
  end;
  //Обработка ошибок:
  if notifyClass is TErrorNotificationClass then
  begin
    tmpErrorNotifClass:=notifyClass as TErrorNotificationClass;
    if errListNameEmpty in tmpErrorNotifClass.Actions then
    begin
      MenuInputListNameEdit.Text:='';
      MenuErrorTextLbl.Caption:=tmpErrorNotifClass.ActionMessage;
      MenuInputListNameEdit.SetFocus;
    end;
    if errListNameAlreadyExists in tmpErrorNotifClass.Actions then
    begin
      MenuInputListNameEdit.Text:='';
      MenuErrorTextLbl.Caption:=tmpErrorNotifClass.ActionMessage;
      MenuInputListNameEdit.SetFocus;
    end;
    if errWrongInputDetailProperty in tmpErrorNotifClass.Actions then
    begin

    end;

  end;
end;

procedure TAppMainForm.ChangeViewState(const ANewState:TViewStates);
begin
  case ANewState of
    TViewStates.vsMainState://Главное окно
      begin
        AppMainMenu.Items[0].Items[0].Enabled:=True;
        AppMainMenu.Items[0].Items[1].Enabled:=True;
        EditScreenPanel.Visible:=False;
        EditScreenPanel.Enabled:=False;
        MainScreenPanel.Visible:=True;
        MainScreenPanel.Enabled:=True;
        UpdateEmployeeItemsList;
        MenuPanel.Hide;
        CurrentListUIPanel.Show;
      end;
    TViewStates.vsListSettingState://Меню управления списками
      begin
        AppMainMenu.Items[0].Items[0].Enabled:=True;
        AppMainMenu.Items[0].Items[1].Enabled:=True;
        SetupMenuListSettings;
        CurrentListUIPanel.Hide;
        DialogNoListExistsPanel.Hide;
        UpdateMenuPanel;
        MenuPanel.Show;
        MenuListSettingsPanel.Show;
      end;
    TViewStates.vsNoListExistsState://Диалг предлагающий создать первый список
      begin
        //ShowMessage(GetEnumName(TypeInfo(TViewState), Trunc(ANewState)));
        AppMainMenu.Items[0].Items[0].Enabled:=False;
        AppMainMenu.Items[0].Items[1].Enabled:=False;
        SetupNoListExistsDialog;
        CurrentListUIPanel.Hide;
        MenuListSettingsPanel.Hide;
        UpdateMenuPanel;
        MenuPanel.Show;
        DialogNoListExistsPanel.Show;
      end;
    TViewStates.vsImportExcelListState:
      begin

      end;
    TViewStates.vsEditEmployeeState:
      begin
        AppMainMenu.Items[0].Items[0].Enabled:=False;
        AppMainMenu.Items[0].Items[1].Enabled:=False;
        MainScreenPanel.Visible:=False;
        //MainScreenPanel.Enabled:=False;
        SetupEditPanel;
        EditScreenPanel.Visible:=True;
        EditScreenPanel.Enabled:=True;
      end;
  end;
end;

procedure TAppMainForm.ShowMainScreen;
begin
  EditScreenPanel.Hide;
  MainScreenPanel.Show;
end;

procedure TAppMainForm.ShowEditScreen;
begin
  EditScreenPanel.Show;
  MainScreenPanel.Hide;
end;

///----------------------------------------------------------------///
///--------------УПРАВЛЕНИЕ СОСТОЯНИЕМ ГЛАВНОГО ЭКРАНА-------------///
///----------------------------------------------------------------///
procedure TAppMainForm.SetupMainPanel;
var
   tmpMainPanelText:TMainPanelDefaultCaptions;
   listSettingsMenuText:TListSettingsMenuText;
begin
  tmpMainPanelText:=FMainViewModel.GetMainPanelText;
  MainListTitleLbl.Caption:=tmpMainPanelText.CurrentListText;
  MainAddUserSB.Caption:=tmpMainPanelText.AddButtonText;
  MainRemoveUserSB.Caption:=tmpMainPanelText.RemoveButtonText;
  MainListFilterCaptionLbl.Caption:=tmpMainPanelText.ListFilterText;
end;

procedure TAppMainForm.MainEmployeeItemMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I,
  firstIndex,
  finalIndex:Integer;
  tmpItemPanel:TEmployeeItemPanel;
begin
  tmpItemPanel:= TImage(Sender).Parent as TEmployeeItemPanel;
  if ssShift in Shift then
  begin
    if FSelectedItemsList.Count > 0 then
    begin
      firstIndex:=FEmployeeItemsPool.IndexOf(FSelectedItemsList.Last);
      finalIndex:=FEmployeeItemsPool.IndexOf(tmpItemPanel);
      for I := firstIndex to finalIndex do
      begin
        tmpItemPanel:=FEmployeeItemsPool[I];
        tmpItemPanel.IsSelected:=True;
        FSelectedItemsList.Add(tmpItemPanel);
      end;
    end
    else
    begin
      tmpItemPanel.IsSelected:=True;
      FSelectedItemsList.Add(tmpItemPanel);
    end;
  end
  else if ssCtrl in Shift then
  begin
    if tmpItemPanel.IsSelected then
    begin
      tmpItemPanel.IsSelected:=False;
      FSelectedItemsList.Remove(tmpItemPanel);
    end
    else
    begin
      tmpItemPanel.IsSelected:=True;
      FSelectedItemsList.Add(tmpItemPanel);
    end;
  end
  else
  begin
    if tmpItemPanel.IsSelected then
    begin
      if FSelectedItemsList.Count = 1 then
         ClearSelectedItemsList
      else
      begin
        ClearSelectedItemsList;
        tmpItemPanel.IsSelected:=True;
        FSelectedItemsList.Add(tmpItemPanel);
      end;
    end
    else
    begin
      ClearSelectedItemsList;
      tmpItemPanel.IsSelected:=True;
      FSelectedItemsList.Add(tmpItemPanel);
    end;
  end;


  if FSelectedItemsList.Count > 0 then
  begin
    FSelectedItemsIDList.Clear;
    for tmpItemPanel in FSelectedItemsList do
        FSelectedItemsIDList.Add(tmpItemPanel.ItemID);
    FMainViewModel.SetSelectedEmployeesID(FSelectedItemsIDList);
  end else
    FMainViewModel.SetSelectedEmployeesID(Nil);
   UpdateMainButtonsState;
end;

procedure TAppMainForm.ClearSelectedItemsList;
var I:Integer;
begin
  if FSelectedItemsList.Count = 0 then
     Exit;

  for I:= 0 to Pred(FSelectedItemsList.Count) do
    FSelectedItemsList[I].IsSelected:=False;

  FSelectedItemsList.Clear;
end;

procedure TAppMainForm.MainAddUserSBClick(Sender: TObject);
begin
  FEditViewModel.CreateNewEmployee(FMainViewModel.GetCurrentListTitle);
end;

procedure TAppMainForm.MainRemoveUserSBClick(Sender: TObject);
begin
  FMainViewModel.RemoveEmployee;
end;

procedure TAppMainForm.OnItemNextButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tmpItemPanel:TEmployeeItemPanel;
begin

  tmpItemPanel:= TImage(Sender).Parent as TEmployeeItemPanel;
  tmpItemPanel.SetEmplyeeItemState(TEmployeeItemState.OutState);
  ClearSelectedItemsList;
  tmpItemPanel.IsSelected:=False;
  FEditViewModel.EditEmployee(tmpItemPanel.ItemID,
    FMainViewModel.GetCurrentListTitle);
end;

procedure TAppMainForm.UpdateEmployeeItemsList;
var
  I,
  posY:Integer;
  tmpEmployeeDTOList:TMainEmployeeDTOList;
  tmpEmployeeDTO:TMainEmployeeDTO;
  fItemPanel:TEmployeeItemPanel;
begin
  //Очищаем текущий список:
  for fItemPanel in FEmployeeItemsPool do fItemPanel.RemoveItem;

  tmpEmployeeDTOList:=TMainemployeeDTOList.Create;
  FMainViewModel.GetEmployeeList(tmpEmployeeDTOList);

  if tmpEmployeeDTOList.Count > 0 then
  begin
    if FEmployeeItemsPool.Count < tmpEmployeeDTOList.Count then
     AddItemsSetToItemsPool(tmpEmployeeDTOList.Count - FEmployeeItemsPool.Count);
    posY:=3;
    for I:=0 to tmpEmployeeDTOList.Count - 1 do
    begin
       tmpEmployeeDTO:=tmpEmployeeDTOList[I];
       fItemPanel:=FEmployeeItemsPool[I];
       fItemPanel.Initialize(tmpEmployeeDTO);
       fItemPanel.Left:=5;
       fItemPanel.Top:=posY;
       fItemPanel.SetItemMouseDown(@MainEmployeeItemMouseDown);
       fItemPanel.SetNextButtonMouseDown(@OnItemNextButtonMouseDown);
       fItemPanel.AddItemTo(MainUserItemsScrollBox);
       posY:=posY + 80;
    end;
  end;
  FreeAndNil(tmpEmployeeDTOList);
  UpdateMainPanelUI;
  UpdateMainButtonsState;
end;


procedure TAppMainForm.UpdateMainButtonsState;
begin
  MainAddUserSB.Enabled:=FMainViewModel.IsAddUserButtonEnabled;
  MainRemoveUserSB.Enabled:=FMainViewModel.IsRemoveUserButtonEnabled;
end;

procedure TAppMainForm.UpdateMainPanelUI;
var
  I:integer;
  sortingCaptions:TStringList;
  tmpMainPanelText:TMainPanelDefaultCaptions;
begin
  MainListTitleLbl.Caption:=FMainViewModel.CurrentListTitle;
  MainListSelectListSB.Enabled:=FMainViewModel.IsSelectListButtonEnabled;
  MainAddUserSB.Enabled:=FMainViewModel.IsAddUserButtonEnabled;
  MainRemoveUserSB.Enabled:=FMainViewModel.IsRemoveUserButtonEnabled;
  if FMainViewModel.IsSortListGroupEnabled then
  begin
    I:=MainViewModel.ListSortingTypeIndex;
    sortingCaptions:=MainViewModel.GetSortingFileterCaptions;
    MainListFilterCaptionLbl.Caption:=sortingCaptions[I];
    MainListFilterCaptionLbl.Enabled:=True;
    MainListFilterSB.Enabled:=True;
  end else
  begin
    tmpMainPanelText:=FMainViewModel.GetMainPanelText;
    MainListFilterCaptionLbl.Caption:=tmpMainPanelText.ListFilterText;
    MainListFilterCaptionLbl.Enabled:=False;
    MainListFilterSB.Enabled:=False;
  end;
end;

///---------------------------------------------------------------///
///---------------Выпадающий список выбора списка-----------------///
///---------------------------------------------------------------///
procedure TAppMainForm.MainListSelectListSBClick(Sender: TObject);
var
  listTitles:TStringList;
  popupItem:TMenuItem;
  listCaption:String;
  pt, pt2: TPoint;
begin
  FPopupSelectListMenu.Items.Clear;
  listCaption:='';
  listTitles:=TStringList.Create;
  MainViewModel.GetExistingListsTitles(listTitles, True);
  for listCaption in listTitles do
  begin
    popupItem:=TMenuItem.Create(FPopupSelectListMenu);
    popupItem.Caption:=listCaption;
    popupItem.OnClick:=@ListSettingPopupMenuItemClick;
    FPopupSelectListMenu.Items.Add(popupItem);
  end;
  pt.X:=MainListSelectListSB.Left;
  pt.Y:=MainListSelectListSB.Top + MainListSelectListSB.Height + 4;
  pt2:=CurrentListUIPanel.ClientToScreen(pt);
  FPopupSelectListMenu.PopUp(pt2.x,pt2.y);
  listTitles.Free;
end;

procedure TAppMainForm.ListSettingPopupMenuItemClick(Sender: TObject);
begin
  MainViewModel.SetCurrentList(TMenuItem(Sender).MenuIndex);
end;

///---------------------------------------------------------------///
///-------------------Выпадающий список фильтра-------------------///
///---------------------------------------------------------------///
procedure TAppMainForm.MainListFilterSBClick(Sender: TObject);
var
   popupItem:TMenuItem;
   filterCaption:String;
   pt, pt2: TPoint;
begin
  FPopupFilterMenu.Items.Clear;
  for filterCaption in MainViewModel.GetSortingFileterCaptions do
  begin
    popupItem:=TMenuItem.Create(FPopupFilterMenu);
    popupItem.Caption:=filterCaption;
    popupItem.OnClick:=@FilterPopupMenuItemClick;
    FPopupFilterMenu.Items.Add(popupItem);
  end;
  pt.X:=MainFilterTextBackImg.Left;
  pt.Y:=MainFilterTextBackImg.Top + MainFilterTextBackImg.Height + 4;
  pt2:=CurrentListUIPanel.ClientToScreen(pt);
  FPopupFilterMenu.PopUp(pt2.x,pt2.y);
end;

procedure TAppMainForm.FilterPopupMenuItemClick(Sender: TObject);
var
   itemIndex:Byte;
begin
  itemIndex:=TMenuItem(Sender).MenuIndex;
  MainViewModel.ListSortingTypeIndex:=itemIndex;
end;

procedure TAppMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var I:Integer;
begin
  for I:=Low(FileNames) to High(FileNames) do
  begin
    ShowMessage(FileNames[I]);
  end;
end;

procedure TAppMainForm.OpenExistingListMenuItemClick(Sender: TObject);
begin

end;

procedure TAppMainForm.ImportListFromExcelMenuItemClick(Sender: TObject);
begin
  Self.Close;
end;

//---------------------------------------------------------------//
//----------------------MENU LIST SETTINGS-----------------------//
//---------------------------------------------------------------//

procedure TAppMainForm.SetupMenuListSettings;
var
  listSettingsMenuText:TListSettingsMenuText;
begin
  listSettingsMenuText:=FMainViewModel.GetListSettingsMenuText;
  MenuTitleLbl.Caption:=listSettingsMenuText.MenuPanelTitle;
  ListMenuYourListTitleLbl.Caption:=listSettingsMenuText.BigYourListsNoteText;
  ListMenuRemoveListSB.Caption:=listSettingsMenuText.ButRemoveListCaption;
  ListMenuRenameListSB.Caption:=listSettingsMenuText.ButRenameListCaption;
  ListMenuAddNewListSB.Caption:=listSettingsMenuText.ButCreateListCaption;
  MenuInputListNameCancelSB.Caption:=listSettingsMenuText.ButCancelSetNameCaption;
  MenuInputListNameSaveSB.Caption:=listSettingsMenuText.ButSaveListNameCaption;
end;

procedure TAppMainForm.ListsSettingsMenuItemClick(Sender: TObject);
begin
  FMainViewModel.InitListSettingsMenu;
end;

procedure TAppMainForm.MenuCloseSBClick(Sender: TObject);
begin
  FMainViewModel.CloseListSettingsMenu;
end;

procedure TAppMainForm.UpdateMenuPanel;
var
  listTitles:TStringList;
  listName:String;
  menuItem:TMenuItem;
begin
  if FMainViewModel.IsListSettingsPanelVisible then
  begin
    ListMenuExistingListsListBox.Clear;
    listTitles:=TStringList.Create;
    FMainViewModel.GetExistingListsTitles(listTitles, True);
    AppMainMenu.Items[0].Items[1].Clear;
    //ShowMessage('List titles count is: ' + IntToStr(listTitles.Count));
    if Assigned(listTitles) and (listTitles.Count > 0) then
    begin
      //Обновляем списки в главном меню:
      for listName in listTitles do
      begin
        menuItem:=TMenuItem.Create(Self);
        menuItem.Caption:=listName;
        menuItem.OnClick:=@ListSettingPopupMenuItemClick;
        AppMainMenu.Items[0].Items[1].Add(menuItem);
      end;
      ListMenuExistingListsListBox.Items.Assign(listTitles);
      if FMainViewModel.SelectedListIndex > -1 then
         ListMenuExistingListsListBox.Selected[FMainViewModel.SelectedListIndex]:=
         True;
    end;
    listTitles.Free;
    MenuListSettingsPanel.Visible:=True;
    MenuListSettingsPanel.Enabled:=True;
    ListMenuRemoveListSB.Enabled:=FMainViewModel.IsRemoveListButtonEnabled;
    ListMenuRenameListSB.Enabled:=FMainViewModel.IsRenameListButtonEnabled;
    ListMenuAddNewListSB.Enabled:=FMainViewModel.IsAddNewListButtonEnabled;
  end;

  if FMainViewModel.IsInputListNamePanelVisible then
  begin
    MenuInputListNamePanel.Visible:=True;
    MenuInputListNamePanel.Enabled:=False;
    MenuInputListNamePanel.Height:=16;
  end;

  if FMainViewModel.IsNoListExistsPanelVisible then
  begin
    DialogNoListTextButtonLbl.Enabled:=True;
  end;

  MenuCloseSB.Visible:=FMainViewModel.IsCloseMenuButtonVisible;
  MenuCloseSB.Enabled:=FMainViewModel.IsCloseMenuButtonVisible;
end;

procedure TAppMainForm.ListMenuRemoveListSBClick(Sender: TObject);
var
  menuText: TListSettingsMenuText;
begin
  menuText:=FMainViewModel.GetListSettingsMenuText;
  if QuestionDlg(menuText.RemoveListDlgCaption,
                  menuText.RemoveListDlgMessage,
                  mtCustom,
                  [mrYes, menuText.RemoveListDlgYesCaption, 'IsDefault',
                  mrNo, menuText.RemoveListDlgNoCaption],'') = mrYes then
  begin
    FMainViewModel.RemoveSelectedEmployeeList;
  end;
end;

procedure TAppMainForm.MenuSettingsListBoxClick(Sender: TObject);
var selIndex:Integer;
begin
  selIndex:=ListMenuExistingListsListBox.ItemIndex;
  //ShowMessage('sel index is: ' +  selIndex.ToString);
  if FMainViewModel.SelectedListIndex = selIndex then
  begin
    FMainViewModel.SelectedListIndex:=-1;
    ListMenuExistingListsListBox.ClearSelection;
    ListMenuRemoveListSB.Enabled:=FMainViewModel.IsRemoveListButtonEnabled;
    ListMenuRenameListSB.Enabled:=FMainViewModel.IsRenameListButtonEnabled;
    ListMenuAddNewListSB.Enabled:=FMainViewModel.IsAddNewListButtonEnabled;
  end else
    FMainViewModel.SelectedListIndex:=ListMenuExistingListsListBox.ItemIndex;
end;

procedure TAppMainForm.ListMenuRenameListSBClick(Sender: TObject);
var
  listTitles:TStringList;
  menuText: TListSettingsMenuText;
begin
  menuText:=FMainViewModel.GetListSettingsMenuText;
  MenuInputListNameNoteLbl.Caption:=menuText.BigRenameListNoteText;
  MenuInputListNameHintLbl.Caption:=menuText.HintRenameListText;
  MenuInputListNameCancelSB.Enabled:=FMainViewModel.IsEditListCancelButtonEnabled;
  MenuInputListNameSaveSB.Enabled:=FMainViewModel.IsEditListOkButtonEnabled;
  MenuInputListNameSaveSB.OnClick:=@ListMenuRenameListClick;
  MenuListSettingsPanel.Enabled:=False;
  //Снимаем выделение(выделение отвлекает внимание)
  ListMenuExistingListsListBox.ClearSelection;

  listTitles:=TStringList.Create;
  MainViewModel.GetExistingListsTitles(listTitles, False);
  MenuInputListNameEdit.Text:=listTitles[FMainViewModel.SelectedListIndex];
  listTitles.Free;

  MenuInputListNamePanel.Enabled:=True;
  MenuInputListNamePanel.Height:=200;
  MenuInputListNameEdit.SetFocus;
end;

procedure TAppMainForm.ListMenuAddNewListSBClick(Sender: TObject);
var
  menuText: TListSettingsMenuText;
begin
  //Если был выделен список в листбоксе - снимаем выделение.
  FMainViewModel.SelectedListIndex:=-1;
  ListMenuExistingListsListBox.ClearSelection;

  menuText:=FMainViewModel.GetListSettingsMenuText;
  MenuInputListNameNoteLbl.Caption:=menuText.BigCreateListNoteText;
  MenuInputListNameHintLbl.Caption:=menuText.HintCreateListText;
  MenuInputListNameCancelSB.Enabled:=FMainViewModel.IsEditListCancelButtonEnabled;
  MenuInputListNameSaveSB.Enabled:=FMainViewModel.IsEditListOkButtonEnabled;
  MenuInputListNameSaveSB.OnClick:=@ListMenuCreateNewListClick;
  MenuListSettingsPanel.Enabled:=False;
  MenuInputListNamePanel.Enabled:=True;
  MenuInputListNameEdit.Text:='';
  MenuInputListNamePanel.Height:=200;
  MenuInputListNameEdit.SetFocus;
end;

procedure TAppMainForm.MenuInputListNameEditChange(Sender: TObject);
begin
  if MenuErrorTextLbl.Caption <> '' then MenuErrorTextLbl.Caption:='';
end;

procedure TAppMainForm.MenuInputListNameCancelSBClick(Sender: TObject);
begin
  UpdateMenuPanel;
end;

procedure TAppMainForm.ListMenuCreateNewListClick(Sender: TObject);
begin
  FMainViewModel.CreateNewEmployeeList(MenuInputListNameEdit.Text);
end;

procedure TAppMainForm.ListMenuRenameListClick(Sender: TObject);
begin
  FMainViewModel.RenameEmployeeList(MenuInputListNameEdit.Text);
end;

///-----------------------------------------------------------------///
///---------------------DialogNoListExists--------------------------///
///-----------Диалог появляется, если нет ни одного списка----------///
///-----------------------------------------------------------------///
procedure TAppMainForm.SetupNoListExistsDialog;
var
  dlgTexts: TNoListExistsDlgText;
begin
  dlgTexts:=FMainViewModel.GetNoListExistsDialogText;
  MenuTitleLbl.Caption:=dlgTexts.MenuPanelTitle;
  DialogNoListNoteLbl.Caption:=dlgTexts.BigNoListNoteText;
  DialogNoListTextButtonLbl.Caption:=dlgTexts.StringButCreateListCaption;
  MenuInputListNameNoteLbl.Caption:=dlgTexts.BigCreateListNoteText;
  MenuInputListNameHintLbl.Caption:=dlgTexts.HintCreateListText;
  MenuInputListNameCancelSB.Caption:=dlgTexts.ButCancelSetNameCaption;
  MenuInputListNameSaveSB.Caption:=dlgTexts.ButSaveListNameCaption;
  MenuInputListNameEdit.Text:='';
end;

procedure TAppMainForm.DialogNoListTextButtonClick(Sender: TObject);
begin
  MenuInputListNameSaveSB.OnClick:=@ListMenuCreateNewListClick;
  DialogNoListTextButtonLbl.Enabled:=False;
  MenuInputListNamePanel.Enabled:=True;
  MenuInputListNamePanel.Height:=200;
  MenuInputListNameEdit.SetFocus;
end;

///-----------------------------------------------------------------///
///------------------------------ЭКСПОРТ----------------------------///
///-----------------------------------------------------------------///
//Экспорт шаблона списка сотрудников:
procedure TAppMainForm.ExportListTemplateMenuItemClick(Sender: TObject);
var
   saveExcelTemplateDialog:TSaveDialog;
   dlgText:TExportExcelTemplateDialogText;
begin
  dlgText:=FMainViewModel.ExportExcelTemplateText;
  saveExcelTemplateDialog:=TSaveDialog.Create(Self);
  With saveExcelTemplateDialog do begin
    Title:=dlgText.DlgTitle;
    DefaultExt:=dlgText.DlgDefaultExt;
    Filter:=dlgText.DlgFilter;
    FileName:=dlgText.DlgDefaultFileName;
    Options:=Options + [ofOverwriteprompt];
  end;

  if saveExcelTemplateDialog.Execute then
  begin
    FMainViewModel.ExportExcelTemplate(saveExcelTemplateDialog.FileName);
    ShowMessage('Путь сохранения шаблона Excel: ' +
        saveExcelTemplateDialog.FileName);
  end;
end;

procedure TAppMainForm.AddItemsSetToItemsPool(const AItemsQuantity:Integer);
var
   I:Integer;
   fItemPanel:TEmployeeItemPanel;
begin
  for I:=0 to AItemsQuantity - 1 do
  begin
     fItemPanel:=TEmployeeItemPanel.CreateNew(FAppResourcesPath);
     FEmployeeItemsPool.Add(fItemPanel);
  end;
end;

procedure TAppMainForm.SetMainViewModel(const AViewModel: IMainViewModelInterface);
begin
  FMainViewModel:=AViewModel;
  if not Assigned(FMainViewModel) then
    raise Exception.Create('Error: Main View Model is required');
  FAppResourcesPath:=FMainViewModel.GetResourcesPath;
  AddItemsSetToItemsPool(20);
  FMainViewModel.Provider.Subscribe(FSubscriber);
  FMainViewModel.InitMainScreen;
  SetupMainPanel;
  UpdateMainPanelUI;
  SetupMenuListSettings;
end;

procedure TAppMainForm.SetEditViewModel(const AViewModel: IEditViewModelInterface);
begin
  FEditViewModel:=AViewModel;
  if not Assigned(FEditViewModel) then
    raise Exception.Create('Error: Edit View Model is required');

    FEditViewModel.Provider.Subscribe(FSubscriber);
end;

procedure TAppMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEmployeeItemsPool);
  FreeAndNil(FSelectedItemsList);
  FreeAndNil(FSelectedItemsIDList);
  FreeAndNil(FEmployeeDetailList);
end;

///------------------------------------------------------------///
///------------------------------------------------------------///
///-------------------EDIT EMPLOYEE STATE----------------------///
///------------------------------------------------------------///
///------------------------------------------------------------///
procedure TAppMainForm.SetupEditPanel;
var
  I,
  posY,
  panelHeightWithSpace:Integer;
  tmpDetailPanel:TEmployeeDetailPanel;
begin
  posY:=3;
  panelHeightWithSpace:=80;
  EditUserInfoItemsScrollBox.VertScrollBar.Position:=0;
  //Фамилия:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetLastNameData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnLastNameChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  Inc(posY, panelHeightWithSpace);
  //Имя:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetFirstNameData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnFirstNameChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  Inc(posY, panelHeightWithSpace);
  //Отчество:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetPatronymicData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnPatronymicChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  Inc(posY, panelHeightWithSpace);
  //Пол:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetGenderData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnGenderChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  Inc(posY, panelHeightWithSpace);
  //Должность:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetJobPositionData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnJobPositionChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  Inc(posY, panelHeightWithSpace);
  //Дата рождения:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetDateOfBirthData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnDateOfBirthChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  Inc(posY, panelHeightWithSpace);
  //Дата трудоустройства:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetDateOfEmploymentData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnDateOfEmploymentChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  Inc(posY, panelHeightWithSpace);
  //Номер телефона:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetPhoneNumberData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnPhoneNumberChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  Inc(posY, panelHeightWithSpace);
  //Адрес электронной почты:
  tmpDetailPanel:=TEmployeeDetailPanel.CreateNew(Self,
  FEditViewModel.GetEmailData, DetailPanelButtonIconsIMGList);
  tmpDetailPanel.Parent:=EditUserInfoItemsScrollBox;
  tmpDetailPanel.Top:=posY;
  tmpDetailPanel.SetEditPropertyClickCallback(@CurrentDetailPanelEditButtonClick);
  tmpDetailPanel.SetPropertyChangedCallback(@OnEmailChanged);
  FEmployeeDetailList.Add(tmpDetailPanel);
  EditBigFIOLbl.Caption:=FEditViewModel.FullFIO;
end;

procedure TAppMainForm.UpdateEditEmployeePanel;
begin

end;

procedure TAppMainForm.CurrentDetailPanelEditButtonClick(Sender: TObject);
begin
  if Assigned(FSelectedDetailPanel) then
     FSelectedDetailPanel.SetEmplyeeDetailState(TDetailState.dsNormalState);
  //ShowMessage('Current Detail panel selected!');

  FSelectedDetailPanel:=TEmployeeDetailPanel(Sender);
end;

procedure TAppMainForm.OnFirstNameChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetFirstNameValue(AProperty);
end;

procedure TAppMainForm.OnLastNameChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetLastNameValue(AProperty);
end;

procedure TAppMainForm.OnPatronymicChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetPatronymicValue(AProperty);
end;

procedure TAppMainForm.OnGenderChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetGenderValue(AProperty);
end;

procedure TAppMainForm.OnJobPositionChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetJobPositionValue(AProperty);
end;

procedure TAppMainForm.OnDateOfBirthChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetDateOfBirthValue(AProperty);
end;

procedure TAppMainForm.OnDateOfEmploymentChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetDateOfEmploymentValue(AProperty);
end;

procedure TAppMainForm.OnPhoneNumberChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetPhoneNumberValue(AProperty);
end;

procedure TAppMainForm.OnEmailChanged(Sender:TObject; AProperty:String);
begin
  FEditViewModel.SetEmailValue(AProperty);
end;

procedure TAppMainForm.EditSaveChangesSBClick(Sender: TObject);
begin
  FEditViewModel.SaveChanges;
end;

procedure TAppMainForm.DiscardEmployeeDetailChanges;
begin

end;

procedure TAppMainForm.AvataraEditButtonImageClick(Sender: TObject);
var
  fOpenPicDialog:TOpenPictureDialog;
begin
  fOpenPicDialog:=TOpenPictureDialog.Create(self);
  fOpenPicDialog.Title:='Открыть существующий файл';
  if fOpenPicDialog.Execute then
  begin
    EditAvaImg.Picture.LoadFromFile(fOpenPicDialog.FileName); // Загрузить файл в imgBack.
    //x:=imgBack.Picture.Width;
    //y:=imgBack.Picture.Height;
    //imgBack.Picture.SaveToFile(opendirectory.FileName);
    //imgBack.Width:=x;
    //imgBack.Height:=y;
  end;
  ShowMessage('Avatara event click works');
end;

procedure TAppMainForm.OnAvatarMouseEnter(Sender: TObject);
begin
  EditAvaChangeButImg.Visible:=True;
end;

procedure TAppMainForm.OnAvatarMouseLeave(Sender: TObject);
begin
  EditAvaChangeButImg.Visible:=False;
end;

procedure TAppMainForm.EditBackButtonSBClick(Sender: TObject);
var
  dlgTexts: TCloseEditPanelDlgTexts;
  dlgResult:TModalResult;
begin
  //ShowMessage('Edit changed is: ' + BoolToStr(FEditViewModel.IsChanged, True));
  if FEditViewModel.IsChanged  then
  begin
    dlgTexts:=FEditViewModel.GetCloseEditPanelDlgTexts;
    dlgResult:=QuestionDlg(dlgTexts.ClosePanelDlgCaption,
                    dlgTexts.ClosePanelDlgMessage,
                    mtCustom,
                    [mrYes, dlgTexts.ClosePanelDlgYesCaption, 'IsDefault',
                    mrNo, dlgTexts.ClosePanelDlgNoCaption,
                    mrCancel, dlgTexts.ClosePanelDlgCancelCaption],'');
    if dlgResult = mrYes then
      FEditViewModel.SaveChanges
    else if dlgResult = mrNo then
       FEditViewModel.DiscardChanges;
    //FreeAndNil(FEmployeeDetailList);
    Exit;
  end;

  FEditViewModel.DiscardChanges;
  //FreeAndNil(FEmployeeDetailList);
end;

end.

