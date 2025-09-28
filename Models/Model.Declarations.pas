unit Model.Declarations;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Model.ProSu.InterfaceActions, Model.ProSu.Interfaces, FGL,
  LazUTF8;

type
  TListSortingType=(lsNone=0, lsByName=1);
  TDetailPropertyType = (dptStringType=0, dptDateType=1);
  TViewStates=(vsMainState=0, vsEditEmployeeState=1, vsNoListExistsState=2,
                vsImportExcelListState = 3, vsListSettingState=4);

  { TEmployee Enitity}

  TEmployee=class
    private
      FID: String;
      FFirstName: String;
      FLastName: String;
      FPatronymic: String;
      FGender:String;
      FJobPosition: String;
      FAvatarName: String;
      FDateOfBirth: String;
      FDateOfEmployment: String;
      FPhoneNumber: String;
      FEmail: String;
    public
      Property ID:String read FID write FID;
      Property FirstName:String read FFirstName write FFirstName;
      Property LastName:String read FLastName write FLastName;
      Property Patronymic:String read FPatronymic write FPatronymic;
      Property Gender:String read FGender write FGender;
      Property JobPosition:String read FJobPosition write FJobPosition;
      Property AvatarName:String read FAvatarName write FAvatarName;
      Property DateOfBirth:String read FDateOfBirth write FDateOfBirth;
      Property DateOfEmployment:String read FDateOfEmployment write FDateOfEmployment;
      Property PhoneNumber:String read FPhoneNumber write FPhoneNumber;
      Property Email:String read FEmail write FEmail;
  end;

  TEmployeeList = specialize TFPGObjectList<TEmployee>;

  TMainEmployeeDTO = class
  private
    FID,
    FAvatarName,
    FShortFullName,
    FJobPosition,
    FGender: string;
  public
    property ID: String read FID write FID;
    property AvatarName: String read FAvatarName write FAvatarName;
    property ShortFullName: String read FShortFullName write FShortFullName;
    property JobPosition: String read FJobPosition write FJobPosition;
    property Gender:String read FGender write FGender;
  end;

  TMainEmployeeDTOList = specialize TFPGObjectList<TMainEmployeeDTO>;

  TListProperties = record
    ListTitle: string;
    ListItemsCount:Integer;
  end;

  TListPropertiesArray=Array of TListProperties;

  TPropertyDetailData = record
    PropertyIsChanged:Boolean;
    PropertyChangedCaption:String;
    PropertyType:TDetailPropertyType;
    PropertyName,
    PropertyValue,
    PropertyHint:String;
  end;

  TNotificationClass = class (TInterfacedObject, INotificationClass)
  private
    FActions: TInterfaceActions;
    FActionValue: Double;
    FActionImageData:TImage;
    FViewStateValue:TViewStates;
    FPropertyDetailData:TPropertyDetailData;
  public
    property Actions: TInterfaceActions read FActions write FActions;
    property ActionValue: double read FActionValue write FActionValue;
    property ActionImageData:TImage read FActionImageData write FActionImageData;
    property ViewStateValue:TViewStates read FViewStateValue write FViewStateValue;
    property PropertyDetailData:TPropertyDetailData read FPropertyDetailData
             write FPropertyDetailData;
  end;

  TErrorNotificationClass = class (TInterfacedObject, INotificationClass)
  private
    FActions: TInterfaceErrors;
    FActionMessage: string;
  public
    property Actions: TInterfaceErrors read FActions write FActions;
    property ActionMessage: string read FActionMessage write FActionMessage;
  end;

  TDefaultResourcesPath = record
    ResoucesFolder,
    EmployeeListsFolder,
    EmployeeAvatarsFolder,
    DefaultAvatarsFolder,
    UIFolder,
    UIAssetsFolder: string;
  end;

  TMainPanelDefaultCaptions = record
    CurrentListText,
    ListFilterText,
    RemoveButtonText,
    AddButtonText: string;
  end;

  TExportExcelTemplateDialogText = record
    DlgTitle,
    DlgDefaultExt,
    DlgFilter,
    DlgDefaultFileName:string;
  end;

  TListSettingsMenuText = record
    //MenuPanel
    MenuPanelTitle,
    //MenuListSettingsPanel
    BigYourListsNoteText,
    ButRemoveListCaption,
    ButRenameListCaption,
    ButCreateListCaption,
    //MenuInputListNamePanel
    BigCreateListNoteText,
    BigRenameListNoteText,
    HintCreateListText,
    HintRenameListText,
    ButCancelSetNameCaption,
    ButSaveListNameCaption,
    //RemoveListDialog
    RemoveListDlgCaption,
    RemoveListDlgMessage,
    RemoveListDlgYesCaption,
    RemoveListDlgNoCaption:string;
  end;

  TCloseEditPanelDlgTexts = record
    //Close Edit Panel dialog
    ClosePanelDlgCaption,
    ClosePanelDlgMessage,
    ClosePanelDlgYesCaption,
    ClosePanelDlgNoCaption,
    ClosePanelDlgCancelCaption:string;
  end;

  TNoListExistsDlgText = record
    //MenuPanel
    MenuPanelTitle,
    //DialogNoListExistsPanel
    BigNoListNoteText,
    StringButCreateListCaption,
    //MenuInputListNamePanel
    BigCreateListNoteText,
    HintCreateListText,
    ButCancelSetNameCaption,
    ButSaveListNameCaption:string;
  end;

  TErrorMessages = record
    ListNameEmptyErrorText,
    ListNameAlreadyExistsText:String;
  end;

implementation

end.

