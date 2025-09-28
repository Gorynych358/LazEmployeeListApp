unit Model.Interfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Model.Declarations, Model.ProSu.Interfaces,
  Model.ProSu.InterfaceActions;
type
  IEmployeeRepository = interface
  ['{D5814587-CD30-4D9B-8F23-B07C01FA96C4}']
    procedure CreateNewEmployeeList(const AListName:String);
    procedure GetEmployeeList(var AEmployeeList: TEmployeeList;
              const ASortIndex: Byte; AListName:String);
    procedure ImportEmployeeListFromExcel(const APathToExcelFile:String);
    procedure GetEmployeeFromID(const AID, AListName: String;
              var AEmployee:TEmployee);
    procedure GetEmployeeFromNameAndSurname(const ALastName,
              AFirstName, AListName: String; var AEmployee: TEmployee);
    procedure AddNewEmployee(const AListName: String;
              const ANewEmployee: TEmployee);
    procedure UpdateEmployee(const AID, AListName: String;
              const AEmployee: TEmployee);
    procedure RemoveEmployeeFromID(const AID, AListName: String);
    procedure RemoveEmployeeFromNameAndSurname(const ALastName, AFirstName,
              AListName: String);
    procedure RenameEmployeeList(const ACurrentListName, ANewListName:String);
    procedure RemoveEmployeeList(const AListName:String);
    procedure GetExistingListsProperties(var AExistingListsProperties:
              TListPropertiesArray);
  end;

  IMainModelInterface = interface
    ['{1345E22D-6229-4C27-8D08-6EA8584BE718}']
    procedure GetEmployeeList(var AEmployeeList: TEmployeeList;
              const AListSortingIndex:Byte);
    procedure SetEmployeeRepository(const AEmployeeRepository:IEmployeeRepository);
    procedure SetCurrentList(const AListName:String);
    procedure GetExistingListsProperties(var AExistingListsProperties:
              TListPropertiesArray);
    procedure SaveStateOnExit;
    procedure DoExportExcelTemplate(const APathToSave:String);
    procedure AddNewEmployee(const ANewEmployee:TEmployee;
              const AListName:String);
    procedure RemoveEmployeeFromID(const AID: String);
    procedure CreateNewEmployeeList(const AListName:String);
    procedure RenameEmployeeList(const ACurrentListName, ANewListName:String);
    procedure RemoveEmployeeList(const AListName:String);
    function GetResourcesPath:TDefaultResourcesPath;
    function GetMainPanelText: TMainPanelDefaultCaptions;
    function GetListSettingsMenuText: TListSettingsMenuText;
    function GetNoListExistsDialogText: TNoListExistsDlgText;
    function GetExportExcelTemplateText: TExportExcelTemplateDialogText;
    function GetErrorMessageText: TErrorMessages;
    function GetCurrentListProperties: TListProperties;
    function GetSortingFileterCaptions:TStringList;
    property EmployeeRepository:IEmployeeRepository write SetEmployeeRepository;
  end;

  { IMainViewModelInterface }

  IMainViewModelInterface = interface
    ['{62CF0DAC-C808-4B1A-A6DC-57D7DC1912CB}']

    procedure InitMainScreen;
    procedure InitListSettingsMenu;
    procedure CloseListSettingsMenu;
    procedure SetSelectedEmployeesID(const AIDList:TStringList=Nil);
    procedure SetSelectedListIndex(const AListIndex:Integer);
    procedure SendNotification(const actions: TInterfaceActions);
    procedure SetMainModel (const AModel: IMainModelInterface);
    procedure SetListSortingTypeIndex (const AListSortingTypeIndex:Byte);
    procedure SetCurrentList(const AListIndex:Integer);
    procedure ExportExcelTemplate(const APathToSave:String);
    procedure GetEmployeeList(var AEmployeeDTOList:TMainEmployeeDTOList);
    procedure AddNewEmployee(const ANewEmployee:TEmployee);
    procedure AddNewEmployeeTo(const ANewEmployee:TEmployee;
      const AListName:String);
    procedure GetExistingListsTitles(var AListTitles:TStringList;
              const AWithCount:Boolean);
    procedure RemoveEmployee;
    procedure CreateNewEmployeeList(const AListName:String);
    procedure RenameEmployeeList(const ANewListName:String);
    procedure RemoveSelectedEmployeeList;
    function GetResourcesPath:TDefaultResourcesPath;
    function GetProvider: IProviderInterface;
    function GetCurrentListTitle:String;
    function GetCurrentListItemsCount:Integer;
    function GetMainPanelText: TMainPanelDefaultCaptions;
    function GetListSettingsMenuText: TListSettingsMenuText;
    function GetNoListExistsDialogText: TNoListExistsDlgText;
    function GetExportExcelTemplateText: TExportExcelTemplateDialogText;
    function GetSortingFileterCaptions:TStringList;
    function GetListSortingTypeIndex:Byte;
    function GetIsAddUserButtonEnabled: Boolean;
    function GetIsSelectListButtonEnabled: Boolean;
    function GetIsRemoveUserButtonEnabled: Boolean;
    function GetIsSortListGroupEnabled: Boolean;
    function GetSelectedListIndex: Integer;
    //MenuPanel
    function GetIsListSettingsPanelVisible: Boolean;
    function GetIsInputListNamePanelVisible: Boolean;
    function GetIsNoListExistsPanelVisible: Boolean;
    function GetIsCloseMenuButtonVisible: Boolean;
    function GetIsAddNewListButtonEnabled: Boolean;
    function GetIsEditListCancelButtonEnabled: Boolean;
    function GetIsEditListOkButtonEnabled: Boolean;
    function GetIsRemoveListButtonEnabled: Boolean;
    function GetIsRenameListButtonEnabled: Boolean;

    property MainModel: IMainModelInterface write SetMainModel;
    property Provider: IProviderInterface read GetProvider;
    property ResourcesPath:TDefaultResourcesPath read GetResourcesPath;
    property CurrentListTitle:String read GetCurrentListTitle;
    property CurrentListCount:Integer read GetCurrentListItemsCount;
    property ListSortingTypeIndex:Byte read GetListSortingTypeIndex write
             SetListSortingTypeIndex;
    property ExportExcelTemplateText:TExportExcelTemplateDialogText read
             GetExportExcelTemplateText;
    property IsAddUserButtonEnabled:Boolean read GetIsAddUserButtonEnabled;
    property IsSelectListButtonEnabled:Boolean read GetIsSelectListButtonEnabled;
    property IsRemoveUserButtonEnabled:Boolean read GetIsRemoveUserButtonEnabled;
    property IsSortListGroupEnabled:Boolean read GetIsSortListGroupEnabled;
    property SelectedListIndex:Integer read GetSelectedListIndex  write
             SetSelectedListIndex;
    //Menu panel
    property IsListSettingsPanelVisible:Boolean read GetIsListSettingsPanelVisible;
    property IsInputListNamePanelVisible:Boolean read GetIsInputListNamePanelVisible;
    property IsNoListExistsPanelVisible:Boolean read GetIsNoListExistsPanelVisible;
    property IsCloseMenuButtonVisible:Boolean read GetIsCloseMenuButtonVisible;
    property IsRemoveListButtonEnabled:Boolean read GetIsRemoveListButtonEnabled;
    property IsRenameListButtonEnabled:Boolean read GetIsRenameListButtonEnabled;
    property IsAddNewListButtonEnabled:Boolean read GetIsAddNewListButtonEnabled;
    property IsEditListOkButtonEnabled:Boolean read GetIsEditListOkButtonEnabled;
    property IsEditListCancelButtonEnabled:Boolean read GetIsEditListCancelButtonEnabled;
  end;

  { IEditModelInterface }

  IEditModelInterface = interface
    ['{2AC3EAB8-71AB-4FB3-A0C9-1E8867402B1D}']
    function GetDateOfBirthData: TPropertyDetailData;
    function GetDateOfEmploymentData: TPropertyDetailData;
    function GetEmailData: TPropertyDetailData;
    function GetJobPositionData: TPropertyDetailData;
    function GetFirstNameData: TPropertyDetailData;
    function GetGenderData: TPropertyDetailData;
    function GetLastNameData: TPropertyDetailData;
    function GetPatronymicData: TPropertyDetailData;
    function GetPhoneNumberData: TPropertyDetailData;
    function GetCloseEditPanelDlgTexts: TCloseEditPanelDlgTexts;
    procedure SetEmployeeRepository(const AEmployeeRepository:IEmployeeRepository);
    procedure SaveEmployee(const ANewEmployee: TEmployee);
    procedure SetCurrentList(const AListName:String);
    procedure GetEmployeeFromID(const AID:String; var AEmployee:TEmployee);
    property FirstNameData:TPropertyDetailData read GetFirstNameData;
    property LastNameData:TPropertyDetailData read GetLastNameData;
    property PatronymicData:TPropertyDetailData read GetPatronymicData;
    property GenderData:TPropertyDetailData read GetGenderData;
    property JobPositionData:TPropertyDetailData read GetJobPositionData;
    property DateOfBirthData:TPropertyDetailData read GetDateOfBirthData;
    property DateOfEmploymentData:TPropertyDetailData read GetDateOfEmploymentData;
    property PhoneNumberData:TPropertyDetailData read GetPhoneNumberData;
    property EmailData:TPropertyDetailData read GetEmailData;
    property EmployeeRepository:IEmployeeRepository write SetEmployeeRepository;
  end;

  { IEditViewModelInterface }

  IEditViewModelInterface = interface
    ['{7C45CCA0-4B48-4A32-A3A3-0CD7B5050D17}']
    function GetDateOfBirthData: TPropertyDetailData;
    function GetDateOfEmploymentData: TPropertyDetailData;
    function GetEmailData: TPropertyDetailData;
    function GetJobPositionData: TPropertyDetailData;
    function GetFirstNameData: TPropertyDetailData;
    function GetGenderData: TPropertyDetailData;
    function GetLastNameData: TPropertyDetailData;
    function GetPatronymicData: TPropertyDetailData;
    function GetPhoneNumberData: TPropertyDetailData;
    function GetFullFioText: String;
    function GetIsChanged: Boolean;
    function GetCloseEditPanelDlgTexts: TCloseEditPanelDlgTexts;
    function GetIsSaveChangesButtonEnabled: Boolean;
    function GetProvider: IProviderInterface;
    function GetEditModel: IEditModelInterface;
    procedure SetFirstNameValue(const AValue:String);
    procedure SetLastNameValue(const AValue:String);
    procedure SetPatronymicValue(const AValue:String);
    procedure SetGenderValue(const AValue:String);
    procedure SetJobPositionValue(const AValue:String);
    procedure SetDateOfBirthValue(const AValue:String);
    procedure SetDateOfEmploymentValue(const AValue:String);
    procedure SetPhoneNumberValue(const AValue:String);
    procedure SetEmailValue(const AValue:String);
    procedure CreateNewEmployee(const ACurrentListName:String);
    procedure EditEmployee(const AID, ACurrentListName: String);
    procedure SaveChanges;
    procedure DiscardChanges;
    procedure SetEditModel (const AModel: IEditModelInterface);
    property IsChanged:Boolean read GetIsChanged;
    property FullFIO:String read GetFullFioText;
    property IsSaveChangesButtonEnabled:Boolean read GetIsSaveChangesButtonEnabled;
    property Provider: IProviderInterface read GetProvider;
    property EditModel: IEditModelInterface read GetEditModel write SetEditModel;
  end;

implementation

end.

