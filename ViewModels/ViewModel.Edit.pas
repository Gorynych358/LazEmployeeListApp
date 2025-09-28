unit ViewModel.Edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, Dialogs, Model.Interfaces, Model.Declarations,
  Model.ProSu.Interfaces, Model.ProSu.Provider, Model.ProSu.InterfaceActions;



function CreateEditViewModelClass: IEditViewModelInterface;

implementation

  { TEditViewModel }
type
  TEditViewModel = class(TInterfacedObject, IEditViewModelInterface)
  private
    FEmployeeID:String;
    FIsChanged: Boolean;
    FIsSaveChangesButtonEnabled:Boolean;
    FFirstNameData,
    FLastNameData,
    FPatronymicData,
    FGenderData,
    FJobPositionData,
    FDateOfBirthData,
    FDateOfEmploymentData,
    FPhoneNumberData,
    FEmailData: TPropertyDetailData;
    FProvider:IProviderInterface;
    FEditModel:IEditModelInterface;
    function GetFullFioText: String;
    function GetIsChanged: Boolean;
    function GetIsSaveChangesButtonEnabled: Boolean;
    function GetProvider: IProviderInterface;
    procedure InitEmployeePropertiesFields;
    procedure SendChangeStateNotification(const AState: TViewStates);
    procedure SendUpdatePropertyNotification(const AData: TPropertyDetailData);
    procedure SendErrorNotification(const AErrorType: TInterfaceErrors;
              const AErrorMsg: String);
  public
    function GetFirstNameData: TPropertyDetailData;
    function GetLastNameData: TPropertyDetailData;
    function GetPatronymicData: TPropertyDetailData;
    function GetGenderData: TPropertyDetailData;
    function GetJobPositionData: TPropertyDetailData;
    function GetDateOfBirthData: TPropertyDetailData;
    function GetDateOfEmploymentData: TPropertyDetailData;
    function GetPhoneNumberData: TPropertyDetailData;
    function GetEmailData: TPropertyDetailData;
    function GetCloseEditPanelDlgTexts: TCloseEditPanelDlgTexts;
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
    
    procedure SaveChanges;
    procedure DiscardChanges;
    procedure CreateNewEmployee(const ACurrentListName:String);
    procedure EditEmployee(const AID, ACurrentListName: String);
    procedure SetEditModel (const AModel: IEditModelInterface);
    property IsChanged:Boolean read GetIsChanged;
    property FullFIO:String read GetFullFioText;
    property IsSaveChangesButtonEnabled:Boolean read GetIsSaveChangesButtonEnabled;
    property EditModel: IEditModelInterface read GetEditModel write SetEditModel;
    property Provider: IProviderInterface read GetProvider;
    constructor Create;
    destructor Destroy; override;
  end;

function CreateEditViewModelClass: IEditViewModelInterface;
begin
  Result:=TEditViewModel.Create;
end;

constructor TEditViewModel.Create;
begin
  FProvider:=CreateProSuProviderClass;
end;

///-----------------------------------------------------------///
///------------------Отправка событий во View-----------------///
///-----------------------------------------------------------///
function TEditViewModel.GetProvider: IProviderInterface;
begin
  if Assigned(FProvider) then Result:=FProvider;
end;

procedure TEditViewModel.SendUpdatePropertyNotification(
  const AData:TPropertyDetailData);
var
  tmpNotificationClass: TNotificationClass;
begin
  if not FIsChanged then
  begin
    FIsChanged:=True;
    FIsSaveChangesButtonEnabled:=True;
  end;

  tmpNotificationClass:=TNotificationClass.Create;
  tmpNotificationClass.Actions:=[updateEmployeeDetailPanel];
  tmpNotificationClass.PropertyDetailData:=AData;
  if Assigned(FProvider) then
   FProvider.NotifySubscribers(tmpNotificationClass);
  tmpNotificationClass.Free;
end;

procedure TEditViewModel.SendChangeStateNotification(const AState:TViewStates);
var
  tmpNotificationClass: TNotificationClass;
begin
  tmpNotificationClass:=TNotificationClass.Create;
  tmpNotificationClass.Actions:=[changeViewStateAction];
  tmpNotificationClass.ViewStateValue:=AState;
  if Assigned(FProvider) then
   FProvider.NotifySubscribers(tmpNotificationClass);
  tmpNotificationClass.Free;
end;

procedure TEditViewModel.SendErrorNotification(
  const AErrorType: TInterfaceErrors; const AErrorMsg: String);
var
  tmpErrorNotificationClass: TErrorNotificationClass;
begin
  tmpErrorNotificationClass:=TErrorNotificationClass.Create;
  try
    tmpErrorNotificationClass.Actions:=AErrorType;
    tmpErrorNotificationClass.ActionMessage:=AErrorMsg;
    FProvider.NotifySubscribers(tmpErrorNotificationClass);
  finally
    tmpErrorNotificationClass.Free;
  end;
end;

function TEditViewModel.GetFullFioText: String;
var fullName:String;
begin
  fullName:='';
  if FLastNameData.PropertyValue <> 'Не указано' then
     fullName := fullName + FLastNameData.PropertyValue;
  if FFirstNameData.PropertyValue <> 'Не указано' then
     fullName := fullName + #10 + FFirstNameData.PropertyValue;
  if FPatronymicData.PropertyValue <> 'Не указано' then
     fullName := fullName + #10 + FPatronymicData.PropertyValue;
  Result:=fullName;
end;

function TEditViewModel.GetIsChanged: Boolean;
begin
  Result:=FIsChanged;
end;

function TEditViewModel.GetIsSaveChangesButtonEnabled: Boolean;
begin
  Result:=FIsSaveChangesButtonEnabled;
end;

function TEditViewModel.GetFirstNameData: TPropertyDetailData;
begin
  Result:=FFirstNameData;
end;

function TEditViewModel.GetLastNameData: TPropertyDetailData;
begin
  Result:=FLastNameData;
end;

function TEditViewModel.GetPatronymicData: TPropertyDetailData;
begin
  Result:=FPatronymicData;
end;

function TEditViewModel.GetGenderData: TPropertyDetailData;
begin
  Result:=FGenderData;
end;

function TEditViewModel.GetJobPositionData: TPropertyDetailData;
begin
  Result:=FJobPositionData;
end;

function TEditViewModel.GetDateOfBirthData: TPropertyDetailData;
begin
  Result:=FDateOfBirthData;
end;

function TEditViewModel.GetDateOfEmploymentData: TPropertyDetailData;
begin
  Result:=FDateOfEmploymentData;
end;

function TEditViewModel.GetPhoneNumberData: TPropertyDetailData;
begin
  Result:=FPhoneNumberData;
end;

function TEditViewModel.GetEmailData: TPropertyDetailData;
begin
  Result:=FEmailData;
end;

function TEditViewModel.GetCloseEditPanelDlgTexts: TCloseEditPanelDlgTexts;
begin
  Result:=FEditModel.GetCloseEditPanelDlgTexts;
end;

procedure TEditViewModel.InitEmployeePropertiesFields;
begin
  FFirstNameData:=FEditModel.FirstNameData;
  FLastNameData:=FEditModel.LastNameData;
  FPatronymicData:=FEditModel.PatronymicData;
  FGenderData:=FEditModel.GenderData;
  FJobPositionData:=FEditModel.JobPositionData;
  FDateOfBirthData:=FEditModel.DateOfBirthData;
  FDateOfEmploymentData:=FEditModel.DateOfEmploymentData;
  FPhoneNumberData:=FEditModel.PhoneNumberData;
  FEmailData:=FEditModel.EmailData;
end;

procedure TEditViewModel.CreateNewEmployee(const ACurrentListName: String);
begin
  FIsChanged:=False;
  FEmployeeID:='';
  FEditModel.SetCurrentList(ACurrentListName);
  InitEmployeePropertiesFields;
  FIsSaveChangesButtonEnabled:=False;
  SendChangeStateNotification(TViewStates.vsEditEmployeeState);
end;

procedure TEditViewModel.EditEmployee(const AID, ACurrentListName: String);
var tmpEmployee:TEmployee;
begin
  FIsChanged:=False;
  FEmployeeID:=AID;
  FEditModel.SetCurrentList(ACurrentListName);
  InitEmployeePropertiesFields;
  tmpEmployee:=TEmployee.Create;
  FEditModel.GetEmployeeFromID(AID, tmpEmployee);
  FFirstNameData.PropertyValue:=tmpEmployee.FirstName;
  FLastNameData.PropertyValue:=tmpEmployee.LastName;
  FPatronymicData.PropertyValue:=tmpEmployee.Patronymic;
  FGenderData.PropertyValue:=tmpEmployee.Gender;
  FJobPositionData.PropertyValue:=tmpEmployee.JobPosition;
  FDateOfBirthData.PropertyValue:=tmpEmployee.DateOfBirth;
  FDateOfEmploymentData.PropertyValue:=tmpEmployee.DateOfEmployment;
  FPhoneNumberData.PropertyValue:=tmpEmployee.PhoneNumber;
  FEmailData.PropertyValue:=tmpEmployee.Email;
  FIsSaveChangesButtonEnabled:=False;
  SendChangeStateNotification(TViewStates.vsEditEmployeeState);
  FreeAndNil(tmpEmployee);
end;

destructor TEditViewModel.Destroy;
begin

  inherited;
end;

function TEditViewModel.GetEditModel: IEditModelInterface;
begin
  Result:=FEditModel;
end;

procedure TEditViewModel.SetFirstNameValue(const AValue: String);
var resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    if FFirstNameData.PropertyValue <> resValue then
    begin
      FFirstNameData.PropertyIsChanged:=True;
      FFirstNameData.PropertyValue:=resValue;
    end;
    SendUpdatePropertyNotification(FFirstNameData);
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
    'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SetLastNameValue(const AValue: String);
var resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    if FLastNameData.PropertyValue <> resValue then
    begin
      FLastNameData.PropertyIsChanged:=True;
      FLastNameData.PropertyValue:=resValue;
    end;
    SendUpdatePropertyNotification(FLastNameData);
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
    'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SetPatronymicValue(const AValue: String);
var resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    if FPatronymicData.PropertyValue <> resValue then
    begin
      FPatronymicData.PropertyIsChanged:=True;
      FPatronymicData.PropertyValue:=resValue;
    end;
    SendUpdatePropertyNotification(FPatronymicData);
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
      'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SetGenderValue(const AValue: String);
var genderValue,
    resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    genderValue:='';
    if UTF8LowerCase(resValue) = 'мужской' then genderValue:= 'Мужской';
    if UTF8LowerCase(resValue) = 'женский' then genderValue:= 'Женский';

    if genderValue <> '' then
    begin
      if FGenderData.PropertyValue <> genderValue then
      begin
        FGenderData.PropertyIsChanged:=True;
        FGenderData.PropertyValue:=resValue;
      end;
      SendUpdatePropertyNotification(FGenderData);
    end else
    begin
      SendErrorNotification([errWrongInputDetailProperty],
       'Ошибка ввода: возможное значение "Мужской|Женский"');
    end;
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
      'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SetJobPositionValue(const AValue: String);
var resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    if FJobPositionData.PropertyValue <> resValue then
    begin
      FJobPositionData.PropertyIsChanged:=True;
      FJobPositionData.PropertyValue:=resValue;
    end;
    SendUpdatePropertyNotification(FJobPositionData);
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
      'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SetDateOfBirthValue(const AValue: String);
var dt:TDate;
    resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    if TryStrToDate(resValue, dt) then
    begin
      if FDateOfBirthData.PropertyValue <> resValue then
      begin
        FDateOfBirthData.PropertyIsChanged:=True;
        FDateOfBirthData.PropertyValue:=resValue;
      end;
      SendUpdatePropertyNotification(FDateOfBirthData);
    end else
    begin
      SendErrorNotification([errWrongInputDetailProperty],
        'Ошибка: допустимый формат данных 00.00.0000');
    end;
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
      'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SetDateOfEmploymentValue(const AValue: String);
var
    dt:TDate;
    resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    if TryStrToDate(resValue, dt) then
    begin
      if FDateOfEmploymentData.PropertyValue <> resValue then
      begin
        FDateOfEmploymentData.PropertyIsChanged:=True;
        FDateOfEmploymentData.PropertyValue:=resValue;
      end;
      SendUpdatePropertyNotification(FDateOfEmploymentData);
    end else
    begin
      SendErrorNotification([errWrongInputDetailProperty],
        'Ошибка: допустимый формат данных 00.00.0000');
    end;
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
      'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SetPhoneNumberValue(const AValue: String);
var resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    if FPhoneNumberData.PropertyValue <> resValue then
    begin
      FPhoneNumberData.PropertyIsChanged:=True;
      FPhoneNumberData.PropertyValue:=resValue;
    end;
    SendUpdatePropertyNotification(FPhoneNumberData);
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
      'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SetEmailValue(const AValue: String);
var resValue:String;
begin
  resValue:=trim(AValue);
  if resValue <> '' then
  begin
    if FEmailData.PropertyValue <> resValue then
    begin
      FEmailData.PropertyIsChanged:=True;
      FEmailData.PropertyValue:=resValue;
    end;
    SendUpdatePropertyNotification(FEmailData);
  end else
  begin
    SendErrorNotification([errWrongInputDetailProperty],
      'Ошибка ввода: Поле не может быть пустым');
  end;
end;

procedure TEditViewModel.SaveChanges;
var tmpEmployee:TEmployee;
begin
  tmpEmployee:=TEmployee.Create;
  tmpEmployee.FirstName:=FFirstNameData.PropertyValue;
  tmpEmployee.LastName:=FLastNameData.PropertyValue;
  tmpEmployee.Patronymic:=FPatronymicData.PropertyValue;
  tmpEmployee.Gender:=FGenderData.PropertyValue;
  tmpEmployee.JobPosition:=FJobPositionData.PropertyValue;
  tmpEmployee.DateOfBirth:=FDateOfBirthData.PropertyValue;
  tmpEmployee.DateOfEmployment:=FDateOfEmploymentData.PropertyValue;
  tmpEmployee.PhoneNumber:=FPhoneNumberData.PropertyValue;
  tmpEmployee.Email:=FEmailData.PropertyValue;
  FEditModel.SaveEmployee(tmpEmployee);
  FreeAndNil(tmpEmployee);
  SendChangeStateNotification(TViewStates.vsMainState);
end;

procedure TEditViewModel.DiscardChanges;
begin
  SendChangeStateNotification(TViewStates.vsMainState);
end;

procedure TEditViewModel.SetEditModel(const AModel: IEditModelInterface);
begin
  FEditModel:=AModel;
end;

end.

