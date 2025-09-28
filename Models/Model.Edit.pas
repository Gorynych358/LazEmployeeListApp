unit Model.Edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Model.Interfaces, Model.Declarations;

function CreateEditModelClass:IEditModelInterface;
implementation
type

  { TModelEdit }


  TModelEdit=class(TInterfacedObject, IEditModelInterface)
    private
      FEmployeeID,
      FCurrentListName:String;
      FEmployeeRepository:IEmployeeRepository;
      FCloseEditPanelDlgTexts:TCloseEditPanelDlgTexts;
      function GetDateOfBirthData: TPropertyDetailData;
      function GetDateOfEmploymentData: TPropertyDetailData;
      function GetEmailData: TPropertyDetailData;
      function GetJobPositionData: TPropertyDetailData;
      function GetFirstNameData: TPropertyDetailData;
      function GetGenderData: TPropertyDetailData;
      function GetLastNameData: TPropertyDetailData;
      function GetPatronymicData: TPropertyDetailData;
      function GetPhoneNumberData: TPropertyDetailData;
      procedure SetEmployeeRepository(const AEmployeeRepository:IEmployeeRepository);
    public
      procedure SetCurrentList(const AListName:String);
      procedure GetEmployeeFromID(const AID:String; var AEmployee:TEmployee);
      procedure SaveEmployee(const ANewEmployee: TEmployee);
      function GetCloseEditPanelDlgTexts: TCloseEditPanelDlgTexts;
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
      constructor Create;
  end;

function CreateEditModelClass: IEditModelInterface;
begin
  Result:=TModelEdit.Create;
end;

{ TModelEdit }

constructor TModelEdit.Create;
begin
  With FCloseEditPanelDlgTexts do
  begin
    //Диалог закрытия панели создания/редактирования анкеты сотрудника:
    ClosePanelDlgCaption:='Данные изменены';
    ClosePanelDlgMessage:='Вы внесли изменения в анкетных данных сотрудника.' +
    'Сохранить изменения?';
    ClosePanelDlgYesCaption:='Да';
    ClosePanelDlgNoCaption:='Нет';
    ClosePanelDlgCancelCaption:='Отмена';
  end;
end;

function TModelEdit.GetCloseEditPanelDlgTexts: TCloseEditPanelDlgTexts;
begin
  Result:=FCloseEditPanelDlgTexts;
end;

procedure TModelEdit.SetCurrentList(const AListName: String);
begin
  FEmployeeID:='';
  FCurrentListName:=AListName;
end;

procedure TModelEdit.GetEmployeeFromID(const AID:String; var AEmployee: TEmployee);
begin
  FEmployeeID:=AID;
  FEmployeeRepository.GetEmployeeFromID(AID, FCurrentListName, AEmployee);
end;

procedure TModelEdit.SaveEmployee(const ANewEmployee: TEmployee);
begin
  if FEmployeeID='' then//Создаём нового сотрудника
     FEmployeeRepository.AddNewEmployee(FCurrentListName, ANewEmployee)
  else //Обновляем существующего сотрудника
    FEmployeeRepository.UpdateEmployee(FEmployeeID, FCurrentListName, ANewEmployee);
end;

//----------------------------------------------------------------//
//----------------Employee properties detail texts----------------//
//----------------------------------------------------------------//

function TModelEdit.GetFirstNameData: TPropertyDetailData;
var tmpFirstNameData:TPropertyDetailData;
begin
  With tmpFirstNameData do
  begin
    PropertyType:=TDetailPropertyType.dptStringType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Имя:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите Имя сотрудника(*Обязательное поле):';
  end;
  Result:=tmpFirstNameData;
end;

function TModelEdit.GetLastNameData: TPropertyDetailData;
var tmpLastNameData:TPropertyDetailData;
begin
  With tmpLastNameData do
  begin
    PropertyType:=TDetailPropertyType.dptStringType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Фамилия:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите Фамилию сотрудника(*Обязательное поле):';
  end;
  Result:=tmpLastNameData;
end;

function TModelEdit.GetPatronymicData: TPropertyDetailData;
var tmpPatronymicData:TPropertyDetailData;
begin
  With tmpPatronymicData do
  begin
    PropertyType:=TDetailPropertyType.dptStringType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Отчество:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите Отчество сотрудника:';
  end;
  Result:=tmpPatronymicData;
end;

function TModelEdit.GetGenderData: TPropertyDetailData;
var tmpGenderData:TPropertyDetailData;
begin
  With tmpGenderData do
  begin
    PropertyType:=TDetailPropertyType.dptStringType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Пол:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите Пол сотрудника(Мужской|Женский):';
  end;
  Result:=tmpGenderData;
end;

function TModelEdit.GetJobPositionData: TPropertyDetailData;
var tmpJobPositionData:TPropertyDetailData;
begin
  With tmpJobPositionData do
  begin
    PropertyType:=TDetailPropertyType.dptStringType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Должность:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите Должность сотрудника:';
  end;
  Result:=tmpJobPositionData;
end;

function TModelEdit.GetDateOfBirthData: TPropertyDetailData;
var tmpDateOfBirthData:TPropertyDetailData;
begin
  With tmpDateOfBirthData do
  begin
    PropertyType:=TDetailPropertyType.dptDateType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Дата рождения:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите Дату рождения сотрудника:';
  end;
  Result:=tmpDateOfBirthData;
end;

function TModelEdit.GetDateOfEmploymentData: TPropertyDetailData;
var tmpDateOfEmploymentData:TPropertyDetailData;
begin
  With tmpDateOfEmploymentData do
  begin
    PropertyType:=TDetailPropertyType.dptDateType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Дата трудоустройства:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите Дату трудоустройства сотрудника:';
  end;
  Result:=tmpDateOfEmploymentData;
end;

function TModelEdit.GetPhoneNumberData: TPropertyDetailData;
var tmpPhoneNumberData:TPropertyDetailData;
begin
  With tmpPhoneNumberData do
  begin
    PropertyType:=TDetailPropertyType.dptStringType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Номер телефона:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите номерт телефона в формате +7(xxx)xxx-xx-xx:';
  end;
  Result:=tmpPhoneNumberData;
end;

function TModelEdit.GetEmailData: TPropertyDetailData;
var tmpEmailData:TPropertyDetailData;
begin
  With tmpEmailData do
  begin
    PropertyType:=TDetailPropertyType.dptStringType;
    PropertyIsChanged:=False;
    PropertyChangedCaption:=' (Изменено)';
    PropertyName:='Адрес электронной почты:';
    PropertyValue:='Не указано';
    PropertyHint:='Введите E-mail сотрудника:';
  end;
  Result:=tmpEmailData;
end;

procedure TModelEdit.SetEmployeeRepository(
  const AEmployeeRepository: IEmployeeRepository);
begin
  if Assigned(AEmployeeRepository) then
     FEmployeeRepository:=AEmployeeRepository;
end;

end.

