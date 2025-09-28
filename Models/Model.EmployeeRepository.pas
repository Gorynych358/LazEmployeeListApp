unit Model.EmployeeRepository;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, fpstypes, fpspreadsheet, fpsallformats,fpsutils,
  laz_fpspreadsheet, Model.Interfaces, Model.Declarations, Dialogs;


function CreateEmployeeRepositoryClass:IEmployeeRepository;

implementation
type
{procedure TForm1.Button1Click(Sender: TObject);
var
  Image1: TImage;
  Bitmap1: TBitmap;
  Workbook1: TsWorkbook;
  Worksheet1: TsWorksheet;
  ImagePath: string;
begin
  // Создание изображения для ячейки Gantt
  Image1 := TImage.Create(nil);
  Bitmap1 := TBitmap.Create;
  Bitmap1.SetSize(100, 100);
  Image1.Picture.Graphic := Bitmap1;
  Image1.Picture.Bitmap := Bitmap1;
  // Рисуем прямоугольник на изображении
  Image1.Canvas.Brush.Color := clRed;
  Image1.Canvas.FillRect(0, 0, 50, 50);
  // Сохраняем изображение в файл
  ImagePath := 'C:\temp\gantt_image.png';
  Bitmap1.SaveToFile(ImagePath);

  // Создание таблицы
  Workbook1 := TsWorkbook.Create;
  Worksheet1 := Workbook1.AddWorksheet('Gantt');
  // Вставка изображения в ячейку Gantt
  Worksheet1.WriteImage(0, 0, ImagePath, 50, 50, 0, 0, False);
  // Обновление таблицы
  Worksheet1.Refresh;
end;}
    { TEmployeeRepository }

TEmployeeRepository = class(TInterfacedObject, IEmployeeRepository)
  private
    PathToExcelStorage:String;
    PathToExcelExportFile:String;
    function GenerateGUID:String;
    procedure ReadEmployeeFromExcelRow(const AWorksheet: TsWorksheet;
      const ARow: Cardinal; var AEmployee: TEmployee);
    procedure WriteEmployeeToExcelRow(const AEmployee: TEmployee;
      const ARow: Cardinal; var AWorksheet: TsWorksheet);
  public
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
    constructor Create;
    destructor Destroy; override;
  end;

function CreateEmployeeRepositoryClass: IEmployeeRepository;
begin
  Result:=TEmployeeRepository.Create;
end;

{ TEmployeeRepository }

function TEmployeeRepository.GenerateGUID: String;
var
  numRepeat:Byte;
  Uid: TGuid;
  creationResult: HResult;
begin
  numRepeat:=10;
  //Делаем десять попыток создания GUID. Если не получается, выадём исключение:
  repeat
    Dec(numRepeat);
    creationResult:=CreateGuid(Uid);
  until (creationResult <> S_OK) or (numRepeat > 0);

  if creationResult = S_OK then
  begin
    Result:=GuidToString(Uid);
  end else
    raise Exception.Create('Employee repository Error: Can not create GUID');
end;

procedure TEmployeeRepository.ReadEmployeeFromExcelRow(const AWorksheet: TsWorksheet;
  const ARow: Cardinal; var AEmployee:TEmployee);
begin
  with AEmployee do
  begin
    ID:=Trim(AWorksheet.ReadAsText(ARow, 0));
    LastName:=Trim(AWorksheet.ReadAsText(ARow, 1));
    FirstName:=Trim(AWorksheet.ReadAsText(ARow, 2));
    Patronymic:=Trim(AWorksheet.ReadAsText(ARow, 3));
    JobPosition:=Trim(AWorksheet.ReadAsText(ARow, 4));
    Gender:='Не указан';
    if UTF8LowerCase(Trim(AWorksheet.ReadAsText(ARow, 5)))='мужской' then
       Gender:='Мужской';
    if UTF8LowerCase(Trim(AWorksheet.ReadAsText(ARow, 5)))='женский' then
       Gender:='Женский';
    DateOfBirth:=Trim(AWorksheet.ReadAsText(ARow, 6));
    DateOfEmployment:=Trim(AWorksheet.ReadAsText(ARow, 7));
    PhoneNumber:=Trim(AWorksheet.ReadAsText(ARow, 8));
    Email:=Trim(AWorksheet.ReadAsText(ARow, 9));
  end;
end;

procedure TEmployeeRepository.WriteEmployeeToExcelRow(const AEmployee:TEmployee;
  const ARow: Cardinal; var AWorksheet: TsWorksheet);
begin
  with AEmployee do
  begin
    AWorksheet.WriteText(ARow, 0, ID);
    AWorksheet.WriteText(ARow, 1, LastName);
    AWorksheet.WriteText(ARow, 2, FirstName);
    AWorksheet.WriteText(ARow, 3, Patronymic);
    AWorksheet.WriteText(ARow, 4, JobPosition);
    AWorksheet.WriteText(ARow, 5, Gender);
    AWorksheet.WriteText(ARow, 6, DateOfBirth);
    AWorksheet.WriteText(ARow, 7, DateOfEmployment);
    AWorksheet.WriteText(ARow, 8, PhoneNumber);
    AWorksheet.WriteText(ARow, 9, Email);
  end;
end;

procedure TEmployeeRepository.CreateNewEmployeeList(const AListName: String);
var
  i:integer;
  row: Cardinal;
  srcCell:PCell;
  newCell:PCell;
  tmpWorkbook:TsWorkbook;
  srcWorksheet:TsWorksheet;
  newWorksheet:TsWorksheet;
begin
  tmpWorkbook:=TsWorkbook.Create;
  try
    tmpWorkbook.ReadFromFile(PathToExcelStorage);
    srcWorksheet:=tmpWorkbook.GetWorksheetByName('FormatSamplePage');
    newWorksheet:=tmpWorkbook.AddWorksheet(AListName);

    for i:=0 to srcWorksheet.GetLastColIndex do
    begin
      srcCell:=srcWorksheet.GetCell(0,i);
      newCell:=newWorksheet.CopyCell(0,i, 0, i,srcWorksheet);
      newWorksheet.WriteColWidth(i, srcWorksheet.GetColWidth(i, suMillimeters),
        suMillimeters);
    end;
    newWorksheet.Options:= newWorksheet.Options + [soHasFrozenPanes];
    newWorksheet.TopPaneHeight:=1;
    tmpWorkbook.WriteToFile(PathToExcelStorage, True);
  finally
    FreeAndNil(tmpWorkbook);
  end;
end;

procedure TEmployeeRepository.GetEmployeeList(var AEmployeeList: TEmployeeList;
  const ASortIndex: Byte; AListName:String);
var
  row: Cardinal;
  isChanged:Boolean;
  tmpWorkbook:TsWorkbook;
  tmpWorksheet:TsWorksheet;
  sortParams: TsSortParams;
  tmpEmployee:TEmployee;
begin
  //CreateNewEmployeeList('');
  isChanged:=False;
  tmpWorkbook:=TsWorkbook.Create;
  tmpWorkbook.ReadFromFile(PathToExcelStorage);
  tmpWorksheet:=tmpWorkbook.GetWorksheetByName(AListName);
  if ASortIndex > 0 then
  begin
    sortParams := InitSortParams(true, 1);
    sortParams.Keys[0].ColRowIndex := ASortIndex;
    sortParams.Keys[0].Options := [];
    tmpWorksheet.Sort(sortParams, 1, 0, tmpWorksheet.GetLastRowIndex,
    tmpWorksheet.GetLastColIndex);
  end;

  for row:=1 to tmpWorksheet.GetLastRowIndex do
  begin
    tmpEmployee:=TEmployee.Create;
    ReadEmployeeFromExcelRow(tmpWorksheet, row, tmpEmployee);
    if tmpEmployee.ID = '' then
    begin
       tmpEmployee.ID:=GenerateGUID;
       tmpWorksheet.WriteText(row, 0, tmpEmployee.ID);
       isChanged:=True;
    end;
    AEmployeeList.Add(tmpEmployee);
  end;

  if isChanged then tmpWorkbook.WriteToFile(PathToExcelStorage, True);

  FreeAndNil(tmpWorkbook);
end;

procedure TEmployeeRepository.ImportEmployeeListFromExcel(
  const APathToExcelFile: String);
begin

end;

procedure TEmployeeRepository.GetEmployeeFromID(const AID, AListName: String;
  var AEmployee: TEmployee);
var
  row: Cardinal;
  tmpWorkbook:TsWorkbook;
  tmpWorksheet:TsWorksheet;
  sortParams: TsSortParams;
begin
  if not Assigned(AEmployee) then Exit;
  tmpWorkbook:=TsWorkbook.Create;
  tmpWorkbook.ReadFromFile(PathToExcelStorage);
  tmpWorksheet:=tmpWorkbook.GetWorksheetByName(AListName);
  for row:=1 to tmpWorksheet.GetLastRowIndex do
  begin
    if tmpWorksheet.ReadAsText(row, 0) = AID then
    begin
      ReadEmployeeFromExcelRow(tmpWorksheet, row, AEmployee);
      break;
    end;
  end;
  FreeAndNil(tmpWorkbook);
end;

procedure TEmployeeRepository.GetEmployeeFromNameAndSurname(const ALastName,
  AFirstName, AListName: String; var AEmployee: TEmployee);
var
  row: Cardinal;
  tmpWorkbook:TsWorkbook;
  tmpWorksheet:TsWorksheet;
  sortParams: TsSortParams;
begin
  if not Assigned(AEmployee) then Exit;
  tmpWorkbook:=TsWorkbook.Create;
  tmpWorkbook.ReadFromFile(PathToExcelStorage);
  tmpWorksheet:=tmpWorkbook.GetWorksheetByName(AListName);
  for row:=1 to tmpWorksheet.GetLastRowIndex do
  begin
    if (tmpWorksheet.ReadAsText(row, 2) = AFirstName) and
      (tmpWorksheet.ReadAsText(row, 1) = ALastName) then
    begin
      ReadEmployeeFromExcelRow(tmpWorksheet, row, AEmployee);
      break;
    end;
  end;
  FreeAndNil(tmpWorkbook);
end;

procedure TEmployeeRepository.AddNewEmployee(const AListName:String;
  const ANewEmployee: TEmployee);
var
  tmpWorkbook:TsWorkbook;
  tmpWorksheet:TsWorksheet;
  rowIndex:Cardinal;
begin
  tmpWorkbook:=TsWorkbook.Create;
  tmpWorkbook.ReadFromFile(PathToExcelStorage);
  tmpWorksheet:=tmpWorkbook.GetWorksheetByName(AListName);
  if ANewEmployee.ID='' then ANewEmployee.ID:=GenerateGUID;
  rowIndex:=tmpWorksheet.GetLastRowIndex + 1;
  tmpWorksheet.InsertRow(rowIndex);
  WriteEmployeeToExcelRow(ANewEmployee, rowIndex, tmpWorksheet);
  {with ANewEmployee do
  begin
    rowIndex:=tmpWorksheet.GetLastRowIndex + 1;
    tmpWorksheet.InsertRow(rowIndex);
    if ID='' then ID:=GenerateGUID;
    tmpWorksheet.WriteText(rowIndex, 0, ID);
    tmpWorksheet.WriteText(rowIndex, 1, LastName);
    tmpWorksheet.WriteText(rowIndex, 2, FirstName);
    tmpWorksheet.WriteText(rowIndex, 3, Patronymic);
    tmpWorksheet.WriteText(rowIndex, 4, JobPosition);
    if Gender='Мужской' then tmpWorksheet.WriteText(rowIndex, 5, 'Мужской');
    if Gender='Женский' then tmpWorksheet.WriteText(rowIndex, 5, 'Женский');
    tmpWorksheet.WriteText(rowIndex, 6, DateOfBirth);
    tmpWorksheet.WriteText(rowIndex, 7, DateOfEmployment);
    tmpWorksheet.WriteText(rowIndex, 8, PhoneNumber);
    tmpWorksheet.WriteText(rowIndex, 9, Email);
  end;}
  tmpWorkbook.WriteToFile(PathToExcelStorage, True);
  FreeAndNil(tmpWorkbook);
end;

procedure TEmployeeRepository.UpdateEmployee(const AID, AListName: String;
  const AEmployee: TEmployee);
var
  row: Cardinal;
  tmpWorkbook:TsWorkbook;
  tmpWorksheet:TsWorksheet;
begin
  tmpWorkbook:=TsWorkbook.Create;
  tmpWorkbook.ReadFromFile(PathToExcelStorage);
  tmpWorksheet:=tmpWorkbook.GetWorksheetByName(AListName);
  for row:=1 to tmpWorksheet.GetLastRowIndex do
  begin
    if tmpWorksheet.ReadAsText(row, 0) = AID then
    begin
      WriteEmployeeToExcelRow(AEmployee, row, tmpWorksheet);
      break;
    end;
  end;
  tmpWorkbook.WriteToFile(PathToExcelStorage, True);
  FreeAndNil(tmpWorkbook);
end;

procedure TEmployeeRepository.RemoveEmployeeFromNameAndSurname(const ALastName,
  AFirstName, AListName: String);
begin

end;

procedure TEmployeeRepository.RemoveEmployeeFromID(const AID, AListName:String);
var
  tmpWorkbook:TsWorkbook;
  tmpWorksheet:TsWorksheet;
  row: Cardinal;
  rowId: String;
begin
  try
    tmpWorkbook:=TsWorkbook.Create;
    tmpWorkbook.ReadFromFile(PathToExcelStorage);
    tmpWorksheet:=tmpWorkbook.GetWorksheetByName(AListName);
    for row:=1 to tmpWorksheet.GetLastRowIndex do
    begin
       rowId := tmpWorksheet.ReadAsText(row, 0);
       if rowId = AID then
         Break
       else
           rowId:='';
    end;
    if rowId <> '' then
    begin
      //ShowMessage('Call delete employee with ID: ' + AID + #10 +
      //'Delete row with ID: ' + rowId);
      tmpWorksheet.DeleteRow(row);
      tmpWorkbook.WriteToFile(PathToExcelStorage, True);
    end;
  finally
    tmpWorkbook.Free;
  end;
end;

procedure TEmployeeRepository.RenameEmployeeList(const ACurrentListName,
  ANewListName: String);
var
  tmpWorkbook:TsWorkbook;
  tmpWorksheet:TsWorksheet;
begin
  try
    tmpWorkbook:=TsWorkbook.Create;
    tmpWorkbook.ReadFromFile(PathToExcelStorage);
    tmpWorksheet:=tmpWorkbook.GetWorksheetByName(ACurrentListName);
    tmpWorksheet.Name:=ANewListName;
    tmpWorkbook.WriteToFile(PathToExcelStorage, True);
  finally
    tmpWorkbook.Free;
  end;
end;

procedure TEmployeeRepository.RemoveEmployeeList(const AListName: String);
var
  tmpWorkbook:TsWorkbook;
  tmpWorksheet:TsWorksheet;
begin
  try
    tmpWorkbook:=TsWorkbook.Create;
    tmpWorkbook.ReadFromFile(PathToExcelStorage);
    tmpWorksheet:=tmpWorkbook.GetWorksheetByName(AListName);
    tmpWorkbook.RemoveWorksheet(tmpWorksheet);
    tmpWorkbook.WriteToFile(PathToExcelStorage, True);
  finally
    tmpWorkbook.Free;
  end;
end;

procedure TEmployeeRepository.GetExistingListsProperties(
  var AExistingListsProperties:TListPropertiesArray);
var
  i,
  itemsCount:Integer;
  listProperties:TListProperties;
  tmpWorkbook:TsWorkbook;
  tmpWorkSheet:TsWorksheet;
begin
  tmpWorkbook:=TsWorkbook.Create;
  try
    tmpWorkbook.ReadFromFile(PathToExcelStorage);
    itemsCount:=tmpWorkbook.GetWorksheetCount();
    if(itemsCount = 1) then Exit;
    SetLength(AExistingListsProperties, 0);
    SetLength(AExistingListsProperties, itemsCount - 1);
    for i := 1 to Pred(itemsCount) do
    begin
      tmpWorkSheet := tmpWorkbook.GetWorksheetByIndex(i);
      listProperties.ListTitle:=tmpWorkSheet.Name;
      listProperties.ListItemsCount:=tmpWorkSheet.GetLastRowIndex;
      AExistingListsProperties[i-1]:=listProperties;
    end;
  finally
    tmpWorkbook.Free;
  end;
end;

constructor TEmployeeRepository.Create;
begin
  inherited;
  PathToExcelStorage:=GetCurrentDir+'\Resources\EmployeeLists\EmployeeList.xlsx';
  PathToExcelExportFile:=GetCurrentDir+
    '\Resources\EmployeeLists\ExprotEmployeeList.xlsx';
end;

destructor TEmployeeRepository.Destroy;
begin
  inherited;
end;


end.

