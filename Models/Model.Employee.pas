unit Model.Employee;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8;


type

  { TEmployee }
  Genders=(Man, Woman);
  TEmployee=class
    private
      FAvataraPath: string;
      FDateOfBirth: TDateTime;
      FDateOfEmployment: TDateTime;
      FEmployeePosition: string;
      FFirstName: string;
      FFullName: string;
      FGender: Genders;
      FID: Integer;
      FLastName: string;
      FPatronymic: string;
      FPhoneNumber: String;
      procedure SetFirstName(AValue: string);
      procedure SetLastName(AValue: string);
      procedure BuildFullName;
    public
      constructor Create(ALastName:String; AFirstName:String; APatronymic:String);
      constructor Create;
      Property ID:Integer read FID write FID;
      Property FirstName:string read FFirstName write FFirstName;
      Property LastName:string read FLastName write FLastName;
      Property Patronymic:string read FPatronymic write FPatronymic;
      Property FullName:string read FFullName;
      Property Gender:Genders read FGender write FGender;
      Property EmployeePosition:string read FEmployeePosition write FEmployeePosition;
      Property AvataraPath:string read FAvataraPath write FAvataraPath;
      Property DateOfBirth:TDateTime read FDateOfBirth write FDateOfBirth;
      Property DateOfEmployment:TDateTime read FDateOfEmployment write FDateOfEmployment;
      Property PhoneNumber:String read FPhoneNumber write FPhoneNumber;
  end;

implementation

{ TEmployee }

procedure TEmployee.SetFirstName(AValue: string);
begin
  if FFirstName=AValue then Exit;
  FFirstName:=AValue;
  BuildFullName;
end;

procedure TEmployee.SetLastName(AValue: string);
begin
  if FLastName=AValue then Exit;
  FLastName:=AValue;
  BuildFullName;
end;

procedure TEmployee.BuildFullName;
var
  aFirstNameLetter,
  aFirstPatornymicLetter:String;
begin
  aFirstNameLetter:=UTF8Copy(FFirstName, 1, 1);//copy('012345', 3, 1);
  aFirstPatornymicLetter:=UTF8Copy(FPatronymic, 1, 1);
  FFullName:=FLastName + ' ' + aFirstNameLetter + '. ' + aFirstPatornymicLetter + '.';

end;

constructor TEmployee.Create(ALastName: String; AFirstName: String;
  APatronymic: String);
begin
  FLastName:=ALastName;
  FFirstName:=AFirstName;
  FPatronymic:=APatronymic;
  BuildFullName;
end;

constructor TEmployee.Create;
begin

end;

end.

