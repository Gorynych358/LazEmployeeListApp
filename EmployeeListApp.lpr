program EmployeeListApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, View.MainForm, View.EmployeeItemPanel, model.edit, Declarations,
  Employee, Repository, datetimectrls, lazcontrols, runtimetypeinfocontrols,
  anchordockpkg, ViewModel.Edit, ViewModel.Main, Model.Interfaces,
  Model.ProSu.InterfaceActions, Model.ProSu.Interfaces, Model.ProSu.Provider,
  Model.ProSu.Subscriber, Model.EmployeeRepository, Model.Main;

{$R *.res}
var
  FMainViewModel:IMainViewModelInterface;
  FMainModel:IMainModelInterface;
  FEditViewModel:IEditViewModelInterface;
  FEditModel:IEditModelInterface;
  FEmployeeRepository:IEmployeeRepository;
begin
  //SetHeapTraceOutput('heaptrace.trc');
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TAppMainForm, AppMainForm);
  //Инжектим зависимости:
  FEmployeeRepository:=CreateEmployeeRepositoryClass;
  //Класс главной формы:
  FMainModel:=CreateMainModelClass;
  FMainModel.EmployeeRepository:=FEmployeeRepository;
  FMainViewModel:=CreateMainViewModelClass;
  FMainViewModel.MainModel:=FMainModel;
  AppMainForm.MainViewModel:=FMainViewModel;
  //Класс обслуживающий редактирование и добавления нового сотрудника
  //в главной форме приложения:
  FEditModel:=CreateEditModelClass;
  FEditModel.EmployeeRepository:=FEmployeeRepository;
  FEditViewModel:=CreateEditViewModelClass;
  FEditViewModel.EditModel:=FEditModel;
  AppMainForm.EditViewModel:=FEditViewModel;
  Application.Run;
end.

