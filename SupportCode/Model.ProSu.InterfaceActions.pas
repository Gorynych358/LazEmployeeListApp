unit Model.ProSu.InterfaceActions;

interface

type
  TInterfaceAction = (changeViewStateAction, employeeListChangeAction,
                   updateListSettingsMenuAction, updateEmployeeDetailPanel);
  TInterfaceError = (errListNameEmpty, errListNameAlreadyExists,
                  errWrongInputDetailProperty);
  TInterfaceActions = set of TInterfaceAction;
  TInterfaceErrors = set of TInterfaceError;

implementation

end.
