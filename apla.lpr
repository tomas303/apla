program apla;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fCommands, uGroups, uCommands, uMain, iMain, fCommand, fLauncher,
  memdslaz, testik, uCategories, fCategories, fCategory
  { you can add units after this };

{$R *.res}

begin
  TMain.Run(TLauncherForm);
  //TMain.Run(TCommandsForm);
end.

