program apla;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  {Forms, fCommands, uGroups, uCommands, fCommand, fLauncher,
  memdslaz, testik, uCategories, fCategories, fCategory,}
  uapp;

{$R *.res}

begin
  TApp.Go;
end.

