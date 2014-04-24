unit fLauncher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, iMain, tvr.rtti.broker.iBroker, uCommands, fCommands, fCategories;

type

  { TLauncherForm }

  TLauncherForm = class(TForm, IMainContextSupport)
    mnMain: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    pmLaunch: TPopupMenu;
    tiLaunch: TTrayIcon;
  private
    fContext: IMainContext;
    fRunList: IRBDataList;
  protected
    function CreateMenuItem(AParentMenu: TMenuItem; const ACaption: string;
      AOnClick: TNotifyEvent; ATag: integer = 0): TMenuItem;
    procedure OnRunCommandClick(Sender: TObject);
    procedure OnCloseApplicationClick(Sender: TObject);
    procedure OnManageCommandsClick(Sender: TObject);
    procedure OnManageCategoriesClick(Sender: TObject);
    procedure AddCloseApplication(AParentMenu: TMenuItem);
    procedure AddManageCommands(AParentMenu: TMenuItem);
    procedure AddManageCategories(AParentMenu: TMenuItem);
    procedure AddSeparator(AParentMenu: TMenuItem);
    procedure AddAllCommnads(AParentMenu: TMenuItem);
    procedure RebuildMenu(const ARootMenu: TMenuItem);
    procedure ReloadCommands;
    procedure Rebuild;
  protected
    procedure AttachMainContext(const AContext: IMainContext);
  public
  end;

var
  LauncherForm: TLauncherForm;

implementation

{$R *.lfm}

{ TLauncherForm }

function TLauncherForm.CreateMenuItem(AParentMenu: TMenuItem;
  const ACaption: string; AOnClick: TNotifyEvent; ATag: integer): TMenuItem;
begin
  Result := TMenuItem.Create(AParentMenu.Owner);
  try
    Result.Caption := ACaption;
    Result.Tag := ATag;
    Result.OnClick := AOnClick;
    AParentMenu.Add(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

procedure TLauncherForm.OnRunCommandClick(Sender: TObject);
var
  mData: IRBData;
begin
  mData := fRunList.AsData[(Sender as TMenuItem).Tag];
  (mData.UnderObject as TCommand).Run;
end;

procedure TLauncherForm.OnCloseApplicationClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TLauncherForm.OnManageCommandsClick(Sender: TObject);
begin
  TCommandsForm.Edit(fContext);
end;

procedure TLauncherForm.OnManageCategoriesClick(Sender: TObject);
begin
  TCategoriesForm.Edit(fContext);
end;

procedure TLauncherForm.AddCloseApplication(AParentMenu: TMenuItem);
begin
  CreateMenuItem(AParentMenu, 'Exit', @OnCloseApplicationClick);
end;

procedure TLauncherForm.AddManageCommands(AParentMenu: TMenuItem);
begin
  CreateMenuItem(AParentMenu, 'Commands', @OnManageCommandsClick);
end;

procedure TLauncherForm.AddManageCategories(AParentMenu: TMenuItem);
begin
  CreateMenuItem(AParentMenu, 'Categories', @OnManageCategoriesClick);
end;

procedure TLauncherForm.AddSeparator(AParentMenu: TMenuItem);
begin
  CreateMenuItem(AParentMenu, '-', nil);
end;

procedure TLauncherForm.AddAllCommnads(AParentMenu: TMenuItem);
var
  i: integer;
  mItem: TMenuItem;
begin
  for i := 0 to fRunList.Count - 1 do
  begin
    mItem := TMenuItem.Create(mnMain);
    mItem.Caption := fRunList.AsData[i].ItemByName['Name'].AsString;
    mItem.Tag := i;
    mItem.OnClick := @OnRunCommandClick;
    AParentMenu.Add(mItem);
  end;
end;

procedure TLauncherForm.RebuildMenu(const ARootMenu: TMenuItem);
var
  mItem: TMenuItem;
begin
  ARootMenu.Clear;
  mItem := TMenuItem.Create(ARootMenu.Owner);
  mItem.Caption := 'All commands';
  ARootMenu.Add(mItem);
  AddAllCommnads(mItem);
  AddSeparator(ARootMenu);
  AddManageCommands(ARootMenu);
  AddManageCategories(ARootMenu);
  AddCloseApplication(ARootMenu);
end;

procedure TLauncherForm.ReloadCommands;
begin
  fRunList := fContext.DataStore.LoadList('TCommand');
end;

procedure TLauncherForm.Rebuild;
begin
  ReloadCommands;
  RebuildMenu(pmLaunch.Items);
end;

procedure TLauncherForm.AttachMainContext(const AContext: IMainContext);
begin
  fContext := AContext;
  Rebuild;
end;

end.

