unit fLauncher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, iMain, rtti_broker_iBroker, uCommands, fCommands, fCategories,
  fgl, uCategories;

type

  ELauncher = class(Exception)
  end;

  { TLauncherForm }

  TCustomLaunchList = specialize TFPGMap<pointer, TObject>;

  { TLaunchList }

  TLaunchList = class(TCustomLaunchList)
  end;

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
    fLaunchList: TLaunchList;
  protected
    function CreateMenuItem(AParentMenu: TMenuItem; const ACaption: string;
      AOnClick: TNotifyEvent; ATag: integer = 0): TMenuItem;
    procedure OnRunCommandClick(Sender: TObject);
    procedure OnRunAllCategoryClick(Sender: TObject);
    procedure OnCloseApplicationClick(Sender: TObject);
    procedure OnManageCommandsClick(Sender: TObject);
    procedure OnManageCategoriesClick(Sender: TObject);
    procedure AddCloseApplication(AParentMenu: TMenuItem);
    procedure AddManageCommands(AParentMenu: TMenuItem);
    procedure AddManageCategories(AParentMenu: TMenuItem);
    procedure AddSeparator(AParentMenu: TMenuItem);
    procedure AddAllCommnads(AParentMenu: TMenuItem);
    procedure AddNonFavoriteCategories(AParentMenu: TMenuItem);
    procedure AddFavoriteCategories(AParentMenu: TMenuItem);
    procedure AddCategoryCommnads(const ACategory: TCategory; AParentMenu: TMenuItem);
    procedure AddRunAllCategoryCommnad(const ACategory: TCategory; AParentMenu: TMenuItem);
    procedure RebuildMenu(const ARootMenu: TMenuItem);
    procedure ReloadCommands;
    procedure Rebuild;
  protected
    procedure AttachMainContext(const AContext: IMainContext);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
  mCommand: TObject;
begin
  mCommand := fLaunchList.KeyData[Sender];
  if mCommand = nil then
    Exit;
  if not (mCommand is TCommand) then
    raise ELauncher.Create('Object is not TCommand');
  (mCommand as TCommand).Run;
end;

procedure TLauncherForm.OnRunAllCategoryClick(Sender: TObject);
var
  mO: TObject;
  mCategory: TCategory;
  i: integer;
begin
  mO := fLaunchList.KeyData[Sender];
  if mO = nil then
    Exit;
  if not (mO is TCategory) then
    raise ELauncher.Create('Object is not TCategory');
  mCategory := mO as TCategory;
  for i := 0 to mCategory.CommandsCount - 1 do
  begin
    try
      mCategory.Commands[i].Run;
    except
    end;
  end;
end;

procedure TLauncherForm.OnCloseApplicationClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TLauncherForm.OnManageCommandsClick(Sender: TObject);
begin
  if TCommandsForm.Edit(fContext) then
    Rebuild;
end;

procedure TLauncherForm.OnManageCategoriesClick(Sender: TObject);
begin
  if TCategoriesForm.Edit(fContext) then
    Rebuild;
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
    mItem.OnClick := @OnRunCommandClick;
    AParentMenu.Add(mItem);
    fLaunchList.Add(mItem, fRunList[i] as TCommand);
  end;
end;

procedure TLauncherForm.AddNonFavoriteCategories(AParentMenu: TMenuItem);
var
  i: integer;
  mItem: TMenuItem;
  mList: IRBDataList;
  mCategory: TCategory;
begin
  mList := fContext.DataStore.LoadList('TCategory');
  for i := 0 to mList.Count - 1 do
  begin
    mCategory := mList[i] as TCategory;
    if mCategory.Favorite then
      Continue;
    if mCategory.CommandsCount = 0 then
      Continue;
    mItem := TMenuItem.Create(mnMain);
    mItem.Caption := mCategory.Name;
    AddRunAllCategoryCommnad(mCategory, mItem);
    AddCategoryCommnads(mCategory, mItem);
    AParentMenu.Add(mItem);
  end;
end;

procedure TLauncherForm.AddFavoriteCategories(AParentMenu: TMenuItem);
var
  i: integer;
  mList: IRBDataList;
  mCategory: TCategory;
begin
  mList := fContext.DataStore.LoadList('TCategory');
  for i := 0 to mList.Count - 1 do
  begin
    mCategory := mList[i] as TCategory;
    if not mCategory.Favorite then
      Continue;
    if mCategory.CommandsCount = 0 then
      Continue;
    AddCategoryCommnads(mCategory, AParentMenu);
  end;
  if AParentMenu.Count > 0 then
    AddSeparator(AParentMenu);
end;

procedure TLauncherForm.AddCategoryCommnads(const ACategory: TCategory;
  AParentMenu: TMenuItem);
var
  i: integer;
  mItem: TMenuItem;
  mCommand: TCommand;
begin
  for i := 0 to ACategory.CommandsCount - 1 do
  begin
    mItem := TMenuItem.Create(mnMain);
    mCommand := ACategory.Commands[i];
    mItem.Caption := mCommand.Name;
    mItem.OnClick := @OnRunCommandClick;
    AParentMenu.Add(mItem);
    fLaunchList.Add(mItem, mCommand);
  end;
end;

procedure TLauncherForm.AddRunAllCategoryCommnad(const ACategory: TCategory;
  AParentMenu: TMenuItem);
var
  mItem: TMenuItem;
begin
  mItem := TMenuItem.Create(mnMain);
  mItem.Caption := 'Run all ' + ACategory.Name;
  mItem.OnClick := @OnRunAllCategoryClick;
  AParentMenu.Add(mItem);
  AddSeparator(AParentMenu);
  fLaunchList.Add(mItem, ACategory);
end;

procedure TLauncherForm.RebuildMenu(const ARootMenu: TMenuItem);
var
  mItem: TMenuItem;
begin
  fLaunchList.Clear;
  ARootMenu.Clear;
  AddFavoriteCategories(ARootMenu);
  AddNonFavoriteCategories(ARootMenu);
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

constructor TLauncherForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fLaunchList := TLaunchList.Create;
end;

destructor TLauncherForm.Destroy;
begin
  FreeAndNil(fLaunchList);
  inherited Destroy;
end;

end.

