unit fLauncher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, uCommands, fCommands, fCategories,
  fgl, uCategories,
  trl_irttibroker, trl_ifactory, trl_ipersist, trl_ipersiststore,
  tvl_iedit, tvl_ibindings;

type

  ELauncher = class(Exception)
  end;

  { TLauncherForm }

  TCustomLaunchList = specialize TFPGMap<pointer, TObject>;

  { TLaunchList }

  TLaunchList = class(TCustomLaunchList)
  end;

  TLauncherForm = class(TForm)
    MenuItem4: TMenuItem;
    mnMain: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    pmLaunch: TPopupMenu;
    tiLaunch: TTrayIcon;
  private
    fCategories: IListData;
    fCommands: IListData;
    fFactory: IPersistFactory;
    fStore: IPersistStore;
    fRunList: IPersistRefList;
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
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rebuild;
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property Store: IPersistStore read fStore write fStore;
    property Commands: IListData read fCommands write fCommands;
    property Categories: IListData read fCategories write fCategories;
  end;

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
  for i := 0 to mCategory.Commands.Count - 1 do
  begin
    try
      (mCategory.Commands[i].Data.UnderObject as TCommand).Run;
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
  Commands.List;
  Rebuild;
end;

procedure TLauncherForm.OnManageCategoriesClick(Sender: TObject);
begin
  Categories.List;
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
    mItem.Caption := fRunList[i].Data.ItemByName['Name'].AsString;
    mItem.OnClick := @OnRunCommandClick;
    AParentMenu.Add(mItem);
    fLaunchList.Add(mItem, fRunList[i].Data.UnderObject as TCommand);
  end;
end;

procedure TLauncherForm.AddNonFavoriteCategories(AParentMenu: TMenuItem);
var
  i: integer;
  mItem: TMenuItem;
  mList: IPersistRefList;
  mCategory: TCategory;
begin
  mList := (Store as IPersistQuery).SelectClass('TCategory');
  for i := 0 to mList.Count - 1 do
  begin
    mCategory := mList[i].Data.UnderObject as TCategory;
    if mCategory.Commands.Count = 0 then
      Continue;
    mItem := TMenuItem.Create(mnMain);
    mItem.Caption := mCategory.Name;
    if mCategory.RunAll then begin
      AddRunAllCategoryCommnad(mCategory, mItem);
      AddSeparator(mItem);
    end;
    AddCategoryCommnads(mCategory, mItem);
    AParentMenu.Add(mItem);
  end;
end;

procedure TLauncherForm.AddFavoriteCategories(AParentMenu: TMenuItem);
var
  i: integer;
  mList: IPersistRefList;
  mCategory: TCategory;
begin
  mList := (Store as IPersistQuery).SelectClass('TCategory');
  for i := 0 to mList.Count - 1 do
  begin
    mCategory := mList[i].Data.UnderObject as TCategory;
    if mCategory.Commands.Count = 0 then
      Continue;
    if mCategory.RunAllFavorite then
      AddRunAllCategoryCommnad(mCategory, AParentMenu);
    if mCategory.Favorite then
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
  for i := 0 to ACategory.Commands.Count - 1 do
  begin
    mItem := TMenuItem.Create(mnMain);
    if ACategory.Commands[i].Data = nil then
      Continue;
    mCommand := ACategory.Commands[i].Data.UnderObject as TCommand;
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
  fRunList := (Store as IPersistQuery).SelectClass('TCommand');
end;

procedure TLauncherForm.Rebuild;
begin
  ReloadCommands;
  RebuildMenu(pmLaunch.Items);
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

