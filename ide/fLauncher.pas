unit fLauncher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, uCommands, fCommands, fCategories,
  fgl, uCategories,
  trl_irttibroker, trl_ifactory, trl_ipersist,
  tvl_iedit, tvl_ibindings, FPimage, tvl_iiconutils, tvl_igui;

type

  ELauncher = class(Exception)
  end;

  { ILauncher }

  ILauncher = interface
  ['{B0DFA1A4-2D6B-4EFA-98D0-53F69D4DA632}']
    function GetCommand: IRBData;
    function GetImageIndex: integer;
    procedure SetCommand(const AValue: IRBData);
    property Command: IRBData read GetCommand write SetCommand;
    property ImageIndex: integer read GetImageIndex;
  end;

  { TLauncher }

  TLauncher = class(TInterfacedObject, ILauncher)
  protected
    fCommand: IRBData;
    fImageIndex: Integer;
    fIcons: TImageList;
    fIconUtils: IIconUtils;
  protected
    // ILauncher
    function GetCommand: IRBData;
    function GetImageIndex: integer;
    procedure SetCommand(const AValue: IRBData);
    property Command: IRBData read GetCommand write SetCommand;
    property ImageIndex: integer read GetImageIndex;
  public
    class function New(const ACommand: IRBData; AIcons: TImageList;
      const AIconUtils: IIconUtils): ILauncher;
  end;

  { TLaunchList }

  TLaunchList = class(specialize TFPGMap<pointer, ILauncher>)
  end;

  { TRunLaunchList }

  TRunList = class(specialize TFPGMap<string, ILauncher>)
  end;

  { TLauncherForm }

  TLauncherForm = class(TForm, IMainForm)
    ilIcons: TImageList;
    MenuItem4: TMenuItem;
    mnMain: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    pmLaunch: TPopupMenu;
    pmFavorites: TPopupMenu;
    tiLaunch: TTrayIcon;
    procedure tiLaunchMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    fFactory: IPersistFactory;
    fStore: IPersistStore;
    fCommands: IListData;
    fCategories: IListData;
    fGUI: IGUI;
    fIconUtils: IIconUtils;
  protected
    // map command ID -> ILauncher
    fRunList: TRunList;
    // map TMenuItem -> ILauncher
    fLaunchList: TLaunchList;
  protected
    //IMainForm
    procedure StartUp;
    procedure ShutDown;
    procedure ConnectCloseHandler(OnCloseHandler: TCloseEvent);
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
    procedure AddRunAllCategoryCommnad(const ACategory: IRBData; AParentMenu: TMenuItem);
    procedure RebuildMenu(const ARootMenu: TMenuItem);
    procedure RebuildFavoritesMenu(const ARootMenu: TMenuItem);
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
    property GUI: IGUI read fGUI write fGUI;
    property IconUtils: IIconUtils read fIconUtils write fIconUtils;
  end;

implementation

{$R *.lfm}

{ TLauncher }

function TLauncher.GetCommand: IRBData;
begin
  Result := fCommand;
end;

function TLauncher.GetImageIndex: integer;
var
  mBitmap: TBitmap;
begin
  if fImageIndex = -1 then
  begin
    mBitmap := TBitmap.Create;
    try
      fIconUtils.RenderAppIcon(Command.ItemByName['Command'].AsString, mBitmap, fIcons.Height);
      if mBitmap.Transparent then
        fImageIndex := fIcons.AddMasked(mBitmap, mBitmap.TransparentColor)
      else
        fImageIndex := fIcons.Add(mBitmap, nil);
    finally
      mBitmap.Free;
    end;
  end;
  Result := fImageIndex;
end;

procedure TLauncher.SetCommand(const AValue: IRBData);
begin
  fCommand := AValue;
end;

class function TLauncher.New(const ACommand: IRBData; AIcons: TImageList;
  const AIconUtils: IIconUtils): ILauncher;
var
  mLauncher: TLauncher;
begin
  mLauncher := TLauncher.Create;
  mLauncher.Command := ACommand;
  mLauncher.fIcons := AIcons;
  mLauncher.fImageIndex := -1;
  mLauncher.fIconUtils := AIconUtils;
  Result := mLauncher;
end;

{ TLauncherForm }

procedure TLauncherForm.tiLaunchMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (pmFavorites.Items.Count > 0) then
    pmFavorites.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TLauncherForm.StartUp;
begin
  ilIcons.Height := GUI.MenuHeight;
  ilIcons.Width := ilIcons.Height;
  Rebuild;
end;

procedure TLauncherForm.ShutDown;
begin
end;

procedure TLauncherForm.ConnectCloseHandler(OnCloseHandler: TCloseEvent);
begin
  AddHandlerClose(OnCloseHandler);
end;

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
  mLauncher: ILauncher;
begin
  mLauncher := fLaunchList.KeyData[Sender];
  mCommand := mLauncher.Command.UnderObject;
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
  mLauncher: ILauncher;
begin
  mLauncher := fLaunchList.KeyData[Sender];
  mO := mLauncher.Command.UnderObject;
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
  mLauncher: ILauncher;
begin
  for i := 0 to fRunList.Count - 1 do
  begin
    mItem := TMenuItem.Create(AParentMenu.Owner);
    mItem.Caption := fRunList.Data[i].Command.ItemByName['Name'].AsString;
    mItem.OnClick := @OnRunCommandClick;
    mLauncher := fRunList.Data[i];
    mItem.ImageIndex := mLauncher.ImageIndex;
    AParentMenu.Add(mItem);
    fLaunchList.Add(mItem, mLauncher);
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
    mItem := TMenuItem.Create(AParentMenu.Owner);
    mItem.Caption := mCategory.Name;
    if mCategory.RunAll then begin
      AddRunAllCategoryCommnad(mList[i].Data, mItem);
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
      AddRunAllCategoryCommnad(mList[i].Data, AParentMenu);
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
  mLauncher: ILauncher;
begin
  for i := 0 to ACategory.Commands.Count - 1 do
  begin
    mItem := TMenuItem.Create(AParentMenu.Owner);
    if ACategory.Commands[i].Data = nil then
      Continue;
    mCommand := ACategory.Commands[i].Data.UnderObject as TCommand;
    mItem.Caption := mCommand.Name;
    mItem.OnClick := @OnRunCommandClick;
    AParentMenu.Add(mItem);
    mLauncher := fRunList.KeyData[ACategory.Commands[i].Data.ItemByName['ID'].AsString];
    mItem.ImageIndex := mLauncher.ImageIndex;
    fLaunchList.Add(mItem, mLauncher);
  end;
end;

procedure TLauncherForm.AddRunAllCategoryCommnad(const ACategory: IRBData;
  AParentMenu: TMenuItem);
var
  mItem: TMenuItem;
  mCategory: TCategory;
begin
  mCategory := ACategory.UnderObject as TCategory;
  mItem := TMenuItem.Create(AParentMenu.Owner);
  mItem.Caption := 'Run all ' + mCategory.Name;
  mItem.OnClick := @OnRunAllCategoryClick;
  AParentMenu.Add(mItem);
  fLaunchList.Add(mItem, TLauncher.New(ACategory, ilIcons, IconUtils));
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

procedure TLauncherForm.RebuildFavoritesMenu(const ARootMenu: TMenuItem);
begin
  ARootMenu.Clear;
  AddFavoriteCategories(ARootMenu);
  AddManageCategories(ARootMenu);
end;

procedure TLauncherForm.ReloadCommands;
var
  mList: IPersistRefList;
  i: integer;
begin
  mList := (Store as IPersistQuery).SelectClass('TCommand');
  for i := 0 to mList.Count - 1 do
  begin
    fRunList.Add(mList[i].Data.ItemByName['ID'].AsString, TLauncher.New(mList[i].Data, ilIcons, IconUtils));
  end;
end;

procedure TLauncherForm.Rebuild;
begin
  ilIcons.Clear;
  fRunList.Clear;
  fLaunchList.Clear;
  ReloadCommands;
  RebuildMenu(pmLaunch.Items);
  RebuildFavoritesMenu(pmFavorites.Items);
end;

constructor TLauncherForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fLaunchList := TLaunchList.Create;
  fRunList := TRunList.Create;
end;

destructor TLauncherForm.Destroy;
begin
  FreeAndNil(fRunList);
  FreeAndNil(fLaunchList);
  inherited Destroy;
end;

end.

