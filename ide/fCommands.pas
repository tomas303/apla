unit fCommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Grids, Buttons, Menus,
  uCommands, fCommand, types,
  trl_irttibroker, trl_ifactory, trl_ipersist, trl_ipersiststore,
  tvl_iedit, tvl_ibindings;

type

  { TCommandsForm }

  TCommandsForm = class(TForm, IListData)
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    edRun: TEdit;
    grdCommands: TDrawGrid;
    pnRunEdit: TPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure grdCommandsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fFactory: IPersistFactory;
    fStore: IPersistStore;
    fBinder: IRBTallyBinder;
    fEdit: IEditData;
    procedure RunCommand(const AData: IRBData);
  protected
    procedure Actualize;
  public
    procedure List;
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property Store: IPersistStore read fStore write fStore;
    property Binder: IRBTallyBinder read fBinder write fBinder;
    property Edit: IEditData read fEdit write fEdit;
  end;

implementation

{$R *.lfm}

{ TCommandsForm }

procedure TCommandsForm.btnAddClick(Sender: TObject);
var
  mData: IRBData;
begin
  mData := Factory.CreateObject('TCommand');
  if Edit.Edit(mData) then
  begin
    Store.Save(mData);
    Store.Flush;
    Actualize;
  end;
end;

procedure TCommandsForm.btnDeleteClick(Sender: TObject);
begin
  Store.Delete(Binder.CurrentData);
  Store.Flush;
  Actualize;
end;

procedure TCommandsForm.btnEditClick(Sender: TObject);
var
  mData, mNewData: IRBData;
begin
  mData := Binder.CurrentData;
  if mData = nil then
    Exit;
  mNewData := Factory.CreateObject('TCommand');
  mNewData.Assign(mData);
  if Edit.Edit(mNewData) then
  begin
    mData.Assign(mNewData);
    Store.Save(mData);
    Store.Flush;
    Actualize;
  end;
end;

procedure TCommandsForm.grdCommandsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    RunCommand(Binder.CurrentData);
end;

procedure TCommandsForm.RunCommand(const AData: IRBData);
begin
  if AData = nil then
    Exit;
  (AData.UnderObject as TCommand).Run;
end;

procedure TCommandsForm.Actualize;
begin
  Binder.Reload;
end;

procedure TCommandsForm.List;
begin
  Binder.Bind(grdCommands, 'TCommand');
  try
    Actualize;
    ShowModal;
  finally
    Binder.Unbind;
  end;
end;

end.

