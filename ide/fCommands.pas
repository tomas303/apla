unit fCommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Grids, Buttons, Menus, iMain, rtti_broker_iBroker,
  uCommands, fCommand, rtti_idebinder_Lib, rtti_idebinder_iBindings,
  types;

type

  { TCommandsForm }

  TCommandsForm = class(TForm, IMainContextSupport)
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
    fContext: IMainContext;
    fBinder: IRBTallyBinder;
    fDirty: Boolean;
    procedure RunCommand(const AData: IRBData);
  protected
    procedure ActualizeRunTab;
  public
    procedure AttachMainContext(const AContext: IMainContext);
    class function Edit(const AContext: IMainContext): Boolean;
  end;

var
  LauncherForm: TCommandsForm;

implementation

{$R *.lfm}

{ TCommandsForm }

procedure TCommandsForm.btnAddClick(Sender: TObject);
var
  mData: IRBData;
begin
  mData := fContext.SerialFactory.CreateObject('TCommand') as IRBData;
  if TCommandForm.Edit(mData, fContext.DataQuery) then
  begin
    fContext.DataStore.Save(mData);
    fContext.DataStore.Flush;
    fDirty := True;
    ActualizeRunTab;
  end;
end;

procedure TCommandsForm.btnDeleteClick(Sender: TObject);
begin
  // mazat zatim serial vrstva neumi
end;

procedure TCommandsForm.btnEditClick(Sender: TObject);
var
  mData, mNewData: IRBData;
begin
  mData := fBinder.CurrentData;
  if mData = nil then
    Exit;
  mNewData := fContext.SerialFactory.CreateObject('TCommand') as IRBData;
  mNewData.Assign(mData);
  if TCommandForm.Edit(mNewData, fContext.DataQuery) then
  begin
    mData.Assign(mNewData);
    fContext.DataStore.Save(mData);
    fContext.DataStore.Flush;
    fDirty := True;
    ActualizeRunTab;
  end;
end;

procedure TCommandsForm.grdCommandsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    RunCommand(fBinder.CurrentData);
end;

procedure TCommandsForm.RunCommand(const AData: IRBData);
begin
  if AData = nil then
    Exit;
  (AData.UnderObject as TCommand).Run;
end;

procedure TCommandsForm.ActualizeRunTab;
var
  mClass: TClass;
begin
  mClass := fContext.SerialFactory.FindClass('TCommand');
  fBinder := TLib.NewListBinder(grdCommands, fContext.DataQuery, mClass);
end;

procedure TCommandsForm.AttachMainContext(const AContext: IMainContext);
begin
  fContext := AContext;
  ActualizeRunTab;
end;

class function TCommandsForm.Edit(const AContext: IMainContext): Boolean;
var
  m: TCommandsForm;
begin
  Application.CreateForm(Self, m);
  try
     if Supports(m, IMainContextSupport) then
       (m as IMainContextSupport).AttachMainContext(AContext);
    m.ShowModal;
    Result := m.fDirty;
  finally
    m.Release;
  end;
end;

end.

