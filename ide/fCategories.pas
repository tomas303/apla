unit fCategories;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, iMain, fCategory,
  rtti_broker_iBroker,
  rtti_idebinder_Lib,
  rtti_idebinder_iBindings, types;

type

  { TCategoriesForm }

  TCategoriesForm = class(TForm, IMainContextSupport)
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    lbCategories: TListBox;
    pnRunEdit: TPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    fContext: IMainContext;
    fBinder: IRBTallyBinder;
  protected
    procedure Actualize;
  public
    procedure AttachMainContext(const AContext: IMainContext);
    class procedure Edit(const AContext: IMainContext);
  end;

implementation

{$R *.lfm}

{ TCategoriesForm }

procedure TCategoriesForm.btnAddClick(Sender: TObject);
var
  mData: IRBData;
begin
  mData := fContext.SerialFactory.CreateObject('TCategory') as IRBData;
  if TCategoryForm.Edit(mData, fContext.DataQuery) then
  begin
    fContext.DataStore.Save(mData);
    fContext.DataStore.Flush;
    Actualize;
  end;
end;

procedure TCategoriesForm.btnDeleteClick(Sender: TObject);
begin
  // not supported now
end;

procedure TCategoriesForm.btnEditClick(Sender: TObject);
var
  mData, mNewData: IRBData;
begin
  mData := fBinder.CurrentData;
  if mData = nil then
    Exit;
  mNewData := fContext.SerialFactory.CreateObject(mData.ClassName) as IRBData;
  mNewData.Assign(mData);
  if TCategoryForm.Edit(mNewData, fContext.DataQuery) then
  begin
    mData.Assign(mNewData);
    fContext.DataStore.Save(mData);
    fContext.DataStore.Flush;
    Actualize;
  end;
end;

procedure TCategoriesForm.Actualize;
begin
  fBinder.Reload;
end;

procedure TCategoriesForm.AttachMainContext(const AContext: IMainContext);
var
  mClass: TClass;
begin
  fContext := AContext;
  mClass := fContext.SerialFactory.FindClass('TCategory');
  fBinder := TLib.NewListBinder(lbCategories, fContext.DataQuery, mClass);
  Actualize;
end;

class procedure TCategoriesForm.Edit(const AContext: IMainContext);
var
  m: TCategoriesForm;
begin
  Application.CreateForm(Self, m);
  try
     if Supports(m, IMainContextSupport) then
       (m as IMainContextSupport).AttachMainContext(AContext);
    m.ShowModal;
  finally
    m.Release;
  end;
end;

end.

