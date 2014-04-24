unit fCategories;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, iMain, fCategory,
  tvr.rtti.broker.iBroker,
  tvr.rtti.idebinder.Lib,
  tvr.rtti.idebinder.iBindings, types;

type

  { TCategoriesForm }

  TCategoriesForm = class(TForm, IMainContextSupport)
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    lbCategories: TListBox;
    pnRunEdit: TPanel;
    procedure btnAddClick(Sender: TObject);
  private
    fContext: IMainContext;
    //fList: IRBDataList;
    fListBinder: IRBTallyBinder;
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

procedure TCategoriesForm.Actualize;
begin
  //fList := fContext.DataStore.LoadList('TCategory');
  fListBinder.Reload;
end;

procedure TCategoriesForm.AttachMainContext(const AContext: IMainContext);
var
  mClass: TClass;
begin
  fContext := AContext;
  //fList := fContext.DataStore.LoadList('TCategory');
  mClass := fContext.SerialFactory.FindClass('TCategory');
  fListBinder := TLib.NewListBinder(lbCategories, fContext.DataQuery, mClass);
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

