unit fCategory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, Buttons, StdCtrls, rtti_broker_iBroker,
  rtti_idebinder_iBindings, rtti_idebinder_Lib;

type

  { TCategoryForm }

  TCategoryForm = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Favorite_bind: TCheckBox;
    RunAll_bind: TCheckBox;
    RunAllFavorite_bind: TCheckBox;
    Name_bind: TLabeledEdit;
    pnDetail: TPanel;
    Commands_bind: TStringGrid;
    pnBottom: TPanel;
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
  public
    procedure Init(const AData: IRBdata; const ADataQuery: IRBDataQuery);
    class function Edit(const AData: IRBdata; const ADataQuery: IRBDataQuery): Boolean;
  end;

implementation

{$R *.lfm}

{ TCategoryForm }

procedure TCategoryForm.Init(const AData: IRBdata;
  const ADataQuery: IRBDataQuery);
begin
  fBehaveBinder := TLib.NewBehavioralBinder;
  fBehaveBinder.Bind(Self);
  fBinder := TLib.NewDataBinder;
  fBinder.Bind(Self, AData, ADataQuery);
end;

class function TCategoryForm.Edit(const AData: IRBdata;
  const ADataQuery: IRBDataQuery): Boolean;
var
  m: TCategoryForm;
begin
  m := TCategoryForm.Create(nil);
  try
    m.Init(AData, ADataQuery);
    Result := m.ShowModal = mrOK;
  finally
    m.Free;
  end;
end;

end.

