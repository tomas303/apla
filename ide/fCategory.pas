unit fCategory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, Buttons, StdCtrls,
  trl_irttibroker, trl_ifactory, trl_ipersist, trl_ipersiststore,
  tvl_iedit, tvl_ibindings;

type

  { TCategoryForm }

  TCategoryForm = class(TForm, IEditData)
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
  protected
    function Edit(const AData: IRBdata): Boolean;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
  end;

implementation

{$R *.lfm}

{ TCategoryForm }

function TCategoryForm.Edit(const AData: IRBdata): Boolean;
begin
  BehaveBinder.Bind(Self);
  try
    Binder.Bind(Self, AData);
    try
      Result := ShowModal = mrOK;
    finally
      Binder.Unbind;
    end;
  finally
    BehaveBinder.Unbind;
  end;
end;

end.

