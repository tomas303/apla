unit fCategory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, Buttons, tvr.rtti.broker.iBroker, tvr.rtti.idebinder.iBindings,
  tvr.rtti.idebinder.Lib;

type

  { TCategoryForm }

  TCategoryForm = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Name_bind: TLabeledEdit;
    pnDetail: TPanel;
    Commands_bind: TStringGrid;
    pnBottom: TPanel;
  private
    fBinder: IRBDataBinder;
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

