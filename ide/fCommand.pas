unit fCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, Buttons, Grids, ComCtrls, tvr.rtti.broker.iBroker,
  tvr.rtti.idebinder.iBindings, tvr.rtti.idebinder.Lib, types;

type

  { TCommandForm }

  TCommandForm = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Command_bind: TMemo;
    Directory_bind: TDirectoryEdit;
    EnvVariables_bind: TStringGrid;
    pnBottom: TPanel;
    Parameters_bind: TStringGrid;
    lblCommand: TLabel;
    lblDirectory: TLabel;
    Name_bind: TLabeledEdit;
    pgCommand: TPageControl;
    tsEnvVariables: TTabSheet;
    tsParameters: TTabSheet;
    tsCommand: TTabSheet;
  private
    fBinder: IRBDataBinder;
  public
    procedure Init(const AData: IRBdata; const ADataQuery: IRBDataQuery);
    class function Edit(const AData: IRBdata; const ADataQuery: IRBDataQuery): Boolean;
  end;

var
  CommandForm: TCommandForm;

implementation

{$R *.lfm}

{ TCommandForm }

procedure TCommandForm.Init(const AData: IRBdata; const ADataQuery: IRBDataQuery);
begin
  fBinder := TLib.NewDataBinder;
  fBinder.Bind(Self, AData, ADataQuery);
end;

class function TCommandForm.Edit(const AData: IRBdata; const ADataQuery: IRBDataQuery): Boolean;
var
  m: TCommandForm;
begin
  m := TCommandForm.Create(nil);
  try
    m.Init(AData, ADataQuery);
    Result := m.ShowModal = mrOK;
  finally
    m.Free;
  end;
end;

end.

