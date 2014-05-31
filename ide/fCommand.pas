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
    lblName: TLabel;
    Name_bind: TEdit;
    EnvVariableTemplate_bind: TComboBox;
    Command_bind: TMemo;
    Directory_bind: TDirectoryEdit;
    EnvVariables_bind: TStringGrid;
    lblEnvVariableTemplate: TLabel;
    pnBottom: TPanel;
    Parameters_bind: TStringGrid;
    lblCommand: TLabel;
    lblDirectory: TLabel;
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

