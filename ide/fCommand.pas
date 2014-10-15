unit fCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, Buttons, Grids, ComCtrls, rtti_broker_iBroker,
  rtti_idebinder_iBindings, rtti_idebinder_Lib, types;

type

  { TCommandForm }

  TCommandForm = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblShowWindow: TLabel;
    ShowWindow_bind: TComboBox;
    lblRunInTerminal: TLabel;
    RunInTerminal_bind: TCheckBox;
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
    fBehaveBinder: IRBBehavioralBinder;
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
  fBehaveBinder := TLib.NewBehavioralBinder;
  fBehaveBinder.Bind(Self);
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

