unit fCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, Buttons, Grids, ComCtrls, types,
  trl_irttibroker, trl_ifactory, trl_ipersist, trl_ipersiststore,
  tvl_iedit, tvl_ibindings;

type

  { TCommandForm }

  TCommandForm = class(TForm, IEditData)
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
  protected
    function Edit(const AData: IRBdata): Boolean;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
  end;

var
  CommandForm: TCommandForm;

implementation

{$R *.lfm}

{ TCommandForm }

function TCommandForm.Edit(const AData: IRBdata): Boolean;
begin
  //fBehaveBinder.Bind(Self);
  try
    Binder.Bind(Self, AData);
    Result := ShowModal = mrOK;
  finally
    Binder.Unbind;
  end;
end;

end.

