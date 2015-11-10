unit uCommands;

{$mode delphi}{$H+}

interface

uses
  sysutils, fgl, process,
  trl_irttibroker, trl_ifactory, trl_ipersist, trl_ipersiststore,
  trl_upersist, trl_upersiststore;

type

  { TEnvVariable }
  TEnvVariableAppend = (Replace, Before, After);

  TEnvVariable = class
  private
    fName: string;
    fValue: string;
    fAppend: TEnvVariableAppend ;
  published
    property Name: string read fName write fName;
    property Value: string read fValue write fValue;
    property Append: TEnvVariableAppend read fAppend write fAppend;
  end;

  { IPersistManyTEnvVariable }

  IPersistManyTEnvVariable = interface(IPersistMany<TEnvVariable>)
  ['{E3912562-64E4-48FF-A7A4-7D8B284E167C}']
  end;

  { TPersistManyTEnvVariable }

  TPersistManyTEnvVariable = class(TPersistManyObjects<TEnvVariable>, IPersistManyTEnvVariable)
  end;

  { TParameter }

  TParameter = class
  private
    fValue: string;
  published
    property Value: string read fValue write fValue;
  end;

  { IPersistManyTParameter }

  IPersistManyTParameter = interface(IPersistMany<TParameter>)
  ['{FC59552C-425C-46CD-9E4D-3FBFA6395DAC}']
  end;

  { TPersistManyTParameter }

  TPersistManyTParameter = class(TPersistManyObjects<TParameter>, IPersistManyTParameter)
  end;

  { TEnvVariableTemplate }

  TEnvVariableTemplate = class
  private type
    TEnvVariables = TFPGObjectList<TEnvVariable>;
  private
    fName: string;
    fEnvVariables: IPersistManyTEnvVariable;
  public
    procedure AfterConstruction; override;
  published
    property Name: string read fName write fName;
    property EnvVariables: IPersistManyTEnvVariable read fEnvVariables;
  end;


  TCommandShowWindow = (None,Maximize,Minimize);

  { TCommand }

  TCommand = class
  private type
    TEnvVariables = TFPGObjectList<TEnvVariable>;
    TParameters = TFPGObjectList<TParameter>;
  private
    fCommand: TMemoString;
    fName: string;
    fDirectory: string;
    fEnvVariables: IPersistManyTEnvVariable;
    fParameters: IPersistManyTParameter;
    fEnvVariableTemplate: IPersistRef<TEnvVariableTemplate>;
    fRunInTerminal: Boolean;
    fShowWindow: TCommandShowWindow;
  private
    procedure ProcessFillEnvVariables(AProcess: TProcess; ACopyActual: Boolean);
    procedure ProcessFillParameters(AProcess: TProcess);
  public
    procedure AfterConstruction; override;
    procedure Run;
  published
    property Command: TMemoString read fCommand write fCommand;
    property Name: string read fName write fName;
    property Directory: string read fDirectory write fDirectory;
    property EnvVariables: IPersistManyTEnvVariable read fEnvVariables;
    property Parameters: IPersistManyTParameter read fParameters;
    property EnvVariableTemplate: IPersistRef<TEnvVariableTemplate> read fEnvVariableTemplate write fEnvVariableTemplate;
    property RunInTerminal: Boolean read fRunInTerminal write fRunInTerminal;
    property ShowWindow: TCommandShowWindow read fShowWindow write fShowWindow;
  end;

implementation

{ TEnvVariableTemplate }

procedure TEnvVariableTemplate.AfterConstruction;
begin
  inherited AfterConstruction;
  fEnvVariables := TPersistManyTEnvVariable.Create;
end;

{ TCommand }

procedure TCommand.ProcessFillEnvVariables(AProcess: TProcess; ACopyActual: Boolean);
var
  i: integer;
  m1,m2: string;

  procedure iAdd(const AEV: TEnvVariable);
  var
    mInd: integer;
  begin
    // on windows exists in environment both variants, but when use it, system takes first one,
    // so in case of variable exists will be removed fires
    mInd := AProcess.Environment.IndexOfName(AEV.Name);
    if mInd = -1 then
      AProcess.Environment.Add(AEV.Name + '=' + AEV.Value)
    else
      case AEV.Append of
        TEnvVariableAppend.Replace:
          AProcess.Environment[mInd] := AEV.Name + '=' + AEV.Value;
        TEnvVariableAppend.Before:
          AProcess.Environment[mInd] := AEV.Name + '=' + AEV.Value + AProcess.Environment.ValueFromIndex[mInd];
        TEnvVariableAppend.After:
          AProcess.Environment[mInd] := AEV.Name + '=' + AProcess.Environment.ValueFromIndex[mInd] + AEV.Value;
      end;
  end;

begin
  if ACopyActual then
  begin
    for i := 1 to GetEnvironmentVariableCount do
    begin
      AProcess.Environment.Add(GetEnvironmentString(i));
    end;
  end;
  if (EnvVariableTemplate <> nil) and (EnvVariableTemplate.Data <> nil) then
  begin
    for i := 0 to EnvVariableTemplate.Item.EnvVariables.Count - 1 do
    begin
      //AProcess.Environment.Add(EnvVariableTemplate[i].Name + '=' + EnvVariableTemplate[i].Value);
      iAdd(EnvVariableTemplate.Item.EnvVariables[i]);
    end;
  end;
  for i := 0 to EnvVariables.Count - 1 do
  begin
    //AProcess.Environment.Add(EnvVariables[i].Name + '=' + EnvVariables[i].Value);
    m1 := EnvVariables[i].Name;
    m2 := EnvVariables[i].Value;
    iAdd(EnvVariables[i]);
  end;
end;

procedure TCommand.ProcessFillParameters(AProcess: TProcess);
var
  i: integer;
begin
  for i := 0 to Parameters.Count - 1 do
  begin
    AProcess.Parameters.Add(Parameters[i].Value);
  end;
end;

procedure TCommand.AfterConstruction;
begin
  fEnvVariables := TPersistManyTEnvVariable.Create;
  fParameters := TPersistManyTParameter.Create;
  fEnvVariableTemplate := TPersistRef<TEnvVariableTemplate>.Create;
end;

procedure TCommand.Run;
var
  mProcess: TProcess;
begin
  mProcess := TProcess.Create(nil);
  try
    ProcessFillEnvVariables(mProcess, True);
    ProcessFillParameters(mProcess);
    mProcess.ApplicationName := '';
    mProcess.Executable := Command;
    mProcess.CurrentDirectory := Directory;
    case ShowWindow of
      TCommandShowWindow.None:
        mProcess.ShowWindow := swoNone;
      TCommandShowWindow.Maximize:
        mProcess.ShowWindow := swoMaximize;
      TCommandShowWindow.Minimize:
      mProcess.ShowWindow := swoMinimize;
    end;
    if RunInTerminal then
      mProcess.Options := mProcess.Options + [poNewConsole];
    mProcess.Execute;
  finally
    mProcess.Free;
  end;
end;

end.

