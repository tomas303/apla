unit uCommands;

{$mode delphi}{$H+}

interface

uses
  sysutils, tvr.rtti.broker.iBroker, tvr.rtti.serializer.uSerialObject, fgl, process;

type

  { TEnvVariable }
  TEnvVariableAppend = (Replace, Before, After);

  TEnvVariable = class(TRBCustomObject)
  private
    fName: string;
    fValue: string;
    fAppend: TEnvVariableAppend ;
  published
    property Name: string read fName write fName;
    property Value: string read fValue write fValue;
    property Append: TEnvVariableAppend read fAppend write fAppend;
  end;

  { TParameter }

  TParameter = class(TRBCustomObject)
  private
    fValue: string;
  published
    property Value: string read fValue write fValue;
  end;

  { TEnvVariableTemplate }

  TEnvVariableTemplate = class(TRBCustomObject)
  private type
    TEnvVariables = TFPGObjectList<TEnvVariable>;
  private
    fName: string;
    fEnvVariables: TEnvVariables;
    function GetEnvVariables(AIndex: Integer; APropIndex: integer): TEnvVariable;
    function GetEnvVariablesCount(AIndex: Integer): integer;
    function GetName: string;
    procedure SetEnvVariables(AIndex: Integer; APropIndex: integer;
      AValue: TEnvVariable);
    procedure SetEnvVariablesCount(AIndex: Integer; AValue: integer);
    procedure SetName(AValue: string);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Name: string read GetName write SetName;
    property EnvVariables[AIndex: integer]: TEnvVariable index crbList + crbObject read GetEnvVariables write SetEnvVariables; default;
    property EnvVariablesCount: integer index crbListCounter read GetEnvVariablesCount write SetEnvVariablesCount;
  end;

  { TCommand }

  TCommand = class(TRBCustomObject)
  private type
    TEnvVariables = TFPGObjectList<TEnvVariable>;
    TParameters = TFPGObjectList<TParameter>;
  private
    fCommand: string;
    fName: string;
    fDirectory: string;
    fEnvVariables: TEnvVariables;
    fParameters: TParameters;
    fEnvVariableTemplate: TEnvVariableTemplate;
    fRunInTerminal: Boolean;
    function GetCommand: string;
    function GetEnvVariables(AIndex: Integer; APropIndex: integer): TEnvVariable;
    function GetEnvVariablesCount(APropIndex: integer): integer;
    function GetName: string;
    function GetDirectory: string;
    function GetParameters(AIndex: Integer; APropIndex: integer): TParameter;
    function GetParametersCount(APropIndex: integer): integer;
    function GetEnvVariableTemplate(AIndex: Integer): TEnvVariableTemplate;
    procedure SetCommand(const AValue: string);
    procedure SetEnvVariablesCount(APropIndex: integer; AValue: integer);
    procedure SetName(AValue: string);
    procedure SetDirectory(AValue: string);
    procedure SetParametersCount(APropIndex: integer; AValue: integer);
    procedure SetEnvVariableTemplate(AIndex: Integer;
      AValue: TEnvVariableTemplate);
  private
    procedure ProcessFillEnvVariables(AProcess: TProcess; ACopyActual: Boolean);
    procedure ProcessFillParameters(AProcess: TProcess);
    procedure SetEnvVariables(AIndex: Integer; APropIndex: integer;
      AValue: TEnvVariable);
    procedure SetParameters(AIndex: Integer; APropIndex: integer; AValue: TParameter
      );
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Run;
  published
    property Command: string read GetCommand write SetCommand;
    property Name: string read GetName write SetName;
    property Directory: string read GetDirectory write SetDirectory;
    property EnvVariables[AIndex: integer]: TEnvVariable index crbList + crbObject read GetEnvVariables write SetEnvVariables;
    property EnvVariablesCount: integer index crbListCounter read GetEnvVariablesCount write SetEnvVariablesCount;
    property Parameters[AIndex: integer]: TParameter index crbList + crbObject read GetParameters write SetParameters;
    property ParametersCount: integer index crbListCounter read GetParametersCount write SetParametersCount;
    property EnvVariableTemplate: TEnvVariableTemplate index crbObject + crbRef read GetEnvVariableTemplate write SetEnvVariableTemplate;
    property RunInTerminal: Boolean read fRunInTerminal write fRunInTerminal;
  end;

implementation

{ TEnvVariableTemplate }

function TEnvVariableTemplate.GetEnvVariables(AIndex: Integer; APropIndex: integer
  ): TEnvVariable;
begin
  if fEnvVariables[AIndex] = nil then
    fEnvVariables[AIndex] := TEnvVariable.Create;
  Result := fEnvVariables[AIndex];
end;

function TEnvVariableTemplate.GetEnvVariablesCount(AIndex: Integer): integer;
begin
  Result := fEnvVariables.Count;
end;

function TEnvVariableTemplate.GetName: string;
begin
  Result := fName;
end;

procedure TEnvVariableTemplate.SetEnvVariables(AIndex: Integer;
  APropIndex: integer; AValue: TEnvVariable);
begin
  if fEnvVariables[AIndex] <> nil then
    fEnvVariables[AIndex].Free;
  fEnvVariables[AIndex] := AValue;
end;

procedure TEnvVariableTemplate.SetEnvVariablesCount(AIndex: Integer;
  AValue: integer);
begin
  fEnvVariables.Count := AValue;
end;

procedure TEnvVariableTemplate.SetName(AValue: string);
begin
  fName := AValue;
end;

procedure TEnvVariableTemplate.AfterConstruction;
begin
  inherited AfterConstruction;
  fEnvVariables := TEnvVariables.Create;
end;

procedure TEnvVariableTemplate.BeforeDestruction;
begin
  FreeAndNil(fEnvVariables);
  inherited BeforeDestruction;
end;

{ TCommand }

function TCommand.GetEnvVariables(AIndex: Integer; APropIndex: integer
  ): TEnvVariable;
begin
  if fEnvVariables[AIndex] = nil then
    fEnvVariables[AIndex] := TEnvVariable.Create;
  Result := fEnvVariables[AIndex];
end;

function TCommand.GetCommand: string;
begin
  Result := fCommand;
end;

function TCommand.GetEnvVariablesCount(APropIndex: integer): integer;
begin
  Result := fEnvVariables.Count;
end;

function TCommand.GetName: string;
begin
  Result := fName;
end;

procedure TCommand.SetCommand(const AValue: string);
begin
  fCommand := AValue;
end;

procedure TCommand.SetEnvVariablesCount(APropIndex: integer; AValue: integer);
begin
  fEnvVariables.Count := AValue;
end;

procedure TCommand.SetName(AValue: string);
begin
  fName := AValue;
end;

function TCommand.GetParameters(AIndex: Integer; APropIndex: integer): TParameter;
begin
  if fParameters[AIndex] = nil then
    fParameters[AIndex] := TParameter.Create;
  Result := fParameters[AIndex];
end;

function TCommand.GetParametersCount(APropIndex: integer): integer;
begin
  Result := fParameters.Count;
end;

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
  if EnvVariableTemplate <> nil then
  begin
    for i := 0 to EnvVariableTemplate.EnvVariablesCount - 1 do
    begin
      //AProcess.Environment.Add(EnvVariableTemplate[i].Name + '=' + EnvVariableTemplate[i].Value);
      iAdd(EnvVariableTemplate[i]);
    end;
  end;
  for i := 0 to EnvVariablesCount - 1 do
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
  for i := 0 to ParametersCount - 1 do
  begin
    AProcess.Parameters.Add(Parameters[i].Value);
  end;
end;

procedure TCommand.SetEnvVariables(AIndex: Integer; APropIndex: integer;
  AValue: TEnvVariable);
begin
  if fEnvVariables[AIndex] <> nil then
    fEnvVariables[AIndex].Free;
  fEnvVariables[AIndex] := AValue;
end;

procedure TCommand.SetEnvVariableTemplate(AIndex: Integer;
  AValue: TEnvVariableTemplate);
begin
  fEnvVariableTemplate := AValue;
end;

procedure TCommand.SetParameters(AIndex: Integer; APropIndex: integer;
  AValue: TParameter);
begin
  if fParameters[AIndex] <> nil then
    fParameters[AIndex].Free;
  fParameters[AIndex] := AValue;
end;

procedure TCommand.AfterConstruction;
begin
  fEnvVariables := TEnvVariables.Create;
  fParameters := TParameters.Create;
end;

procedure TCommand.BeforeDestruction;
begin
  FreeAndNil(fEnvVariables);
  FreeAndNil(fParameters);
  inherited BeforeDestruction;
end;

procedure TCommand.SetDirectory(AValue: string);
begin
  fDirectory := AValue;
end;

procedure TCommand.SetParametersCount(APropIndex: integer; AValue: integer);
begin
  fParameters.Count := AValue;
end;

function TCommand.GetEnvVariableTemplate(AIndex: Integer): TEnvVariableTemplate;
begin
  Result := fEnvVariableTemplate;
end;

function TCommand.GetDirectory: string;
begin
  Result := fDirectory;
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
    if RunInTerminal then
      mProcess.Options := mProcess.Options + [poNewConsole];
    mProcess.Execute;
  finally
    mProcess.Free;
  end;
end;

end.
