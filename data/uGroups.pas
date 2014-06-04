unit uGroups;

{$mode delphi}{$H+}

interface

uses
  sysutils, tvr.rtti.broker.iBroker, tvr.rtti.serializer.uSerialObject, fgl, uCommands;

type

  { TGroupItem }

  TGroupItem = class(TRBCustomObject)
  private type
    TChilds = TFPGObjectList<TGroupItem>;
  private
    fParent: TGroupItem;
    fChilds: TChilds;
    fName: string;
    fCaption: string;
    fCommand: TCommand;
    function GetCaption: string;
    function GetChilds(AIndex: Integer; APropIndex: integer): TGroupItem;
    function GetChildsCount(AIndex: Integer): integer;
    function GetCommand(APropIndex: integer): TCommand;
    function GetName: string;
    function GetParent(AIndex: Integer): TGroupItem;
    procedure SetCaption(AValue: string);
    procedure SetChilds(AIndex: Integer; APropIndex: integer; AValue: TGroupItem);
    procedure SetChildsCount(AIndex: Integer; AValue: integer);
    procedure SetCommand(APropIndex: integer; AValue: TCommand);
    procedure SetName(AValue: string);
    procedure SetParent(AIndex: Integer; AValue: TGroupItem);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Name: string read GetName write SetName;
    property Caption: string read GetCaption write SetCaption;
    property Parent: TGroupItem index crbObject + crbNotStored read GetParent write SetParent;
    property Childs[AIndex: integer]: TGroupItem index crbList + crbObject read GetChilds write SetChilds; default;
    property ChildsCount: integer index crbListCounter read GetChildsCount write SetChildsCount;
    property Command: TCommand index crbObject + crbRef  read GetCommand write SetCommand;
  end;

  { TGroup }

  TGroup = class(TRBCustomObject)
  private
    fName: string;
    fRoot: TGroupItem;
    function GetName: string;
    function GetRoot(AIndex: Integer): TGroupItem;
    procedure SetName(AValue: string);
    procedure SetRoot(AIndex: Integer; AValue: TGroupItem);
  public
    destructor Destroy; override;
  published
    property Name: string read GetName write SetName;
    property Root: TGroupItem index crbObject read GetRoot write SetRoot;
  end;


implementation

{ TGroupItem }

function TGroupItem.GetChilds(AIndex: Integer; APropIndex: integer): TGroupItem;
begin
  if fChilds[AIndex] = nil then
  begin
    fChilds[AIndex] := TGroupItem.Create;
    fChilds[AIndex].Parent := Self;
  end;
  Result := fChilds[AIndex];
end;

function TGroupItem.GetCaption: string;
begin
  Result := fCaption;
end;

function TGroupItem.GetChildsCount(AIndex: Integer): integer;
begin
  Result := fChilds.Count;
end;

function TGroupItem.GetCommand(APropIndex: integer): TCommand;
begin
  Result := fCommand;
end;

function TGroupItem.GetName: string;
begin
  Result := fName;
end;

function TGroupItem.GetParent(AIndex: Integer): TGroupItem;
begin
  Result := fParent;
end;

procedure TGroupItem.SetCaption(AValue: string);
begin
  fCaption := AValue;
end;

procedure TGroupItem.SetChilds(AIndex: Integer; APropIndex: integer;
  AValue: TGroupItem);
begin
  if fChilds[AIndex] <> nil then
    fChilds[AIndex].Free;
  fChilds[AIndex] := AValue;
end;

procedure TGroupItem.SetChildsCount(AIndex: Integer; AValue: integer);
begin
  fChilds.Count := AValue;
end;

procedure TGroupItem.SetCommand(APropIndex: integer; AValue: TCommand);
begin
  fCommand := AValue;
end;

procedure TGroupItem.SetName(AValue: string);
begin
  fName := AValue;
end;

procedure TGroupItem.SetParent(AIndex: Integer; AValue: TGroupItem);
begin
  fParent := AValue;
end;

procedure TGroupItem.AfterConstruction;
begin
  inherited AfterConstruction;
  fChilds := TChilds.Create(True);
end;

procedure TGroupItem.BeforeDestruction;
begin
  FreeAndNil(fChilds);
  inherited BeforeDestruction;
end;

{ TGroup }

function TGroup.GetRoot(AIndex: Integer): TGroupItem;
begin
  if fRoot = nil then
    fRoot := TGroupItem.Create;
  Result := fRoot;
end;

function TGroup.GetName: string;
begin
  Result := fName;
end;

procedure TGroup.SetName(AValue: string);
begin
  fName := AValue;
end;

procedure TGroup.SetRoot(AIndex: Integer; AValue: TGroupItem);
begin
  if fRoot <> nil then
    fRoot.Free;
  fRoot := AValue;
end;

destructor TGroup.Destroy;
begin
  FreeAndNil(fRoot);
  inherited Destroy;
end;

end.
