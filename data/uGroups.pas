unit uGroups;

{$mode delphi}{$H+}

interface

uses
  sysutils, fgl, uCommands,
  trl_irttibroker, trl_ifactory, trl_ipersist, trl_ipersiststore,
  trl_upersist, trl_upersiststore;

type

  { TGroupItem }

  TGroupItem = class
  protected type

    { IPersistManyTGroupItem }

    IPersistManyTGroupItem = interface(IPersistMany<TGroupItem>)
    ['{FD4E1FF2-1DA2-4A59-AEBE-57B5AC827DF6}']
    end;

    { TPersistManyTGroupItem }

    TPersistManyTGroupItem = class(TPersistManyObjects<TGroupItem>, IPersistManyTGroupItem)
    end;

  private
    fParent: TGroupItem;
    fChilds: IPersistManyTGroupItem;
    fName: string;
    fCaption: string;
    fCommand: IPersistRef<TCommand>;
  public
    procedure AfterConstruction; override;
    property Parent: TGroupItem read fParent write fParent;
  published
    property Name: string read fName write fName;
    property Caption: string read fCaption write fCaption;
    property Childs: IPersistManyTGroupItem read fChilds;
    property Command: IPersistRef<TCommand> read fCommand write fCommand;
  end;

  { TGroup }

  TGroup = class
  private
    fName: string;
    fRoot: TGroupItem;
    function GetRoot: TGroupItem;
    procedure SetRoot(AValue: TGroupItem);
  public
    destructor Destroy; override;
  published
    property Name: string read fName write fName;
    property Root: TGroupItem read GetRoot write SetRoot;
  end;


implementation

{ TGroupItem }

procedure TGroupItem.AfterConstruction;
begin
  inherited AfterConstruction;
  fChilds := TPersistManyTGroupItem.Create;
  fCommand := TPersistRef<TCommand>.Create;
end;

{ TGroup }

function TGroup.GetRoot: TGroupItem;
begin
  if fRoot = nil then
    fRoot := TGroupItem.Create;
  Result := fRoot;
end;

procedure TGroup.SetRoot(AValue: TGroupItem);
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

