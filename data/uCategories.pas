unit uCategories;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, rtti_broker_iBroker, rtti_serializer_uSerialObject,
  fgl, uCommands;

type

  { TCategory }

  TCategory = class(TRBCustomObject)
  private type
    TCommands = TFPGObjectList<TCommand>;
  private
    fName: string;
    fCommands: TCommands;
    fFavorite: Boolean;
    function GetCommands(AIndex: Integer; APropIndex: integer): TCommand;
    function GetCommandsCount(AIndex: Integer): integer;
    procedure SetCommands(AIndex: Integer; APropIndex: integer; AValue: TCommand);
    procedure SetCommandsCount(AIndex: Integer; AValue: integer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Name: string read fName write fName;
    property Commands[AIndex: integer]: TCommand index crbList + crbObject + crbRef read GetCommands write SetCommands; default;
    property CommandsCount: integer index crbListCounter read GetCommandsCount write SetCommandsCount;
    property Favorite: Boolean read fFavorite write fFavorite;
  end;

implementation

{ TCategory }

function TCategory.GetCommands(AIndex: Integer; APropIndex: integer): TCommand;
begin
  Result := fCommands[AIndex];
end;

function TCategory.GetCommandsCount(AIndex: Integer): integer;
begin
  Result := fCommands.Count;
end;

procedure TCategory.SetCommands(AIndex: Integer; APropIndex: integer;
  AValue: TCommand);
begin
  fCommands[AIndex] := AValue;;
end;

procedure TCategory.SetCommandsCount(AIndex: Integer; AValue: integer);
begin
  fCommands.Count := AValue;
end;

procedure TCategory.AfterConstruction;
begin
  inherited AfterConstruction;
  fCommands := TCommands.Create(False);
end;

procedure TCategory.BeforeDestruction;
begin
  FreeAndNil(fCommands);
  inherited BeforeDestruction;
end;

end.

