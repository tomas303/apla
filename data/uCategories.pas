unit uCategories;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  fgl, uCommands,
  trl_irttibroker, trl_ifactory, trl_ipersist,
  trl_upersist, trl_upersiststore;

type

  { TCategory }

  TCategory = class
  private
    fName: string;
    fCommands: IPersistManyRefs<TCommand>;
    fFavorite: Boolean;
    fRunAll: Boolean;
    fRunAllFavorite: Boolean;
  public
    procedure AfterConstruction; override;
  published
    property Name: string read fName write fName;
    property Commands: IPersistManyRefs<TCommand> read fCommands;
    property Favorite: Boolean read fFavorite write fFavorite;
    property RunAll: Boolean read fRunAll write fRunAll;
    property RunAllFavorite: Boolean read fRunAllFavorite write fRunAllFavorite;
  end;

implementation

{ TCategory }

procedure TCategory.AfterConstruction;
begin
  inherited AfterConstruction;
  fCommands := TPersistManyRefs<TCommand>.Create;
end;

end.

