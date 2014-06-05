unit uMain;

{$mode objfpc}{$H+}

interface

uses
  forms, sysutils, classes,
  iMain, rtti_serializer_iManager, rtti_serializer_uXmlStore,
  rtti_serializer_uFactory,
  uCommands, uGroups, uCategories, rtti_broker_iBroker;

type
   EMain = class(Exception)

   end;

  { TMain }

  TMain = class(TComponent, IMainContext)
  private
    fSerialFactory: ISerialFactory;
    fDataStore: ISerialStore;
    fMainF: TForm;
  protected
    // IMainContext
    function GetDataStore: ISerialStore;
    function GetSerialFactory: ISerialFactory;
    function GetDataQuery: IRBDataQuery;
  protected
    function PrepareHomeDir(const AHomeSubdir: string): string;
    function OpenDataStore(const AFile: string; ACanCreate: Boolean): Boolean;
    procedure InitData;
    procedure StartUp;
    procedure ShutDown;
    procedure Go(const AFormClass: TFormClass);
  public
    class procedure Run(const AFormClass: TFormClass);
    property SerialFactory: ISerialFactory read GetSerialFactory;
    property DataStore: ISerialStore read GetDataStore;
    property DataQuery: IRBDataQuery read GetDataQuery;
  end;

implementation

{ TMain }

function TMain.GetDataStore: ISerialStore;
begin
  Result := fDataStore;
end;

function TMain.GetSerialFactory: ISerialFactory;
begin
  Result := fSerialFactory;
end;

function TMain.GetDataQuery: IRBDataQuery;
begin
  Result := fDataStore as IRBDataQuery;
end;

function TMain.PrepareHomeDir(const AHomeSubdir: string): string;
begin
{$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + PathDelim + AHomeSubdir + PathDelim;
{$ENDIF UNIX}
{$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + AHomeSubdir + PathDelim;
{$ENDIF WINDOWS}
  if not DirectoryExists(Result) then
  begin
    if not ForceDirectories(Result) then
      raise EMain('Cannot create directory ' + Result);
  end;
end;

function TMain.OpenDataStore(const AFile: string; ACanCreate: Boolean): Boolean;
begin
  Result := False;
  if FileExists(AFile) or ACanCreate then
  begin
    fDataStore := TXmlStore.Create(fSerialFactory, AFile);
    Result := True;
  end;
end;

procedure TMain.InitData;
var
  mHomeDataFile: string;
begin
  fSerialFactory := TSerialFactory.Create;
  if OpenDataStore('./store/data.xml', False) then
    Exit;
  mHomeDataFile := PrepareHomeDir('.apla') + 'data.xml';
  if not OpenDataStore(mHomeDataFile, True) then
    raise EMain.Create('Cannot oper or create ' + mHomeDataFile);
end;

procedure TMain.StartUp;
begin
  InitData;
  fSerialFactory.RegisterClass(TCommand);
  fSerialFactory.RegisterClass(TEnvVariableTemplate);
  fSerialFactory.RegisterClass(TEnvVariable);
  fSerialFactory.RegisterClass(TParameter);
  fSerialFactory.RegisterClass(TCategory);
end;

procedure TMain.ShutDown;
begin
  fDataStore.Flush;
end;

procedure TMain.Go(const AFormClass: TFormClass);
begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(AFormClass, fMainF);
  if Supports(fMainF, IMainContextSupport) then
    (fMainF as IMainContextSupport).AttachMainContext(self);
  Application.ShowMainForm := False;
  Application.Run;
end;

class procedure TMain.Run(const AFormClass: TFormClass);
var
  m: TMain;
begin
  m := TMain.Create(nil);
  try
    m.StartUp;
    try
      m.Go(AFormClass);
    finally
      m.ShutDown;
    end;
  finally
    m.Free;
  end;
end;

end.


