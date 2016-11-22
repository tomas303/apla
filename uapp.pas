unit uapp;

{$mode delphi}{$H+}

interface

uses
  tal_uapp, tal_ilauncher, tal_uguilauncher,
  fLauncher,
  uCommands, uCategories,
  fCommand, fCommands,
  fCategory, fCategories,
  forms,
  trl_ipersist, trl_upersiststore,
  trl_dicontainer,
  trl_irttibroker, trl_urttibroker,
  trl_upersistxml,
  tvl_udatabinder, tvl_utallybinders,
  tvl_ibindings, tvl_iedit, tvl_ubehavebinder,
  tvl_iiconutils, tvl_uiconutils,
  tvl_igui, tvl_ugui;

type

  { TApp }

  TApp = class(TALApp)
  protected
    procedure RegisterCore;
    procedure RegisterGUI;
    procedure RegisterPersist;
  protected
    procedure RegisterAppServices; override;
    procedure BeforeLaunch; override;
    procedure AfterLaunch; override;
  end;

implementation

{ TApp }

procedure TApp.RegisterCore;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TGUILauncher, ILauncher);
  mReg.InjectProp('MainForm', IMainForm);
  mReg := DIC.Add(TGUI, IGUI);
  mReg := DIC.Add(TIconUtils, IIconUtils);
end;

procedure TApp.RegisterGUI;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TRBBehavioralBinder, IRBBehavioralBinder);
  //
  mReg := DIC.Add(TLauncherForm, Application, IMainForm);
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Commands', IListData, 'CommandsForm');
  mReg.InjectProp('Categories', IListData, 'CategoriesForm');
  mReg.InjectProp('GUI', IGUI);
  mReg.InjectProp('IconUtils', IIconUtils);
  //
  mReg := DIC.Add(TCommandsForm, Application, IListData, 'CommandsForm');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Binder', IRBTallyBinder, 'drawgrid', DIC);
  mReg.InjectProp('Edit', IEditData, 'CommandForm');
  //
  mReg := DIC.Add(TCommandForm, Application, IEditData, 'CommandForm');
  mReg.InjectProp('Binder', IRBDataBinder);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  //
  mReg := DIC.Add(TCategoriesForm, Application, IListData, 'CategoriesForm');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', DIC);
  mReg.InjectProp('Edit', IEditData, 'CategoryForm');
  //
  mReg := DIC.Add(TCategoryForm, Application, IEditData, 'CategoryForm');
  mReg.InjectProp('Binder', IRBDataBinder);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
end;

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TRBData, IRBData);
  //
  mReg := DIC.Add(TSIDList, ISIDList);
  //
  mReg := DIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := DIC.Add(TPersistManyRefs, IPersistManyRefs);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := DIC.Add(TPersistRefList, IPersistRefList);
  // persist data
  RegisterDataClass(DIC, TEnvVariable);
  RegisterDataClass(DIC, TParameter);
  RegisterDataClass(DIC, TEnvVariableTemplate);
  RegisterDataClass(DIC, TCommand);
  RegisterDataClass(DIC, TCategory);
  //
  mReg := DIC.Add(TStoreCache);
  //
  mReg := DIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := DIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('XMLFile', DataFile);
  mReg.InjectProp('Factory', IPersistFactory);
  // factory for persist data
  mReg := DIC.Add(TPersistFactory, IPersistFactory, '', ckSingle);
  mReg.InjectProp('Container', TDIContainer);
  // binders(conection between data and GUI)
  mReg := DIC.Add(TListBoxBinder, IRBTallyBinder, 'listbox');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := DIC.Add(TDrawGridBinder, IRBTallyBinder, 'drawgrid');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := DIC.Add(TRBDataBinder, IRBDataBinder);
end;

procedure TApp.RegisterAppServices;
begin
  inherited RegisterAppServices;
  RegisterCore;
  RegisterGUI;
  RegisterPersist;
end;

procedure TApp.BeforeLaunch;
var
  mStore: IPersistStore;
begin
  inherited BeforeLaunch;
  mStore := DIC.Locate(IPersistStore);
  mStore.Open;
end;

procedure TApp.AfterLaunch;
var
  mStore: IPersistStore;
begin
  inherited AfterLaunch;
  mStore := DIC.Locate(IPersistStore);
  mStore.Close;
end;

end.

