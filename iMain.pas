unit iMain;

{$mode objfpc}{$H+}

interface

uses
  tvr.rtti.serializer.iManager, tvr.rtti.broker.iBroker;

type

  { IMainContext }

  IMainContext = interface
  ['{44D9302B-B326-45FD-B659-9B1A28E274C5}']
    function GetDataStore: ISerialStore;
    function GetSerialFactory: ISerialFactory;
    function GetDataQuery: IRBDataQuery;
    property SerialFactory: ISerialFactory read GetSerialFactory;
    property DataStore: ISerialStore read GetDataStore;
    property DataQuery: IRBDataQuery read GetDataQuery;
  end;

  IMainContextSupport = interface
  ['{E8F2A90A-20DE-446A-8F19-FBD6A9DD0833}']
    procedure AttachMainContext(const AContext: IMainContext);
  end;

implementation

end.

