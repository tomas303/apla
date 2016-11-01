unit uOsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OsUtils, LCLIntf, LCLType, base64;

type

  { TOsUtils }

  TOsUtils = class(TInterfacedObject, IOsUtils)
  protected
    // IOsUtils
    function MenuHeight: integer;
    function NewGID: string;
  end;

implementation

{ TOsUtils }

function TOsUtils.MenuHeight: integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
end;

function TOsUtils.NewGID: string;
var
  mGuid: TGuid;
  mEncStream: TBase64EncodingStream;
  mStrStream: TStringStream;
begin
  CreateGUID(mGuid);
  mStrStream := TStringStream.Create;
  try
    mEncStream := TBase64EncodingStream.Create(mStrStream);
    try
      mEncStream.Write(mGuid, SizeOf(mGuid));
    finally
      mEncStream.Free;
    end;
    Result := mStrStream.DataString;
    while Result[Length(Result)] = '=' do
      Delete(Result, Length(Result), 1);
  finally
    mStrStream.Free;
  end;
end;

end.

