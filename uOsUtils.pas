unit uOsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OsUtils, LCLIntf, LCLType, base64, ImgList, FPimage,
  Graphics;

type

  { TOsUtils }

  TOsUtils = class(TInterfacedObject, IOsUtils)
  public type
    TIconDir = record
      Height: integer;
      Name: string;
    end;
    TIconDirHeight = (ic16=16,ic22=22,ic24=24,ic32=32,ic36=36,ic48=48,
      ic64=64,ic72=72,ic96=96,ic128=128,ic192=192,ic259=256,ic512=512);
  public const
    cHiColorPath = '/usr/share/icons/hicolor/';
  protected
    function Scan(const APath: string): string;
    function FindDirHeight(AHeight: integer): TIconDirHeight;
  protected
    // IOsUtils
    function FindAppIconFile(const AApplication: string; AHeight: integer): string;
    function MenuHeight: integer;
    function AddApplicationImage(const AApplication: string; AHeight: integer;
      AImageList: TCustomImageList): integer;
    function NewGID: string;
  end;

implementation

{ TOsUtils }

function TOsUtils.Scan(const APath: string): string;
var
  mRec: TRawByteSearchRec;
  mExt: string;
begin
  Result := '';
  if FindFirst(APath, faanyfile, mRec) = 0 then
  repeat
    mExt := ExtractFileExt(mRec.Name);
    if SameText('.png', mExt) or SameText('.ico', mExt) then
    begin
      Result := mRec.Name;
      Exit;
    end;
  until FindNext(mRec) <> 0;
end;

function TOsUtils.FindDirHeight(AHeight: integer): TIconDirHeight;
var
  mDirHeight: TIconDirHeight;
begin
  Result := High(TIconDirHeight);
  for mDirHeight := High(TIconDirHeight) downto Low(TIconDirHeight) do
  begin
    if AHeight >= Ord(mDirHeight) then
    begin
      Result := mDirHeight;
      Break;
    end;
  end;
end;

function TOsUtils.FindAppIconFile(const AApplication: string; AHeight: integer): string;
var
  mDirHeight: TIconDirHeight;
  mActDirHeight: TIconDirHeight;
  mDir: string;
begin
  mActDirHeight := FindDirHeight(AHeight);
  for mDirHeight := mActDirHeight downto Low(TIconDirHeight) do
  begin
    // for now only hicolor - match or partial match
    mDir := IntToStr(Ord(mDirHeight));
    mDir := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(mDir + 'x' + mDir) + 'apps');
    Result := Scan(IncludeTrailingPathDelimiter(cHiColorPath) + mDir + ExtractFileName(AApplication) + '.*');
    if Result = '' then
       Result := Scan(IncludeTrailingPathDelimiter(cHiColorPath) + mDir + ExtractFileName(AApplication) + '*.*');
    if Result <> '' then
    begin
      Result := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(cHiColorPath) + mDir) + Result;
      Break;
    end;
  end;
end;

function TOsUtils.MenuHeight: integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
end;

function TOsUtils.AddApplicationImage(const AApplication: string;
  AHeight: integer; AImageList: TCustomImageList): integer;
var
  mIcoFile: string;
  mImage: TFPMemoryImage;
  mGraphic: TPortableNetworkGraphic;
  mBitmap: TBitmap;
begin
  Result := -1;
  mIcoFile := FindAppIconFile(AApplication, AHeight);
  if mIcoFile <> '' then
  begin
    mGraphic := TPortableNetworkGraphic.Create;
    try
      mGraphic.LoadFromFile(mIcoFile);
      mBitmap := TBitmap.Create;
      try
        if mGraphic.Height <= AHeight then
          mBitmap.Assign(mGraphic)
        else begin
          mBitmap.Width := AImageList.Width;
          mBitmap.Height := AImageList.Height;
          mBitmap.TransparentMode := mGraphic.TransparentMode;
          mBitmap.TransparentColor := mGraphic.TransparentColor;
          mBitmap.PixelFormat := mGraphic.PixelFormat;
          mBitmap.Masked := mGraphic.Masked;
          mBitmap.Canvas.StretchDraw(TRect.Create(0, 0, mBitmap.Width - 1, mBitmap.Height - 1), mGraphic);
        end;
        Result := AImageList.AddMasked(mBitmap, clBlack);
      finally
        mBitmap.Free;
      end;
    finally
      mGraphic.Free;
    end;
  end;
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

