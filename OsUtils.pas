unit OsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImgList;

type
  IOsUtils = interface
  ['{5A09C8CA-AB75-4438-8534-E4F7B69A1446}']
    function MenuHeight: integer;
    function NewGID: string;
  end;

implementation

end.


