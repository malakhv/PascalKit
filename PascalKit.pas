{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PascalKit;

{$warn 5023 off : no warning about unused units}
interface

uses
  Mikhan.Util.StrUtils, Mikhan.Util.AppArgs, Mikhan.Util.AppVersion, 
  Mikhan.Util.AppLogs, Mikhan.Util.Dump, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PascalKit', @Register);
end.
