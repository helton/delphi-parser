#!/usr/bin/python3
# -*- coding: utf-8 -*-


from delphi_lexer import DelphiLexer

statement = """
unit Functions.Classes.SSIOUtils;

interface

uses
  IOUtils, Classes;

type
  TSSDirectory = class
  public
    /// <summary> <para>função que retorna o diretório que a aplicação está rodando</para>
    /// </summary>
    class function ApplicationDirectory: String; static;
    /// <summary> <para>função que retorna o diretório binário da aplicação que está rodando</para>
    /// </summary>
    class function BinDirectory: String; static;
    class var
      DelphiDirectory: TDirectory;
    class function GetParentDirectory(APath: String; AFullPath: Boolean = True): String;
    class function DirectoryExists(ADir: String): Boolean;
    class function CreateDir(ADir: String): Boolean;
    class function RemoveDir(ADir: String): Boolean;
    class function RemoveDirIsEmpty(ADir: String): Boolean;
    class function IsSubDir(ADir: String; ASubDir: String): Boolean;
  end;

  TSSFile = class
  public
    class procedure StringToStream(ADataFile: String; ADest: TStream); static;
    class function StreamToString(ASource: TStream): string; static;
    class var
      DelphiFile: TFile;
    class function Move(ASourceFileName, ADestFileName: String): Boolean; static;
  end;

implementation

uses
  Functions.Classes.SSComponent, SysUtils, Basic.Classes.SSSmartPointer, ShellAPI,
  Basic.Classes.SSPersistent;

{ TSSDirectory }

class function TSSDirectory.ApplicationDirectory: String;
begin
  try    
    if TSSComponent.RunByDelphi then
      Result := 'C:\SuperSoft\'
    else
      Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class function TSSDirectory.BinDirectory: String;
begin
  try
    Result := ApplicationDirectory+'Bin\';
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class function TSSDirectory.CreateDir(ADir: String): Boolean;
begin
  Result := False;
  try
    Result := System.SysUtils.CreateDir(ADir);
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class function TSSDirectory.RemoveDir(ADir: String): Boolean;
begin
  Result := False;
  try
    Result := DelphiDirectory.Exists(ADir);
    if Result then
      DelphiDirectory.Delete(ADir, True);
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class function TSSDirectory.RemoveDirIsEmpty(ADir: String): Boolean;
begin
  Result := False;
  try
    if TSSDirectory.DelphiDirectory.IsEmpty(ADir) then
      Result := TSSDirectory.RemoveDir(ADir);
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class function TSSDirectory.DirectoryExists(ADir: String): Boolean;
begin
  Result := False;
  try
    Result := System.SysUtils.DirectoryExists(ADir);
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class function TSSDirectory.GetParentDirectory(APath: String; AFullPath: Boolean): String;
begin
  Result := '';
  try
    Result := ExpandFileName(APath + '\..') + '\';
    if not AFullPath then
      Result := TSSArrayString(ExtractFilePath(Result).Split(['\'])).Last;
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class function TSSDirectory.IsSubDir(ADir, ASubDir: String): Boolean;
begin
  Result := False;
  try
    Result := TSSArrayString(ExtractFilePath(ADir).Split(['\'])).Contains(ASubDir);
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

{ TSSFile }

class function TSSFile.Move(ASourceFileName, ADestFileName: String): Boolean;
begin
  Result := False;
  try
    ForceDirectories(ExtractFilePath(ADestFileName));
    if FileExists(ADestFileName) then
      DeleteFile(ADestFileName);
    TSSFile.DelphiFile.Move(ASourceFileName, ADestFileName);
    Result := FileExists(ADestFileName);
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class function TSSFile.StreamToString(ASource: TStream): string;
var
  SS: TSSSmartPointer<TStringStream>;
begin
  Result := '';
  try
    if ASource <> nil then
    begin
      SS := TStringStream.Create('', TEncoding.Default);
      SS.Instance.CopyFrom(ASource, 0);
      Result := SS.Instance.DataString;
    end;
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

class procedure TSSFile.StringToStream(ADataFile: String; ADest: TStream);
var
  SS: TSSSmartPointer<TStringStream>;
begin
  try
    if (ADest <> nil) and (ADataFile <> '') then
    begin
      SS := TStringStream.Create('', TEncoding.Default);
      SS.Instance.WriteString(ADataFile);
      ADest.CopyFrom(SS, 0);
      ADest.Position := 0;
    end;
  except
    on E: Exception do
      Assert(False, E.Message);
  end;
end;

//teste sdf sdf 
sdfs df

{ outro comment
asdf~lasd
asfçskdjfs
sfjsadfj asfçskdjfs
a
}

teste1
teste2
teste3.teste2.teste1
function teste: String;
begin
  Result := '';
end;

(* este
é o 
meu teste 
  *)

end.
"""

line_format = "| %20s | %30s | %6s | %6s |"
separator = '=' * 66

print('statement =', statement)
print(separator)
print(line_format % ('TYPE', 'VALUE', 'LINE', 'COLUMN'))
print(separator)
for token in DelphiLexer(statement).get_token():
    print(line_format % token)
print(separator)
