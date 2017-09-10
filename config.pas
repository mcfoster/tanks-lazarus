(******************************************************************************
*
*
******************************************************************************)
unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,INIFiles;

const sectionScore = 'Place ';
const sKeyUser = 'User';
const sKeyScore = 'Score';

type
    TConfig = class
    private
        INI: TINIFile;
    public
        constructor Create;
        destructor Destroy;
        function GetScore(Place : Integer; var user : String) : Integer;
        procedure SetScore(Place : Integer; user : String; score : Integer);
    end;

implementation

(******************************************************************************
*
******************************************************************************)
constructor TConfig.Create;
var
    path : String;
begin
    path := ParamStr(0);
    path := ChangeFileExt(path, '.ini');
    INI :=  TINIFile.Create(path,False);
    //nil;
end;

destructor TConfig.Destroy;
begin
    // Cleanup
    INI.Free;
end;

(******************************************************************************
*
******************************************************************************)
function TConfig.GetScore(Place : Integer; var user : String) : Integer;
var
    iScore: Integer;
begin
    iScore := INI.ReadInteger(sectionScore + IntToStr(Place), sKeyScore, 0);
    user := INI.ReadString(sectionScore + IntToStr(Place), sKeyUser, 'User');
    GetScore := iScore;
end;

(******************************************************************************
*
******************************************************************************)
procedure TConfig.SetScore(Place : Integer; user : String; score : Integer);
begin
    INI.WriteInteger(sectionScore + IntToStr(Place), sKeyScore,score);
    INI.WriteString(sectionScore + IntToStr(Place), sKeyUser,user);
end;

end.

