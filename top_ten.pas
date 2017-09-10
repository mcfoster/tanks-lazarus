(******************************************************************************
*
******************************************************************************)
unit top_ten;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Grids, Buttons, Config;
const
  PLACES = 10;

type
    TScores = Record
       Score: Integer;
       User: String;
    End;

  { TFormTopTen }

  TFormTopTen = class(TForm)
      BitBtnOk: TBitBtn;
      Panel1: TPanel;
      StringGrid1: TStringGrid;
      procedure BitBtnOkClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
  private
    { private declarations }
      cfg : TConfig;
      scores : Array[1 .. PLACES] of TScores;
      procedure readScore;
      procedure sortScores(user : String; score : Integer);
      procedure saveScores;
      procedure showScores;
  public
    { public declarations }
    procedure PostScore(user : String; Score : Integer);
  end; 

var
  FormTopTen: TFormTopTen;


implementation

{$R *.lfm}

{ TFormTopTen }
(******************************************************************************
*
******************************************************************************)
procedure TFormTopTen.readScore;
var
    i : Integer;
    iScore : Integer;
    sName : String;
begin
    for i := 1 to PLACES do
    begin
        iScore := cfg.GetScore(i,sName);
        scores[i].Score:= iScore;
        scores[i].User:= sName;
    end;
end;

(******************************************************************************
*
******************************************************************************)
procedure TFormTopTen.saveScores;
var
    i : Integer;
    iScore : Integer;
    sName : String;
begin
    for i := 1 to PLACES do
    begin
        cfg.SetScore(i,scores[i].User,scores[i].Score);
    end;
end;

(******************************************************************************
*
******************************************************************************)
procedure TFormTopTen.sortScores(user : String; score : Integer);
var
    i : Integer;
    UserName : String;
    val : Integer;
    done : Boolean;
begin
    UserName := user;
    val := score;
    i := PLACES;
    if(scores[i].Score < val) then
    begin
        scores[i].Score := val;
        scores[i].User:= UserName;
        done := False;
        repeat
            if(scores[i].Score >  scores[i-1].Score) then
            begin
                val :=  scores[i].Score;
                UserName := scores[i].User;
                scores[i].User := scores[i-1].User;
                scores[i].Score := scores[i-1].Score;
                scores[i-1].Score := val;
                scores[i-1].User := UserName;
                Dec(i);
                if(i < 2) then
                    done := true;
            end
            else
                done := true;
        until done;
    end;

end;

(******************************************************************************
*
******************************************************************************)
procedure TFormTopTen.showScores;
var
    i: Integer;
begin
    for i := 1 to PLACES do
    begin
        StringGrid1.Cells[1, i] := scores[i].User;
        StringGrid1.Cells[2, i] := IntToStr(scores[i].Score);
    end;

end;

(******************************************************************************
*
******************************************************************************)
procedure TFormTopTen.PostScore(user : String; Score : Integer);
begin
    cfg := TConfig.Create;
    readScore;
    sortScores(user, score);
    saveScores;
    showScores;
    self.ShowModal();
    cfg.Free;
end;

(******************************************************************************
*
******************************************************************************)
procedure TFormTopTen.BitBtnOkClick(Sender: TObject);
begin
    Close();
end;

(******************************************************************************
*
******************************************************************************)
procedure TFormTopTen.FormShow(Sender: TObject);
var
    w, h : Integer;
begin
    w := StringGrid1.ColWidths[0] + StringGrid1.ColWidths[1]
        + StringGrid1.ColWidths[2] + 6;
    h := Panel1.Height + (StringGrid1.RowHeights[0] * 11) + 11;
    self.Width := w;
    self.Height:= h;
end;

end.

