(******************************************************************************
* Tank Battle, Main form
* Author:  Martin Foster
* Date: January 10, 2012
* Revision history:
* Date      Author          Description
* --------  --------------- -------------------------------------------------
*
******************************************************************************)
unit mainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, top_ten;
const
    BlueTank = 1;
    RedTank = 2;
    DeadTank = 3;

    DIR_COUNT = 8;
    EXP_COUNT = 3;
    DEAD_COUNT = 2;

    GoodGuyIdx = 0;
type
  TBulletRec = Record
              direction, x, y, scrn, dist : Integer;
             End;
  TTankRec = Record
              direction, x, y, scrn, tnkColor : Integer;
             End;
  TdeadTankRec = Record
              x, y, scrn, idx : Integer;
             End;
  Texplode = Record
              x, y, scrn, idx : Integer;
             End;
  TItemRec = Record
              x, y, scrn : Integer;
             End;

  { TForm1 }

  TForm1 = class(TForm)
      Image1: TImage;
      SpeedButton1: TSpeedButton;
      StatusBar1: TStatusBar;
      Timer1: TTimer;
      ToolBar1: TToolBar;
      procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
      procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure FormShow(Sender: TObject);
      procedure Image1Click(Sender: TObject);
      procedure Image1Paint(Sender: TObject);
      procedure SpeedButton1Click(Sender: TObject);
      procedure Timer1Timer(Sender: TObject);

  private
      score : Integer;
    { private declarations }
      procedure MoveTank(tankIdx, cnt:Integer);
      function MoveTopLeft(pos, cnt : Integer): Integer;
      function MoveBtmRight(pos, cnt, max : Integer): Integer;
      procedure NewScreenCheck(tankIdx : Integer);
      procedure badGuyRoutine(tankIdx : Integer);
      procedure InitShotOffset;
      procedure InitImages;
      procedure InitScrn1;
      procedure InitScrn2;
      procedure InitScrn3;
      procedure InitScrn4;
      procedure InitLists;
      procedure CheckGameOver;
      procedure ChkCollisions;
      function Collision(x, y, x1, y1, x2, y2: Integer): Boolean;
      function chkBump( x, y : Integer): Boolean;
      function TankCollision(x,y : Integer; var idx : Integer) : Boolean;
      function WallCollision(x,y : Integer; var idx : Integer) : Boolean;
      function AimingAtTarget(idx : Integer) : Boolean;
 public
    { public declarations }
  end; 

var
  Form1: TForm1; 
  blueTanks : Array[1..DIR_COUNT] of TBitmap;
  redTanks : Array[1..DIR_COUNT] of TBitmap;
  deadTanks : Array[1..DEAD_COUNT] of TBitmap;
  explosions : Array[1..EXP_COUNT] of TBitmap;
  ShotStartX : Array[1..DIR_COUNT] of Integer;
  ShotStartY : Array[1..DIR_COUNT] of Integer;
  MaxX, MaxY : Integer;
  BlockImage : TBitmap;
  TreeImage  : TBitmap;
  blueCount, redCount : Integer;

  brPtr : ^TBulletRec;
  bulletList : TList;
  tanksList : TList;
  explosionList : TList;
  blocksList : TList;
  treeList : TList;
  curScrn : Integer;
  sUserName : String;
implementation

{$R *.lfm}

{ TForm1 }

(******************************************************************************
*
******************************************************************************)
procedure TForm1.FormShow(Sender: TObject);
var
  br : TBrush;
begin
    // Setup tank field
    br := TBrush.Create;
    br.Color:= clWhite;
    Image1.Canvas.Brush := br;
    Image1.Canvas.FillRect(0,0,Image1.Width, Image1.Height);

    bulletList := TList.Create;
    tanksList := TList.Create;
    explosionList := TList.Create;
    blocksList  := TList.Create;
    treeList := TList.Create;
    InitImages;
    InitLists;
    InitShotOffset;
    MaxX := Image1.Width - blueTanks[1].Width;
    MaxY := Image1.Height - blueTanks[1].Height;
    Image1.Repaint;
    Timer1.Enabled:=true;
    curScrn := 1;
    sUserName := '';
    //Form1.Repaint;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin

end;

(******************************************************************************
*
******************************************************************************)
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
    TnkPtr : ^TTankRec;
    i : Integer;
begin
    Timer1.Enabled:=False;
    // Init Tank Images
    for i := 1 to DIR_COUNT do
    begin
        blueTanks[i].Destroy;
        redTanks[i].Destroy;
    end;
    // Explosion images
    for i:= 1 to EXP_COUNT do
    begin
        explosions[i].Destroy;
    end;
    // Dead tank images
    for i:= 1 to DEAD_COUNT do
    begin
        deadTanks[i].Destroy;
    end;

    for i := bulletList.Count-1 downto 0 do
    begin
        brPtr :=  bulletList.Items[i];
        Dispose (brPtr);
        bulletList.Delete(i);
    end;

    for i:= tanksList.Count-1 downto 0 do
    begin
        TnkPtr := tanksList.Items[i];
        Dispose (TnkPtr);
        tanksList.Delete(i);
    end;
end;

(*****************************************************************************
* Shot should start after canon barrel. This routin sets the offset relative
* to the tank image based on the direction.
*****************************************************************************)
procedure TForm1.InitShotOffset;
begin
    // Up
    ShotStartX[1] := blueTanks[1].Width DIV 2;
    ShotStartY[1] := 0;
    // Up/Right
    ShotStartX[2] := blueTanks[1].Width;
    ShotStartY[2] := 0;
    // Right
    ShotStartX[3] := blueTanks[1].Width;
    ShotStartY[3] := blueTanks[1].Height DIV 2;
    // Rignt/Down
    ShotStartX[4] := blueTanks[1].Width;
    ShotStartY[4] := blueTanks[1].Height;
    // Down
    ShotStartX[5] := blueTanks[1].Width DIV 2;
    ShotStartY[5] := blueTanks[1].Height;
    // Down/Left
    ShotStartX[6] := 0;
    ShotStartY[6] := blueTanks[1].Height;
    // Left
    ShotStartX[7] := 0;
    ShotStartY[7] := blueTanks[1].Height DIV 2;
    // Left/Up
    ShotStartX[8] := 0;
    ShotStartY[8] := 0;

end;

(******************************************************************************
*
******************************************************************************)
procedure TForm1.InitImages;
var
    i : Integer;
begin
    // Init Tank Images
    for i := 1 to DIR_COUNT do
    begin
        blueTanks[i] := TBitmap.Create;
        redTanks[i] := TBitmap.Create;
        blueTanks[i].LoadFromFile('Tank' + IntToStr(i) + '.bmp');
        blueTanks[i].Transparent := True;
        blueTanks[i].TransparentColor:=clWhite;
        redTanks[i].LoadFromFile('TankR' + IntToStr(i) + '.bmp');
        redTanks[i].Transparent := True;
        redTanks[i].TransparentColor:=clWhite;
    end;
    // Explosion images
    for i:= 1 to EXP_COUNT do
    begin
        explosions[i] := TBitmap.Create;
        explosions[i].LoadFromFile('explode' + IntToStr(i) + '.bmp');
        explosions[i].Transparent := True;
        explosions[i].TransparentColor:=clWhite;
    end;
    // Smoldering tank images
    for i:= 1 to DEAD_COUNT do
    begin
        deadTanks[i] := TBitmap.Create;
        deadTanks[i].LoadFromFile('deadTank' + IntToStr(i) + '.bmp');
        deadTanks[i].Transparent := True;
        deadTanks[i].TransparentColor:=clWhite;
    end;
    BlockImage := TBitmap.Create;
    BlockImage.LoadFromFile('Bricks.bmp');

    TreeImage := TBitmap.Create;
    TreeImage.LoadFromFile('Tree1.bmp');
    TreeImage.Transparent:= True;
    TreeImage.TransparentColor:= clWhite;
end;

(******************************************************************************
* Procedure: InitScrn1;
* Parameters: None
* This function initializes screen #1, the top left game screen.
*     ||
*  1  ||  2
* ==========
*  3  ||  4
*     ||
******************************************************************************)
procedure TForm1.InitScrn1;
const
    boxTop = 110;
    boxLeft = 90;
    boxH = 20; // blocks high
    boxW = 20; // blocks wide
    scrNo = 1;
var
    TnkPtr : ^TTankRec;
    ptRec : ^TItemRec;
    i, w, h, x, y: Integer;
    gate_y1, gate_y2, gate_x1, gate_x2 : Integer;
begin
    w := BlockImage.Width;
    h := BlockImage.Height;
    gate_y1 := (Image1.Height DIV 4) + (BlockImage.Height DIV 2);
    gate_y2 := gate_y1 * 2;
    gate_x1 := (Image1.Width DIV 4) + (BlockImage.Width DIV 2);
    gate_x2 := gate_x1 * 2;

    i:= w;
    repeat
        New(ptRec);
        ptRec^.x := i;
        ptRec^.y := 0;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        if((i < gate_x1) or (i >= gate_x2)) then
        begin
            New(ptRec);
            ptRec^.x := i;
            ptRec^.y := Image1.Height - h;
            ptRec^.scrn:= scrNo;
            blocksList.Add(ptRec);
        end;
        i := i + w;
    until i >= Image1.Width - w;

    i:= 0;
    repeat
        New(ptRec);
        ptRec^.x := 0;
        ptRec^.y := i;
        ptRec^.scrn := scrNo;
        blocksList.Add(ptRec);
        if((i < gate_y1) or (i >= gate_y2)) then
        begin
            New(ptRec);
            ptRec^.x := Image1.Width - w;
            ptRec^.y := i;
            ptRec^.scrn := scrNo;
            blocksList.Add(ptRec);
        end;
        i := i + h;
    until i > Image1.Height - h;

    x := boxLeft;
    y := boxTop;
    for i := 0 to boxW do
    begin
        //top
        New(ptRec);
        ptRec^.x := x;
        ptRec^.y := y;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        // bottom
        New(ptRec);
        ptRec^.x := x;
        ptRec^.y := boxTop + (boxH * h);
        ptRec^.scrn := scrNo;
        blocksList.Add(ptRec);
        x := x + w;
    end;

    New(TnkPtr);
    TnkPtr^.direction:= 5;
    TnkPtr^.tnkColor:= BlueTank; // Good guy
    TnkPtr^.x:= 20;
    TnkPtr^.y:= 20;
    TnkPtr^.scrn := scrNo;
    tanksList.Add(TnkPtr);
    New(TnkPtr);
    TnkPtr^.direction:= 4;
    TnkPtr^.tnkColor:= RedTank; // Bad guy
    TnkPtr^.x:= 150;
    TnkPtr^.y:= 30;
    TnkPtr^.scrn := scrNo;
    tanksList.Add(TnkPtr);
    New(TnkPtr);
    TnkPtr^.direction:= 2;
    TnkPtr^.tnkColor:= RedTank; // Bad guy
    TnkPtr^.x:= 160;
    TnkPtr^.y:= 200;
    TnkPtr^.scrn := scrNo;
    tanksList.Add(TnkPtr);

    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 40;
    ptRec^.y := Image1.Height DIV 2;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 60;
    ptRec^.y := (Image1.Height DIV 2) + 10;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 50;
    ptRec^.y := (Image1.Height DIV 2) + 15;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    {***********************************************}
end;

(******************************************************************************
* Procedure: InitScrn2;
* Parameters: None
* This function initializes screen #2, the top right game screen.
*     ||
*  1  ||  2
* ==========
*  3  ||  4
*     ||
******************************************************************************)
procedure TForm1.InitScrn2;
const
    boxTop = 110;
    boxLeft = 90;
    boxH = 20; // blocks high
    boxW = 20; // blocks wide
    scrNo = 2;
var
    TnkPtr : ^TTankRec;
    ptRec : ^TItemRec;
    i, w, h, x, y: Integer;
    gate_y1, gate_y2, gate_x1, gate_x2 : Integer;
begin
    w := BlockImage.Width;
    h := BlockImage.Height;
    gate_y1 := (Image1.Height DIV 4) + (BlockImage.Height DIV 2);
    gate_y2 := gate_y1 * 2;
    gate_x1 := (Image1.Width DIV 4) + (BlockImage.Width DIV 2);
    gate_x2 := gate_x1 * 2;

    i:= w;
    repeat
        New(ptRec);
        ptRec^.x := i;
        ptRec^.y := 0;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        if((i < gate_x1) or (i >= gate_x2)) then
        begin
            New(ptRec);
            ptRec^.x := i;
            ptRec^.y := Image1.Height - h;
            ptRec^.scrn:= scrNo;
            blocksList.Add(ptRec);
        end;
        i := i + w;
    until i >= Image1.Width - w;

    i:= 0;
    repeat
        New(ptRec);
        ptRec^.x := Image1.Width - w;
        ptRec^.y := i;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        if((i < gate_y1) or (i >= gate_y2)) then
        begin
            New(ptRec);
            ptRec^.x := 0;
            ptRec^.y := i;
            ptRec^.scrn:= scrNo;
            blocksList.Add(ptRec);
        end;
        i := i + h;
    until i > Image1.Height - h;

    x := boxLeft;
    y := boxTop;
    for i := 0 to boxH do
    begin
        //top
        New(ptRec);
        ptRec^.x := x;
        ptRec^.y := y;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        // bottom
        New(ptRec);
        ptRec^.x := x + (boxW * w);
        ptRec^.y := y;
        ptRec^.scrn := scrNo;
        blocksList.Add(ptRec);
        y := y + h;
    end;

    New(TnkPtr);
    TnkPtr^.direction:= 4;
    TnkPtr^.tnkColor:= RedTank; // Bad guy
    TnkPtr^.x:= Image1.Width - 40;
    TnkPtr^.y:= 30;
    TnkPtr^.scrn:= scrNo;
    tanksList.Add(TnkPtr);
    New(TnkPtr);
    TnkPtr^.direction:= 2;
    TnkPtr^.tnkColor:= RedTank; // Bad guy
    TnkPtr^.x:= 160;
    TnkPtr^.y:= 200;
    TnkPtr^.scrn:= scrNo;
    tanksList.Add(TnkPtr);

    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 40;
    ptRec^.y := Image1.Height DIV 2;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 60;
    ptRec^.y := (Image1.Height DIV 2) + 10;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 50;
    ptRec^.y := (Image1.Height DIV 2) + 15;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 4) - 20;
    ptRec^.y := (Image1.Height DIV 4) + 20;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 3) - 60;
    ptRec^.y := (Image1.Height DIV 4) + 10;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 3) - 50;
    ptRec^.y := (Image1.Height DIV 4) + 15;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    {***********************************************}
end;

(******************************************************************************
* Procedure: InitScrn3;
* Parameters: None
* This function initializes screen #2, the top right game screen.
*
*     ||
*  1  ||  2
* ==========
*  3  ||  4
*     ||
******************************************************************************)
procedure TForm1.InitScrn3;
const
    boxTop = 115;
    boxLeft = 90;
    boxH = 18; // blocks high
    boxW = 22; // blocks wide
    scrNo = 3;
var
    TnkPtr : ^TTankRec;
    ptRec : ^TItemRec;
    i, w, h, x, y: Integer;
    gate_y1, gate_y2, gate_x1, gate_x2 : Integer;
begin
    w := BlockImage.Width;
    h := BlockImage.Height;
    gate_y1 := (Image1.Height DIV 4) + (BlockImage.Height DIV 2);
    gate_y2 := gate_y1 * 2;
    gate_x1 := (Image1.Width DIV 4) + (BlockImage.Width DIV 2);
    gate_x2 := gate_x1 * 2;

    i:= w;
    repeat
        New(ptRec);
        ptRec^.x := i;
        ptRec^.y := Image1.Height - h;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        if((i < gate_x1) or (i >= gate_x2)) then
        begin
            New(ptRec);
            ptRec^.x := i;
            ptRec^.y := 0;
            ptRec^.scrn:= scrNo;
            blocksList.Add(ptRec);
        end;
        i := i + w;
    until i >= Image1.Width - w;

    i:= 0;
    repeat
        New(ptRec);
        ptRec^.x := 0;
        ptRec^.y := i;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        if((i < gate_y1) or (i >= gate_y2)) then
        begin
            New(ptRec);
            ptRec^.x := Image1.Width - w;
            ptRec^.y := i;
            ptRec^.scrn:= scrNo;
            blocksList.Add(ptRec);
        end;
        i := i + h;
    until i > Image1.Height - h;

    x := boxLeft;
    y := boxTop;
    for i := 0 to boxH do
    begin
        //top
        New(ptRec);
        ptRec^.x := x;
        ptRec^.y := y;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        // bottom
        New(ptRec);
        ptRec^.x := x + (boxW * w);
        ptRec^.y := y;
        ptRec^.scrn := scrNo;
        blocksList.Add(ptRec);
        y := y + h;
    end;

    New(TnkPtr);
    TnkPtr^.direction:= 4;
    TnkPtr^.tnkColor:= RedTank; // Bad guy
    TnkPtr^.x:= Image1.Width - 40;
    TnkPtr^.y:= 30;
    TnkPtr^.scrn:= scrNo;
    tanksList.Add(TnkPtr);
    New(TnkPtr);
    TnkPtr^.direction:= 2;
    TnkPtr^.tnkColor:= RedTank; // Bad guy
    TnkPtr^.x:= 160;
    TnkPtr^.y:= Image1.Height - 60;
    TnkPtr^.scrn:= scrNo;
    tanksList.Add(TnkPtr);

    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 45;
    ptRec^.y := Image1.Height DIV 2 - 10;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 60;
    ptRec^.y := (Image1.Height DIV 2) + 10;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 2) - 50;
    ptRec^.y := (Image1.Height DIV 2) + 15;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 4) - 20;
    ptRec^.y := (Image1.Height DIV 4) + 20;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 3) - 60;
    ptRec^.y := (Image1.Height DIV 4) + 10;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := (Image1.Width DIV 3) - 50;
    ptRec^.y := (Image1.Height DIV 4) + 15;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    {***********************************************}
end;

(******************************************************************************
* Procedure: InitScrn4;
* Parameters: None
* This function initializes screen #1, the top left game screen.
*     ||
*  1  ||  2
* ==========
*  3  ||  4
*     ||
******************************************************************************)
procedure TForm1.InitScrn4;
const
    boxTop = 110;
    boxLeft = 110;
    boxH = 18; // blocks high
    boxW = 18; // blocks wide
    scrNo = 4;
var
    TnkPtr : ^TTankRec;
    ptRec : ^TItemRec;
    i, w, h, x, y: Integer;
    gate_y1, gate_y2, gate_x1, gate_x2 : Integer;
begin
    w := BlockImage.Width;
    h := BlockImage.Height;
    gate_y1 := (Image1.Height DIV 4) + (BlockImage.Height DIV 2);
    gate_y2 := gate_y1 * 2;
    gate_x1 := (Image1.Width DIV 4) + (BlockImage.Width DIV 2);
    gate_x2 := gate_x1 * 2;

    i:= w;
    repeat
        New(ptRec);
        ptRec^.x := i;
        ptRec^.y := Image1.Height - h;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        if((i < gate_x1) or (i >= gate_x2)) then
        begin
            New(ptRec);
            ptRec^.x := i;
            ptRec^.y := 0;
            ptRec^.scrn:= scrNo;
            blocksList.Add(ptRec);
        end;
        i := i + w;
    until i >= Image1.Width - w;

    i:= 0;
    repeat
        New(ptRec);
        ptRec^.x := Image1.Width - w;
        ptRec^.y := i;
        ptRec^.scrn := scrNo;
        blocksList.Add(ptRec);
        if((i < gate_y1) or (i >= gate_y2)) then
        begin
            New(ptRec);
            ptRec^.x := 0;
            ptRec^.y := i;
            ptRec^.scrn := scrNo;
            blocksList.Add(ptRec);
        end;
        i := i + h;
    until i > Image1.Height - h;

    x := boxLeft;
    y := boxTop;
    for i := 0 to boxW do
    begin
        //top
        New(ptRec);
        ptRec^.x := x;
        ptRec^.y := y;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        // bottom
        New(ptRec);
        ptRec^.x := x;
        ptRec^.y := boxTop + (boxH * h);
        ptRec^.scrn := scrNo;
        blocksList.Add(ptRec);
        x := x + w;
    end;
    x := boxLeft;
    for i := 0 to boxH do
    begin
        //top
        New(ptRec);
        ptRec^.x := x;
        ptRec^.y := y;
        ptRec^.scrn:= scrNo;
        blocksList.Add(ptRec);
        y := y + h;
    end;

    New(TnkPtr);
    TnkPtr^.direction:= 4;
    TnkPtr^.tnkColor:= RedTank; // Bad guy
    TnkPtr^.x:= 140;
    TnkPtr^.y:= 20;
    TnkPtr^.scrn := scrNo;
    tanksList.Add(TnkPtr);
    New(TnkPtr);
    TnkPtr^.direction:= 2;
    TnkPtr^.tnkColor:= RedTank; // Bad guy
    TnkPtr^.x:= 130;
    TnkPtr^.y:= 130;
    TnkPtr^.scrn := scrNo;
    tanksList.Add(TnkPtr);

    New(ptRec);
    ptRec^.x := 30;
    ptRec^.y := 30;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := 60;
    ptRec^.y := 40;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := 50;
    ptRec^.y := 55;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := Image1.Width  - 60;
    ptRec^.y := Image1.Height - 60;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    New(ptRec);
    ptRec^.x := Image1.Width  - 50;
    ptRec^.y := Image1.Height - 65;
    ptRec^.scrn:= scrNo;
    treeList.Add(ptRec);
    {***********************************************}
end;

(******************************************************************************
*
******************************************************************************)
procedure TForm1.InitLists;
var
    TnkPtr : ^TTankRec;
    ptRec : ^TItemRec;
    i : Integer;
begin
    for i := explosionList.Count-1 downto 0 do
    begin
        brPtr :=  explosionList.Items[i];
        Dispose (brPtr);
        explosionList.Delete(i);
    end;
    explosionList.Clear;

    for i := bulletList.Count-1 downto 0 do
    begin
        brPtr :=  bulletList.Items[i];
        Dispose (brPtr);
        bulletList.Delete(i);
    end;
    bulletList.Clear;

    for i:= tanksList.Count-1 downto 0 do
    begin
        TnkPtr := tanksList.Items[i];
        Dispose (TnkPtr);
        tanksList.Delete(i);
    end;
    tanksList.Clear;

    for i:= blocksList.Count-1 downto 0 do
    begin
        ptRec := blocksList.Items[i];
        Dispose (ptRec);
        blocksList.Delete(i);
    end;
    blocksList.Clear;

    for i:= treeList.Count-1 downto 0 do
    begin
        ptRec := treeList.Items[i];
        Dispose (ptRec);
        treeList.Delete(i);
    end;
    treeList.Clear;
    blueCount := 1;
    redCount  := 8;
    score := 1000;
    InitScrn1;
    InitScrn2;
    InitScrn3;
    InitScrn4;
end;

(******************************************************************************
* Key down event handler.
******************************************************************************)
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
    TnkPtr : ^TTankRec;
    v : Word;
    pnt : Boolean; // Repaint flag
begin
    v := Key;
    pnt := False;
    TnkPtr := tanksList.Items[GoodGuyIdx];
    if(TnkPtr^.tnkColor < DeadTank) then
    begin
        case v of
            $27: Begin
                   TnkPtr^.direction:=TnkPtr^.direction+1;
                   if(TnkPtr^.direction > DIR_COUNT)then
                        TnkPtr^.direction := 1;
                    pnt := True;
                End;
            $25: Begin
                    TnkPtr^.direction := TnkPtr^.direction-1;
                    if(TnkPtr^.direction < 1)then
                        TnkPtr^.direction := DIR_COUNT;
                    pnt := True;
                End;
            $26: Begin
                    MoveTank(GoodGuyIdx, 2);
                    pnt := True;
                End;
            $20: Begin
                    New(brPtr);
                    brPtr^.direction:= TnkPtr^.direction;
                    brPtr^.dist:= 0;
                    brPtr^.x:= TnkPtr^.x + ShotStartX[TnkPtr^.direction];
                    brPtr^.y:= TnkPtr^.y + ShotStartY[TnkPtr^.direction];
                    brPtr^.scrn:= curScrn;
                    bulletList.Add(brPtr);
                End;
            $51 : Close; // 'Q', Quit the application
        end;
    end;
    if(pnt) then
        Image1.Repaint;
 //   StatusBar1.SimpleText:= IntToHex(v,4);
end;

(******************************************************************************
* On paint event for the
******************************************************************************)
procedure TForm1.Image1Paint(Sender: TObject);
var
    TnkPtr : ^TTankRec;
    ExpRec: ^Texplode;
    ptRec : ^TItemRec;
    i, temp : Integer;
begin

    // Draw Wall
    for i := 0 to blocksList.Count - 1 do
    begin
        ptRec := blocksList.Items[i];
        if(ptRec^.scrn = curScrn) then
            Image1.Canvas.Draw(ptRec^.x, ptRec^.y, BlockImage);
    end;

    // Draw tanks
    for i := 0 to tanksList.Count-1 do
    begin
        TnkPtr := tanksList.Items[i];
        if(TnkPtr^.scrn = curScrn) then
        begin
          if (TnkPtr^.tnkColor = BlueTank) then
          begin
              Image1.Canvas.Draw(TnkPtr^.x, TnkPtr^.y,
                          blueTanks[TnkPtr^.direction]);

          end
          else if (TnkPtr^.tnkColor = RedTank) then
          begin
              Image1.Canvas.Draw(TnkPtr^.x, TnkPtr^.y,
                          redTanks[TnkPtr^.direction]);

          end
          else if (TnkPtr^.tnkColor = DeadTank) then
          begin
              Image1.Canvas.Draw(TnkPtr^.x, TnkPtr^.y,
                              deadTanks[TnkPtr^.direction]);
          end;
        end;

    end;
    // Draw bullets
    for i := 0 to bulletList.Count-1 do
    begin
        brPtr :=  bulletList.Items[i];
        if(brPtr^.scrn = curScrn) then
        begin
            canvas.Brush.color:= clBlack;
            Image1.Canvas.Ellipse(brPtr^.x-1,brPtr^.y-1,brPtr^.x+2,brPtr^.y+2);
        end;
    end;
    // Draw explosions
    {*****************}
    for i := 0 to explosionList.Count - 1 do
    begin
        ExpRec := explosionList.Items[i];
        if(ExpRec^.scrn = curScrn) then
           Image1.Canvas.Draw(ExpRec^.x, ExpRec^.y,
                       explosions[ExpRec^.idx]);
    end;
    {******************}
    // Draw Trees
    for i := 0 to treeList.Count - 1 do
    begin
        ptRec := treeList.Items[i];
        if(ptRec^.scrn = curScrn) then
            Image1.Canvas.Draw(ptRec^.x, ptRec^.y, TreeImage);
    end;
end;

(*****************************************************************************
*
*****************************************************************************)
procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
    Timer1.Enabled:= False;
    InitLists;
    curScrn := 1;
    Timer1.Enabled:= True;
end;

(*****************************************************************************
*
*****************************************************************************)
procedure TForm1.Timer1Timer(Sender: TObject);
const
    moveCnt = 6;
var
    ExpRec: ^Texplode;
    TnkPtr : ^TTankRec;
    i : Integer;
    done : Boolean;
    s : String;
begin
    // Move bullets
    for i := bulletList.Count-1 downto 0 do
    begin
        done := False;
        brPtr :=  bulletList.Items[i];
        brPtr^.dist:= brPtr^.dist + moveCnt;
        case brPtr^.direction of
            1: begin   // Up
                   if((brPtr^.y - moveCnt) > 0) then
                        brPtr^.y := brPtr^.y - moveCnt
                   else
                        done := True;
               end;
            2: begin  // Up / right
                   if((brPtr^.y - moveCnt) > 0) then
                        brPtr^.y := brPtr^.y - moveCnt
                   else
                        done := True;
                   if((brPtr^.x + moveCnt) < Image1.Width) then
                        brPtr^.x := brPtr^.x + moveCnt
                   else
                        done := True;
               end;
            3: begin  //  right
                   if((brPtr^.x + moveCnt) < Image1.Width) then
                        brPtr^.x := brPtr^.x + moveCnt
                   else
                        done := True;
               end;
            4: begin  // down / right
                   if((brPtr^.y + moveCnt)< Image1.Height) then
                        brPtr^.y := brPtr^.y + moveCnt
                   else
                        done := True;
                   if((brPtr^.x + moveCnt) < Image1.Width) then
                        brPtr^.x := brPtr^.x + moveCnt
                   else
                        done := True;
               end;
            5: begin  // down
                   if((brPtr^.y + moveCnt)< Image1.Height) then
                        brPtr^.y := brPtr^.y + moveCnt
                   else
                        done := True;
               end;
            6: begin  // down / left
                   if((brPtr^.y + moveCnt)< Image1.Height) then
                        brPtr^.y := brPtr^.y + moveCnt
                   else
                        done := True;
                   if((brPtr^.x - moveCnt) > 0) then
                        brPtr^.x := brPtr^.x - moveCnt
                   else
                        done := True;
               end;
            7: begin  // left
                    if((brPtr^.x - moveCnt) > 0) then
                         brPtr^.x := brPtr^.x - moveCnt
                    else
                         done := True;
                end;
            8: begin  // left / Up
                    if((brPtr^.y - moveCnt) > 0) then
                         brPtr^.y := brPtr^.y - moveCnt
                    else
                         done := True;
                    if((brPtr^.x - moveCnt) > 0) then
                         brPtr^.x := brPtr^.x - moveCnt
                    else
                         done := True;
                end;
        end;
        if(done) then
        begin
            Dispose (brPtr);
            bulletList.Delete(i);
        end;
    end;
    // Animate explosions
    for i := explosionList.Count - 1 downto 0 do
    begin
        ExpRec := explosionList.Items[i];
        Inc(ExpRec^.idx);
        if(ExpRec^.idx > EXP_COUNT) then
        begin
            Dispose (ExpRec);
            explosionList.Delete(i);
        end;
    end;

    blueCount := 0;
    redCount := 0;
    for i:= tanksList.Count-1 downto 0 do
    begin
        TnkPtr := tanksList.Items[i];

        if (TnkPtr^.tnkColor = DeadTank) then
        begin
             Inc(TnkPtr^.direction);
             if(TnkPtr^.direction > DEAD_COUNT) then
                TnkPtr^.direction := 1;
        end
        else if(TnkPtr^.tnkColor = BlueTank) then
            Inc(blueCount)
        else if(TnkPtr^.tnkColor = RedTank) then
        begin
            Inc(redCount);
            badGuyRoutine(i);
        end;
    end;
    if(score > 0) then
        Dec(score);
    StatusBar1.SimpleText:= 'Score: ' + IntToStr(score);
    ChkCollisions;
    Image1.Repaint;
    CheckGameOver;
 end;

(*****************************************************************************
*
*****************************************************************************)
procedure TForm1.CheckGameOver;
var
    frm : TFormTopTen;
begin
    if(blueCount < 1) then
     begin
         Timer1.Enabled:= False;
         ShowMessage('Game Over!' + sLineBreak +
           'Your tank has been destroyed.');
     end
     else if(redCount < 1) then
     begin
         Timer1.Enabled:= False;
         sUserName := InputBox('Mission Complete!',
         'Game Over! Your score: '
            + IntToStr(score) + sLineBreak +
           'Enter your name: ', sUserName);
         FormTopTen.PostScore(sUserName, Score);
     end;
end;

(*****************************************************************************
* Procedure: ChkCollisions;
* Parameters: None
*
*****************************************************************************)
procedure TForm1.ChkCollisions;
var
    TnkPtr : ^TTankRec;
    i, j, x1 : Integer;
    ExpRec: ^Texplode;
begin
    j := 0;
    for i := bulletList.Count-1 downto 0 do
    begin
        brPtr :=  bulletList.Items[i];
        if(TankCollision(brPtr^.x, brPtr^.y, j)) then
        begin
            try
               TnkPtr := tanksList.Items[j];
               New(ExpRec);
               ExpRec^.x:= brPtr^.x - (explosions[1].Width DIV 2);
               ExpRec^.y:= brPtr^.y - (explosions[1].Height DIV 2);
               ExpRec^.idx:= 1;
               ExpRec^.scrn:= curScrn;
               explosionList.Add(ExpRec);
               TnkPtr^.tnkColor := DeadTank;
               TnkPtr^.direction := 1;
               Dispose (brPtr);
               bulletList.Delete(i);
            except
                 x1 := j;
            end;
        end
        else if(WallCollision(brPtr^.x, brPtr^.y, j)) then
        begin
            try
               New(ExpRec);
               ExpRec^.x:= brPtr^.x - (explosions[1].Width DIV 2);
               ExpRec^.y:= brPtr^.y - (explosions[1].Height DIV 2);
               ExpRec^.idx:= 1;
               ExpRec^.scrn:= curScrn;
               explosionList.Add(ExpRec);
               Dispose (brPtr);
               bulletList.Delete(i);
            except
                 x1 := j;
            end;
        end;
    end;
end;

(*****************************************************************************
* Procedure:
* Parameters:
*
*****************************************************************************)
function TForm1.chkBump( x, y : Integer): Boolean;
var
    idx : Integer;
    retVal : Boolean;
begin
    retVal := False;
    idx := 0;
    if(TankCollision(x,y,idx)) then
        retVal := True
    else if(WallCollision(x,y, idx)) then
        retVal := True;

    chkBump := retVal;
end;

(*****************************************************************************
* Procedure:
* Parameters:
*
*****************************************************************************)
procedure TForm1.badGuyRoutine(tankIdx : Integer);
var
    TnkPtr : ^TTankRec;
    i : Integer;
begin
    TnkPtr := tanksList.Items[tankIdx];
//    if (TnkPtr^.scrn = curScrn) then
//    begin
      i := Random(10);
      if(i = 2) then
      begin
          Inc(TnkPtr^.direction);
          if(TnkPtr^.direction > DIR_COUNT)then
              TnkPtr^.direction := 1;
      end
      else if (i = 4) then
      begin
          Dec(TnkPtr^.direction);
          if(TnkPtr^.direction < 1)then
              TnkPtr^.direction := DIR_COUNT;
      end
      else
      begin
          MoveTank(tankIdx, 2);
          if(TnkPtr^.y < 2) then
          begin
              if(TnkPtr^.direction <= 2) then
                  Inc(TnkPtr^.direction)
              else if(TnkPtr^.direction = 8) then
                  Dec(TnkPtr^.direction);
          end
          else if(TnkPtr^.y >= (MaxY-2)) then
          begin
              if((TnkPtr^.direction >= 6) and (TnkPtr^.direction >3)) then
                  Dec(TnkPtr^.direction)
              else if(TnkPtr^.direction = 6) then
                  Inc(TnkPtr^.direction);
           end
          else if(TnkPtr^.x < 2) then
          begin
              if(TnkPtr^.direction = 8) then
                  TnkPtr^.direction := 1
              else if(TnkPtr^.direction > 5) then
                  Dec(TnkPtr^.direction);
          end
          else if(TnkPtr^.x >= (MaxX-2)) then
          begin
              if(TnkPtr^.direction = 2) then
                  Dec(TnkPtr^.direction)
              else if(TnkPtr^.direction = 3) or (TnkPtr^.direction = 4)then
                  Inc(TnkPtr^.direction);
          end;
      end;
      if(AimingAtTarget(tankIdx) and (TnkPtr^.scrn = curScrn)) then
      begin
          New(brPtr);
          brPtr^.direction:= TnkPtr^.direction;
          brPtr^.dist:= 0;
          brPtr^.scrn:= TnkPtr^.scrn;
          brPtr^.x:= TnkPtr^.x + ShotStartX[TnkPtr^.direction];
          brPtr^.y:= TnkPtr^.y + ShotStartY[TnkPtr^.direction];
          bulletList.Add(brPtr);
      end;
//    end;
end;

(*****************************************************************************
* Procedure:
* Parameters:
*
*****************************************************************************)
procedure TForm1.MoveTank(tankIdx, cnt:Integer);
var
    TnkPtr : ^TTankRec;
    x, y : Integer;
    xt, yt : Integer;
begin
    TnkPtr  := tanksList.Items[tankIdx];
    x := TnkPtr^.x;
    y := TnkPtr^.y;
    case TnkPtr^.direction of
        1: begin
            y := MoveTopLeft(TnkPtr^.y, cnt);
        end;
        2: begin
            y := MoveTopLeft(TnkPtr^.y, cnt);
            x := MoveBtmRight(TnkPtr^.x, cnt, MaxX );
        end;
        3: begin
            x := MoveBtmRight(TnkPtr^.x, cnt, MaxX );
        end;
        4: begin
            x := MoveBtmRight(TnkPtr^.x, cnt, MaxX);
            y := MoveBtmRight(TnkPtr^.y, cnt, MaxY);
        end;
        5: begin
            y := MoveBtmRight(TnkPtr^.y, cnt, MaxY);
        end;
        6: begin
            y := MoveBtmRight(TnkPtr^.y, cnt, MaxY);
            x := MoveTopLeft(TnkPtr^.x, cnt);
        end;
        7: begin
            x := MoveTopLeft(TnkPtr^.x, cnt);
        end;
        8: begin
            y := MoveTopLeft(TnkPtr^.y, cnt);
            x := MoveTopLeft(TnkPtr^.x, cnt);
        end;
    end;
    xt := TnkPtr^.x + ShotStartX[TnkPtr^.direction];
    yt := TnkPtr^.y + ShotStartY[TnkPtr^.direction];

    if(not chkBump( xt,yt)) then
    begin
        TnkPtr^.x := x;
        TnkPtr^.y := y;
    end;
    NewScreenCheck(tankIdx);
end;

(*****************************************************************************
* Procedure:
* Parameters:
*
*****************************************************************************)
function TForm1.MoveTopLeft(pos, cnt : Integer): Integer;
begin
    pos := pos - cnt;
    if(pos <= 1)  then
        pos := 1;
    MoveTopLeft := pos;
end;

(*****************************************************************************
* Procedure:
* Parameters:
* Returns:
*****************************************************************************)
function TForm1.MoveBtmRight(pos, cnt, max : Integer): Integer;
begin
    pos := pos + cnt;
    if(pos > max) then
        pos := max;
    MoveBtmRight := pos;
end;

(*****************************************************************************
* Procedure:
* Parameters:
* Returns:
*****************************************************************************)
procedure TForm1.NewScreenCheck(tankIdx : Integer);
var
    TnkPtr : ^TTankRec;
begin
    TnkPtr := tanksList.Items[tankIdx];
    if(TnkPtr^.scrn = 1) then
    begin
        if(TnkPtr^.x >= MaxX-4) then
        begin
            TnkPtr^.scrn := 2;
            TnkPtr^.x:= 5;
            if (TnkPtr^.tnkColor = BlueTank) then // Goog guy?
                curScrn := 2;
        end
        else if(TnkPtr^.y >= MaxY-4) then
        begin
            // Screen 3?
          TnkPtr^.scrn := 3;
          TnkPtr^.y := 5;
          if (TnkPtr^.tnkColor = BlueTank) then // Goog guy?
              curScrn := 3;
        end;
    end
    else if(TnkPtr^.scrn = 2) then
    begin
        if(TnkPtr^.x < 4) then
        begin
            TnkPtr^.scrn := 1;
            TnkPtr^.x:= MaxX - (blueTanks[1].Width + 1);
            if (TnkPtr^.tnkColor = BlueTank) then // Goog guy?
                curScrn := 1;
        end
        else if(TnkPtr^.y >= MaxY-4) then
        begin
            TnkPtr^.scrn := 4;
            TnkPtr^.y := 5;
            if (TnkPtr^.tnkColor = BlueTank) then // Goog guy?
                curScrn := 4;
        end;
    end
    else if(TnkPtr^.scrn = 3) then
    begin
        if(TnkPtr^.y < 4) then
        begin
            TnkPtr^.scrn := 1;
            TnkPtr^.y:= MaxY - (blueTanks[1].Height + 1);
            if (TnkPtr^.tnkColor = BlueTank) then // Goog guy?
                curScrn := 1;
        end
        else if(TnkPtr^.x >= MaxX-4) then
        begin
            TnkPtr^.scrn := 4;
            TnkPtr^.x:= 5;
            if (TnkPtr^.tnkColor = BlueTank) then // Goog guy?
                curScrn := 4;
        end;
    end
    else if(TnkPtr^.scrn = 4) then
    begin
        if(TnkPtr^.x < 4) then
        begin
            TnkPtr^.scrn := 3;
            TnkPtr^.x:= MaxX - (blueTanks[1].Width + 1);
            if (TnkPtr^.tnkColor = BlueTank) then // Goog guy?
                curScrn := 3;
        end
        else if(TnkPtr^.y < 4) then
        begin
            TnkPtr^.scrn := 2;
            TnkPtr^.y:= MaxY - (blueTanks[1].Height + 1);
            if (TnkPtr^.tnkColor = BlueTank) then // Goog guy?
                curScrn := 2;
        end;
    end;
end;

(*****************************************************************************
*
*****************************************************************************)
function TForm1.TankCollision(x,y : Integer; var idx : Integer) : Boolean;
var
    TnkPtr : ^TTankRec;
    retval : Boolean;
    i, x1, x2, y1, y2 : Integer;
begin
    retval := False;
    idx := -1;
    i := tanksList.Count-1;
    while(i >=0) do
    begin
        TnkPtr := tanksList.Items[i];
        if(TnkPtr^.scrn = curScrn) then
        begin
          x1 := TnkPtr^.x;
          y1 := TnkPtr^.y;
          x2 := TnkPtr^.x + blueTanks[1].Width;
          y2 := TnkPtr^.y + blueTanks[1].Height;
          if(Collision(x, y, x1, y1, x2, y2)) then
          begin
              retval := True;
              idx := i;
              i := 0;
              break;
          end;
        end;
        i := i-1;
    end;

    TankCollision  := retval;
end;

(*****************************************************************************
* Procedure:
* Parameters: x,y  - Point to be tested
*             idx  - Index of the wall block to ne checked
* Returns: True if point is inside the wall block
* Test collision of point with a wall block.
*****************************************************************************)
function TForm1.WallCollision(x,y : Integer; var idx : Integer) : Boolean;
var
    retval : Boolean;
    i, x1, x2, y1, y2: Integer;
    ptRec : ^TItemRec;
begin
    retval := False;
    i := blocksList.Count-1;
    while(i >=0) do
    begin
        ptRec := blocksList.Items[i];
        if(ptRec^.scrn = curScrn)  then
        begin
            x1 := ptRec^.x -1;
            y1 := ptRec^.y -1;
            x2 := ptRec^.x + BlockImage.Width;
            y2 := ptRec^.y + BlockImage.Height;
            if(Collision(x, y,
                            x1, y1, x2, y2)) then
            begin
                retval := True;
                idx := i;
                i := 0;
                break;
            end;
        end;
        i := i-1;
    end;
    WallCollision  := retval;
end;

(*****************************************************************************
* Test to see if point is inside rectangle.
* Parameters:
*   x, y            - Point
*   x1, y1, x2, y2  - Rectangle
* Returns:
*****************************************************************************)
function TForm1.Collision(x, y, x1, y1, x2, y2: Integer): Boolean;
var
    retval : Boolean;
begin
    retval := False;
    if((x >x1) and (x <x2) and (y > y1) and (y < y2)) then
        retval := True;
    Collision := retval;
end;  // Collision

(******************************************************************************
* Test to see if tank[idx] is aiming at a target.
******************************************************************************)
function TForm1.AimingAtTarget(idx : Integer) : Boolean;
var
    retVal : Boolean;
    TnkPtr : ^TTankRec;
    dir, deltaX, deltaY, t : Integer;
    x1, x2, y1, y2 : Integer;
begin
    retVal := false;
    TnkPtr := tanksList.Items[idx];
    x2 := TnkPtr^.x + (blueTanks[1].Width DIV 2);
    y2 := TnkPtr^.y + (blueTanks[1].Height DIV 2);
    dir := TnkPtr^.direction;

    TnkPtr := tanksList.Items[GoodGuyIdx];

    if(TnkPtr^.tnkColor = BlueTank ) then
    begin
        x1 := TnkPtr^.x + (blueTanks[1].Width DIV 2);
        y1 := TnkPtr^.y + (blueTanks[1].Height DIV 2);

        deltaX  := (x1 - x2);
        deltaY  := (y2 - y1);
        if(deltaY = 0) then
            t := 999
        else
            t := (deltaX * 100) DIV deltaY;
        case dir of
            1 : retVal := (deltaY > 1) and ( Abs(t) < 20);
            2 : retVal := (deltaX > 1) and ( t > 40) and (t < 300);
            3 : retVal := (deltaX > 1) and ( Abs(t) > 400);
            4 : retVal := (deltaX > 1) and ( t < -40) and (t > -300);
            5 : retVal := (deltaY < -1) and ( Abs(t) < 20);
            6 : retVal := (deltaX < -1) and ( t > 40) and (t < 300);
            7 : retVal := (deltaX < -1) and ( Abs(t) > 400);
            8 : retVal := (deltaX < -1) and ( t < -40) and (t > -300);
        end;
    end;

    AimingAtTarget := retVal;
end;

end.

