program Tanks;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainForm, Config, top_ten
  { you can add units after this };

{$R *.res}

begin
    Application.Title:='Tank Battle';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
    Application.CreateForm(TFormTopTen, FormTopTen);
  Application.Run;
end.

