program currency;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {Form17};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm17, Form17);
  Application.Run;
end.
