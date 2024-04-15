unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, FMX.Effects, FMX.StdCtrls, FMX.Ani,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,System.JSON;

type
  TfrmMain = class(TForm)
    Rectangle1: TRectangle;
    Layout1: TLayout;
    laUpdate: TLayout;
    circleUpdate: TCircle;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    ListView1: TListView;
    FDConnection1: TFDConnection;
    qData: TFDQuery;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    qCcy: TFDQuery;
    BindSourceDB2: TBindSourceDB;
    LinkListControlToFieldccy: TLinkListControlToField;
    rErr: TRectangle;
    Rectangle2: TRectangle;
    ErrText: TLabel;
    ShadowEffect1: TShadowEffect;
    rDelete: TRectangle;
    Rectangle4: TRectangle;
    lblAsk: TLabel;
    ShadowEffect2: TShadowEffect;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    qExec: TFDQuery;
    FDTransaction1: TFDTransaction;
    Timer1: TTimer;
    procedure Edit1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1DeletingItem(Sender: TObject; AIndex: Integer; var ACanDelete: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure FDConnection1BeforeConnect(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    cur_ccy:string;
    procedure SetErrorText(txt:string);
    procedure LogExcept(code,errtext,msg:string);
    procedure UpdateRate(isAll:boolean);
    procedure ResetCcy(ccy:string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses  System.IOUtils;



procedure TfrmMain.Button2Click(Sender: TObject);
begin
rDelete.Visible:=false;
end;

procedure TfrmMain.ComboBox1Change(Sender: TObject);
begin
   qData.Close;
   qData.ParamByName('BASE_CCY').Value:=ComboBox1.Items[ComboBox1.ItemIndex];
         try
            qData.Open;
         except on e:exception do
             LogExcept('CCY',e.Message,'Exception on request');
         end;
end;

procedure TfrmMain.Edit1Change(Sender: TObject);
begin
    if (StrToFloatdef(edit1.text,0)=0)and(edit1.Text<>'0') then
     //SetErrorText('Invalid value for converting')
      LogExcept('SUMM_IN','can''t convert string to float','Invalid value for converting')
     else begin
       SetErrorText('');
       qData.Close;
       qData.ParamByName('SUMM').Value:=strtofloat(edit1.Text);

         try
            qData.Open;
         except on e:exception do
             LogExcept('SUMM',e.Message,'Exception on request');
         end;
     end;
end;

procedure TfrmMain.FDConnection1BeforeConnect(Sender: TObject);
begin
{$IF DEFINED(iOS) or DEFINED(ANDROID)}
  FDConnection1.Params.Values['Database']:=TPath.Combine(TPath.GetDocumentsPath, 'rates.s3db');
{$ENDIF}
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

  rdelete.Visible:=false; // hide form for delete ask
  SetErrorText('');       // Clear error message
  try
    FDConnection1.Connected:=true;
    qCcy.Open;
    qData.Open;
  except on e:exception do
     LogExcept('START',e.Message,'Invalid database connect');
  end;
end;



procedure TfrmMain.ListView1DeletingItem(Sender: TObject; AIndex: Integer; var ACanDelete: Boolean);
var ccy:string;
begin
  ccy:=ListView1.Items[AIndex].Text.Split([' '])[0];
  if ccy<>'UAH' then begin  // UAH is important currency for calculate rate
    lblAsk.Text:='Confirm to delete the cyrrency: '+ccy;
    lblAsk.TagString:=ccy;
    rDelete.Align:=TAlignLayout.Client;
    rDelete.Visible:=true;
  end;
  ACanDelete:=false;
end;

procedure TfrmMain.LogExcept(code, errtext, msg: string);
var
   fLog: TextFile;
   fnm:string;
begin
     try
       fnm:='currency.log';
{$IF DEFINED(iOS) or DEFINED(ANDROID)}
    fnm :=TPath.Combine(TPath.GetDocumentsPath, fnm);
{$ENDIF}
       AssignFile(fLog, fnm);
       if FileExists(fnm) then Append(fLog) else Rewrite(fLog);
       WriteLn(fLog, DateTimeToStr(now)+#9+code+#9+errtext);
       CloseFile(fLog);
     except
       // exception on create log is not critical )
     end;
    SetErrorText(msg);
end;

procedure TfrmMain.ResetCcy(ccy: string);
var x:integer;
begin
    try
      x:=ComboBox1.Items.IndexOf(ccy);
    except
      x:=0;
    end;
    if ComboBox1.Items.Count>x then ComboBox1.ItemIndex:=x;
end;

procedure TfrmMain.SetErrorText(txt: string);
begin
  if txt='' then begin  // for hide error box - message is empty
      rErr.Visible:=false;
      ListView1.Visible:=true;
  end else begin
      ErrText.Text:=txt;
      rErr.Visible:=true;
      ListView1.Visible:=false;
  end;
end;


procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
// for real-time display rate!
Timer1.Enabled:=false;
  UpdateRate(false);
timer1.Enabled:=true;
end;



procedure TfrmMain.Button1Click(Sender: TObject);
begin
  try
    if ComboBox1.ItemIndex>=0 then cur_ccy:=ComboBox1.Items[ComboBox1.ItemIndex];
    qCcy.close;
    qData.close;
  FDTransaction1.StartTransaction;
    qExec.ExecSQL('delete from rates where ccy= :CCY',[lblAsk.TagString]);
  FDTransaction1.commit;
    qCcy.Open;
    ResetCcy(cur_ccy);
    qData.Open;
  except on e:exception do begin
    if FDTransaction1.Active then FDTransaction1.Rollback;
     LogExcept('DELETE',e.Message,'Error on delete currency');
  end;
  end;
  rDelete.Visible:=false;
end;


procedure TfrmMain.Image1Click(Sender: TObject);
begin
  updateRate(true);
end;




procedure TfrmMain.UpdateRate(isAll: boolean);
var
  HTTP:TnetHTTPClient;
  rs, ccy:string;
  rate:extended;
  JSON:TJSONArray;
  x:integer;
begin
  if ComboBox1.ItemIndex>=0 then cur_ccy:=ComboBox1.Items[ComboBox1.ItemIndex];
  qCcy.close;
  qData.close;
  FDTransaction1.StartTransaction;
  try
    if isall then qExec.ExecSQL('delete from rates where ccy<> ''UAH'''); // clear all rates
     HTTP := TnetHTTPClient.Create(nil);
      try
        HTTP.ConnectionTimeout := 1000;
        HTTP.ResponseTimeout := 5000;
        http.UserAgent:='Mozilla/5.0 (Linux; Android 7.0; SM-G892A Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/67.0.3396.87 Mobile Safari/537.36';
        // Get date from NBU

          rs:=HTTP.Get('https://bank.gov.ua/NBUStatService/v1/statdirectory/exchange?json').ContentAsString;
          try
            JSON:=TJSONObject.ParseJSONValue(rs) as TJSONArray;
            for x:=0 to JSON.Count -1 do begin
             ccy:=(json.Items[x] as TJSONObject).GetValue('cc').Value;
             rate:=StrToFloatDef((json.Items[x] as TJSONObject).GetValue('rate').value.Replace('.',','),0);
             if isall then qExec.ExecSQL('insert into rates (ccy,rate) values(:CCY, :RATE)',[ccy,rate]) else
                           qExec.ExecSQL('update rates set rate= :RATE where ccy =:CCY',[ccy,rate]);
            end;
           except on e:exception do begin
            if FDTransaction1.Active then FDTransaction1.Rollback;
            LogExcept('RESPONCE',e.Message,'Invalid server responce');
           end;
          end;

        if FDTransaction1.Active then FDTransaction1.commit;
       SetErrorText('');
      except on e:exception do begin
            if FDTransaction1.Active then FDTransaction1.Rollback;
            LogExcept('REQUEST',e.Message,'Server is not available');
         end;
      end;
    HTTP.Free;
    qCcy.Open;
    ResetCcy(cur_ccy);
    qData.Open;
  except on e:exception do  begin
     if FDTransaction1.Active then FDTransaction1.Rollback;
     LogExcept('UPDATE',e.Message,'Error on update rates');
  end;
  end;
end;



end.
