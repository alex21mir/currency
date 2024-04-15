unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, FMX.Effects, FMX.StdCtrls;

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
    procedure Edit1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1DeletingItem(Sender: TObject; AIndex: Integer; var ACanDelete: Boolean);
    procedure Button2Click(Sender: TObject);
  private
    procedure SetErrorText(txt:string);
    procedure LogExcept(code,errtext,msg:string);
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

procedure TfrmMain.FormCreate(Sender: TObject);
begin
{$IF DEFINED(iOS) or DEFINED(ANDROID)}
  FDConnection1.Params.Values['Database']:=TPath.Combine(TPath.GetDocumentsPath, 'rates.s3db');
{$ENDIF}
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
  lblAsk.Text:='Confirm to delete the cyrrency: '+ccy;
  rDelete.Align:=TAlignLayout.Client;
  rDelete.Visible:=true;
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

end.
