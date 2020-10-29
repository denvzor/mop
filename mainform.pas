unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TAChartListbox,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Spin, ExtCtrls,
  moptask, complexes, moptype, mopmaterial, mopformula, mopproblem;

type

  { TmForm }

  TmForm = class(TForm)
    SortMat: TButton;
    EvalTaskBut: TButton;
    ExportTaskBut: TButton;
    ThiEdit: TFloatSpinEdit;
    OptThiEdit: TFloatSpinEdit;
    WvEdit: TFloatSpinEdit;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    NameMatMemo: TMemo;
    DelTaskBut: TButton;
    FrmEdit: TEdit;
    GroupBox1: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    opd: TOpenDialog;
    exd: TSaveDialog;
    StepEdit: TSpinEdit;
    svd: TSaveDialog;
    LayList: TListBox;
    UpDateFrm: TButton;
    MatSel: TComboBox;
    PolarEdit: TFloatSpinEdit;
    AngleEdit: TFloatSpinEdit;
    StartWvl: TFloatSpinEdit;
    EndWvl: TFloatSpinEdit;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    k_Line: TLineSeries;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label9: TLabel;
    TaskList: TListBox;
    LoadMat: TButton;
    Label8: TLabel;
    ComMatMemo: TMemo;
    LoadTask: TButton;
    NewTaskBut: TButton;
    n_Line: TLineSeries;
    SaveMat: TButton;
    matmemo: TMemo;
    NewMatBut: TButton;
    k_Chat: TChart;
    n_Chat: TChart;
    ChartListbox1: TChartListbox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MatList: TListBox;
    DelMatBut: TButton;
    SaveTask: TButton;
    T_Chart: TChart;
    R_Chart: TChart;
    ChartListbox2: TChartListbox;
    FormTabs: TPageControl;
    MaterialTab: TTabSheet;
    TaskTab: TTabSheet;
    VisTab: TTabSheet;
    procedure AngleEditEnter(Sender: TObject);
    procedure ComMatMemoExit(Sender: TObject);
    procedure DelMatButClick(Sender: TObject);
    procedure DelTaskButClick(Sender: TObject);
    procedure EndWvlEnter(Sender: TObject);
    procedure EvalTaskButClick(Sender: TObject);
    procedure ExportTaskButClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LayListSelectionChange(Sender: TObject; User: boolean);
    procedure LoadMatClick(Sender: TObject);
    procedure LoadTaskClick(Sender: TObject);
    procedure MatListSelectionChange(Sender: TObject; User: boolean);
    procedure matmemoEnter(Sender: TObject);
    procedure MatSelChange(Sender: TObject);
    procedure NameMatMemoEnter(Sender: TObject);
    procedure NewMatButClick(Sender: TObject);
    procedure NewTaskButClick(Sender: TObject);
    procedure OptThiEditEnter(Sender: TObject);
    procedure PolarEditEnter(Sender: TObject);
    procedure SaveMatClick(Sender: TObject);
    procedure SaveTaskClick(Sender: TObject);
    procedure SortMatClick(Sender: TObject);
    procedure StartWvlEnter(Sender: TObject);
    procedure StepEditEnter(Sender: TObject);
    procedure TaskListSelectionChange(Sender: TObject; User: boolean);
    procedure ThiEditEnter(Sender: TObject);
    procedure UpDateFrmClick(Sender: TObject);
    procedure WvEditEnter(Sender: TObject);
  private
    tsk: TMOPTask;
    function UpDateMat: boolean;
    function UpDateLay: boolean;
    function UpDatePro: boolean;
    procedure UpDateProName( index: integer );
    procedure UpDateLayName( index: integer );
    procedure DrawSolv( index: integer);
  public
    { public declarations }
  end;

var
  mForm: TmForm;

implementation

{$R *.lfm}

{ TmForm }

procedure TmForm.LoadMatClick(Sender: TObject);
begin
 if opd.Execute then
   begin
     tsk.LoadFromXML(opd.FileName, true);
     UpDateMat;
   end;
end;

procedure TmForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  tsk.free;
end;

procedure TmForm.ComMatMemoExit(Sender: TObject);
begin
  if MatList.ItemIndex >= 0 then
   tsk.FMaterials.Items[MatList.ItemIndex].Comment:=ComMatMemo.Text;
end;

procedure TmForm.AngleEditEnter(Sender: TObject);
begin
  if TaskList.ItemIndex >=0 then
  begin
   tsk.FProblems.Items[TaskList.ItemIndex].Angle:=abs(AngleEdit.Value);
   UpDateProName(TaskList.ItemIndex);
  end;
end;

procedure TmForm.DelMatButClick(Sender: TObject);
begin
  if MatList.ItemIndex >= 0 then
   begin
     tsk.DeleteMaterial(MatList.ItemIndex);
     UpDateMat;
   end;
end;

procedure TmForm.DelTaskButClick(Sender: TObject);
begin
  if MatList.ItemIndex >= 0 then
   begin
     tsk.FProblems.Delete(MatList.ItemIndex);
     UpDatePro;
   end;
end;

procedure TmForm.EndWvlEnter(Sender: TObject);
begin
  if TaskList.ItemIndex >=0 then
  begin
   tsk.FProblems.Items[TaskList.ItemIndex].EndWaveLength:=abs(EndWvl.Value);
   UpDateProName(TaskList.ItemIndex);
  end;
end;

procedure TmForm.EvalTaskButClick(Sender: TObject);
begin
  if TaskList.ItemIndex >=0 then
   begin
     if tsk.Solve(TaskList.ItemIndex) then
      begin
       DrawSolv(TaskList.ItemIndex);
      end;
   end;
end;

procedure TmForm.ExportTaskButClick(Sender: TObject);
begin
  if (TaskList.ItemIndex >= 0)
  and (assigned(tsk.FProblems.Items[TaskList.ItemIndex].Solvation))
  and (exd.Execute) then
   begin
    tsk.ExportResult(exd.FileName,TaskList.ItemIndex);
   end;
end;

procedure TmForm.FormCreate(Sender: TObject);
begin
  Randomize;
  tsk:=TMOPTask.Create;
end;

procedure TmForm.LayListSelectionChange(Sender: TObject; User: boolean);
begin
  MatSel.ItemIndex:=tsk.FLays.Lays.Items[LayList.ItemIndex].LinkMaterialIndex;
  WvEdit.text:=floattostr(tsk.FLays.Lays.Items[LayList.ItemIndex].Wavelength);
  ThiEdit.Text:=floattostr(tsk.FLays.Lays.Items[LayList.ItemIndex].Thickness);
  OptThiEdit.Text:=floattostr(tsk.FLays.Lays.Items[LayList.ItemIndex].OpticThickness);
end;

procedure TmForm.LoadTaskClick(Sender: TObject);
begin
  if opd.Execute then
   begin
     tsk.LoadFromXML(opd.FileName, false);
     UpDateMat;
     UpDateLay;
     UpDatePro;
     FrmEdit.Text:=tsk.FLays.GetFormula;
   end;
end;

procedure TmForm.MatListSelectionChange(Sender: TObject; User: boolean);
var j: integer;
    n_k: complex;
    st: string;
begin
  NameMatMemo.Text :=  tsk.FMaterials.Items[MatList.ItemIndex].MaterialName;
  ComMatMemo.Text := tsk.FMaterials.Items[MatList.ItemIndex].Comment;
  matmemo.Clear;
  k_Line.Clear;
  n_Line.Clear;
  st:='';
  for j:=0 to tsk.FMaterials.Items[MatList.ItemIndex].Points.Count-1 do
   begin
    n_k:=csqrt( tsk.FMaterials.Items[MatList.ItemIndex].Points.Items[j].eps );
    n_Line.AddXY(tsk.FMaterials.Items[MatList.ItemIndex].Points.Items[j].wln, n_k.re);
    k_Line.AddXY(tsk.FMaterials.Items[MatList.ItemIndex].Points.Items[j].wln, n_k.im);
    st:=st+floattostr(tsk.FMaterials.Items[MatList.ItemIndex].Points.Items[j].wln)+' '+
                   floattostr(n_k.re)+' '+floattostr(n_k.im)+#13;
   end;
  matmemo.Text:=st;
end;

procedure TmForm.matmemoEnter(Sender: TObject);
var i: integer;
    ww,nn,kk: real;
begin
  if MatList.ItemIndex >= 0 then
   begin
      tsk.FMaterials.Items[MatList.ItemIndex].Points.Clear;
      k_Line.Clear;
      n_Line.Clear;
      for i:=0 to matmemo.Lines.Count-1 do
       begin
        try
          ReadStr(matmemo.Lines.Strings[i],ww,nn,kk);
        finally
          tsk.FMaterials.Items[MatList.ItemIndex].Points.Add(TNKPoint.Create);
          tsk.FMaterials.Items[MatList.ItemIndex].Points.Last.wln:=ww;
          tsk.FMaterials.Items[MatList.ItemIndex].Points.Last.eps:=(nn+_i*kk)**2;
          n_Line.AddXY(ww, nn);
          k_Line.AddXY(ww, kk);
        end;
       end;
   end;
end;

procedure TmForm.MatSelChange(Sender: TObject);
begin
  if LayList.ItemIndex >= 0 then
   begin
    //tsk.FLays.Lays.Items[LayList.ItemIndex].LinkMaterialIndex:=MatSel.ItemIndex;
    tsk.FLays.UpdateMaterialIndex(tsk.FMaterials,
                               LayList.ItemIndex,MatSel.ItemIndex);
    UpDateLayName(LayList.ItemIndex);
   end;
end;

procedure TmForm.NameMatMemoEnter(Sender: TObject);
begin
  if MatList.ItemIndex >= 0 then
  begin
   tsk.FMaterials.Items[MatList.ItemIndex].MaterialName:=trim(NameMatMemo.Text);
   MatList.Items.Strings[MatList.ItemIndex]:=trim(NameMatMemo.Text);
   MatSel.Items.Strings[MatList.ItemIndex]:=trim(NameMatMemo.Text);
  end;
end;

procedure TmForm.NewMatButClick(Sender: TObject);
begin
  tsk.FMaterials.Add(TMOPMaterial.Create);
  tsk.FMaterials.Last.MaterialName:='New No-Name';
  UpDateMat;
end;

procedure TmForm.NewTaskButClick(Sender: TObject);
begin
  tsk.FProblems.Add(TMOPProblem.Create);
  tsk.FProblems.Last.StartWaveLength:=100;
  tsk.FProblems.Last.EndWaveLength:=1000;
  tsk.FProblems.Last.Steps:=100;
  UpDatePro;
end;

procedure TmForm.OptThiEditEnter(Sender: TObject);
begin
 if LayList.ItemIndex >= 0 then
 begin
   tsk.FLays.Lays.Items[LayList.ItemIndex].OpticThickness:=abs(strtofloat(OptThiEdit.Text));
   UpDateLayName(LayList.ItemIndex);
 end;
end;

procedure TmForm.PolarEditEnter(Sender: TObject);
begin
 if TaskList.ItemIndex >=0 then
 begin
  tsk.FProblems.Items[TaskList.ItemIndex].Polarization:=abs(PolarEdit.Value);
  UpDateProName(TaskList.ItemIndex);
 end;
end;

procedure TmForm.SaveMatClick(Sender: TObject);
begin
  if svd.Execute then
   tsk.SaveToXML(svd.FileName,true);
end;

procedure TmForm.SaveTaskClick(Sender: TObject);
begin
  if svd.Execute then
   tsk.SaveToXML(svd.FileName,false);
end;

procedure TmForm.SortMatClick(Sender: TObject);
begin
  tsk.SortMatByName;
end;

procedure TmForm.StartWvlEnter(Sender: TObject);
begin
  if TaskList.ItemIndex >=0 then
  begin
   tsk.FProblems.Items[TaskList.ItemIndex].StartWaveLength:=abs(StartWvl.Value);
   UpDateProName(TaskList.ItemIndex);
  end;
end;

procedure TmForm.StepEditEnter(Sender: TObject);
begin
  if TaskList.ItemIndex >=0 then
  begin
   tsk.FProblems.Items[TaskList.ItemIndex].Steps:=StepEdit.Value;
  end;
end;

procedure TmForm.TaskListSelectionChange(Sender: TObject; User: boolean);
begin
  if TaskList.ItemIndex >=0 then
  begin
   StartWvl.Value:=tsk.FProblems.Items[TaskList.ItemIndex].StartWaveLength;
   EndWvl.Value:=tsk.FProblems.Items[TaskList.ItemIndex].EndWaveLength;
   StepEdit.Value:=tsk.FProblems.Items[TaskList.ItemIndex].Steps;
   PolarEdit.Value:=tsk.FProblems.Items[TaskList.ItemIndex].Polarization;
   AngleEdit.Value:=tsk.FProblems.Items[TaskList.ItemIndex].Angle;
  end;
end;

procedure TmForm.ThiEditEnter(Sender: TObject);
begin
  if LayList.ItemIndex >= 0 then
  begin
   tsk.FLays.Lays.Items[LayList.ItemIndex].Thickness:=abs(ThiEdit.Value);
   UpDateLayName(LayList.ItemIndex);
  end;
end;

procedure TmForm.UpDateFrmClick(Sender: TObject);
begin
  tsk.FLays.Parse(FrmEdit.Text);
  UpDateLay;
end;

procedure TmForm.WvEditEnter(Sender: TObject);
begin
  if LayList.ItemIndex >= 0 then
  begin
   tsk.FLays.Lays.Items[LayList.ItemIndex].Wavelength:=abs(strtofloat(WvEdit.Text));
   UpDateLayName(LayList.ItemIndex);
  end;
end;

function TmForm.UpDateMat: boolean;
var j: integer;
begin
  Result:=true;
  MatList.Clear;
  MatSel.Clear;
  matmemo.Clear;
  ComMatMemo.Clear;
  NameMatMemo.Clear;
  n_Line.Clear;
  k_Line.Clear;
  for j:=0 to tsk.FMaterials.Count-1 do
   begin
   MatList.AddItem(tsk.FMaterials.Items[j].MaterialName, nil);
   MatSel.AddItem(tsk.FMaterials.Items[j].MaterialName, nil);
   end;
end;

function TmForm.UpDateLay: boolean;
var j: integer;
begin
  LayList.Clear;
  ThiEdit.Text:='-';
  OptThiEdit.Text:='-';
  WvEdit.Text:='-';
  MatSel.ItemIndex:=-1;
  for j:=0 to tsk.FLays.Lays.Count-1 do
   if tsk.FLays.Lays.Items[j].LinkMaterialIndex >=0 then
    LayList.AddItem(tsk.FLays.Lays.Items[j].LayName+' >> '+
    tsk.FMaterials.Items[tsk.FLays.Lays.Items[j].LinkMaterialIndex].MaterialName+
    ', '+floattostr(tsk.FLays.Lays.Items[j].Thickness)+' nm', nil)
   else LayList.AddItem(tsk.FLays.Lays.Items[j].LayName, nil);
end;

function TmForm.UpDatePro: boolean;
var j: integer;
begin
  StartWvl.Value:=0;
  EndWvl.Value:=0;
  PolarEdit.Value:=0;
  AngleEdit.Value:=0;
  TaskList.Clear;
  for j:=0 to tsk.FProblems.Count-1 do
   TaskList.AddItem(
    inttostr(j+1)+'. '+floattostr(tsk.FProblems.Items[j].StartWaveLength)+' - '+
    floattostr(tsk.FProblems.Items[j].EndWaveLength)+' nm, '+
    floattostr(tsk.FProblems.Items[j].Angle)+' rad, s/(s+p)= '+
    floattostr(tsk.FProblems.Items[j].Polarization),nil);
end;

procedure TmForm.UpDateProName(index: integer);
begin
  TaskList.Items[index]:=
    inttostr(index+1)+'. '+floattostr(tsk.FProblems.Items[index].StartWaveLength)+' - '+
    floattostr(tsk.FProblems.Items[index].EndWaveLength)+' nm, '+
    floattostr(tsk.FProblems.Items[index].Angle)+' rad, s/(s+p)= '+
    floattostr(tsk.FProblems.Items[index].Polarization)
end;

procedure TmForm.UpDateLayName(index: integer);
begin
  tsk.LayThiUpd(index);
  ThiEdit.Value := tsk.FLays.Lays.Items[index].Thickness;
  with tsk.FLays.Lays.Items[index] do
   if LinkMaterialIndex >=0 then
    LayList.Items[index]:=LayName+' >> '+
    tsk.FMaterials.Items[LinkMaterialIndex].MaterialName+
    ', '+floattostr(Thickness)+' nm'
   else LayList.Items[index]:=LayName;
end;

procedure TmForm.DrawSolv(index: integer);
var st: string;
    tl,rl: TLineSeries;
    cl: TColor;
    j: integer;
begin
  st:=floattostr(tsk.FProblems.Items[index].StartWaveLength)+' - '+
    floattostr(tsk.FProblems.Items[index].EndWaveLength)+' nm, '+
    floattostr(tsk.FProblems.Items[index].Angle)+' rad, s/(s+p)= '+
    floattostr(tsk.FProblems.Items[index].Polarization);
  cl:=RGBToColor(Random(150),Random(150),Random(150));

  with tsk.FProblems.Items[index].Solvation do
  begin
   tl:=TLineSeries.Create(T_Chart);
   tl.SeriesColor:=cl;
   tl.Title:=st;
   rl:=TLineSeries.Create(R_Chart);
   rl.SeriesColor:=cl;
   rl.Title:=st;
   for j:=0 to Count-1 do
    begin
     tl.AddXY(Items[j].W,Items[j].T);
     rl.AddXY(Items[j].W,Items[j].R);
    end;
   T_Chart.AddSeries(tl);
   R_Chart.AddSeries(rl);
  end;

end;

end.

