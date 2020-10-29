unit moptask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mopformula, mopmaterial, mopproblem, moptype, XMLRead,
  XMLWrite, DOM, complexes, mopan;

type

  { TMOPTask }

  TMOPTask = class
  private
    //----- ParseFile
    function MaterialParse(Doc: TDOMNode): boolean;
    function FormulaParse(Doc: TDOMNode): boolean;
    function LaysParse(Doc: TDOMNode): boolean;
    function ProblemParse(Doc: TDOMNode): boolean;
    //----- SaveFile
    function MaterialAdd(var xDoc: TXMLDocument; Materials: TIntegerPoints): boolean;
    function FormulaAdd(var xDoc: TXMLDocument): boolean;
    function LaysAdd(var xDoc: TXMLDocument): boolean;
    function ProblemAdd(var xDoc: TXMLDocument): boolean;
  public
    FLays: TMOPFormula;
    FProblems: TMOPProblems;
    FMaterials: TMOPMaterials;
    constructor Create;
    destructor Destroy; override;
    function DeleteMaterial( Index: Integer ): boolean;
    function Solve(ProblemNumber: integer): boolean;
    function LoadFromXML(FileName: string; MaterialsOnly: boolean): boolean;
    function SaveToXML(FileName: string; JustMaterials: boolean): boolean;
    function ExportResult(FileName: string; Problem: integer): boolean;
    function LayThiUpd( LayIndex: Integer): boolean;
    function SortMatByName: boolean;
  end;

implementation

{ TMOPTask }

function TMOPTask.MaterialParse(Doc: TDOMNode): boolean;
var
  mat, matopt, pnt: TDOMNode;
  vm: TMOPMaterial;
  nkp: TNKPoint;
  n, k: real;
begin
  Result := True;
  mat := Doc.FindNode('materials');
  if Assigned(mat) then
    mat := mat.FindNode('material');
  while Assigned(mat) do
  begin
    vm := TMOPMaterial.Create;

    matopt := mat.FindNode('name');
    if (Assigned(matopt)) and (length(matopt.TextContent) > 0) then
      vm.MaterialName := Trim({UTF8Encode(}matopt.TextContent){)}
    else
      vm.MaterialName := 'Noname-' + IntToStr(FMaterials.Count);

    matopt := mat.FindNode('comment');
    if Assigned(matopt) then
      vm.Comment := Trim({UTF8Encode(}matopt.TextContent){)};

    matopt := mat.FindNode('points');
    if Assigned(matopt) then
      matopt := matopt.FindNode('point');
    while Assigned(matopt) do
    begin
      nkp := TNKPoint.Create;

      pnt := matopt.FindNode('w');
      if Assigned(pnt) then
      begin
        nkp.wln := strtofloat(pnt.TextContent);
        n := 1;
        k := 0;
        pnt := matopt.FindNode('n');
        if Assigned(pnt) then
          n := strtofloat(pnt.TextContent);
        pnt := matopt.FindNode('k');
        if Assigned(pnt) then
          k := strtofloat(pnt.TextContent);
        nkp.eps := (n + _i * k) ** 2;
        vm.Points.Add(nkp);
      end;

      matopt := matopt.NextSibling;
    end;

    if FMaterials.FindByName(vm.MaterialName) < 0 then //Why, i don't remember
    FMaterials.Add(vm);

    mat := mat.NextSibling;
  end;
end;

function TMOPTask.FormulaParse(Doc: TDOMNode): boolean;
var
  frml, opt: TDOMNode;
  bl: TMOPBlock;
  st: string;
  j: integer;
begin
  Result := True;
  frml := Doc.FindNode('formula');
  if Assigned(frml) then
    frml := frml.FindNode('block');
  FLays.Blocks.Clear;
  while Assigned(frml) do
  begin
    bl := TMOPBlock.Create;

    opt := frml.FindNode('repetition');
    if Assigned(opt) then
      bl.Repetition := StrToInt(opt.TextContent);

    opt := frml.FindNode('lays');
    if Assigned(opt) then
      opt := opt.FindNode('lay');
    while Assigned(opt) do
    begin
      st := Trim({UTF8Encode(}opt.TextContent);
      bl.LayIndexes.Add(-1);
      for j := 0 to FLays.Lays.Count - 1 do
        if st = FLays.Lays.Items[j].LayName then
        begin
          bl.LayIndexes.Items[bl.LayIndexes.Count - 1] := j;
        end;
      opt := opt.NextSibling;
    end;

    FLays.Blocks.Add(bl);
    frml := frml.NextSibling;
  end;

end;

function TMOPTask.LaysParse(Doc: TDOMNode): boolean;
var
  ls, opt: TDOMNode;
  lay: TMOPLay;
begin
  Result := True;
  ls := Doc.FindNode('lays');
  if Assigned(ls) then
    ls := ls.FindNode('lay');
  FLays.Lays.Clear;
  while Assigned(ls) do
  begin
    lay := TMOPLay.Create;

    opt := ls.FindNode('alias');
    if Assigned(opt) then
      lay.LayName := Trim({UTF8Encode(}opt.TextContent);

    opt := ls.FindNode('thickness');
    if Assigned(opt) then
      lay.Thickness := strtofloat(opt.TextContent);

    opt := ls.FindNode('material');
    if Assigned(opt) then
      lay.MaterialName := Trim({UTF8Encode(}opt.TextContent);

    opt := ls.FindNode('otic_thickness');
    if Assigned(opt) then
      lay.OpticThickness := strtofloat(opt.TextContent);

    opt := ls.FindNode('wavelength');
    if Assigned(opt) then
      lay.Wavelength := strtofloat(opt.TextContent);

    FLays.Lays.Add(lay);
    ls := ls.NextSibling;
  end;
end;

function TMOPTask.ProblemParse(Doc: TDOMNode): boolean;
var
  prob, opt: TDOMNode;
  pr: TMOPProblem;
begin
  Result := True;
  prob := Doc.FindNode('problems');
  if Assigned(prob) then
    prob := prob.FindNode('problem');
  FProblems.Clear;
  while Assigned(prob) do
  begin
    pr := TMOPProblem.Create;

    opt := prob.FindNode('range_start');
    if Assigned(opt) then
      pr.StartWaveLength := strtofloat(opt.TextContent);

    opt := prob.FindNode('range_end');
    if Assigned(opt) then
      pr.EndWaveLength := strtofloat(opt.TextContent);

    opt := prob.FindNode('step_count');
    if Assigned(opt) then
      pr.Steps := StrToInt(opt.TextContent);

    opt := prob.FindNode('angle');
    if Assigned(opt) then
      pr.Angle := strtofloat(opt.TextContent);

    opt := prob.FindNode('polarization');
    if Assigned(opt) then
      pr.Polarization := strtofloat(opt.TextContent);

    FProblems.Add(pr);
    prob := prob.NextSibling;
  end;
end;

function TMOPTask.MaterialAdd(var xDoc: TXMLDocument;
  Materials: TIntegerPoints): boolean;
var
  nd, td, tm, tk, tv, tp, tp_nk: TDOMNode;
  j, i: integer;
begin
  Result := True;
  nd := xDoc.DocumentElement;
  td := xDoc.CreateElement('materials');
  for j := 0 to Materials.Count - 1 do
  begin
    tm := xDoc.CreateElement('material');

    tk := xDoc.CreateElement('name');
    tv := xDoc.CreateTextNode(FMaterials.Items[Materials.Items[j]].MaterialName);
    tk.AppendChild(tv);
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('comment');
    tv := xDoc.CreateTextNode(FMaterials.Items[Materials.Items[j]].Comment);
    tk.AppendChild(tv);
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('points');
    i := 0;
    while (i < FMaterials.Items[Materials.Items[j]].Points.Count) do
    begin
      tp := xDoc.CreateElement('point');

      tp_nk := xDoc.CreateElement('w');
      tp_nk.AppendChild(xDoc.CreateTextNode(floattostr(
        FMaterials.Items[Materials.Items[j]].Points.Items[i].wln)));
      tp.AppendChild(tp_nk);

      tp_nk := xDoc.CreateElement('n');
      tp_nk.AppendChild(xDoc.CreateTextNode(floattostr(
        (csqrt(FMaterials.Items[Materials.Items[j]].Points.Items[i].eps)).re)));
      tp.AppendChild(tp_nk);

      tp_nk := xDoc.CreateElement('k');
      tp_nk.AppendChild(xDoc.CreateTextNode(floattostr(
        (csqrt(FMaterials.Items[Materials.Items[j]].Points.Items[i].eps)).im)));
      tp.AppendChild(tp_nk);

      tk.AppendChild(tp);
      Inc(i);
    end;
    tm.AppendChild(tk);

    td.AppendChild(tm);
  end;
  nd.AppendChild(td);
end;

function TMOPTask.FormulaAdd(var xDoc: TXMLDocument): boolean;
var
  nd, td, tm, tk, tp: TDOMNode;
  j, m: integer;
begin
  Result := True;
  nd := xDoc.DocumentElement;
  td := xDoc.CreateElement('formula');
  for j := 0 to FLays.Blocks.Count - 1 do
  begin
    tm := xDoc.CreateElement('block');

    tk := xDoc.CreateElement('repetition');
    tk.AppendChild(xDoc.CreateTextNode(IntToStr(
      FLays.Blocks.Items[j].Repetition)));
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('lays');
    m := 0;
    while (m < FLays.Blocks.Items[j].LayIndexes.Count) do
    begin
      tp := xDoc.CreateElement('lay');
      tp.AppendChild(xDoc.CreateTextNode(
        FLays.Lays.Items[FLays.Blocks.Items[j].LayIndexes.Items[m]].LayName));
      tk.AppendChild(tp);
      Inc(m);
    end;
    tm.AppendChild(tk);

    td.AppendChild(tm);
  end;
  nd.AppendChild(td);
end;

function TMOPTask.LaysAdd(var xDoc: TXMLDocument): boolean;
var
  nd, td, tm, tk: TDOMNode;
  j: integer;
begin
  Result := True;
  nd := xDoc.DocumentElement;
  td := xDoc.CreateElement('lays');
  for j := 0 to FLays.Lays.Count - 1 do
  begin
    tm := xDoc.CreateElement('lay');

    tk := xDoc.CreateElement('alias');
    tk.AppendChild(xDoc.CreateTextNode(FLays.Lays.Items[j].LayName));
    tm.AppendChild(tk);

    if FLays.Lays.Items[j].LinkMaterialIndex >= 0 then
    begin
     tk := xDoc.CreateElement('material');
     tk.AppendChild(xDoc.CreateTextNode(
          FMaterials.Items[FLays.Lays.Items[j].LinkMaterialIndex].MaterialName));
     tm.AppendChild(tk);
    end;

    tk := xDoc.CreateElement('thickness');
    tk.AppendChild(xDoc.CreateTextNode(floattostr(
      FLays.Lays.Items[j].Thickness)));
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('otic_thickness');
    tk.AppendChild(xDoc.CreateTextNode(floattostr(
      FLays.Lays.Items[j].OpticThickness)));
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('wavelength');
    tk.AppendChild(xDoc.CreateTextNode(floattostr(
      FLays.Lays.Items[j].Wavelength)));
    tm.AppendChild(tk);

    td.AppendChild(tm);
  end;
  nd.AppendChild(td);
end;

function TMOPTask.ProblemAdd(var xDoc: TXMLDocument): boolean;
var
  nd, td, tm, tk: TDOMNode;
  j: integer;
begin
  Result := True;
  nd := xDoc.DocumentElement;
  td := xDoc.CreateElement('problems');
  for j := 0 to FProblems.Count - 1 do
  begin
    tm := xDoc.CreateElement('problem');

    tk := xDoc.CreateElement('range_start');
    tk.AppendChild(xDoc.CreateTextNode(floattostr(
      FProblems.Items[j].StartWaveLength)));
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('range_end');
    tk.AppendChild(xDoc.CreateTextNode(floattostr(
      FProblems.Items[j].EndWaveLength)));
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('step_count');
    tk.AppendChild(xDoc.CreateTextNode(floattostr(
      FProblems.Items[j].Steps)));
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('angle');
    tk.AppendChild(xDoc.CreateTextNode(floattostr(
      FProblems.Items[j].Angle)));
    tm.AppendChild(tk);

    tk := xDoc.CreateElement('polarization');
    tk.AppendChild(xDoc.CreateTextNode(floattostr(
      FProblems.Items[j].Polarization)));
    tm.AppendChild(tk);

    td.AppendChild(tm);
  end;
  nd.AppendChild(td);
end;

constructor TMOPTask.Create;
begin
  FLays := TMOPFormula.Create;
  FProblems := TMOPProblems.Create;
  FMaterials := TMOPMaterials.Create;
end;

destructor TMOPTask.Destroy;
begin
  FLays.Free;
  FMaterials.Free;
  FProblems.Free;
  inherited Destroy;
end;

function TMOPTask.DeleteMaterial(Index: Integer): boolean;
var j: integer;
begin
  if Index < FMaterials.Count then
  begin
   for j:=0 to FLays.Lays.Count-1 do
    begin
     if FLays.Lays.Items[j].LinkMaterialIndex = Index then
      FLays.Lays.Items[j].LinkMaterialIndex := -1;
     if FLays.Lays.Items[j].LinkMaterialIndex > Index then
      inc(FLays.Lays.Items[j].LinkMaterialIndex);
    end;
   FMaterials.Delete(Index);
   Result:=true;
  end else
   Result:=false;
end;

function TMOPTask.Solve(ProblemNumber: integer): boolean;
var
  rs: complex;
  j: integer;
  w, dw: real;
  ls, mt: TIntegerPoints;
  dd: TRealPoints;
  val: TMopRT;
begin
  Result := True;

  if assigned(FProblems.Items[ProblemNumber].Solvation) then
    FProblems.Items[ProblemNumber].Solvation.Clear
  else
    FProblems.Items[ProblemNumber].Solvation := TMopRTs.Create;

  ls := FLays.LaysOrder;
  dd := FLays.GetThickness(ls);
  mt := FLays.GetMaterials(ls);

  w := FProblems.Items[ProblemNumber].StartWaveLength;
  if FProblems.Items[ProblemNumber].Steps > 1 then
    dw := (FProblems.Items[ProblemNumber].EndWaveLength -
      FProblems.Items[ProblemNumber].StartWaveLength) /
      (FProblems.Items[ProblemNumber].Steps - 1)
  else
    dw := 0;

  for j := 0 to FProblems.Items[ProblemNumber].Steps - 1 do
  begin
    rs := TRTsp.FullEvalution(FMaterials.GetEpsToWave(mt, w),
      dd, w, FProblems.Items[ProblemNumber].Angle,
      FProblems.Items[ProblemNumber].Polarization);
    val.R := rs.im;
    val.T := rs.re;
    val.W := w;
    FProblems.Items[ProblemNumber].Solvation.Add(val);
    w := w + dw;
  end;

  ls.Free;
  mt.Free;
end;

function TMOPTask.SaveToXML(FileName: string; JustMaterials: boolean): boolean;
var
  xDoc: TXMLDocument;
  nd: TDOMNode;
  lst: TIntegerPoints;
  j: integer;
begin
  xDoc := TXMLDocument.Create;
  nd := xDoc.CreateElement('mopdocument');
  xDoc.Appendchild(nd);
  //------------------------
  lst := TIntegerPoints.Create;
  if not JustMaterials then
  begin
    FormulaAdd(xDoc);
    LaysAdd(xDoc);
    ProblemAdd(xDoc);
    for j := 0 to FLays.Lays.Count - 1 do
      if (FLays.Lays.Items[j].LinkMaterialIndex >= 0) and
        (lst.IndexOf(FLays.Lays.Items[j].LinkMaterialIndex) < 0) then
        lst.Add(FLays.Lays.Items[j].LinkMaterialIndex);
  end
  else
  begin
    for j := 0 to FMaterials.Count - 1 do
      lst.Add(j);
  end;
  MaterialAdd(xDoc, lst);
  lst.Free;
  //------------------------
  writeXMLFile(xDoc, FileName);
  xDoc.Free;
  Result := True;
end;

function TMOPTask.ExportResult(FileName: string; Problem: integer): boolean;
var
  str: TStringList;
  j, i: integer;
begin
  if (Problem < FProblems.Count) and
    (assigned(FProblems.Items[Problem].Solvation)) then
    Result := True
  else
  begin
    Result := False;
    exit;
  end;
  str := TStringList.Create;
  str.Append('; This is a MOP document containing results of evaluation');
  str.Append('; Current date is ' + DateTimeToStr(Now));
  str.Append('; ----------------------------------------------');
  str.Append('; Formula is ');
  for j := 0 to FLays.Blocks.Count - 1 do
  begin
    str.Strings[str.Count - 1] :=
      str.Strings[str.Count - 1] + ' ' + IntToStr(FLays.Blocks.Items[j].Repetition);
    for i := 0 to FLays.Blocks.Items[j].LayIndexes.Count - 1 do
      str.Strings[str.Count - 1] := str.Strings[str.Count - 1] + '-' +
        FLays.Lays.Items[FLays.Blocks.Items[j].LayIndexes.Items[i]].LayName;
  end;
  str.Append(';');
  str.Append('; Lays are ');
  for j := 0 to FLays.Lays.Count - 1 do
  begin
    str.Append(';  Alias             ' + FLays.Lays.Items[j].LayName);
    str.Append(';  Material          ' + FLays.Lays.Items[j].MaterialName);
    str.Append(';  Thickness         ' + floattostr(FLays.Lays.Items[j].Thickness) + ' nm');
    if FLays.Lays.Items[j].Wavelength > 0 then
    begin
      str.Append(';  OpticThickness    ' + floattostr(
        FLays.Lays.Items[j].OpticThickness) + ' nm');
      str.Append(';  Wavelength        ' + floattostr(
        FLays.Lays.Items[j].Wavelength) + ' nm');
    end;
    str.Append(';');
  end;
  str.Append('; Task is');
  str.Append(';   Wavelength range is ' + floattostr(
    FProblems.Items[Problem].StartWaveLength) + ' -- ' +
    floattostr(FProblems.Items[Problem].EndWaveLength) + ' nm');
  str.Append(';   Angle is ' + floattostr(FProblems.Items[Problem].Angle));
  str.Append(';   Proportion p polarization field E is ' +
    floattostr(FProblems.Items[Problem].Polarization));
  str.Append(';');
  str.Append('; ----------------------------------------------');
  str.Append('; Wavelength,nm     Reflectance     Transmittance');
  str.Append(';');
  for j := 0 to FProblems.Items[Problem].Solvation.Count - 1 do
    str.Append(floattostr(FProblems.Items[Problem].Solvation.Items[j].W) + '   ' +
      floattostr(FProblems.Items[Problem].Solvation.Items[j].R) + '   ' +
      floattostr(FProblems.Items[Problem].Solvation.Items[j].T));
  str.SaveToFile(FileName);
  str.Free;
end;

function TMOPTask.LayThiUpd(LayIndex: Integer): boolean;
begin
  FLays.FindOutLayThickness(FMaterials, LayIndex);
end;

function TMOPTask.SortMatByName: boolean;
begin

end;

function TMOPTask.LoadFromXML(FileName: string; MaterialsOnly: boolean): boolean;
var
  Doc: TXMLDocument;
begin
  Result := True;

  ReadXMLFile(Doc, FileName);
  if Doc.DocumentElement.NodeName <> 'mopdocument' then
  begin
    Result := False;
    Exit;
  end;

  Result := Result and MaterialParse(Doc.DocumentElement);
  if not MaterialsOnly then
  begin
    Result := Result and LaysParse(Doc.DocumentElement);
    FLays.LinkToMaterialsByName(FMaterials);
    FLays.FindOutThickness(FMaterials);
    Result := Result and FormulaParse(Doc.DocumentElement);
    Result := Result and ProblemParse(Doc.DocumentElement);
  end;

  Doc.Free;
end;

end.
