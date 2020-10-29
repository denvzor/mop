unit mopformula;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, mopmaterial, moptype, complexes;

type

  { TMOPLay }

  TMOPLay = class   // характеристики слоя
  public
    LayName: string;
    LinkMaterialIndex: integer;
    MaterialName: string;
    Thickness: real;
    OpticThickness: real;
    Wavelength: real;
    constructor Create;
  end;

  TMOPLays = specialize TFPGList<TMOPLay>;

  { TMOPBlock }

  TMOPBlock = class
  public
    Repetition: integer;
    LayIndexes: TIntegerPoints;
    constructor Create;
    destructor Destroy; override;
  end;

  TMOPBlocks = specialize TFPGList<TMOPBlock>;

  { TMOPFormula }

  TMOPFormula = class
  public
    Blocks: TMOPBlocks;
    Lays: TMOPLays;
    constructor Create;
    destructor Destroy; override;
    function Parse(InText: string): boolean;
    function LinkToMaterialsByName(const Materials: TMOPMaterials): integer;
    function UpdateMaterialIndex(const Materials: TMOPMaterials; LayNumber: Integer; MatIndex: Integer): boolean;
    function FindOutThickness(const Materials: TMOPMaterials): boolean;
    function FindOutLayThickness(const Materials: TMOPMaterials; LayIndex: integer): boolean;
    function LaysOrder: TIntegerPoints;
    function GetFormula: string;
    function GetThickness(Materials: TIntegerPoints): TRealPoints;
    function GetMaterials(Materials: TIntegerPoints): TIntegerPoints;
    function IsLayExist(const InLays:TMOPLays; layname: string): integer;
  end;

implementation

{ TMOPLay }

constructor TMOPLay.Create;
begin
  LayName:='';
  MaterialName:='';
  LinkMaterialIndex:=-1; // не связан с материалами
  Thickness:=0;
  OpticThickness:=0;
  Wavelength:=0;
end;

{ TMOPBlock }

constructor TMOPBlock.Create;
begin
  Repetition:=0;
  LayIndexes:=TIntegerPoints.Create;
end;

destructor TMOPBlock.Destroy;
begin
  LayIndexes.Free;
  inherited Destroy;
end;

{ TMOPFormula }

constructor TMOPFormula.Create;
begin
  Blocks:=TMOPBlocks.Create;
  Lays:=TMOPLays.Create;
end;

destructor TMOPFormula.Destroy;
begin
  Blocks.Free;
  Lays.Free;
  inherited Destroy;
end;

function TMOPFormula.Parse(InText: string): boolean;
var i,j,k: integer;
    txt: string;
    blo, cob: TStringList;
    bltxt: boolean;
    oldls: TMOPLays;

const chn = ['0'..'9'];
      ltsbl = ['a'..'z','-',':'];
begin
  Result:=true;

  txt:=Trim(LowerCase(InText));
  blo:=TStringList.Create;
  cob:=TStringList.Create;

  // Разделение на блоки
  blo.Add('');
  cob.Add('1');
  bltxt:= true;
  for i:=1 to Length(txt) do
   begin
    if not((txt[i] in chn) xor (bltxt)) then
    begin
      bltxt := not bltxt;
      if not bltxt then
      begin
       blo.Append('');
       cob.Append('');
      end;
    end;
    if bltxt then
    begin
     if (txt[i] in ltsbl) then
      blo.Strings[blo.Count-1]:= blo.Strings[blo.Count-1]+txt[i];
    end else begin
     cob.Strings[cob.Count-1]:= cob.Strings[cob.Count-1]+txt[i];
    end;
   end;

  // Удаление пустых блоков
  i:=0;
  while ( i <= blo.Count-1 ) do
   begin
    if blo.Strings[i] = '' then
    begin
     blo.Delete(i);
     cob.Delete(i);
    end;
    inc(i);
   end;

  //Перезапись блоков
  Blocks.Clear;
  for i:=0 to cob.Count-1 do
  begin
   Blocks.Add(TMOPBlock.Create);
   txt:=cob.Strings[i];
   Blocks.Last.Repetition:=strtoint(txt);
  end;

  // Копирование старого списка слоев и создане нового списка слоев
  oldls:=Lays;
  Lays:=TMOPLays.Create;

  //Проход по блокам и парсинг слоев
  for j:=0 to blo.Count-1 do
  begin
   txt:=blo.Strings[j];
   for i:=1 to length(txt) do
    if (txt[i]='-') or (txt[i]=':') then txt[i]:=#13;

   // Делаем парсинг формулы
   cob.Text:=txt;
   // Удаляем пустые элементы
   i:=0;
   while ( i <= cob.Count-1 ) do
    begin
     if cob.Strings[i]='' then cob.Delete(i);
     inc(i);
    end;

   // Обработка слоев
   for i:=0 to cob.Count-1 do
    begin
     k := IsLayExist(Lays,cob.Strings[i]);
     if k < 0 then
     begin
      k := IsLayExist(oldls,cob.Strings[i]);
      if k < 0 then
      begin
       Lays.Add(TMOPLay.Create);
       Lays.Last.LayName:=cob.Strings[i];
      end
      else begin
       lays.Add(oldls.Items[k]);
       oldls.Delete(k);
      end;
      Blocks.Items[j].LayIndexes.Add(Lays.Count-1);
     end else begin
      Blocks.Items[j].LayIndexes.Add(k);
     end;
    end;
  end;

  oldls.Free;
  blo.Free;
  cob.Free;

end;

function TMOPFormula.LinkToMaterialsByName(const Materials: TMOPMaterials
  ): integer;
var j: integer;
begin
  Result:=-1;
  for j:=0 to Lays.Count-1 do
   begin
    Lays.Items[j].LinkMaterialIndex:=
                     Materials.FindByName( Lays.Items[j].MaterialName );
    if Lays.Items[j].LinkMaterialIndex < 0 then
     begin
       Result:=j;
       Break;
     end;
   end;
end;

function TMOPFormula.UpdateMaterialIndex(const Materials: TMOPMaterials;
  LayNumber: Integer; MatIndex: Integer): boolean;
begin
    If (LayNumber <  Lays.Count) and (LayNumber >= 0) then
   begin
        Lays.Items[LayNumber].LinkMaterialIndex := MatIndex;
        Lays.Items[LayNumber].MaterialName:=
        Materials.Items[MatIndex].MaterialName;
   end;
end;


function TMOPFormula.FindOutThickness(const Materials: TMOPMaterials): boolean;
var j: integer;
begin
  Result:=true;
  for j:=0 to Lays.Count-1 do
   FindOutLayThickness(Materials, j);
end;

function TMOPFormula.FindOutLayThickness(const Materials: TMOPMaterials;
  LayIndex: integer): boolean;
var int: TIntegerPoints;
    cmp: TComplexPoints;
begin
    result:=true;
    if (Lays.Items[LayIndex].Wavelength > 0)
       and (Lays.Items[LayIndex].OpticThickness > 0)
       and (Lays.Items[LayIndex].LinkMaterialIndex >= 0) then
    begin
     int := TIntegerPoints.Create;
     int.Add(Lays.Items[LayIndex].LinkMaterialIndex);
     cmp := Materials.GetEpsToWave(int, Lays.Items[LayIndex].Wavelength);
     Lays.Items[LayIndex].Thickness:=Lays.Items[LayIndex].OpticThickness/
                 (csqrt(cmp.Items[cmp.Count-1])).re;
    end;
end;

function TMOPFormula.LaysOrder: TIntegerPoints;
var j,h,k: integer;
begin
    Result:=TIntegerPoints.Create;
    for j:=0 to Blocks.Count-1 do
     for h:=0 to Blocks.Items[j].Repetition-1 do
      for k:=0 to Blocks.Items[j].LayIndexes.Count-1 do
       Result.Add(Blocks.Items[j].LayIndexes.Items[k]);
end;

function TMOPFormula.GetFormula: string;
var j,i: integer;
begin
   result:='';
   for j:=0 to Blocks.Count-1 do
    begin
     result:=result+inttostr(Blocks.Items[j].Repetition);
     for i:=0 to Blocks.Items[j].LayIndexes.Count-1 do
      if Blocks.Items[j].LayIndexes.Items[i] >=0 then
       result:=result+':'+Lays.Items[Blocks.Items[j].LayIndexes.Items[i]].LayName
      else result:=result+':x';
    end;
end;

function TMOPFormula.GetThickness(Materials: TIntegerPoints): TRealPoints;
var j: integer;
begin
   Result:=TRealPoints.Create;
   for j:=0 to Materials.Count-1 do
    begin
     Result.Add( Lays.Items[Materials.Items[j]].Thickness );
    end;
end;

function TMOPFormula.GetMaterials(Materials: TIntegerPoints): TIntegerPoints;
var j: integer;
begin
   Result:=TIntegerPoints.Create;
   for j:=0 to Materials.Count-1 do
    begin
     Result.Add( Lays.Items[ Materials.Items[j] ].LinkMaterialIndex );
    end;
end;

function TMOPFormula.IsLayExist(const InLays: TMOPLays; layname: string): integer;
var i: integer;
begin
   result:=-1;
   if assigned(InLays) then
   begin
    for i:=0 to InLays.Count-1 do
     if InLays.Items[i].LayName = layname then
     begin
      result:=i;
      break;
     end;
   end;
end;

end.

