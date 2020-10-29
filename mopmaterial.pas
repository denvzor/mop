unit mopmaterial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, complexes, moptype, math;

type

  { TNKPoint }

  TNKPoint = class
  public
    eps: complex;  // квадрат показателя преломления
    wln: real;     // волны
    constructor Create;
  end;

  TNKPoints = specialize TFPGList<TNKPoint>;

  { TMOPMaterial }

  TMOPMaterial = class  // матриал
  public
    MaterialName: string;
    Comment: string;
    Points: TNKPoints;
    constructor Create;
    destructor Destroy; override;
  end;

  TMOPMaterialArray = specialize TFPGList<TMOPMaterial>;

  { TMOPMaterials }

  TMOPMaterials = class (TMOPMaterialArray)
  private
    function EpsToWave(Material: Integer; Wvlng: real): Complex;
  public
    function FindByName(MaterialName: string): integer;
    function MinRange(Materials: TIntegerPoints): real;
    function MaxRange(Materials: TIntegerPoints): real;
    function GetEpsToWave(Materials: TIntegerPoints; WaveLength: real): TComplexPoints;
  end;

implementation

{ TMOPMaterials }

function TMOPMaterials.EpsToWave(Material: Integer; Wvlng: real): Complex;
var j: integer;
    part: real;
begin
  for j:=0 to Items[Material].Points.Count - 2 do
   if (Wvlng >= Items[Material].Points.Items[j].wln) and
      (Wvlng <= Items[Material].Points.Items[j+1].wln) then
       begin
        part:=(Wvlng - Items[Material].Points.Items[j].wln)/
              (Items[Material].Points.Items[j+1].wln -
                       Items[Material].Points.Items[j].wln);
        result:=Items[Material].Points.Items[j].eps +
                part*(Items[Material].Points.Items[j+1].eps -
                       Items[Material].Points.Items[j].eps);
        break;
       end;
end;

function TMOPMaterials.FindByName(MaterialName: string): integer;
var j: integer;
begin
  Result:=-1;
  for j:=0 to Count-1 do
   if Items[j].MaterialName = MaterialName then
       begin
        Result:=j;
        Break;
       end;
end;

function TMOPMaterials.MinRange(Materials: TIntegerPoints): real;
var j: integer;
begin
  Result:=-1;
  for j:=0 to Materials.Count-1 do
   if j = 0 then
    Result:= Items[Materials.Items[j]].Points.Items[0].wln
   else
    Result:= Min(Result,Items[Materials.Items[j]].Points.Items[0].wln);
end;

function TMOPMaterials.MaxRange(Materials: TIntegerPoints): real;
var j: integer;
begin
  Result:=-1;
  for j:=0 to Materials.Count-1 do
   if j = 0 then
    Result:= Items[Materials.Items[j]].Points.Items[0].wln
   else
    Result:= Max(Result,Items[Materials.Items[j]].Points.Items[0].wln);
end;

function TMOPMaterials.GetEpsToWave(Materials: TIntegerPoints; WaveLength: real
  ): TComplexPoints;
var j: integer;
begin
  Result:=TComplexPoints.Create;
  for j:=0 to Materials.Count-1 do
   Result.Add( EpsToWave(Materials.Items[j],WaveLength) );
end;

{ TNKPoint }

constructor TNKPoint.Create;
begin
  eps:=0;
  wln:=0;
end;

{ TMOPMaterial }

constructor TMOPMaterial.Create;
begin
  MaterialName:='';
  Comment:='';
  Points:=TNKPoints.Create;
end;

destructor TMOPMaterial.Destroy;
begin
  Points.Free;
  inherited Destroy;
end;

end.

