unit mopan;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, complexes, fgl, moptype;

type
  TReals = array of real;
  TComplexes = array of Complex;
  Com2Mat = array [1..2, 1..2] of Complex;

  { TEH }

  TEH = record
    TEs: Complex;
    REs: Complex;
    THp: Complex;
    RHp: Complex;
  end;

  { TRTsp }

  TRTsp = class(TObject)
  private
    class function rtmt(k1, k2, e1, e2: complex; d: real): Com2Mat;
    class function kz(es: TComplexes; wavelng: real; ang: real): TComplexes;
    class function rtE(k: TComplexes; d: TReals): TEH;
    class function rtH(k, e: TComplexes; d: TReals): TEH;
  public
    class function Evalution(_es: TComplexPoints;
      _ds: TRealPoints; wavelng: real;
      ang: real): TEH;
    class function FullEvalution(_es: TComplexPoints;
      _ds: TRealPoints; wavelng: real;
      ang: real; polar: real): Complex;
  end;

function mlt(m1, m2: Com2Mat): Com2Mat;     // перемножение матриц
function e_Com2Mat: Com2Mat;                    // еденичная матрица

implementation

function mlt(m1, m2: Com2Mat): Com2Mat;
begin
  Result[1, 1] := m1[1, 1] * m2[1, 1] + m1[1, 2] * m2[2, 1];
  Result[1, 2] := m1[1, 1] * m2[1, 2] + m1[1, 2] * m2[2, 2];
  Result[2, 1] := m1[2, 1] * m2[1, 1] + m1[2, 2] * m2[2, 1];
  Result[2, 2] := m1[2, 1] * m2[1, 2] + m1[2, 2] * m2[2, 2];
end;

function e_Com2Mat: Com2Mat;
begin
  Result[1, 1] := 1;
  Result[1, 2] := 0;
  Result[2, 1] := 0;
  Result[2, 2] := 1;
end;

class function TRTsp.rtmt(k1, k2, e1, e2: complex; d: real): Com2Mat;
var
  ph1, ph2, kn: complex;
begin
  // формирование трансфер-матрицы для какой-то границы
  ph1 := cexp(_i * (k2 - k1) * d);
  ph2 := cexp(_i * (k2 + k1) * d);
  kn := (k2 * e1) / (k1 * e2);
  Result[1, 1] := 0.5 * (1 + kn) * ph1;
  Result[1, 2] := 0.5 * (1 - kn) / ph2;
  Result[2, 1] := 0.5 * (1 - kn) * ph2;
  Result[2, 2] := 0.5 * (1 + kn) / ph1;
end;

class function TRTsp.kz(es: TComplexes; wavelng: real; ang: real): TComplexes;
var
  tk: complex;
  j: integer;
  mlt: real;
begin
  setlength(Result, length(es));
  if length(es) > 0 then
  begin
    mlt := 2 * Pi / wavelng;
    Result[0] := mlt * cos(ang) * csqrt(es[0]);
    // номальная компонента падающего k
    for j := 1 to length(es) - 1 do
    begin
      tk := es[j] - es[0] * sqr(sin(ang));
      // квадрат нормальной компоненты k
      if (tk.im < 0) and (tk.re < 0) then // условие на физичность k
        tk := -1 * csqrt(tk)
      else
        tk := csqrt(tk);
      Result[j] := mlt * tk;
    end;
  end;
end;

class function TRTsp.rtE(k: TComplexes; d: TReals): TEH;
  // по полю E, дает s-поляризацию
var
  j: integer;
  mt: Com2Mat;
  amp: complex;
begin
  mt := e_Com2Mat; // еденичная матрица
  for j := 0 to length(k) - 2 do
    mt := mlt(mt, rtmt(k[j], k[j + 1], 1, 1, d[j]));
  // перемножение матриц

  amp := mt[2, 1] / mt[1, 1]; // отражение по амплитуде
  amp := amp * cong(amp);   // вычисление квадрата по модулю
  Result.REs := amp.re;   // запись отражения

  amp := 1 / mt[1, 1];       // прохождение по амплитуде
  amp := amp * cong(amp);   // вычисление квадрата по модулю
  amp := amp * exp(-2 * (k[length(k) - 1].im) * (d[length(d) - 1]));
  amp := ((k[length(k) - 1] / k[0]).re) * amp;  // прохождение по модулю
  Result.TEs := amp.re;

  Result.RHp := 0;
  Result.THp := 0;
end;

class function TRTsp.rtH(k, e: TComplexes; d: TReals): TEH;
  // по полю H, дает p-поляризацию
var
  j: integer;
  mt: Com2Mat;
  amp: complex;
begin
  mt := e_Com2Mat;
  for j := 0 to length(k) - 2 do
    mt := mlt(mt, rtmt(k[j], k[j + 1], e[j], e[j + 1], d[j]));

  amp := mt[2, 1] / mt[1, 1];
  amp := amp * cong(amp);
  Result.RHp := amp.re;

  amp := 1 / mt[1, 1];
  amp := amp * cong(amp);
  amp := amp * exp(-2 * (k[length(k) - 1].im) * (d[length(d) - 1]));
  amp := (((k[length(k) - 1] * e[0]) / (k[0] * e[length(e) - 1])).re) * amp;
  Result.THp := amp.re;

  Result.REs := 0;
  Result.TEs := 0;
end;

class function TRTsp.Evalution(_es: TComplexPoints;
  _ds: TRealPoints; wavelng: real;
  ang: real): TEH;
var
  rt: TEH;
  j: integer;
  ks: TComplexes;
  es: TComplexes;
  ds: TReals;
  sm: real;
begin
  // инициализация результата
  Result.REs := 0;
  Result.TEs := 0;
  Result.RHp := 0;
  Result.THp := 0;

  // убрать
  SetLength(es, _es.Count);
  for j := 0 to _es.Count - 1 do
    es[j] := _es.Items[j];
  SetLength(ds, _ds.Count - 1);
  sm := 0;
  for j := 0 to _ds.Count - 2 do
  begin
    ds[j] := sm;
    sm := sm + _ds.Items[j + 1];
  end;

  if (length(es) > 1) and  // есть хоть одна граница
    (length(ds) = length(es) - 1) and
    // коллчиество координат соответствует
    (abs(ang) < 0.5 * Pi) then // угол лежит в нужном диапазоне
  begin
    for j := 0 to length(ds) - 1 do
      if (j > 0) and (ds[j] < ds[j - 1]) then
        exit; // если слои не по порядку
    ks := kz(es, wavelng, abs(ang)); // вычисление kz
    rt := rtE(ks, ds);
    // вычисление парметров s-поляризации по полю E
    Result.REs := rt.REs;
    Result.TEs := rt.TEs;
    rt := rtH(ks, es, ds);
    // вычисление парметров p-поляризации по полю H
    Result.RHp := rt.RHp;
    Result.THp := rt.THp;
  end;

end;

class function TRTsp.FullEvalution(_es: TComplexPoints; _ds: TRealPoints;
  wavelng: real; ang: real; polar: real): Complex;
var
  tr: TEH;
begin
  tr := Evalution(_es, _ds, wavelng, ang);
  Result.re := polar * tr.TEs.re + (1 - polar) * tr.THp.re;
  Result.im := polar * tr.REs.re + (1 - polar) * tr.RHp.re;
end;

end.
