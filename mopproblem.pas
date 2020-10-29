unit mopproblem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, moptype;

type

  { TMOPProblem }

  TMOPProblem = class  // то что надо вычислить
  public
    StartWaveLength: real;
    EndWaveLength: real;
    Steps: integer;
    Angle: real;
    Polarization: real;
    Solvation: TMopRTs;
    constructor Create;
    destructor Destroy; override;
  end;

  TMOPProblems = specialize TFPGList<TMOPProblem>;

implementation

{ TMOPProblem }

constructor TMOPProblem.Create;
begin
  Angle:=0;
  EndWaveLength:=-1;
  StartWaveLength:=0;
  Polarization:=0;
  Steps:=1;
  solvation:=nil;
end;

destructor TMOPProblem.Destroy;
begin
  if Assigned(solvation) then solvation.Free;
  inherited Destroy;
end;

end.

