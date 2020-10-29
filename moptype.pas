unit moptype;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Complexes, fgl;

type
  TIntegerPoints = specialize TFPGList<Integer>;
  TRealPoints = specialize TFPGList<Real>;
  TComplexPoints = specialize TFPGList<Complex>;

  { TMopRT }

  TMopRT = record
    R: real;
    T: real;
    W: real;
    class operator = (v1,v2:TMopRT) v: boolean;
  end;

  TMopRTs = specialize TFPGList<TMopRT>;

implementation

{ TMopRT }

class operator TMopRT.=(v1, v2: TMopRT)v: boolean;
begin
  v := (v1.R=v2.R) and (v1.T=v2.T) and (v1.W=v2.W);
end;

end.

