program lazmop;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  tachartlazaruspkg,
  mainform,
  moptask,
  mopformula,
  mopmaterial,
  moptype,
  mopproblem,
  complexes,
  mopan;

{$R *.res}

begin
  Application.Title:='wmop';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TmForm, mForm);
  Application.Run;
end.
