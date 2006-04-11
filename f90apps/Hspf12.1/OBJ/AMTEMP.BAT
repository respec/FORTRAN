@REM Compile HSPFBAT.FOR (no object file)
@lf90 -ap -chk -g -co -nf90 -fix -nlst -nhed -pca -sav -stchk -trace -w -nwo -nxref -c HSPFBAT.FOR
@IF ERRORLEVEL 1 GOTO QUIT

@REM Compile HSPSTA.FOR (no object file)
@lf90 -ap -chk -g -co -nf90 -fix -nlst -nhed -pca -sav -stchk -trace -w -nwo -nxref -c HSPSTA.FOR
@IF ERRORLEVEL 1 GOTO QUIT

:QUIT
