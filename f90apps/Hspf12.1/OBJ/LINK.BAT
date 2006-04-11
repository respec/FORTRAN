@REM Relink ..\bin\hspf.exe (Newly compiled object files)
@amrsp--f
@lf90 @AUTOMAKE.RSP -exe ..\bin\hspf.exe -lisk -map ..\bin\hspf.map -bind -g -LIBPath c:\lib3.0\lf90libs -Lib hspf13a hspdss hec wdm aide adwdm util
:QUIT
