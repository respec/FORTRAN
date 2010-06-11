erase ..\obj\*.obj
lf90 peakfq.for -c -trace -o ..\obj\peakfq.obj     >comp.out   -xref -lst
lf90 j407wc.for -c -trace -o ..\obj\j407wc.obj     >>comp.out  -xref -lst
lf90 j407xe.for -c -trace -o ..\obj\j407xe.obj -O0 >>comp.out  -xref -lst
lf90 utj407.for -c -trace -o ..\obj\utj407.obj     >>comp.out
lf90 stgaus.for -c -trace -o ..\obj\stgaus.obj     >>comp.out
rem lf90 utchar.for -c -o ..\obj\utchar.obj     >>comp.out
rem lf90 utnumb.for -c -o ..\obj\utnumb.obj     >>comp.out
rem lf90 utdate.for -c -o ..\obj\utdate.obj     >>comp.out
rem lf90 utcpgn.for -c -o ..\obj\utcpgn.obj     >>comp.out
rem lf90 datsys90.for -c -o ..\obj\datsys90.obj >>comp.out
rem lf90 emafit.for -c -trace -o ..\obj\emafit.obj     >>comp.out
lf90 emafit.f -c -o ..\obj\emafit.obj >>comp.out
lf90 emadata.for -c -o ..\obj\emadata.obj       >>comp.out
lf90 probfun.f -c -o ..\obj\probfun.obj       >>comp.out
rem lf90 pkkeyb.for -c -o ..\obj\pkkeyb.obj >>comp.out
rem lf90 pkutil.for -c -o ..\obj\pkutil.obj >>comp.out
lf90 pkwdm.for  -c -trace -o ..\obj\pkwdm.obj -O0  >>comp.out
rem lf90 wdoppc90.for  -c -trace -o ..\obj\wdoppc90.obj  >>comp.out
rem lf90 utgnrl.for -c -o ..\obj\utgnrl.obj -O0     >>comp.out
lf90 utstat.for -c -o ..\obj\utstat.obj -O0     >>comp.out
lf90 wdpeak.for -c -o ..\obj\wdpeak.obj -O0     >>comp.out
rem lf90 wdtble.for -c -trace -o ..\obj\wdtble.obj -O0     >>comp.out
lf90 stutil.for -c -o ..\obj\stutil.obj -O0     >>comp.out
rem lf90 pkplot.for -c -o ..\obj\pkplot.obj -O0     >>comp.out
rem lf90 agplot.for -c -o ..\obj\agplot.obj -O0     >>comp.out
rem lf90 agpltx.for -c -o ..\obj\agpltx.obj -O0     >>comp.out
rem lf90 ..\obj\*.obj ..\..\ema\*.obj -lib \lib3.0\lf90libs\aide \lib3.0\lf90libs\wdm \lib3.0\lf90libs\adwdm \lib3.0\lf90libs\util \lib3.0\lf90libs\graph90 -lisk -nwin -bind -vm -exe ..\bin\peakfq.exe >>comp.out
rem lf90 ..\obj\*.obj -lib \lib3.0\lf90libs\aide \lib3.0\lf90libs\wdm \lib3.0\lf90libs\adwdm \lib3.0\lf90libs\util \lib3.0\lf90libs\graph90 -lisk -nwin -bind -vm -exe ..\bin\peakfq.exe >>comp.out
rem lf90 ..\obj\*.obj ..\..\ema\*.obj lib4graph\*.obj -lib \lib3.0\lf90libs\aide \lib3.0\lf90libs\wdm \lib3.0\lf90libs\adwdm \lib3.0\lf90libs\util \int\lib\shortl90 \int\lib\intpcl90 -nwin -bind -vm -exe ..\bin\PKFQBat.exe >>comp.out
rem lf90 ..\obj\*.obj ..\..\ema\*.obj lib4graph\*.obj -lib \mylib3.0\lf90libs\aide \mylib3.0\lf90libs\wdm \mylib3.0\lf90libs\adwdm \mylib3.0\lf90libs\util \int\lib\shortl90 \int\lib\intmwl90 \int\lib\winspool -winconsole -exe ..\bin\PKFQBat.exe >>comp.out
lf90 ..\obj\*.obj ..\..\ema\*.obj lib4graph\*.obj -lib \mylib3.0\lf90libs\wdm \mylib3.0\lf90libs\adwdm \mylib3.0\lf90libs\util \int\lib\shortl90 \int\lib\intmwl90 \int\lib\winspool -winconsole -exe ..\bin\PKFQBat.exe >>comp.out
