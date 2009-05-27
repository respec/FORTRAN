copy %1.sav %1.sta
copy %1.dq1 %1.upd
..\..\hspfuci\bin\hspfbat.exe %1.uci >%1.lis
copy %1.dq2 %1.upd
..\..\hspfuci\bin\hspfbat.exe %1.uci >>%1.lis
