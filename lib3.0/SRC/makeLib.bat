rem ant -f makeLib.xml -DLibName=%1 -d >makeLibDebug.lis
ant -f makeLib.xml %2 -DLibName=%1 >makeLib.lis
