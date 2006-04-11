      PROGRAM MAIN
      CHARACTER*132 FMT
      WRITE(*,*) ' Enter FORMAT -> '
      READ (*,*) FMT
      WRITE(*,*) FMT
      X1=3.62112
      X2=26.8531
      X3=2.92412
      X4=1922.91
      WRITE(10,10) X1
      WRITE(10,FMT) X2
      WRITE(10,FMT) X3
      WRITE(10,FMT) X4
   10 FORMAT(1X,F11.4)
      STOP
      END
