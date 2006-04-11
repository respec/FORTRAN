C=======================================================================
C     Block data was last updated December, 1990.
C=======================================================================
      BLOCK DATA RUNOF
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'SUBCAT.INC'
C=======================================================================
      DATA PMONTH/'January','February','March','April','May','June',
     1            'July','August','September','October','November',
     2            'December'/
      DATA PDAYS/31.0,28.0,31.0,30.0,31.0,30.0,31.0,
     1           31.0,30.0,31.0,30.0,31.0/
      DATA FACT1/453600.0,1.0E06,1.0/,FACT2/453.6,453.6E06,
     1           453.6/,FACT3/28.3,28.3,1.0/
      DATA KEROS/' Erosion','mg/l    '/
      END
