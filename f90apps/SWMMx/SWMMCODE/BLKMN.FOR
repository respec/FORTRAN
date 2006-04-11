      BLOCK DATA BLKMN
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      DATA PNAME/10*'        '/,PUNIT/10*'       '/,NDIM/10*0/
C#### WCH (CDM), 8/93. ADDITIONAL ELEMENT IN ARRAYS JKP AND FFNAME.
      DATA N5/98/,N6/99/,INCNT/0/,IOUTCT/0/,JKP/58*0/,
     *     JIN/25*0/,JOUT/25*0/
C##### WCH, 4/1/93
      DATA QCONV/1.0/
C=======================================================================
C     CMET ARRAY(11,2)   2nd POSITION: 1 - U.S. CUSTOMARY UNITS
C                                      2 - METRIC UNITS
C=======================================================================
C     CMET ARRAY  1    2     3        4    5        6            
      DATA CMET/1.0, 1.0, 12.0, 43200.0, 1.0, 43200.0,
C=======================================================================
C     CMET ARRAY        7    8      9   10   11            
     1          1036800.0, 1.0, 1.486, 1.0, 1.0,
C=======================================================================
C     CMET ARRAY     1      2      3          4     5         6            
     2          3.2808, 2.471, 304.8, 1097280.0, 25.4, 1.9751E6,
C=======================================================================
C     CMET ARRAY         7          8     9      10       11            
     2          2.633427E7, 35.314667, 1.00, 32.808, 0.62137/ 
C=======================================================================
      DATA FFNAME/'JIN.UF','JIN.UF','JIN.UF','JIN.UF','JIN.UF',
     +   'JIN.UF','JIN.UF','JIN.UF','JIN.UF','JIN.UF','JIN.UF',
     +   'JIN.UF','JIN.UF','JIN.UF','JIN.UF','JIN.UF','JIN.UF',
     +   'JIN.UF','JIN.UF','JIN.UF','JIN.UF','JIN.UF','JIN.UF',
     +   'JIN.UF','JIN.UF','JOT.UF',
     +   'JOT.UF','JOT.UF','JOT.UF','JOT.UF','JOT.UF','JOT.UF',
     +   'JOT.UF','JOT.UF','JOT.UF','JOT.UF','JOT.UF','JOT.UF',
     +   'JOT.UF','JOT.UF','JOT.UF','JOT.UF','JOT.UF','JOT.UF',
     +   'JOT.UF','JOT.UF','JOT.UF','JOT.UF','JOT.UF','JOT.UF',
     +   'SCRT1.UF','SCRT2.UF','SCRT3.UF','SCRT4.UF','SCRT5.UF',
     +   'SCRT6.UF','SCRT7.UF','SCRT8.UF'/
       END
