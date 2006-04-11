      SUBROUTINE SBTABL(J,NLABEL,NOMOS)
C	STATS BLOCK
C=======================================================================
C     This subroutine is called by the STATS block.  For a given series
C     of event data, it will calculate and print a rank-order table of
C     event magnitude, return period and frequency.  The table includes
C     the starting date and time for each event.
C=======================================================================
C     Modified by WCH, 7/21/93 to add heading for return period in years.
C     WCH, 8/1/95.  Change rainfall station ID (LOCRN) to character.
C     WCH, 12/4/96. Correct return period calculation.
C     CIM, 6/6/97. Simplify table to make easier to pull into spreadsheet.
C    
C#######################################################################
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STCOM.INC'
C=======================================================================
      CHARACTER*8 UNITS,FORM*90,F*90,NOTED(5)*8
      DATA NOTED/'Quantity','Quan/hr ','Quan/hr ','        ','        '/
      DATA F /'(1X,2(I8,1X,F5.2,1X,1PG9.3,1X,0PF7.3,2X,F7.2,A4),I8,1X,F5
     +.2,1X,1PG9.3,1X,0PF7.3,2X,F7.2)'/
C=======================================================================
C     Set format string to 'F' format.
C=======================================================================
      FORM = F
C=======================================================================
C     Calculate number of pages required for table at 180 events per page
C=======================================================================
                                          MPRINT = NPOINT
      IF(NPOINT.EQ.0.OR.NPOINT.GT.NEVNTS) MPRINT = NEVNTS
      NPAGES = (MPRINT+179)/180
C=======================================================================
C     NOMOS = rounded number of months for LRET=1 or rounded number
C     of years for LRET=0.
C     Return period = (NOMOS+1-2A)/(Rank-A)    Rank = XX later. 
C=======================================================================
      THETOP    = FLOAT(NOMOS+1) - 2.0*A
      DENOM    = FLOAT(NEVNTS)
      DO 1000 N = 1,NPAGES
C=======================================================================
C     Print appropriate page heading.
C=======================================================================
      IF(N.EQ.1) WRITE(N6,20)
      IF(N.GT.1) WRITE(N6,35)
C=======================================================================
C     Analysis is for flow or rainfall.
C=======================================================================
      IF(NLABEL.EQ.0) THEN
                      J1 = J
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####                      IF(LOCRN.GT.0) J1 = J + 10
                      IF(LOCRN.NE.' ') J1 = J + 10
                      IF(METRIC.EQ.1) THEN
                                      UNITS = ENGLAB(J)
                                      ELSE
                                      UNITS = SILAB(J)
                                      ENDIF
                      IF(LOCRQ.GT.0) WRITE(N6,45) UNITS
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####                      IF(LOCRN.GT.0) WRITE(N6,50) UNITS
                      IF(LOCRN.NE.' ') WRITE(N6,50) UNITS
C=======================================================================
C                     Analysis is for a pollutant.
C=======================================================================
                      ELSE
                      J1 = J+5
                      IF(METRIC.EQ.1.AND.NDIM(NLABEL).EQ.0) THEN
                                            UNITS = ENGLAB(J1)
                                            ENDIF
                      IF(METRIC.EQ.2.AND.NDIM(NLABEL).EQ.0) THEN
                                            UNITS = SILAB(J1)
                                            ENDIF
                      IF(NDIM(NLABEL).EQ.1) THEN
                                            UNITS = NOTED(J)
                                            ENDIF
                      IF(NDIM(NLABEL).EQ.2) THEN
                                            UNITS = CNOTE
                                            ENDIF
                      IF(J.EQ.4.OR.J.EQ.5)  THEN
                                            UNITS = PUNIT(NLABEL)
                                            ENDIF
                      WRITE(N6,75) PNAME(NLABEL),UNITS
                      ENDIF
C=======================================================================
C     Print table heading.
C=======================================================================
      WRITE(N6,160) PARLAB(J1)
c simplify to eliminate two columns in table
      WRITE(N6,200)
      WRITE(N6,250)
C#### WCH, 7/21/93. FIX UNITS FOR RETURN PERIOD HEADING.
      IF(LRET.EQ.1) WRITE(N6,265) UNITS
      IF(LRET.EQ.0) WRITE(N6,266) UNITS
CIM simplify to print one set of columns in table
      DO 800 I =1,NEVNTS
      NN       = I
      RPSET1 = THETOP/(NN-A)
      PSET1    = (1.0-(NN-1)/DENOM)*100.0
      WRITE(N6,FORM) ISRT(NN),TIMSRT(NN),
     +               PARAM(NN,J),RPSET1,PSET1
800   CONTINUE
C=======================================================================
C     Print unit message if NDIM = 2.
C=======================================================================
      IF(NLABEL.NE.0.AND.NDIM(NLABEL).EQ.2) THEN
      IF(J.EQ.1)   THEN
                   IF(METRIC.EQ.1) THEN
                                   WRITE(N6,822) PUNIT(NLABEL)
                                   ELSE
                                   WRITE(N6,826) PUNIT(NLABEL)
                                   ENDIF
                   ENDIF
      IF(J.GT.1.AND.J.LE.3) THEN
                   IF(METRIC.EQ.1) THEN
                                   WRITE(N6,910) PUNIT(NLABEL)
                                   ELSE
                                   WRITE(N6,930) PUNIT(NLABEL)
                                   ENDIF
                   ENDIF
      IF(J.GE.4)   WRITE(N6,950) PUNIT(NLABEL)
      ENDIF
1000  CONTINUE
C=======================================================================
20    FORMAT(///,11X,55(1H=),/,
     1 11X,'Table of Magnitude, Return period and Frequency',/,
     2 11X,55(1H=))
35    FORMAT(///,11X,65(1H=),11X,/,
     1 'Table of Magnitude, Return period and Frequency (continued)',/,
     2 11X,65(1H=))
45    FORMAT(//,6X,'Constituent analyzed    :',1X,'Flow',1X,A8)
50    FORMAT(//,6X,'Constituent analyzed    :',26X,'Rain',1X,A8)
75    FORMAT(//,6X,'Constituent analyzed    :',22X,A8,1X,A8)
160   FORMAT(6X,'Event parameter analyzed:',A20)
C simplify table with only one set of columns
200   FORMAT(1X,//,' Date    Time  Magnitude  Return  Percent')
250   FORMAT(1X,26X,'Period  < or =')
265   FORMAT(9X,'(Hour)',2X,A8,1X,'(Months)',/,
     + '------- ------',2X,'--------',1X,
     + '-------- -------')
C#### WCH, 7/21/93. ADD'L FORMAT FOR RETURN PERIOD IN YEARS.
266   FORMAT(9X,'(Hour)',2X,A8,1X,'(Years) ',/,
     + ' ------ ------',2X,'--------',1X,
     + '-------- -------')
822   FORMAT(/,1X,'Note:  Magnitude has units of (cubic feet) * ',
     +   A8,'.  See user''s manual for explanation.')
826   FORMAT(/,1X,'Note:  Magnitude has units of (liters) * ',
     +   A8,'.  See user''s manual for explanation.')
910   FORMAT(/,1X,'Note:  Magnitude has units of (cfs) * ',
     +   A8,'  See user''s manual for explanation.')
930   FORMAT(/,1X,'Note:  Magnitude has units of (liters/s) * ',
     +   A8,'.  See user''s manual for explanation.')
950   FORMAT(/,1X,'Note:  Magnitude has units of ',
     +   A8,'.  See user''s manual for explanation.')
C=======================================================================
      RETURN
      END
