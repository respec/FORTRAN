      SUBROUTINE SBTABL(J,NLABEL,NOMOS)
C=======================================================================
C     This subroutine is called by the STATS block.  For a given series
C     of event data, it will calculate and print a rank-order table of
C     event magnitude, return period and frequency.  The table includes
C     the starting date and time for each event.
C=======================================================================
C     Modified by WCH, 7/21/93 to add heading for return period in years.
C#######################################################################
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STCOM.INC'
C=======================================================================
      CHARACTER*8 UNITS,FORM*90,F*90,DASH*4,NOTED(5)*8
      DATA NOTED/'Quantity','Quan/hr ','Quan/hr ','        ','        '/
      DATA DASH/' ## '/
      DATA F /'(1X,2(I6,2X,F5.2,1X,1PG9.3,1X,0PF7.3,2X,F7.2,A4),I6,2X,F5
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
C## WCH.  NOMOS = ROUNDED MONTHS FOR LRET=1, ROUNDED YEARS FOR LRET=0.
      THETOP    = FLOAT(NOMOS+1) - 2.0*A
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
                      IF(LOCRN.GT.0) J1 = J + 10
                      IF(METRIC.EQ.1) THEN
                                      UNITS = ENGLAB(J)
                                      ELSE
                                      UNITS = SILAB(J)
                                      ENDIF
                      IF(LOCRQ.GT.0) WRITE(N6,45) UNITS
                      IF(LOCRN.GT.0) WRITE(N6,50) UNITS
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
      WRITE(N6,170)
      WRITE(N6,200)
      WRITE(N6,250)
C#### WCH, 7/21/93. FIX UNITS FOR RETURN PERIOD HEADING.
      IF(LRET.EQ.1) WRITE(N6,265) UNITS,UNITS,UNITS
      IF(LRET.EQ.0) WRITE(N6,266) UNITS,UNITS,UNITS
C=======================================================================
C     CALCULATE NUMBER OF ROWS FOR THIS PAGE
C     PRINT SIXTY ROWS PER PAGE IN THREE SETS OF COLUMNS
C=======================================================================
                      NROWS = (MPRINT-(N-1)*180+2)/3
      IF(NROWS.GT.60) NROWS = 60
C=======================================================================
C     Calculate number of empty sets of columns for this row.
C=======================================================================
      XY       = 0.0
      DENOM    = FLOAT(NEVNTS)
      TOP      = FLOAT(N-1)
      XX       = FLOAT(N-1)*180.0 + 1.0
      DO 800 I = 1,NROWS
      MTCOLS   = 3*I-(MPRINT-(N-1)*180)
      XX       = XX + FLOAT(I-1)*3.0
      RPSET1   = THETOP/(XX-A)
      RPSET2   = THETOP/(XX+1.0-A)
      RPSET3   = THETOP/(XX+2.0-A)
      XY       = XY + 3.0
      DIFF     = 180.0*TOP + XY - 3.0
      PSET1    = (1.0-DIFF/DENOM)*100.0
      DIFF     = 180.0*TOP + XY - 2.0
      PSET2    = (1.0-DIFF/DENOM)*100.0
      DIFF     = 180.0*TOP + XY - 1.0
      PSET3    = (1.0-DIFF/DENOM)*100.0
      NN       = (N-1)*180 + 3*I
C=======================================================================
C     Print either 3, 2, or 1 set of columns.
C=======================================================================
      IF(MTCOLS.LE.0) WRITE(N6,FORM) ISRT(NN-2),TIMSRT(NN-2),
     +                PARAM(NN-2,J),RPSET1,PSET1,DASH,ISRT(NN-1),
     +                TIMSRT(NN-1),PARAM(NN-1,J),RPSET2,PSET2,DASH,
     +                ISRT(NN),TIMSRT(NN),PARAM(NN,J),RPSET3,PSET3
      IF(MTCOLS.EQ.1) WRITE(N6,FORM) ISRT(NN-2),TIMSRT(NN-2),
     +                PARAM(NN-2,J),RPSET1,PSET1,DASH,ISRT(NN-1),
     +                TIMSRT(NN-1),PARAM(NN-1,J),RPSET2,PSET2
      IF(MTCOLS.EQ.2) WRITE(N6,FORM) ISRT(NN-2),TIMSRT(NN-2),
     +                PARAM(NN-2,J),RPSET1,PSET1,DASH
800   CONTINUE
C=======================================================================
C     Print unit message if NDIM = 2.
C=======================================================================
      WRITE(N6,280)
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
45    FORMAT(//,6X,'Constituent analyzed    :',26X,'Flow',1X,A8)
50    FORMAT(//,6X,'Constituent analyzed    :',26X,'Rain',1X,A8)
75    FORMAT(//,6X,'Constituent analyzed    :',22X,A8,1X,A8)
160   FORMAT(6X,'Event parameter analyzed:',A30)
170   FORMAT(//,1X,T42,' ## ',T86,' ## ')
200   FORMAT(1X,2(' Date   Time  Magnitude  Return  Percent ## '),
     2 ' Date   Time  Magnitude  Return  Percent')
250   FORMAT(1X,2(25X,'Period  < or =  ## '),25X,'Period  < or =')
265   FORMAT(8X,'(Hour)',2X,A8,1X,'(Months)',9X,'##',8X,'(Hour)',
     +  2X,A8,1X,'(Months)',9X,'##',8X,'(Hour)',2X,A8,1X,'(Months)',/,
     + ' ------ ------',2X,'--------',1X,
     + '-------- ------- ## ------ ------',
     +  2X,'--------',1X,'-------- ------- ## ------ ------',
     +     ' -------- -------- --------')
C#### WCH, 7/21/93. ADD'L FORMAT FOR RETURN PERIOD IN YEARS.
266   FORMAT(8X,'(Hour)',2X,A8,1X,'(Years) ',9X,'##',8X,'(Hour)',
     +  2X,A8,1X,'(Years) ',9X,'##',8X,'(Hour)',2X,A8,1X,'(Years) ',/,
     + ' ------ ------',2X,'--------',1X,
     + '-------- ------- ## ------ ------',
     +  2X,'--------',1X,'-------- ------- ## ------ ------',
     +     ' -------- -------- --------')
280   FORMAT(1X,T42,' ## ',T86,' ## ')
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