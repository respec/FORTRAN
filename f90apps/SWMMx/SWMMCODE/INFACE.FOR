      SUBROUTINE INFACE(IDO,NTAPE)
C=======================================================================
C     This subroutine reads or writes the header information
C     for a SWMM interface file.  Created FEBRUARY, 1987
C     by Robert E. Dickinson.  Last updated December, 1990.
C     Updated 4/13/93 by WCH for minor format change.
C     Added and updated error messages, WCH, 9/8/93 and 11/23/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      CHARACTER KJN*10
      DIMENSION JN(NIE),KJN(NIE)
C=======================================================================
C     Read interface headers.
C=======================================================================
      IF(IDO.EQ.0) THEN
           REWIND NTAPE
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) TITLE(1)
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) TITLE(2)
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) IDATEZ,TZERO
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) TITLE(3)
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) TITLE(4)
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) SOURCE,LOCATS,
     1        NQUAL,TRIBA,NJCE
           IF(NJCE.EQ.0)READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000)
     1        (NLOC(I),I=1,LOCATS)
           IF(NJCE.EQ.1)READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000)
     1        (KAN(I),I=1,LOCATS)
           IF(NQUAL.GT.0) THEN
                READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) 
     1             (PNAME(J),J=1,NQUAL)
                READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) 
     1             (PUNIT(J),J=1,NQUAL)
                READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) 
     1             (NDIM(J),J=1,NQUAL)
                ENDIF
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) QCONV
           ENDIF
C=======================================================================
C     Read and write interface headers.
C=======================================================================
      IF(IDO.EQ.1) THEN
           REWIND NTAPE
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) TITLE(1)
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) TITLE(2)
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) IDATEZ,TZERO
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) TITLE(3)
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) TITLE(4)
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) SOURCE,LOCATS,
     1        NQUAL,TRIBA,NJCE
           WRITE(N6,1)  TITLE(1),TITLE(2)
           WRITE(N6,2)  TITLE(3),TITLE(4)
           WRITE(N6,3)  SOURCE
           WRITE(N6,4)  IDATEZ,TZERO
           WRITE(N6,5)  LOCATS,NQUAL,TRIBA,NJCE
C=======================================================================
C     Read sequence of location numbers.
C=======================================================================
           IF(NJCE.EQ.0) THEN
                READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) 
     1             (NLOC(I),I=1,LOCATS)
                WRITE(N6,6) (NLOC(I),I=1,LOCATS)
                ELSE
                READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000)  
     1             (KAN(I),I=1,LOCATS)
                WRITE(N6,66) (KAN(I),I=1,LOCATS)
                ENDIF
           IF(NQUAL.GT.0) THEN
                READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000)   
     1             (PNAME(J),J=1,NQUAL)
                READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000)   
     1             (PUNIT(J),J=1,NQUAL)
                READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000)   
     1             (NDIM(J),J=1,NQUAL)
                WRITE(N6,7) (J,PNAME(J),PUNIT(J),NDIM(J),J=1,NQUAL)
                ENDIF
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000)  QCONV
           WRITE(N6,8)                   QCONV
           ENDIF
C=======================================================================
C     Write interface headers.
C=======================================================================
      IF(IDO.EQ.2) THEN
           REWIND NTAPE
           READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) NEWOUT,NPOLL
           IF(JCE.EQ.0) READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000)  
     1        (JN(I),I=1,NEWOUT)
           IF(JCE.EQ.1) READ(NTAPE,ERR=999,IOSTAT=IOS,END=2000) 
     1        (KJN(I),I=1,NEWOUT)
           REWIND NTAPE
           WRITE(NTAPE,ERR=998,IOSTAT=IOS) TITLE(1)
           WRITE(NTAPE,ERR=998,IOSTAT=IOS) TITLE(2)
           WRITE(NTAPE,ERR=998,IOSTAT=IOS) IDATEZ,TZERO
           WRITE(NTAPE,ERR=998,IOSTAT=IOS) TITLE(3)
           WRITE(NTAPE,ERR=998,IOSTAT=IOS) TITLE(4)
           WRITE(NTAPE,ERR=998,IOSTAT=IOS) SOURCE,NEWOUT,NPOLL,TRIBA,JCE
           IF(JCE.EQ.0) WRITE(NTAPE,ERR=998,IOSTAT=IOS) 
     1        (JN(I),I=1,NEWOUT)
           IF(JCE.EQ.1) WRITE(NTAPE,ERR=998,IOSTAT=IOS) 
     1        (KJN(I),I=1,NEWOUT)
           IF(NPOLL.GT.0) THEN
                WRITE(NTAPE,ERR=998,IOSTAT=IOS) (PNAME(J),J=1,NPOLL)
                WRITE(NTAPE,ERR=998,IOSTAT=IOS) (PUNIT(J),J=1,NPOLL)
                WRITE(NTAPE,ERR=998,IOSTAT=IOS) (NDIM(J),J=1,NPOLL)
                ENDIF
           WRITE(NTAPE,ERR=998,IOSTAT=IOS)   QCONV
           ENDIF
      RETURN
  998 WRITE(N6,1998) NTAPE,FFNAME(IOUTCT+25),IOS
      STOP
  999 WRITE(N6,1999) NTAPE,FFNAME(INCNT),IOS
      STOP
C#### WCH, 9/8/93
 2000 WRITE(N6,2010) NTAPE,FFNAME(INCNT)
      WRITE(*,2010)  NTAPE,FFNAME(INCNT)
      STOP
C=======================================================================
1     FORMAT(//,
     1' ###########################################',/,
     2' # Header information from interface file: #',/,
     3' ###########################################',//,
     4 ' Title from first computational block:',/,1X,A80,/,1X,A80)
2     FORMAT(/,' Title from immediately preceding computational block:',
     1 /,1X,A80,/,1X,A80)
3     FORMAT(/,
     1 ' Name of preceding block:................',A20)
4     FORMAT(
     1 ' Initial Julian date (IDATEZ)......................',I8,/,
     2 ' Initial time of day in seconds (TZERO)............',F8.1)
C#### WCH, 4/13/93.  VERY MINOR FORMAT CHANGE FOR AREA.
5     FORMAT(' No. transfered input locations....................',I8,/,
     1       ' No. transfered pollutants.........................',I8,/,
     2       ' Size of total catchment area (acres)..............',
     2 F10.2,
     3     /,' ID numbers (JCE=0) or alphanumeric (JCE=1)........',I8)
6     FORMAT(/,' #################################################',/,
     1' # Element numbers of interface inlet locations: # ',/,
     2' #################################################',/,(9I8))
7     FORMAT(/,' #########################################',/,
     1' # Quality parameters on interface file: #',/,
     2' #########################################',//,
     3 ' No. Name      Units     Type of units',/,
     4 ' --- ----      -----     -------------',/,
     5 (1X,I2,2X,A8,2X,A8,I7))
8     FORMAT (/,' Conversion factor to cfs for flow units',/,
     1           ' on interface file.  Multiply by: ',F11.5)
   66 FORMAT(/,' #################################################',/,
     1' # Element numbers of interface inlet locations: # ',/,
     2' #################################################',/,
     3(9(1X,A10)))
C#### WCH, 11/23/93
 1998 FORMAT(/,' ===> Error !!  Writing the interface file on unit # ',
     1    I3,/,'      File name... ',A60,/,
     2         '      Fortran error number =',I5,'. Run stopped.')
 1999 FORMAT(/,' ===> Error !!  Reading the interface file on unit # ',
     1    I3,/,'      File name... ',A60,/,
     2         '      Fortran error number =',I5,'. Run stopped.')
C#### WCH, 9/8/93.
 2010 FORMAT(/,' ERROR.  End-of-file reached unexpectedly while reading 
     1header information on',/,' interface file on unit no.',I3,
     2'. File name = ',A20,/,' Check to see that this file is the proper
     3 interface file.  Run stopped.')
C=======================================================================
      END
