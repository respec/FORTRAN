C#### WCH, 12/31/93.  REARRANGE ORDER OF COMMON/TIMER/
C#### DWD (CSC) - Begin change.
C     Date: Tuesday, 10 May 1994.  Time: 12:33:51.
C     Moved TIME to end of common block to correct warning generated
C     during compilation using Lahey 32-bit FORTRAN, version 5.01
CWarning - Previous common variable causes misalignment of DOUBLE PRECISION 
Cvariable (TIME) , File TIMER.INC, line 2.
C
      COMMON/TIMER/IPRNGW,LUNIT,DELT,WET,DRY,WETDRY,LONG,DMEAN,DLAST,
     1             JSTART(10),JSTOP(10),IPRN(7),PMONTH(12),TIME
C     1             TIME,JSTART(10),JSTOP(10),IPRN(7),PMONTH(12)
C
C#### DWD (CSC) - End change.
C     Date: Tuesday, 10 May 1994.  Time: 12:33:51.
C
C#### WCH (RED), 9/93. CHANGE "TIME" TO DOUBLE PRECISION.
      DOUBLE PRECISION TIME
      REAL         LONG,DELT,WET,DRY,WETDRY,DMEAN,DLAST
      CHARACTER*10 PMONTH
