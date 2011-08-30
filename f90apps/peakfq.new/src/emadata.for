c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      SUBROUTINE EMADATA(NPKS,Q,WY,WYMIN,WYMAX,
     I                   NT,THBY,THEY,THLO,THUP,GAGEB,
     I                   NINTVL,INTVLYR,INTVLLWR,INTVLUPR,
     M                   NO,
     O                   OWY,QL,QU,TL,TU,DTYPE)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c    
c    This routine arranges a data set using the EMA algorithm 
c
c    Originally prepared for Aquaterra by Tim Cohn, US Geological Survey
c    Timothy A. Cohn        05 December 2003
c
c    Modified 6/2007 to accomodate perception thresholds
c      modifications by AQUA TERRA and Tim Cohn
c
c    Revised 5/2011 by Paul Hummel of AQUA TERRA to 
c    include interval data
c
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            NPKS       i*4  number of peaks
c            Q(NPKS)    r*4  array of peak values
c            WY(NPKS)   i*4  array of water year values
c            WYMIN      i*4  Min threshold beginning year
c            WYMAX      i*4  Max threshold ending year
c            NT         I*4  Number of perception thresholds
c            THBY       I*4  Array of threshold beginning years
c            THEY       I*4  Array of threshold ending years
c            THLO       R*4  Array of threshold lower bounds
c            THUP       R*4  Array of threshold upper bounds
c            GAGEB      R*4  Gage base discharge
c            NINTVL     I*4  Number of interval data values
c            INTVLYR    I*4  Array of interval data years (0 = no value)
c            INTVLLWR   R*4  Array of low interval data values
c            INTVLUPR   R*4  Array of high interval data values
c
c       output variables: (input to "emafit")
c       ---------------------------------------------------------------------------
c            NO         i*4  total number of observations, 
c                            determined from start/end of threshold years
c            OW(NO)     i*4  vector of water years
c            QL(NO)     r*8  vector of lower bounds on floods
c            QU(NO)     r*8  vector of upper bounds on floods
c            TL(NO)     r*8  vector of lower bounds on flood threshold
c            TU(NO)     r*8  vector of upper bounds on flood threshold
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       N.B.  The output arrays, {ql,qu} and {tl,tu}, represent two distinct 
c             concepts that need to be understood clearly:
c            a.  The observations, which are described by {ql,qu};
c                  specifically, the true value of q is assumed to lie
c                    wihin the interval ql(i) <= q(i) <= qu(i);
c                  if q(i) is known exactly, then ql(i) = q(i) = qu(i);
c
c                  {ql,qu} are required by EMA to fit P3 dist'n to data
c
c            b.  The censoring pattern, which is described by {tl,tu}
c                  {tl,tu} define interval on which data get observed:
c                   if q is not inside {tl,tu}, q is either left or right censored.
c                  Examples:
c                    {-inf,+inf}  => Systematic data (everything known exactly)
c                    {T,   +inf}  => Historical data (q>=t known exactly; q<t censored)
c                
c                  {tl,tu} are required by EMA to estimate confidence intervals
c
c        Also:
c            EMAFIT requires the logarithms of the floods; this routine will work 
c            with either the real-space flood data or the log-space data.
c
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      implicit none

C     + + + DUMMY ARGUMENTS + + +      
      INTEGER NPKS,NT,WYMIN,WYMAX,NO,NINTVL
      INTEGER WY(NPKS),THBY(NT),THEY(NT),INTVLYR(NINTVL),OWY(NO)
      REAL  Q(NPKS),THLO(NT),THUP(NT),GAGEB,
     $      INTVLLWR(NINTVL),INTVLUPR(NINTVL)
      DOUBLE PRECISION QL(NO),QU(NO),TL(NO),TU(NO)
      CHARACTER*4 DTYPE(NO)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,J,K
      DOUBLE PRECISION  MISSING,QMIN,QAVE
     
      DATA MISSING, QMIN 
     $      /-10.0, 1.0D-99/

C     + + + END SPECIFICATIONS + + +
C
      write(*,*)"EMADATA: entry"
C     init arrays
      DO 10 I = 1,NO
        OWY(I) = WYMIN + I - 1
        QL(I) = MISSING
        QU(I) = MISSING
        TL(I) = MISSING
        TU(I) = MISSING
        DTYPE(I)= 'Othr'
 10   CONTINUE
      write(*,*)"EMADATA: initialized arrays, NT",NT
C
C     N.B. If gage base discharge is greater than zero, this provides a lower
C          bound on the observation threshold corresponding to all flows
      DO 30 J = 1,NT
C       write(*,*)"EMADATA: inside 30, J,THBY(J),THEY(J)",
C     $                                J,THBY(J),THEY(J)
        DO 20 K = THBY(J),THEY(J)
          I = K - WYMIN + 1
C         write(*,*)"EMADATA: inside 20, K,I",K,I
          TU(I) = THUP(J)
          IF (GAGEB .GT. 0) THEN
            TL(I) = MAX(LOG(GAGEB),THLO(J))
          ELSE
            TL(I) = THLO(J)
          END IF         
C         write(*,*)"EMADATA: assigned TL(I)",TL(I)
 20     CONTINUE
 30   CONTINUE
      write(*,*)"EMADATA: set TL based on GAGEB",GAGEB
C
C     fill in measured peaks
      DO 40 J=1,NPKS
c       write(*,*)"EMADATA: in 40,J,Q(j)",J,Q(J)
        IF (Q(J) .GT. MISSING) THEN
c         write(*,*)"EMADATA: in 40,WY(J),WYMIN",ABS(WY(J)),WYMIN
          I = ABS(WY(J)) - WYMIN + 1
          IF (TL(I) .EQ. MISSING) THEN  ! this should never occur
            write(*,*) ' *** Problem ***'
            write(*,*) ' Water Year ',ABS(WY(J)),
     $                 ' has peak without tl, tu'
          END IF
          QL(I) = Q(J)
          QU(I) = Q(J)
          DTYPE(I)= 'Syst'
        END IF
 40   CONTINUE
      write(*,*)"EMADATA: set QL/QU for NPKS",NPKS
C
C     fill in interval data
      DO 45 J=1,NINTVL
        IF (INTVLYR(J).GT.0) THEN
          I = INTVLYR(J) - WYMIN + 1
          QL(I) = INTVLLWR(J)
          QU(I) = INTVLUPR(J)
        END IF
 45   CONTINUE
C
C     For years without peaks, assume peak is less than threshold tl
      DO 50 K = WYMIN, WYMAX
        I = K - WYMIN + 1
        IF (QL(I) .EQ. MISSING) THEN  ! no peak for this year
          IF(TL(I) .GT. 0.d0) THEN
            QL(I) = 0.0
            QU(I) = TL(I)
          END IF
        END IF
 50   CONTINUE
      write(*,*)"EMADATA: set QL/QU for years w/out peak"

C     collapse the data set, eliminating any periods of missing record
      NO = 0
      DO 60 J = 1, WYMAX - WYMIN + 1
C       write(*,*)"EMADATA: in 60, J,QL(J)",J,QL(J)
        IF (ABS(QL(J)-MISSING) .GT. QMIN) THEN
          NO = NO + 1
C         write(*,*)"EMADATA: in 60, fill pos NO",NO
          TL(NO) = TL(J)
          TU(NO) = TU(J)
          QL(NO) = QL(J)
          QU(NO) = QU(J) 
          OWY(NO) = OWY(J)
          DTYPE(NO)= DTYPE(J)
        END IF
 60   CONTINUE
      write(*,*)"EMADATA: collapsed arrays, NO",NO

      write(*,*) 'Year        QLow        QUpr        TLow        TUpr'
 1000 format(1X,I4,2F12.1,2D12.5)
      DO 70 I = 1,NO
        write(*,1000) OWY(I),QL(I),QU(I),TL(I),TU(I)
 70   CONTINUE

      I = 0
      QAVE = 0.0
      DO 100 J = 1,NO
        QL(J) = LOG10(MAX(QMIN,QL(J)))
        QU(J) = LOG10(MAX(QMIN,QU(J)))
        TL(J) = LOG10(MAX(QMIN,TL(J)))
        TU(J) = LOG10(MAX(QMIN,TU(J)))
        IF (QL(J).GT.QMIN) THEN
          QAVE = QAVE + QL(J)
          I = I + 1
        END IF
 100  CONTINUE
      QAVE = QAVE/I
      write(*,*)"EMADATA: Transformed to log, QL Count,Ave",I,QAVE
C      
      RETURN
      END
      
