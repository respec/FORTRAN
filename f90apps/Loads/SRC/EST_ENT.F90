      !estimator:gpcode
      SUBROUTINE F90_GPCODE (NVARS,IVARS,FNAME)

        DLL_EXPORT F90_GPCODE

        INTEGER, INTENT(OUT)         :: NVARS,IVARS(1000)
        CHARACTER(LEN=*), INTENT(IN) :: FNAME

        CHARACTER*100            :: FNAME1
        INTEGER                 :: FUN

        FNAME1 = FNAME
        FUN    = 11

        CALL GPCODE (FUN,FNAME1, &
                     NVARS,IVARS)

      END SUBROUTINE F90_GPCODE


      !estimator:realdate
      SUBROUTINE F90_REALDATE (IYR,IMO,IDY,RDATE)

        DLL_EXPORT F90_REALDATE

        INTEGER, INTENT(IN)         :: IYR,IMO,IDY
        REAL, INTENT(OUT)           :: RDATE

        INTEGER             :: DATE1(6),DATE2(6),TCODE,TSTEP,NV1,NV2

        TSTEP = 1
        TCODE = 4

        DATE1(1) = IYR
        DATE1(2) = 1
        DATE1(3) = 1
        DATE1(4) = 0
        DATE1(5) = 0
        DATE1(6) = 0

        DATE2(1) = IYR
        DATE2(2) = IMO
        DATE2(3) = IDY
        DATE2(4) = 0
        DATE2(5) = 0
        DATE2(6) = 0

        CALL TIMDIF (DATE1,DATE2,TCODE,TSTEP, &
                     NV1)

        DATE1(1) = IYR
        DATE1(2) = 1
        DATE1(3) = 1

        DATE2(1) = IYR
        DATE2(2) = 12
        DATE2(3) = 31

        CALL TIMDIF (DATE1,DATE2,TCODE,TSTEP, &
                     NV2)

        RDATE = FLOAT(IYR) + (FLOAT(NV1)/FLOAT(NV2))

      END SUBROUTINE F90_REALDATE


      !estimator:sixidate
      SUBROUTINE F90_I3DATE (RDATE,IDATE)

        DLL_EXPORT F90_I3DATE

        DOUBLE PRECISION, INTENT(IN) :: RDATE
        INTEGER, INTENT(OUT)         :: IDATE(3)

        INTEGER             :: DATE1(6),DATE2(6),TCODE,TSTEP

        TSTEP = 1
        TCODE = 4

        IDATE(1) = INT(RDATE)

        DATE1(1) = IDATE(1)
        DATE1(2) = 1
        DATE1(3) = 1
        DATE1(4) = 0
        DATE1(5) = 0
        DATE1(6) = 0

        DATE2(1) = IDATE(1)
        DATE2(2) = 12
        DATE2(3) = 31
        DATE2(4) = 0
        DATE2(5) = 0
        DATE2(6) = 0

        CALL TIMDIF (DATE1,DATE2,TCODE,TSTEP, &
                     NDAYSY)

        RDAYS = FLOAT(NDAYSY+1) * (RDATE - FLOAT(IDATE(1)))

        NDAYS = INT(RDAYS) + 1

        CALL JDMODY (IDATE(1),NDAYS, &
                     IDATE(2),IDATE(3))

      END SUBROUTINE F90_I3DATE


      !estimator:estimate
      SUBROUTINE F90_ESTIM (MESSFG,LIEASY,LYRBEG,LYREND,IPCODE, &
                            RBEGIN,REND,NREG,IREG,ERRCOD,MESCOD, &
                            OFNAME,CNAME,DLNAME,QNAME,PNAME,CMNAME)

        DLL_EXPORT F90_ESTIM

        INTEGER, INTENT(IN)          :: MESSFG,LIEASY,LYRBEG,LYREND
        INTEGER, INTENT(IN)          :: IPCODE,NREG,IREG(200)
        REAL, INTENT(IN)             :: RBEGIN,REND
        CHARACTER(LEN=*), INTENT(IN) :: OFNAME,CNAME,DLNAME,QNAME
        CHARACTER(LEN=*), INTENT(IN) :: PNAME,CMNAME
        INTEGER, INTENT(OUT)         :: ERRCOD,MESCOD

        CHARACTER*100       :: OFNAME1,CNAME1,DLNAME1
        CHARACTER*100       :: QNAME1,PNAME1,CMNAME1
        INTEGER             :: IUNIT,CUNIT,QUNIT,PUNIT,CMUNIT

        OFNAME1 = OFNAME
        CNAME1  = CNAME
        DLNAME1 = DLNAME
        QNAME1  = QNAME
        PNAME1  = PNAME
        CMNAME1 = CMNAME

        IUNIT  = 19
        CUNIT  = 16
        QUNIT  = 15
        PUNIT  = 11
        CMUNIT = 12

        CALL ESTIMATE (MESSFG,OFNAME1,LIEASY,CNAME1,LYRBEG,LYREND,   &
                       DLNAME1,IUNIT,CUNIT,QNAME1,QUNIT,PNAME1,PUNIT, &
                       CMUNIT,IPCODE,RBEGIN,REND,NREG,IREG,CMNAME1, &
                       ERRCOD,MESCOD)

      END SUBROUTINE F90_ESTIM

      !estimator:dploti
      SUBROUTINE F90_DPLOTI

        DLL_EXPORT F90_DPLOTI

        CALL DPLOTI

      END SUBROUTINE F90_DPLOTI

      !estimator:gboxpn
      SUBROUTINE F90_GBOXPN(INUM)

        DLL_EXPORT F90_GBOXPN

        INTEGER, INTENT(OUT) :: INUM

        CALL GBOXPN(INUM)

      END SUBROUTINE F90_GBOXPN

      !estimator:gboxp
      SUBROUTINE F90_GBOXP_XX(IBPLOT,NGROUP,NI,X,ITITLE)
        dll_export F90_GBOXP_XX

        INTEGER,          INTENT(IN)  :: IBPLOT
        INTEGER,          INTENT(OUT) :: NGROUP,NI(100),ITITLE(*)
        DOUBLE PRECISION, INTENT(OUT) :: X(3000)

        CHARACTER(LEN=1)              :: TITLE(80)
        INTEGER                       :: I

        CALL GBOXP (IBPLOT, &
                    NGROUP,NI,X,TITLE)

        DO 10 I = 1,80
          ITITLE(I)= ICHAR(TITLE(I))
 10     CONTINUE

      END SUBROUTINE F90_GBOXP_XX

      !estimator:gplotp
      SUBROUTINE F90_GPLOTP_XX(IPLOT,NDATA,NFUN,X,A,RANGE, &
                               ISYM,IXTITL,IYTITL,ITITLE)
        dll_export F90_GPLOTP_XX

        INTEGER,          INTENT(IN)  :: IPLOT
        INTEGER,          INTENT(OUT) :: NDATA,NFUN,ITITLE(*)
        INTEGER,          INTENT(OUT) :: ISYM(10),IXTITL(*),IYTITL(*)
        REAL,             INTENT(OUT) :: RANGE(4)
        DOUBLE PRECISION, INTENT(OUT) :: X(5000),A(5000,10)

        CHARACTER(LEN=1)              :: TITLE(80),XTITLE(80),YTITLE(80)
        CHARACTER(LEN=1)              :: SYMBOL(10)
        INTEGER                       :: I

        CALL GPLOTP (IPLOT, &
                     NDATA,NFUN,X,A,RANGE,SYMBOL, &
                     XTITLE,YTITLE,TITLE)

        DO 10 I = 1,80
          ITITLE(I)= ICHAR(TITLE(I))
          IXTITL(I)= ICHAR(XTITLE(I))
          IYTITL(I)= ICHAR(YTITLE(I))
 10     CONTINUE
        DO 20 I = 1,10
          ISYM(I)= ICHAR(SYMBOL(I))
 20     CONTINUE

      END SUBROUTINE F90_GPLOTP_XX

      !estimator:gplotn
      SUBROUTINE F90_GPLOTN(INUM)

        DLL_EXPORT F90_GPLOTN

        INTEGER, INTENT(OUT) :: INUM

        CALL GPLOTN(INUM)

      END SUBROUTINE F90_GPLOTN
