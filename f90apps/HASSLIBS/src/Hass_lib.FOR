      !util:utchar
      SUBROUTINE F90_DATLST_XX(DATE,IDSTR,LEN,ERRCOD)
        dll_export F90_DATLST_XX

        INTEGER,          INTENT(IN)  :: DATE(6)
        INTEGER,          INTENT(OUT) :: IDSTR(21),LEN,ERRCOD

        CHARACTER(LEN=1)              :: DSTRNG(21)
        INTEGER                       :: I,J

        CALL DATLST(DATE,DSTRNG,LEN,ERRCOD)
        J = 0
        I = 0
        DO
          J = J+ 1
          I = I+ 1
          IF (I == 10) THEN
            IF (DSTRNG(I+1) == ' ') THEN
              IDSTR(J) = 32                 ! single digit day, move right
              J  = J+ 1
              LEN= LEN+ 1
            END IF
          END IF
          IF (J <= LEN) THEN
            IDSTR(J)= ICHAR(DSTRNG(I))
          ELSE
            EXIT
          END IF
        END DO

      END SUBROUTINE F90_DATLST_XX

      !util:utchar
      SUBROUTINE F90_DECCHX_XX(REAIN,LEN,SIGDIG,DECPLA,ISTR)
        dll_export F90_DECCHX_XX

        INTEGER,          INTENT(IN)  :: LEN,SIGDIG,DECPLA
        REAL,             INTENT(IN)  :: REAIN
        INTEGER,          INTENT(OUT) :: ISTR(*)

        CHARACTER(LEN=1)              :: STR(20)
        INTEGER                       :: I

        CALL DECCHX(REAIN,LEN,SIGDIG,DECPLA,STR)
        DO I = 1,LEN
          ISTR(I) = ICHAR(STR(I))
        END DO

      END SUBROUTINE F90_DECCHX_XX

      !util:utdate
      SUBROUTINE F90_CMPTIM(TCODE1,TSTEP1,TCODE2,TSTEP2,TSTEPF,TCDCMP)
        dll_export F90_CMPTIM

        INTEGER,       INTENT(IN)  :: TCODE1,TSTEP1,TCODE2,TSTEP2
        INTEGER,       INTENT(OUT) :: TSTEPF,TCDCMP

        CALL CMPTIM(TCODE1,TSTEP1,TCODE2,TSTEP2, &
                    TSTEPF,TCDCMP)

      END SUBROUTINE F90_CMPTIM

      !util:utdate
      SUBROUTINE F90_DATNXT(INTRVL,UPBACK,DATEX)
        dll_export F90_DATNXT

        INTEGER,       INTENT(IN)    :: INTRVL,UPBACK
        INTEGER,       INTENT(INOUT) :: DATEX(6)

        CALL DATNXT(INTRVL, UPBACK, &
                    DATEX)

      END SUBROUTINE F90_DATNXT

      !util:utdate
      FUNCTION F90_DAYMON (YR,MO)
        dll_export F90_DAYMON

        INTEGER,       INTENT(IN)    :: YR,MO
        INTEGER                      :: F90_DAYMON
        INTEGER                      :: DAYMON

        F90_DAYMON = DAYMON(YR,MO)

      END FUNCTION F90_DAYMON


      !util:utdate
      SUBROUTINE F90_TIMADD(DATE1,TCODE,TSTEP,NVALS,DATE2)
        dll_export F90_TIMADD
        INTEGER,       INTENT(IN)  :: DATE1(6),TCODE,TSTEP,NVALS
        INTEGER,       INTENT(OUT) :: DATE2(6)

        CALL TIMADD(DATE1,TCODE,TSTEP,NVALS,DATE2)

      END SUBROUTINE F90_TIMADD

      !util:utdate
      SUBROUTINE F90_TIMDIF(DATE1,DATE2,TCODE,TSTEP,NVALS)
        dll_export F90_TIMDIF
        INTEGER,       INTENT(IN)  :: DATE1(6),DATE2(6),TCODE,TSTEP
        INTEGER,       INTENT(OUT) :: NVALS

        CALL TIMDIF(DATE1,DATE2,TCODE,TSTEP,NVALS)

      END SUBROUTINE F90_TIMDIF

      !util:utdate
      SUBROUTINE F90_TIMCNV(DATE1)
        dll_export F90_TIMCNV
        INTEGER,       INTENT(INOUT) :: DATE1(6)

        CALL TIMCNV(DATE1)

      END SUBROUTINE F90_TIMCNV

      !util:utdate
      SUBROUTINE F90_TIMCVT(DATE1)
        dll_export F90_TIMCVT
        INTEGER,       INTENT(INOUT) :: DATE1(6)

        CALL TIMCVT(DATE1)

      END SUBROUTINE F90_TIMCVT

      !util:utdate
      SUBROUTINE F90_TIMBAK(TCODE,DATE1)
        dll_export F90_TIMBAK
        INTEGER,       INTENT(IN)    :: TCODE
        INTEGER,       INTENT(INOUT) :: DATE1(6)

        CALL TIMBAK(TCODE,DATE1)

      END SUBROUTINE F90_TIMBAK

      !util:utdate
      FUNCTION F90_TIMCHK (DATE1,DATE2)
        dll_export F90_TIMCHK

        INTEGER,       INTENT(IN)    :: DATE1(6),DATE2(6)
        INTEGER                      :: F90_TIMCHK
        INTEGER                      :: TIMCHK

        F90_TIMCHK = TIMCHK(DATE1,DATE2)

      END FUNCTION F90_TIMCHK

      !util:utdate
      SUBROUTINE F90_JDMODY (YEAR,JDAY,MON,DAY)
        dll_export F90_JDMODY

        INTEGER,       INTENT(IN)    :: YEAR,JDAY
        INTEGER,       INTENT(OUT)   :: MON,DAY

        CALL JDMODY(YEAR,JDAY,MON,DAY)

      END SUBROUTINE F90_JDMODY

      !util:utdate
      SUBROUTINE F90_DTMCMN(NDAT,STRT,STOP,TSTEP,TCODE, &
                            SDAT,EDAT,TS,TC,RETCOD)
        dll_export F90_DTMCMN
        INTEGER,       INTENT(IN)  :: NDAT,TCODE,TSTEP
        INTEGER,       INTENT(IN)  :: STRT(6,NDAT),STOP(6,NDAT)
        INTEGER,       INTENT(OUT) :: SDAT(6),EDAT(6),TS,TC,RETCOD

        CALL DTMCMN(NDAT,STRT,STOP,TSTEP,TCODE, &
                    SDAT,EDAT,TS,TC,RETCOD)
      END SUBROUTINE F90_DTMCMN

      !util:utnumb
      SUBROUTINE F90_DECPRC(SIGDIG,DECPLA, &
                            RVAL)
        dll_export F90_DECPRC
        INTEGER,    INTENT(IN)     :: SIGDIG,DECPLA
        REAL,       INTENT(INOUT)  :: RVAL

        CALL DECPRC(SIGDIG,DECPLA, &
                    RVAL)
      END SUBROUTINE F90_DECPRC

      !util:utsort
      SUBROUTINE F90_ASRTRP (CNT,RVAL)
        dll_export F90_ASRTRP
        INTEGER        :: CNT
        REAL           :: RVAL(CNT)

        CALL ASRTRP(CNT, &
                    RVAL)

      END SUBROUTINE F90_ASRTRP
