      !adwdm:wdopxx
      FUNCTION F90_WDBOPN(WDNAME) RESULT(WDMSFL)
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_WDBOPN

        CHARACTER(LEN=*),INTENT(IN) :: WDNAME
        INTEGER                     :: WDMSFL

        INTEGER                     :: RWFLG
        CHARACTER*64                :: LNAME
        INTEGER                     :: RETCOD
        CHARACTER*3                 :: WSTAT

        RWFLG = 0
        LNAME = WDNAME
        CALL GET_WDM_FUN(WDMSFL)
        !WRITE(*,*) 'F90_WDBOPN:RWFLG,WDMSFL:',RWFLG,WDMSFL
        CALL WDBOPN(WDMSFL,LNAME,RWFLG,RETCOD)
        !WRITE(*,*) 'F90_WDBOPN:',RWFLG,WSTAT
        IF (RETCOD .NE. 0) THEN
          IF (RETCOD .LT. -10) THEN
            RETCOD = ABS(RETCOD) - 16384
          END IF
          WRITE(*,*) 'F90_WDBOPN: "'//LNAME,'" retc:',RETCOD
          WDMSFL= 0
        END IF
      END FUNCTION F90_WDBOPN

      !local
      SUBROUTINE GET_WDM_FUN(WDMSFL)
        INTEGER      :: WDMSFL
        INTEGER,SAVE :: NXTWDM
        LOGICAL      :: OPFG
        DATA NXTWDM/0/

        IF (WDMSFL == -1) THEN
          DO WHILE (NXTWDM > 0)
            INQUIRE (UNIT=NXTWDM,OPENED=OPFG)
            IF (OPFG) THEN
              !WRITE(*,*) 'GET_WDM_FUN:close:',NXTWDM
              CLOSE(NXTWDM)
            END IF
            NXTWDM = NXTWDM - 1
            IF (NXTWDM < 40) THEN
              NXTWDM = 0
            END IF
          END DO
        ELSE
          IF (NXTWDM == 0) THEN
            NXTWDM = 40
          END IF
          WDMSFL = NXTWDM
          NXTWDM = NXTWDM+ 1
        END IF
      END SUBROUTINE

      !adwdm:utwdmd
      FUNCTION F90_WDFLCL(WDMSFL) RESULT (RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDFLCL

        INTEGER  :: WDMSFL, RETCOD

        CALL WDFLCL(WDMSFL,RETCOD)
      END FUNCTION F90_WDFLCL

      FUNCTION F90_WDFLCL2(WDMSFL) RESULT (RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDFLCL2

        INTEGER  :: WDMSFL, RETCOD

        CLOSE (WDMSFL)
        RETCOD = 0
      END FUNCTION F90_WDFLCL2

      !wdm:wdtms1
      SUBROUTINE F90_WDTGET(WDMSFL,DSN,DELT,DATES,NVAL, &
                           DTRAN,TUNITS, &
                           RVAL,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDTGET
        INTEGER           :: WDMSFL,DSN,DELT,DATES(6),NVAL,DTRAN
        INTEGER           :: QUALFG,TUNITS,RETCOD
        REAL              :: RVAL(NVAL)

        QUALFG = 0
        CALL WDTGET(WDMSFL,DSN,DELT,DATES,NVAL,DTRAN,QUALFG,TUNITS, &
                    RVAL,RETCOD)
      END SUBROUTINE F90_WDTGET

      !adwdm:utdwmd
      FUNCTION F90_WDCKDT(WDMSFL,DSN) RESULT(DSTYP)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDCKDT
        INTEGER  :: WDMSFL,DSN,DSTYP,WDCKDT

        DSTYP = WDCKDT(WDMSFL,DSN)
      END FUNCTION F90_WDCKDT

      !wdm:wdbtch
      SUBROUTINE F90_WDBSGC (WDMSFL,DSN,SAIND,SALEN,IVAL)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSGC
        INTEGER           :: WDMSFL,DSN,IVAL(*),RETCOD,I,SAIND,SALEN
        CHARACTER(LEN=1)  :: CSAVAL(80)

        CALL WDBSGC(WDMSFL,DSN,SAIND,SALEN, &
                    CSAVAL,RETCOD)
        IF (RETCOD == 0) THEN
          DO I = 1,SALEN
            IVAL(I) = ICHAR(CSAVAL(I))
          END DO
        ELSE
          DO I= 1,SALEN
            IVAL(I) = 32
          END DO
        END IF
      END SUBROUTINE F90_WDBSGC

      !wdm:wdtms2
      SUBROUTINE F90_WTFNDT (WDMSFL,DSN, &
                             SDAT,EDAT,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WTFNDT
        INTEGER           :: WDMSFL,DSN
        INTEGER           :: SDAT(6),EDAT(6),RETCOD

        INTEGER           :: GPFLG
        INTEGER           :: TDSFRC

        GPFLG = 1
        !WRITE(*,*) 'HASS_ENT:F90_WTFNDT:',WDMSFL,DSN,GPFLG
        CALL WTFNDT (WDMSFL,DSN,GPFLG, &
                     TDSFRC,SDAT,EDAT,RETCOD)
        !WRITE(*,*) 'HASS_ENT:F90_WTFNDT:',RETCOD,TDSFRC
      END SUBROUTINE F90_WTFNDT

      !wdm:wdbtch
      SUBROUTINE F90_WDDSCL (WDMSFL,DSN,NWDMFL,NDSN, &
                             RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDDSCL
        INTEGER           :: WDMSFL,DSN,NWDMFL,NDSN
        INTEGER           :: RETCOD

        INTEGER           :: I

        I = 0
        CALL WDDSCL (WDMSFL,DSN,NWDMFL,NDSN, &
                     I,RETCOD)

      END SUBROUTINE F90_WDDSCL
