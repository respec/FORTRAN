    !local
    INTEGER FUNCTION INQUIRE_NAME(NAME,FUN_DEF)
        CHARACTER*256      :: NAME
        INTEGER            :: FUN_DEF

        CHARACTER*512      :: MSG
        INTEGER            :: FUN_TRY 
        INTEGER,SAVE       :: FUN_BASE = 101
        INTEGER            :: FUN_OPN  
        LOGICAL            :: OPEN

        FUN_OPN  = 0

        IF (FUN_DEF .LE. 0) THEN !try unit numbers starting here
          FUN_TRY = FUN_BASE
        ELSE                     !try beginning based on input arg
          FUN_TRY = FUN_DEF
        END IF

        !WRITE(*,*) 'INQUIRE_NAME:BEG:',FUN_DEF,FUN_TRY,FUN_OPN,FUN_BASE,' ' // TRIM(NAME)
        INQUIRE (FILE=NAME,NUMBER=FUN_OPN,OPENED=OPEN)

        !WRITE(*,*) 'INQUIRE_NAME:INX:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
        IF (OPEN .EQV. .FALSE.) THEN !unit not open
           IF (FUN_DEF .GE. 0) THEN  !don't use old information from file unit table
             FUN_OPN = 0
           END IF
        END IF

        DO WHILE (FUN_OPN .EQ. 0) !assign first available unit number to the file
           !WRITE(*,*) 'INQUIRE_NAME:INF:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
           IF (OPEN) THEN  !open, try the next one
             !WRITE(*,*) 'INQUIRE_NAME:INQ:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
             FUN_TRY = FUN_TRY+ 1
           ELSE            !this will be it
             !WRITE(*,*) 'INQUIRE_NAME:DON:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
             FUN_OPN = FUN_TRY 
             IF (FUN_DEF .LT. 0) THEN !don't reuse unit number 
               FUN_BASE= FUN_BASE+ 1
               !WRITE(*,*) 'INQUIRE_NAME:FBS:',FUN_DEF,FUN_OPN,FUN_BASE
             END IF
           END IF
        END DO

        !WRITE(*,*) 'INQUIRE_NAME:ASN:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
        INQUIRE_NAME = FUN_OPN
    END FUNCTION INQUIRE_NAME
    
    FUNCTION F90_WDMOPN(UNIT,NAME)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDMOPN
        INTEGER           :: F90_WDMOPN
        INTEGER           :: UNIT
        CHARACTER(LEN=*)  :: NAME

        CHARACTER*256     :: MSG
        LOGICAL           :: OPEN
            
        INQUIRE(UNIT=UNIT,OPENED=OPEN)
        
        !WRITE(*,*) 'F90_WDMOPN:',OPEN,' ',TRIM(NAME)
        IF (OPEN) THEN
          F90_WDMOPN= 1
        ELSE
          OPEN(UNIT=UNIT,FILE=NAME,ACCESS='DIRECT',RECL=2048,STATUS='OLD',ERR=99)
          F90_WDMOPN= 0
        END IF
        RETURN
 99     CONTINUE
        WRITE(*,*) 'Error opening ',UNIT,NAME
        F90_WDMOPN=-1
    END FUNCTION F90_WDMOPN  
    
    FUNCTION F90_WDMCLO(UNIT)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDMCLO
        INTEGER           :: F90_WDMCLO
        INTEGER           :: UNIT, IOS
            
        CHARACTER*256     :: MSG

        !WRITE(*,*) 'F90_WDMCLO:UNIT',UNIT
        CLOSE(UNIT=UNIT,ERR=99,IOSTAT=IOS)
  
        !WRITE(MSG,*) '                    NO PROBLEM'
        F90_WDMCLO=0
        !mark as unusable
        CALL WDFLNU(UNIT)
        RETURN

 99     CONTINUE
        WRITE(*,*) '                    Error code ',MOD(IOS,16384)

        F90_WDMCLO= -1
    END FUNCTION F90_WDMCLO
    
    !adwdm:wdopxx
    FUNCTION F90_WDBOPN(RWFLG,WDNAME) RESULT(WDMSFL)
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_WDBOPN

        CHARACTER(LEN=*),INTENT(IN) :: WDNAME
        INTEGER,         INTENT(IN) :: RWFLG
        INTEGER                     :: WDMSFL

        CHARACTER*256               :: LNAME
        INTEGER                     :: RETCOD

        LNAME = WDNAME
        
        IF (RWFLG .EQ. 1) THEN
          !read only, assign special number
          WDMSFL = INQUIRE_NAME(LNAME,100)
          !WRITE(*,*) 'F90_WDBOPN:READONLY'
        ELSE
          WDMSFL= INQUIRE_NAME(LNAME,0)
        END IF
        
        CALL GET_WDM_FUN(WDMSFL)
        !WRITE(*,*) 'F90_WDBOPN:RWFLG,WDMSFL:',RWFLG,WDMSFL
        CALL WDBOPN(WDMSFL,LNAME,RWFLG,RETCOD)
        
        IF (RETCOD .NE. 0) THEN
          IF (RETCOD .LT. -10) THEN
            RETCOD = ABS(RETCOD) - 16384
          END IF
          WRITE(*,*) 'F90_WDBOPN: "'//LNAME,'" retc:',RETCOD
          WDMSFL= 0
        END IF
    END FUNCTION F90_WDBOPN

    !adwdm:wdopxx
    SUBROUTINE F90_WDBOPNR(RWFLG,WDNAME,WDMSFL,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_WDBOPNR

        CHARACTER(LEN=*),INTENT(IN) :: WDNAME
        INTEGER,         INTENT(IN) :: RWFLG
        INTEGER                     :: WDMSFL
        INTEGER                     :: RETCOD

        CHARACTER*256               :: LNAME

        LNAME = WDNAME
        !WDMSFL= INQUIRE_NAME(LNAME,WDMSFL)

        IF (RWFLG .EQ. 1) THEN
          !special case for hspfmsg.wdm (or any other readonly file)
          WDMSFL = 100
        ELSE
          WDMSFL= INQUIRE_NAME(LNAME,WDMSFL)
        END IF
        !CALL GET_WDM_FUN(WDMSFL)

        !WRITE(*,*) 'F90_WDBOPNR:entr:WDMSFL,RWFLG:',WDMSFL,RWFLG,' ',TRIM(LNAME)

        !IF (WDMSFL .LT. 0) THEN
        !  WDMSFL = -WDMSFL
        !END IF

        CALL WDBOPN(WDMSFL,LNAME,RWFLG,RETCOD)

        IF (RETCOD .NE. 0) THEN
          IF (RETCOD .LT. -10) THEN
            RETCOD = ABS(RETCOD) - 16384
          END IF
          WDMSFL= 0
        END IF
        !WRITE(*,*) 'F90_WDBOPNR:exit:WDMSFL,RETCOD',WDMSFL,RETCOD

    END SUBROUTINE F90_WDBOPNR

    !local
    SUBROUTINE GET_WDM_FUN(WDMSFL)
        INTEGER          :: WDMSFL

        LOGICAL          :: OPFG
        INTEGER          :: NXTWDM

        !WRITE(*,*)  'GET_WDM_FUN:entry:WDMSFL:',WDMSFL

        IF (WDMSFL .GE. 100) THEN
          NXTWDM = WDMSFL
        ELSE
          NXTWDM = 101
        END IF
        WDMSFL = 0
        DO WHILE (WDMSFL .EQ. 0)
          INQUIRE (UNIT=NXTWDM,OPENED=OPFG)
          IF (OPFG) THEN
            NXTWDM= NXTWDM+ 1
          ELSE
            WDMSFL= NXTWDM
          END IF
        END DO
        !WRITE(*,*) 'GET_WDM_FUN:exit :WDMSFL:',WDMSFL
    END SUBROUTINE

    !adwdm:utwdmd
    FUNCTION F90_WDFLCL(WDMSFL) RESULT (RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDFLCL

        INTEGER, INTENT(IN) :: WDMSFL
        INTEGER             :: RETCOD
 
        CHARACTER*200 :: FNAM
        INTEGER       :: FUN
        LOGICAL       :: OPEN
            
        !WRITE(*,*) 'F90_WDFLCL:entry:WDMSFL:',WDMSFL

        INQUIRE(UNIT=WDMSFL,OPENED=OPEN,NAME=FNAM)

        IF (OPEN) THEN
          CALL WDFLCL(WDMSFL,RETCOD)
          !WRITE(*,*) 'F90_WDFLCL:close:WDMSFL:RETCOD:', WDMSFL,RETCOD

          INQUIRE(UNIT=WDMSFL,OPENED=OPEN)
          !WRITE(*,*) "F90_WDFLCL:opned:WDMSFL:", WDMSFL,OPEN
          INQUIRE(FILE=FNAM,NUMBER=FUN,OPENED=OPEN)
          !WRITE(*,*) "F90_WDFLCL:final:WDMSFL:", FUN,OPEN,' ',TRIM(FNAM)
        ELSE
          !not open, cant close it
          WRITE(*,*) 'F90_WDFLCL:not open'
          RETCOD = -255
        END IF
    END FUNCTION F90_WDFLCL

    FUNCTION F90_WDFLCL2(WDMSFL) RESULT (RETCOD)
        !Note this function is not from hass_ent!
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDFLCL2

        INTEGER  :: WDMSFL, RETCOD

        CLOSE (WDMSFL)
        RETCOD = 0
    END FUNCTION F90_WDFLCL2

    !adwdm:utdwmd
    FUNCTION F90_WDCKDT(WDMSFL,DSN) RESULT(DSTYP)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDCKDT
        INTEGER  :: WDMSFL,DSN,DSTYP,WDCKDT

        DSTYP = WDCKDT(WDMSFL,DSN)
    END FUNCTION F90_WDCKDT

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
    
    !wdm:wdtms1
    SUBROUTINE F90_WDTPUT (WDMSFL,DSN,DELT,DATES,NVAL, &
                           DTOVWR,QUALFG,TUNITS,RVAL, &
                           RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDTPUT
        INTEGER           :: WDMSFL,DSN,DELT,DATES(6),NVAL,DTOVWR
        INTEGER           :: QUALFG,TUNITS,RETCOD
        REAL              :: RVAL(NVAL)
        CHARACTER*256     :: MSG

        !WRITE(*,*) 'F90_WDTPUT:call:', &
        !  wdmsfl,dsn,delt,dates,nval,dtovwr,qualfg,tunits
        CALL WDTPUT(WDMSFL,DSN,DELT,DATES,NVAL,DTOVWR,QUALFG,TUNITS,RVAL, &
                    RETCOD)
        !WRITE(*,*) 'F90_WDTPUT:exit:', RETCOD
    END SUBROUTINE F90_WDTPUT

    !wdm:wdtms2
    SUBROUTINE F90_WTFNDT (WDMSFL,DSN, &
                           SDAT,EDAT,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WTFNDT
        INTEGER           :: WDMSFL,DSN
        INTEGER           :: SDAT(6),EDAT(6),RETCOD

        INTEGER           :: GPFLG
        INTEGER           :: TDSFRC

        GPFLG = 1
        !WRITE(*,*) 'F90_WTFNDT:',WDMSFL,DSN,GPFLG
        CALL WTFNDT (WDMSFL,DSN,GPFLG, &
                     TDSFRC,SDAT,EDAT,RETCOD)
        !WRITE(*,*) 'F90_WTFNDT:',RETCOD,TDSFRC
    END SUBROUTINE F90_WTFNDT
    
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

    !wdm:wdbtch
    SUBROUTINE F90_WDBSGI (WDMSFL,DSN,SAIND,SALEN, &
                           SAVAL,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSGI
        INTEGER            :: WDMSFL,DSN,SAIND,SALEN,RETCOD
        INTEGER            :: SAVAL(SALEN)

        CALL WDBSGI(WDMSFL,DSN,SAIND,SALEN, &
                    SAVAL,RETCOD)
    END SUBROUTINE F90_WDBSGI

    !wdm:wdbtch
    SUBROUTINE F90_WDBSGR (WDMSFL,DSN,SAIND,SALEN, &
                           SAVAL,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSGR
        INTEGER            :: WDMSFL,DSN,SAIND,SALEN,RETCOD
        REAL               :: SAVAL(SALEN)

        CALL WDBSGR(WDMSFL,DSN,SAIND,SALEN, &
                    SAVAL,RETCOD)
    END SUBROUTINE F90_WDBSGR

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
