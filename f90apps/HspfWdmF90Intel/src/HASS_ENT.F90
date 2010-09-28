

      !local utility used throughout this file
      SUBROUTINE LOG_MSG(MSG)
        CHARACTER(LEN=256),SAVE:: ERROR_FILE_NAME
        CHARACTER(LEN=*) :: MSG
        CHARACTER(LEN=1) :: C
        CHARACTER(LEN=10):: TIME, DATE, ZONE
        CHARACTER(LEN=15):: TIMEX
        INTEGER          :: DT(8)
        INTEGER          :: IO_STATUS
        LOGICAL          :: EXIST_FLAG
        LOGICAL          :: OPEN_FLAG
        LOGICAL,SAVE     :: WRITE_FLAG = .FALSE.
        
        !DATA ERROR_FILE_NAME /'C:\TEMP\ERROR.FIL'/
        DATA ERROR_FILE_NAME /'ERROR.FIL'/

        INQUIRE(FILE=ERROR_FILE_NAME, &
                EXIST=EXIST_FLAG, &
                OPENED=OPEN_FLAG, &
                IOSTAT=IO_STATUS,ERR=98)

        !WRITE(*,*) OPEN_FLAG,EXIST_FLAG,IO_STATUS,TRIM(MSG)

        IF (TRIM(MSG) .EQ. 'WRITE') THEN
          WRITE_FLAG = .TRUE.
        END IF

        TIMEX = ""
        CALL DATE_AND_TIME(DATE,TIME,ZONE,DT)
        TIMEX = TIME(1:2) // ":" // TIME(3:4) // ":" // TIME(5:10) // " : "
        
        IF (TRIM(MSG) .EQ. 'OPEN' .OR. TRIM(MSG) .EQ. 'WRITE') THEN 
          IF (.NOT. OPEN_FLAG) THEN
            IF (EXIST_FLAG) THEN 
              OPEN(UNIT=99,FILE=ERROR_FILE_NAME,POSITION='APPEND',ACTION='READWRITE', &
                   ERR=98,IOSTAT=IO_STATUS,STATUS='OLD')
            ELSE
              OPEN(UNIT=99,FILE=ERROR_FILE_NAME,POSITION='APPEND',ACTION='READWRITE', &
                   ERR=98,IOSTAT=IO_STATUS,STATUS='NEW')
            END IF
            WRITE(99,*) TIMEX // 'LOG_MSG:ERROR.FIL OPENED'
          ELSE
            WRITE(99,*) TIMEX // 'LOG_MSG:ERROR.FIL ALREADY OPEN'
          END IF
        ELSE IF (TRIM(MSG) .EQ. 'CLOSE') THEN
          IF (OPEN_FLAG) THEN
            WRITE(99,*) TIMEX // 'LOG_MSG:ERROR.FIL CLOSING'
            CLOSE(99)
            OPEN_FLAG = .FALSE.
          END IF
          WRITE_FLAG = .FALSE.
        ELSE IF (OPEN_FLAG .AND. WRITE_FLAG) THEN
          WRITE(99,*) TIMEX // TRIM(MSG)
          CALL FLUSH(99)
        END IF

        RETURN

 98     CONTINUE
        WRITE (*,*) 'Error ',MOD(IO_STATUS,16384),' opening ERROR.FIL',OPEN_FLAG
        INQUIRE(99,ERR=99,IOSTAT=IO_STATUS,OPENED=OPEN_FLAG)
 99     CONTINUE
        WRITE (*,*) 'Status',MOD(IO_STATUS,16384),OPEN_FLAG
        READ(*,*) C
        WRITE (*,*) C

      END SUBROUTINE


      !used by atcWDM
      SUBROUTINE F90_MSG(MSG)
        !DEC$ ATTRIBUTES DLLEXPORT :: f90_msg
        !DEC$ ATTRIBUTES STDCALL   :: f90_msg
        !DEC$ ATTRIBUTES REFERENCE :: msg

        CHARACTER(LEN=256) :: MSG

        CALL LOG_MSG(MSG)

      END SUBROUTINE F90_MSG


      !local utility used throughout this file
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

        WRITE(MSG,*) 'HASS_ENT:INQUIRE_NAME:BEG:',FUN_DEF,FUN_TRY,FUN_OPN,FUN_BASE,' ' // TRIM(NAME)
        CALL LOG_MSG(MSG)

        INQUIRE (FILE=NAME,NUMBER=FUN_OPN,OPENED=OPEN)

        WRITE(MSG,*) 'HASS_ENT:INQUIRE_NAME:INX:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
        CALL LOG_MSG(MSG)

        IF (OPEN .EQV. .FALSE.) THEN !unit not open
           IF (FUN_DEF .GE. 0) THEN  !don't use old information from file unit table
             FUN_OPN = 0
           ELSE                      !use old unit number (vb6 code)
             WRITE(MSG,*) 'HASS_ENT:INQUIRE_NAME:OLD:USE:',FUN_OPN
           END IF
        END IF

        DO WHILE (FUN_OPN .EQ. 0) !assign first available unit number to the file
           WRITE(MSG,*) 'HASS_ENT:INQUIRE_NAME:INF:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
           CALL LOG_MSG(MSG)

           IF (OPEN) THEN  !open, try the next one
             WRITE(MSG,*) 'HASS_ENT:INQUIRE_NAME:INQ:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
             CALL LOG_MSG(MSG)
             FUN_TRY = FUN_TRY+ 1
           ELSE            !this will be it
             WRITE(MSG,*) 'HASS_ENT:INQUIRE_NAME:DON:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
             CALL LOG_MSG(MSG)
             FUN_OPN = FUN_TRY 
             IF (FUN_DEF .LT. 0) THEN !don't reuse unit number (vb6 code)
               FUN_BASE= FUN_BASE+ 1
               WRITE(MSG,*) 'HASS_ENT:INQUIRE_NAME:FBS:',FUN_DEF,FUN_OPN,FUN_BASE
             END IF
           END IF
        END DO

        WRITE(MSG,*) 'HASS_ENT:INQUIRE_NAME:ASN:',FUN_DEF,FUN_TRY,FUN_OPN,OPEN
        CALL LOG_MSG(MSG)

        INQUIRE_NAME = FUN_OPN

      END FUNCTION INQUIRE_NAME


      !used by atcWDM, HSPFEngineNet, and WinHSPFLt
      SUBROUTINE F90_W99OPN()
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_W99OPN
        CALL LOG_MSG('WRITE')
      END SUBROUTINE F90_W99OPN

      
      !adwdm:wdopxx
      !used by HSPFEngineNet
      FUNCTION F90_WDBOPN(RWFLG,WDNAME) RESULT(WDMSFL) 
        !DEC$ ATTRIBUTES DLLEXPORT :: f90_wdbopn
        !DEC$ ATTRIBUTES STDCALL   :: f90_wdbopn
        !DEC$ ATTRIBUTES REFERENCE :: rwflg,wdname

        CHARACTER(LEN=256),INTENT(IN) :: WDNAME
        INTEGER,         INTENT(IN) :: RWFLG
        INTEGER                     :: WDMSFL

        CHARACTER(LEN=384)            :: MSG
        INTEGER                     :: RETCOD

        IF (RWFLG .EQ. 1) THEN
          !read only, assign special number
          WDMSFL = INQUIRE_NAME(WDNAME,100)
          WRITE(MSG,*) 'HASS_ENT:F90_WDBOPN:READONLY'
          CALL LOG_MSG(MSG)
        ELSE
          WDMSFL= INQUIRE_NAME(WDNAME,0)
        END IF
        
        CALL GET_WDM_FUN(WDMSFL)
 
        WRITE(MSG,*) 'HASS_ENT:F90_WDBOPN:RWFLG,WDMSFL:',RWFLG,WDMSFL,' ',TRIM(WDNAME)
        CALL LOG_MSG(MSG)

        CALL WDBOPN(WDMSFL,LNAME,RWFLG,RETCOD)
        
        IF (RETCOD .NE. 0) THEN
          IF (RETCOD .LT. -10) THEN
            RETCOD = ABS(RETCOD) - 16384
          END IF
          !WRITE(*,*) 'HASS_ENT:F90_WDBOPN: "'//LNAME,'" retc:',RETCOD
          WDMSFL= 0
        END IF

        WRITE(MSG,*) 'HASS_ENT:F90_WDBOPN:RETCOD,WDMSFL:',RETCOD,WDMSFL
        CALL LOG_MSG(MSG)

      END FUNCTION F90_WDBOPN


      !adwdm:wdopxx
      !used by atcWDM and WinHSPFLt
      SUBROUTINE F90_WDBOPNR(RWFLG,WDNAME,WDMSFL,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: f90_wdbopnr
        !DEC$ ATTRIBUTES STDCALL   :: f90_wdbopnr
        !DEC$ ATTRIBUTES REFERENCE :: rwflg, wdname, wdmsfl, retcod

        CHARACTER(LEN=256),INTENT(IN) :: wdname
        INTEGER,         INTENT(IN)   :: rwflg
        INTEGER                       :: wdmsfl
        INTEGER                       :: retcod

        CHARACTER*384                 :: MSG
        CHARACTER*256                 :: LNAME

        LNAME = WDNAME
        !WDMSFL= INQUIRE_NAME(LNAME,WDMSFL)

        IF (RWFLG .EQ. 1) THEN
          !special case for hspfmsg.wdm (or any other readonly file)
          WDMSFL = 100
        ELSE
          WDMSFL= INQUIRE_NAME(LNAME,WDMSFL)
        END IF
        !CALL GET_WDM_FUN(WDMSFL)

        WRITE(MSG,*) 'HASS_ENT:F90_WDBOPNR:entr:WDMSFL,RWFLG:',WDMSFL,RWFLG,' ',TRIM(LNAME)
        CALL LOG_MSG(MSG)

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

        WRITE(MSG,*) 'HASS_ENT:F90_WDBOPNR:exit:WDMSFL,RETCOD',WDMSFL,RETCOD
        CALL LOG_MSG(MSG)

      END SUBROUTINE F90_WDBOPNR


      !local used by WDBOPN
      SUBROUTINE GET_WDM_FUN(WDMSFL)
        INTEGER          :: WDMSFL

        LOGICAL          :: OPFG
        CHARACTER*256    :: MSG
        INTEGER          :: NXTWDM

        WRITE(MSG,*)  'HASS_ENT:GET_WDM_FUN:entry:WDMSFL:',WDMSFL
        CALL LOG_MSG(MSG)

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

        WRITE(MSG,*) 'HASS_ENT:GET_WDM_FUN:exit :WDMSFL:',WDMSFL
        CALL LOG_MSG(MSG)

      END SUBROUTINE


      !adwdm:utwdmd
      !used by atcWDM
      FUNCTION F90_WDFLCL(WDMSFL) RESULT (RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDFLCL

        INTEGER, INTENT(IN) :: WDMSFL
        INTEGER             :: RETCOD
 
        CHARACTER*384 :: MSG
        CHARACTER*256 :: FNAM
        INTEGER       :: FUN
        LOGICAL       :: OPEN
            
        WRITE(MSG,*) 'HASS_ENT:F90_WDFLCL:entry:WDMSFL:',WDMSFL
        CALL LOG_MSG(MSG)

        INQUIRE(UNIT=WDMSFL,OPENED=OPEN,NAME=FNAM)

        IF (OPEN) THEN
          CALL WDFLCL(WDMSFL,RETCOD)
          WRITE(MSG,*) 'HASS_ENT:F90_WDFLCL:close:WDMSFL:RETCOD:', &
             WDMSFL,RETCOD
          CALL LOG_MSG(MSG)

          INQUIRE(UNIT=WDMSFL,OPENED=OPEN)
          WRITE(MSG,*) "HASS_ENT:F90_WDFLCL:opned:WDMSFL:", &
             WDMSFL,OPEN
          CALL LOG_MSG(MSG)
          INQUIRE(FILE=FNAM,NUMBER=FUN,OPENED=OPEN)
          WRITE(MSG,*) "HASS_ENT:F90_WDFLCL:final:WDMSFL:", &
             FUN,OPEN,' ',TRIM(FNAM)
          CALL LOG_MSG(MSG)
        ELSE
          !not open, cant close it
          WRITE(MSG,*) 'HASS_ENT:F90_WDFLCL:not open'
          CALL LOG_MSG(MSG)
          RETCOD = -255
        END IF
      END FUNCTION F90_WDFLCL

      FUNCTION f90_inqnam (nam) RESULT (file_unit)
        !check to see if a file name is open
        !DEC$ ATTRIBUTES DLLEXPORT :: f90_inqnam
        !DEC$ ATTRIBUTES STDCALL   :: f90_inqnam
        !DEC$ ATTRIBUTES REFERENCE :: rwflg, wdname, wdmsfl, retcod


        INTEGER          :: file_unit
        CHARACTER(LEN=*) :: nam

        CHARACTER*384    :: MSG
        CHARACTER*256    :: LNAM
        INTEGER          :: FUN = 0
        LOGICAL          :: OPEN

        WRITE(LNAM,*) NAM
        INQUIRE (FILE=LNAM,NUMBER=FILE_UNIT,OPENED=OPEN)

        WRITE(MSG,*) 'HASS_ENT:F90_INQNAM:',FILE_UNIT,OPEN,' ',TRIM(LNAM)
        CALL LOG_MSG(MSG)

        IF (.NOT. OPEN) THEN     
          FILE_UNIT = 0
        END IF

      END FUNCTION F90_INQNAM

 
      !adwdm:utwdmd
      !used by HSPFEngineNet and WinHSPFLt
      SUBROUTINE F90_WDBFIN()
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBFIN

        CALL WDBFIN
      END SUBROUTINE F90_WDBFIN


      !adwdm:utwdmd
      !used by atcWDM
      SUBROUTINE F90_WDDSNX(WDMSFL,DSN)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDDSNX
        INTEGER           :: WDMSFL,DSN

        CALL WDDSNX(WDMSFL,DSN)
      END SUBROUTINE F90_WDDSNX


      !adwdm:utdwmd
      !used by atcWDM
      FUNCTION F90_WDCKDT(WDMSFL,DSN) RESULT(DSTYP)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDCKDT
        INTEGER  :: WDMSFL,DSN,DSTYP,WDCKDT
        !CHARACTER*256      :: MSG

        DSTYP = WDCKDT(WDMSFL,DSN)

        !WRITE(MSG,*) 'WDCKDT:',WDMSFL,DSN,DSTYP       
        !CALL LOG_MSG(MSG)
        
      END FUNCTION F90_WDCKDT


      !wdm:wdtms1
      !used by atcWDM
      SUBROUTINE F90_WDTGET(WDMSFL,DSN,DELT,DATES,NVAL, &
                           DTRAN,QUALFG,TUNITS, &
                           RVAL,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDTGET
        INTEGER           :: WDMSFL,DSN,DELT,DATES(6),NVAL,DTRAN
        INTEGER           :: QUALFG,TUNITS,RETCOD
        REAL              :: RVAL(NVAL)

        CALL WDTGET(WDMSFL,DSN,DELT,DATES,NVAL,DTRAN,QUALFG,TUNITS, &
                    RVAL,RETCOD)
      END SUBROUTINE F90_WDTGET


      !wdm:wdtms1
      !used by atcWDM
      SUBROUTINE   F90_WDTPUT (WDMSFL,DSN,DELT,DATES,NVAL, &
                               DTOVWR,QUALFG,TUNITS,RVAL, &
                               RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDTPUT
        INTEGER           :: WDMSFL,DSN,DELT,DATES(6),NVAL,DTOVWR
        INTEGER           :: QUALFG,TUNITS,RETCOD
        REAL              :: RVAL(NVAL)
        CHARACTER*256     :: MSG

        WRITE(MSG,*) 'F90_WDTPUT:call:', &
          wdmsfl,dsn,delt,dates,nval,dtovwr,qualfg,tunits
        CALL LOG_MSG(MSG)
        CALL WDTPUT(WDMSFL,DSN,DELT,DATES,NVAL,DTOVWR,QUALFG,TUNITS,RVAL, &
                    RETCOD)
        WRITE(MSG,*) 'F90_WDTPUT:exit:', RETCOD
        CALL LOG_MSG(MSG)

      END SUBROUTINE F90_WDTPUT


      !wdm:wdtms2
      !used by atcWDM
      SUBROUTINE F90_WTFNDT (WDMSFL,DSN,GPFLG, &
                            TDSFRC,SDAT,EDAT,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WTFNDT
        INTEGER           :: WDMSFL,DSN
        INTEGER           :: GPFLG
        INTEGER           :: TDSFRC,SDAT(6),EDAT(6),RETCOD

        !WRITE(*,*) 'HASS_ENT:F90_WTFNDT:',WDMSFL,DSN,GPFLG
        CALL WTFNDT (WDMSFL,DSN,GPFLG, &
                     TDSFRC,SDAT,EDAT,RETCOD)
        !WRITE(*,*) 'HASS_ENT:F90_WTFNDT:',RETCOD,TDSFRC
      END SUBROUTINE F90_WTFNDT


      !wdm:wdbtch
      !used by atcWDM
      SUBROUTINE F90_WDBSGC_XX (WDMSFL,DSN,SAIND,SALEN,IVAL)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSGC_XX
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
      END SUBROUTINE F90_WDBSGC_XX


      !wdm:wdbtch
      !used by atcWDM
      SUBROUTINE F90_WDLBAD (WDMSFL,DSN,DSTYPE, &
                             PSA)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDLBAD
        INTEGER   WDMSFL,DSN,DSTYPE,PSA

        CALL WDLBAD (WDMSFL,DSN,DSTYPE, &
                     PSA)

      END SUBROUTINE F90_WDLBAD


      !wdm:wdbtch
      !used by atcWDM
      SUBROUTINE F90_WDBSGI (WDMSFL,DSN,SAIND,SALEN, &
                             SAVAL,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSGI
        INTEGER            :: WDMSFL,DSN,SAIND,SALEN,RETCOD
        INTEGER            :: SAVAL(SALEN)

        CALL WDBSGI(WDMSFL,DSN,SAIND,SALEN, &
                    SAVAL,RETCOD)

      END SUBROUTINE F90_WDBSGI


      !wdm:wdbtch
      !used by atcWDM
      SUBROUTINE F90_WDBSGR (WDMSFL,DSN,SAIND,SALEN, &
                             SAVAL,RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSGR
        INTEGER            :: WDMSFL,DSN,SAIND,SALEN,RETCOD
        REAL               :: SAVAL(SALEN)

        CALL WDBSGR(WDMSFL,DSN,SAIND,SALEN, &
                    SAVAL,RETCOD)

      END SUBROUTINE F90_WDBSGR


      !wdm:wdbtch
      !used by atcWDM
      SUBROUTINE F90_WDDSRN (WDMSFL,ODSN,NDSN, &
                             RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDDSRN
        INTEGER            :: WDMSFL,ODSN,NDSN,RETCOD

        CALL WDDSRN(WDMSFL,ODSN,NDSN, &
                    RETCOD)

      END SUBROUTINE F90_WDDSRN


      !wdm:wdatrb
      !used by atcWDM
      SUBROUTINE F90_WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD,CVAL)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSAC
        INTEGER           :: WDMSFL,DSN,RETCOD,SAIND,SALEN,MESSFL
        CHARACTER(LEN=*)  :: CVAL

        CHARACTER*1       :: CVAL1(80)
        CHARACTER*80      :: CVAL80

        CVAL80 = CVAL
        CALL CVARAR (SALEN,CVAL80,SALEN,CVAL1)

        CALL WDBSAC(WDMSFL,DSN,MESSFL,SAIND,SALEN, &
                    CVAL1,RETCOD)

      END SUBROUTINE F90_WDBSAC


      !wdm:wdatrb
      !used by atcWDM
      SUBROUTINE F90_WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL, &
                             RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSAI
        INTEGER            :: WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
        INTEGER            :: SAVAL(SALEN)

        CALL WDBSAI(WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL, &
                    RETCOD)

      END SUBROUTINE F90_WDBSAI


      !wdm:wdatrb
      !used by atcWDM
      SUBROUTINE F90_WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL, &
                             RETCOD)
        !DEC$ ATTRIBUTES DLLEXPORT :: F90_WDBSAR
        INTEGER            :: WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
        REAL               :: SAVAL(SALEN)

        CALL WDBSAR(WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL, &
                    RETCOD)

      END SUBROUTINE F90_WDBSAR

 
      !used by HSPFEngineNet and WinHSPFLt
      SUBROUTINE F90_PUTOLV(OUTLEV)
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_PUTOLV
        INTEGER, INTENT(IN)         :: OUTLEV
        CALL PUTOLV(OUTLEV)
      END SUBROUTINE F90_PUTOLV

 
      !adwdm:wdmess
      !used by atcWDM
      SUBROUTINE F90_WDLBAX (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP, &
                             PSA)

        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_WDLBAX

        INTEGER, INTENT(IN)  :: WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP
        INTEGER, INTENT(OUT) :: PSA

        CALL WDLBAX (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP, &
                     PSA)

      END SUBROUTINE F90_WDLBAX


      !adwdm:wdmess
      !used by atcWDM
      SUBROUTINE F90_WDDSDL (WDMSFL,DSN, &
                             RETCOD)

        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_WDDSDL

        INTEGER, INTENT(IN)  :: WDMSFL,DSN
        INTEGER, INTENT(OUT) :: RETCOD
        CHARACTER*256      :: MSG

        WRITE(MSG,*) "HASS_ENT:F90_WDDSDL:ENTRY:",WDMSFL,DSN,RETCOD
        CALL LOG_MSG(MSG)
        CALL WDDSDL (WDMSFL,DSN, &
                     RETCOD)
        WRITE(MSG,*) "HASSENT:F90_WDDSDL:EXIT: ",WDMSFL,DSN,RETCOD
        CALL LOG_MSG(MSG)

      END SUBROUTINE F90_WDDSDL

 
      !wdm:wdsagy
      !used by atcWDM
      SUBROUTINE F90_WDSAGY_XX (WDMSFL,ID, &
                                LEN, TYPE, &
                                ATMIN, ATMAX, ATDEF, &
                                HLEN, HREC, HPOS, VLEN,&
                                ANAME,ADESC,AVALID)
!     get characteristics of this wdm search attribute

        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_WDSAGY_XX

        INTEGER, INTENT(IN)           :: WDMSFL,ID
        INTEGER, INTENT(OUT)          :: LEN,TYPE,ANAME(*),ADESC(*)
        INTEGER, INTENT(OUT)          :: HLEN,HREC,HPOS,AVALID(*),VLEN
        REAL, INTENT(OUT)             :: ATMIN,ATMAX,ATDEF

        INTEGER                 :: I,DPTR,SARQWD,SAUPFG,ILEN
        CHARACTER*6             :: SANAM
        CHARACTER*1             :: SADESC(47),SATVAL(240)

        CALL WDSAGY (WDMSFL,ID, &
                     SANAM,DPTR,TYPE,LEN,SARQWD,SAUPFG)
        IF (DPTR.GT.0) THEN
          CALL WADGDS (WDMSFL,DPTR, &
                       SADESC)
          CALL WADGRA (WDMSFL,DPTR,TYPE, &
                       ATMIN,ATMAX)
          CALL WADGDF (WDMSFL,DPTR,TYPE, &
                       ATDEF)
          ILEN = 240
          CALL WADGVA (WDMSFL,DPTR,ILEN, &
                       VLEN,SATVAL)
          CALL WADGHL (WDMSFL,DPTR, &
                       HLEN,HREC,HPOS)
        END IF

        DO I = 1,6
          ANAME(I) = ICHAR(SANAM(I:I))
        END DO
        DO I = 1,47
          ADESC(I) = ICHAR(SADESC(I))
        END DO
        DO I = 1,240
          AVALID(I) = ICHAR(SATVAL(I))
        END DO

      END SUBROUTINE F90_WDSAGY_XX


      !used by atcWDM
      SUBROUTINE F90_GETATT (WDMSFL,DSN, &
                             INIT, &
                             SAIND,SAVAL)
       
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_GETATT

        INTEGER        :: WDMSFL, DSN, INIT, SAIND, SAVAL(256)

        INTEGER        :: SAPOS
        SAVE           :: SAPOS

        IF (INIT .EQ. 1) THEN
          !first time thru
          SAPOS= 0
          INIT = 0
        END IF

        SAPOS = SAPOS+ 1
        CALL WDGTAT (WDMSFL,DSN,SAPOS,SAIND,SAVAL)

      END SUBROUTINE F90_GETATT

  