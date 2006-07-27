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
              OPEN(UNIT=99,FILE=ERROR_FILE_NAME,POSITION='APPEND',ACTION='DENYNONE', &
                   ERR=98,IOSTAT=IO_STATUS,STATUS='OLD')
            ELSE
              OPEN(UNIT=99,FILE=ERROR_FILE_NAME,POSITION='APPEND',ACTION='DENYNONE', &
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

      SUBROUTINE F90_MSG(MSG)
        DLL_EXPORT F90_MSG

        CHARACTER(LEN=*) :: MSG

        CALL LOG_MSG(MSG)

      END SUBROUTINE F90_MSG

      !local
      INTEGER FUNCTION INQUIRE_NAME(NAME,FUN_DEF)
        CHARACTER*64       :: NAME
        INTEGER            :: FUN_DEF

        CHARACTER*256      :: MSG
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

      !general
      SUBROUTINE F90_W99OPN()
        dll_export F90_W99OPN
        CALL LOG_MSG('WRITE')
      END SUBROUTINE F90_W99OPN

      SUBROUTINE F90_W99CLO()
        dll_export F90_W99CLO
        CALL LOG_MSG('HASS_ENT:F90_W99CLO')
        CALL LOG_MSG('CLOSE')
      END SUBROUTINE F90_W99CLO

      FUNCTION F90_WDMOPN(UNIT,NAME)
        dll_export F90_WDMOPN
        INTEGER           :: F90_WDMOPN
        INTEGER           :: UNIT
        CHARACTER(LEN=*)  :: NAME

        CHARACTER*256     :: MSG
        LOGICAL           :: OPEN
            
        INQUIRE(UNIT=UNIT,OPENED=OPEN)
        
        WRITE(MSG,*) 'HASS_ENT:F90_WDMOPN:',OPEN,' ',TRIM(NAME)
        CALL LOG_MSG(MSG)

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
        dll_export F90_WDMCLO
        INTEGER           :: F90_WDMCLO
        INTEGER           :: UNIT, IOS
            
        CHARACTER*256     :: MSG

        WRITE(MSG,*) 'HASS_ENT:F90_WDMCLO:UNIT',UNIT
        CALL LOG_MSG(MSG)

        CLOSE(UNIT=UNIT,ERR=99,IOSTAT=IOS)
  
        WRITE(MSG,*) '                    NO PROBLEM'
        CALL LOG_MSG(MSG)

        F90_WDMCLO=0

        RETURN

 99     CONTINUE
        WRITE(MSG,*) '                    Error code ',MOD(IOS,16384)
        CALL LOG_MSG(MSG)

        F90_WDMCLO= -1

      END FUNCTION F90_WDMCLO
      
      !adwdm:wdopxx
      FUNCTION F90_WDBOPN(RWFLG,WDNAME) RESULT(WDMSFL) 
        dll_export F90_WDBOPN

        CHARACTER(LEN=*),INTENT(IN) :: WDNAME
        INTEGER,         INTENT(IN) :: RWFLG
        INTEGER                     :: WDMSFL

        CHARACTER*256               :: MSG
        CHARACTER*64                :: LNAME
        INTEGER                     :: RETCOD

        LNAME = WDNAME
        
        IF (RWFLG .EQ. 1) THEN
          !read only, assign special number
          WDMSFL = INQUIRE_NAME(LNAME,100)
          WRITE(MSG,*) 'HASS_ENT:F90_WDBOPN:READONLY'
          CALL LOG_MSG(MSG)
        ELSE
          WDMSFL= INQUIRE_NAME(LNAME,0)
        END IF
        
        CALL GET_WDM_FUN(WDMSFL)
 
        WRITE(MSG,*) 'HASS_ENT:F90_WDBOPN:RWFLG,WDMSFL:',RWFLG,WDMSFL,' ',TRIM(LNAME)
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
      SUBROUTINE F90_WDBOPNR(RWFLG,WDNAME,WDMSFL,RETCOD)
        dll_export F90_WDBOPNR

        CHARACTER(LEN=*),INTENT(IN) :: WDNAME
        INTEGER,         INTENT(IN) :: RWFLG
        INTEGER                     :: WDMSFL
        INTEGER                     :: RETCOD

        CHARACTER*256               :: MSG
        CHARACTER*64                :: LNAME

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

      !local
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
      FUNCTION F90_WDFLCL(WDMSFL) RESULT (RETCOD)
        dll_export F90_WDFLCL

        INTEGER, INTENT(IN) :: WDMSFL
        INTEGER             :: RETCOD
 
        CHARACTER*256 :: MSG
        CHARACTER*200 :: FNAM
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

      FUNCTION F90_INQNAM (NAM)
        !check to see if a file name is open
        dll_export F90_INQNAM

        INTEGER          :: F90_INQNAM
        CHARACTER(LEN=*) :: NAM

        CHARACTER*256    :: MSG
        CHARACTER*200    :: LNAM
        INTEGER          :: FUN = 0
        LOGICAL          :: OPEN

        WRITE(LNAM,*) NAM
        INQUIRE (FILE=LNAM,NUMBER=FUN,OPENED=OPEN)

        WRITE(MSG,*) 'HASS_ENT:F90_INQNAM:',FUN,OPEN,' ',TRIM(LNAM)
        CALL LOG_MSG(MSG)

        IF (OPEN) THEN     
          F90_INQNAM = FUN
        ELSE
          F90_INQNAM = 0
        END IF

      END FUNCTION F90_INQNAM

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

      !adwdm:utwdmd
      SUBROUTINE F90_WDBFIN()
        dll_export F90_WDBFIN

        CALL WDBFIN
      END SUBROUTINE F90_WDBFIN

      !adwdm:utwdmd
      SUBROUTINE F90_WDDSNX(WDMSFL,DSN)
        dll_export F90_WDDSNX
        INTEGER           :: WDMSFL,DSN

        CALL WDDSNX(WDMSFL,DSN)
      END SUBROUTINE F90_WDDSNX

      !adwdm:utdwmd
      FUNCTION F90_WDCKDT(WDMSFL,DSN) RESULT(DSTYP)
        dll_export F90_WDCKDT
        INTEGER  :: WDMSFL,DSN,DSTYP,WDCKDT
        !CHARACTER*256      :: MSG

        DSTYP = WDCKDT(WDMSFL,DSN)

        !WRITE(MSG,*) 'WDCKDT:',WDMSFL,DSN,DSTYP       
        !CALL LOG_MSG(MSG)
        
      END FUNCTION F90_WDCKDT

      !adwdm:ztwdmf
      SUBROUTINE F90_WMSGTX_XX (MESSFL,SCLU,SGRP,                 &
                    LNFLDS,LSCOL,LFLEN,ILFTYP,LAPOS,LIMIN,LIMAX,  &
                    LIDEF,LRMIN,LRMAX,LRDEF,LNMHDR,IHDRBUF,RETCOD)
!       Get selected information about a table 2d parm screen

        dll_export F90_WMSGTX_XX
        INTEGER,INTENT(IN)  :: MESSFL,SCLU,SGRP
        INTEGER,INTENT(OUT) :: LNFLDS,LSCOL(30),LFLEN(30),        &
                               LAPOS(30),LIMIN(30),LIMAX(30),     &
                               LIDEF(30),LNMHDR,RETCOD
        REAL,   INTENT(OUT) :: LRMIN(30),LRMAX(30),LRDEF(30)
        INTEGER,INTENT(OUT) :: ILFTYP(30),IHDRBUF(78,5)

        CHARACTER(LEN=1)    :: LFTYP(30),HDRBUF(78,5)
        INTEGER             :: I,J

        !write(*,*) 'hass_ent:f90_wmsgtx_xx:',messfl,sclu,sgrp
        CALL WMSGTX (MESSFL,SCLU,SGRP,                            &
                     LNFLDS,LSCOL,LFLEN,LFTYP,LAPOS,LIMIN,LIMAX,  &
                     LIDEF,LRMIN,LRMAX,LRDEF,LNMHDR,HDRBUF,RETCOD)
        !write(*,*) 'hass_ent:f90_wmsgtx_xx:',retcod

        DO I = 1,30
          ILFTYP(I) = ICHAR(LFTYP(I))
        END DO
        DO I = 1,5
          DO J = 1,78
            IHDRBUF(J,I) = ICHAR(HDRBUF(J,I))
          END DO
        END DO

      END SUBROUTINE F90_WMSGTX_XX

      !wdm:ztwdmf
      SUBROUTINE F90_WMSGTT_XX (WDMSFL,DSN,GNUM,INITFG, &
                                OLEN,                   &
                                CONT,LOBUFF)
        dll_export F90_WMSGTT_XX
        INTEGER,INTENT(IN)  :: WDMSFL,DSN,GNUM,INITFG
        INTEGER             :: OLEN
        INTEGER,INTENT(OUT) :: CONT,LOBUFF(256)

        CHARACTER(LEN=1)    :: OBUFF(256)
        INTEGER             :: I

        IF (OLEN .GT. 256) THEN
          OLEN = 256
        END IF

        CALL WMSGTT(WDMSFL,DSN,GNUM,INITFG,OLEN,OBUFF,CONT)

        DO I = 1,OLEN
          LOBUFF(I)= ICHAR(OBUFF(I))
        END DO

      END SUBROUTINE F90_WMSGTT_XX

      !wdm:wdtms1
      SUBROUTINE F90_WDTGET(WDMSFL,DSN,DELT,DATES,NVAL, &
                           DTRAN,QUALFG,TUNITS, &
                           RVAL,RETCOD)
        dll_export F90_WDTGET
        INTEGER           :: WDMSFL,DSN,DELT,DATES(6),NVAL,DTRAN
        INTEGER           :: QUALFG,TUNITS,RETCOD
        REAL              :: RVAL(NVAL)

        CALL WDTGET(WDMSFL,DSN,DELT,DATES,NVAL,DTRAN,QUALFG,TUNITS, &
                    RVAL,RETCOD)
      END SUBROUTINE F90_WDTGET

      !wdm:wdtms1
      SUBROUTINE   F90_WDTPUT (WDMSFL,DSN,DELT,DATES,NVAL, &
                               DTOVWR,QUALFG,TUNITS,RVAL, &
                               RETCOD)
        dll_export F90_WDTPUT
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
      SUBROUTINE F90_WTFNDT (WDMSFL,DSN,GPFLG, &
                            TDSFRC,SDAT,EDAT,RETCOD)
        dll_export F90_WTFNDT
        INTEGER           :: WDMSFL,DSN
        INTEGER           :: GPFLG
        INTEGER           :: TDSFRC,SDAT(6),EDAT(6),RETCOD

        !WRITE(*,*) 'HASS_ENT:F90_WTFNDT:',WDMSFL,DSN,GPFLG
        CALL WTFNDT (WDMSFL,DSN,GPFLG, &
                     TDSFRC,SDAT,EDAT,RETCOD)
        !WRITE(*,*) 'HASS_ENT:F90_WTFNDT:',RETCOD,TDSFRC
      END SUBROUTINE F90_WTFNDT

      !wdm:wdbtch
      SUBROUTINE F90_WDBSGC_XX (WDMSFL,DSN,SAIND,SALEN,IVAL)
        dll_export F90_WDBSGC_XX
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
      SUBROUTINE F90_WDLBAD (WDMSFL,DSN,DSTYPE, &
                             PSA)
        dll_export F90_WDLBAD
        INTEGER   WDMSFL,DSN,DSTYPE,PSA

        CALL WDLBAD (WDMSFL,DSN,DSTYPE, &
                     PSA)

      END SUBROUTINE F90_WDLBAD

      !wdm:wdbtch
      SUBROUTINE F90_WDBSGI (WDMSFL,DSN,SAIND,SALEN, &
                             SAVAL,RETCOD)
        dll_export F90_WDBSGI
        INTEGER            :: WDMSFL,DSN,SAIND,SALEN,RETCOD
        INTEGER            :: SAVAL(SALEN)

        CALL WDBSGI(WDMSFL,DSN,SAIND,SALEN, &
                    SAVAL,RETCOD)

      END SUBROUTINE F90_WDBSGI

      !wdm:wdbtch
      SUBROUTINE F90_WDBSGR (WDMSFL,DSN,SAIND,SALEN, &
                             SAVAL,RETCOD)
        dll_export F90_WDBSGR
        INTEGER            :: WDMSFL,DSN,SAIND,SALEN,RETCOD
        REAL               :: SAVAL(SALEN)

        CALL WDBSGR(WDMSFL,DSN,SAIND,SALEN, &
                    SAVAL,RETCOD)

      END SUBROUTINE F90_WDBSGR

      !wdm:wdbtch
      SUBROUTINE F90_WDDSRN (WDMSFL,ODSN,NDSN, &
                             RETCOD)
        dll_export F90_WDDSRN
        INTEGER            :: WDMSFL,ODSN,NDSN,RETCOD

        CALL WDDSRN(WDMSFL,ODSN,NDSN, &
                    RETCOD)

      END SUBROUTINE F90_WDDSRN

      !wdm:wdatrb
      SUBROUTINE F90_WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD,CVAL)
        dll_export F90_WDBSAC
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
      SUBROUTINE F90_WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL, &
                             RETCOD)
        dll_export F90_WDBSAI
        INTEGER            :: WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
        INTEGER            :: SAVAL(SALEN)

        CALL WDBSAI(WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL, &
                    RETCOD)

      END SUBROUTINE F90_WDBSAI

      !wdm:wdatrb
      SUBROUTINE F90_WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL, &
                             RETCOD)
        dll_export F90_WDBSAR
        INTEGER            :: WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
        REAL               :: SAVAL(SALEN)

        CALL WDBSAR(WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL, &
                    RETCOD)

      END SUBROUTINE F90_WDBSAR

      !wdm:wddlg
      SUBROUTINE F90_WDLGET (WDMSFL,DSN,ITYPE,ATT1,ATT2,LEN,ID, &
                             OLEN,DLGBUF,RETCOD)
        dll_export F90_WDLGET
        INTEGER WDMSFL,DSN,ITYPE,ATT1,ATT2,LEN,ID,OLEN,RETCOD
        REAL    DLGBUF(LEN)

        CALL WDLGET (WDMSFL,DSN,ITYPE,ATT1,AT2,LEN,ID, &
                     OLEN,DLGBUF,RETCOD)
      END SUBROUTINE F90_WDLGET

      !wdm:wddlg
      SUBROUTINE F90_WDLLSU (WDMSFL,DSN,ILEN, &
                             OLEN,TYPE,ATT1,ATT2,RETCOD)
        dll_export F90_WDLLSU
        INTEGER   WDMSFL,DSN,ILEN,OLEN,RETCOD
        INTEGER   TYPE(ILEN),ATT1(ILEN),ATT2(ILEN)

        CALL WDLLSU (WDMSFL,DSN,ILEN, &
                     OLEN,TYPE,ATT1,ATT2,RETCOD)
      END SUBROUTINE F90_WDLLSU

      !wdm:wdmid
      SUBROUTINE F90_WDIINI

        dll_export F90_WDIINI

        CALL LOG_MSG('HASS_ENT:F90_WDIINI')
        CALL WDIINI

      END SUBROUTINE F90_WDIINI

      !wdm:wdmid
      SUBROUTINE F90_WIDADD (WDMSFL,MXINTV,WDID)

        dll_export F90_WIDADD

        INTEGER,INTENT(IN)          :: WDMSFL,MXINTV
        CHARACTER(LEN=*),INTENT(IN) :: WDID

        CHARACTER(LEN=4)            :: LWDID

        LWDID = WDID
        CALL WIDADD (WDMSFL,MXINTV,LWDID)

      END SUBROUTINE F90_WIDADD

      !graph:grutil
      SUBROUTINE F90_SCALIT (ITYPE,MMIN,MMAX,PLMN,PLMX)
        dll_export F90_SCALIT
        INTEGER        :: ITYPE
        REAL           :: MMIN,MMAX,PLMN,PLMX

        CALL SCALIT(ITYPE, &
                    MMIN,MMAX,PLMN,PLMX)
        IF (ITYPE .EQ. 2 .AND. PLMN .LT. 1.0) THEN
          PLMN = 1.0
        END IF

      END SUBROUTINE F90_SCALIT

      !newaqt:utdir
      SUBROUTINE F90_TSDRRE (WDMSFL,DATOPT,DOSTUS)
        dll_export F90_TSDRRE
        INTEGER   WDMSFL,DATOPT,DOSTUS
        INTEGER   IOPT,I0
        EXTERNAL  HDMES3

        IOPT= 10
        I0  = 0
        IF (DOSTUS.EQ.0) THEN
          CALL HDMES3(IOPT,'GenScnInitialize')
        ELSE IF (DOSTUS.EQ.1) THEN
          CALL HDMES3(IOPT,'WDM Initialize')
        END IF
        IOPT= 1
        CALL HDMES3(IOPT,'Reading WDM Dataset Information')

        CALL TSDRRE (WDMSFL,DATOPT,I0)

        IOPT= 99
        IF (DOSTUS.EQ.0) THEN
          CALL HDMES3(IOPT,'GenScnInitialize Complete')
        ELSE IF (DOSTUS.EQ.1) THEN
          CALL HDMES3(IOPT,'WDM Initialize Complete')
        END IF

      END SUBROUTINE F90_TSDRRE

      !newaqt:utdir
      SUBROUTINE F90_TSDSIN (WDMSFL,UCIPTH)
        dll_export F90_TSDSIN
        INTEGER          :: WDMSFL
        CHARACTER(LEN=*) :: UCIPTH

        CHARACTER*256    :: MSG
        INTEGER      MXLOC,NLOC,MKSTAT(200),LCID(200),DOSTUS
        REAL         LAT(200),LNG(200)
        CHARACTER*40 LCNAME(200)
        CHARACTER*8  CLOCID(200)
        CHARACTER*64 LUCIPTH

        LUCIPTH= UCIPTH
        MXLOC  = 200
        DOSTUS = 0

        WRITE(MSG,*) 'F90_tsdsvl_xx:ucipth: ',UCIPTH
        CALL LOG_MSG(MSG)

        CALL TSDSVL (WDMSFL,MXLOC,DOSTUS,LUCIPTH, &
                     NLOC,LAT,LNG,MKSTAT,LCID,LCNAME,CLOCID)

      END SUBROUTINE F90_TSDSIN

      !newaqt:utdir
      SUBROUTINE F90_TSDSPC_XX (DSN,  &
                               ISENNM,IRCHNM,ICONNM, &
                               TU,TS,SDATE,EDATE,GRPSIZ)
        dll_export F90_TSDSPC_XX
        INTEGER     DSN,TU,TS,SDATE(6),EDATE(6),GRPSIZ
        INTEGER     ISENNM(8),IRCHNM(8),ICONNM(8)

        CHARACTER*256    :: MSG
        INTEGER     L
        CHARACTER*8 LSENNM,LRCHNM,LCONNM

        CALL TSDSPC (DSN, &
                     LSENNM,LRCHNM,LCONNM, &
                     TU,TS,SDATE,EDATE,GRPSIZ)

        WRITE(MSG,*) 'F90_TSDSPC_XX:',DSN,LSENNM,LRCHNM,LCONNM,TU,TS
        CALL LOG_MSG(MSG)

        DO L = 1,8
          ISENNM(L) = ICHAR(LSENNM(L:L))
          IRCHNM(L) = ICHAR(LRCHNM(L:L))
          ICONNM(L) = ICHAR(LCONNM(L:L))
        END DO

      END SUBROUTINE F90_TSDSPC_XX

      !newaqt:utdir
      SUBROUTINE F90_TSDSCT  &
                           (LNTLOC,LNTSEN,LNTCON,LNTDSN)
        dll_export F90_TSDSCT
        INTEGER     LNTLOC,LNTSEN,LNTCON,LNTDSN

        CALL TSDSCT (LNTLOC,LNTSEN,LNTCON,LNTDSN)

      END SUBROUTINE F90_TSDSCT

      !newaqt:utdir
      SUBROUTINE F90_TSDSUN_XX (LNTLOC,LNTSEN,LNTCON, &
                                IUNLOC,IUNSEN,IUNCON)
        dll_export F90_TSDSUN_XX
        INTEGER     IUNLOC(8,LNTLOC),IUNSEN(8,LNTSEN),IUNCON(8,LNTCON)

        CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:) :: LUNLOC
        CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:) :: LUNSEN
        CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:) :: LUNCON
        CHARACTER(LEN=8)                          :: CTMP
        INTEGER                                   :: I,L

        ALLOCATE (LUNLOC(LNTLOC))
        ALLOCATE (LUNSEN(LNTSEN))
        ALLOCATE (LUNCON(LNTCON))

        CALL TSDSUN (LUNLOC,LUNSEN,LUNCON)

        DO I = 1,LNTLOC
          DO L = 1,8
            CTMP = LUNLOC(I)
            IUNLOC(L,I) = ICHAR(CTMP(L:L))
          END DO
        END DO

        DO I = 1,LNTSEN
          DO L = 1,8
            CTMP = LUNSEN(I)
            IUNSEN(L,I) = ICHAR(CTMP(L:L))
          END DO
        END DO

        DO I = 1,LNTCON
          DO L = 1,8
            CTMP = LUNCON(I)
            IUNCON(L,I) = ICHAR(CTMP(L:L))
          END DO
        END DO

        DEALLOCATE (LUNLOC,LUNSEN,LUNCON)

      END SUBROUTINE F90_TSDSUN_XX

      !newaqt:utdir
      SUBROUTINE F90_TSESPC (SPSEN,SPLOC,SPCON)
        dll_export F90_TSESPC
        CHARACTER(LEN=*) :: SPSEN,SPLOC,SPCON
        CHARACTER*8      :: LSEN,LLOC,LCON

        LSEN= SPSEN
        LLOC= SPLOC
        LCON= SPCON
        CALL TSESPC (LSEN,LLOC,LCON)
      END SUBROUTINE F90_TSESPC

      !newaqt:utdir
      SUBROUTINE F90_TSAPUT (DSN,SPSEN,SPLOC,SPCON)
        dll_export F90_TSAPUT
        INTEGER          :: DSN
        CHARACTER(LEN=*) :: SPSEN,SPLOC,SPCON
        CHARACTER*8      :: LSEN,LLOC,LCON

        LSEN= SPSEN
        LLOC= SPLOC
        LCON= SPCON
        CALL TSAPUT (DSN,LSEN,LLOC,LCON)
      END SUBROUTINE F90_TSAPUT

      !newaqt:utdir
      SUBROUTINE F90_TSDSM (DSN)
        dll_export F90_TSDSM
        INTEGER   DSN

        CALL TSDSM (DSN)
      END SUBROUTINE F90_TSDSM

      !newaqt:tsplot
      SUBROUTINE F90_CMSTRM (START,ENDDT,NSTRM,LSTRM,FSTRM, &
                             BUFMAX,YX,TS,TU,NVALS,         &
                             INDX,                          &
                             CSTRM,STSTRM,ENSTRM,TRSTRM,STRMPK,AVSTVL)
        dll_export F90_CMSTRM
        INTEGER, INTENT(IN)      :: START(6),ENDDT(6),NSTRM,LSTRM,CSTRM
        INTEGER, INTENT(IN)      :: STSTRM(6,NSTRM),ENSTRM(6,NSTRM),BUFMAX
        INTEGER, INTENT(INOUT)   :: INDX(BUFMAX)
        REAL, INTENT(IN)         :: FSTRM,YX(BUFMAX)
        REAL, INTENT(OUT)        :: TRSTRM(NSTRM),STRMPK(NSTRM),AVSTVL(NSTRM)

        CALL CMSTRM (START,ENDDT,NSTRM,LSTRM,FSTRM, &
                     BUFMAX,YX,TS,TU,NVALS,         &
                     INDX,                          &
                     CSTRM,STSTRM,ENSTRM,TRSTRM,STRMPK,AVSTVL)

      END SUBROUTINE F90_CMSTRM

      !newaqt:tsplot
      SUBROUTINE F90_FITLIN (NPTS,BUFMAX,YX, &
                             ACOEF,BCOEF,RSQUAR)
        dll_export F90_FITLIN
        INTEGER, INTENT(IN)      :: NPTS,BUFMAX
        REAL, INTENT(IN)         :: YX(BUFMAX)
        REAL, INTENT(OUT)        :: ACOEF,BCOEF,RSQUAR

        CALL FITLIN (NPTS,BUFMAX,YX, &
                     ACOEF,BCOEF,RSQUAR)

      END SUBROUTINE F90_FITLIN


      !newaqt:tsplot
      SUBROUTINE F90_SGLABL_XX (NDSN,ISENNM,ILOCNM,ICONNM,TU,DTRAN, &
                               WHICH,TYPIND,                        &
                               CNTCON,CNTSEN,CNTLOC,                &
                               IALAB,IYRLAB,IYLLAB,ITITL,           &
                               ILAB,ITRAN,ITUNIT)
        dll_export F90_SGLABL_XX
        INTEGER, INTENT(IN)         :: NDSN,TU,DTRAN
        INTEGER, INTENT(INOUT)      :: WHICH(NDSN),TYPIND(NDSN)
        INTEGER, INTENT(OUT)        :: CNTCON,CNTSEN,CNTLOC
        INTEGER, INTENT(IN)         :: ISENNM(*)
        INTEGER, INTENT(IN)         :: ILOCNM(*)
        INTEGER, INTENT(IN)         :: ICONNM(*)
        INTEGER, INTENT(OUT)        :: ITRAN(8)
        INTEGER, INTENT(OUT)        :: ITUNIT(8)
        INTEGER, INTENT(OUT)        :: IALAB(80)
        INTEGER, INTENT(OUT)        :: IYRLAB(80)
        INTEGER, INTENT(OUT)        :: IYLLAB(80)
        INTEGER, INTENT(OUT)        :: ITITL(240)
        INTEGER, INTENT(OUT)        :: ILAB(20,NDSN)

        INTEGER                                    :: L, I, J
        CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:)  :: LSENNM
        CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:)  :: LLOCNM
        CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:)  :: LCONNM
        CHARACTER(LEN=8)                           :: CLOC,CSEN,CCON
        CHARACTER(LEN=8)                           :: CTRAN
        CHARACTER(LEN=8)                           :: CTUNIT
        CHARACTER(LEN=80)                          :: YRLAB
        CHARACTER(LEN=80)                          :: YLLAB
        CHARACTER(LEN=80)                          :: ALAB
        CHARACTER(LEN=240)                         :: TITL
        CHARACTER(LEN=20),ALLOCATABLE,DIMENSION(:) :: CLAB

        ALLOCATE (LSENNM(NDSN))
        ALLOCATE (LLOCNM(NDSN))
        ALLOCATE (LCONNM(NDSN))
        ALLOCATE (CLAB(NDSN))

        DO I = 1, NDSN
          DO L = 1, 8
            J = (I-1) * 8 + L
            CSEN(L:L) = CHAR(ISENNM(J))
            CLOC(L:L) = CHAR(ILOCNM(J))
            CCON(L:L) = CHAR(ICONNM(J))
          END DO
          LSENNM(I) = CSEN
          LLOCNM(I) = CLOC
          LCONNM(I) = CCON
        END DO

        CALL SGLABL                                        &
                   (NDSN,LSENNM,LLOCNM,LCONNM,TU,DTRAN,    &
                    WHICH,TYPIND,                          &
                    CNTCON,CNTSEN,CNTLOC,                  &
                    ALAB,YRLAB,YLLAB,TITL,CLAB,CTRAN,CTUNIT)

        DO L = 1, 80
          IALAB(L) = ICHAR(ALAB(L:L))
          IYRLAB(L)= ICHAR(YRLAB(L:L))
          IYLLAB(L)= ICHAR(YLLAB(L:L))
        END DO

        DO L = 1, 240
          ITITL(L) = ICHAR(TITL(L:L))
        END DO

        DO L = 1,8
          ITRAN(L) = ICHAR(CTRAN(L:L))
          ITUNIT(L)= ICHAR(CTUNIT(L:L))
        END DO

        DO I = 1, NDSN
          DO L = 1, 20
            ILAB(L,I) = ICHAR(CLAB(I)(L:L))
          END DO
        END DO

        DEALLOCATE (LSENNM,LLOCNM,LCONNM,CLAB)

      END SUBROUTINE F90_SGLABL_XX

      SUBROUTINE F90_PUTOLV(OUTLEV)
        dll_export F90_PUTOLV
        INTEGER, INTENT(IN)         :: OUTLEV
        CALL PUTOLV(OUTLEV)
      END SUBROUTINE F90_PUTOLV

      !newaqt:durbat
      SUBROUTINE F90_DAANST (NVALS,RVALS)

        dll_export F90_DAANST

        INTEGER, INTENT(IN)         :: NVALS
        REAL, INTENT(IN)            :: RVALS(NVALS)

        CALL DAANST(NVALS,RVALS)

      END SUBROUTINE F90_DAANST

      SUBROUTINE F90_DAANWV (DMXLEV,NCINT,RCLINT, &
                             DMXDUR,NDURA,DURAT, &
                             DMXLC,NCRIT,RLCLEV, &
                             SDATE,EDATE,SSDATE,SEDATE,TU,TS,NVALS, &
                             PRFG,LCOUT,LCGTLT, &
                             NUM,FRQNW,SNW,SQNW, &
                             FRQPOS,SPOS,SQPOS,FRQNEG,SNEG,SQNEG, &
                             MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG, &
                             PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG, &
                             DELT,MAX,MINIM,MEAN,SUMSQ,LCTSTO,DURNAM,FILNAM)

        DLL_EXPORT F90_DAANWV

        INTEGER, INTENT(IN)          :: DMXLEV,NCINT,DMXDUR,NDURA
        INTEGER, INTENT(IN)          :: DURAT(DMXDUR),DMXLC,NCRIT
        INTEGER, INTENT(IN)          :: SDATE(6),EDATE(6),SSDATE(2)
        INTEGER, INTENT(IN)          :: SEDATE(2),TU,TS,NVALS
        INTEGER, INTENT(IN)          :: PRFG,LCOUT
        INTEGER, INTENT(IN)          :: LCGTLT
        INTEGER, INTENT(OUT)         :: NUM,LCTSTO(DMXLC)
        REAL, INTENT(IN)             :: RCLINT(DMXLEV)
        REAL, INTENT(IN)             :: RLCLEV(DMXLC,DMXLEV)
        REAL, INTENT(OUT)            :: FRQNW(DMXDUR),SNW(DMXDUR)
        REAL, INTENT(OUT)            :: SQNW(DMXDUR)
        REAL, INTENT(OUT)            :: FRQPOS(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: SPOS(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: SQPOS(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: FRQNEG(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: SNEG(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: SQNEG(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: MNW(DMXDUR),MPOS(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: MNEG(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: PTNW(DMXDUR)
        REAL, INTENT(OUT)            :: PTPOS(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: PTNEG(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: PT1NW(DMXDUR)
        REAL, INTENT(OUT)            :: PT1POS(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: PT1NEG(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: FREVNW(DMXDUR)
        REAL, INTENT(OUT)            :: FREVPS(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: FREVNG(DMXDUR,DMXLEV)
        REAL, INTENT(OUT)            :: DELT,MAX,MINIM,MEAN,SUMSQ
        CHARACTER(LEN=*), INTENT(IN) :: DURNAM,FILNAM

        CHARACTER*80            :: DINFO,FNAME
        INTEGER                 :: PUNIT

        DINFO = DURNAM
        FNAME = FILNAM
        IF (FNAME.NE.' ') THEN
          PUNIT = 10
          OPEN(UNIT=PUNIT,FILE=FNAME,ERR=10)
        ELSE
          PUNIT = 0
        END IF
        GO TO 20
 10     CONTINUE
          PUNIT = 0
 20     CONTINUE

        CALL DAANWV (DINFO, &
                     DMXLEV,NCINT,RCLINT, &
                     DMXDUR,NDURA,DURAT, &
                     DMXLC,NCRIT,RLCLEV, &
                     SDATE,EDATE,SSDATE,SEDATE,TU,TS,NVALS, &
                     PRFG,PUNIT,LCOUT,LCGTLT, &
                     NUM,FRQNW,SNW,SQNW, &
                     FRQPOS,SPOS,SQPOS,FRQNEG,SNEG,SQNEG, &
                     MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG, &
                     PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG, &
                     DELT,MAX,MINIM,MEAN,SUMSQ,LCTSTO)

        IF (PUNIT.GT.0) THEN
          CLOSE(UNIT=PUNIT)
        END IF

      END SUBROUTINE F90_DAANWV

      !awstat:tscbat
      SUBROUTINE F90_TSCBAT (NPTS,YX,NCI,CLASS,BADVAL, &
                             SDATE,EDATE,TU,TS,DTRAN, &
                             ZANB,ZB,ZA,ZBNA,ZAB,TNUM,TSDIF,TPDIF, &
                             TSDIF2,TPDIF2,TBIAS,TPBIAS,STEST,ETOT, &
                             CPCTA,CPCTB, &
                             LABEL1,LABEL2,FILNAM)

        DLL_EXPORT F90_TSCBAT

        INTEGER, INTENT(IN)          :: NPTS,NCI
        INTEGER, INTENT(IN)          :: SDATE(6),EDATE(6)
        INTEGER, INTENT(IN)          :: TU,TS,DTRAN
        REAL, INTENT(IN)             :: CLASS(35),BADVAL(2),YX(NPTS*2)
        INTEGER, INTENT(OUT)         :: ZANB,ZB,ZA,ZBNA,ZAB
        INTEGER, INTENT(OUT)         :: TNUM,ETOT(8)
        REAL, INTENT(OUT)            :: TSDIF,TPDIF
        REAL, INTENT(OUT)            :: TSDIF2,TPDIF2,TBIAS,TPBIAS
        REAL, INTENT(OUT)            :: STEST,CPCTA(35),CPCTB(35)
        CHARACTER(LEN=*), INTENT(IN) :: LABEL1,LABEL2,FILNAM

        CHARACTER*80            :: LAB180,LAB280,FNAME
        CHARACTER*1             :: LTITLE(80,2)
        INTEGER                 :: PUNIT

        LAB180= LABEL1
        LAB280= LABEL2
        CALL CVARAR(80,LAB180,80,LTITLE(1,1))
        CALL CVARAR(80,LAB280,80,LTITLE(1,2))

        FNAME = FILNAM
        IF (FNAME.NE.' ') THEN
          PUNIT = 10
          OPEN(UNIT=PUNIT,FILE=FNAME,ERR=10)
        ELSE
          FNAME = 'compare.out'
          PUNIT = 10
          OPEN(UNIT=PUNIT,FILE=FNAME,ERR=10)
        END IF

        CALL TSCBWD (NPTS,YX,LTITLE, &
                     NCI,CLASS,PUNIT,0,BADVAL, &
                     SDATE,EDATE,TU,TS,DTRAN, &
                     ZANB,ZB,ZA,ZBNA,ZAB,TNUM,TSDIF,TPDIF, &
                     TSDIF2,TPDIF2,TBIAS,TPBIAS,STEST,ETOT, &
                     CPCTA,CPCTB)

        IF (PUNIT.GT.0) THEN
          CLOSE(UNIT=PUNIT)
        END IF
        GO TO 20
 10     CONTINUE
          PUNIT = 0
 20     CONTINUE

      END SUBROUTINE F90_TSCBAT

      !ann:gnmexe
      SUBROUTINE F90_GNMEXE (WDMSFL,DSN,FCTN,NIN,TSTEP1,TUNIT1, &
                             TSDATE,EDATE,RVAL,K1,MESSFL, &
                             RETC, &
                             FILNAM,CSCEN,CLOC,CCONS)

        DLL_EXPORT F90_GNMEXE

        INTEGER, INTENT(IN)          :: WDMSFL,DSN(3),TSTEP1
        INTEGER, INTENT(IN)          :: FCTN, NIN, TUNIT1, MESSFL
        INTEGER, INTENT(IN)          :: TSDATE(6), EDATE(6), K1
        REAL, INTENT(IN)             :: RVAL(2)
        INTEGER, INTENT(OUT)         :: RETC
        CHARACTER(LEN=*), INTENT(IN) :: FILNAM,CSCEN,CLOC,CCONS

        INTEGER    KSIZ
        PARAMETER (KSIZ = 100)

        REAL                    :: C1(KSIZ),C2(KSIZ)
        CHARACTER*80            :: FNAME
        CHARACTER*8             :: SCEN,LOC,CONS
        CHARACTER*1             :: IDSCEN(8),IDLOCN(8),IDCONS(8)
        INTEGER                 :: FLTABL,SAIND,SALEN,TSTEP(3),TUNIT(3)
        INTEGER                 :: I,TBLSIZ,ERRFLG,SUM,KOUNT

        RETC = 0
        TSTEP(1) = TSTEP1
        TSTEP(2) = TSTEP1
        TSTEP(3) = TSTEP1
        TUNIT(1) = TUNIT1
        TUNIT(2) = TUNIT1
        TUNIT(3) = TUNIT1

        !output data set does not exist, copy input attrib
        CALL WDDSCL (WDMSFL,DSN(2),WDMSFL,DSN(1),0,RETC)

        IF (RETC.EQ.0) THEN
          !put attribute for scenario name
          SAIND = 288
          SALEN = 8
          SCEN = CSCEN
          CALL CVARAR(SALEN,SCEN,SALEN,IDSCEN)
          CALL WDBSAC (WDMSFL,DSN(1),MESSFL,SAIND,SALEN,IDSCEN,RETC)
          !put attribute for constituent name
          SAIND = 289
          SALEN = 8
          CONS = CCONS
          CALL CVARAR(SALEN,CONS,SALEN,IDCONS)
          CALL WDBSAC (WDMSFL,DSN(1),MESSFL,SAIND,SALEN,IDCONS,RETC)
          !put attribute for location name
          SAIND = 290
          SALEN = 8
          LOC = CLOC
          CALL CVARAR(SALEN,LOC,SALEN,IDLOCN)
          CALL WDBSAC (WDMSFL,DSN(1),MESSFL,SAIND,SALEN,IDLOCN,RETC)
        END IF

        IF (RETC.EQ.0 .AND. FCTN.EQ.24) THEN
          !open file for table
          FNAME = FILNAM
          IF (FNAME.NE.' ') THEN
            FLTABL = 10
            OPEN(UNIT=FLTABL,FILE=FNAME,ERR=10)
            I = 0
 162        CONTINUE
              I = I + 1
              READ (FLTABL,*,END=165,ERR=165) C1(I), C2(I)
              IF (I.GT.1) THEN
                !C1 must be increasing function
                IF (C1(I) .LE. C1(I-1)) THEN
                  !problem with C1 values
                  RETC = 1
                END IF
              END IF
            IF (I .LT. KSIZ) GO TO 162
            I = I + 1
 165        CONTINUE
            TBLSIZ = I - 1
            IF (TBLSIZ .LE. 1) THEN
              !not enough values read
              RETC = 1
            END IF
            !close table file
            IF (FLTABL.GT.0) THEN
              CLOSE(UNIT=FLTABL)
            END IF
          ELSE
            RETC = 1
          END IF
        ELSE IF (RETC.EQ.0) THEN
          !table size is 1 for all other options
          TBLSIZ= 1
          C1(1) = RVAL(1)
          C2(1) = RVAL(2)
        END IF

        IF (RETC.EQ.0) THEN
          CALL GNMEXE (WDMSFL,DSN,FCTN,NIN,TSTEP,TUNIT,TSDATE,EDATE, &
                       TBLSIZ,C1,C2,K1, &
                       ERRFLG,SUM,KOUNT)
          RETC = ERRFLG
          IF (RETC.EQ.0) THEN
            !if no errors add to time series buffer
            !CALL TSDSGN (WDMSFL,DSN(1))
          END IF
        END IF

        GO TO 20
 10     CONTINUE
          RETC = 1
 20     CONTINUE

      END SUBROUTINE F90_GNMEXE

      !ann:gmanex
      SUBROUTINE F90_GMANEX (MESSFL,WDMSFL,DSN,TSTEP,TUNIT,SDATE,EDATE, &
                             INTERP,C1,DEXIST, &
                             ERRCOD,CRECOD, &
                             CSCEN,CLOC,CCONS)

        DLL_EXPORT F90_GMANEX

        INTEGER, INTENT(IN)          :: WDMSFL,DSN,MESSFL,TSTEP,TUNIT
        INTEGER, INTENT(IN)          :: SDATE(6),EDATE(6),INTERP,DEXIST
        REAL, INTENT(IN)             :: C1
        INTEGER, INTENT(OUT)         :: ERRCOD,CRECOD
        CHARACTER(LEN=*), INTENT(IN) :: CSCEN,CLOC,CCONS

        CHARACTER*8             :: SCEN,LOC,CONS
        CHARACTER*1             :: IDSCEN(8),IDLOCN(8),IDCONS(8)
        CHARACTER*1             :: TSTYPE(4)
        INTEGER                 :: SAIND,SALEN,RETC

        !write (*,*) 'in f90_gmanex',dsn,tstep,tunit,sdate,edate,interp,c1
        CALL GMANEX (MESSFL,WDMSFL,DSN,TSTEP,TUNIT,SDATE,EDATE, &
                     INTERP,C1, &
                     ERRCOD,CRECOD)
        !write (*,*) 'completed gmanex',errcod,crecod

        IF (CRECOD.EQ.1 .AND. DEXIST.EQ.0) THEN
          !created this data set, put attribute for scenario name
          SAIND = 288
          SALEN = 8
          SCEN = CSCEN
          CALL CVARAR(SALEN,SCEN,SALEN,IDSCEN)
          CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,IDSCEN,RETC)
          !put attribute for constituent name
          SAIND = 289
          SALEN = 8
          CONS = CCONS
          CALL CVARAR(SALEN,CONS,SALEN,IDCONS)
          CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,IDCONS,RETC)
          !put attribute for tstype
          SAIND = 1
          SALEN = 4
          CONS = CCONS
          CALL CVARAR(SALEN,CONS,SALEN,TSTYPE)
          CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,TSTYPE,RETC)
          !put attribute for location name
          SAIND = 290
          SALEN = 8
          LOC = CLOC
          CALL CVARAR(SALEN,LOC,SALEN,IDLOCN)
          CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,IDLOCN,RETC)
          IF (RETC.EQ.0) THEN
            !if no errors add to time series buffer
            !CALL TSDSGN (WDMSFL,DSN)
          END IF
        END IF

      END SUBROUTINE F90_GMANEX

      !ann:gnttrn
      SUBROUTINE F90_GNTTRN (MESSFL,WDMSFL,DSN,NAORO, &
                             DATES,TRANF,TSTEP,TUNIT, &
                             CRTFLG,CMPFLG,RETCOD,FLAG1, &
                             CSCEN,CLOC,CCONS)

        DLL_EXPORT F90_GNTTRN

        INTEGER, INTENT(IN)          :: WDMSFL,DSN(2),MESSFL,NAORO
        INTEGER, INTENT(IN)          :: DATES(6,3),TRANF,TSTEP,TUNIT
        INTEGER, INTENT(OUT)         :: CRTFLG,CMPFLG,RETCOD,FLAG1
        CHARACTER(LEN=*), INTENT(IN) :: CSCEN,CLOC,CCONS

        CHARACTER*8             :: SCEN,LOC,CONS
        CHARACTER*1             :: IDSCEN(8),IDLOCN(8),IDCONS(8)
        INTEGER                 :: SAIND,SALEN,RETC,DUMDAT(6,2)
        INTEGER                 :: ATRIBI(2),LRET,FLGTS,FLGTU,I
        INTEGER                 :: DTRAN,NUMI,NUMR,INDXI(2),INDXR(1)
        INTEGER                 :: ITMP(6),FLAG2
        REAL                    :: ATRIBR(1)

        DTRAN = TRANF

        FLAG1 = 0
        IF (NAORO.EQ.2 .OR. NAORO.EQ.3) THEN
          !data set exists, check it
          NUMI    = 2
          INDXI(1)= 33
          INDXI(2)= 17
          NUMR    = 1
          INDXR(1)= 32
          CALL WDAINF (WDMSFL,DSN(2),NUMI,NUMR,INDXI,INDXR, &
                       DUMDAT,ATRIBI,ATRIBR,LRET)
          IF (LRET .EQ. 0 .OR. LRET .EQ. -6 .OR. LRET .EQ. -107) THEN
            !data set exists, are time steps compatible?
            CALL CMPTIM ( TUNIT, TSTEP, ATRIBI(2), ATRIBI(1), &
                          FLGTS, FLGTU )
            IF (FLGTS .EQ. 1  .OR.  FLGTU .EQ. -1) THEN
              !time steps are not compatible
              FLAG1 = 2
            ELSE
              FLAG1 = 0
              DO 10 I = 1, 3
                CALL COPYI (6,DATES(1,I),ITMP)
                CALL CKBDRY ( ITMP, TSTEP, TUNIT, FLAG2 )
                IF (FLAG2 .NE. 0) THEN
                  !date is not compatible with time step
                  FLAG1 = 1
                END IF
 10           CONTINUE
            END IF
          END IF
        END IF

        CRTFLG = 0
        RETCOD = 0
        IF (FLAG1.EQ.0) THEN
          !write (*,*) dates
          CALL GNTTRN (MESSFL,WDMSFL,DSN,NAORO, &
                       DATES,DTRAN,TSTEP,TUNIT, &
                       CRTFLG,CMPFLG,RETCOD)
        END IF

        IF (CRTFLG.EQ.1 .AND. RETCOD.EQ.0) THEN
          !created this data set, put attribute for scenario name
          SAIND = 288
          SALEN = 8
          SCEN = CSCEN
          CALL CVARAR(SALEN,SCEN,SALEN,IDSCEN)
          CALL WDBSAC (WDMSFL,DSN(2),MESSFL,SAIND,SALEN,IDSCEN,RETC)
          !put attribute for constituent name
          SAIND = 289
          SALEN = 8
          CONS = CCONS
          CALL CVARAR(SALEN,CONS,SALEN,IDCONS)
          CALL WDBSAC (WDMSFL,DSN(2),MESSFL,SAIND,SALEN,IDCONS,RETC)
          !put attribute for location name
          SAIND = 290
          SALEN = 8
          LOC = CLOC
          CALL CVARAR(SALEN,LOC,SALEN,IDLOCN)
          CALL WDBSAC (WDMSFL,DSN(2),MESSFL,SAIND,SALEN,IDLOCN,RETC)
          IF (RETC.EQ.0) THEN
            !if no errors add to time series buffer
            !CALL TSDSGN (WDMSFL,DSN(2))
          END IF
        END IF

      END SUBROUTINE F90_GNTTRN

      !newaqt:umakpr
      SUBROUTINE F90_UMAKPR (WDMSFL,OUTLEV,RESMFG,RUNFG, &
                             SPOUT,UNIT,SDATIM,EDATIM,EXPFG,RETCOD, &
                             RCHNAM,CONNAM,ARENAM,FTBNAM,LUNAM, &
                             METNAM,NEWNAME,RNINFO)
!     supervisor subroutine to prepare to create a uci file
!     from gis output files

        DLL_EXPORT F90_UMAKPR
        EXTERNAL   LENSTR

        INTEGER, INTENT(IN)          :: WDMSFL,OUTLEV,RESMFG,RUNFG
        INTEGER, INTENT(IN)          :: SPOUT,UNIT,SDATIM(5),EDATIM(5),EXPFG
        INTEGER, INTENT(OUT)         :: RETCOD
        CHARACTER(LEN=*), INTENT(IN) :: RCHNAM,CONNAM,ARENAM,FTBNAM,LUNAM
        CHARACTER(LEN=*), INTENT(IN) :: METNAM,NEWNAME,RNINFO

        CHARACTER*80            :: NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        CHARACTER*80            :: INFO80,WDMNAM
        CHARACTER*8             :: CSCEN
        INTEGER                 :: FILES(5),UFTAB,RDBFG(5)

        CSCEN = NEWNAME
        INFO80= RNINFO

        NAME1 = CONNAM
        FILES(1) = 11
        OPEN(UNIT=FILES(1),FILE=NAME1,ERR=10)
        ILEN = LENSTR(80,NAME1)
        IF (NAME1(ILEN-2:ILEN).EQ.'RDB' .OR. &
            NAME1(ILEN-2:ILEN).EQ.'rdb') THEN
          RDBFG(1) = 1
        ELSE
          RDBFG(1) = 0
        END IF

        NAME2 = RCHNAM
        FILES(2) = 12
        OPEN(UNIT=FILES(2),FILE=NAME2,ERR=10)
        ILEN = LENSTR(80,NAME2)
        IF (NAME2(ILEN-2:ILEN).EQ.'RDB' .OR. &
            NAME2(ILEN-2:ILEN).EQ.'rdb') THEN
          RDBFG(2) = 1
        ELSE
          RDBFG(2) = 0
        END IF

        NAME3 = LUNAM
        IF (NAME3.NE.'<none>') THEN
          FILES(3) = 13
          OPEN(UNIT=FILES(3),FILE=NAME3,ERR=10)
          ILEN = LENSTR(80,NAME3)
          IF (NAME3(ILEN-2:ILEN).EQ.'RDB' .OR. &
              NAME3(ILEN-2:ILEN).EQ.'rdb') THEN
            RDBFG(3) = 1
          ELSE
            RDBFG(3) = 0
          END IF
        ELSE
          FILES(3) = 0
          RDBFG(3) = 0
        END IF

        NAME4 = ARENAM
        FILES(4) = 14
        OPEN(UNIT=FILES(4),FILE=NAME4,ERR=10)
        ILEN = LENSTR(80,NAME4)
        IF (NAME4(ILEN-2:ILEN).EQ.'RDB' .OR. &
            NAME4(ILEN-2:ILEN).EQ.'rdb') THEN
          RDBFG(4) = 1
        ELSE
          RDBFG(4) = 0
        END IF

        NAME5 = METNAM
        IF (NAME5.NE.'<none>') THEN
          FILES(5) = 15
          OPEN(UNIT=FILES(5),FILE=NAME5,ERR=10)
          ILEN = LENSTR(80,NAME5)
          IF (NAME5(ILEN-2:ILEN).EQ.'RDB' .OR. &
              NAME5(ILEN-2:ILEN).EQ.'rdb') THEN
            RDBFG(5) = 1
          ELSE
            RDBFG(5) = 0
          END IF
        ELSE
          FILES(5) = 0
          RDBFG(5) = 0
        END IF

        NAME6 = FTBNAM
        UFTAB = 16
        OPEN(UNIT=UFTAB,FILE=NAME6,ERR=10)

        INQUIRE(UNIT=WDMSFL,NAME=WDMNAM)

        !write(*,*) 'into umakpr'
        CALL UMAKPR (CSCEN, &
                     OUTLEV,RESMFG,RUNFG,SPOUT,UNIT, &
                     SDATIM,EDATIM,INFO80,EXPFG, &
                     WDMNAM,WDMSFL,UFTAB,FILES,RDBFG, &
                     RETCOD)
        !write(*,*) 'out of umakpr'

        CLOSE(UNIT=FILES(1))
        CLOSE(UNIT=FILES(2))
        IF (FILES(3).GT.0) THEN
          CLOSE(UNIT=FILES(3))
        END IF
        CLOSE(UNIT=FILES(4))
        IF (FILES(5).GT.0) THEN
          CLOSE(UNIT=FILES(5))
        END IF
        CLOSE(UNIT=UFTAB)
 10     CONTINUE

      END SUBROUTINE F90_UMAKPR

      !newaqt:umakdo
      SUBROUTINE F90_UMAKDO (MESSFL,EXPFG,NEWNAME)
!     make a new uci file

        DLL_EXPORT F90_UMAKDO

        INTEGER, INTENT(IN)          :: MESSFL
        INTEGER, INTENT(IN)          :: EXPFG
        CHARACTER(LEN=*), INTENT(IN) :: NEWNAME

        CHARACTER*80            :: UCINAM
        INTEGER                 :: MESSU,OUTFL

        UCINAM= NEWNAME // '.UCI'
        OUTFL = 17
        OPEN(UNIT=OUTFL,FILE=UCINAM,ERR=10)

        MESSU = 99

        !write(*,*) 'into umakdo'
        CALL UMAKDO (MESSFL,MESSU,OUTFL, &
                     EXPFG)
        !write(*,*) 'out of umakdo'

        CLOSE(UNIT=OUTFL)
 10     CONTINUE

      END SUBROUTINE F90_UMAKDO

      !newaqt:simnet
      SUBROUTINE F90_SIMNET (IRANK,RLARGE,RSMALL,RETCOD, &
                             RCHNAM,CONNAM,ARENAM,LUNAM, &
                             ORCHNAM,OCONNAM,OARENAM)
!     simplify a network of reaches

        DLL_EXPORT F90_SIMNET
        EXTERNAL   LENSTR

        INTEGER, INTENT(IN)          :: IRANK
        REAL, INTENT(IN)             :: RLARGE,RSMALL
        INTEGER, INTENT(OUT)         :: RETCOD
        CHARACTER(LEN=*), INTENT(IN) :: RCHNAM,CONNAM,ARENAM,LUNAM
        CHARACTER(LEN=*), INTENT(IN) :: ORCHNAM,OCONNAM,OARENAM

        CHARACTER*80            :: NAME1,NAME2,NAME3,NAME4
        CHARACTER*80            :: ONAME1,ONAME2,ONAME4
        INTEGER                 :: FILES(5),RDBFG(5),FUNRCH,FUNARE,FUNCON
        INTEGER                 :: EXPFG

        NAME1 = CONNAM
        FILES(1) = 11
        !write (*,*) 'opening ',name1,files(1)
        OPEN(UNIT=FILES(1),FILE=NAME1,ERR=10)
        ILEN = LENSTR(80,NAME1)
        IF (NAME1(ILEN-2:ILEN).EQ.'RDB' .OR. &
            NAME1(ILEN-2:ILEN).EQ.'rdb') THEN
          RDBFG(1) = 1
        ELSE
          RDBFG(1) = 0
        END IF

        NAME2 = RCHNAM
        FILES(2) = 12
        !write (*,*) 'opening ',name2,files(2)
        OPEN(UNIT=FILES(2),FILE=NAME2,ERR=10)
        ILEN = LENSTR(80,NAME2)
        IF (NAME2(ILEN-2:ILEN).EQ.'RDB' .OR. &
            NAME2(ILEN-2:ILEN).EQ.'rdb') THEN
          RDBFG(2) = 1
        ELSE
          RDBFG(2) = 0
        END IF

        NAME3 = LUNAM
        IF (NAME3.NE.'<none>') THEN
          FILES(3) = 13
          !write (*,*) 'opening ',name3,files(3)
          OPEN(UNIT=FILES(3),FILE=NAME3,ERR=10)
          ILEN = LENSTR(80,NAME3)
          IF (NAME3(ILEN-2:ILEN).EQ.'RDB' .OR. &
              NAME3(ILEN-2:ILEN).EQ.'rdb') THEN
            RDBFG(3) = 1
          ELSE
            RDBFG(3) = 0
          END IF
          EXPFG = 1
        ELSE
          FILES(3) = 0
          RDBFG(3) = 0
          EXPFG = 0
        END IF

        NAME4 = ARENAM
        FILES(4) = 14
        !write (*,*) 'opening ',name4,files(4)
        OPEN(UNIT=FILES(4),FILE=NAME4,ERR=10)
        ILEN = LENSTR(80,NAME4)
        IF (NAME4(ILEN-2:ILEN).EQ.'RDB' .OR. &
            NAME4(ILEN-2:ILEN).EQ.'rdb') THEN
          RDBFG(4) = 1
        ELSE
          RDBFG(4) = 0
        END IF

        FILES(5) = 0
        RDBFG(5) = 0

        ONAME1 = OCONNAM
        FUNCON = 16
        !write (*,*) 'opening ',oname1,funcon
        OPEN(UNIT=FUNCON,FILE=ONAME1,ERR=10)

        ONAME2 = ORCHNAM
        FUNRCH = 17
        !write (*,*) 'opening ',oname2,funrch
        OPEN(UNIT=FUNRCH,FILE=ONAME2,ERR=10)

        ONAME4 = OARENAM
        FUNARE = 18
        !write (*,*) 'opening ',oname4,funare
        OPEN(UNIT=FUNARE,FILE=ONAME4,ERR=10)

        !write (*,*) 'about to call gisrea with files = ',files
        CALL GISREA (FILES,RDBFG,EXPFG, &
                     RETCOD)
        !check for proper reach ordering
        IF (RETCOD.GE.0) THEN
          CALL CHKRCH (RETCOD)
        END IF
        !write (*,*) 'returned from gisrea with retcod = ',retcod
        IF (RETCOD.GE.0) THEN
          !write (*,*) 'calling gissim'
          CALL GISSIM (IRANK,RLARGE,RSMALL, &
                       FUNRCH,FUNARE,FUNCON, &
                       RETCOD)
          !write (*,*) 'returned from gissim with retcod = ',retcod
        END IF

        GO TO 20
 10     CONTINUE
          RETCOD = -2
 20     CONTINUE

        CLOSE(UNIT=FILES(1))
        CLOSE(UNIT=FILES(2))
        IF (FILES(3).GT.0) THEN
          CLOSE(UNIT=FILES(3))
        END IF
        CLOSE(UNIT=FILES(4))
        IF (FILES(5).GT.0) THEN
          CLOSE(UNIT=FILES(5))
        END IF
        CLOSE(UNIT=FUNARE)
        CLOSE(UNIT=FUNCON)
        CLOSE(UNIT=FUNRCH)

      END SUBROUTINE F90_SIMNET

      !newaqt:ucgnrc
      SUBROUTINE F90_UCGNRC (NR)
!     get number of reaches for new uci

        DLL_EXPORT F90_UCGNRC

        INTEGER, INTENT(OUT)         :: NR

        CALL UCGNRC (NR)

      END SUBROUTINE F90_UCGNRC

      !newaqt:ucgnco
      SUBROUTINE F90_UCGNCO (NC)
!     get number of reach connections for new uci

        DLL_EXPORT F90_UCGNCO

        INTEGER, INTENT(OUT)         :: NC

        CALL UCGNCO (NC)

      END SUBROUTINE F90_UCGNCO

      !newaqt:ucgnla
      SUBROUTINE F90_UCGNLA (NL)
!     get number of land uses for new uci

        DLL_EXPORT F90_UCGNLA

        INTEGER, INTENT(OUT)         :: NL

        CALL UCGNLA (NL)

      END SUBROUTINE F90_UCGNLA

      !newaqt:ucgnex
      SUBROUTINE F90_UCGNEX (NE)
!     get number of external source data sets for new uci

        DLL_EXPORT F90_UCGNEX

        INTEGER, INTENT(OUT)         :: NE

        CALL UCGNEX (NE)

      END SUBROUTINE F90_UCGNEX

      !newaqt:ucgnme
      SUBROUTINE F90_UCGNME (NM,NE)
!     get number of met segments for new uci

        DLL_EXPORT F90_UCGNME

        INTEGER, INTENT(OUT)         :: NM,NE

        CALL UCGNME (NM,NE)

      END SUBROUTINE F90_UCGNME

      !newaqt:ucgout
      SUBROUTINE F90_UCGOUT (NR,OUT)
!     get number of external targets for new uci

        DLL_EXPORT F90_UCGOUT

        INTEGER, INTENT(IN)          :: NR
        INTEGER, INTENT(OUT)         :: OUT(*)

        CALL UCGOUT (NR,OUT)

      END SUBROUTINE F90_UCGOUT

      !newaqt:ucgirc
      SUBROUTINE F90_UCGIRC_XX (IR,INAME)
!     get name of this reach for new uci

        DLL_EXPORT F90_UCGIRC_XX

        INTEGER, INTENT(IN)     :: IR
        INTEGER, INTENT(OUT)    :: INAME(12)

        INTEGER                 :: L
        CHARACTER*12            :: CNAME

        CALL UCGIRC (IR, &
                     CNAME)
        DO L = 1,12
          INAME(L) = ICHAR(CNAME(L:L))
        END DO

      END SUBROUTINE F90_UCGIRC_XX

      !iowdm:watinp
      SUBROUTINE F90_WATINI (WATNAM,INWAT)

        DLL_EXPORT F90_WATINI

        INTEGER, INTENT(IN)          :: INWAT
        CHARACTER(LEN=*), INTENT(IN) :: WATNAM

        CHARACTER*80            :: FNAME

        FNAME = WATNAM
        OPEN(UNIT=INWAT,FILE=FNAME,CARRIAGECONTROL="LIST")

        CALL WATINI

      END SUBROUTINE F90_WATINI

      !iowdm:watinp
      SUBROUTINE F90_WATHED_XX (MESSFL,INWAT,IVAL,RVAL,ISIT, &
                                ITYP,ID,INAME)

        DLL_EXPORT F90_WATHED_XX

        INTEGER, INTENT(IN)     :: MESSFL,INWAT
        REAL, INTENT(OUT)       :: RVAL(7)
        INTEGER, INTENT(OUT)    :: IVAL(6),ID(8),INAME(48)
        INTEGER, INTENT(OUT)    :: ISIT(2),ITYP(4)

        INTEGER                 :: L
        CHARACTER*2             :: CSITE
        CHARACTER*4             :: CTSTYP
        CHARACTER*8             :: CSTAID
        CHARACTER*48            :: CSTANM

        CALL WATHED (MESSFL,INWAT, &
                     IVAL,RVAL, &
                     CSITE,CTSTYP,CSTAID,CSTANM)
        DO L = 1,8
          ID(L) = ICHAR(CSTAID(L:L))
        END DO
        DO L = 1,48
          INAME(L) = ICHAR(CSTANM(L:L))
        END DO
        DO L = 1,2
          ISIT(L) = ICHAR(CSITE(L:L))
        END DO
        DO L = 1,4
          ITYP(L) = ICHAR(CTSTYP(L:L))
        END DO

      END SUBROUTINE F90_WATHED_XX

      !iowdm:watinp
      SUBROUTINE F90_WATINP (MESSFL,INWAT,WDMSFL,RETCOD,IVAL,RVAL, &
                             CSCEN,CLOC,CCONS, &
                             ID,NAME,SITE,TYPE)

        DLL_EXPORT F90_WATINP

        INTEGER, INTENT(IN)          :: MESSFL,WDMSFL,IVAL(6),INWAT
        REAL, INTENT(IN)             :: RVAL(7)
        INTEGER, INTENT(OUT)         :: RETCOD
        CHARACTER(LEN=*), INTENT(IN) :: CSCEN,CLOC,CCONS,ID
        CHARACTER(LEN=*), INTENT(IN) :: NAME,SITE,TYPE

        INTEGER                :: FE
        CHARACTER*2            :: SITE2
        CHARACTER*4            :: TYPE4
        CHARACTER*8            :: CSCEN8,CLOC8,CCONS8,ID8
        CHARACTER*48           :: CNAME

        CSCEN8 = CSCEN
        CLOC8  = CLOC
        CCONS8 = CCONS
        ID8    = ID
        CNAME  = NAME
        SITE2  = SITE
        TYPE4  = TYPE

        FE = 99

        CALL WATINP (MESSFL,WDMSFL,FE,INWAT,IVAL,RVAL, &
                     CSCEN8,CLOC8,CCONS8,ID8,CNAME,SITE2,TYPE4, &
                     RETCOD)

      END SUBROUTINE F90_WATINP

      !iowdm:watclo
      SUBROUTINE F90_WATCLO (INWAT)

        DLL_EXPORT F90_WATCLO

        INTEGER, INTENT(IN)     :: INWAT

        CLOSE(UNIT=INWAT)

      END SUBROUTINE F90_WATCLO

      !iowdm:tsflat
      SUBROUTINE   F90_TSFLAT (WDMSFL,DSN,FILNAM,IHEAD,DAFMT,BEFLG, &
                               TS,TU,QC,OV,TSFILL,BSDATE,BEDATE,IRET)

        DLL_EXPORT F90_TSFLAT

        INTEGER, INTENT(IN)      :: WDMSFL,DSN,IHEAD,BEFLG
        INTEGER, INTENT(IN)      :: TS,TU,QC,OV,BSDATE(6),BEDATE(6)
        INTEGER, INTENT(OUT)     :: IRET
        REAL, INTENT(IN)         :: TSFILL
        CHARACTER(LEN=*)         :: FILNAM,DAFMT

        INTEGER      SFILE
        CHARACTER*64 FNAME,LDAFMT

        LDAFMT= DAFMT

        SFILE = 10
        FNAME = FILNAM
        OPEN(UNIT=SFILE,FILE=FNAME,ERR=10)

        CALL TSFLAT (WDMSFL,DSN,SFILE,IHEAD,LDAFMT,BEFLG, &
                     TS,TU,QC,OV,TSFILL,BSDATE,BEDATE, &
                     IRET)

        GO TO 20
 10     CONTINUE
          IRET = -2
 20     CONTINUE
        CLOSE(UNIT=SFILE)

      END SUBROUTINE F90_TSFLAT

      !iowdm:inwbat
      SUBROUTINE F90_INFREE (WDMFL,DSTYPE,DSN,DSNINC, &
                             DSNF,RETC)
        DLL_EXPORT F90_INFREE

        INTEGER, INTENT(IN)     :: WDMFL,DSTYPE,DSN,DSNINC
        INTEGER, INTENT(OUT)    :: DSNF,RETC

        CALL INFREE (WDMFL,DSTYPE,DSN,DSNINC, &
                     DSNF,RETC)

      END SUBROUTINE F90_INFREE


      !newaqt:stspec
      SUBROUTINE F90_STSPGG_XX (INITFG,RETCOD,INAME,IDESC)
!     get group name and description for special actions translator

        DLL_EXPORT F90_STSPGG_XX

        INTEGER, INTENT(IN)     :: INITFG
        INTEGER, INTENT(OUT)    :: INAME(6),IDESC(64)
        INTEGER, INTENT(OUT)    :: RETCOD

        INTEGER                 :: UVQFIL,L
        CHARACTER*6             :: GNAME
        CHARACTER*64            :: GDESC

        UVQFIL = 11
        IF (INITFG.EQ.1) THEN
          OPEN(UNIT=UVQFIL,FILE="uvquan.inp",ERR=10)
        END IF

        CALL STSPGG (UVQFIL,INITFG, &
                     GNAME,GDESC,RETCOD)
        DO L = 1,6
          INAME(L) = ICHAR(GNAME(L:L))
        END DO
        DO L = 1,64
          IDESC(L) = ICHAR(GDESC(L:L))
        END DO

        IF (RETCOD.NE.0) THEN
          CLOSE(UNIT=UVQFIL)
        END IF

        GO TO 20
 10     CONTINUE
          !error opening file
          RETCOD = -1
 20     CONTINUE

      END SUBROUTINE F90_STSPGG_XX

      !newaqt:stspec
      SUBROUTINE F90_STSPGU_XX (INITFG,RETCOD,INAME,IGROUP,IDESC, &
                                IVNAME,UMIN,UMAX,UDEF,INTFG)
!     get uvquan name and description for special actions translator

        DLL_EXPORT F90_STSPGU_XX

        INTEGER, INTENT(IN)     :: INITFG
        INTEGER, INTENT(OUT)    :: INAME(6),IDESC(64),IVNAME(6)
        INTEGER, INTENT(OUT)    :: RETCOD,INTFG,IGROUP(6)
        REAL, INTENT(OUT)       :: UMIN,UMAX,UDEF

        INTEGER                 :: UVQFIL,L
        CHARACTER*6             :: UVQUAN,UVNAME,CGROUP
        CHARACTER*64            :: UVDESC

        UVQFIL = 11
        IF (INITFG.EQ.1) THEN
          OPEN(UNIT=UVQFIL,FILE="uvquan.inp",ERR=10)
        END IF

        CALL STSPGU (UVQFIL, &
                     UVQUAN,CGROUP,UVDESC,UVNAME, &
                     UMIN,UMAX,UDEF,INTFG,RETCOD)
        DO L = 1,6
          INAME(L) = ICHAR(UVQUAN(L:L))
        END DO
        DO L = 1,6
          IGROUP(L) = ICHAR(CGROUP(L:L))
        END DO
        DO L = 1,6
          IVNAME(L) = ICHAR(UVNAME(L:L))
        END DO
        DO L = 1,64
          IDESC(L) = ICHAR(UVDESC(L:L))
        END DO

        IF (RETCOD.NE.0) THEN
          CLOSE(UNIT=UVQFIL)
        END IF

        GO TO 20
 10     CONTINUE
          !error opening file
          RETCOD = -1
 20     CONTINUE

      END SUBROUTINE F90_STSPGU_XX

      !newaqt:stspec
      SUBROUTINE F90_STSPFN_XX (INTFG,KEY, &
                                IDAT,AVAL,IAC,ICOND,UVNAME)
!     find next use of this uvquan within the special actions block

        DLL_EXPORT F90_STSPFN_XX

        CHARACTER(LEN=*), INTENT(IN) :: UVNAME
        INTEGER, INTENT(IN)          :: INTFG
        INTEGER, INTENT(INOUT)       :: KEY
        INTEGER, INTENT(OUT)         :: IDAT(5),ICOND(64),IAC(3)
        REAL, INTENT(OUT)            :: AVAL

        INTEGER                 :: L
        CHARACTER*1             :: CAC(3)
        CHARACTER*6             :: UVN6
        CHARACTER*64            :: ACOND

        UVN6 = UVNAME

        CALL STSPFN (UVN6,INTFG, &
                     KEY,IDAT,AVAL,CAC,ACOND)

        DO L = 1,3
          IAC(L) = ICHAR(CAC(L))
        END DO
        DO L = 1,64
          ICOND(L) = ICHAR(ACOND(L:L))
        END DO

      END SUBROUTINE F90_STSPFN_XX

      !newaqt:stspec
      SUBROUTINE F90_STSPPN (AKEY, AVAL, INTFG, &
                             IDATES, AC)
!     put uvquan name line to special actions block

        DLL_EXPORT F90_STSPPN

        CHARACTER(LEN=*), INTENT(IN) :: AC
        INTEGER, INTENT(IN)          :: INTFG,AKEY,IDATES
        REAL, INTENT(IN)             :: AVAL

        CHARACTER*3             :: CAC

        CAC = AC

        CALL STSPPN (AKEY,AVAL,INTFG, &
                     IDATES,CAC)

      END SUBROUTINE F90_STSPPN

      !util:utsort
      SUBROUTINE F90_ASRTRP (CNT,RVAL)
        dll_export F90_ASRTRP
        INTEGER        :: CNT
        REAL           :: RVAL(CNT)

        CALL ASRTRP(CNT, &
                    RVAL)

      END SUBROUTINE F90_ASRTRP

      !adwdm:wdmess
      SUBROUTINE F90_WDLBAX (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP, &
                             PSA)

        DLL_EXPORT F90_WDLBAX

        INTEGER, INTENT(IN)  :: WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP
        INTEGER, INTENT(OUT) :: PSA

        CALL WDLBAX (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP, &
                     PSA)

      END SUBROUTINE F90_WDLBAX

      !adwdm:wdmess
      SUBROUTINE F90_WDDSDL (WDMSFL,DSN, &
                             RETCOD)

        DLL_EXPORT F90_WDDSDL

        INTEGER, INTENT(IN)  :: WDMSFL,DSN
        INTEGER, INTENT(OUT) :: RETCOD

        CALL WDDSDL (WDMSFL,DSN, &
                     RETCOD)

      END SUBROUTINE F90_WDDSDL

      !newaqt:utdir
      SUBROUTINE F90_TSDSGN (WDMSFL,DSN)

        DLL_EXPORT F90_TSDSGN

        INTEGER        :: WDMSFL,DSN

        CALL TSDSGN (WDMSFL,DSN)

      END SUBROUTINE F90_TSDSGN

      !newaqt:ucirept
      FUNCTION F90_REPEXT (REPID)
!     check to see if a standard report exists
        dll_export F90_REPEXT

        INTEGER,       INTENT(IN)    :: REPID
        INTEGER                      :: F90_REPEXT
        INTEGER                      :: REPEXT

        F90_REPEXT = REPEXT(REPID)

      END FUNCTION F90_REPEXT

      !newaqt:ucirept
      SUBROUTINE F90_ADDREPT (WDMSFL,REPID)
!     add a standard report

        DLL_EXPORT F90_ADDREPT

        INTEGER        :: REPID,WDMSFL

        CALL ADDREPT (WDMSFL,REPID)

      END SUBROUTINE F90_ADDREPT

      !newaqt:ucirept
      SUBROUTINE F90_DELREPT (REPID)
!     delete a standard report

        DLL_EXPORT F90_DELREPT

        INTEGER        :: REPID

        CALL DELREPT (REPID)

      END SUBROUTINE F90_DELREPT

      !newaqt:ucirept
      SUBROUTINE F90_DELALLR
!     delete all reports

        DLL_EXPORT F90_DELALLR

        CALL DELALLR

      END SUBROUTINE F90_DELALLR

      !newaqt:ucirept
      SUBROUTINE F90_ADDBMP (WDMSFL,BMPID)
!     add bmp tables

        DLL_EXPORT F90_ADDBMP

        INTEGER        :: BMPID,WDMSFL

        CALL ADDBMP (WDMSFL,BMPID)

      END SUBROUTINE F90_ADDBMP

      !newaqt:getocr
      SUBROUTINE F90_GETOCR (ITYPE, &
                             NOCCUR)
!     get number of occurrances of this table

        DLL_EXPORT F90_GETOCR

        INTEGER, INTENT(IN)          :: ITYPE
        INTEGER, INTENT(OUT)         :: NOCCUR

        CALL GETOCR (ITYPE, &
                     NOCCUR)

      END SUBROUTINE F90_GETOCR

      !wdm:wdsagy
      SUBROUTINE F90_WDSAGY_XX (WDMSFL,ID, &
                                LEN, TYPE, &
                                ATMIN, ATMAX, ATDEF, &
                                HLEN, HREC, HPOS, VLEN,&
                                ANAME,ADESC,AVALID)
!     get characteristics of this wdm search attribute

        DLL_EXPORT F90_WDSAGY_XX

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

      SUBROUTINE F90_GETATT (WDMSFL,DSN, &
                             INIT, &
                             SAIND,SAVAL)
       
        DLL_EXPORT F90_GETATT

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

      SUBROUTINE F90_WUA2ID (WDMSFL,DSN,ID,CVAL)
        dll_export F90_WUA2ID
        INTEGER           :: WDMSFL,DSN,ID
        CHARACTER(LEN=*)  :: CVAL

        CALL WUA2ID (WDMSFL,DSN,CVAL,ID)

      END SUBROUTINE F90_WUA2ID


      SUBROUTINE F90_WID2UD (WDFLG,ID,WDMSFL,DSN)
        dll_export F90_WID2UD
        INTEGER           :: WDMSFL,DSN,ID,WDFLG

        CALL WID2UD (WDFLG,ID,WDMSFL,DSN)

      END SUBROUTINE F90_WID2UD


      SUBROUTINE F90_WDDSCL (OWDMFL,ODSN,NWDMFL,NDSN,NTYPE,RETCOD)
        dll_export F90_WDDSCL
        INTEGER           :: OWDMFL,ODSN,NWDMFL,NDSN,NTYPE,RETCOD

        CALL WDDSCL (OWDMFL,ODSN,NWDMFL,NDSN,NTYPE,RETCOD)

      END SUBROUTINE F90_WDDSCL


      !hspf:hiouci
      SUBROUTINE F90_GETUCI_XX (RECTYP,KEY,                       &
                                IUCIBF)
!       Get a record from the uci in memory

        dll_export F90_GETUCI_XX
        INTEGER,INTENT(IN)     :: RECTYP
        INTEGER,INTENT(INOUT)  :: KEY
        INTEGER,INTENT(OUT)    :: IUCIBF(80)

        CHARACTER(LEN=1)    :: UCIBUF(80)
        INTEGER             :: I

        CALL GETUCI (RECTYP,                                      &
                     KEY,                                         &
                     UCIBUF)

        DO I = 1,80
          IUCIBF(I) = ICHAR(UCIBUF(I))
        END DO

      END SUBROUTINE F90_GETUCI_XX