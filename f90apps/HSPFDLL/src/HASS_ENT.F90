      !general
      SUBROUTINE F90_W99OPN()
        dll_export F90_W99OPN
        CHARACTER(LEN=1)              :: C
        INTEGER                       :: I
        LOGICAL                       :: L

        INQUIRE(FILE='ERROR.FIL',EXIST=L,IOSTAT=I,ERR=98)
        IF (L) THEN 
          OPEN(99,FILE='ERROR.FIL',ACTION='DENYNONE',ERR=98,IOSTAT=I,STATUS='OLD')
        ELSE
          OPEN(99,FILE='ERROR.FIL',ACTION='DENYNONE',ERR=98,IOSTAT=I,STATUS='NEW')
        END IF
        CALL GET_WDM_FUN(-1)
        RETURN

 98     CONTINUE
        WRITE (*,*) 'Error ',MOD(I,16384),' opening ERROR.FIL',L
        INQUIRE(99,ERR=99,IOSTAT=I,OPENED=L)
 99     CONTINUE
        WRITE (*,*) 'Status',MOD(I,16384),L
        READ(*,*) C

      END SUBROUTINE F90_W99OPN
      

      SUBROUTINE F90_W99CLO()
        dll_export F90_W99CLO

        close(102)
        CLOSE(99)
      END SUBROUTINE F90_W99CLO

      !adwdm:utwdmd
      SUBROUTINE F90_WDBFIN()
        dll_export F90_WDBFIN

        CALL WDBFIN
      END SUBROUTINE F90_WDBFIN

      SUBROUTINE F90_PUTOLV(OUTLEV)
        dll_export F90_PUTOLV
        INTEGER, INTENT(IN)         :: OUTLEV
        CALL PUTOLV(OUTLEV)
      END SUBROUTINE F90_PUTOLV

      !adwdm:wdopxx
      FUNCTION F90_WDBOPN(RWFLG,WDNAME) RESULT(WDMSFL)
        dll_export F90_WDBOPN

        CHARACTER(LEN=*),INTENT(IN) :: WDNAME
        INTEGER,         INTENT(IN) :: RWFLG
        INTEGER                     :: WDMSFL

        CHARACTER*64                :: LNAME
        INTEGER                     :: RETCOD
        CHARACTER*3                 :: WSTAT

        LNAME = WDNAME
        CALL GET_WDM_FUN(WDMSFL)
        !WRITE(*,*) 'HASS_ENT:F90_WDBOPN:RWFLG,WDMSFL:',RWFLG,WDMSFL
        CALL WDBOPN(WDMSFL,LNAME,RWFLG,RETCOD)
        !WRITE(*,*) 'HASS_ENT:F90_WDBOPN:',RWFLG,WSTAT
        IF (RETCOD .NE. 0) THEN
          IF (RETCOD .LT. -10) THEN
            RETCOD = ABS(RETCOD) - 16384
          END IF
          !WRITE(*,*) 'HASS_ENT:F90_WDBOPN: "'//LNAME,'" retc:',RETCOD
          !WRITE(99,*) 'HASS_ENT:F90_WDBOPN: "'//LNAME,'" retc:',RETCOD
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
            IF (NXTWDM < 102) THEN
              NXTWDM = 0
            END IF
          END DO
        ELSE
          IF (NXTWDM == 0) THEN
            NXTWDM = 102
          END IF
          WDMSFL = NXTWDM
          NXTWDM = NXTWDM+ 1
        END IF
      END SUBROUTINE

