      SUBROUTINE   F90_TREE_BLD(OPT,DBG,
     #                          WDMFSL,RETCOD)

        USE TREE
        DLL_EXPORT F90_TREE_BLD
        INTEGER    OPT,DBG,WDMSFL,RETCOD

        INCLUDE 'CVBTREE.INC'

        !WRITE(*,*) 'F90_TREE:F90_TREE_BLD',OPT,DBG,VBNAME
        CALL TREE_BLD (OPT,VBNAME,DBG,
     #                 WDMSFL,RETCOD)
        !WRITE(*,*)  'F90_TREE:F90_TREE_BLD:wdmsfl,retcod',WDMSFL,RETCOD

      END SUBROUTINE F90_TREE_BLD

      SUBROUTINE  F90_TREE_END
        DLL_EXPORT F90_TREE_END

        CLOSE(UNIT=99)

      END SUBROUTINE F90_TREE_END

      SUBROUTINE F90_TREE_SUM (VMXLEV,GFLG,LEVUSE,LEVCNT,LEVPOSMX)
        USE TREE
        DLL_EXPORT F90_TREE_SUM

        INTEGER, INTENT(IN)        :: VMXLEV
        INTEGER, INTENT(IN)        :: GFLG
        INTEGER, INTENT(OUT)       :: LEVUSE
        INTEGER, INTENT(OUT)       :: LEVCNT(VMXLEV)
        INTEGER, INTENT(OUT)       :: LEVPOSMX(VMXLEV)

        INCLUDE 'CVBTREE.INC'

        WRITE(99,*) 'F90_TREE_SUM:',GFLG,VBNAME

        IF (VMXLEV >= MXLEV) THEN
          IF (GFLG == 0) THEN
            CALL TREE_SUM(LEVUSE,LEVCNT,LEVPOSMX)
          ELSE IF (GFLG > 0) THEN
            CALL TREE_SUM(LEVUSE,LEVCNT,LEVPOSMX,VBNAME)
          ELSE
            CALL TREE_SUM(LEVUSE,LEVCNT,LEVPOSMX,VBNAME,'D')
          END IF
        ELSE
          WRITE(99,*) 'F90_TREE_SUM:problem',VMXLEV,MXLEV
          LEVUSE  = 0
          LEVCNT  = 0
          LEVPOSMX= 0
        END IF

        WRITE(99,*) 'F90_TREE_SUM:',LEVUSE,LEVCNT

      END SUBROUTINE F90_TREE_SUM

      SUBROUTINE F90_TREE_ROOT (INAM)
        USE TREE, ONLY : TREE_ROOT
        DLL_EXPORT    F90_TREE_ROOT

        INTEGER             :: INAM(24)

        CHARACTER(LEN=24)   :: LNAM

        CALL TREE_ROOT(LNAM)

        DO I= 1,24
          INAM(I) = ICHAR(LNAM(I:I))
        END DO

      END SUBROUTINE F90_TREE_ROOT


      SUBROUTINE  F90_TREE_SET_NAME (LNAME)
        USE TREE
        DLL_EXPORT F90_TREE_SET_NAME

        CHARACTER(LEN=*),INTENT(IN) :: LNAME

        INCLUDE 'CVBTREE.INC'

        !WRITE(*,*) 'F90_TREE:F90_TREE_SET_NAME:',LNAME

        VBNAME = LNAME

      END SUBROUTINE F90_TREE_SET_NAME

      SUBROUTINE F90_FILT_ADD(NAME)
        USE TREE
        DLL_EXPORT F90_FILT_ADD

        CHARACTER(LEN=*),INTENT(IN) :: NAME

        CHARACTER(LEN=24)           :: FNAME

        FNAME = NAME
        WRITE(99,*) 'F90_FILT_ADD:entry ',FNAME

        CALL FILT_ADD (FNAME)

      END SUBROUTINE F90_FILT_ADD

      SUBROUTINE F90_FILT_MOD (UFLG,DOPT)
        USE TREE
        DLL_EXPORT F90_FILT_MOD

        INTEGER  UFLG,DOPT(MXDOPT)
        LOGICAL  FLG

        INCLUDE 'CVBTREE.INC'

        IF (UFLG == 1) THEN
          FLG = .TRUE.
        ELSE
          FLG = .FALSE.
        END IF
        CALL FILT_MOD (VBNAME,FLG,DOPT)

      END SUBROUTINE F90_FILT_MOD

      SUBROUTINE F90_FILT_LIS(FLG,
     #                        INAM,UFLG,CNT,DOPT,DCUR)
        USE TREE
        DLL_EXPORT F90_FILT_LIS

        INTEGER              :: FLG
        INTEGER, INTENT(OUT) :: INAM(24),UFLG,CNT,DOPT(MXDOPT),DCUR

        INTEGER              :: I
        LOGICAL              :: LFLG
        CHARACTER(LEN=24)    :: LNAM

        CALL FILT_LIS(FLG,
     #                LNAM,LFLG,CNT,DOPT,DCUR)
        IF (LFLG) THEN
          UFLG = 1
        ELSE
          UFLG = 0
        END IF

        DO I= 1,24
          INAM(I)  = ICHAR(LNAM(I:I))
        END DO

      END SUBROUTINE F90_FILT_LIS


      SUBROUTINE F90_DISP_LIS(FLG,
     #                        INAM,IDESC,LEV,POS,
     #                        CNT,CONID,IPCNAM)
        USE TREE
        DLL_EXPORT F90_DISP_LIS

        INTEGER              :: FLG
        INTEGER, INTENT(OUT) :: LEV,POS,CNT,CONID(MXCON)
        INTEGER, INTENT(OUT) :: INAM(24),IDESC(80)
        INTEGER, INTENT(OUT) :: IPCNAM(24)

        INTEGER              :: I
        CHARACTER(LEN=24)    :: LNAM,LPCNAM
        CHARACTER(LEN=16)    :: LTYP
        CHARACTER(LEN=80)    :: LDESC

        CALL DISP_LIS(FLG,
     #                LNAM,LDESC,LEV,POS,
     #                CNT,CONID,LPCNAM,LTYP)

        DO I= 1,24
          INAM(I)  = ICHAR(LNAM(I:I))
          IPCNAM(I)= ICHAR(LPCNAM(I:I))
        END DO
        DO I= 1,80
          IDESC(I)= ICHAR(LDESC(I:I))
        END DO

      END SUBROUTINE F90_DISP_LIS


      SUBROUTINE F90_BRAN_GET_PARM (IND,MXSIZ,
     #                              IPNAM,PSIZ,IPVAL)
        USE TREE
        DLL_EXPORT F90_BRAN_GET_PARM

        INCLUDE 'CVBTREE.INC'

        INTEGER              :: IND,MXSIZ
        INTEGER, INTENT(OUT) :: IPNAM(16),PSIZ,IPVAL(24)

        INTEGER              :: PVAL(MXPARM),PTYP,I
        REAL                 :: RVAL
        CHARACTER(LEN=16)    :: PNAM
        CHARACTER(LEN=24)    :: T,S

        CALL BRAN_GET_PARM(VBNAME,IND,MXSIZ,
     #                     PNAM,PSIZ,PVAL,PTYP)
        IF (PTYP == -1) THEN                          ! no parms
          IPNAM = 32                                  ! blanks
          IPVAL = 32                                  ! blanks
        ELSE
          DO I= 1,16
            IPNAM(I)  = ICHAR(PNAM(I:I))
          END DO
          IF (PTYP == 0) THEN
            IF (PSIZ > 6) THEN
              PSIZ = 6
            END IF
            WRITE(T,'(6A4)') PVAL(1:PSIZ)
          ELSE IF (PTYP == 1) THEN
            I = INDEX(PNAM,'DATE')
            IF (I > 0) THEN
              WRITE(S,*) MOD(PVAL(1),100)
              T = ADJUSTL(S)
              WRITE(S,*) PVAL(2)
              T = T(1:LEN_TRIM(T)) // '/' // ADJUSTL(S)
              WRITE(S,*) PVAL(3)
              T = T(1:LEN_TRIM(T)) // '/' // ADJUSTL(S)
              IF (SUM(PVAL(4:6)) > 0) THEN
                IF (PNAM .NE. 'EDATE' .OR. PVAL(4) .NE. 24) THEN
                  WRITE(S,*) PVAL(4)
                  T = T(1:LEN_TRIM(T)) // ' ' // ADJUSTL(S)
                  WRITE(S,'(I2.2)') PVAL(5)
                  T = T(1:LEN_TRIM(T)) // ':' // ADJUSTL(S)
                  IF (PVAL(6) > 0) THEN
                    WRITE(S,*) PVAL(6)
                    T = T(1:LEN_TRIM(T)) // ':' // ADJUSTL(S)
                  END IF
                END IF
              END IF
            ELSE
              WRITE(T,*) PVAL(1)
            END IF
          ELSE
            RVAL = TRANSFER(PVAL(1),RVAL)
            CALL DECCHX (RVAL,16,3,3,
     #                   T)
          END IF
          T = ADJUSTL(T)
          DO I = 1,24
            IPVAL(I)  = ICHAR(T(I:I))
          END DO
        END IF

      END SUBROUTINE F90_BRAN_GET_PARM
