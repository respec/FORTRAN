      !GFILE = 'c:\f90apps\geod\bin\AREA.PAR'
      !nadinit
      SUBROUTINE F90_NADINIT (IERROR,ANAME)
        ! initialize routine to convert a point
        ! from nad27 to nad83 or vice versa

        DLL_EXPORT F90_NADINIT

        CHARACTER(LEN=*),INTENT(IN) :: ANAME
        INTEGER, INTENT(OUT) :: IERROR
        ! ierror: 0 - initialized okay
        !         1 - problem initializing

        !local variables
        INTEGER          :: NC(8),NAREA,MXAREA
        INTEGER          :: IOS,I,IFLAG1,IFLAG2,NC1,N1,N2,LENG
        INTEGER          :: N3,IERR,ITEMP,NAPAR
        CHARACTER*15     :: AREAS(8),AAREA
        CHARACTER*20     :: B20
        CHARACTER*65     :: AFILE,GFILE
        CHARACTER*80     :: B80, CARD, CCARD, DUM
        DOUBLE PRECISION :: DX1, DY1, XMAX1, XMIN1, YMAX1, YMIN1
        DOUBLE PRECISION :: DX(8),DY(8),XMAX(8),XMIN(8),YMAX(8),YMIN(8)
        LOGICAL          :: NOGO, GFLAG

        IERROR = 0
        MXAREA = 8
        B20    = '                   '
        B80    = B20//B20//B20//B20
        DUM    = B80
        IFLAG1 = 1
        IFLAG2 = 2
        NAREA  = 0
        NOGO   = .FALSE.

        !Try to open the 'AREA.PAR' file
        GFILE = ANAME
        NAPAR = 21
!Briel:
!       +--------------------------------------------------------+
!       | NOTE: To allow for simultaneous access by more than    |
!       | one user, input files can be opened in READ-only mode. |
!       | Output files (if any) can be opened with a unique name |
!       | tag, such as:  userdatetime_outfilename.               |
!       +--------------------------------------------------------+
!
        OPEN (NAPAR,FILE=GFILE,FORM='FORMATTED',STATUS='OLD', &
              ACCESS='SEQUENTIAL',ERR=9000,IOSTAT=IOS, &
              action='READ')

        DO 120 I = 1, MXAREA
          !for each line of area.par file
  100     READ (NAPAR,110,ERR=9000,END=9100,IOSTAT=IOS) CARD
  110     FORMAT (A80)

          !Check for comment records and blank records
          IF ( CARD(1:1) .EQ. '*' ) THEN
            CALL NBLANK (CARD, IFLAG2, N2)
            GOTO 100
          ELSEIF ( CARD .EQ. B80 ) THEN
            GOTO 100
          ENDIF

          !Get area name and basename of file
          !(i.e. location without extensions)
          DUM = CARD
          AAREA = DUM(1:15)
          CALL NBLANK (CARD(16:80), IFLAG1, N1)
          DUM(1:65) = CARD(15+N1:80)
          LENG = 65
          AFILE = CCARD(DUM, LENG, IERR)
          IF (IERR .NE. 0) GOTO 9000
          IF (AFILE .EQ. '        ') GOTO 9000
          !find last non-blank in afile to check for hpgn at end of name
          CALL NBLANK (AFILE(1:65),IFLAG2,N3)
          !check for state hpgn file; only 1 file can be open, must end
          !in 'hpgn'
          IF(AFILE(N3-3:N3).eq.'hpgn') GO TO 120
          ITEMP = NAREA + 1
          CALL OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX1, DY1, &
                       XMAX1, XMIN1, YMAX1, YMIN1, NC1, CARD)
          IF (.NOT. NOGO) THEN
            !files opened OK and variables read
            NAREA = ITEMP
            AREAS(NAREA) = AAREA
            DX(NAREA) = DX1
            DY(NAREA) = DY1
            XMAX(NAREA) = XMAX1
            XMIN(NAREA) = XMIN1
            YMAX(NAREA) = YMAX1
            YMIN(NAREA) = YMIN1
            NC(NAREA) = NC1
            CALL NBLANK (CARD, IFLAG2, N2)
          END IF
  120   CONTINUE

        GOTO 9100
9000    CONTINUE
        !continue here on file read error
        IERROR = 1
9100    CONTINUE
        !reached end of file successfully

        !put area details to common
        CALL PUTAREA (NAREA,AREAS,NC,DX,DY,XMAX,XMIN,YMAX,YMIN)

      END SUBROUTINE F90_NADINIT


      !nadcon
      SUBROUTINE F90_NADCON (IDATUM,DINX,DINY,DOUTX,DOUTY,IERROR)
        !convert a point from nad27 to nad83 or vice versa

        DLL_EXPORT F90_NADCON

        INTEGER, INTENT(IN)           :: IDATUM
        !idatum:  1 - nad27 to nad83
        !        -1 - nad83 to nad27
        DOUBLE PRECISION, INTENT(IN)  :: DINX,DINY
        DOUBLE PRECISION, INTENT(OUT) :: DOUTX,DOUTY
        INTEGER, INTENT(OUT)          :: IERROR

        !local variables
        INTEGER           :: ITYPE
        LOGICAL           :: NOGO
        DOUBLE PRECISION  :: DLAM, DLOM, DLAS, DLOS, TDINX, TDINY
        CHARACTER*15      :: RESP

        IERROR= 0

        TDINX = -DINX
        TDINY = DINY
        NOGO = .FALSE.
        CALL TRANSF (NOGO, RESP, TDINX, TDINY, DOUTX, DOUTY, &
                     DLAM, DLOM, DLAS, DLOS, IDATUM, ITYPE)
        DOUTX = -DOUTX
        IF (IDATUM .EQ. -1) THEN
          !for nad 83 to nad 27
          DOUTX = -TDINX
          DOUTY = TDINY
        ENDIF
        IF (NOGO) THEN
          IERROR = 1
        END IF

      END SUBROUTINE F90_NADCON


      !hms
      SUBROUTINE F90_HMS (DD,IH,IM,DS)
        ! convert decimal degrees to hour, minute, second

        DLL_EXPORT F90_HMS

        DOUBLE PRECISION, INTENT(IN)  :: DD
        INTEGER, INTENT(OUT)          :: IH,IM
        DOUBLE PRECISION, INTENT(OUT) :: DS

        CALL HMS(DD,IH,IM,DS)

      END SUBROUTINE F90_HMS


      !vertinit
      SUBROUTINE F90_VERTINIT (IERROR)
        ! initialize routine to convert from vertical
        ! datum ngvd29 to navd88 or vice versa

        DLL_EXPORT F90_VERTINIT

        INTEGER, INTENT(OUT) :: IERROR
        ! ierror: 0 - initialized okay
        !         1 - problem initializing

        !local variables
        CHARACTER*12     FILES(3)
        REAL*4           MARGIN(3)
        LOGICAL          NOGO

        IERROR = 0
        FILES(1)='vertconw.94 '
        MARGIN(1)=5.0
        FILES(2)='vertconc.94 '
        MARGIN(2)=5.0
        FILES(3)='vertcone.94 '
        MARGIN(3)=0.0
        NOGO = .FALSE.

        !put vert details to common
        CALL PUTVERT (FILES,MARGIN)

        CALL LOADGRD(NOGO)

        IF (NOGO) THEN
          IERROR = 1
        END IF

      END SUBROUTINE F90_VERTINIT


      !vertcon
      SUBROUTINE F90_VERTCON (DINX,DINY,GHT,IERROR)
        ! convert a point from vertical
        ! datum ngvd29 to navd88 or vice versa

        DLL_EXPORT F90_VERTCON

        DOUBLE PRECISION, INTENT(IN)  :: DINX,DINY
        REAL, INTENT(OUT)             :: GHT
        INTEGER, INTENT(OUT)          :: IERROR

        !local variables
        DOUBLE PRECISION              :: XEAST,DHPRED
        INTEGER                       :: IOS

        IERROR = 0
        XEAST = 360.0 - DINX
        CALL INTERP(DINY,XEAST,DHPRED,IOS)
        IF (IOS.NE.0) THEN
          IERROR = 1
        ELSE
          GHT = DHPRED * 0.001d0
          GHT = vROUND(GHT)
        END IF

      END SUBROUTINE F90_VERTCON
