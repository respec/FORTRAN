        SUBROUTINE PUTAREA (INAREA,IAREAS,INC,IDX,IDY,
     I                      IXMAX,IXMIN,IYMAX,IYMIN)

C       created by pbd to put area values into common block for nadcom dll

        INTEGER          INAREA, INC(INAREA)
        CHARACTER*15     IAREAS(INAREA)
        DOUBLE PRECISION IDX(INAREA), IDY(INAREA), IXMAX(INAREA)
        DOUBLE PRECISION IXMIN(INAREA), IYMAX(INAREA), IYMIN(INAREA)

        INTEGER  MXAREA
        PARAMETER (MXAREA=8)

        CHARACTER*15 AREAS
        COMMON /AREAS/ AREAS(MXAREA)

        DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
        INTEGER NC, NAREA
        COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                  XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                  NC(MXAREA), NAREA

        INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
        COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

        INTEGER  I

        LUOUT = 99

        NAREA = INAREA
        DO 10 I = 1,INAREA
          AREAS(I) = IAREAS(I)
          DX(I)    = IDX(I)
          DY(I)    = IDY(I)
          XMAX(I)  = IXMAX(I)
          XMIN(I)  = IXMIN(I)
          YMAX(I)  = IYMAX(I)
          YMIN(I)  = IYMIN(I)
          NC(I)    = INC(I)
 10     CONTINUE

        RETURN
        END



        SUBROUTINE PUTVERT (IFILES,IMARGIN)

C       created by pbd to put flag parameters for vertcon into
C       common block for vertcon dll

        CHARACTER*12, INTENT(IN) :: IFILES(3)
        REAL, INTENT(IN)         :: IMARGIN(3)

        parameter (MAXAREA=10,NSPACE1=6*MAXAREA,NSPACE2=2*MAXAREA)
        REAL*4        SPACE1,SPACE2,MARGIN
        CHARACTER*12  FILES
        COMMON /vGRIDS/ FILES(MAXAREA)
        COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
        common/GSTUFF/ SPACE1(NSPACE1),MARGIN(MAXAREA),SPACE2(NSPACE2)

        INTEGER   I

        LUOUT = 99
        MARGIN(1) = IMARGIN(1)
        MARGIN(2) = IMARGIN(2)
        MARGIN(3) = IMARGIN(3)
        FILES(1) = IFILES(1)
        FILES(2) = IFILES(2)
        FILES(3) = IFILES(3)
        DO 10 I = 4,MAXAREA
          MARGIN(I) = 0.0
          FILES(I) = ' '
 10     CONTINUE

        RETURN
        END
