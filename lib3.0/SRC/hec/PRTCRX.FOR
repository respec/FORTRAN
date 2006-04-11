      SUBROUTINE PRTCRX (IX, IY)
C
C        SUBROUTINE 'PRTCRX' RETURNS THE XY COORDINATES OF
C        THE AREA ON THE BIT PAD SPECIFIED BY THE USER.  THE
C        SUBROUTINE FIRST DETERMINES WHAT KIND OF PAD IS BEING
C        USED, AND HOW TO TURN THE PAD ON AND OFF.  THE SUB-
C        ROUTINE THEN COMPUTES THE X AND Y COORDINATES BY
C        REFERENCING INTERNAL DATA, THE REFERENCE DEPENDING
C        ON THE TYPE OF PAD BEING USED.
C
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PCCDEF                                                           H
      INCLUDE 'pccdef.h'                                                MLlg
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLlg
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX
C
      CHARACTER*80 CTABIN
C
      PARAMETER (NBUFF=20)
      INTEGER IBUFF(NBUFF)
      NERROR=0
C
C
C     TURN ON HOT READ
C
      CALL CHRIT1(IKB,IBUFF,NBUFF)
C
 1000 NLINE=0
C     CHECK FOR TEKTRONIX 4014 TABLET
C
      IF(LTKTAB) THEN
       CALL CHRWT(IKB,CANM,3)
       CALL CHRWT(IKB,CBELL,3)
       CALL CHRWT(IKB,CTON,3)
      ELSE
       CALL CHRWT(IKB,CBELL,3)
      ENDIF
C     READ IN DATA
 1100 CALL CHRRD1(CTABIN(NLINE+1:80),NCHS)
C     DO 220 K=1,NCHS                                                   H
C
C     J=NLINE+K                                                         H
C     STRIP ALL BUT 7 BITS WORTH
C     I=ICHAR(CTABIN(J:J)) .AND. '177                                   H
C     CTABIN(J:J)= CHAR(I)                                              H
C 220 CONTINUE                                                          H
C     CHECK FOR CONTROL X EXIT
      NLINE=NLINE+NCHS
      IF (NLINE.LT.IPMAX) GO TO 1100
C     DETERMINE THE COORDINATE POINTS
      CALL PRTCR1(IX,IY,CTABIN,ISTAT)
      IF(CTRMTY.EQ.'ASY') CALL CHRFL1
C     CHECK FOR TEKTRONIX 4014 TABLET AND TURN OFF
      IF (LTKTAB) CALL CHRWT(IKB,CTOFF,3)
      IF (ISTAT.NE.0) THEN
      NERROR=NERROR+1
       IF (NERROR.EQ.3) THEN
        CALL CHRWT(IKB,CHAR(10)//'ONE LAST CHANCE'//CHAR(13),17)
       ELSE IF(NERROR.GT.3) THEN
        CALL CHRWT(IKB,CHAR(10)//'OUT OF LUCK - ABORTING IN PRTCRX'//
     .CHAR(13),34)
        GO TO 6000
        ENDIF
        CALL CHRWT(IKB,CHAR(10)//'RE-ENTER LAST POINT'//CHAR(13),21)
        GO TO 1000
      ENDIF
C
 5000 CONTINUE
       CALL CHRWT(IKB,CBELL,3)
C     TURN OFF HOT READ
      CALL CHRFN1
      RETURN
 6000 CONTINUE
      CALL CHRFN1
      STOP
      END
