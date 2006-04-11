      SUBROUTINE PRTCR1 (IX, IY, CTABIN, ISTAT)
C
C        SUBROUTINE 'PRTCR1' RETURNS THE XY COORDINATES OF
C        THE AREA ON THE BIT PAD SPECIFIED BY THE USER.  THE
C        SUBROUTINE IS USED ONLY WHEN THE CHARACTER STRING
C        CONTAINING THE TABLET'S RESPONSE IS ALREADY KNOWN.
C        IF THE RESPONSE IS NOT KNOWN, THE SUBROUTINE
C        'PRTCRX' SHOULD BE CALLED.  'PRTCR1' WAS DEVELOPED
C        IN ORDER TO ALLOW 'PANRED' TO DETERMINE THE TYPE
C        OF TABLET BEING USED, AND SIMPLY READ FROM THE
C        TABLET OR THE KEYBOARD, DEPENDING ON WHICH IS USED.
C
C
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLlg
CADD C.PCCDEF                                                           H
      INCLUDE 'pccdef.h'                                                MLlg
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX
C
      CHARACTER CTABIN*(*)
C
C
      ISTAT=0
C   ***
C   ***
C   ** IF THE TABLET IS A TEK 4014 THE CONTROL IS SENT HERE
C   ***
      IF (LTKTAB) THEN
 15     CONTINUE
        IX = 0
        IY = 0
        IF ((CTABIN(1:1).EQ.'J').OR.(CTABIN(1:1).EQ.'M')) GO TO 50
        DO 80 K=1,6
          IF (ICHAR(CTABIN(K:K)).NE.19) GO TO 100
   80   CONTINUE
        CALL ABORT
  100   CONTINUE
        ISTAT = -1
        RETURN
 50     CONTINUE
        IX=((ICHAR(CTABIN(5:5))-32)*128+(ICHAR(CTABIN(6:6))-32)*4
     1      +MOD(ICHAR(CTABIN(3:3)),4))
        IY=((ICHAR(CTABIN(2:2))-32)*128+(ICHAR(CTABIN(4:4))-32)*4
     1      +MOD((ICHAR(CTABIN(3:3))/4),4))
        RETURN
      ELSE
  16    CONTINUE
C   ***
C   ***
C   ** DECODE COORDINATES BASED ON TABLET TYPE AND FORMAT
C   ***
C   ***
C   ** IF THE TABLET IS A SUMMAGRAPHICS, 13 CHAR FORMAT
C   ** CONTROL OF THE SUBROUTINE GOES HERE
C   ***
        IF (ITBLTY.EQ.2) THEN
          IF (ICHAR(CTABIN(5:5)) .NE. 44) GO TO 30
          IF (ICHAR(CTABIN(10:10)) .NE. 44) GO TO 30
          DO 22 I = 1,4
            IF (ICHAR(CTABIN(I:I)) .GT. 57) GO TO 30
            IF (ICHAR(CTABIN(I:I)) .LT. 48) GO TO 30
  22      CONTINUE
          DO 23 I = 6,9
            IF (ICHAR(CTABIN(I:I)) .GT. 57) GO TO 30
            IF (ICHAR(CTABIN(I:I)) .LT. 48) GO TO 30
  23      CONTINUE
          IX=((ICHAR(CTABIN(1:1))-48)*1000+(ICHAR(CTABIN(2:2))-48)
     1       *100+(ICHAR(CTABIN(3:3))-48)*10+(ICHAR(CTABIN(4:4))-48))
          IY=((ICHAR(CTABIN(6:6))-48)*1000+(ICHAR(CTABIN(7:7))-48)
     1       *100+(ICHAR(CTABIN(8:8))-48)*10+(ICHAR(CTABIN(9:9))-48))
C
C   ***
C   ***
C   ** IF THE TABLET IS A TEK, 18 CHARACTER FORMAT
C   ** CONTROL OF THE SUBROUTINE GOES HERE
C   ***
        ELSE IF (ITBLTY.EQ.3) THEN
          IF (ICHAR(CTABIN(6:6)) .NE. 44) GO TO 30
          IF (ICHAR(CTABIN(12:12)) .NE. 44) GO TO 30
          IF (ICHAR(CTABIN(15:15)) .NE. 44) GO TO 30
          DO 122 I = 2,5
            IF (ICHAR(CTABIN(I:I)) .GT. 57) GO TO 30
            IF (ICHAR(CTABIN(I:I)) .LT. 48) GO TO 30
  122     CONTINUE
          DO 123 I = 8,11
            IF (ICHAR(CTABIN(I:I)) .GT. 57) GO TO 30
            IF (ICHAR(CTABIN(I:I)) .LT. 48) GO TO 30
  123     CONTINUE
          IX=((ICHAR(CTABIN(2:2))-48)*1000+(ICHAR(CTABIN(3:3))-48)
     1       *100+(ICHAR(CTABIN(4:4))-48)*10+(ICHAR(CTABIN(5:5))-48))
          IF (ICHAR(CTABIN(1:1)) .EQ. 45) IX = -IX
          IY=((ICHAR(CTABIN(8:8))-48)*1000+(ICHAR(CTABIN(9:9))-48)
     1       *100+(ICHAR(CTABIN(10:10))-48)*10
     1       +(ICHAR(CTABIN(11:11))-48))
          IF (ICHAR(CTABIN(7:7)) .EQ. 45) IY = -IY
C   ***
C   ***
C   ** IF THE TABLET IS A TEK, 20 CHARACTER FORMAT
C   ** CONTROL OF THE SUBROUTINE IS SENT HERE
C   ***
        ELSE IF (ITBLTY.EQ.4) THEN
          IF (ICHAR(CTABIN(7:7)) .NE. 44) GO TO 30
          IF (ICHAR(CTABIN(14:14)) .NE. 44) GO TO 30
          DO 222 I = 2,6
            IF (ICHAR(CTABIN(I:I)) .GT. 57) GO TO 30
            IF (ICHAR(CTABIN(I:I)) .LT. 48) GO TO 30
 222      CONTINUE
          DO 223 I = 9,13
            IF (ICHAR(CTABIN(I:I)) .GT. 57) GO TO 30
            IF (ICHAR(CTABIN(I:I)) .LT. 48) GO TO 30
 223      CONTINUE
          IX=((ICHAR(CTABIN(2:2))-48)*10000+(ICHAR(CTABIN(3:3))-48)
     1       *1000+(ICHAR(CTABIN(4:4))-48)*100
     1       +(ICHAR(CTABIN(5:5))-48)*10+(ICHAR(CTABIN(6:6))-48))
          IF (ICHAR(CTABIN(1:1)).EQ.45) IX=-IX
          IY=((ICHAR(CTABIN(9:9))-48)*10000+(ICHAR(CTABIN(10:10))-48)
     1       *1000+(ICHAR(CTABIN(11:11))-48)*100
     1       +(ICHAR(CTABIN(12:12))-48)*10+(ICHAR(CTABIN(13:13))-48))
          IF (ICHAR(CTABIN(8:8)).EQ.45) IY=-IY
C   ***
C   ***
C   ** IF THE TABLET IS A TEK, 6 CHARACTER FORMAT
C   ** CONTROL OF THE SUBROUTINE IS SENT HERE
C   ***
        ELSE IF ((ITBLTY.EQ.5).OR.(ITBLTY.EQ.1)) THEN
          IX=((ICHAR(CTABIN(5:5))-32)*128+(ICHAR(CTABIN(6:6))-32)
     1       *4+MOD(ICHAR(CTABIN(3:3)),4))
          IY=((ICHAR(CTABIN(2:2))-32)*128+(ICHAR(CTABIN(4:4))-32)
     1       *4+MOD((ICHAR(CTABIN(3:3))/4),4))
        ENDIF
        RETURN
C   ***
C   ***
C   ** BAD POINT TRY AGAIN
C   ***
 30     CONTINUE
        ISTAT = -1
        RETURN
      ENDIF
      END
