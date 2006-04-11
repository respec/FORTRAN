      LOGICAL FUNCTION LPOPT (COPT)
C
C     PROGRAM OPTION IS A LOGICAL FUNCTION TO TELL IF
C     A OPTION CHARACTER IS USED IN THE EXECUTION.  FOR
C     EXAMPLE IF PROGRAM IS EXCUTED BY:
C         MYPROG.D or MYPROG -D
C     THEN THE CALLS TO LPOPT YIELD:
C         IF (LPOPT('D')) THEN .....   (LPOPT RETURNS TRUE)
C         IF (LPOPT('F')) THEN .....   (LPOPT RETURNS FALSE)
C     VALID OPTION CHARACTERS ARE THE LETTERS A-X.
C     USER MUST DEFINE LPOPT AS A LOGICAL VARIABLE:
C     LOGICAL LPOPT
C
C     LOGICAL LOPT, LOP                                                 H
C     INTEGER IOP                                                       H
C     EQUIVALENCE (IOP,LOP)                                             H
      CHARACTER COPT*(*)
      CHARACTER CPARM*80, COPTN*2                                       MlgL
      INTEGER IBF(20), IEF(20), ILF(20)                                 MlgL
      LOGICAL LFIRST                                                    MlgL
C
C
      DATA LFIRST /.TRUE./                                              MlgL
C
C
C     ICH = ICHAR (COPT)                                                H
C     ICH = ICH - 64                                                    H
C     IF ((ICH.GT.24).OR.(ICH.LT.1)) GO TO 100                          H
C
C     CALL OPTION (IOP)                                                 H
C     IOP = IOP.ROTAT.-ICH                                              H
C     LPOPT = LOP                                                       H
C     RETURN                                                            H
C
      IF (LFIRST) THEN                                                  MlgL
      LFIRST = .FALSE.                                                  MlgL
C     CALL CPARMS (CPARM, NPARM)                                        Mlg
      CPARM = ' '                                                       L
      CALL GETCL (CPARM)                                                L
      CALL CHRLNB (CPARM, NPARM)                                        L
      IF (NPARM.EQ.0) GO TO 100                                         MlgL
      CALL UPCASE (CPARM(1:NPARM))                                      MlgL
      CALL PARSLI (CPARM(1:NPARM), 20, NFIELD, IBF, IEF, ILF)           MlgL
      ENDIF                                                             MlgL
C
      IF (NPARM.GT.0) THEN                                              MlgL
      COPTN = '-' // COPT                                               MlgL
      DO 80 I=1,NFIELD                                                  MlgL
      IF (CPARM(IBF(I):IEF(I)).EQ.COPTN) THEN                           MlgL
      LPOPT = .TRUE.                                                    MlgL
      RETURN                                                            MlgL
      ENDIF                                                             MlgL
 80   CONTINUE                                                          MlgL
      ENDIF                                                             MlgL
C
C     ERROR CONDITION
 100  LPOPT = .FALSE.
      RETURN
      END
