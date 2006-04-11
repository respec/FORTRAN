      SUBROUTINE LINK(JNEXT)
C     TRANSPORT BLOCK
C     CALLED BY INTRAN NEAR LINE 985
C=======================================================================
C     Subroutine for computing flows, depths, and velocities
C     to be passed to the WASP model for simulating quality.
C
C     Created September 1993 by James Martin of AScI Corp. 
C     Last updated: 10/21/93 by WCH for minor editing.  
C     Updated 1/4/93 by WCH for timing correction to TEND, plus error
C       check.  
C=======================================================================
C
C#### WCH, 10/93.  NOTE, ISUMRY PLACED IN HUGO.INC, NOT TAPES.INC.
C
      INCLUDE 'TAPES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'TST.INC'
      INCLUDE 'INTER.INC'
      DIMENSION SURGE1(NET),SURGE2(NET),WELL1(NET),WELL2(NET),
     1            QO(NET),QI(NET),QO1(NET),QO2(NET),PUMP(NET)
      DIMENSION SST(17),SDE(17)
C
      DIMENSION JUNSEG(NET),IC1(NET),IFLOW(NET,2),WVOL(NET),WDEP(NET),
     1        WVEL(NET)
      CHARACTER*10 DNAME
C#### WCH
      CHARACTER*60 FNAME
CWRMb  save values between calls of subroutine
      SAVE IFIRST,N22,N23,TSTART,IDEP,NC1,NSEG,JUNSEG,IC1
CWRMe
C
      EQUIVALENCE (PUMP(1),DIST(1)),(SURGE1(1),P1(1)),(SURGE2(1),P2(1))
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
      EQUIVALENCE (WELL1(1),SLOPE(1)),(WELL2(1),ROUGH(1))
C
      IF(JNEXT.EQ.0) THEN
C
C#######################################################################
C     WCH, 10/93.  CHANGE FROM JLM.  INPUT ALL DATA ON OPTIONAL
C       H2 AND H3 LINES.  
C     SO, FIRST CHECK TO SEE IF H2 LINE EXISTS.
C=======================================================================
C#######################################################################
C     WCH, 10/93.  CHANGE FROM JLM.  INPUT ALL DATA ON OPTIONAL
C       H2 AND H3 LINES.  
C     SO, FIRST CHECK TO SEE IF H2 LINE EXISTS.
C=======================================================================
C#### WCH, 5/10/94.  INCLUDE ERR=888.
C      READ (N5,*) CC
      READ (N5,*,ERR=888) CC
      IF(CC.NE.'H2') THEN
           BACKSPACE N5
           RETURN
           ELSE 
           BACKSPACE N5
           ISUMRY = 1
           ENDIF
C
C
C  ###########################################################
C  #    UPON  1ST CALL TO THIS SUBROUTINE, OPEN ALL FILES    #
C  #    AND CONSTRUCT FLOW MAP                               #
C  ###########################################################
C
C     A)  OPEN INPUT FILES
C         ----------------
C#### WCH, 10/93.  REPLACE HARD 22 AND 23 WITH SOFT N22 AND N23.
      N22 = 22
      N23 = 23
C
      WRITE(*,3)
    3 FORMAT(/,1X,
     1'#####################################################',/,1X,
     2'# Entry made to the WASP linkage routines, last     #',/,1X,
     3'#     updated by AScI, January 1994.                #',/,1X,
     4'#####################################################',/)
C
C#######################################################################
C     WCH, 10/93.  ADD OPTION FOR USING REGULAR NSCRAT(8).
C#######################################################################
      IF(NSCRAT(8).EQ.0.OR.(NSCRAT(8).GT.0.AND.FFNAME(58).EQ.
     1'SCRT8.UF'.AND.JKP(58).NE.2)) THEN
C
C     HERE, NEED TO INPUT WASP INTERFACE FILE NAME FROM SCREEN.
C           
           WRITE(*,1)
           READ(*,2) FNAME
    1      FORMAT(/,' Enter your WASP linkage DOS file name  -  ')
    2      FORMAT(A)
           IF(NSCRAT(8).GT.0) THEN
                CLOSE (NSCRAT(8))
                N22 = NSCRAT(8)
                ENDIF
    4      CONTINUE
C=======================================================================
C     CHECK THAT VALUE FOR N22 ISN'T ALREADY TAKEN BY A SCRATCH FILE
C       OR BY AN INTERFACE FILE.
C=======================================================================
           DO 15 I = 1,8
           IF(NSCRAT(I).EQ.N22) THEN
                N22 = N22 + 10
                GO TO 4
                ENDIF
   15      CONTINUE
           DO 17 I = 1,25
           IF(JIN(I).EQ.N22.OR.JOUT(I).EQ.N22) THEN
                N22 = N22 + 10
                GO TO 4
                ENDIF
   17      CONTINUE
           ELSE
           FNAME = FFNAME(58)
           N22   = NSCRAT(8)
           ENDIF
C
         OPEN(UNIT=N22,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED')
C
C     B)  INITIALIZE JUNSEG 
C         -----------------
            DO 10 II=1,NE
               JUNSEG(II) = 999
10         CONTINUE
C
            IFIRST = 0
C
C     C)   READ  DATA FROM CARD GROUPS
C      -------------------------------
C        TSTART = START TIME OF DAY FOR WRITTING HYDRODYNAMIC LINKAGE 
C                 (DECIMAL HOURS).  OK FOR TSTART TO BE > 24 IF 
C                 NECESSARY.  MUST HAVE TSTART >= TZERO.  
C        HDEP   = OPTION TO WRITE OUT DEPTHS AND VELOCITIES
C          HDEP = 0  TIME VARYING DEPTHS AND VELOCITIES WRITTEN OUT
C          HDEP = 1  CONSTANT DEPTHS AND VELOCITIES (FROM FIRST WRITE)
C        NWAS     = NUMBER OF ELEMENTS TO MAP TO WASP
C                 < 0: IF NWAS < 0, THEN SET ISUMRY = 99 FOR 
C                 DEBUGGING PRINT-OUT OF TRANSPORT QUALITY ON UNIT N23.
C
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>>>>>> READ H2 DATA GROUP <<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
            READ(N5,*,ERR=888) CC,TSTART,IDEP,NWAS
C
            IF(NWAS.LT.0) THEN
C
C (NOTE: DUMMY.PRN IS A FILE TO AID IN COMPARISON OF WASP AND TRANSPORT
C        PREDICTIONS)
                 ISUMRY = 99
                 NWAS = -NWAS
                 OPEN(UNIT=N23,FILE='LINK.PRN',STATUS='UNKNOWN')
                 ENDIF
C#### WCH, 10/93.  ADD FNAME
            WRITE(N6,960)  FNAME,TSTART,IDEP,NWAS
                 DELTQ = DT
C
C  NOTE, TSTART IS AN ABSOLUTE TIME OF DAY AND MUST BE >= TZERO.
C  TSTART CAN EXCEED 24 IF NECESSARY.
C
                 TTAPE = TSTART*3600.
C
C#### WCH, 1/3/94.  ENDING TIME IS DISPLACEMENT FROM TZERO.  TZERO
C####               IS CONVERTED TO SECONDS IN SUB. INTRAN. 
C
                 TEND  = NDT*DT + TZERO
C
C#### WCH, 1/3/94.  ADD CHECK FOR TEND VS. TTAPE. VS. TZERO.
C
      IF(TEND.LE.TTAPE.OR.TTAPE.LT.TZERO) THEN
           WRITE(N6,975) TZERO/3600.,TSTART,TEND/3600.
           STOP
           ENDIF
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>>>>> READ H3 DATA GROUP <<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
               DO 20 J = 1,NWAS
                IF(JCE.EQ.0) THEN
                    READ (N5,*,ERR=888) CC,JJ,JUN
                    DO 30 K=1,NE
                          KK = JR(K)
                          IF(NOE(KK).EQ.JJ)THEN
                              JUNSEG(KK) = JUN
                              WRITE(N6,961)JJ,KK,JUNSEG(KK)
                          END IF
30                 CONTINUE
                ELSE
                    READ (N5,*,ERR=888) CC,DNAME,JUN
                    DO 40 K=1,NE
                          KK = JR(K)
                          IF(KOE(KK).EQ.DNAME)THEN
                             JUNSEG(KK) = JUN
                             WRITE(N6,962)DNAME,KK,JUNSEG(KK)
                          END IF
40                 CONTINUE
                END IF
C
20         CONTINUE
C
C
C
C     D)  COMPUTE FLOW MAPPING AND DETERMINE BOUNDARIES
C         ---------------------------------------------
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    ASSIGN 0 (A BOUNDARY) TO MANHOLES 
C    FOR THREE CONDITIONS:  A) A WASP SEGMENT UPSTREAM, AND
C                            1) THERE IS A CONSTANT INFLOW (IDIST(MM)>0)
C                            2) THERE ARE SEWER INFILTRATION INFLOWS
C                               (NINFIL = 1.OR.NFILTH = 1)
C                            3)  DATA ARE OBTAINED FROM UNFORMMATED
C                                FILE 
C                            4) THERE IS NOT A SEGMENT DOWNSTREAM
C                           B) NOTHING UPSTREAM BUT THERE
C                              IS A WASP SEGMENT DOWNSTREAM
C                           C) A WASP SEGMENT DOWNSTREAM AND
C                              A JUNCTION ELEMENT (2 OR MORE 
C                              ELEMENTS MEET AT THE MANHOLE) 
C                              IS NOT A SEGMENT
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        DO 100 J=1,NE
           IM = JR(J)
           NTPE = NTYPE(IM)
C
C   FOR THIS LOOP, PICK DO ANALYSIS ONLY FOR MANHOLES.
C
           IF(NTPE.EQ.19) THEN
              INO    = 0
              IYES   = 0
              IDOWN  = 0
C
C ******  1ST, CHECK TO SEE IF THERE IS AN UPSTREAM SEGMENT
C              (IYES = 1), AND IF THERE IS ANY ELEMENT UPSTREAM
C              (IF THERE IS, INO=1 AND THIS IS NOT AN UPSTREAM TERMINAL
C               ELEMENT)
C
              DO 105 K=1,3
                 JJ = INUE(IM,K)
                 IF(JJ.LE.NE.AND.JUNSEG(JJ).LE.NE) INO=1
                 IF(JUNSEG(JJ).GT.0.AND.JUNSEG(JJ).LE.NE) IYES=1
105           CONTINUE
C
C            1)  CHECK TO SEE IF THERE IS AN INFLOW TO THIS
C                MANHOLE (I* = 0 (N), I*=1 (Y))
C
              IF(IYES.EQ.1) THEN
                      I1 = 0
                      I2 = 0
                      I3 = 0
                      I4 = 0
                      IF(NCNTRL.EQ.0) THEN
                        DO 108 KK=1,LOCATS
                             NN     = NLOC(KK)
                             DNAME       = KAN(KK)
                             IF(NN.GE.0) THEN
                                  NS2 = NIN(NN,DNAME)
                                  IF(NS2.EQ.J)I1 = 1
                                  END IF
108                     CONTINUE
                        END IF
                      IF(DIST(IM).GT.0) I2 = 1
                      IF(NINFIL.GT.0)   I3 = 1
C
C             2)  CHECK TO SEE IF THERE IS AN INFLOW FROM AN UPSTREAM
C                 CONDUIT THAT IS NOT A SEGMENT
                      DO 109 K=1,3
                        JJ = INUE(IM,K)
                        IF(JJ.LE.NE.AND.JUNSEG(JJ).GT.NE) I4=1
109                   CONTINUE
C
                      IF(I1.EQ.0.AND.I2.EQ.0.AND.I3.EQ.0.AND.I4.EQ.0) 
     1                                                           IYES=0
                      END IF
C
CC
C ******  2ND CHECK TO TO FIND DOWNSTREAM MOST MANHOLES
C
              DO 106 K=1,NE
                IMM = JR(K)
                DO 106 KK=1,3
                      JJ = INUE(IMM,KK)
                      IF(JJ.EQ.IM.AND.JUNSEG(IMM).LE.NE) IDOWN = 1
106           CONTINUE
C
C
C******** 2ND, IF IM IS A TERMINAL SEGMENT (UPSTREAM), MAKE SURE THERE IS A
C              WASP SEGMENT BELOW IT
C
              IF(INO.EQ.0) THEN
                     DO 107  K=1,NE
                        IMM = JR(K)
                        DO 107 KK=1,3
                           IDUM = INUE(IMM,KK)
                           IF(IDUM.EQ.IM.AND.JUNSEG(IMM).GT.NE) INO = 1
107                  CONTINUE
                     END IF
              IF(INO.EQ.0.OR.IYES.EQ.1.OR.IDOWN.EQ.0) THEN
                        NWAS = NWAS +1
                        JUNSEG(IM) = 0
                        END IF
              END IF
100    CONTINUE

C     E) COUNT NUMBER OF SEGMENTS AND CHECK FOR CONTINUITY
C        -------------------------------------------------
       NSEG = 0
       DO 200 I = 1, NE
         IF(JUNSEG(I).NE.0.AND.JUNSEG(I).NE.999) NSEG = NSEG + 1
200    CONTINUE

C
C     F)  COUNT NUMBER OF CHANNELS AND MAP
C          NC1 = NUMBER OF FLOW PAIRS
C          ---------------------------
C
          NC1 = 0
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C           1ST, DETERMINE THE WASP SEGMENT NUMBER THAT FLOW 
C                IS COMING FROM FROM  (K1)
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          DO 300 II=1,NE
             IM = JR(II)
             NTPE = NTYPE(IM)
             K1 = JUNSEG(IM)
             IF(K1.LT.NE) THEN
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C     >        2ND, DETERMINE WASP SEGMENT NUMBER (K2) THAT FLOW FROM
C     >             K1 IS GOING TO
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                DO 400 J=1,NE
                  L = JR(J)
                  NTPET = NTYPE(L)
                  DO 400 K=1,3
                   IF(INUE(L,K).EQ.IM) THEN
                     ISS = J
                     K2  = JUNSEG(L)
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C     >     3RD, IF K2= 0, CHECK TO MAKE SURE: IT IS MOST DOWNSTREAM ELEMENT
C     >        OR ELEMENT BELOW IS NOT A WASP SEGMENT
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                    IF(K1.GT.0.AND.K2.EQ.0) THEN
                        DO 500 JJ=1,NE
                         IMM = JR(JJ)
                         DO 500 JK=1,3
                          IF(INUE(IMM,JK).EQ.L) THEN
                             IF(JUNSEG(IMM).LE.NE) K2 = 999
                             END IF
500                      CONTINUE
                        END IF
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C     >     4TH, WRITE OUT K1 AND K2 TO STORAGE ARRAY (IFLOW)
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                    IF(K1.LE.NE.AND.K2.LE.NE) THEN
                       NC1 = NC1 + 1
                       IFLOW(NC1,1) = K1
                       IFLOW(NC1,2) = K2
                       IC1(NC1) = II
                       END IF
                    END IF
400             CONTINUE
C
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C     >     5TH, DO SECOND PASS, IF K2 CORRESPONDS TO A NON-CONDUIT 
C     >     ELEMENT, TO DETERMINE WHICH (IF ANY) WASP SEGMENT IS BELOW IT
C     >     THAT K1 IS CONNECTED TO (FLOW FROM K1 TO K2)
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                L = JR(ISS)
                NTPET = NTYPE(L)
                IF(NTPET.GE.19.AND.NTPET.NE.22) THEN
                   DO 600 J=1,NE
                   IMM = JR(J)
                   DO 600 K=1,3
                     IF(INUE(IMM,K).EQ.L.AND.JUNSEG(IMM).GT.0
     +                    .AND.JUNSEG(IMM).LT.NE) THEN
                          K2 = JUNSEG(IMM)
                          NC1 = NC1 + 1
                          IFLOW(NC1,1) = K1
                          IFLOW(NC1,2) = K2
                          IC1(NC1) = II
                          END IF
600                CONTINUE
                   END IF

                END IF
300          CONTINUE

C        G) WRITE HEADER INFORMATION TO LINKAGE FILE
C           (This is only written once).
C           ----------------------------------------
C
          WRITE (N22, 3000) NSEG, NC1, DELTQ, TTAPE, TEND, IDEP
C
       WRITE(N6,2100)
       DO 700 II=1,NC1
          WRITE(N22,2000)(IFLOW(II,J),J=1,2)
          WRITE(N6,2000) (IFLOW(II,J),J=1,2)
700    CONTINUE
C
C=======================================================================
C      END OF CALCULATIONS FOR JNEXT = 0
C=======================================================================
       ELSE
C=======================================================================
       TIMHRS = TIME/3600.
       IF(TIMHRS.GE.TSTART) THEN
C
C  ###########################################################
C  #    UPON  2ND CALL TO THIS SUBROUTINE, COMPUTE AND WRITE #
C  #    OUT VOLUMES, DEPTHS, VELOCITIES, AND FLOWS           #
C  ###########################################################
C
        DUM = 0.
C  A) 1ST COMPUTE VOLUMES, DEPTHS AND VELOCITIES
C  ----------------------------------------------
           DO 1000 I=1,NE
              M = JR(I)
              NTPE = NTYPE(M)
              IF(JUNSEG(M).GT.0.AND.JUNSEG(M).LE.NE) THEN
C
C  VOLUMES ARE PLACED ON INTERFACE FILE IN ORDER OF COMPUTATION.
C  REMEMBER, CAN'T HAVE JUNSEG > NE.
C
               ISEG  = JUNSEG(M)
               QIN1  = Q(M,1,1)
               QOUT1 = Q(M,2,1)
               QIN2  = Q(M,1,2)
               QOUT2 = Q(M,2,2)
C=======================================================================
C     1) Compute conduit volumes, depths and velocities. Conduit 
C        volume found by averaging upstream and downstream
C        flow areas and by multiplying by length of conduit.
C
C=======================================================================
      IF(NTPE.LE.18) THEN
C
    
C a)  compute average cross-sectional areas at old and new times
C
C
            AREA1 = (A(M,1,1) + A(M,2,1))/2.0*BARREL(M)
            AREA2 = (A(M,1,2) + A(M,2,2))/2.0*BARREL(M)
C
C
C  b) compute volumes at old and new times

            VOL1  = DIST(M)*AREA1
            VOL2  = DIST(M)*AREA2

C  c)  compute velocities at old and new times

         IF(AREA1.GT.0.AND.AREA2.GT.0) THEN
            VEL1  = (QIN1+QOUT1)/(2*AREA1)
C#### WCH, 6/5/95.  VEL2 NOT USED.  ELIMINATE SO NOT TO ANNOY LAHEY.
C####            VEL2  = (QIN2+QOUT2)/(2*AREA2)
          ELSE
            VEL1  = 0.0
C####            VEL2  = 0.0
          END IF
C
C   d) compute depths at old and new times
C
                     A1   = A(M,1,1)/AFULL(M)
                     A2   = A(M,2,1)/AFULL(M)
                     A3   = (A1+A2)/2.0
                     A4 =    DEPTH(A3)
                     DEPTH1 =  A4*GEOM1(M)
                     A1   = A(M,1,2)/AFULL(M)
                     A2   = A(M,2,2)/AFULL(M)
                     A3   = (A1+A2)/2.0
                     A4 =    DEPTH(A3)
                     DEPTH2 =  A4*GEOM1(M)
C
            ENDIF
C=======================================================================
C     Storage unit.  Retrieve old and new storage volumes.
C=======================================================================
      IF(NTPE.EQ.22) THEN
C
C   a)  determine volumes
                   LSTOR = KSTORE(M)
                   VOL1  = STORL(LSTOR)
                   VOL2  = STORE(LSTOR)
C   b)  determine depths by interpolatiom
      MINT   = MINTS(LSTOR)
          DO 110 J = 1,MINT
               SDE(J)   = TSDEP(LSTOR,J)
               SST(J)   = TSTORE(LSTOR,J)
110       CONTINUE
                   CALL TINTRP(SST,SDE,MINT,VOL1,DEP)
                    DEPTH1 = DEP
                   CALL TINTRP(SST,SDE,MINT,VOL2,DEP)
                    DEPTH2 = DEP
C
C   c)  determine velocities  (assumes GEOM1(M) is average width)
C
             IF(GEOM1(M).GT.0)THEN
                VEL1  = (QIN1+QOUT1)/(2*DEPTH1*GEOM1(M))
C#### WCH, 6/5/95.  VEL2 NOT USED.  ELIMINATE SO NOT TO ANNOY LAHEY.
C####                VEL2  = (QIN2+QOUT2)/(2*DEPTH2*GEOM1(M))
             ELSE
                     VEL1 = 0.
C####                     VEL2 = 0.
             END IF
C
                   ENDIF
C=======================================================================
C
C   CONVERT VOLUMES, DEPTHS AND VELOCITIES TO METRIC
C
               WVOL(ISEG) = VOL1 * 0.02832
               WDEP(ISEG) = DEPTH1 * 0.3048
               WVEL(ISEG) = VEL1 * 0.3048
C
                   IF(VOL1.LE.0)THEN
                     WRITE(N6,6000)TIMHRS,M
                     WRITE(*,6000)TIMHRS,M
                   END IF
               END IF
1000      CONTINUE
C
C  B) 2ND WRITE OUT VOLUMES
C     ---------------------
       DO 1500 I=1,NSEG
               IF(IFIRST.EQ.0.OR.IDEP.EQ.0) THEN
                  WRITE(N22,4000)WVOL(I),DUM,WDEP(I),WVEL(I)
               ELSE
                  WRITE(N22,4000)WVOL(I)
               END IF
1500   CONTINUE
       IFIRST = 1
C
C  C) FINALLY, WRITE OUT FLOWS
C   --------------------------
C
       DO 1100 I=1,NC1
           IFL = IC1(I)
           M =   JR(IFL)
           NTPET = NTYPE(M)
           IF(NTPET.LT.19.OR.NTPET.EQ.22) THEN
              QQ = Q(M,2,1)
            ELSE
              QSUM = 0.
              DO 1200 J=1,3
                 JS = INUE(M,J)
                 IF(JUNSEG(JS).LE.NE) QSUM = QSUM + Q(JS,2,1)
1200          CONTINUE
              QQ = Q(M,2,1) - QSUM
            END IF
C
C   CONVERT INTERNAL TRANSPORT FLOWS FROM CFS TO CMS
C
              QQ = QQ * 0.02832
              WRITE(N22,5000) QQ
1100        CONTINUE

C WRITE OUT CONCENTRATION DATA TO DUMMY FILE FOR COMPARISONS
C
             IF(ISUMRY.EQ.99) THEN
                WRITE(N23,4001)TIMHRS,(CPOL2(JR(IC1(KK)),2,1),KK=1,NC1)
4001           FORMAT(20F8.3)
              END IF
C 
C       END IF STATEMENTS FOR TIME CHECK AND FOR MAIN (NSCRATCH LOOP)
C
         END IF
       END IF
C
C#### WCH, 10/93.
  960  FORMAT(//,
     1' **********************************************************',/,
     2' * TRANSPORT OUTPUT WILL BE WRITTEN TO A LINKAGE FILE     *',/,
     3' * FOR WASP USING THE FOLLOWING DATA FROM H2 AND H3 GROUPS*',/,
     4' **********************************************************',//
     4         ,' LINKAGE FILE NAME............................',A60,/
     5         ,' START TIME FOR WASP LINKAGE (HOURS)..........',
     6 F8.2,/  ,' VELOCITY/DEPTH OPTION (O=TIME VARIANT).......',
     7 I8,/    ,' NUMBER OF ELEMENTS TO MAP TO WASP............',I8,
     8 //,'  EXTERNAL      INTERNAL        WASP',/,
     9    ' ELEMENT NO.   ELEMENT NO.   SEGMENT NO.',/,
     1    ' -----------   -----------   -----------')
C#### WCH, 10/93.
  961 FORMAT(1X,I10,I11,I13)
  962 FORMAT(1X,A10,I11,I13)
C#### WCH, 1/3/94.
  975 FORMAT(/,' TIMING PROBLEM WITH WASP LINKAGE.  MUST HAVE TZERO <= T
     1START < TEND.',/,' BUT ENTERED VALUES ARE:',/,
     2' TZERO  = ',F7.2,' HRS',/,
     3' TSTART = ',F7.2,' HRS',/,
     4' TEND   = ',F7.2,' HRS = TZERO + NDT*DT',/,
     5' RUN STOPPED AFTER READING LINE H2 FROM TRANSPORT BLOCK.')  
C4000      FORMAT(F10.2,I5,2(F12.2,2F10.2,2F10.4))
2000      FORMAT(2I5)
2100  FORMAT(/,
     1' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>',/,
     2' > FLOWS WILL BE MAPPED FROM AND TO THE FOLLOWING WASP   >',/,
     3' > SEGMENTS: NOTE: ANY SEGMENT WITH A 0 (BOUNDARY) FLOW  >',/,
     4' >   MUST HAVE A BOUNDARY CONDITION IN WASP INPUT        >',/,
     4' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>')
3000  FORMAT(2I5,3F20.5,I5)
4000  FORMAT(4F20.5)
5000  FORMAT(F20.5)
6000  FORMAT(//,
     1' **********************************************************',/,
     2' * WARNING !!!!  ZERO VOLUMES WRITTEN TO WASP LINKAGE FILE ',/,
     3' * AT TIME = ',F10.3,'  HOURS, AT INTERNAL ELEMENT  ',I4,/,
     4' **********************************************************')
      RETURN
  888 CALL IERROR
      END
