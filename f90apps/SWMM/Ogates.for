      SUBROUTINE OGATES(DELTT,YYY,VVV)
C     EXTRAN BLOCK
C==========================================================================
C  THIS SUBROUTINE WAS CREATED TO PERFORM CALCULATIONS FOR GATED ORIFICES.
C   C. MOORE  1/96
C  TYPO CORRECTION, 7/97, CIM.  
      INCLUDE 'TAPES.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'JUNC.INC'
      DIMENSION YYY(NEE),VVV(NEE)
      IF (NOGATES.EQ.0) RETURN
      DO 500 I=1,NORIF
      N=LORIF(I)
      ICN=ICNODE(I)
CIM      write(7,*) icn,icyc,time/3600.0
      IF  (ICN)  100,500,200
CIM  GOES HERE IF ICNODE<0 - TIMED CLOSURE GATED CONTROL      
CIM  FIND TARGET OPENING BASED ON DEPTH AT CONTROL JUNCTION
  100 ICN=-ICN
CIM      write(7,*) 'case 1'
      TAREA=AFULL(N)
      IF (OOPEN(I).GE.OCLOSE(I)) THEN 
          IF (YYY(ICN).LE.OCLOSE(I)) TAREA=OCAREA(I)
          IF (YYY(ICN).GE.OOPEN(I)) TAREA=AORIF(I)
      ELSE
          IF (YYY(ICN).LE.OOPEN(I)) TAREA=AORIF(I)
          IF (YYY(ICN).GE.OCLOSE(I)) TAREA=OCAREA(I)
      ENDIF
CIM      write(7,*) tarea
      GO TO 400
CIM  GOES HERE IF ICNODE>0 - HEAD DEPENDENT GATED CONTROL
  200 RATIO = (YYY(ICN)-OCLOSE(I))/(OOPEN(I)-OCLOSE(I))
      RATIO = AMAX1(RATIO,0.0)
      RATIO = AMIN1(RATIO,1.0)
      TAREA = OCAREA(I) + RATIO*(AORIF(I)-OCAREA(I))
CIM      write(7,*) 'case 2',RATIO,TAREA
  400 CONTINUE
CIM  NOW CHECK FOR MAXIMUM RATE OF CLOSURE AND COMPUTE PARAMETERS   
CIM  ONLY IF TAREA CHANGES
      IF (ABS(TAREA-AFULL(N)).GT.0.0000001) THEN 
CIM  CHECK FOR ZERO TAREA AND MAKE IT A SMALL NUMBER TO AVOID ZERO DIVIDES
      TAREA = amax1(TAREA,0.00001)
      DAREA = TAREA-AFULL(N)
      DMAX = ORATE(I)*DELTT
CIM      write(7,*)  'case 3',darea,dmax,deltt
      IF(ABS(DAREA).GT.DMAX) TAREA = AFULL(N)+SIGN(DMAX,DAREA)
      IF (ABS(TAREA-AORIF(I)).LE.0.00001) THEN 
               INGATE(N) = IDIR(I)
          ELSE
               INGATE(N) = 0
          ENDIF          
CIM          write(7,*) 'ingate',ingate(i)
      IF (NKLASS(N).EQ.51.OR.NKLASS(N).EQ.52) THEN
CIM  CIRCULAR ORIFICES
          DEEPOLD  = DEEP(N)
          DEEP(N)  = SQRT(4.0*TAREA/3.14159)
          WIDE(N)  = DEEP(N)
          AOLD = AFULL(N)
          AFULL(N) = TAREA
          RFULL(N) = DEEP(N)/4.0
CIM          write(7,*) 'circle',deep(n),wide(n),afull(n),rfull(n)
CIM CHANGE INVERT
CIM  NOTE  ZU + DEEPOLD = OLD CROWN ELEV.
CIM        ZU + DEEPOLD - DEEP = new invert elev.
cim 7/97 change IOINV(N) to IOINV(I) in next line 
                    IF (IOINV(I).EQ.1) then 
                    ZU(N) = ZU(N)+DEEPOLD-DEEP(N)
                    ZD(N) = ZD(N)+DEEPOLD-DEEP(N)
                    ENDIF
      ELSE
CIM  Rectangular orifices
          DEEPOLD  = DEEP(N)
          DEEP(N)  = TAREA/WIDE(N)
          AFULL(N) = TAREA
          RFULL(N) = AFULL(N)/(2.0*DEEP(N)+2.0*WIDE(N))
CIM          write(7,*) 'rectangle',deep(n),wide(n),afull(n),rfull(n)
CIM CHANGE INVERT
CIM  NOTE  ZU + DEEPOLD = OLD CROWN ELEV.
CIM        ZU + DEEPOLD - DEEP = new invert elev.
cim 7/97 change IOINV(N) to IOINV(I) in next line 
                    IF (IOINV(I).EQ.1) then 
                    ZU(N) = ZU(N)+DEEPOLD-DEEP(N)
                    ZD(N) = ZD(N)+DEEPOLD-DEEP(N)
                    ENDIF
      ENDIF
CIM      check length and adjust as necessary for currant condition
      CLEN     = 2.0*DELT*SQRT(GRVT*DEEP(N))
      LEN(N)   = AMAX1(200.,CLEN)
      CMANN    = CMET(9,METRIC)
      ROUGH(N) = CMANN*RFULL(N)**.66667/(CORIF(I)*SQRT(LEN(N)*2.0*GRVT))
CIM          write(7,*) delt,delt2,len(n),cmann,rough(n)
      IF (IOPRNT(I).EQ.1) 
C#### WCH, 8/8/97.  ADD TIME AND CYCLE PRINT OUT.
     .         WRITE(N6,6000) I,ICYC,TIME/3600.,AFULL(N),DEEP(N),
     1              LEN(N),ROUGH(N)
 6000 FORMAT(/,' REVISED GATED ORIFICE NO. ',I7,
     1', CYCLE NUMBER ',I7,
     2', TIME =',F12.3,' HOURS',/,
     3' AREA =',F12.3,
     4', DEEP =',F6.2,
     5', LEN =',F6.1,
     6', MANNINGS N =',F7.4)
C======================================================================
C     Conduit roughness initialization as used in INDAT3
C======================================================================
      ROUGH(N)  = GRVT*ROUGH(N)**2/CMET(9,METRIC)**2
CIM   ADJUST V FOR NEW AREA   NEEDED???      
      VVV(N) = VVV(N)*AOLD/AFULL(N)
      END IF
CIM      WRITE(7,*) I,N,YYY(ICN),OOPEN(I),OCLOSE(I),TAREA
CIM      WRITE(7,*) NKLASS(N),AFULL(N)
  500 CONTINUE      
      RETURN
      END
      
