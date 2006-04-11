       PROGRAM WDMPTRCK

       CHARACTER*64 WDNAME,NAME,EXPNAME,MESSNM
       INTEGER      WDMSFL,RONWFG,RETCOD,EXPFLG,ATMSFL

       OPEN(UNIT=99,FILE='ERROR.FIL')
       WDMSFL = 11
       RONWFG = 1

       OPEN(UNIT=10,FILE="WDMPTRCK.INP",STATUS="OLD")
10     CONTINUE
         READ(10,1000,END=20) EXPFLG,NAME
1000     FORMAT(I1,1X,A64)
         WDNAME = NAME
         WRITE(99,*) "PROCESSING ",WDNAME
         CALL WDBOPN
     I             (WDMSFL,WDNAME,RONWFG,
     O              RETCOD)
         WRITE(*,*) "WDMOPN: RETCOD ",RETCOD," FROM ",WDNAME
         IF ((RETCOD .EQ. -13 .AND. EXPFLG .EQ. 1).OR.EXPFLG.EQ.2) THEN
           EXPNAME = "TEMP.EXP"
           ATMSFL = 9
           MESSNM = "C:\LIB3.0\HSPFMSG.WDM"
           CALL WDBOPN(ATMSFL,MESSNM,I0,
     O                 RETCOD)
           CALL WDMDUMP(WDMSFL,ATMSFL,EXPNAME,RETCOD)
         END IF
         CALL WDFLCL (WDMSFL,RETCOD)
         WDMSFL = WDMSFL+ 1
         WRITE(*,*) "WDFLCL: RETCOD ",RETCOD
         WRITE(*,*)
         WRITE(99,*)
       GO TO 10
20     CONTINUE

       STOP
       END
