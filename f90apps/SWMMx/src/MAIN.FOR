      PROGRAM SWMM43
C#######################################################################
C            EPA Storm Water Management Model Main Program
C
C     This is the Executive Block and includes calls to graph routines.
C#######################################################################
C     This program reads the off-line file assignments, and then reads
C     the alphanumeric codes for the block to be called.  Each major
C     subroutine returns here for new block assignments.  Last updated
C     September, 1993 by the Oregon State University with help from
C     Robert E. Dickinson (RED) and Chuck Moore (from CDM).
C     Add elapsed time in seconds, RED, 11/29/93.
C#######################################################################
      INCLUDE 'TAPES.INC'
C#### WCH (CDM), 8/93.  ADD OPTION FOR COMMAND LINE FILE NAMES.
      INCLUDE 'CLNAME.INC'
C
C#### DWD (CSC) - Begin change.
C     Date: Wednesday, 11 May 1994.  Time: 08:37:28.
C     Added code for Lahey Date/Time routines.
      CHARACTER*11 SYSDAT, SYSTIM
C#### DWD (CSC) - End change.
C     Date: Wednesday, 11 May 1994.  Time: 08:37:28.
C
C#### WCH (CDM), 8/93. REMOVE BMJ*1 (AND DATA STATEMENT)
      CHARACTER CNAME*4,FNAMI*60,FNAMO*60,FINTER*60,MNAME(12)*10
CJK   LOGICAL*1 HAMLET
      LOGICAL   HAMLET
C####      DATA BMJ/' '/
      DATA MNAME/'January   ','February  ','March     ','April     ',
     +           'May       ','June      ','July      ','August    ',
     +           'September ','October   ','November  ','December  '/
C=======================================================================
C                 N5 = Data input,  (Set = 98 in BLKMN.FOR).
C                 N6 = Data output, (Set = 99 in BLKMN.FOR).
C     Parameters N5 and N6 are used in read\write statemtnts in
C     all the blocks of SWMM.  This is the location to change unit
C     number assignments if necessary.
C=======================================================================
C     Read initial time and date using GETTIM and GETDAT
C     Subroutines that are RM-FORTRAN specific.
C=======================================================================
C#### DWD (CSC) - Begin change.
C     Date: Wednesday, 11 May 1994.  Time: 08:36:07.
C     Added code for Lahey Date/Time routines.
C     Lahey date format: SYSDAT(MM/DD/YY)
C     Lahey time format: SYSTIM(HH:MM:SS:HH)
      CALL DATE(SYSDAT)
      CALL TIME(SYSTIM)
    4 FORMAT(I2)
      READ(SYSDAT(1:2),4) N11
      READ(SYSDAT(4:5),4) N12
      READ(SYSDAT(7:8),4) N13
      READ(SYSTIM(1:2),4) M11
      READ(SYSTIM(4:5),4) M12
      READ(SYSTIM(7:8),4) M13
      READ(SYSTIM(10:11),4) M14
C
C      CALL GETTIM(M11,M12,M13,M14)
C      CALL GETDAT(N11,N12,N13)
C#### DWD (CSC) - End change.
C     Date: Wednesday, 11 May 1994.  Time: 08:36:07.
C=======================================================================
C     Read input and output file names from screen.
C=======================================================================
C#### DWD (CSC) - Begin change.
C     Date: Thursday, 12 May 1994.  Time: 15:53:08.
C     Added microcomputer specific clear screen WRITE statement.
      WRITE(*,*) ' [2J'
C#### DWD (CSC) - End change.
C     Date: Thursday, 12 May 1994.  Time: 15:53:08.
      WRITE(*,15)
      N55   = 55
      JCE   = 0
      NOQ   = 0
C#### WCH (CDM), 8/93.  FOR COMMAND LINE FILE NAME INPUT:
      CALL CLINE
C=======================================================================
C     IF FILE 'GENERIC.INP' WAS NOT DEFINED ON THE COMMAND LINE
C        PROMPT THE USER FOR INPUT AND OUTPUT FILE NAMES
C=======================================================================
      INQUIRE(FILE='GENERIC.INP',EXIST=HAMLET)
      IF(.NOT.HAMLET) GO TO 3100
      OPEN(N55,ERR=3100,FILE='GENERIC.INP',STATUS='OLD')
      OPEN(N5,STATUS='SCRATCH')
      FNAMI = 'GENERIC.INP'
      FNAMO = 'GENERIC.OUT'
      OPEN(N6,FILE='GENERIC.OUT',STATUS='UNKNOWN')
      GO TO 3200
C#######################################################################
C     WCH (CDM - CHUCK MOORE), 8/93.  ADD OPTION FOR COMMAND LINE
C     INPUT OF INPUT AND OUTPUT FILE NAMES.
C=======================================================================
 3100 IF(INAMES.LT.NNAMES) THEN
         INAMES = INAMES + 1
         FNAMI  = CNAMES(INAMES)
         WRITE (*,*) ' INPUT FILE NAME FROM COMMAND LINE IS:  ',FNAMI
         ELSE
C       
         WRITE(*,1)
         READ(*,2) FNAMI
         ENDIF
      OPEN(N55,ERR=3000,FILE=FNAMI,STATUS='OLD')
      OPEN(N5,STATUS='SCRATCH')
C#### WCH (CDM), 8/93
      IF(INAMES.LT.NNAMES) THEN
         INAMES = INAMES + 1
         FNAMO  = CNAMES(INAMES)
         WRITE (*,*) ' OUTPUT FILE NAME FROM COMMAND LINE IS:  ',FNAMO
         ELSE
C
         WRITE(*,3)
         READ(*,2) FNAMO
         ENDIF
C=======================================================================
C#### WCH, 5/10/94.  Add error check for duplicate I/O file names.
C=======================================================================
      IF(FNAMO.EQ.FNAMI) THEN
           WRITE(*,9000)  FNAMO
           STOP
           ENDIF
C
      OPEN(N6,FILE=FNAMO,STATUS='UNKNOWN')
C=======================================================================
C     Strip comment lines and make temporary file containing the data.
C=======================================================================
 3200 CALL STRIP(N55)
      READ(N5,*,ERR=888) CC,NBLOCK,(JIN(J),JOUT(J),J=1,NBLOCK)
      READ(N5,*,ERR=888) CC,NITCH,(NSCRAT(I),I=1,NITCH)
      WRITE(N6,10)
      WRITE(N6,15)
      DO 80 I = 1,NBLOCK
      IF(JIN(I).EQ.5.OR.JIN(I).EQ.6.OR.JIN(I).EQ.55)    CALL ERROR(117)
      IF(JOUT(I).EQ.5.OR.JOUT(I).EQ.6.OR.JOUT(I).EQ.55) CALL ERROR(117)
   80 CONTINUE
      DO 90 I = 1,NITCH
      IF(NSCRAT(I).EQ.5.OR.NSCRAT(I).EQ.6.OR.NSCRAT(I).EQ.55)
     +                                         CALL ERROR(117)
   90 CONTINUE
C=======================================================================
C     @ - READ THE NAMES OF PERMANENTLY SAVED
C         INTERFACE OR SCRATCH FILES
C     $ - READ THE NAME OF THE NEXT PROGRAM BLOCK
C=======================================================================
 100  READ(N5,*,ERR=888) CC
      IF(CC.EQ.'$') THEN
                    BACKSPACE N5
                    READ(N5,110,ERR=888) CNAME
                    ELSE IF(CC.EQ.'@') THEN
                              BACKSPACE N5
                              READ(N5,*,ERR=888) CC,IFF,FINTER
                              IF(IFF.EQ.5.OR.IFF.EQ.6.OR.
     +                                       IFF.EQ.55) CALL ERROR(117)
                              DO 50 J = 1,NBLOCK
                              IF(JIN(J).EQ.IFF)  THEN
                                                 FFNAME(J) = FINTER
                                                 ENDIF
                              IF(JOUT(J).EQ.IFF) THEN
                                                 FFNAME(J+25) = FINTER
                                                 ENDIF
  50                          CONTINUE
                              DO 75 J    = 1,NITCH
                              IF(NSCRAT(J).EQ.IFF) THEN
                                                   JKP(J+50)    = 1
                                                   FFNAME(J+50) = FINTER
                                                   GO TO 76
                                                   ENDIF
  75                          CONTINUE
  76                          CONTINUE
                    GO TO 100
                    ENDIF
C=======================================================================
C     Find similar named interface and scratch files.
C=======================================================================
      DO 34 J = 1,NBLOCK
      DO 21 K = J,NBLOCK
      IF(JIN(K).EQ.JOUT(J)) THEN
                            FFNAME(K) = FFNAME(25+J)
                            JKP(K)    = 1
                            JKP(25+J) = 1
                            ENDIF
   21 CONTINUE
      DO 25 K = 1,NITCH
      IF(NSCRAT(K).EQ.JIN(J))  THEN
                               FFNAME(50+K) = FFNAME(J)
                               JKP(50+K)    = 2
                               JKP(J)       = 1
                               ENDIF
   25 CONTINUE
      DO 27 K = 1,NITCH
      IF(NSCRAT(K).EQ.JOUT(J)) THEN
                               FFNAME(50+K) = FFNAME(25+J)
                               JKP(50+K)    = 2
                               JKP(25+J)    = 1
                               ENDIF
   27 CONTINUE
      DO 32 K = J,NBLOCK
      IF(JIN(K).EQ.JIN(J)) THEN
                           FFNAME(K) = FFNAME(J)
                           JKP(J)    = 1
                           JKP(K)    = 1
                           ENDIF
   32 CONTINUE
   34 CONTINUE
C=======================================================================
C     Write the names of the JIN and JOUT files.
C=======================================================================
      WRITE(N6,45)
      DO 450       J = 1,NBLOCK
      WRITE(N6,55) J,JIN(J),FFNAME(J)
      WRITE(N6,56) J,JOUT(J),FFNAME(25+J)
  450 CONTINUE
C=======================================================================
C     Write the names of the SCRATCH files.
C=======================================================================
      WRITE(N6,46)
      DO 460       J = 1,NITCH
      WRITE(N6,57) J,NSCRAT(J),FFNAME(50+J)
  460 CONTINUE
C=======================================================================
      WRITE(N6,300) NW,NG,NRQ,NLU,NET,NTSE,NTH,NEE,NGW,NIE,NEP,NEO,
     +              NTG,NEW,NPO,NTE,NNC,NVSE,NTVAL,NVST,NEH
C=======================================================================
C     Call the individual SWMM 4.3 blocks.
C=======================================================================
 2000 CONTINUE
      write(*,*) 'main:2000 loop',cname
C=======================================================================
C     Close the last JIN, JOUT and NSCRAT files.
C=======================================================================
      IFIND = 0
      IF(INCNT.GT.0.AND.JIN(INCNT).GT.0.AND.JKP(INCNT).NE.1)
     +                                     CLOSE(JIN(INCNT))
      IF(INCNT.GT.0.AND.JOUT(IOUTCT).GT.0.AND.JKP(25+IOUTCT).NE.1)
     +                                         CLOSE(JOUT(IOUTCT))
      IF(INCNT.GT.0.AND.NSCRAT(1).GT.0.AND.JKP(51).NE.2)CLOSE(NSCRAT(1))
      IF(INCNT.GT.0.AND.NSCRAT(2).GT.0.AND.JKP(52).NE.2)CLOSE(NSCRAT(2))
      IF(INCNT.GT.0.AND.NSCRAT(3).GT.0.AND.JKP(53).NE.2)CLOSE(NSCRAT(3))
      IF(INCNT.GT.0.AND.NSCRAT(4).GT.0.AND.JKP(54).NE.2)CLOSE(NSCRAT(4))
      IF(INCNT.GT.0.AND.NSCRAT(5).GT.0.AND.JKP(55).NE.2)CLOSE(NSCRAT(5))
      IF(INCNT.GT.0.AND.NSCRAT(6).GT.0.AND.JKP(56).NE.2)CLOSE(NSCRAT(6))
      IF(INCNT.GT.0.AND.NSCRAT(7).GT.0.AND.JKP(57).NE.2)CLOSE(NSCRAT(7))
C#### WCH, 12/31/93.  ALSO CLOSE NSCRAT(8).
      IF(INCNT.GT.0.AND.NSCRAT(8).GT.0.AND.JKP(58).NE.2)CLOSE(NSCRAT(8))
C=======================================================================
C    ANUM ==> ALPHANUMERIC LABELS (IDs).
C=======================================================================
      IF(CNAME.EQ.'ANUM') THEN
                          IFIND = 1
                          JCE   = 1
                          ENDIF
C=======================================================================
C    NOQU ==> NO LITERARY QUOTES DISPLAYED OR PRINTED.
C=======================================================================
      IF(CNAME.EQ.'NOQU') THEN
                          IFIND = 1
                          NOQ   = 1
                          ENDIF
      IF(CNAME.EQ.'ENDP') THEN
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'GRAP') THEN
C                         CALL GRAPH
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'COMB') THEN
C                         CALL COMBIN
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'RAIN') THEN
C                         CALL RAIN
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'TEMP') THEN
C                         CALL TEMP
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'RUNO') THEN
C                         CALL RUNOFF
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'TRAN') THEN
                          write(*,*) 'main:call trans'
                          CALL TRANS
                          write(*,*) 'main:back from trans'
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'STOR') THEN
C                         CALL STRT
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'STAT') THEN
C                         CALL STATS
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'EXTR') THEN
                          write(*,*) 'main:call extran'
                          CALL EXTRAN
                          write(*,*) 'main:back from extran'
                          IFIND = 1
                          ENDIF
      IF(CNAME.EQ.'ENDP'.OR.IFIND.EQ.0) THEN
                          IF(IFIND.EQ.0) THEN
                                         WRITE(*,230)  CNAME
                                         WRITE(N6,230) CNAME
                                         ELSE
                                         WRITE(*,185)
                                         WRITE(N6,185)
                                         ENDIF
                          IF(FNAMI.NE.'GENERIC.INP') THEN
                             WRITE(*,180)  FNAMI,FNAMO
                             WRITE(N6,180) FNAMI,FNAMO
                             ENDIF
C=======================================================================
C     Read last time and date using GETTIM and GETDAT
C     Subroutines that are RM-FORTRAN specific.
C=======================================================================
C#### DWD (CSC) - Begin change.
C     Date: Wednesday, 11 May 1994.  Time: 08:41:21.
C     Added code for Lahey Date/Time routines.
                          write(*,*) 'main,check date'
                          CALL DATE(SYSDAT)
                          write(*,*) 'main,check time'
                          CALL TIME(SYSTIM)
                          READ(SYSDAT(1:2),4) N21
                          READ(SYSDAT(4:5),4) N22
                          READ(SYSDAT(7:8),4) N23
                          READ(SYSTIM(1:2),4) M21
                          READ(SYSTIM(4:5),4) M22
                          READ(SYSTIM(7:8),4) M23
                          READ(SYSTIM(10:11),4) M24
C                          CALL GETTIM(M21,M22,M23,M24)
C                          CALL GETDAT(N21,N22,N23)
                          IF(N22.NE.N12) M22 = M22 + 24
C                          IF(N23.NE.N13) M22 = M22 + 24
                          ELAPSE = FLOAT(M21)*60.0 + FLOAT(M22) +
     +                             FLOAT(M23)/60.0
                          ELAPSE = ELAPSE     - FLOAT(M11)*60.0 -
     +                             FLOAT(M12) - FLOAT(M13)/60.0
C#### WCH (RED), 11/29/93.  ADD ELAPSED TIME IN SECONDS.
                          ELAPSS = ELAPSE*60.0
                          WRITE(N6,310) MNAME(N11),N12,N13,M11,M12,
     +                                  M13,M14,MNAME(N21),N22,N23,
     +                                  M21,M22,M23,M24,ELAPSE,ELAPSS
C                          WRITE(N6,310) MNAME(N12),N13,N11,M11,M12,
C     +                                  M13,M14,MNAME(N22),N23,N21,
C     +                                  M21,M22,M23,M24,ELAPSE,ELAPSS
C#### DWD (CSC) - End change.
C     Date: Wednesday, 11 May 1994.  Time: 08:41:21.
C=======================================================================
                          STOP
                          ENDIF
C=======================================================================
C     Read the new block name.
C=======================================================================
      READ(N5,110,ERR=888,END=888) CNAME
      GO TO 2000
3000  WRITE(*,3001)
3001  FORMAT(' ===> File error !!! - Missing input file.',/)
      GO TO 3100
 888  CALL IERROR
      STOP
C=======================================================================
C
C#### DWD (CSC) - Begin change.
C     Date: Tuesday, 10 May 1994.  Time: 12:23:37.
C     Modified file name screen prompts to improve user/program interface
C
C    1 FORMAT(' Enter your input file name  -  ')
C    3 FORMAT(' Enter your output file name -  ')
C
    1 FORMAT(1X,'Type input file name, then press <Enter> -> ')
    2 FORMAT(A)
    3 FORMAT(1X,'Type output file name, then press <Enter> -> ')
C
C#### DWD (CSC) - End change.
C     Date: Tuesday, 10 May 1994.  Time: 12:23:37.
C
C#### DWD (CSC) - Begin change.
C     Date: Tuesday, 10 May 1994.  Time: 12:28:28.
C     Modified/updated output header information
C
   10 FORMAT(1H1,T11,
     1'*************************************************',/,T11,
     1'*    U.S. Environmental Protection Agency       *',/,T11,
     1'*    Storm Water Management Model  (SWMM)       *',/,T11,
     1'*                Version 4.30                   *',/,T11,
     2'*************************************************',//,T11,
     3'                 Developed by                    ',//,T11,
     3'*************************************************',/,T11,
     2'*             Metcalf & Eddy, Inc.              *',/,T11,
     3'*            University of Florida              *',/,T11,
     4'*       Water Resources Engineers, Inc.         *',/,T11,
     4'*     (Now Camp, Dresser and McKee, Inc.)       *',/,T11,
     4'*              September 1970                   *',/,T11,
     5'*************************************************',//,T11,
     4'         Distributed and Maintained by           ',//,T11,
     4'*************************************************',/T11,
     4'*      U.S. Environmental Protection Agency     *',/,T11,
     4'* Center for Exposure Assessment Modeling (CEAM)*'/,T11,
     5'*    Athens Environmental Research Laboratory   *',/,T11,
     5'*            960 College Station Road           *',/,T11,
     4'*            Athens, GA    30605-2720           *',/,T11,
     4'*************************************************',//,T11,
     4'*************************************************',/,T11,
     5'*     This is a new release of SWMM.  If any    *',/,T11,
     6'*     problems occur executing this model       *',/,T11,
     7'*     system, contact Mr. Dermont Bouchard,     *',/,T11,
     8'*     U.S. Environmental Protection Agency.     *',/,T11,
     8'*     706/546-3130 (voice) 706/546-3402 (BBS)   *'/,T11,
     8'*     e-mail: bouchard@athens.ath.epa.gov       *'/,T11,
     9'*************************************************',/)
   15 FORMAT(T11,
     @'*************************************************',/,T11,
     1'*  This is an implementation of EPA SWMM 4.30.  *',/,T11,
     1'*  "Nature is full of infinite causes which     *',/,T11,
     1'*   have never occurred in experience" da Vinci *',/,T11,
     3'*************************************************',/)
C
C#### DWD (CSC) - End change.
C     Date: Tuesday, 10 May 1994.  Time: 12:28:28.
C
   45 FORMAT(/,
     1' ###########################################',/,
     2' #        File names by SWMM Block         #',/,
     3' #         JIN  -> Input to a Block        #',/,
     4' #        JOUT  -> Output from a Block     #',/,
     5' ###########################################',/)
   46 FORMAT(/,
     1' ###########################################',/,
     2' # Scratch file names for this simulation. #',/,
     5' ###########################################',/)
   55 FORMAT('    JIN for Block # ',I5,' File #',I5,2X,A60)
   56 FORMAT('   JOUT for Block # ',I5,' File #',I5,2X,A60)
   57 FORMAT(' NSCRAT # ',I5,' File #',I5,2X,A60)
  110 FORMAT(4X,A4)
  180 FORMAT(/,' ===> Your input file was named : ',A24,/,
     +         ' ===> Your output file was named: ',A24)
C
C#### DWD (CSC) - Begin change.
C     Date: Tuesday, 10 May 1994.  Time: 12:30:33.
C     Updated version number to 4.30 from 4.3
C
  185 FORMAT(/,' ===> SWMM 4.30 simulation ended normally.')
C
C#### DWD (CSC) - End change.
C     Date: Tuesday, 10 May 1994.  Time: 12:30:33.
C
  230 FORMAT(/,' ===> Correct Block Name not found. ',/,
     1         ' ===> ',A4,' was found instead.  Program stops.',/,
     2    ' ===> Check your data input for the following problems:',/,
     3    '      1.  Using the wrong executable file.',/,
     4    '      2.  Too many hydrograph input data lines.',/,
     5    '      3.  SWMM Block is commented out of MAIN.FOR',/,
     6    '      4.  Wrong input sequence of data (likely!).',/)
  300 FORMAT(/,
     +' ***************************************************',/,
     +' *    Parameter Values on the Tapes Common Block   *',/,
     +' ***************************************************',//,
     +'  Number of Subcatchments in the Runoff Block (NW)....',I5,/,
     +'  Number of Channel/Pipes in the Runoff Block (NG)....',I5,/,
     +'  Number of Runoff Water Quality Constituents (NRQ)...',I5,/,
     +'  Number of Runoff Land Uses per Subcatchment (NLU)...',I5,/,
     +'  Number of Elements in the Transport Block (NET).....',I5,/,
     +'  Number of Storage Junctions in Transport (NTSE).....',I5,/,
     +'  Number of Input Hydrographs in Transport (NTH)......',I5,/,
     +'  Number of Elements in the Extran Block (NEE)........',I5,/,
     +'  Number of Groundwater Subcatchments in Runoff (NGW).',I5,/,
     +'  Number of Interface locations for all Blocks (NIE)..',I5,/,
     +'  Number of Pumps in Extran (NEP).....................',I5,/,
     +'  Number of Orifices in Extran (NEO)..................',I5,/,
     +'  Number of Tide Gates/Free Outfalls in Extran (NTG)..',I5,/,
     +'  Number of Extran Weirs (NEW)........................',I5,/,
     +'  Number of Extran Printout Locations (NPO)...........',I5,/,
     +'  Number of Tide Elements in Extran (NTE).............',I5,/,
     +'  Number of Natural Channels (NNC)....................',I5,/,
     +'  Number of Storage Junctions in Extran (NVSE)........',I5,/,
     +'  Number of Time History Data Points in Extran(NTVAL).',I5,/,
     +'  Number of Data Points for Variable Storage Elements',/,
     +'  in the Extran Block (NVST)..........................',I5,/,
     +'  Number of Input Hydrographs in Extran (NEH).........',I5,//)
C#### WCH (RED), 11/29/93.  ADD SECONDS.
C
C#### DWD (CSC) - Begin change.
C     Date: Tuesday, 10 May 1994.  Time: 12:32:01.
C     Updated version number to 4.30 from 4.3
C
  310 FORMAT(/,
     +' ******************************************************',/,
     +' *       SWMM 4.30 Simulation Date and Time Summary   *',/
     +' ******************************************************',/,
     +' * Starting Date... ',A10,I3,', ',I4,'                *',/,
     +' *          Time...         ',3(I2,':'),I2,'                *',/,
     +' *   Ending Date... ',A10,I3,', ',I4,'                *',/,
     +' *          Time...         ',3(I2,':'),I2,'                *',/,
     +' *  Elapsed Time...          ',F10.3,' minutes.       *',/,
     +' *  Elapsed Time...          ',F10.3,' seconds.       *',/,
     +' *******************************************************',/)
C
C#### DWD (CSC) - End change.
C     Date: Tuesday, 10 May 1994.  Time: 12:32:01.
C=======================================================================
C#### WCH, 5/10/94.
 9000 FORMAT(/,' WHOOPS! YOUR INPUT FILE NAME IS THE SAME AS YOUR OUTPUT
     1 FILE NAME:',/,A61,/,' THIS WILL CAUSE YOUR INPUT FILE TO BE OVER-
     2WRITTEN.',/,' CAN''T HAVE THAT! PLEASE CHANGE ONE NAME. RUN STOPPE
     3D.')
C=======================================================================
      END

