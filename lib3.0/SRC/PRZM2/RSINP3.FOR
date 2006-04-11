C
C
C
      SUBROUTINE   VADINP
     I                   (INPFL,OUTFL,TAP10,NLDLT,NCHEM,IVZONE,
     I                    TRNSIM,FLOSIM,PRZMON,FRSTRD,MCARLO)
C
C     + + + PURPOSE + + +
C     reads VADOFT input file
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INPFL,OUTFL,TAP10,NLDLT,IVZONE,NCHEM
      LOGICAL   TRNSIM,FLOSIM,PRZMON,FRSTRD,MCARLO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INPFL  - ???
C     OUTFL  - ???
C     TAP10  - ???
C     NLDLT  - ???
C     NCHEM  - ???
C     IVZONE - ???
C     TRNSIM - ???
C     FLOSIM - ???
C     PRZMON - ???
C     FRSTRD - ???
C     MCARLO - ???
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXTMV.INC'
      INCLUDE 'PMXTIM.INC'
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXPRT.INC'
      INCLUDE 'PMXNLY.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CSWHDA.INC'
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CVNTR2.INC'
      INCLUDE 'CVWRKM.INC'
      INCLUDE 'CWORKN.INC'
      INCLUDE 'CIN2VD.INC'
      INCLUDE 'CADISC.INC'
      INCLUDE 'CTPDEF.INC'
      INCLUDE 'CVVLM.INC'
      INCLUDE 'CBACK.INC'
      INCLUDE 'CPCHK.INC'
      INCLUDE 'CBDISC.INC'
      INCLUDE 'CDAOBS.INC'
      INCLUDE 'CVMISC.INC'
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL      FATAL,MOUTFL
      CHARACTER*80 TITLE,LINE,MESAGE,LINCOD
      INTEGER      IERROR,ISTRT,IEND,NLAYRG,
     1             IMAT,J,N,NPM1,NTN1,NTNP,IVSTED,ICNTER,
     2             NPIN,ICHEM,NDCOUN,ILAYR,NONU,IE,NEL,NDM1,IP1,
     3             I,IHORIZ,NCHMRD,ITSGN,IKALL,INTSPC,ITMGEN,
     4             JCK,NUMPI,NUMKI,II,JJ,KKK
      REAL         DX,HINV,STMARK,DTMARK
      REAL*8       DUMR4(MXTIM),DUMR42(2,MXTMV),TFAC,TMAX
C
C     + + + EXTERNALS + + +
      EXTERNAL  SUBIN,ECHORD,ERRCHK,READVC,RDPINT,READTM,IRDVC,MCVAD,
     1          CONVER,SUBOUT
C
C     + + + INPUT FORMATS + + +
1000  FORMAT(4X,A1,2I5)
1010  FORMAT(16I5)
1011  FORMAT(3I5,E10.3)
1012  FORMAT(I5,2E10.3)
1020  FORMAT(8E10.3)
1021  FORMAT(3(2E10.3))
1031  FORMAT(3(E10.3,I5))
1032  FORMAT(E10.3,I5)
1040  FORMAT(I5,7E10.3)
1050  FORMAT(2I5,2E10.3,2I5,2E10.3)
1110  FORMAT('Error reading VADOFT input data line [',A7,']')
1111  FORMAT(A4)
C
C     + + + OUTPUT FORMATS + + +
   3  FORMAT(/10X,80('-')//10X,A80,//10X,80('-')/)
  12  FORMAT(//10X,'PROBLEM CONTROL DATA'/10X,20('-'),/
     * 10X,'TOTAL NUMBER OF NODES .......................(NP) =',I5/
     * 10X,'NUMBER OF POROUS MATRIX MATERIALS .........(NMAT) =',I5/
     * 10X,'INITIAL CONDITION NON-UNIFORMITY INDEX.....(NONU) =',I5/
     * 10X,'SIMULATION INDEX (1=TRANS, 0=STEADY) ....(ITRANS) =',I5/
     * 10X,'MODEL TYPE INDEX (1=FLOW,0=TRANSPORT).....(IMODL) =',I5/
     * 10X,'TIME STEPPING(1=BACKWARD,0=CENTRAL).......(IKALL) =',I5/
     * 10X,'MASS BALANCE COMPUTATION INDEX ...........(IMBAL) =',I5)
  13   FORMAT(
     * 10X,'CONVERT INITIAL HEAD VALUES(1=YES, 0=NO).(INTSPC) =',I5/
     * 10X,'FLOW DIRECTIONAL INDICATOR'/
     * 10X,'   (0=VERTICAL, 1=HORIZONTAL)............(IHORIZ) =',I5)
  14  FORMAT(//10X,'ITERATION CONTROL DATA'/10X,22('-'),/
     * 10X,'MAXIMUM NON-LINEAR ITERATIONS ...........(NITMAX) =',I5/
     * 10X,'ITERATION TYPE (0=PICARD, 1 or 2=NEWTON)..(INEWT) =',I5/
     * 10X,'MAXIMUM NUMBER OF TIME STEP REFINEMENTS .(IRESOL) =',I5/
     * 10X,'ITERATION TOLERANCE HEAD/CONCENTRATION ....(HTOL) =',E12.4)
  15  FORMAT(//
     * 10X,'DECAY CHAIN SIMULATION INDEX.............(ICHAIN) =',I5/
     * 10X,'FREUNDLICH ISOTHERM INDEX................(IFREUN) =',I5)
  17  FORMAT(//10X,'SPATIAL DISCRETIZATION DATA'/10X,27('-')//
     * 10X,'LAYER NO.',4X,'NO. OF ELEM.',4X,'MATL. NO.',4X,'THICKNESS'/)
  21  FORMAT(8E10.3)
  23  FORMAT(//10X,'INPUT / OUTPUT CONTROL DATA'/10X,27('-')/
     * 10X,'INDEX FOR RELATIVE PERMEABILITY/SATURATION  & ',/
     * 10X,'PRESSURE HEAD/SATURATION (1=TAB, 0=FUNC)..(KPROP) =',I5/
     * 10X,'TIME STEP GENERATION INDEX (1=YES, 0=NO) .(ITSGN) =',I5/
     * 10X,'BACKUP FILE OUTPUT TIME MARKER INDEX.....(ITMARK) =',I5/
     * 10X,'NODAL VALUE PRINTOUT CONTROL INDEX........(NSTEP) =',I5/
     * 10X,'VELOCITY PRINTOUT CONTROL INDEX............(NVPR) =',I5/
     * 10X,'OBSERVATION NODE INDEX...................(IOBSND) =',I5/
     * 10X,'PRINT CHECK CONTROL INDEX................(IPRCHK) =',I5)
  24  FORMAT(//10X,'LIST OF ORIGINAL TIME VALUES'/10X,28('-')/)
  29  FORMAT(10X,I5,9X,I6,10X,I5,8X,E10.3)
  33  FORMAT(//10X,'TIME STEPPING DATA'/10X,18('-'),/
     * 10X,'INITIAL TIME VALUE ........................(TIMA) =',E12.4)
  34  FORMAT(
     * 10X,'VALUE OF FIRST TIME STEP ...................(TIN) =',E12.4/
     * 10X,'TIME STEP MULTIPLIER ......................(TFAC) =',E12.4/
     * 10X,'MAXIMUM TIME STEP SIZE ....................(TMAX) =',E12.4)
  48  FORMAT(//10X,'OUTPUT MARKER TIME DATA'/10X,23('-')//10X,
     * 'LIST OF BACKUP FILE OUTPUT MARKER TIME VALUES:'/)
  49  FORMAT(/10X,'MATERIAL NUMBER',I5/10X,20('-'))
  53  FORMAT(8E10.4)
  63  FORMAT(///10X,'*** NODE NUMBERS AND COORDINATES ***'
     */10X,36('-')/)
  73  FORMAT(5(I6,E12.3))
  83  FORMAT(//10X,'BOUNDARY CONDITION DATA'/10X,23('-')/
     * 10X,'BOUNDARY CONDITION OF FIRST NODE'/
     * 10X,' (1=HEAD OR CONCENTRATION, 0=FLUX).......(IBTND1) =',I10/
     * 10X,'BOUNDARY CONDITION OF LAST NODE'/
     * 10X,' (1=HEAD OR CONCENTRATION, 0=FLUX).......(IBTNDN) =',I10/
     * 10X,'PRESCRIBED VALUE AT THE FIRST NODE.......(VALND1) =',E10.3/
     * 10X,'PRESCRIBED VALUE AT THE LAST NODE........(VALNDN) =',E10.3/
     * 10X,'TRANSIENT B.C. INDEX OF FIRST NODE.......(ITCND1) =',I5/
     * 10X,'TRANSIENT B.C. INDEX OF LAST NODE........(ITCNDN) =',I5)
 87   FORMAT(//10X,'INITIAL CONDITION DATA'/10X,22('-')/
     * 10X,'DEFAULT INITIAL VALUE......................(HINV) =',E10.3/
     * 10X,'NUMBER OF NONDEFAULT NODES.................(NPIN) =',I5)
 88   FORMAT(//10X,'INITIAL CONDITION DATA, CHEMICAL',I5/10X,22('-')/
     * 10X,'DEFAULT INITIAL VALUE......................(HINV) =',E10.3/
     * 10X,'NUMBER OF NONDEFAULT NODES.................(NPIN) =',I5)
 89   FORMAT(
     * 10X,'FLUID FLUX INJECTED INTO NODE 1............(FLX1) =',E10.3/
     * 10X,'FLUID FLUX INJECTED INTO NODE NP...........(FLXN) =',E10.3)
 93   FORMAT(/10X,'MATERIAL NUMBERS FOR ALL NODES'/10X,30('-')/)
 103  FORMAT(5(5X,I5,5X,I5,5X))
 113  FORMAT(//,10X,'SOIL PROPERTY DATA'/10X,18('-')/)
 123  FORMAT(10X,'SATURATED HYDRAULIC CONDUCTIVITY',
     * '................  =',E10.3/
     * 10X,'EFFECTIVE POROSITY .............................  =',E10.3/
     * 10X,'SPECIFIC STORAGE ...............................  =',E10.3/
     * 10X,'AIR ENTRY PRESSURE HEAD VALUE ..................  =',E10.3)
 127  FORMAT(
     * 10X,'LONGITUDINAL DISPERSIVITY.........................=',E10.3/
     * 10X,'EFFECTIVE POROSITY................................=',E10.3)
 6127 FORMAT(10X,'...FOR CHEMICAL',I5/
     * 10X,'RETARDATION COEFFICIENT...........................=',E10.3/
     * 10X,'MOLECULAR DIFFUSION COEFFICIENT...................=',E10.3)
 7127 FORMAT(10X,'...FOR CHEMICAL',I5/
     * 10X,'BULK DENSITY * FREUNDLICH COEFFICIENT.............=',E10.3/
     * 10X,'MOLECULAR DIFFUSION COEFFICIENT...................=',E10.3/
     * 10X,'EXPONENT OF FREUNDLICH ISOTHERM...................=',E10.3)
 133  FORMAT(10X,'RESIDUAL WATER SATURATION ................',
     * '.(SWR)  =',E10.3/
     * 10X,'POWER INDEX .................................(N)  =',E10.3/
     * 10X,'LEADING COEFFICIENT......................(ALPHA)  =',E10.3/
     * 10X,'POWER INDEX ..............................(BETA)  =',E10.3/
     * 10X,'POWER INDEX .............................(GAMMA)  =',E10.3)
 143  FORMAT(//,10X,'SATURATION VS PERMEABILITY FOR MATERIAL: ',I5/
     * 10X,46('-')/)
 146  FORMAT(//,10X,'SATURATION VS PRESSURE HEAD FOR MATERIAL: ',I5/
     * 10X,47('-')/)
 149  FORMAT(//10X,'TRANSPORT PARAMETERS FOR MATERIAL',I5/10X,33('-'),/
     * 10X,'DEFAULT VALUE OF DARCY VELOCITY............(VDF)  =',E12.4/
     * 10X,'DEFAULT VALUE OF WATER SATURATION.........(SWDF)  =',E12.4)
 6149 FORMAT(10X,'...FOR CHEMICAL',I5/
     * 10X,'SOLUTE MASS DECAY CONSTANT..............(DLAMDA)  =',E12.4/
     * 10X,'TRANSFORMATION MASS FRACTION..................... =',E12.4)
 153  FORMAT(10X,E10.3,E10.3,4X,E10.3,E10.3,4X,E10.3,E10.3)
 159  FORMAT(//10X,'VELOCITY INPUT CONTROL'/10X,22('-'),/
     * 10X,'INPUT OF UNIT 10 VELOCITY FILE(1=YES,0=NO)...',/
     * 10X,'........................................(NVREAD)  =',I5/
     * 10X,'INDICATOR OF STEADY STATE VELOCITY(1=YES,0=NO).',/
     * 10X,'.......................................(IVSTED)  =',I5)
 162  FORMAT(/10X,' NODE NUMBER AND INITIAL HEAD VALUES'/10X,36('-')/)
 172  FORMAT(//10X,'NODE NO.,STEADY-STATE VELOCITY AND SATURATION',/10X,
     *46('-')/)
 179  FORMAT(4(I5,2X,E10.3,2X,E10.3))
 209  FORMAT(//10X,'TRANSIENT BOUNDARY CONDITION DATA FOR NODE 1',/)
 217  FORMAT(4(4X,'TIME VALUE',4X,'SOLUTE FLUX')/)
 219  FORMAT(/4(4X,'TIME VALUE',4X,'FLUID FLUX ')/)
 229  FORMAT(4(4X,E10.3,4X,E10.3))
 262  FORMAT(/10X,' NODE NUMBER AND INITIAL CONCENTRATION VALUES',/10X
     1,/10X,45('-')/)
 309  FORMAT(//10X,'TRANSIENT BOUNDARY CONDITION DATA FOR NODE NP',
     1           /)
 329  FORMAT(4(4X,E10.3,4X,E10.3))
 3020 FORMAT('Requested value of NOBSND [',I3,'] greater than MXPRT [',
     1  I3,']')
 3030 FORMAT('NTOMT < NLDLT in linked mode, NTOMT reset to',I4)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'VADINP'
      CALL SUBIN(MESAGE)
C
      DO 30 II = 1,MXMAT
        DO 31 JJ = 1,5
          DO 32 KKK = 1,3
            CPROP(II,JJ,KKK) = 0.0
32        CONTINUE
31      CONTINUE
30    CONTINUE
C
C     record 1.0
      LINCOD = ' 1.0'
      CALL ECHORD(
     I            INPFL, LINCOD, FRSTRD,
     O            TITLE)
      IF (FRSTRD) WRITE(OUTFL,3) TITLE
C
C     record 2.0
      LINCOD = ' 2.0'
      CALL ECHORD(
     I            INPFL, LINCOD, FRSTRD,
     O            LINE)
      READ(LINE,1010,ERR=2000,IOSTAT=IERROR) NP,NMAT,NONU,
     1     ITRANS,IMODL,IKALL,IMBAL,INTSPC,IHORIZ,ICHAIN,IFREUN
C
C     ensure model compatibility
      IF (PRZMON) THEN
        IF (ITRANS .NE. 1) THEN
          IERROR = 3020
          MESAGE =
     1    'Attempt to run VADOFT with PRZM on and ITRANS .NE. 1'
          FATAL  = .TRUE.
          CALL ERRCHK(IERROR,MESAGE,FATAL)
        ENDIF
      ENDIF
C
      IERROR = 0
      IF (FLOSIM) THEN
        IF (IMODL .NE. 1) THEN
          IERROR = 3030
        ENDIF
      ELSE
        IF (IMODL .NE. 0) THEN
          IERROR = 3040
        ENDIF
      ENDIF
      IF (IERROR .NE. 0) THEN
        MESAGE = 'Incorrect value for IMODL in VADOFT input'
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      ENDIF
C
C     number of time steps determined by model
      NTS=NLDLT
C
      NPV = NP
C
      MARK=1
      IF(IHORIZ.EQ.1) MARK=0
      IF (IMODL.EQ.1.OR.ITRANS.EQ.0) IKALL=1
      THETA=0.5
      IF (IKALL.EQ.1) THETA=1.
      IF (ITRANS.EQ.0) THEN
        NTS = 1
      ENDIF
      IF (FRSTRD)
     1    WRITE(OUTFL,12) NP,NMAT,NONU,ITRANS,IMODL,IKALL,IMBAL
      IF(IMODL.EQ.1) THEN
C
C       set up number of chemicals to read
        NCHMRD = 1
        ICHAIN=0
        IF (FRSTRD) WRITE(OUTFL,13) INTSPC, IHORIZ
      ELSE
C
C       set up number of chemicals to read
        NCHMRD = NCHEM
        IF (FRSTRD) WRITE(OUTFL,15) ICHAIN, IFREUN
      END IF
C
C     record 3.0
      IF ((IMODL.NE.0).OR.(IFREUN.NE.0)) THEN
C
C       iteration control record for flow simulations
        LINCOD = ' 3.0'
        CALL ECHORD(
     I              INPFL, LINCOD, FRSTRD,
     O              LINE)
        READ(LINE,1011,ERR=2000,IOSTAT=IERROR) NITMAX,INEWT,
     1       IRESOL,HTOL
        IF (HTOL.LT.1.E-20) HTOL=0.01
      ENDIF
      IF (IMODL.NE.0) THEN
        IMOD=0
        IF (INEWT.EQ.2) IMOD=1
      ELSE
C
C       defaults for transport simulation
        MARK  = 0
        INTSPC= 0
        IF (IFREUN.LE.0) THEN
          NITMAX= 1
        ELSE
          IF (NITMAX.LE.1) NITMAX=5
        ENDIF
        INEWT = 0
        IRESOL= 0
      END IF
      IF ((IMODL.NE.0).OR.(IFREUN.NE.0)) THEN
        IF (FRSTRD) WRITE(OUTFL,14)  NITMAX,INEWT,IRESOL,HTOL
      ENDIF
C
C     record 4.0
      LINCOD = ' 4.0'
      CALL ECHORD(
     I            INPFL, LINCOD, FRSTRD,
     O            LINE)
      READ(LINE,1010,ERR=2000,IOSTAT=IERROR) KPROP,ITSGN,ITMARK,
     1     NSTEP,NVPR,IOBSND,NOBSND,IPRCHK,NOBSWD
      IF (NOBSND .GT. MXPRT) THEN
        WRITE(MESAGE,3020) NOBSND, MXPRT
        IERROR = 3050
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      ENDIF
C
      IF (FRSTRD)
     1  WRITE(OUTFL,23)  KPROP,ITSGN,ITMARK,NSTEP,NVPR,IOBSND,IPRCHK
C
      IF(IPRCHK.EQ.1) THEN
        NVPR=NTS
        NSTEP=NTS
        IMBAL=1
      END IF
C
      IF (ITRANS.EQ.0) THEN
        TIN=1.0
        TIMA=0.0
        TFAC=1.0
        TMAX=1.0
        TIMAKP=TIMA
        TMVEC(1)=1.0
      ELSE
C
C       record 5.0
        LINCOD = ' 5.0'
        CALL ECHORD(
     I    INPFL, LINCOD, FRSTRD,
     O    LINE)
        READ(LINE,1020,ERR=2000,IOSTAT=IERROR) TIMA,TIN,TFAC,TMAX
        TIMAKP=TIMA
        IF (FRSTRD) WRITE(OUTFL,33)  TIMA
        IF (ITSGN.EQ.1) THEN
          IF (FRSTRD) WRITE(OUTFL,34) TIN,TFAC,TMAX
C
C         generate time values
          TMVEC(1)= TIMA
          DO 18 I = 1,NTS
            TIMA    = TIMA+TIN
            TMVEC(I)= TIMA
            TIN     = TIN*TFAC
            IF (TIN.GT.TMAX) TIN=TMAX
  18      CONTINUE
C
        ELSE
C         read computational time values
C         record 6.0
          LINCOD = ' 6.0'
          MESAGE = '(8F10.3)'
          CALL READVC(
     I                INPFL, MESAGE, LINCOD, FRSTRD,
     I                NTS,
     O                DUMR4, IERROR)
          DO 19 I=1, NTS
            TMVEC(I)= DUMR4(I)
 19       CONTINUE
          IF (IERROR .NE. 0) GO TO 2000
        END IF
C
        IF (FRSTRD) THEN
          WRITE(OUTFL,24)
          WRITE(OUTFL,21) (TMVEC(I),I=1,NTS)
        ENDIF
C
        IF (ITMARK.EQ.1) THEN
C
C         record 7.0
          LINCOD = ' 7.0'
          CALL ECHORD(
     I                INPFL, LINCOD, FRSTRD,
     O                LINE)
          READ(LINE,1012,ERR=2000,IOSTAT=IERROR) ITMGEN,
     1      STMARK,DTMARK
          NTOMT=NLDLT
          IF (ITMGEN.EQ.1) THEN
C
            IF ((PRZMON) .AND. TRNSIM) THEN
              IF (NTOMT .LT. NLDLT) THEN
                IF (FRSTRD) THEN
                  IERROR = 3180
                  WRITE(MESAGE,3030) NLDLT
                  FATAL  = .FALSE.
                  CALL ERRCHK(IERROR,MESAGE,FATAL)
                ENDIF
                NTOMT  = NLDLT
              ENDIF
            ENDIF
            TMFOMT(1)=STMARK+DTMARK
            DO 28 I=2,NTOMT
              TMFOMT(I)=TMFOMT(I-1)+DTMARK
  28        CONTINUE
          ELSE
            LINCOD = ' 8.0'
            MESAGE = '(8E10.3)'
            CALL READVC(
     I                  INPFL, MESAGE, LINCOD, FRSTRD,
     I                  NTOMT,
     O                  DUMR4, IERROR)
            DO 429 I=1, NTOMT
              TMFOMT(I)= DUMR4(I)
 429        CONTINUE
            IF (IERROR .NE. 0) GO TO 2000
          END IF
C
          IF (FRSTRD) THEN
            WRITE(OUTFL,48)
            WRITE(OUTFL,53)(TMFOMT(I),I=1,NTOMT)
          ENDIF
        ELSE
          NTOMT=NTS
          DO 16 I=1,NTOMT
            TMFOMT(I)=TMVEC(I)
  16      CONTINUE
        END IF
      END IF
C
C     read layered grid data
C     record 9.0
      LINCOD = ' 9.0'
      CALL ECHORD(
     I            INPFL, LINCOD, FRSTRD,
     O            LINE)
      READ(LINE,1010,ERR=2000,IOSTAT=IERROR) NLAYRG
      IF (FRSTRD) WRITE(OUTFL,17)
C
C     layers must be numbered sequentially from top to bottom
C     of soil column
      DO 27 I=1,NLAYRG
C       record 10.0
        LINCOD = '10.0'
        CALL ECHORD(
     I              INPFL, LINCOD, FRSTRD,
     O              LINE)
        READ(LINE,1011,ERR=2000,IOSTAT=IERROR) ILAYR,NELM(ILAYR),
     1    IMATL(ILAYR),THL(ILAYR)
        IF (FRSTRD)
     1    WRITE(OUTFL,29) ILAYR,NELM(ILAYR),IMATL(ILAYR),THL(ILAYR)
  27  CONTINUE
C
C     generate material numbers and nodal coordinates
      NDCOUN=1
      CORD(1)=0.0
      DO 37 I=1,NLAYRG
        NEL=NELM(I)
        DX=THL(I)/NEL
        DO 36 IE=1,NEL
          NDM1=NDCOUN
          NDCOUN=NDCOUN+1
          IPROP(NDM1)=IMATL(I)
          CORD(NDCOUN)=CORD(NDM1)+DX
  36    CONTINUE
  37  CONTINUE
C
      IF (FRSTRD) THEN
        WRITE(OUTFL,63)
        WRITE(OUTFL,73)(I,CORD(I),I=1,NP)
C
        IF (NMAT.GT.1) THEN
          WRITE(OUTFL,93)
          WRITE(OUTFL,103) (IPROP(I),I=1,NP)
        END IF
      ENDIF
C
C     record 11.0
      LINCOD = '11.0'
      CALL ECHORD(
     I            INPFL, LINCOD, FRSTRD,
     O            LINE)
      IF (IMODL .EQ. 1) THEN
        READ(LINE,1032,ERR=2000,IOSTAT=IERROR)
     1    CHINV(1),CNPIN(1)
        HINV = CHINV(1)
        NPIN = CNPIN(1)
      ELSE
        READ(LINE,1031,ERR=2000,IOSTAT=IERROR)
     1    (CHINV(ICHEM),CNPIN(ICHEM),ICHEM=1,NCHMRD)
        HINV = CHINV(1)
        NPIN = CNPIN(1)
      ENDIF
C
      IF (FRSTRD) THEN
        IF (IMODL .EQ. 1) THEN
          WRITE(OUTFL,87)  HINV,NPIN
        ELSE
          DO 40 ICHEM = 1, NCHMRD
            WRITE(OUTFL,88) ICHEM, CHINV(ICHEM), CNPIN(ICHEM)
 40       CONTINUE
        ENDIF
      ENDIF
C
C     record 12.0
      LINCOD = '12.0'
      CALL ECHORD(
     I            INPFL, LINCOD, FRSTRD,
     O            LINE)
      READ(LINE,1050,ERR=2000,IOSTAT=IERROR) IBTND1,IBTNDN,
     1  VALND1,VALNDN,ITCND1,ITCNDN,FLX1,FLXN
      IF (FRSTRD) THEN
        WRITE(OUTFL,83)  IBTND1,IBTNDN,VALND1,VALNDN,ITCND1,ITCNDN
        IF (IMODL.EQ.0) WRITE(OUTFL,89)FLX1,FLXN
      ENDIF
      IF (FLX1.LT.0.) FLX1=0.
      IF (FLXN.LT.0.) FLXN=0.
      IF (FRSTRD) WRITE(OUTFL,113)
      DO 55 I = 1,NMAT
C
C       record 13.0
        LINCOD = '13.0'
        CALL ECHORD(
     I              INPFL, LINCOD, FRSTRD,
     O              LINE)
C
        IF (FRSTRD) WRITE(OUTFL,49) I
        IF (IMODL.EQ.1) THEN
          READ(LINE,1020,ERR=2000,IOSTAT=IERROR)(PROP(I,J),J=1,4)
          IF (FRSTRD) WRITE(OUTFL,123) (PROP(I,J),J=1,4)
        ELSE
          READ(LINE,1020,ERR=2000,IOSTAT=IERROR)
     1    (CPROP(I,J,1),J=1,2)
        LINCOD = '14.0'
          CALL ECHORD(
     I                INPFL, LINCOD, FRSTRD,
     O                LINE)
          READ(LINE,1021,ERR=2000,IOSTAT=IERROR)
     1    (CPROP(I,3,ICHEM),ICHEM=1,NCHMRD),
     2    (CPROP(I,4,ICHEM),ICHEM=1,NCHMRD)
          IF (FRSTRD) WRITE(OUTFL,127) (CPROP(I,J,1),J=1,2)
C 
          DO 52 ICHEM = 1, NCHMRD
            IF (FRSTRD) THEN
              IF (IFREUN.EQ.0) THEN
                WRITE(OUTFL,6127) ICHEM, (CPROP(I,J,ICHEM),J=3,4)
              ELSE
                WRITE(OUTFL,7127) ICHEM, (CPROP(I,J,ICHEM),J=3,5)
              ENDIF
            ENDIF
C
C           as a safty precaution, initialize non-chemical varying
C           components to 1st value
            CPROP(I,1,ICHEM) = CPROP(I,1,1)
            CPROP(I,2,ICHEM) = CPROP(I,2,1)
 52       CONTINUE
C
C         following is redundant since VADCHM (called from EXESUP) will
C         do these transfers by chemical
          DO 54 J = 1, 5
            PROP(I,J) = CPROP(I,J,1)
 54       CONTINUE
        END IF
 55   CONTINUE
C
      IF (IMODL.EQ.1) THEN
C       doing a flow simulation
        IF (KPROP.EQ.1) THEN
C         functional parameters supplied
          DO 65 I = 1,NMAT
C
C           record 15.0
            IF (FRSTRD) WRITE(OUTFL,49)I
            LINCOD = '15.0'
            CALL ECHORD(
     I                  INPFL, LINCOD, FRSTRD,
     O                  LINE)
            READ(LINE,1020,ERR=2000,IOSTAT=IERROR) (FVAL(I,J),J=1,5)
            IF (FRSTRD) WRITE(OUTFL,133) (FVAL(I,J),J=1,5)
            CTRFAC(I)=1.0
  65      CONTINUE
        ELSE
C
          DO 85 I=1,NMAT
C           record 16.0
            LINCOD = '16.0'
            CALL ECHORD(
     I                  INPFL, LINCOD, FRSTRD,
     O                  LINE)
            READ(LINE,1010,ERR=2000,IOSTAT=IERROR) NUMK(I)
            NUMKI=NUMK(I)
C
C           record 17.0
            LINCOD = '17.0'
            DO 86 ISTRT = 1, NUMKI, 4
              IEND = ISTRT + 3
              IF (IEND.GT.NUMKI) IEND = NUMKI
              CALL ECHORD(
     I                    INPFL, LINCOD, FRSTRD,
     O                    LINE)
              READ(LINE,1020,ERR=2000,IOSTAT=IERROR)
     1          (SWV(I,J),PKRW(I,J),J=ISTRT,IEND)
              IF (FRSTRD) THEN
                WRITE(OUTFL,143) I
                WRITE(OUTFL,153) (SWV(I,J),PKRW(I,J),J=ISTRT,IEND)
              ENDIF
  86        CONTINUE
C
C           record 18.0
            LINCOD = '18.0'
            CALL ECHORD(
     I                  INPFL, LINCOD, FRSTRD,
     O                  LINE)
            READ(LINE,1010,ERR=2000,IOSTAT=IERROR) NUMP(I)
            NUMPI=NUMP(I)
C
C           record 19.0
            LINCOD = '19.0'
            DO 84 ISTRT = 1, NUMPI, 4
              IEND = ISTRT + 3
              IF (IEND.GT.NUMPI) IEND = NUMPI
              CALL ECHORD(
     I                    INPFL, LINCOD, FRSTRD,
     O                    LINE)
              READ(LINE,1020,ERR=2000,IOSTAT=IERROR)
     1          (SSWV(I,J),HCAP(I,J),J=ISTRT,IEND)
              IF (FRSTRD) THEN
                WRITE(OUTFL,146) I
                WRITE(OUTFL,153) (SSWV(I,J),HCAP(I,J),J=ISTRT,IEND)
C
                ICNTER=0
                DO 7777 JCK=1,NUMPI
                  IF (HCAP(I,JCK).GT.1.0E-10) ICNTER=ICNTER+1
 7777           CONTINUE
                IF (ICNTER .GE. 1) THEN
                  IERROR = 3220
                  MESAGE =
     1              'UNSATURATED PRESSURE HEADS ARE NEGATIVE'
                  FATAL  = .TRUE.
                  CALL ERRCHK(IERROR,MESAGE,FATAL)
                ENDIF
              ENDIF
  84        CONTINUE
  85      CONTINUE
        END IF
      END IF
C
C     set initial conditions
      DO 115 ICHEM = 1, NCHMRD
        HINV = CHINV(ICHEM)
        DO 115 I=1,NP
          IF (ICHEM .EQ. 1) PINT(I)= HINV
          CPINT(I,ICHEM) = HINV
 115  CONTINUE
C
C     record 20.0
C
C     read PINT and CPINT
C
      IF (NONU .EQ. 1) THEN
        LINCOD = '20.0'
        CALL RDPINT(
     I            INPFL, NP, NCHMRD, NONU, FRSTRD)
C
        IF (IERROR .NE. 0) GO TO 2000
C
      ENDIF  
        DO 135 I=1,NP
C
C         setup the old & new head/concentration arrays
          PINT(I) = PINT(I)-INTSPC*(CORD(NP)-CORD(I))
C
C         correct CPINT for EXESUP initialization
          CPINT(I,1) = PINT(I)
          PCUR(I) = PINT(I)
          DIS(I)  = PINT(I)
 135    CONTINUE
      IF (IMODL.EQ.0) THEN
C
C       record 21.0 required for transport simulation
        DO 139 N= 1,NMAT
          LINCOD = '21.0'
          CALL ECHORD(
     I                INPFL, LINCOD, FRSTRD,
     O                LINE)
          READ(LINE,1040,ERR=2000,IOSTAT=IERROR) I,VDFI(I),
     1      SWDFI(I),UWFI(I)
          IF (FRSTRD) WRITE(OUTFL,149) I,VDFI(I),SWDFI(I)
C
          LINCOD = '22.0'
          CALL ECHORD(
     I                INPFL, LINCOD, FRSTRD,
     O                LINE)
          READ(LINE,1040,ERR=2000,IOSTAT=IERROR)
     1      I, (CLAMDI(I,ICHEM),ICHEM=1,NCHMRD),
     2      (CRACMP(I,ICHEM),ICHEM=1,NCHMRD)
          IF (FRSTRD) THEN
            DO 138 ICHEM = 1, NCHMRD
              WRITE(OUTFL,6149) ICHEM,CLAMDI(I,ICHEM),CRACMP(I,ICHEM)
 138        CONTINUE
          ENDIF
C
C         following is redundant since VADCHM (called from EXESUP) will
C         do these transfers by chemical
C
          FRACMP(I) = CRACMP(I,1)
 139    CONTINUE
        UWF   = UWFI(1)
C
C       record 23.0
        LINCOD = '23.0'
        CALL ECHORD(
     I              INPFL, LINCOD, FRSTRD,
     O              LINE)
        READ(LINE,1010,ERR=2000,IOSTAT=IERROR) NVREAD,IVSTED
C
C       in linked mode, must read velocities from scratch file. If
C       stand-alone, user decides as per input instructions (record 22).
        IF (PRZMON) THEN
          IF (TRNSIM .AND. (.NOT. FLOSIM)) THEN
            IF (FRSTRD) THEN
              IF (NVREAD .NE. 1) THEN
                IERROR = 3060
                MESAGE = 'Transport simulation, NVREAD reset to 1'
                FATAL  = .FALSE.
                CALL ERRCHK(IERROR,MESAGE,FATAL)
              ENDIF
            ENDIF
            NVREAD = 1
          ENDIF
        ENDIF
C
        IF (PRZMON) THEN
          IF (FRSTRD) THEN
            IF (IVSTED .NE. 1) THEN
              IERROR = 3070
              MESAGE = 'PRZM is on, IVSTED reset to 1'
              FATAL  = .FALSE.
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ENDIF
          ENDIF
          IVSTED = 1
        ENDIF
C
        IF (FRSTRD) WRITE(OUTFL,159) NVREAD,IVSTED
        NPM1=NP-1
        DO 156 I= 1,NPM1
C
C         initialize darcy velocity & saturation to default values
          IMAT   = IPROP(I)
          VDAR(I)= VDFI(IMAT)
          SWEL(I)= SWDFI(IMAT)
 156    CONTINUE
        VDAR1 = VDAR(1)
        VDARN = VDAR(NPM1)
        IF (NVREAD.EQ.1) THEN
C
C         velocity file being used
          IF (IVSTED.EQ.1) THEN
C
            NPM1=NP-1
            IF (.NOT. PRZMON) THEN
              READ(TAP10,END=202,ERR=203,IOSTAT=IERROR)
     1          VDAR1, VDARN
              READ(TAP10,END=202,ERR=203,IOSTAT=IERROR)
     1          (VDAR(I),I=1,NPM1)
              READ(TAP10,END=202,ERR=203,IOSTAT=IERROR)
     1          (SWEL(I),I=1,NPM1)
            ENDIF
          END IF
        END IF
C
        IF (NVPR.EQ.1.AND.IVSTED.EQ.1) THEN
C
C         velocity output if requested every timestep and if steadystate
          NPM1=NP-1
          IF (FRSTRD .AND. (.NOT. PRZMON)) THEN
            WRITE(OUTFL,172)
            WRITE(OUTFL,179) (I,VDAR(I),SWEL(I),I=1,NPM1)
          ENDIF
        END IF
      END IF
C
      IF ((ITCND1.EQ.1) .AND. (.NOT. PRZMON)) THEN
C       if PRZM is on, values will be supplied by PRZM
C
C       record 24.0
        LINCOD = '24.0'
        CALL ECHORD(
     I              INPFL, LINCOD, FRSTRD,
     O              LINE)
        READ(LINE,1010,ERR=2000,IOSTAT=IERROR) NTSNDH(1)
        NTN1=NTSNDH(1)
        ITSTH(1)=1
C
C       record 25.0
        LINCOD = '25.0'
        MESAGE = '(8E10.3)'
        CALL READTM(
     I              INPFL, MESAGE, LINCOD, FRSTRD,
     I              2, MXTMV,
     I              1, NTN1,
     O              DUMR42, IERROR)
        DO 444 I=1, NTN1
          TMHV(1,I)= DUMR42(1,I)
 444    CONTINUE
        IF (IERROR .NE. 0) GO TO 2000
C
C       record 26.0
        LINCOD = '26.0'
        MESAGE = '(8E10.3)'
        CALL READTM(
     I              INPFL, MESAGE, LINCOD, FRSTRD,
     I              2, MXTMV,
     I              1, NTN1,
     O              DUMR42, IERROR)
        DO 445 I=1, NTN1
          HVTM(1,I)= DUMR42(1,I)
 445    CONTINUE
        IF (IERROR .NE. 0) GO TO 2000
C
        IF (IMODL.EQ.0.AND.IBTND1.EQ.0) THEN
C
C         record 27.0
          LINCOD = '27.0'
          MESAGE = '(8F10.3)'
          CALL READTM(
     I                INPFL, MESAGE, LINCOD, FRSTRD,
     I                2, MXTMV,
     I                1, NTN1,
     O                DUMR42, IERROR)
          DO 446 I=1, NTN1
            QVTM(1,I)= DUMR42(1,I)
 446      CONTINUE
          IF (IERROR .NE. 0) GO TO 2000
C
        END IF
C
        IF (FRSTRD) THEN
          IF (ITCND1.EQ.1) WRITE(OUTFL,209)
          IF (IMODL.EQ.0)  WRITE(OUTFL,217)
          IF (IMODL.EQ.1)  WRITE(OUTFL,219)
          WRITE(OUTFL,229) (TMHV(1,J),HVTM(1,J),J=1,NTN1)
          IF (IMODL.EQ.0 .AND. IBTND1.EQ.0) THEN
C
C           print volumetric water flux for selected time values
C             (see record 25.0)
            WRITE(OUTFL,219)
            WRITE(OUTFL,229) (TMHV(1,J),QVTM(1,J),J=1,NTN1)
          ENDIF
        ENDIF
      END IF
C
      IF ((ITCNDN.EQ.1) .AND. (.NOT. PRZMON)) THEN
C     these records will overwrite boundary conditions for node NP
C                                    
C       record 28.0
        LINCOD = '28.0'
        CALL ECHORD(
     I              INPFL, LINCOD, FRSTRD,
     O              LINE)
        READ(LINE,1010,ERR=2000,IOSTAT=IERROR) NTSNDH(2)
        NTNP    = NTSNDH(2)
        ITSTH(2)= 1
C
C       record 29.0
        LINCOD = '29.0'
        MESAGE = '(8E10.3)'
        CALL READTM(
     I              INPFL, MESAGE, LINCOD, FRSTRD,
     I              2, MXTMV,
     I              2, NTNP,
     O              DUMR42, IERROR)
        DO 447 I=1, NTNP
          TMHV(2,I)= DUMR42(2,I)
 447    CONTINUE
        IF (IERROR .NE. 0) GO TO 2000
C
C       record 30.0
        LINCOD = '30.0'
C
        MESAGE = '(8E10.3)'
        CALL READTM(
     I              INPFL, MESAGE, LINCOD, FRSTRD,
     I              2, MXTMV,
     I              2, NTNP,
     O              DUMR42, IERROR)
        DO 448 I=1, NTNP
          HVTM(2,I)= DUMR42(2,I)
 448    CONTINUE
        IF (IERROR .NE. 0) GO TO 2000
C
        IF (IMODL.EQ.0.AND.IBTNDN.EQ.0) THEN
C
C         record 31.0
          LINCOD = '31.0'
          MESAGE = '(8E10.3)'
          CALL READTM(
     I                INPFL, MESAGE, LINCOD, FRSTRD,
     I                2, MXTMV,
     I                2, NTNP,
     O                DUMR42, IERROR)
          DO 449 I=1, NTNP
            QVTM(2,I)= DUMR42(2,I)
 449      CONTINUE
          IF (IERROR .NE. 0) GO TO 2000
C
        END IF
C
        IF (FRSTRD) THEN
          IF (ITCNDN.EQ.1) WRITE(OUTFL,309)
          IF (IMODL.EQ.0)  WRITE(OUTFL,217)
          IF (IMODL.EQ.1)  WRITE(OUTFL,219)
          WRITE(OUTFL,329) (TMHV(2,J),HVTM(2,J),J=1,NTNP)
          IF (IMODL.EQ.0 .AND. IBTNDN.EQ.0) THEN
            WRITE(OUTFL,219)
            WRITE(OUTFL,229) (TMHV(2,J),QVTM(2,J),J=1,NTNP)
          END IF
        END IF
      END IF
C
      IF (IOBSND.GT.0.AND.NOBSND.GT.0) THEN
C
C       record 32.0
        LINCOD = '32.0'
        MESAGE = '(16I5)'
        CALL IRDVC(
     I             INPFL, MESAGE, LINCOD, FRSTRD,
     I             NOBSND,
     O             NDOBS, IERROR)
        IF (IERROR .NE. 0) GO TO 2000
      END IF
C
C     record 33.0  added for output control of VADOFT
      IF (IMODL .EQ. 1) THEN
        LINCOD = '33.0'
          CALL ECHORD(
     I       INPFL,LINCOD,FRSTRD,
     O       LINE)
            READ(LINE,1111,ERR=2000,IOSTAT=IERROR)
     1           OUTF
      ELSE
          CALL ECHORD(
     I       INPFL,LINCOD,FRSTRD,
     O       LINE)
            READ(LINE,1111,ERR=2000,IOSTAT=IERROR)
     1           OUTT
C
      ENDIF
C
C       record 34.0
      IF (NOBSWD .GT. 0) THEN
         LINCOD = '34.0'
           DO 137 I = 1,NOBSWD
             CALL ECHORD(
     I          INPFL,LINCOD,FRSTRD,
     O          LINE)
           READ(LINE,1000,ERR=2000,IOSTAT=IERROR)
     1          COD(I),NODE(I),VADDSN(I)
 137       CONTINUE
      ENDIF
C
      NE= NP-1
C
      DO 136 I=1,NE
C       setup element values
        IP1  = I+1
        EL(I)= CORD(IP1)-CORD(I)
 136  CONTINUE
C
C     print initial nodal values
      IF (FRSTRD) THEN
        IF (IMODL.EQ.0) WRITE(OUTFL,262)
        IF (IMODL.EQ.1) WRITE(OUTFL,162)
        WRITE(OUTFL,73) (I,PINT(I),I=1,NP)
      ENDIF
C
C     check for PRZM compatibility
      IF (FLOSIM) THEN
C
C       set ITCND. flags if PRZM (ignore user defined values)
        IF (PRZMON) THEN
C
C         head or concentration condition
          IF (FRSTRD) THEN
            IF (IBTND1 .NE. 0) THEN
              IERROR = 3080
              MESAGE =
     1    'PRZM is on, flow boundary conditions will be overwritten'
              FATAL  = .FALSE.
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ENDIF
          ENDIF
C
C         steady B.C. for 1st node
          IF (FRSTRD) THEN
            IF (ITCND1 .NE. 0) THEN
              IERROR = 3090
              MESAGE = 'PRZM is on, transient data at top node ignored'
              FATAL  = .FALSE.
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ENDIF
          ENDIF
          IBTND1 = 0
          ITCND1 = 0
          ITCNDN = 0
          FLX1   = 0.0
          FLXN   = 0.0
          VALND1 = 0.0
        ENDIF
C
      ELSE
C
C       a transport simulation
        IF (PRZMON) THEN
          IF (FRSTRD) THEN
C
C           head or concentration condition
            IF (IBTND1 .NE. 0) THEN
              IERROR = 3120
              MESAGE =
     1   'PRZM is on, transport boundary conditions will be overwritten'
              FATAL  = .FALSE.
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ENDIF
C
C           steady B.C. for 1st node
            IF (ITCND1 .NE. 0) THEN
              IERROR = 3130
              MESAGE = 'PRZM is on, transient data at top node ignored'
              FATAL  = .FALSE.
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ENDIF
          ENDIF
C
          IBTND1 = 0
          ITCND1 = 0
          ITCNDN = 0
          VALND1 = 0.0
          FLX1   = 0.0
          FLXN   = 0.0
        ENDIF
C
C       end of vadoft boundary checks
      ENDIF
C
C     transfer random inputs to VADOFT variables (Monte Carlo)
      IF(MCARLO) THEN
        MOUTFL = .FALSE.
        CALL MCVAD(
     I             FLOSIM,MOUTFL,IVZONE,IVZONE)
      ENDIF
C
C     following initialization code moved to here from main VADOFT
C     calculation code
      TIMA  = TIMAKP
C
      IF ((INEWT .EQ. 2) .AND. (KPROP .EQ. 1)) THEN
        CALL CONVER(
     I              NMAT,IPRCHK,OUTFL,FRSTRD)
      ENDIF
C
C     end of code move
C
C     skip error mesages
      GO TO 2010
 202  CONTINUE
C
C     end-of-file error
      FATAL = .TRUE.
      MESAGE = 'End of file reading VADOFT Darcy velocities'
      IERROR = 3210
      CALL ERRCHK(IERROR,MESAGE,FATAL)
 203  CONTINUE
      FATAL = .TRUE.
      MESAGE = 'Error reading VADOFT Darcy velocity file'
      CALL ERRCHK(IERROR,MESAGE,FATAL)
 2000 CONTINUE
      FATAL = .TRUE.
      WRITE(MESAGE,1110) LINCOD(1:7)
      CALL ERRCHK(IERROR,MESAGE,FATAL)
 2010 CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   VADPUT
     I                   (LVRST,IVZONE,FLOSIM)
C
C     + + + PURPOSE + + +
C     to write vadoft values into an unformatted file
C     for use in the vadoft restart mode
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LVRST,IVZONE
      LOGICAL    FLOSIM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LVRST  - vadoft restart file unit number
C     IVZONE - current segment number
C     FLOSIM - vadoft flow on flag
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXTMV.INC'
      INCLUDE 'PMXTIM.INC'
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXPRT.INC'
      INCLUDE 'PMXNLY.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CSWHDA.INC'
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CVNTR1.INC'
      INCLUDE 'CVNTR2.INC'
      INCLUDE 'CVWRKM.INC'
      INCLUDE 'CWORKN.INC'
      INCLUDE 'CIN2VD.INC'
      INCLUDE 'CADISC.INC'
      INCLUDE 'CTPDEF.INC'
      INCLUDE 'CVVLM.INC'
      INCLUDE 'CBACK.INC'
      INCLUDE 'CPCHK.INC'
      INCLUDE 'CBDISC.INC'
      INCLUDE 'CDAOBS.INC'
      INCLUDE 'CVMISC.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT,PZSCRN
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Writing VADOFT restart data for flow simulation, ',
     1       'zone [',I2,']')
 2010 FORMAT('Writing VADOFT restart data for transport simulation, ',
     1       'zone [',I2,']')
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'VADPUT'
      CALL SUBIN(MESAGE)
C
      IF (FLOSIM) THEN
C       writing restart data for flow
        WRITE(MESAGE,2000)IVZONE
      ELSE
C       writing restart data for transport
        WRITE(MESAGE,2010)IVZONE
      ENDIF
      CALL PZSCRN(2,MESAGE)
C
      WRITE(LVRST) IVZONE
C
C     include file cvntr1.inc
      WRITE(LVRST) IMOD
C
C     include file cvntr2.inc
      WRITE(LVRST) CUWVIF,CUSMIF,CUSMEF,CUWVEF
C
C     include file ccontr.inc
      WRITE(LVRST) NP,NE,ITRANS,NITMAX,NUMKM,NUMKPM,NSTEP,NVPR,
     1             INEWT,MARK,KPROP,IMODL,IMBAL,
     2             NTS,NMAT,ITMARK,NOWRIT,IOBSND,NOBSND,NTOMT,
     3             IRESOL,IFREUN,ITER,NOBSWD,
     4             TIN,TIMA,UWF,DLAMDA,FLX1,FLXN,HTOL,TMACCW,
     5             TMDCYV,TIMAKP,TMACCP,TMDCYS, TMATMW, TMATMP
      WRITE(LVRST) (COD(I),    I=1,MXOWD)
      WRITE(LVRST) (NODE(I),   I=1,MXOWD)
      WRITE(LVRST) (VADDSN(I), I=1,MXOWD)
      WRITE(LVRST) OUTF
      WRITE(LVRST) OUTT
C
C     include file cvmisc.inc
      WRITE(LVRST) IBTND1,IBTNDN,VALND1,VALNDN,NPV,NVREAD
C
C     include file cbsolv.inc
      WRITE(LVRST) (PINT(I), I=1,MXNOD)
      WRITE(LVRST) (PCUR(I), I=1,MXNOD)
      WRITE(LVRST) (DIS(I),  I=1,MXNOD)
C
C     include file cswhda.inc
      WRITE(LVRST) ((SWV(I,J),  I=1,MXMAT),J=1,30)
      WRITE(LVRST) ((PKRW(I,J), I=1,MXMAT),J=1,30)
      WRITE(LVRST) ((SSWV(I,J), I=1,MXMAT),J=1,30)
      WRITE(LVRST) ((HCAP(I,J), I=1,MXMAT),J=1,30)
      WRITE(LVRST) (NUMK(I),    I=1,MXMAT)
      WRITE(LVRST) (NUMP(I),    I=1,MXMAT)
C
C     include file cadisc.inc
      WRITE(LVRST) (CORD(I),   I=1,MXNOD)
      WRITE(LVRST) (TMVEC(I),  I=1,MXTIM)
      WRITE(LVRST) (TMFOMT(I), I=1,MXTIM)
C
C     include file cmdata.inc
      WRITE(LVRST) ((PROP(I,J), I=1,MXMAT),J=1,5)
      WRITE(LVRST) ((FVAL(I,J), I=1,MXMAT),J=1,5)
      WRITE(LVRST) (IPROP(I),   I=1,MXNOD)
C
C     include file cin2vd.inc
      WRITE(LVRST) (CNPIN(I),       I=1,3)
      WRITE(LVRST) (((CPROP(I,J,K), I=1,MXMAT),J=1,5),K=1,3)
      WRITE(LVRST) ((CLAMDI(I,J),   I=1,MXMAT),J=1,3)
      WRITE(LVRST) ((CRACMP(I,J),   I=1,MXMAT),J=1,3)
      WRITE(LVRST) (CHINV(I),       I=1,3)
      WRITE(LVRST) ((CPINT(I,J),    I=1,MXNOD),J=1,3)
C
C     include file cworkn.inc
      WRITE(LVRST) (SWRKP(I),  I=1,MXMAT)
      WRITE(LVRST) (CTRFAC(I), I=1,MXMAT)
C
C     include file cvwrkm.inc
      WRITE(LVRST) (FRACMP(I), I=1,MXMAT)
      WRITE(LVRST) (RCOFP(I),  I=1,MXMAT)
      WRITE(LVRST) ICHAIN
C
C     include file ctpdef.inc
      WRITE(LVRST) (VDFI(I),  I=1,MXMAT)
      WRITE(LVRST) (SWDFI(I), I=1,MXMAT)
      WRITE(LVRST) (UWFI(I),  I=1,MXMAT)
C
C     include file cvvlm.inc
      WRITE(LVRST) (VDAR(I),   I=1,MXNOD)
      WRITE(LVRST) (SWEL(I),   I=1,MXNOD)
      WRITE(LVRST) (EL(I),     I=1,MXNOD)
      WRITE(LVRST) (DLAMND(I), I=1,MXNOD)
      WRITE(LVRST) (VDARPT(I), I=1,MXNOD)
      WRITE(LVRST) (SWELPT(I), I=1,MXNOD)
      WRITE(LVRST) VDAR1,VDARN,ITCND1,ITCNDN
C
C     include file cback.inc
      WRITE(LVRST) (NTSNDH(I),  I=1,2)
      WRITE(LVRST) ((TMHV(I,J), I=1,2),J=1,MXTMV)
      WRITE(LVRST) ((HVTM(I,J), I=1,2),J=1,MXTMV)
      WRITE(LVRST) ((QVTM(I,J), I=1,2),J=1,MXTMV)
      WRITE(LVRST) (ITSTH(I),   I=1,2)
C
C     include file cpchk.inc
      WRITE(LVRST) IPRCHK,THETA,THETM1
C
C     include file cbdisc.inc
      WRITE(LVRST) (THL(I),   I=1,MXNLAY)
      WRITE(LVRST) (NELM(I),  I=1,MXNLAY)
      WRITE(LVRST) (IMATL(I), I=1,MXNLAY)
C
C     include file cdaobs.inc
      WRITE(LVRST) (NDOBS(I),    I=1,MXPRT)
      WRITE(LVRST) ((HDOBS(I,J), I=1,MXTIM),J=1,MXPRT)
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   VADGET
     I                   (LVRST,IPZCHK,FLOSIM)
C
C     + + + PURPOSE + + +
C     to pass vadoft values from an unformatted file
C     for use in vadoft restart
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LVRST,IPZCHK
      LOGICAL    FLOSIM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LVRST   - vadoft restart file unit number
C     IPZCHK  - current segment number
C     FLOSIM  - vadoft flow on flag
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXTMV.INC'
      INCLUDE 'PMXTIM.INC'
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXPRT.INC'
      INCLUDE 'PMXNLY.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CSWHDA.INC'
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CVNTR1.INC'
      INCLUDE 'CVNTR2.INC'
      INCLUDE 'CVWRKM.INC'
      INCLUDE 'CWORKN.INC'
      INCLUDE 'CIN2VD.INC'
      INCLUDE 'CADISC.INC'
      INCLUDE 'CTPDEF.INC'
      INCLUDE 'CVVLM.INC'
      INCLUDE 'CBACK.INC'
      INCLUDE 'CPCHK.INC'
      INCLUDE 'CBDISC.INC'
      INCLUDE 'CDAOBS.INC'
      INCLUDE 'CVMISC.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,IPRZM,IERROR
      CHARACTER*80 MESAGE
      LOGICAL      FATAL
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,ERRCHK,SUBOUT,PZSCRN
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Reading VADOFT restart data for flow simulation, ',
     1       'zone [',I2,']')
 2010 FORMAT('Reading VADOFT restart data for transport simulation, ',
     1       'zone [',I2,']')
 2020 FORMAT('Attempted to read VADOFT zone',I2,
     1       ', restart file indicated zone',I2)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'VADGET'
      CALL SUBIN(MESAGE)
C
      IF (FLOSIM) THEN
C       reading restart data for flow
        WRITE(MESAGE,2000)IPZCHK
      ELSE
C       reading restart data for transport
        WRITE(MESAGE,2010)IPZCHK
      ENDIF
      CALL PZSCRN(1,MESAGE)
C
      READ(LVRST) IPRZM
      IF (IPRZM .NE. IPZCHK) THEN
        IERROR = 2020
        WRITE(MESAGE,2020) IPZCHK,IPRZM
        FATAL = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      ENDIF
C
C     include file cvntr1.inc
      READ(LVRST) IMOD
C
C     include file cvntr2.inc
      READ(LVRST) CUWVIF,CUSMIF,CUSMEF,CUWVEF
C
C     include file ccontr.inc
      READ(LVRST) NP,NE,ITRANS,NITMAX,NUMKM,NUMKPM,NSTEP,NVPR,
     1            INEWT,MARK,KPROP,IMODL,IMBAL,
     2            NTS,NMAT,ITMARK,NOWRIT,IOBSND,NOBSND,NTOMT,
     3            IRESOL,IFREUN,ITER,NOBSWD,
     4            TIN,TIMA,UWF,DLAMDA,FLX1,FLXN,HTOL,TMACCW,
     5            TMDCYV,TIMAKP,TMACCP,TMDCYS, TMATMW, TMATMP
      READ(LVRST) (COD(I),    I=1,MXOWD)
      READ(LVRST) (NODE(I),   I=1,MXOWD)
      READ(LVRST) (VADDSN(I), I=1,MXOWD)
      READ(LVRST) OUTF
      READ(LVRST) OUTT
C
C     include file cvmisc.inc
      READ(LVRST) IBTND1,IBTNDN,VALND1,VALNDN,NPV,NVREAD
C
C     include file cbsolv.inc
      READ(LVRST) (PINT(I), I=1,MXNOD)
      READ(LVRST) (PCUR(I), I=1,MXNOD)
      READ(LVRST) (DIS(I),  I=1,MXNOD)
C
C     include file cswhda.inc
      READ(LVRST) ((SWV(I,J),  I=1,MXMAT),J=1,30)
      READ(LVRST) ((PKRW(I,J), I=1,MXMAT),J=1,30)
      READ(LVRST) ((SSWV(I,J), I=1,MXMAT),J=1,30)
      READ(LVRST) ((HCAP(I,J), I=1,MXMAT),J=1,30)
      READ(LVRST) (NUMK(I),    I=1,MXMAT)
      READ(LVRST) (NUMP(I),    I=1,MXMAT)
C
C     include file cadisc.inc
      READ(LVRST) (CORD(I),   I=1,MXNOD)
      READ(LVRST) (TMVEC(I),  I=1,MXTIM)
      READ(LVRST) (TMFOMT(I), I=1,MXTIM)
C
C     include file cmdata.inc
      READ(LVRST) ((PROP(I,J), I=1,MXMAT),J=1,5)
      READ(LVRST) ((FVAL(I,J), I=1,MXMAT),J=1,5)
      READ(LVRST) (IPROP(I),   I=1,MXNOD)
C
C     include file cin2vd.inc
      READ(LVRST) (CNPIN(I),       I=1,3)
      READ(LVRST) (((CPROP(I,J,K), I=1,MXMAT),J=1,5),K=1,3)
      READ(LVRST) ((CLAMDI(I,J),   I=1,MXMAT),J=1,3)
      READ(LVRST) ((CRACMP(I,J),   I=1,MXMAT),J=1,3)
      READ(LVRST) (CHINV(I),       I=1,3)
      READ(LVRST) ((CPINT(I,J),    I=1,MXNOD),J=1,3)
C
C     include file cworkn.inc
      READ(LVRST) (SWRKP(I),  I=1,MXMAT)
      READ(LVRST) (CTRFAC(I), I=1,MXMAT)
C
C     include file cvwrkm.inc
      READ(LVRST) (FRACMP(I), I=1,MXMAT)
      READ(LVRST) (RCOFP(I),  I=1,MXMAT)
      READ(LVRST) ICHAIN
C
C     include file ctpdef.inc
      READ(LVRST) (VDFI(I),  I=1,MXMAT)
      READ(LVRST) (SWDFI(I), I=1,MXMAT)
      READ(LVRST) (UWFI(I),  I=1,MXMAT)
C
C     include file cvvlm.inc
      READ(LVRST) (VDAR(I),   I=1,MXNOD)
      READ(LVRST) (SWEL(I),   I=1,MXNOD)
      READ(LVRST) (EL(I),     I=1,MXNOD)
      READ(LVRST) (DLAMND(I), I=1,MXNOD)
      READ(LVRST) (VDARPT(I), I=1,MXNOD)
      READ(LVRST) (SWELPT(I), I=1,MXNOD)
      READ(LVRST) VDAR1,VDARN,ITCND1,ITCNDN
C
C     include file cback.inc
      READ(LVRST) (NTSNDH(I),  I=1,2)
      READ(LVRST) ((TMHV(I,J), I=1,2),J=1,MXTMV)
      READ(LVRST) ((HVTM(I,J), I=1,2),J=1,MXTMV)
      READ(LVRST) ((QVTM(I,J), I=1,2),J=1,MXTMV)
      READ(LVRST) (ITSTH(I),   I=1,2)
C
C     include file cpchk.inc
      READ(LVRST) IPRCHK,THETA,THETM1
C
C     include file cbdisc.inc
      READ(LVRST) (THL(I),   I=1,MXNLAY)
      READ(LVRST) (NELM(I),  I=1,MXNLAY)
      READ(LVRST) (IMATL(I), I=1,MXNLAY)
C
C     include file cdaobs.inc
      READ(LVRST) (NDOBS(I),    I=1,MXPRT)
      READ(LVRST) ((HDOBS(I,J), I=1,MXTIM),J=1,MXPRT)
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   READVC
     I                   (INPFL, FRMAT, LINCOD, FRSTRD,
     I                    NI,
     O                    XVECT, IERROR)
C
C     + + + PURPOSE + + +
C     uses COMRD to read in vectors
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4    INPFL,NI,IERROR
      REAL*8       XVECT(NI)
      CHARACTER*80 FRMAT,LINCOD
      LOGICAL      FRSTRD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INPFL  - ???
C     FRMAT  - ???
C     LINCOD - ???
C     FRSTRD - ???
C     NI     - ???
C     XVECT  - ???
C     IERROR - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    I,ISTRT,IEND,LINENO
      CHARACTER*80 LINE,MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     LSUFIX,ECHORD
C
C     + + + END SPECIFICATIONS + + +
C
      IERROR = 0
      LINENO = 0
C
      DO 11 ISTRT = 1, NI, 8
        CALL LSUFIX(
     I              LINCOD, FRSTRD,
     M              LINENO,
     O              MESAGE)
        IEND = ISTRT + 7
        IF (IEND .GT. NI) IEND = NI
        CALL ECHORD(
     I              INPFL, MESAGE, FRSTRD,
     O              LINE)
        READ(LINE,FRMAT,ERR=2000,IOSTAT=IERROR)
     1    (XVECT(I),I=ISTRT,IEND)
 11   CONTINUE
C
 2000 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   RDPINT
     I                   (INPFL, NP, NCHMRD, NONU, FRSTRD)
C
C     + + + PURPOSE + + +
C     to read in non-default nodes from vadoft input file
C     Modification date: 7/21/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 INPFL,NP,NCHMRD,NONU
      LOGICAL   FRSTRD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INPFL  - input file unit number
C     NP     - number of nodes
C     NCHMRD - number of chemicals read
C     NONU   - number of non-default nodes
C     FRSTRD - restart file 
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXMAT.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CIN2VD.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    L,I,ISTRT,IEND,N(MXNOD),NPIN,ICHEM
      CHARACTER*80 LINE, LINCOD
C
C     + + + EXTERNALS + + +
      EXTERNAL     ECHORD
C
C     + + + OUTPUT FORMATS + + +
 1060 FORMAT(5(I5,E10.3))
 6100 FORMAT(' 20.',I1)
C
C     + + + END SPECIFICATIONS + + +
C
C     loop through chemicals
      DO 44 ICHEM = 1, NCHMRD
        WRITE(LINCOD,6100) ICHEM
        NPIN = CNPIN(ICHEM)
        IF ((NONU .NE. 0) .OR. (NPIN .GT. 0)) THEN
C         Loop through number of non-standard nodes
          DO 33 ISTRT = 1, NPIN, 5
            IEND = ISTRT + 4
            IF (IEND .GT. NPIN) IEND = NPIN
C           Get unit to read
            CALL ECHORD(
     I                  INPFL, LINCOD, FRSTRD,
     O                  LINE)
C           read 5 non-standard nodes at a time
            READ (LINE,1060) (N(I),CPINT(N(I),ICHEM),I=ISTRT,IEND)
 33       CONTINUE
        ENDIF
        IF (ICHEM .EQ. 1) THEN
          DO 22 L = 1,NP
            PINT(L) = CPINT(L,1)
 22       CONTINUE
        ENDIF
 44   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   READTM
     I                   (INPFL, FRMAT, LINCOD, FRSTRD,
     I                    MXI, MXJ,
     I                    I, NJ,
     O                    XTM, IERROR)
C
C     + + + PURPOSE + + +
C     reads in HVTM, TMHV, QVTM
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4    INPFL,MXI,MXJ,I,NJ,IERROR
      CHARACTER*80 FRMAT,LINCOD
      REAL*8       XTM(MXI,MXJ)
      LOGICAL      FRSTRD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INPFL  - ???
C     FRMAT  - ???
C     LINCOD - ???
C     FRSTRD - ???
C     MXI    - ???
C     MXJ    - ???
C     I      - ???
C     NJ     - ???
C     XTM    - ???
C     IERROR - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    J,JSTRT,JEND,LINENO
      CHARACTER*80 LINE,MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     LSUFIX,ECHORD
C
C     + + + END SPECIFICATIONS + + +
C
      IERROR = 0
      LINENO = 0
      MESAGE = ' '
C
      DO 11 JSTRT = 1, NJ, 8
        CALL LSUFIX(
     I              LINCOD, FRSTRD,
     M              LINENO,
     O              MESAGE)
        JEND = JSTRT + 7
        IF (JEND .GT. NJ) JEND = NJ
        CALL ECHORD(
     I              INPFL, MESAGE, FRSTRD,
     O              LINE)
        READ(LINE,FRMAT,ERR=2000,IOSTAT=IERROR)
     1    (XTM(I,J),J=JSTRT,JEND)
 11   CONTINUE
C
 2000 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   LSUFIX
     I                   (LINCOD, FRSTRD,
     M                    LINENO,
     O                    MESAGE)
C
C     + + + PURPOSE + + +
C     performs internal read
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 LINCOD, MESAGE
      LOGICAL      FRSTRD
      INTEGER*4    LINENO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LINCOD - ???
C     MESAGE - ???
C     FRSTRD - ???
C     LINENO - ???
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CECHOT.INC'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 TMP
      INTEGER*4    LN1, LN2
C
C     + + + FUNCTIONS + + +
      INTEGER*4    LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LFTJUS,LNGSTR
C
C     + + + OUTPUT FORMATS + + +
 6000 FORMAT(I10)
C
C     + + + END SPECIFICATIONS + + +
C
      IF ((ECHOLV .GE. 8) .AND. FRSTRD) THEN
        LINENO = LINENO + 1
        WRITE(TMP,6000) LINENO
        CALL LFTJUS(TMP)
        LN1 = LNGSTR(LINCOD)
        LN2 = LNGSTR(TMP)
        MESAGE = LINCOD(1:LN1) // '.' // TMP(1:LN2)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IRDVC
     I                  (INPFL, FRMAT, LINCOD, FRSTRD,
     I                   NI,
     O                   IVECT, IERROR)
C
C     + + + PURPOSE + + +
C     reads in integer vectors - 16 per line
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4    NI
      INTEGER*4    IVECT(NI),INPFL,IERROR
      CHARACTER*80 FRMAT,LINCOD
      LOGICAL      FRSTRD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INPFL  - ???
C     FRMAT  - ???
C     LINCOD - ???
C     FRSTRD - ???
C     NI     - ???
C     IVECT  - ???
C     IERROR - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    I,ISTRT,IEND,LINENO
      CHARACTER*80 LINE,MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     LSUFIX,ECHORD
C
C     + + + END SPECIFICATIONS + + +
C
      IERROR = 0
      LINENO = 0
C
      DO 11 ISTRT = 1, NI, 16
        CALL LSUFIX(
     I              LINCOD, FRSTRD,
     M              LINENO,
     O              MESAGE)
        IEND = ISTRT + 15
        IF (IEND .GT. NI) IEND = NI
        CALL ECHORD(
     I              INPFL, MESAGE, FRSTRD,
     O              LINE)
        READ(LINE,FRMAT,ERR=2000,IOSTAT=IERROR)
     1    (IVECT(I),I=ISTRT,IEND)
 11   CONTINUE
C
 2000 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   MCVAD(
     I                   FLOSIM,OUT,IZ,NMCDAY)
C
C     + + + PURPOSE + + +
C     transfers VADOFT variables between Monte Carlo arrays
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  IZ,NMCDAY
      LOGICAL  OUT,FLOSIM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IZ     - ???
C     NMCDAY - ???
C     OUT    - ???
C     FLOSIM - ???
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXMAT.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMCRVR.INC'
      INCLUDE 'CMCSTR.INC'
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CIN2VD.INC'
      INCLUDE 'CWORKN.INC'
      INCLUDE 'CVADST.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    I,II,ICHM
      REAL*8       YMC
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      INTEGER*4    FNDCHM
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,MAXAVG,SUBOUT,FNDCHM
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'MCVAD'
      CALL SUBIN(MESAGE)
C
C     transfer random inputs to VADOFT variables
      IF(.NOT. OUT)THEN
        DO 20 I=1,MCVAR
          IF (INDZ(I,1).EQ.IZ) THEN
            II = IND1(I,1)
            ICHM = FNDCHM(PNAME(I))
C
C           flow variables
            IF(FLOSIM)THEN
C
C             hydraulic conductivity
              IF (PNAME(I) .EQ. 'HYDRAULIC CONDUC')THEN
                PROP(II,1) = RMC(I)
C
C             residual saturation
              ELSEIF (PNAME(I) .EQ. 'RESIDUAL SATURATION')THEN
                FVAL(II,1) = RMC(I)
C
C             Van-Genuchten alpha
              ELSEIF (PNAME(I) .EQ. 'V-G ALPHA')THEN
                FVAL(II,3) = RMC(I)
C
C             Van-Genuchten N
              ELSEIF (PNAME(I) .EQ. 'V-G POWER N')THEN
                FVAL(II,1) = RMC(I)
              ELSE
              END IF
C
C           transport variables
            ELSE
C
C             decay rate chemical 1
              IF (PNAME(I)(1:12) .EQ. 'VADOFT DECAY')THEN
                CLAMDI(II,ICHM) = RMC(I)
C
C               dispersion coefficient, chemical 1
                ELSEIF (PNAME(I)(1:9) .EQ. 'VAD DISPC')THEN
                  CPROP(II,1,ICHM) = RMC(I)
C
C               retardation, chemical 1
                ELSEIF (PNAME(I)(1:10) .EQ. 'VAD RETARD')THEN
                  CPROP(II,3,ICHM) = RMC(I)
                ELSE
              END IF
C
            END IF
          END IF
   20   CONTINUE
C
C
C     transfer VADOFT outputs to Monte Carlo arrays
      ELSE
        DO 30 I=1,NVAR
          IF(INDZ(I,2).EQ.IZ)THEN
            II = IND1(I,2)
            ICHM = FNDCHM(SNAME(I,1))
C
C           flow variables
            IF(FLOSIM)THEN
C
C             hydraulic conductivity
              IF (SNAME(I,1).EQ.'HYDRAULIC CONDUC')THEN
                YMC = PROP(II,1)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             residual saturation
              ELSEIF (SNAME(I,1).EQ.'RESIDUAL SATURATION')THEN
                YMC = SWRKP(II)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             Van-Genuchten Alpha
              ELSEIF (SNAME(I,1).EQ.'V-G ALPHA')THEN
                YMC = FVAL(II,3)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             Van-Genuchten N
              ELSEIF (SNAME(I,1).EQ.'V-G POWER N')THEN
                YMC = FVAL(II,1)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             total water flux
              ELSEIF (SNAME(I,1).EQ.'VAD WATER FLUX')THEN
                YMC = WFLXMC
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
              ELSE
              END IF
C
C           transport variables
            ELSE
C             decay rate chemical 1
              IF (SNAME(I,1)(1:12) .EQ. 'VADOFT DECAY')THEN
                YMC = CLAMDI(II,ICHM)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             dispersion coefficient, chemical 1
              ELSE IF (SNAME(I,1)(1:9) .EQ. 'VAD DISPC')THEN
                YMC = CPROP(II,1,ICHM)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             retardation coefficient, chemical 1
              ELSE IF (SNAME(I,1)(1:10) .EQ. 'VAD RETARD')THEN
                YMC = CPROP(II,3,ICHM)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             advective flux, chemical 1
              ELSE IF (SNAME(I,1)(1:13) .EQ. 'VAD ADVECTION')THEN
                YMC = ADVMC(ICHM,IZ)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             dispersion flux, chemical 1
              ELSE IF (SNAME(I,1)(1:14) .EQ. 'VAD DISPERSION')THEN
                YMC = DISMC(ICHM,IZ)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             decay flux, chemical 1
              ELSE IF (SNAME(I,1)(1:14) .EQ. 'VAD DECAY FLUX')THEN
                YMC = DKMC(ICHM,IZ)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
C
C             concentration, chemical 1
              ELSE IF (SNAME(I,1)(1:8) .EQ. 'VAD CONC')THEN
                YMC = SAVCNC(IZ,II,ICHM)
                CALL MAXAVG(
     I                      NMAX,NPMAX,I,NMCDAY,NAVG(I),YMC,
     O                      STOR,XMC(I))
              ELSE
              END IF
C
            END IF
          END IF
   30   CONTINUE
C
      END IF
C
      CALL SUBOUT
      RETURN
      END
C
C
C
      SUBROUTINE   CONVER
     I                   (NMAT,IPRCHK,OUTFL,
     I                    FRSTRD)
C
C     + + + PURPOSE + + +
C     computes limiting value of water saturation for
C     each material
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 OUTFL,NMAT,IPRCHK
      LOGICAL   FRSTRD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTFL  - ???
C     NMAT   - number of soil materials
C     IPRCHK - output printcheck control parameter
C     FRSTRD - ???
C
C     + + + PARAMETERS + +
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXNOD.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CWORKN.INC'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 MESAGE
      INTEGER*4    IMAT,III
      REAL*8       PKRMIN,SEMIN,EN,GAMMA,DSETOL,TERM,RESID,SEOLD,
     1             TOLD,DFDS,SEINCR,GAMINV
C
C     + + + INTRINSICS + + +
      INTRINSIC    DSQRT,DABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT

C     + + + OUTPUT FORMATS + + +
 3    FORMAT(//10X,'MATL #',4X,'ORIG. RES. SAT.',4X,
     *'MOD. RES. SAT.',4X,'CAPACITY FACTOR',4X,'TOLERANCE'/)
 13   FORMAT(10X,I4,6X,E14.4,4X,E14.4,5X,E14.4,5X,E12.4)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'CONVER'
      CALL SUBIN(MESAGE)
C
      IF((IPRCHK.EQ.1) .AND. FRSTRD) WRITE(OUTFL,3)
      PKRMIN=1.E-4
      DO 10 IMAT=1,NMAT
C        SEMIN=0.90  ???
        SEMIN=0.99
        CTRFAC(IMAT)=1.0
        EN=FVAL(IMAT,2)
        IF(EN.GT.1.E-10) GO TO 10
        GAMMA=FVAL(IMAT,5)
        GAMINV=1./GAMMA
        SWRKP(IMAT)=FVAL(IMAT,1)
        FVAL(IMAT,2)=1.0
C
C       determine (using Newton-Raphson procedure) saturation
C       corresponding to PKRMIN
        DSETOL=1.E-4
        DO 20 III=1,10
          TERM=DSQRT(SEMIN)*(1.-(1.-SEMIN**GAMINV)**GAMMA)**2
          RESID=PKRMIN-TERM
          SEOLD=SEMIN-DSETOL
          IF(SEOLD.LT.DSETOL) THEN
            DSETOL=-DSETOL
            SEOLD=SEMIN-DSETOL
          END IF
          TOLD=DSQRT(SEOLD)*(1.-(1.-SEOLD**GAMINV)**GAMMA)**2
          DFDS=-(TERM-TOLD)/DSETOL
          SEINCR=-RESID/DFDS
          SEMIN=SEMIN+SEINCR
          IF(DABS(SEINCR).LE.0.01) GO TO 25
 20     CONTINUE
 25     CONTINUE
        FVAL(IMAT,1)=SEMIN*(1.-SWRKP(IMAT))+SWRKP(IMAT)
        CTRFAC(IMAT)=(1.-SWRKP(IMAT))/(1.-FVAL(IMAT,1))
C
        IF ((IPRCHK.EQ.1) .AND. FRSTRD)
     *    WRITE(OUTFL,13) IMAT,SWRKP(IMAT),FVAL(IMAT,1),CTRFAC(IMAT),
     1    SEINCR
 10   CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END

