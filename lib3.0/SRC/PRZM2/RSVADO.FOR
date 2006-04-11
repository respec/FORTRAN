C
C
C
      SUBROUTINE VADOFT(
     I  FLOSIM, TRNSIM, PRZMON, TOPFLX, TOWFLX,
     I  OUTFL, TAP10, PINTMP, NLDLT, IPZONE, NVREAD,
     I  IBTND1, IBTNDN, VALND1, VALNDN,ICHEM,
     I  PARENT, SIMTYM,
     O  SAVTMP)
C
C     +  +  + PURPOSE +  +  +
C
C     This subroutine saves information between flow and transport
C     simualtions.  VADOFT is called by XFLOW and XTRANS.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXVDT.INC'
      LOGICAL      FLOSIM, TRNSIM, PRZMON
      INTEGER*4    OUTFL,TAP10,NLDLT,IPZONE,NVREAD,
     1             ICHEM,PARENT,IBTND1,IBTNDN
      CHARACTER*80 SIMTYM
      REAL         TOPFLX(MXVDT), TOWFLX(MXVDT)
      REAL*8       VALND1,VALNDN,XTMP
      REAL         PINTMP(MXNOD),
     2             SAVTMP(MXNOD)
C
C     +  +  + ARGUMENT DEFINITIONS +  +  + 
C      
C     FLOSIM - logical for flow being ON or OFF
C     TRNSIM - logical for transport being ON or OFF
C     PRZMON - logical for PRZM being ON or OFF
C     OUTFL  - unit number for output file
C     TAP10  - unit number for tape 10 file
C     NLDLT  - maximum number of days in a time step (31)
C     IPZONE - zone number
C     NVREAD - flag for reading velocities from scratch file
C     ICHEM  - current chemical number
C     PARENT - current parent of daughter number
C     IBTND1 - flag for top node boundary condition
C     IBTNDN - flag for bottom node boundary condition
C     SIMTYM - current date of simulation
C     TOPFLX - weighted water or pesticide flux leaving PRZM
C     TOWFLX - water flux leaving PRZM
C     VALNDN - value of flux, conc, or head at bottom vadoft node
C     VALND1 - value of flux, conc, or head at top vadoft node
C     SAVTMP - array for saving fluxes out the bottom of vadoft 
C     PINTMP - temporary array for initial conditions
      
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PMXTMV.INC'
      INCLUDE 'PMXOWD.INC'
      INCLUDE 'PMXTIM.INC'
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXPRT.INC'
      INCLUDE 'PMXNLY.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CECHOT.INC'
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER*4    I,INODE
      CHARACTER*80 MESAGE
C
C     +  +  + EXTERNALS +  +  + 
C
      EXTERNAL VADCAL,SUBIN,SUBOUT
C
C     +  +  + OUTPUT FORMATS +  +  +
C
 2000 FORMAT(1X,79('*'))
 2010 FORMAT(' Flow simulation from ',A22)
 2020 FORMAT(' Transport simulation from ',A22)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'VADOFT'
      CALL SUBIN(MESAGE)
C
C     Initialize
C
      NTS = NLDLT
      NSTEP = NLDLT
      IF (ECHOLV .GE. 4) THEN
        WRITE(OUTFL,2000)
        IF (FLOSIM) THEN
          WRITE(OUTFL,2010) SIMTYM
        ELSE
          WRITE(OUTFL,2020) SIMTYM
        ENDIF
        WRITE(OUTFL,2000)
      ENDIF
C
        DO 199 INODE = 1, NP
          XTMP        = PINTMP(INODE)
          PINT(INODE) = XTMP
          DIS(INODE)  = XTMP
 199    CONTINUE
C
C     Call calculation routine
C
      CALL VADCAL(
     I  TRNSIM, FLOSIM, PRZMON, TOPFLX, TOWFLX, 
     I  OUTFL,TAP10,IBTND1,IBTNDN,NVREAD,SIMTYM,
     I  VALND1,VALNDN, IPZONE, PARENT, ICHEM)
C
      DO 999 I = 1, NP
        SAVTMP(I) = DIS(I)
 999  CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE VADCAL(
     I  TRNSIM, FLOSIM, PRZMON, TOPFLX, TOWFLX, 
     I  OUTFL,TAP10,IBTND1,IBTNDN,NVREAD,SIMTYM,
     I  VALND1,VALNDN,
     I  IPZONE, PARENT, ICHEM)
C
C     + + + PURPOSE + + +
C
C     Calls relevant subroutines to compute nodal head and concentration
C     values ath the end of the current time step
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXVDT.INC'
      LOGICAL   TRNSIM, FLOSIM, PRZMON
      INTEGER*4 OUTFL,TAP10,
     1          IBTND1,IBTNDN,NVREAD,ICHEM,IPZONE,PARENT
      REAL*8    VALND1,VALNDN
      REAL      TOPFLX(MXVDT),TOWFLX(MXVDT)
      CHARACTER*80 SIMTYM
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXTMV.INC'
      INCLUDE 'PMXTIM.INC'
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXPRT.INC'
      INCLUDE 'PMXZON.INC'
C      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CWELEM.INC'
      INCLUDE 'CTRNBC.INC'
      INCLUDE 'CADISC.INC'
      INCLUDE 'CFILEX.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CVVLM.INC'
      INCLUDE 'CBACK.INC'
      INCLUDE 'CPCHK.INC'
      INCLUDE 'CDAOBS.INC'
      INCLUDE 'CVNTR1.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CECHOT.INC'
C
C     + + + LOCAL VARIABLES + + +
      REAL*8       BALSTO(8),FLSTO(2),FLXO(2),VDAR1O,VDARNO,THETKP,
     1             TMCUR,TMVECX,TDIFF,DTEPS
      CHARACTER*80 MESAGE
      INTEGER*4    IERROR,DATES(6),DLT,DTOVWR,QUALFG,TUNITS,NPM1,ITMFC,
     1             IREPMX,I,NUMT,INOCTS,IREP,ICONVG,NDNO,ITIME
      LOGICAL      FATAL
      REAL         RTEMP(MXNOD),VRVAL(MXTIM,MXOWD)
C
C     +  +  + INTRINSICS +  +  + 
C      
      INTRINSIC MOD
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL SUBIN,HFINTP,VARCAL,BALCHK,VSWCOM,WDTPUT,ERRCHK,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
C
 169  FORMAT(//10X,'ELEMENT NUMBER, DARCY VELOCITY AND SATURATION',/
     * 10X,42('-'),/)
 173  FORMAT(///3X,75('=')//5X,'ELAPSED TARGET SIMULATION TIME:',
     *E12.4,/5X,'TARGET TIME STEP NUMBER:',I5,3X,'TARGET TIME STEP:'
     *,E10.3,2X,'****'//3X,75('=')/)
 179  FORMAT(4(I5,2X,E10.3,2X,E10.3))
1243  FORMAT(/5X,'SOLUTION IS NOW COMPLETED FOR TARGET TIME STEP NO.'
     1 ,I4/5X,50('-'),/
     3  5X,'NUMBER OF COMPUTATIONAL TIME STEPS PERFORMED =',I4/5X
     2 ,50('-'))
1273  FORMAT(//10X,'TIME VS CONCENTRATION AT NODE NUMBER',I5/10X,36('-'
     1)/)
1275  FORMAT(//10X,'TIME VS HEAD AT NODE NUMBER',I5/10X,27('-')/)
1283  FORMAT(4(6X,2E12.4))
2020  FORMAT (1X,110(1H*),/,1X,110(1H*),//,50X,'E R R O R',//,
     1        5X,'ERROR WRITING DSN ',I5,' ON WDMSFL, RETCOD:',I4,//,
     2        1X,110(1H*))
2040  FORMAT(///15X,'*** NODAL HEAD VALUES AT TIME =',E12.4,' ***'/
     1 15X,47('-')//5(6X,'NODE',4X,'HEAD VALUE')/5(6X,4('-'),4X,
     2 10('-'))/)
2050  FORMAT(///,15X,'***  CONCENTRATION VALUES  ***'/
     1 15X,30('-')//5(4X,'NODE',4X,'CONCENTRATION')/
     2 5(4X,4('-'),4X,13('-'))/)
2060  FORMAT(5(4X,I4,4X,G13.4))
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'VADCAL'
      CALL SUBIN(MESAGE)
C
C       zero BALSTO array here
        DO 131 I=1,8
          BALSTO(I) = 0.0
131     CONTINUE
        NPM1 = NP - 1
C
        ITMFC=1
        IREPMX=0
C
        TIMA = TIMAKP
        THETKP = THETA
        DO 138 I=1,2
          FLSTO(I)=0.
          FLXO(I)=0.
 138    CONTINUE
        IF (IRESOL.GT.0) IREPMX=2**IRESOL
C
        DO 200 NUMT=1,NTS
C
C         Overwrite 1st node BC if PRZM is on (don't interpolate)
C
          IF (PRZMON) THEN
            VALND1 = TOPFLX(NUMT)
            FLX1 = 0.0
            IF (.NOT. FLOSIM) FLX1 = TOWFLX(NUMT)
          ENDIF
C
C         Time loop
C
          INOCTS=0
          THETA = THETKP
          IF (NUMT.EQ.1) THETA=1.0
          THETM1= THETA-1.0
C
          IF (IMODL.EQ.0.AND.NVREAD.EQ.1) THEN
C
C           Read darcy vel. and water sat. from flow run                     
C           only read for first chemical (values will be the same for
C           all chemicals in the current zone)
C
            READ(TAP10,END=2000,ERR=2010,IOSTAT=IERROR) TMCUR, VDAR1,
     *      VDARN
            READ(TAP10,END=2000,ERR=2010,IOSTAT=IERROR)
     1        (VDAR(I),I=1,NPM1)
            READ(TAP10,END=2000,ERR=2010,IOSTAT=IERROR)
     1        (SWEL(I),I=1,NPM1)
          END IF
C
C         Compute time increment
C
          ITER= NUMT
          TMCUR=TMVEC(NUMT)
          TIN = TMCUR-TIMA
          TMVECX=TIMA
          IF(IMODL.EQ.1) TMVEX1 = TMVEX1 + 1.0
          IF(IMODL.EQ.0.AND.ICHEM.EQ.1) TMVEX2 = TMVEX2 + 1.0
          IREP= 0
          ICONVG=1
          IF(NSTEP.GT.0.OR.IPRCHK.GT.0) THEN
            IF(MOD(ITER,NSTEP).EQ.0) THEN
              IF (ECHOLV .GE. 6 .AND. MCOFLG .EQ. 0) THEN
                WRITE(OUTFL,173) TMCUR,NUMT,TIN
              ENDIF
            END IF
          END IF
C
          IF (NVPR.GT.0.AND.IMODL.EQ.0) THEN
C
C           Print Darcy velocity and water saturation for each node
C
            IF(MOD(ITER,NVPR).EQ.0) THEN
              IF (ECHOLV .GE. 6 .AND. MCOFLG .EQ. 0) THEN
                WRITE(OUTFL,169)
                WRITE(OUTFL,179)(I,VDAR(I),SWEL(I),I=1,NPM1)
              ENDIF
            END IF
          END IF
C
C         Loop to calculate current nodal values.  For unsaturated
C         flow problem, repeat nonlinear solutions until convergence
C         is reached
C
1220      CONTINUE
C
          IF (ICONVG.NE.0) THEN
C
C           Solution converges, update initial nodal vector and tmvecx
C
            DO 1240 I= 1,NP
              PINT(I)= DIS(I)
1240        CONTINUE
            TMVECX=TMVECX+TIN
C
C
          ELSE
C
C           reset original value if time step halved
C
            DO 1260 I=1,NP
              DIS(I)=PINT(I)
1260        CONTINUE       
C
          END IF
C
          IF(ITCND1.EQ.1 .OR. ITCNDN .EQ. 1) THEN
            IF(IMODL.EQ.0.AND.THETA.LT.0.999) THEN
              CALL HFINTP(
     I          MXTMV,ITCND1,ITCNDN,NTSNDH,HVTM,QVTM,TMHV,
     I          TMVEX1,TMVEX2,IMODL,
     M          ITSTH,FLX1,FLXN,VALND1,VALNDN)
              FLXO(1) = FLX1
              FLXO(2) = FLXN
              FLSTO(1) = VALND1
              FLSTO(2) = VALNDN
            ENDIF
C
            CALL HFINTP(
     I        MXTMV,ITCND1,ITCNDN,NTSNDH,HVTM,QVTM,TMHV,
     I        TMVEX1,TMVEX2,IMODL,
     M        ITSTH,FLX1,FLXN,VALND1,VALNDN)
          ENDIF
C
          CALL VARCAL(
     I      IREPMX,ITER,OUTFL,IPRCHK,THETA,
     I      THETM1,IBTND1,IBTNDN,VALND1,VALNDN,VDAR,EL,
     I      IPZONE, PARENT, NUMT, 
     M      IREP,TMVECX,SWEL,DLAMND,FLSTO,FLXO,
     O      ICONVG)
C
C         When solution does not converge, repeat the scheme using
C         reduced time step
C
          IF (ICONVG.EQ.0) GO TO 1220
C
C         When solution converges, compute mass balance
C
          TDIFF=TMCUR-TMVECX
          INOCTS=INOCTS+1
C
          IF (IMBAL.EQ.1) CALL BALCHK (
     I                      THETA,THETM1,
     1                      EL,VDAR,DLAMND,OUTFL,TMVECX,
     2                      BALSTO,SWEL,VDAR1,VDARN,SIMTYM,
     O                      RTEMP,
     I                      IPZONE, PARENT, NUMT, ICHEM, 
     I                      IBTND1, IBTNDN)
C
C         Check if the end of the time step is reached
C
          DTEPS=0.01*TIN
          IF (TDIFF.GT.DTEPS) GO TO 1220
C
C         Solution has been performed until the end of the specified
C         time step
C
          IF (NOBSND.GT.0.AND.IOBSND.EQ.1) THEN
C           Record values from observation nodes for those requested
            DO 1268 I= 1,NOBSND
              NDNO= NDOBS(I)
              HDOBS(ITER,I)= DIS(NDNO)
1268        CONTINUE
          END IF
          IF (NOBSWD.GT.0.AND.IOBSND.EQ.1) THEN
            DO 35 I= 1,NOBSWD
              NDNO= NODE(I)
              IF (COD(I) .EQ. 'H' .OR. COD(I) .EQ. 'C') THEN
C               head or concentration
                VRVAL(ITER,I)= DIS(NDNO)
              ELSE IF (COD(I) .EQ. 'V') THEN
C               darcy velocity
                VRVAL(ITER,I)= VDAR(NDNO)
              ELSE IF (COD(I) .EQ. 'S') THEN
C               water saturation
                VRVAL(ITER,I)= SWEL(NDNO)
              ELSE IF (COD(I) .EQ. 'F') THEN
C               flux
                IF (NDNO .EQ. 1) THEN
C                 flux at first node
                  VRVAL(ITER,I)= BALSTO(1)
                ELSE IF (NDNO .EQ. NP) THEN
C                 flux at last node
                  VRVAL(ITER,I)= -BALSTO(2)
                ELSE
C                 error, flux available only at first and last nodes
                  VRVAL(ITER,I)= -999
                ENDIF
              ENDIF
  35        CONTINUE
          END IF
C
C
          IF (NSTEP.GT.0) THEN
C
C           We want a detailed printout
C
            IF (MOD(ITER,NSTEP).EQ.0) THEN
C
C             Time for detailed printout
C
              IF (ECHOLV .GE. 6 .AND. MCOFLG .EQ. 0) THEN
                IF (IMODL.EQ.1) WRITE(OUTFL,2040) TMCUR
                IF (IMODL.EQ.0) WRITE(OUTFL,2050)
                WRITE(OUTFL,2060)(I,DIS(I),I=1,NP)
              ENDIF
            END IF
          END IF
C
          IF (IMODL.EQ.1) THEN
C
C           Compute nodal values of water saturation and Darcy velocity
C
            CALL VSWCOM(
     I        TRNSIM,
     I        ITER,NE,NP,NVPR,MARK,NTOMT,
     I        DIS,EL,PKND,OUTFL,TAP10,ITMARK,ITMFC,
     I        TMCUR,TIMA,
     M        SWEL,
     O        VDAR,SWELPT,VDARPT,VDAR1O,VDARNO)
C
C           next variable is flag for printing observation nodes
            ITIME = 0
C
C           print daily,cumulative, and yearly output
            IF (OUTF .EQ.' DAY' .AND. ECHOLV .GE. 6) THEN
              IF(IMBAL.EQ.1 .AND. MCOFLG .EQ. 0) THEN
                WRITE(OUTFL,1243) ITER,INOCTS
                ITIME = 1
              ENDIF
            ENDIF
            IF(OUTF .EQ. 'MNTH' .AND. NUMT .EQ. NTS) THEN 
              IF(IMBAL .EQ. 1 .AND. ECHOLV .GE. 6) THEN
                ITIME = 1
              ENDIF
            ENDIF
            IF(OUTF .EQ. 'YEAR' .AND. SIMTYM .EQ. 'Dec') THEN
              IF (IMBAL .EQ. 1 .AND. ECHOLV .GE. 6) THEN
                IF(NTS .EQ. NUMT) THEN
                  ITIME = 1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C         Update initial time value
C
          TIMA = TMVEC(NUMT)
C
 200    CONTINUE
C
        IF (IOBSND.EQ.1.AND.NOBSND.GT.0) THEN
C
C         Print observation node information
C
          IF (ITIME .EQ. 1 .AND. ECHOLV .GE. 4) THEN
            IF (MCOFLG .EQ. 0) THEN
              DO 1278 I= 1,NOBSND
                IF (IMODL.EQ.0) WRITE(OUTFL,1273) NDOBS(I)
                IF (IMODL.EQ.1) WRITE(OUTFL,1275) NDOBS(I)
                WRITE(OUTFL,1283)(TMVEC(ITER),HDOBS(ITER,I),ITER=1,NTS)
1278          CONTINUE
            ENDIF
          ENDIF
C
          IF (NOBSWD .GT. 0) THEN
C           print observation node information to wdms file
            DLT    = 1
            DTOVWR = 1
            QUALFG = 0
            TUNITS = 4
            DATES(1) = ISTYR + 1900
            DATES(2) = ISMON
            DATES(3) = ISDAY
            DATES(4) = 0
            DATES(5) = 0
            DATES(6) = 0
C
            DO 25 I= 1,NOBSWD
              CALL WDTPUT(FWDMS,VADDSN(I),DLT,DATES,NTS,DTOVWR,
     I                    QUALFG,TUNITS,VRVAL(1,I),
     O                    RETCOD)
              IF (RETCOD.NE.0 .AND. MCOFLG .EQ. 0) THEN
C               problem writing wdmsfl
                WRITE (FVADOT(IPZONE),2020) VADDSN(I),RETCOD
              END IF
  25        CONTINUE
          ENDIF
        END IF
C
C
      GO TO 3000
 2000 CONTINUE
C
C       Read error
C
        FATAL = .TRUE.
        MESAGE = 'End of file reading VADOFT Darcy velocities'
        IERROR = 3210
        CALL ERRCHK(IERROR,MESAGE,FATAL)
 2010 CONTINUE
        FATAL = .TRUE.
        MESAGE = 'Error reading VADOFT Darcy velocity file'
        CALL ERRCHK(IERROR,MESAGE,FATAL)
 3000 CONTINUE
      CALL SUBOUT
      RETURN
C
      END
C
C
C
      SUBROUTINE VARCAL(
     I  IREPMX,ITERA,OUTFL,IPRCHK,THETA,
     I  THETM1,IBTND1,IBTNDN,VALND1,VALNDN,VDAR,EL,
     I  IPZONE, PARENT, NUMT,
     M  IREP,TMVECX,SWEL,DLAMND,FLSTO,FLXO,
     O  ICONVG)
C
C     + + + PURPOSE + + +
C
C     Routine to compute current nodal values of head or concentration
C     VARCAL is called by main and calls ASSEMF, ASSEMT, and TRIDIV.
C     this subroutine is called once each simulation time step.
C     Mdofication date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXNOD.INC'
      INTEGER*4 ITERA,OUTFL,IREP,ICONVG,IPRCHK,
     1          IBTND1,IBTNDN,IREPMX,NUMT,IPZONE,PARENT
      REAL*8    VALND1,VALNDN,TMVECX,THETA,THETM1,
     1          VDAR(MXNOD),EL(MXNOD),SWEL(MXNOD),
     2          DLAMND(MXNOD),FLSTO(2),FLXO(2)
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CVASLV.INC'
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CHYDR.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      INTEGER*4    IERROR,III,I,NCOUNT,NDMAX
      REAL*8       RELAXF,HDMAX,HDIF,EPSLN2,EPSLN1,ES,HOLD,TINKP
      REAL*8       CTOL,RTOL
      CHARACTER*80 MESAGE
      LOGICAL      FATAL
C
C     + + + INTRINSICS + + + 
C      
      INTRINSIC ABS,DABS
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,ASSEMT,ASSEMF,TRIDIV,ERRCHK,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT(/36X,'NUMBER OF'/34X,'NON-CONVERGENT',5X,'MAXIMUM',6X,
     1  'NODE',6X,'RELAXATION'/20X,'ITERATION',9X,'NODES',11X,'ERROR',
     2  6X,'NUMBER',7X,'FACTOR'/20X,9('-'),5X,14('-'),3X,11('-'),3X,
     3  6('-'),5X,10('-')/)
2010  FORMAT(22X,I4,13X,I4,7X,1P,G12.4,0P,4X,I4,6X,1P,G10.3,0P)
2020  FORMAT(/10X,'...... SOLUTION NOT CONVERGING......',10X,
     1'WORST HEAD ERROR =',E12.4)
2030  FORMAT('VARCAL- time step',I5,
     1 ' solution fails to converge after',I4,' reductions')
2040  FORMAT(///10X,
     1 'ELAPSED SIMUL. TIME:',E12.4,4X,
     2 'CURRENT TIME STEP VALUE:',E12.4,4X,
     3 'CUM. NO. OF NONLINEAR SOLUTION CYCLES :',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'VARCAL'
      CALL SUBIN(MESAGE)
C
      ICONVG= 1
      RELAXF= 1.
      CTOL=   0.0
      RTOL=   0.001
C
C     Set up iterative loop, do until all nodes converge or
C     for NITMAX iterations
C
      III = 1
      IREP = IREP+1
 10   CONTINUE
        NCOUNT=0
        IF (IMODL.EQ.0) THEN
C
          CALL ASSEMT(
     I      ITERA,PINT,IBTND1,IBTNDN,DLAMND,
     I      VDAR,SWEL,EL,OUTFL,NP,TIN,UWF,DLAMDA,FLX1,FLXN,
     I      IPRCHK,THETA,THETM1,VALND1,VALNDN,FLSTO,FLXO,ITRANS,
     I      PARENT, NUMT, NITMAX, DIS)
C
        ELSE
C
C         Compute and assemble element matrices for variably saturated
C         water flow simulation
C
          CALL ASSEMF
     I               (ITERA,OUTFL,IPRCHK,NP,ITRANS,TIN,HTOL,
     I                INEWT,MARK,KPROP,IBTND1,IBTNDN,
     I                VALND1,VALNDN,EL,
     O                SWEL)
        END IF
C
C       Perform tridiagonal matrix solution to computer pressure head
C       values based on flow equations or concentration values based
C       on transport equation
C
        CALL TRIDIV(
     I    NP,A,B,C,D,
     O    PCUR)
C
C       Check convergence
C
        IF (NITMAX.NE.1) THEN
C
C         Perform nonlinear iterations for flow simulations 
C         (NITMAX is set equal to 1 for all transport runs in main)
C
          HDMAX = 0.
          DO 50 I=1,NP
C
C           Obtain head differences between old and new values
C
            HDIF = PCUR(I)-DIS(I)
            CTOL = HTOL+RTOL*ABS(DIS(I))
            IF (DABS(HDIF).GT.CTOL) NCOUNT=NCOUNT+1
            IF (DABS(HDMAX).LE.DABS(HDIF)) THEN
C
C        Count the # of differences that exceed the head tolerance and
C        find the node that has the greatest difference.
C
              HDMAX = HDIF
              NDMAX = I
            END IF
  50      CONTINUE
          EPSLN2 = HDMAX
          IF (III.EQ.1) EPSLN1=EPSLN2
C
C         Reset relaxation factor, RELAXF
C
          ES = 1.
          IF (DABS(EPSLN1).GE.1.E-10) ES = EPSLN2/(EPSLN1*RELAXF)
          RELAXF = (3.+ES)/(3.+DABS(ES))
          IF (ES.LE.-1.) RELAXF = 0.5/(DABS(ES))
C
C         Print on same days as PRZM if it is on
C
          IF (ECHOLV .GE. 6 .AND. MCOFLG .EQ. 0) THEN
            IF (III.EQ.1 .AND. IPRCHK.EQ.1) THEN
C
C               Output header for no converge
C
                WRITE(OUTFL,2040) TMVECX,TIN,IREP
                WRITE(OUTFL,2000)
                WRITE(OUTFL,2010) III,NCOUNT,HDMAX,NDMAX,
     *                            RELAXF
            END IF
          ENDIF
          EPSLN1= EPSLN2
        END IF
C
C       Reset head values (PCUR values set to current to DIS values,
C       DIS values updated based on results of current iterations)
C
      DO 80 I=1,NP
        PCUR(I) = DIS(I)+RELAXF*(PCUR(I)-DIS(I))
        HOLD = DIS(I)
        DIS(I) = PCUR(I)
        PCUR(I) = HOLD
 80   CONTINUE
C
C
      III = III+ 1
      IF (NCOUNT.GT.0.AND.III.LE.NITMAX) GO TO 10
C
      IF (NCOUNT.NE.0) THEN
C
C       We did not converge
C
        ICONVG= 0
        IF (MCOFLG .EQ. 0) THEN
          IF((IPRCHK.GE.1) .AND. (ECHOLV .GE. 6))
     1    WRITE(OUTFL,2020) HDMAX
        ENDIF
        TINKP = TIN
        TIN = TIN*0.5
        TMVECX = TMVECX-(TINKP-TIN)
        IF (IREP.GE.IREPMX) THEN
C
C         Fatal error, max number of refinements exceeded
C
          IERROR = 3010
          WRITE(MESAGE,2030) ITERA,IRESOL
          FATAL  = .TRUE.
          CALL ERRCHK(IERROR,MESAGE,FATAL)
        END IF
      END IF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE HFINTP(
     I  MXTMV,ITCND1,ITCNDN,NTSNDH,HVTM,QVTM,
     I  TMHV,TMVEX1,TMVEX2,IMODL,
     M  ITSTH,FLX1,FLXN,VALND1,VALNDN)
C
C     +  +  + PURPOSE +  +  +
C
C     Determines the boundary values for head, concentration, or flux
C     for simulation time step by interpolating values.
C     HFINTP is called by VADCAL and makes no subroutine calls.
C     This subroutine is called once each time step to 
C     update transient boundary conditions.
C     Modification date: 2/7/92 JAM
C
C     +  +  + DUMMY ARGUMENTS +  +  +
C
      INTEGER*4    MXTMV,ITCND1,ITCNDN,NTSNDH(2),ITSTH(2),IMODL,IBC(2)
      REAL*8       HVTM(2,MXTMV),QVTM(2,MXTMV),TMHV(2,MXTMV),
     1             FLX1,FLXN,VALND1,VALNDN,TMVEX1,TMVEX2
C
C     +  +  + ARGUMENT DEFINITIONS +  +  + 
C
C     MXTMV  -
C     ITCND1 -
C     ITCNDN -
C     NTSNDH -
C     HVTM   -
C     QVTM   -
C     TMHV   -
C     TMVEX1 -
C     TMVEX2 -
C     IMODL  - flag for flow or transport simulation
C     ITSTH  - previous time value location of time graph
C     FLX1   - value of fluid injected into top node (transient)
C     FLXN   - value of fluid injected into bottom node (transient)
C     VALND1 - value of pressure, water flux or conc. at top node
C     VALNDN - value of pressure, water flux or conc. at bottom node
C
C
C     +  +  + LOCAL VARIABLES
C
      INTEGER*4    I,J,NN,NTSTEM,DONFG,JM1,IERROR
      LOGICAL      FATAL
      REAL*8       TMK,TMK1,TMK2,HVCUR,QVCUR,SLOPE,QLOPE
      CHARACTER*80 MESAGE
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL SUBIN,SUBOUT,ERRCHK
C
C     +  +  + END SPECIFICATIONS +  +  +
C
      MESAGE = 'HFINTP'
      CALL SUBIN(MESAGE)
C
      IBC(1)= ITCND1
      IBC(2)= ITCNDN
      IF(IMODL.EQ.1) TMK = TMVEX1
      IF(IMODL.EQ.0) TMK = TMVEX2
      NN = 2
      DO 30 I=1,NN
C
C-------Derive head/conc value for first(I=1) and last(I=2) nodes
C
        IF (IBC(I).NE.0) THEN
C
C---------Boundary condition is transient
C
          NTSTEM= NTSNDH(I)
          J     = ITSTH(I)
          DONFG = 0
C
C---------Do until (j.gt.ntstem.or.tmk.le.tmhv(i,j)(donfg=1))
C         i.e. until j .gt. number of time values on time graph
C                    tmk .gt. upper interpolation point
C                    done flag shows completion
C
10        CONTINUE
            IF (TMK.LE.TMHV(I,J)) THEN
C
C-------------Time is less than some interpolation point on time graph
C
              IF(J.EQ.1) THEN
                HVCUR=HVTM(I,1)
                QVCUR=QVTM(I,1)
                GO TO 20
              END IF
              JM1     = J-1
              ITSTH(I)= JM1
              TMK1    = TMHV(I,JM1)
              TMK2    = TMHV(I,J)
              SLOPE   = (HVTM(I,J)-HVTM(I,JM1))/(TMK2-TMK1)
              QLOPE   = (QVTM(I,J)-QVTM(I,JM1))/(TMK2-TMK1)
              HVCUR   = HVTM(I,JM1)+SLOPE*(TMK-TMK1)
              QVCUR   = QVTM(I,JM1)+QLOPE*(TMK-TMK1)
              DONFG   = 1
            ELSE
C
C-------------Try next value on time graph
C
              J= J+ 1
            END IF
C
          IF (J.LE.NTSTEM.AND.DONFG.EQ.0) GO TO 10
C
C---------If still have more TABLE or WDMS values and haven't yet
C         found proper bounds, repeat loop until end of loop
C
          IF (DONFG.EQ.0) THEN
            IERROR = 3000
            FATAL  = .TRUE.
            MESAGE =  'Fatal error in HFINTP, interpolation failed'
            CALL ERRCHK(
     I        IERROR, MESAGE, FATAL)
          END IF
  20      CONTINUE
C
          IF (I.EQ.1) THEN
C
C-----------Set head and flux values for first node
C
            VALND1= HVCUR
            FLX1  = QVCUR
          ELSE
C
C-----------Set head and flux values for last node
C
            VALNDN= HVCUR
            FLXN  = QVCUR
          END IF
        ELSE
C
C-------We get here if IBC(I)=0
C
        END IF
30    CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE INTERP(
     I  MXMAT,IRLTYP,IMAT,XX,SWV,PKRW,SSWV,HCAP,
     I  NUMK,NUMP,
     O  YY,SLOPE)
C
C     + + + PURPOSE + + +
C
C     Performs linear interpolation using tabulated data of
C     relative permeability versus water saturation, and
C     pressure head versus water saturation.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER*4        MXMAT,IRLTYP,IMAT,
     1                 NUMK(MXMAT),NUMP(MXMAT)
      REAL*8           XX,YY,SLOPE,HCAP(MXMAT,30),
     1                 PKRW(MXMAT,30),SSWV(MXMAT,30),SWV(MXMAT,30)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     
C     MXMAT  - maximum number of materials 
C     IRLTYP - ???
C     IMAT   - current material number
C     NUMK   - ???
C     NUMP   - ???
C     XX     - ???
C     YY     - ???
C     SLOPE  - ???
C     HCAP   - ???
C     PKRW   - ???
C     SSWV   - ???
C     SWV    - ???
C
C     + + + LOCAL VARIABLES + + +
C
      INTEGER*4    I,IM1,N,DONFG
      REAL*8       SWMIN,X1,X2,Y1,Y2,DLOG,XRATIO,YRATIO,DABSXX
      REAL*8       ATERM,BTERM
      CHARACTER*80 MESAGE
C
C     +  +  + INTRINSICS +  +  + 
C      
      INTRINSIC DABS,DLOG,DEXP
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL SUBIN,SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'INTERP'
      CALL SUBIN(MESAGE)
C
      IF (IRLTYP.EQ.1) THEN
C
C       Evaluate using pressure head vs saturation table
C
        N    = NUMP(IMAT)
        SWMIN= SSWV(IMAT,1)+0.005
        DONFG= 0
        I    = 2
C
C       Do until value fits into the table or the value exceeds the
C       maximum tabled value
C
 10     CONTINUE
          IM1= I-1
          X1 = HCAP(IMAT,IM1)
          X2 = HCAP(IMAT,I)
          IF (XX.GE.X1.AND.XX.LE.X2) THEN
C
C           Found where value fits into the table, now interpolate
C           to evaluate corresponding water saturation value
C
            Y1   = SSWV(IMAT,IM1)
            Y2   = SSWV(IMAT,I)
            SLOPE= (Y2-Y1)/(X2-X1)
            YY   = Y1+(XX-X1)*SLOPE
C
C           Assign minimum value for water saturation if interpolated
C           value is very small
C
            IF (YY.LE.SWMIN) YY= SWMIN
            DONFG= 1
          END IF
          I= I+ 1
        IF (I.LE.N.AND.DONFG.EQ.0) GO TO 10
C
C       End of do until
C
        IF (DONFG.EQ.0) THEN
C
          IF (XX.GT.HCAP(IMAT,N)) THEN 
            YY=1.
          ELSE  
            X2=HCAP(IMAT,2)
            X1=HCAP(IMAT,1)
            Y2=SSWV(IMAT,2)
            Y1=SSWV(IMAT,1)
            YRATIO=DABS(Y2/Y1)
            XRATIO=DABS(X2/X1)
            ATERM=DLOG(YRATIO)/DLOG(XRATIO)
            X1=DABS(X1)
            BTERM=DLOG(Y1)-ATERM*DLOG(X1)
            DABSXX=DABS(XX)
            YY=ATERM*DLOG(DABSXX)+BTERM
            YY=DEXP(YY)
            IF (YY.LT.SWMIN) YY=SWMIN
          ENDIF
        END IF
      ELSE
C
C       Compute interpolated valuefor permeability based on water
C       saturation value passed into INTERP
C
        N    = NUMK(IMAT)
        I    = 2
        DONFG= 0
C
C      Do until value fits into the table or until the value exceeds the
C      maximum tabled value
C
 40     CONTINUE
          IM1= I-1
          X1 = SWV(IMAT,IM1)
          X2 = SWV(IMAT,I)
          IF (XX.GE.X1.AND.XX.LE.X2) THEN
C
C           Found where water saturation value fits into table, now
C           interpolate to evaluate corresponding permeability value
C
            Y1   = PKRW(IMAT,IM1)
            Y2   = PKRW(IMAT,I)
            SLOPE= (Y2-Y1)/(X2-X1)
            YY   = Y1+(XX-X1)*SLOPE
            DONFG= 1
          END IF
          I= I+ 1
        IF (I.LE.N.AND.DONFG.EQ.0) GO TO 40
C
C       End of do until
C
        IF (DONFG.EQ.0) THEN
C
C         Computed saturation value is outside specified interpolation.
C         Need to apply log interpolation.
C
C 
          IF (XX.GT.SWV(IMAT,N)) THEN 
            YY=1.
C
          ELSE  
C
            Y2=PKRW(IMAT,2)
            Y1=PKRW(IMAT,1)
            X2=SWV(IMAT,2)
            X1=SWV(IMAT,1)
            YRATIO=Y2/Y1
            XRATIO=X2/X1
            ATERM=DLOG(YRATIO)/DLOG(XRATIO)
            BTERM=DLOG(Y1)-ATERM*DLOG(X1)
            YY=ATERM*DLOG(XX)+BTERM
C
            YY=DEXP(YY)
          ENDIF
        END IF
      END IF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE ASSEMT(
     I  ITER,PINT,IBTND1,IBTNDN,DLAMND,VDAR,
     I  SWEL,EL,OUTFL,NP,TIN,UWF,DLAMDA,FLX1,FLXN,
     I  IPRCHK,THETA,THETM1,VALND1,VALNDN,FLSTO,FLXO,
     I  ITRANS,
     I  PARENT, NUMT, NITMAX, DIS)
C
C     + + + PURPOSE + + +
C
C     Routine to compute and assemble element matrices for solute
C     transport simulation.
C     ASSEMT is called by VARCAL and makes no subroutine calls. This
C     subroutine is executed only once for each simulation time step
C     since NITMAX = 1 for transport simulation.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXNOD.INC'
      INTEGER*4 ITER,IBTND1,IBTNDN,OUTFL,NP,IPRCHK,PARENT,NUMT,
     1          ITRANS,NITMAX
      REAL*8    TIN,UWF,DLAMDA,FLX1,FLXN,THETA,THETM1,
     1          PINT(MXNOD),VDAR(MXNOD),DIS(MXNOD),
     1          SWEL(MXNOD),EL(MXNOD),DLAMND(MXNOD),
     1          VALND1,VALNDN,SWAVE,CAVE,DLAMD1,DLAMD2
C
C     + + + PARAMETERS + + +
C
C      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXVDT.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CVASLV.INC'
      INCLUDE 'CWELEM.INC'
      INCLUDE 'CWORKA.INC'
      INCLUDE 'CVWRKM.INC'
      INCLUDE 'CVCHMK.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      INTEGER*4 I,IM1,IMAT,IP1,ITCHK,NPM1,IMAT1,IMAT2
      REAL*8    ALFAL,AN,BN,CN,CONS1,CONS2,DM,EL2,ELNE,POROS,EL1,
     1          RETARD,TCONS,UWFM1,UWFP1,FRACM,ETA,ETAM1,TERM
      REAL*8    DE(MXNOD),VE(MXNOD),SE(MXNOD),
     1          AZ(MXNOD),BZ(MXNOD),CZ(MXNOD),FLSTO(2),
     2          FLXO(2),VESQ,SETMP
      CHARACTER*80 MESAGE
C
C     +  +  + EXTERNALS +  +  + 
C      
      EXTERNAL SUBIN,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT(//10X,'PRINT CHECK FOR TIME STEP',I5//
     1  4X,'NODE #',2X,'COEFF. A(I)',4X,'COEFF. B(I)',
     2  4X,'COEFF. C(I)',4X,'COEFF. D(I)',6X,'COEFF. AZ(I)',
     3  6X,'COEFF. BZ(I)',5X,'COEFF. CZ(I)'/)
2010  FORMAT(4X,I4,4(4X,E11.4),3(4X,E13.4))
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'ASSEMT'
      CALL SUBIN(MESAGE)
C
C     Specify # of times debugging/print check information is to be
C     output for A,B,C,D,A*,B*,C*, coefficient values
C
      ITCHK= 2
C
C     Evaluate run constants to simplify equations
C
      UWFM1= (1.-UWF)*0.5
      UWFP1= (1.+UWF)*0.5
      TCONS= 1./(6.*TIN)
C
C     Set TCONS = 0.0 for STEADY STATE TRANSPORT SIMULATION
C
      IF(ITRANS.EQ.0) TCONS = 0.0
C
C
      NPM1=NP-1
      DO 150 I=1,NPM1
        IMAT= IPROP(I)
        IP1 = I+1
        ALFAL = PROP(IMAT,1)
        POROS = PROP(IMAT,2)
        SWAVE = SWEL(I)
C
        IF (NITMAX.LE.1) THEN
          RETARD = 1.0 + (PROP(IMAT,3) - 1.0) / SWAVE
        ELSE
          CAVE = 0.5*(DIS(I)+DIS(IP1))
          IF (CAVE.LT.1.0E-20) CAVE=1.0E-20
          ETA = PROP(IMAT,5)
          ETAM1 = ETA - 1.0
          RETARD = 1.0 + (PROP(IMAT,3)*ETA*CAVE**ETAM1)/(SWAVE*POROS)
        ENDIF
C
C
        DM    = PROP(IMAT,4)
C
C       Compute velocity, dispersion, and moisture capacity terms as
C       per Equation 2-9 in text
C
        VE(I) = VDAR(I)
        DE(I) = ALFAL*VE(I)+DM*POROS*SWAVE
        SETMP = POROS*RETARD*SWAVE
        SE(I) = SETMP
        ETAND(I)=SE(I)
 150  CONTINUE
C
      IF(IBTND1.EQ.1)PINT(1)=VALND1
      IF(IBTNDN.EQ.1)PINT(NP)=VALNDN
C
C     Set up A, B, C, D arrays
C
      DO 200 I= 2,NPM1
C
C       Evaluate arrays for all nodes except the first and last
C
        IM1  = I-1
        IP1  = I+1
        EL1  = EL(I-1)
        EL2  = EL(I)
        DLAMD1=DLAMND(I-1)
        DLAMD2=DLAMND(I)
        VESQ = VE(I)*VE(IM1)
        IF (VESQ.GT.0.0) THEN
          IF (VE(I).GE.0.0) THEN
            CONS1= (DE(IM1)/EL1)+UWFP1*VE(IM1)
            CONS2= (DE(I)/EL2)-UWFM1*VE(I)
          ELSE
            CONS1= (DE(IM1)/EL1)+UWFM1*VE(IM1)
            CONS2= (DE(I)/EL2)-UWFP1*VE(I)
          ENDIF
        ELSE
          CONS1= (DE(IM1)/EL1)+0.5*VE(IM1)
          CONS2= (DE(I)/EL2)-0.5*VE(I)
        ENDIF
        AN   = -CONS1+DLAMD1*SE(IM1)*EL1/6.
        CN   = -CONS2+DLAMD2*SE(I)*EL2/6.
        BN   = CONS1+CONS2+(DLAMD1*SE(IM1)*EL1+DLAMD2*SE(I)*EL2)/3.
C
        A(I) = THETA*AN+SE(IM1)*EL1*TCONS
        C(I) = THETA*CN+SE(I)*EL2*TCONS
        B(I) = THETA*BN+2.*TCONS*(SE(IM1)*EL1+SE(I)*EL2)
        D(I) = THETM1*(AN*PINT(IM1)+BN*PINT(I)+CN*PINT(IP1))
     1         +TCONS*SE(IM1)*EL1*(PINT(IM1)+2.*PINT(I))
     2         +TCONS*SE(I)*EL2*(PINT(IP1)+2.*PINT(I))
        AZ(I)= AN
        BZ(I)= BN
        CZ(I)= CN
C       Calculate the amount of formation from parent compound
        IF ((ICHAIN.EQ.1) .AND. (PARENT .NE. 0)) THEN
          IMAT1 = IPROP(I-1)
          IMAT2 = IPROP(I)
          FRACM = (FRACMP(IMAT1)*EL1+FRACMP(IMAT2)*EL2)/(EL1+EL2)
          TERM = FRACM*TRTERM(I,NUMT,PARENT)
          D(I) = D(I) + TERM
        ENDIF
  200 CONTINUE
C
C     The following code assigns or calculates coefficient values for
C     first node dependent on soil moisture boundary conditions
C     Simplify and evaluate coefficient equations 3-6A through 3-6G
C     for first node
C
C     Node above soil surface does not exist
C
      A(1) = 0.
C
C     Node below bottom of column does not exist
C
      C(NP)= 0.
      EL1  = EL(1)
      DLAMDA=DLAMND(1)
C
      IF (VE(1).GE.0.0) THEN
        CONS2= (DE(1)/EL(1))-UWFM1*VE(1)
      ELSE
        CONS2= (DE(1)/EL(1))-UWFP1*VE(1)
      ENDIF
      CN   = -CONS2+DLAMDA*SE(1)*EL1/6.
      BN   = CONS2+DLAMDA*SE(1)*EL1/3.
      CSTOR1 = THETA*CN+SE(1)*EL1*TCONS
      BSTOR1 = THETA*BN+2.*TCONS*(SE(1)*EL1)
      DSTOR1 = THETM1*(BN*PINT(1)+CN*PINT(2))+TCONS*SE(1)*EL1*
     1       (PINT(2)+2.*PINT(1))
      BZ(1)= BN
      CZ(1)= CN
      AZ(1)= 0.
C
C     Calculate the amount of formation from parent compound
C
      IF ((ICHAIN.EQ.1) .AND. (PARENT .NE. 0)) THEN
        IMAT = IPROP(1)
        TERM = FRACMP(IMAT) * TRTERM(1,NUMT,PARENT)
        D(1) = D(1) + TERM
      ENDIF
C
      IF (IBTND1.EQ.1) THEN
C
C       1st type boundary condition (prescribed concentration) 
C
        B(1) = 1.
        C(1) = 0.
        D(1) = VALND1
      ELSE
C
C       3rd type boundary condition (prescribed flux) (first node)
C
C
        B(1) = BSTOR1+FLX1*THETA
        C(1) = CSTOR1
        D(1) = DSTOR1+VALND1*THETA-THETM1*FLSTO(1)+
     1         THETM1*FLXO(1)*PINT(1)
      END IF
      NPM1 = NP-1
      ELNE = EL(NPM1)
C
      IF (VE(NPM1).GE.0.0) THEN
        CONS1 = (DE(NPM1)/ELNE)+UWFP1*VE(NPM1)
      ELSE
        CONS1 = (DE(NPM1)/ELNE)+UWFM1*VE(NPM1)
      ENDIF
      DLAMDA=DLAMND(NPM1)
      AN   = -CONS1+DLAMDA*SE(NPM1)*ELNE/6.
      BN   = CONS1+DLAMDA*SE(NPM1)*ELNE/3.
      ASTORN= THETA*AN+SE(NPM1)*ELNE*TCONS
      BSTORN= THETA*BN+2.*TCONS*(SE(NPM1)*ELNE)
      DSTORN= THETM1*(AN*PINT(NPM1)+BN*PINT(NP))+TCONS*SE(NPM1)*ELNE
     1       *(PINT(NPM1)+2.*PINT(NP))
      AZ(NP)= AN
      BZ(NP)= BN
      CZ(NP)= 0.
C
C     Calculate the amount of formation from parent compound
C
      IF ((ICHAIN.EQ.1) .AND. (PARENT .NE. 0)) THEN
        IMAT = IPROP(NPM1)
        TERM = FRACMP(IMAT) * TRTERM(NP,NUMT,PARENT)
        D(NP) = D(NP) + TERM
      ENDIF
C
      IF (IBTNDN.EQ.1) THEN
C
C       1st type boundary condition (prescribed concentration) 
C
        B(NP)= 1.
        A(NP)= 0.
        D(NP)= VALNDN
C
      ELSE
C
C       3rd type boundary condition (prescribed flux) (last node)
C
        A(NP)= ASTORN
        B(NP)= BSTORN+FLXN*THETA
        D(NP)= DSTORN+VALNDN*THETA-THETM1*FLSTO(2)+
     1        THETM1*FLXO(2)*PINT(NP)
      END IF
C
      IF (IPRCHK.EQ.2.AND.ITER.LE.ITCHK) THEN
C
C       Print A, B, C, D arrays if debugging/print check wanted
C
        WRITE(OUTFL,2000) ITER
        DO 374 I=1,NP
          WRITE(OUTFL,2010) I,A(I),B(I),C(I),D(I),AZ(I),BZ(I),CZ(I)
  374   CONTINUE
      END IF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE ASSEMF(
     I  ITER,OUTFL,IPRCHK,NP,ITRANS,TIN,HTOL,
     I  INEWT,MARK,KPROP,IBTND1,IBTNDN,
     I  VALND1,VALNDN,EL,
     O  SWEL)
C
C     + + + PURPOSE + + +
C
C     Routine to compute and assemble element matrices for
C     variably saturated water flow simulation.
C     ASSEMF is called by VARCAL and calls SWFUN, PKWFUN, DSWFUN, and
C     INTERP. This subroutine is executed each iteration for one
C     simulation time step until successful solution to the flow
C     equation has been attained or allowable # of iterations has
C     been exceeded.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXNOD.INC'
      INTEGER*4    ITER,OUTFL,IPRCHK,NP,ITRANS,INEWT,MARK,KPROP,
     1             IBTND1,IBTNDN
      REAL*8       TIN,HTOL,VALND1,VALNDN,EL(MXNOD),SWEL(MXNOD)
      CHARACTER*80 MESAGE
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PMXMAT.INC'
C      INCLUDE 'PMXNOD.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CWELEM.INC'
      INCLUDE 'CSWHDA.INC'
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CVASLV.INC'
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CWORKA.INC'
      INCLUDE 'CECHOT.INC'
C
C     + + + LOCAL VARIABLES + + +
C                                
      REAL*8       AN(MXNOD),BN(MXNOD),CN(MXNOD),DN(MXNOD)
      REAL*8       HAVE,HCRIT,SWAVE,PKRAVE,DSWAVE,DPKRAV,HAVOLD,
     1             PKROLD,SWOLD,DSWOLD,DPKROL,DELSW,DELH,PMXX,
     2             HAVINT,SWINT,DSWINT,DELHOL,DH,DDSW,POROS,SS,
     3             EL1,EL2,PK1,PK2,CONS1,CONS2,CMI,ELND1,CM1,ELNE,
     4             ELNDN,PKN,CONSN,CMN,ZERO,ELNDI,GRAD1,GRAD2,
     5             DPKIM1,DPKI,DPK1,DPKNE
      INTEGER*4    ITCHK,NPM1,I,MM,IMAT,IP1,IRLTYP,IM1
C
C     + + + FUNCTIONS + + +
      REAL*8       SWFUN,DSWFUN
C
C     + + + INTRINSICS + + +
C
      INTRINSIC DABS
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SWFUN,PKWFUN,DSWFUN,INTERP,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT(//10X,'PRINT CHECK FOR TIME STEP',I5//
     1       2X,'NODE #',2X,'COEF. A(I)',2X,'COEF. B(I)',
     2       2X,'COEF. C(I)',2X,'COEF. D(I)',2X,
     3       2X,'COND. PKND(I-1)',2X,'COND. PKND(I)',
     4       2X,'STORAGE ETAND(I-1)',6X,'STORAGE ETAND(I)'/)
2010  FORMAT(2X,I4,4(2X,E10.3),4(2X,E14.4))
2020  FORMAT(//10X,'NEWTON-RAPHSON ADDITIONAL PRINT CHECK'//
     1 2X,'NODE #',2X,'COEF. AN(I)',2X,'COEF. BN(I)',2X,'COEF. CN(I)'
     2 ,2X,'COEF. DN(I)',3X,'DPKND(I-1)',5X,'DPKND(I)',
     3 2X,'DETAND(I-1)',4X,'DETAND(I)'/)
2040  FORMAT(2X,I4,4(2X,E11.4),4(2X,E11.4))
2050  FORMAT(//10X,'LIST OF NODE NO. AND NEW AND OLD HEAD VALUES'/)
2060  FORMAT(4(I5,2E12.4))
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'ASSEMF'
      CALL SUBIN(MESAGE)
C
C     The following loop computes nodal conductivity and storage
C     coefficients for use in evaluating the matrix coefficients
C     for solution of the flow equations (value must be calculated for
C     current and last iterations in order to determine successful
C     solution of flow equation).
C
      ITCHK= 1
      NPM1=NP-1
      DETAND(NP)=0.0
C
      DO 150 I=1,NPM1
C
C       Determine saturation, permeability, and moisture capacity
C       terms for current iteration
C
        MM  = I
C
C       Assign appropriate hydraulic properties to element
C
        IMAT= IPROP(MM)
        IP1 = I+1
C
C       Calculate head at element centroid
C
        HAVE = 0.5*(DIS(I)+DIS(IP1))
        HCRIT= PROP(IMAT,4)
        IF (HAVE.GE.HCRIT) THEN
C
C         Set maximum values of water saturation, relative
C         permeability, and moisture capacity because critial value
C         has been exceeded (new values)
C
          SWAVE = 1.
          PKRAVE= 1.
          DSWAVE= 0.
        ELSE
C
C         Compute values of water saturation, relative permeability and
C         moisture capacity by using functions or tables (new values)
C
          IF (KPROP.EQ.1) THEN
C
C           using function
C
            SWAVE= SWFUN(MXMAT,HCRIT,HAVE,FVAL,IPROP(MM))
            CALL PKWFUN
     I                 (MXMAT,SWAVE,FVAL,IPROP(MM),
     O                  DPKRAV,PKRAVE)
            DSWAVE=DSWFUN(HCRIT,HAVE,HTOL,FVAL,IPROP(MM))
          ELSE
C
C           Using table of pressure vs relative permeability
C
            IRLTYP=1
            CALL INTERP
     I                 (MXMAT,IRLTYP,IMAT,HAVE,SWV,PKRW,SSWV,HCAP,
     I                  NUMK,NUMP,
     O                  SWAVE,DSWAVE)
            IRLTYP=2
            CALL INTERP
     I                 (MXMAT,IRLTYP,IMAT,SWAVE,SWV,PKRW,SSWV,HCAP,
     I                  NUMK,NUMP,
     O                  PKRAVE,DPKRAV)
          END IF
        END IF
C
C       Determine saturation, permeability, and moisture capacity
C       terms for previous iteration
C       Calculate head at element centroid for previous iteration
C
        HAVOLD= 0.5*(PCUR(I)+PCUR(IP1))
        IF (HAVOLD.GE.HCRIT) THEN
C
C         Set maximum values of water saturation, rel. permeability and
C         moisture capacity because critial value has been exceeded
C
          PKROLD= 1.
          SWOLD = 1.
          DSWOLD= 0.
        ELSE
C
C         Element was only partially saturated
C         Compute values of water saturation, rel. permeability and
C         moisture capacity by using functions or tables (old values)
C
          IF (KPROP.EQ.1) THEN
C
C           Using function
            SWOLD = SWFUN(MXMAT,HCRIT,HAVOLD,FVAL,IPROP(MM))
            CALL PKWFUN
     I                 (MXMAT,SWOLD,FVAL,IPROP(MM),
     O                  DPKROL,PKROLD)
            DSWOLD= DSWFUN(HCRIT,HAVOLD,HTOL,FVAL,IPROP(MM))
          ELSE
C
            IRLTYP=1
            CALL INTERP(
     I        MXMAT,IRLTYP,IMAT,HAVOLD,SWV,PKRW,SSWV,HCAP,
     I        NUMK,NUMP,
     O        SWOLD,DSWOLD)
C
C           Use saturation vs relative permeability table
            IRLTYP=2
            CALL INTERP(
     I        MXMAT,IRLTYP,IMAT,SWOLD,SWV,PKRW,SSWV,HCAP,
     I        NUMK,NUMP,
     O        PKROLD,DPKROL)
          END IF
        END IF
C
C       Compute change in water saturation for element between current
C       and previous iterations
C
        DELSW= SWAVE-SWOLD
C
C       Compute change in pressure head for element between current
C       and previous iterations
C
        DELH = HAVE-HAVOLD
C
C       For changes in head greater than tolerance, calcualte derivative
C       of water saturation w.r.t. head
C
        IF (DABS(DELH).GT.HTOL)  DSWAVE = DELSW/DELH
C
C       For changes in water saturation greater than one percent,
C       calculate derivative of permeability w.r.t. saturation
C
        IF (DABS(DELSW).GE.0.01) DPKRAV = (PKRAVE-PKROLD)/DELSW
C
C       Initialize effective water storage capacity and derivative array
C
        ETAND(I) = 0.
        DETAND(I)= 0.
C
C       Assign element values for water saturation to nodes
        SWEL(I)  = SWAVE
        PMXX     = PROP(IMAT,1)
C
C       Compute PKND and DPKND values as needed for solution of flow eq.
        PKND(I)  = PMXX*PKRAVE
        DPKND(I) = PMXX*DPKRAV*DSWAVE
C
        IF (ITRANS.EQ.1) THEN
C
C         For transient simulation find initial water saturation
          HAVINT=PINT(I)
          IF (HAVINT.GE.HCRIT) THEN
C
C           Set initial water sat. to max because 
C           critical value was exceeded
            SWINT=1.
          ELSE
C
C           Compute initial water saturation by using function or table
            IF (KPROP.EQ.1) THEN
C
C             Using function
              SWINT = SWFUN(MXMAT,HCRIT,HAVINT,FVAL,IPROP(MM))
            ELSE
C
C             Using table of pressure head vs saturation
C
              IRLTYP= 1
              CALL INTERP
     I                   (MXMAT,IRLTYP,IMAT,HAVINT,SWV,PKRW,SSWV,HCAP,
     I                    NUMK,NUMP,
     M                    SWINT,DSWINT)
            END IF
          END IF
C
C         Compute change in pressure head between initial value and
C         current iteration value
C
          DELH  = HAVE-HAVINT
C
C         For changes in head greater than tolerance, calculate 
C         derivative of water saturation w.r.t. head
C
          IF (DABS(DELH).GT.HTOL) DSWAVE=(SWAVE-SWINT)/DELH
C
C         Compute change in pressure head between initial value and
C         value of previous iteration
C
          DELHOL= HAVOLD-HAVINT
C
C         For calculated difference in head greater than tolerance,
C         calculate derivative of saturation w.r.t. head
C
          IF (DABS(DELHOL).GT.HTOL) DSWOLD=(SWOLD-SWINT)/DELHOL
          DH    = HAVE-HAVOLD
          DDSW  = 0.
C
C         Calculate second derivative of saturation w.r.t. head
C
          IF (DABS(DH).GT.HTOL) DDSW=(DSWAVE-DSWOLD)/DH
C
          POROS = PROP(IMAT,2)
          SS    = PROP(IMAT,3)
C
C         Compute effective water storage capacity values and derivative
C         for solution of flow equation as per Eq. 2-2A of text
C
          ETAND(I) = SS*SWAVE+POROS*DSWAVE
          DETAND(I)= SS*DSWAVE+POROS*DDSW
        END IF
 150  CONTINUE
C
C     Set up A, B, C, D arrays of the PICARD scheme
C
      NPM1= NP-1
      DO 200 I=2,NPM1
C
C       Initialize A, B, C, D arrays
C
        IM1  = I-1
        IP1  = I+1
        EL1  = EL(IM1)
        EL2  = EL(I)
        PK1  = PKND(IM1)
        PK2  = PKND(I)
        CONS1= PK1/EL1
        CONS2= PK2/EL2
        CMI  = (ETAND(I-1)*EL1+ETAND(I)*EL2)*0.5/TIN
        A(I) = -CONS1
        C(I) = -CONS2
        B(I) = CONS1+CONS2+CMI
        D(I) = CMI*PINT(I)+(PK1-PK2)*MARK
 200  CONTINUE
C
C     The following code assigns or calculates coefficient values for
C     first node dependent on soil surface boundary conditions
C     Node above soil surface does not exist
C
      A(1) = 0.
C
C     Node below bottom of column does not exist
C
      C(NP)= 0.
      EL1  = EL(1)
      ELND1= EL1*0.5
      PK1  = PKND(1)
      CONS1= PK1/EL1
      CM1  = ETAND(1)*ELND1/TIN
      BSTOR1 = CONS1+CM1
      CSTOR1 = -CONS1
      DSTOR1 = CM1*PINT(1)-PK1*MARK
      IF (IBTND1.EQ.1) THEN
C
C       1st type boundary condition (prescribed head) (first node)
C
        B(1)= 1.
        C(1)= 0.
        D(1)= VALND1
      ELSE
C
C       3rd type boundary condition (prescribed flux) (first node)
C
        B(1)=BSTOR1
        C(1)=CSTOR1
        D(1)=DSTOR1+VALND1
      END IF
C
C     The following code assigns or calculates coefficient values for
C     last node dependent on water table boundary conditions
C
      NPM1 = NP-1
      ELNE = EL(NPM1)
      ELNDN= 0.5*ELNE
      PKN  = PKND(NPM1)
      CONSN= PKN/ELNE
      CMN  = ETAND(NPM1)*ELNDN/TIN
      BSTORN= CONSN+CMN
      ASTORN= -CONSN
      DSTORN= CMN*PINT(NP)+PKN*MARK
C
      IF (IBTNDN.EQ.1) THEN
C
C       1st type boundary condition (prescribed head) (last node)
C
        B(NP)= 1.
        A(NP)= 0.
        D(NP)= VALNDN
      ELSE
C
C       3rd type boundary condition (prescribed flux) (last node)
C
        B(NP)= BSTORN
        A(NP)= ASTORN
        D(NP)= DSTORN+VALNDN
      END IF
C
      IF((IPRCHK .EQ. 2).AND.(ITER .LE. ITCHK).AND.(ECHOLV .GE. 6)) THEN
C
C       Print A, B, C, D arrays if debugging/print check wanted
C
        WRITE(OUTFL,2000)ITER
C
        ZERO=0.
        I=1
        WRITE(OUTFL,2010) I,A(I),B(I),C(I),D(I),ZERO,PKND(I),
     1                      ZERO,ETAND(I)
        DO 374 I=2,NPM1

          WRITE(OUTFL,2010) I,A(I),B(I),C(I),D(I),PKND(I-1),PKND(I),
     1                      ETAND(I-1),ETAND(I)
 374    CONTINUE
        I=NP
          WRITE(OUTFL,2010) I,A(I),B(I),C(I),D(I),PKND(I-1),ZERO,
     1                      ETAND(I-1),ZERO
      END IF
C
      IF (INEWT.GE.1) THEN
C
C       Compute additional arrays AN,BN,CN,DN for NEWTON-RAPHSON scheme
C
        DO 208 I=2,NPM1
C
C         Initialize AN, BN, CN, DN arrays
C
          IM1   = I-1
          IP1   = I+1
          EL1   = EL(IM1)
          EL2   = EL(I)
C
C         Compute elevation at element centroid
C
          ELNDI = 0.5*(EL1+EL2)
          GRAD1 = (DIS(I)-DIS(IM1))/EL1
          GRAD2 = (DIS(I)-DIS(IP1))/EL2
          DPKIM1= DPKND(IM1)
          DPKI  = DPKND(I)
          AN(I) = 0.5*DPKIM1*(-MARK+GRAD1)
          CN(I) = 0.5*DPKI*(MARK+GRAD2)
          BN(I) = AN(I)+CN(I)+ELNDI*DETAND(I)*(DIS(I)-PINT(I))/TIN
          DN(I) = AN(I)*DIS(IM1)+BN(I)*DIS(I)+CN(I)*DIS(IP1)
 208    CONTINUE
C
C       Assign or calculate coefficient values for first and last nodes
C       dependent on boundary conditions
C
        AN(1) = 0.
        BN(1) = 0.
        CN(1) = 0.
        DN(1) = 0.
        AN(NP)= 0.
        BN(NP)= 0.
        CN(NP)= 0.
        DN(NP)= 0.
        IF (IBTND1.EQ.1) THEN
C
C         1st type boundary condition (prescribed head) (first node)
C
          ELND1= 0.5*EL(1)
          GRAD2= (DIS(1)-DIS(2))/EL(1)
          DPK1 = DPKND(1)
          CN(1)= 0.5*DPK1*(MARK+GRAD2)
          BN(1)= CN(1)+ELND1*DETAND(1)*(DIS(1)-PINT(1))/TIN
          DN(1)= BN(1)*DIS(1)+CN(1)*DIS(2)
        END IF
C
        IF (IBTNDN.EQ.1) THEN
C
C         1st type boundary condition (prescribed head) (last node)
C
          ELNDN = 0.5*EL(NPM1)
          GRAD1 = (DIS(NP)-DIS(NPM1))/EL(NPM1)
          DPKNE = DPKND(NPM1)
          AN(NP)= 0.5*DPKNE*(-MARK+GRAD1)
          BN(NP)= AN(NP)+ELNDN*DETAND(NP)*(DIS(NP)-PINT(NP))/TIN
          DN(NP)= AN(NP)*DIS(NPM1)+BN(NP)*DIS(NP)
        END IF
C
        DO 408 I=1,NP
C
C         Set up A, B, C, D arrays of the modified NEWTON-RAPHSON scheme
C
          A(I) = A(I)+AN(I)
          B(I) = B(I)+BN(I)
          C(I) = C(I)+CN(I)
          D(I) = D(I)+DN(I)
 408    CONTINUE
C
        IF((IPRCHK.EQ.2).AND.(ITER.LE.ITCHK).AND.(ECHOLV.GE.6)) THEN
C
C         Print new & old values of AN, BN, CN, DN arrays
C
          WRITE(OUTFL,2050)
          WRITE(OUTFL,2060) (I,DIS(I),PCUR(I),I=1,NP)
          WRITE(OUTFL,2020)
          I=1
          WRITE(OUTFL,2040) I,AN(I),BN(I),CN(I),DN(I),
     1                      ZERO,DPKND(I),ZERO,DETAND(I)
          DO 474 I=2,NPM1
            WRITE(OUTFL,2040) I,AN(I),BN(I),CN(I),DN(I),
     1                        DPKND(I-1),DPKND(I),DETAND(I-1),DETAND(I)
 474      CONTINUE
          I=NP
          WRITE(OUTFL,2040) I,AN(I),BN(I),CN(I),DN(I),
     1                      DPKND(I),ZERO,DETAND(I),ZERO
        END IF
      END IF
C
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE TRIDIV(
     I  NP,A,B,C,D,
     O  T)
C
C     + + + PURPOSE + + +
C
C     Performs tridiagonal matrix solution
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXNOD.INC'
      INTEGER*4  NP
      REAL*8     A(MXNOD),B(MXNOD),C(MXNOD),D(MXNOD),
     1                   T(MXNOD)
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     NP     - TOTAL NUMBER OF NODAL POINTS
C     A      - 1ST DIAGONAL VECTOR OF THE TRIDIAGONAL MATRIX
C     B      - 2ND DIAGONAL VECTOR OF THE TRIDIAGONAL MATRIX
C     C      - 3RD DIAGONAL VECTOR OF THE TRIDIAGONAL MATRIX
C     D      - RIGHT HAND SIDE VECTOR
C     T      - WORKING ARRAY (SCRATCH ARRAY)
C
C     + + + PARAMETERS + + +
C
C      INCLUDE 'PMXNOD.INC'
       INCLUDE 'CONSTP.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      INTEGER*4    I,IM1,J,K,N
      REAL*8       BETA(MXNOD),GAM(MXNOD),RELTST
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + + 
C      
      EXTERNAL RELTST,SUBIN,SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'TRIDIV'
      CALL SUBIN(MESAGE)
C
      N      = NP
      BETA(1)= B(1)
      GAM(1) = D(1)/BETA(1)
C
C     Loop to number of nodal points setting up matrix parameters
C
      DO 10 I=2,N
        IM1    = I-1
        BETA(I)= B(I)-(A(I)*C(IM1))/BETA(IM1)
        IF (BETA(I) .LT. R0MIN) BETA(I) = 0.001
        GAM(I) = (D(I)-(A(I)*GAM(IM1)))/BETA(I)
C
          GAM(I)=RELTST(GAM(I))
C
 10   CONTINUE
C
      T(N)= GAM(N)
      K   = N-1
C
C     Loop to number of nodal points determining matrix values
C
      DO 20 I=1,K
        J   = N-I
        T(J)= GAM(J)-(C(J)*T(J+1))/BETA(J)
 20   CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE VSWCOM(
     I  TRNSIM,
     I  ITER,NE,NP,NVPR,MARK,NTOMT,
     I  DIS,EL,PKND,OUTFL,TAP10,ITMARK,ITMFC,
     I  TMCUR,TIMA,
     M  SWEL,
     O  VDAR,SWELPT,VDARPT,VDAR1O,VDARNO)
C
C     + + + PURPOSE + + +
C
C     Routine to compute nodal values of water saturation and Darcy
C     velocity
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXNOD.INC'
      LOGICAL   TRNSIM
      INTEGER*4 ITER,NE,NP,NVPR,MARK,OUTFL,TAP10,NTOMT,
     1          ITMARK,ITMFC
      REAL*8    DIS(MXNOD), EL(MXNOD),
     1          PKND(MXNOD),SWEL(MXNOD),VDAR(MXNOD),
     2          SWELPT(MXNOD),VDARPT(MXNOD),
     3          TMCUR,TIMA
C
C     + + + PARAMETERS + + +
C
C      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXTIM.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CWORKA.INC'
      INCLUDE 'CADISC.INC'
      INCLUDE 'CHYDR.INC'
C
C     + + + LOCAL VARIABLES + + + 
C     
      INTEGER*4 I,ND1,ND2,NPM1
      REAL*8    GRDX,HD1,HD2,PMXX
      REAL*8    SWTEMP(MXNOD),VDAR1,VDARN,VDAR1O,VDARNO,
     1          VDUM1,VDUMN,TMARK,TWF,TWFM1
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + + 
C      
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT(/5X,'ELEM. NO.,DARCY VELOCITY, AND SATURATION AT TIME =',
     1 E12.4/)
2010  FORMAT(4(I5,2X,E10.3,2X,E10.3))
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'VSWCOM'
      CALL SUBIN(MESAGE)
C
      NPM1=NP-1
C
C     Loop to determine element velocity (1-number of elements)
C
      DO 10 I=1,NE
        ND1 = I
        ND2 = I+1
        PMXX= PKND(I)
C
        IF (MARK.EQ.0) THEN
C
C         Horizontal flow
C
          HD1= DIS(ND1)
          HD2= DIS(ND2)
        ELSE
C
C         Vertical flow
C
          HD1= DIS(ND1)+CORD(NP)-CORD(ND1)
          HD2= DIS(ND2)+CORD(NP)-CORD(ND2)
        END IF
C
        GRDX   = (HD1-HD2)/EL(I)
        VDAR(I)= PMXX*GRDX
  10  CONTINUE
      VDAR1 = BSTOR1*DIS(1)+CSTOR1*DIS(2)-DSTOR1
      VDARN = ASTORN*DIS(NPM1)+BSTORN*DIS(NP)-DSTORN
C
      IF ((NVPR.GT.0) .AND. (ECHOLV .GE. 6)) THEN
C
C       We may want to print results
C
        IF (MOD(ITER,NVPR).EQ.0 .AND. MCOFLG .EQ. 0) THEN
C
C         Time to print results
C
          WRITE(OUTFL,2000) TMCUR
          WRITE(OUTFL,2010) (I,VDAR(I),SWEL(I),I=1,NPM1)
        END IF
      END IF
C
      IF (ITMARK.EQ.0) THEN
C
C       Output is written at each target simulation time value
C
        IF (TRNSIM) THEN
          WRITE(TAP10) TMCUR, VDAR1, VDARN
          WRITE(TAP10) (VDAR(I),I=1,NPM1)
          WRITE(TAP10) (SWEL(I),I=1,NPM1)
        ENDIF
      ELSE
C
C       Check the current marker time value
C
        IF (ITMFC.LE.NTOMT) THEN
          IF (ITER.EQ.1) THEN
            DO 35 I=1,NPM1
              VDARPT(I)=VDAR(I)
              SWELPT(I)=SWEL(I)
  35        CONTINUE
            VDAR1O=VDAR1
            VDARNO=VDARN
          END IF
          TMARK=TMFOMT(ITMFC)
          IF (TMARK.GE.TIMA.AND.TMARK.LE.TMCUR) THEN
C
C           Compute time weighting factor
C
            TWF=(TMARK-TIMA)/(TMCUR-TIMA)
            TWFM1=TWF-1.0
              VDUM1=TWF*VDAR1-TWFM1*VDAR1O
              VDUMN=TWF*VDARN-TWFM1*VDARNO
C
C           Write marker time value
C
            IF (TRNSIM) WRITE(TAP10) TMARK, VDUM1, VDUMN
            DO 40 I=1,NPM1
              SWTEMP(I)=TWF*VDAR(I)-TWFM1*VDARPT(I)
  40        CONTINUE
C
C           Write velocity at marker time value
C
            IF (TRNSIM) WRITE(TAP10)(SWTEMP(I),I=1,NPM1)
            DO 45 I=1,NPM1
              SWTEMP(I)=TWF*SWEL(I)-TWFM1*SWELPT(I)
  45        CONTINUE
C
C           Write water saturation at marker time value
C
            IF (TRNSIM) WRITE(TAP10)(SWTEMP(I),I=1,NPM1)
C
C           Update marker time index
C
            ITMFC=ITMFC+1
          END IF
C
C
            DO 55 I=1,NPM1
              VDARPT(I)=VDAR(I)
              SWELPT(I)=SWEL(I)
  55        CONTINUE
            VDAR1O=VDAR1
            VDARNO=VDARN
C
        END IF
      END IF
C
      CALL SUBOUT
      RETURN
      END
C
C
C
      DOUBLE PRECISION FUNCTION SWFUN(
     I  MXMAT,HCRIT,HAVE,FVAL,IMAT)
C
C     + + + PURPOSE + + +
C
C     Computes water saturation values for grid element
C     SWFUN is a function used by ASSEMF and DSWFUN. In DSWFUN, the
C     function is used once for each call. In ASSEMF, the function is
C     used once for each nodal point for each iteration of the flow eq.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER*4 MXMAT,IMAT
      REAL*8    HCRIT,HAVE,FVAL(MXMAT,5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     FVAL  - FUNCTIONAL COEFFICIENT VALS 
C     HCRIT - CRITICAL HEAD VALUE
C     HAVE  - AVERAGE HEAD VALUE
C     IMAT  - COUNTER USED IN LOOPING WITH RESPECT TO MATERIALS
C     MXMA  - MAXIMUM NUMBER OF MATERIALS ALLOWED
C
C     + + + LOCAL VARIABLES + + +
C
      REAL*8   ALPHA,BETA,GAMMA,PSI,SWMIN,SWR,TERM,SWMAX,DSWTOL
C
C     + + + FUNCTIONS + + +
C
      INTRINSIC   DABS
C
C     + + + END SPECIFICATIONS + + +
C
      SWMAX  = 0.999999E0
      DSWTOL = 1.E-4
      SWR    = FVAL(IMAT,1)
      ALPHA  = FVAL(IMAT,3)
      BETA   = FVAL(IMAT,4)
      GAMMA  = FVAL(IMAT,5)
C
C     Compute water saturation value for element
C
      PSI    = DABS(HAVE-HCRIT)
      TERM   = 1.+(ALPHA*PSI)**BETA
      SWFUN  = (1.-SWR)/(TERM**GAMMA)
C
C     Define minimum water saturation as residual water phase saturation
C     plus an arbitrary small number
C
      SWMIN  = SWR+DSWTOL
C
C     Define elemental water saturation as sum of calculated increment
C
      SWFUN  = SWFUN+SWR
      IF (SWFUN.LT.SWMIN) SWFUN=SWMIN
      IF (SWFUN.GT.SWMAX) SWFUN=SWMAX
C
      RETURN
      END
C
C
C
      SUBROUTINE PKWFUN(
     I  MXMAT,SWAVE,FVAL,IMAT,
     O  DPKRAV,PKWOUT)
C
C     + + + PURPOSE + + +
C
C     Routine to compute relative permeability function
C     PKWFUN is called by ASSEMF and makes no subroutine calls. This
C     subroutine is called once for each node for iteration of the
C     flow eq.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER*4 MXMAT,IMAT
      REAL*8    SWAVE,FVAL(MXMAT,5),DPKRAV,PKWOUT
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     DPKRAV - AVERAGE RELATIVE PERMEABILITY
C     FVAL   - FUNCTIONAL COEFFICIENT VALS 
C     IMAT   - COUNTER USED IN LOOPING WITH RESPECT TO MATERIALS
C     MXMAT  - MAXIMUM NUMBER OF MATERIALS ALLOWED
C     PKWOUT - RELATIVE PERMEABILITY COMPUTED USING FUNCTION
C     SWAVE  - AVERAGE WATER SATURATION
C
C     + + + LOCAL VARIABLES + + +
C
      REAL*8   EN,SWEFF,SWR,SWRM1,DSWTOL,SWE,GAMMA,GAMINV,PKWI,
     1         ACOEFF,BCOEFF,SWOLD,PKWOLD
C
C     + + + END SPECIFICATIONS + + +
C
      SWR   = FVAL(IMAT,1)
      SWRM1 = 1.-SWR
      SWEFF = SWAVE-SWR
      EN    = FVAL(IMAT,2)
      IF (EN.GE.1.E-10) THEN
C
C       Compute derivative of element relative permeability value
C       w.r.t. water saturation
C
        DPKRAV= (EN*SWEFF**(EN-1.))/(SWRM1**EN)
        SWEFF = SWEFF/SWRM1
        PKWOUT= SWEFF**EN
      ELSE
C
C       Use Van Genuchten relationship to compute element values for
C       relative permeability
C
        DSWTOL=1.E-4
        SWE   = SWEFF
        SWEFF = SWE/SWRM1
        GAMMA = FVAL(IMAT,5)
        GAMINV= 1./GAMMA
        PKWI  = SWEFF**0.5
        ACOEFF= (1.-SWEFF**GAMINV)**GAMMA
        BCOEFF= 1.-ACOEFF
        BCOEFF= BCOEFF*BCOEFF
        PKWI  = (SWEFF**0.5)*BCOEFF
        SWOLD = SWAVE-DSWTOL
        SWE   = SWOLD-SWR
        IF(SWE.LT.DSWTOL) THEN
          DSWTOL=-DSWTOL
          SWOLD=SWAVE-DSWTOL
          SWE=SWOLD-SWR
        END IF
        SWEFF = SWE/SWRM1
        PKWOLD= SWEFF**0.5
        ACOEFF= (1.-SWEFF**GAMINV)**GAMMA
        BCOEFF= 1.-ACOEFF
        BCOEFF= BCOEFF*BCOEFF
        PKWOLD= (SWEFF**0.5)*BCOEFF
        DPKRAV= (PKWI-PKWOLD)/DSWTOL
C
C       Calculate element value for relative permeability
C
        PKWOUT = PKWI
      END IF
C
      RETURN
      END
C
C
C
      DOUBLE PRECISION FUNCTION DSWFUN(
     I  HCRIT,HAVE,HTOL,FVAL,IMAT)
C
C     + + + PURPOSE + + +
C
C     Computes moisture capacity function for grid elements
C     DSWFUN is used by ASSEMF and makes no subroutine calls.
C     This function is used once for each node and estimates the
C     derivative of water saturation with respect to head. The
C     calculated term is used in the computation of effective water
C     storage capacity in ASSEMF.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXMAT.INC'
      INTEGER*4 IMAT
      REAL*8    HCRIT,HAVE,HTOL,FVAL(MXMAT,5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     HCRIT - CRITICAL HEAD VALUE
C     HAVE  - AVERAGE HEAD VALUE
C     HTOL  - HEAD TOLERANCE ALLOWED FOR NONLINEAR SOLUTION
C
C     + + + PARAMETERS + + +
C
C      INCLUDE 'PMXMAT.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CVNTR2.INC'
      INCLUDE 'CVNTR1.INC'
      INCLUDE 'CWORKN.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      REAL*8   HAVOLD,SWI,SWIOLD
C
C     + + + FUNCTIONS + + +
C
      REAL*8    SWFUN
C
C     + + + INTRINSICS + + + 
C      
      INTRINSIC  DABS
C
C     + + + EXTERNALS + + + 
C
      EXTERNAL SWFUN
C
C     + + + END SPECIFICATIONS + + +
C
      HAVOLD= HAVE- HTOL
C
C     Compute water saturation for element based on computed head
C     of current iteration
C
      SWI   = SWFUN(MXMAT,HCRIT,HAVE,FVAL,IMAT)
C
C     Compute water saturation for element based on computed head
C     of current iteration less head tolerance
C
      SWIOLD= SWFUN(MXMAT,HCRIT,HAVOLD,FVAL,IMAT)
C
C     Compute derivative of water saturation w.r.t. head
C
      DSWFUN= (SWI-SWIOLD)/HTOL
      DSWFUN= DABS(DSWFUN)
C
C     If water saturation values adjusted using subroutine CONVER,
C     apply adjustment factor to derivative
C
      IF(IMOD.EQ.1)DSWFUN=DSWFUN*CTRFAC(IMAT)
C
      RETURN
      END
C
C
C
      SUBROUTINE BALCHK(
     I  THETA,THETM1,EL,VDAR,DLAMND,
     *  OUTFL,TMVECX,BALSTO,SWEL,VDAR1,VDARN,SIMTYM,
     O  RTEMP,
     I  IPZONE,
     I  PARENT, NUMT, ICHEM, IBTND1 ,IBTNDN)
C
C     +  +  +  PURPOSE +  +  +
C     Performs mass balance calculations for flow and transport
C     BALCHK is called by VADCAL and makes no subroutine calls
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'CONSTP.INC'
      INTEGER*4 OUTFL,IPZONE,PARENT,NUMT,ICHEM,IBTND1,IBTNDN
      REAL      RTEMP(MXNOD)
      REAL*8    VDAR(MXNOD),EL(MXNOD),DLAMND(MXNOD),
     *          BALSTO(8),TMVECX,SWEL(MXNOD),RELTST,
     *          VDAR1,VDARN,THETA,THETM1
      CHARACTER*80 SIMTYM
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     OUTFL  - output file number for all output
C     IPZONE - current zone number
C     PARENT - number of parent chemical  
C     NUMT   - day of current monthly time step
C     ICHEM  - chemical number being simulated (1-3)
C     IBTND1 - type of boundary condition at top node
C     IBTNDN - type of boundary condition at bottom node
C     RTEMP  - temporary array
C     VDAR   - nodal darcy velocities
C     EL     - element length
C     DLAMND - nodal value of decay constant
C     BALSTO - array containing daily and cumulative fluxes
C     TMVECX - extra time value due to non-convergence
C     SWEL   - default value of water saturation for current material
C     RELTST - function to test for overflow and underflow
C     VDAR1  - darcy velocity at top node
C     VDARN  - darcy velocity at bottom node
C     THETA  - value of saturation at current time step
C     THETM1 - THETA -1
C
C     +  +  + PARAMETERS +  +  +
C
      INCLUDE 'PMXMAT.INC'
C      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CBSOLV.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CVNTR1.INC'
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CWORKA.INC'
      INCLUDE 'CVWRKM.INC'
      INCLUDE 'CWELEM.INC'
      INCLUDE 'CVCHMK.INC'
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CCUMUL.INC'
      INCLUDE 'CMCSTR.INC'
      INCLUDE 'CHYDR.INC'
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER*4    NPM1,I,IM1,IP1,IMAT1,IMAT2,IMAT
      REAL*8       DFLUX1,DFLUXN,DFLSUM,DFLSAB,QACCU,QACCAB,
     1             TERM,QERR,DENOM,QERRN,TCONS,QDECAY,W1D6,
     2             SFIM1,SFI,QTERM,HAVI,HAVIM1,HAVIP1,QDTERM,
     3             RET1,RET2,HMIDI,VDIFI,SF1,SFNPM1,HAV1,HAV2,
     4             HAVNM1,HAVNP,DLW1D6,QDECAB,RETN,HMID1,HMIDN,
     5             VGRAD1,VGRADN,ADD1,ADD2,QCHAIN,QCHAAB,EL1,EL2,
     6             FRACM,TERM1,TERM2,QCSUM,QCSUAB,SMIFLX,SMEFLX,
     7             ALFAL,POROS,DM,DEP,QACCJ,QACCM
      CHARACTER*80 MESAGE
C
C     +  +  + INTRINSICS +  +  +
C      
      INTRINSIC DABS,ABS,SNGL
C
C     +  +  + EXTERNALS +  +  +
C 
      EXTERNAL RELTST,SUBIN,SUBOUT
C
C     +  +  + OUTPUT FORMATS +  +  +
C
   2  FORMAT(//10X,'SUMMARY OF VOLUMETRIC FLOW BALANCE OUTPUT'/10X,
     1 41('-')//10X,'Elapsed simulation time = ',E12.4/
     2 10X,'Current value of time increment = ',E12.4/)
   3  FORMAT(/
     210X,'Fluid flux value at first node =',E12.4,4X/10X,
     3'Fluid flux value at last node =',E12.4//,10X,
     4'NOTE: These "net" values are for one day summary'/,
     510X,'Net value of fluid flux =',E12.4/10X,
     6'Net rate of volumetric storage =',E12.4/10X,
     7'FLOW BALANCE ERROR =',E12.4/10X,
     8'NORMALIZED BALANCE ERROR =',E12.4/)
  33  FORMAT(/,10X,
     1 'NOTE: These "cumulative" values are for one month summary'/,
     1 10X,'Cumulative volumetric storage =',E12.4/10X,
     2'Cumulative inflow volume =',E12.4/10X,
     3'Cumulative outflow volume =',E12.4//)
  34  FORMAT(//10X,'ANNUAL SUMMARY OF CUMULATIVE FLOW VALUES',/,10X,
     1             '------ ------- -- ---------- ---- ------',//,10X,
     2'NOTE: These "cumulative" values are for one year summary',/
     310X,'Annual cumulative volumetric storage =',E12.4/10X,
     4'Annual cumulative inflow volume =',E12.4/10X,
     5'Annual cumulative outflow volume =',E12.4//)
  13  FORMAT(/14X,'MASS TRANSPORT BALANCE, CHEMICAL',I2,1X,
     1       /14X,'---- --------- -------- -------- --'//,10X,
     2'Elapsed simulation time = ',E12.4/,
     310X,'Current value of time increment = ',E12.4//,10X,
     4'NOTE: These "net" values are for one day summary',/,
     510X,'Net dispersive flux =          ',G12.4/
     610X,'Net advective  flux =          ',G12.4/
     710X,'Net rate of mass accumulation =',G12.4/
     810X,'Net rate of formation =        ',G12.4/
     910X,'Net rate of mass decay =       ',G12.4/
     110X,'MASS BALANCE ERROR =           ',G12.4/
     210X,'NORMALIZED MASS BALANCE ERROR =',G12.4//)
 133  FORMAT(/10X,'..........FOR CHEMICAL',I2,/10X,
     1'NOTE: These "cumulative" values are for one month summary',/,
     210X,'Cumulative mass storage =      ',G12.4/
     310X,'Cumulative mass decay =        ',G12.4/
     410X,'Cumulative inflow mass =       ',G12.4/
     510X,'Cumulative outflow mass =      ',G12.4//)
 135  FORMAT(10X,'ANNUAL SUMMARY OF CUMULATIVE CONCENTRATIONS',
     1      /10X,'------ ------- -- ---------- --------------'//,
     910X,'..........FOR CHEMICAL',I2,/,
     210X,'NOTE: These "cumulative" values are for one year summary'/,
     310X,'Annual cumulative mass storage =      ',G12.4/
     410X,'Annual cumulative mass decay =        ',G12.4/
     510X,'Annual cumulative inflow mass =       ',G12.4/
     510X,'Annual cumulative outflow mass =      ',G12.4//)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'BALCHK'
      CALL SUBIN(MESAGE)
C
      NPM1=NP-1
C
C     Compute nodal flux (fluid flux if IMODL = 1
C     dispersive solute flux, if IMODL= 0)
C
      DFLUX1 = BSTOR1*DIS(1)+CSTOR1*DIS(2)-DSTOR1
      DFLUXN = ASTORN*DIS(NPM1)+BSTORN*DIS(NP)-DSTORN
      IF (IMODL.EQ.0) DFLUXN = -(DABS(DFLUXN))
      DFLSUM = DFLUX1+DFLUXN
      DFLSAB=DABS(DFLUX1)+DABS(DFLUXN)
C
C     Compute nodal values of solution increment
C
      DO 10 I=1,NP
        RTEMP(I) = DIS(I)-PINT(I)
C           IF (ABS(SNGL(DIS(I))-SNGL(PINT(I))).LT.R0MIN) THEN
C             RTEMP(I) = 0.0
C           ELSE
C             RTEMP(I) = SNGL(DIS(I))-SNGL(PINT(I))
C           ENDIF
  10  CONTINUE
C
C     Doing a flow simulation
      IF(IMODL.EQ.1) THEN
C
C     Compute rate of water volumetric storage
C
        QACCU = 0.
        QACCAB=0.
        DO 30 I=2,NPM1
          TERM = (ETAND(I-1)*EL(I-1)+ETAND(I)*EL(I))*0.5/TIN
          QACCU = QACCU + TERM*RTEMP(I)
          QACCAB= QACCAB + DABS(TERM*RTEMP(I))
  30    CONTINUE
        QACCU = QACCU+(ETAND(1)*EL(1)*RTEMP(1)+ETAND(NPM1)*EL(NPM1)
     1  *RTEMP(NP))*0.5/TIN
        QACCAB = QACCAB+(DABS(ETAND(1)*EL(1)*RTEMP(1))+DABS(ETAND(NPM1)*
     1           EL(NPM1)*RTEMP(NP)))*0.5/TIN
        QERR=DFLSUM-QACCU
        DENOM = DFLSAB+QACCAB
        IF (DENOM.LE.0.0) DENOM = 1.0
        QERRN=QERR/DENOM
        TMACCW=TMACCW+QACCU*TIN
        CUWVIF=CUWVIF+DFLUX1*TIN
        CUWVEF=CUWVEF+DFLUXN*TIN
C
C     Doing a transport simulation   
      ELSE
        TCONS=1./(6.*TIN)
C
C      Set TCONS = 0.0 for steady state transport
C
        IF(ITRANS.EQ.0) TCONS=0.0
C
C       Compute storage, decay, and daughter formation 
        QACCU=0.
        QACCJ=0.
        QACCM=0.
        QACCAB=0.
        QDECAY=0.
        W1D6=1./6.
        DO 35 I=2,NPM1
          IM1=I-1
          IP1=I+1
          SFIM1=ETAND(IM1)*EL(IM1)
          SFI=ETAND(I)*EL(I)
          QTERM=SFIM1*RTEMP(IM1)+2.*(SFIM1+SFI)*RTEMP(I)+SFI*RTEMP(IP1)
          QACCU=QACCU+QTERM*TCONS
          QACCAB=QACCAB+DABS(QTERM*TCONS)
          HAVI=THETA*DIS(I)-THETM1*PINT(I)
          HAVIM1=THETA*DIS(IM1)-THETM1*PINT(IM1)
          HAVIP1=THETA*DIS(IP1)-THETM1*PINT(IP1)
          QDTERM=W1D6*(DLAMND(I-1)*SFIM1*HAVIM1+2.*(DLAMND(I-1)*SFIM1+
     1           DLAMND(I)*SFI)*HAVI+DLAMND(I)*SFI*HAVIP1)
          QDECAY=QDECAY+QDTERM
C
C         Store decay of this chemical for transformation to daughters
C         from intermidiate nodes
          IF ((ICHAIN .EQ. 1) .AND. (ICHEM .LE. 2)) THEN
C
C           Only dissolved component is transformed to daughter
C
            IMAT1 = IPROP(I-1)
            IMAT2 = IPROP(I)
            RET1=1.0+(PROP(IMAT1,3)-1.0)/SWEL(I-1)
            RET2=1.0+(PROP(IMAT2,3)-1.0)/SWEL(I)
            QDTERM=W1D6*(DLAMND(I-1)*SFIM1*HAVIM1/RET1+
     1           2.*(DLAMND(I-1)*SFIM1/RET1+DLAMND(I)*
     2           SFI/RET2)*HAVI+DLAMND(I)*SFI*HAVIP1/RET2)
            TRTERM(I,NUMT,ICHEM) = TRTERM(I,NUMT,ICHEM) + QDTERM
          ENDIF
          HMIDI=THETA*DIS(I)-THETM1*PINT(I)
          VDIFI=VDAR(I)-VDAR(I-1)
          QACCU=QACCU-VDIFI*HMIDI
          QACCAB=QACCAB+DABS(VDIFI*HMIDI)
          QACCU=RELTST(QACCU)
 35     CONTINUE
C
C       Top and bottom node computations
        SF1=ETAND(1)*EL(1)
        SFNPM1=ETAND(NPM1)*EL(NPM1)
        QACCU=QACCU+(2.*SF1*RTEMP(1)+SF1*RTEMP(2))*TCONS
        QACCAB=QACCAB+DABS((2.*SF1*RTEMP(1)+SF1*RTEMP(2))*TCONS)
        QACCU=QACCU+(SFNPM1*RTEMP(NPM1)+2.*SFNPM1*RTEMP(NP))*TCONS
        QACCAB=QACCAB+DABS((SFNPM1*RTEMP(NPM1)+2.*SFNPM1*RTEMP(NP))*
     1         TCONS)
        HAV1=THETA*DIS(1)-THETM1*PINT(1)
        HAV2=THETA*DIS(2)-THETM1*PINT(2)
        HAVNM1=THETA*DIS(NPM1)-THETM1*PINT(NPM1)
        HAVNP=THETA*DIS(NP)-THETM1*PINT(NP)
        DLW1D6=DLAMND(1)*W1D6
        QDTERM = DLW1D6 * (2.*SF1*HAV1 + SF1*HAV2)
        QDECAY = QDECAY + QDTERM
C
C       Store decay of this chemical for transformation to daughters
C       for top node
        IF ((ICHAIN .EQ. 1) .AND. (ICHEM .LE. 2)) THEN
C
C         Only dissolved component is transformed to daughter
C
          IMAT = IPROP(1)
          RET1 = 1.0 + (PROP(IMAT,3) - 1.0)/SWEL(1)
          QDTERM = QDTERM/RET1
          TRTERM(1,NUMT,ICHEM) = TRTERM(1,NUMT,ICHEM) + QDTERM
        ENDIF
        DLW1D6=DLAMND(NP)*W1D6
        QDTERM = DLW1D6 * (SFNPM1*HAVNM1 + 2.*SFNPM1*HAVNP)
        QDECAY = QDECAY + QDTERM
        QDECAB = DABS(QDECAY)
C
C       Store decay of this chemical for transformation to daughters
C       for bottom node
        IF ((ICHAIN .EQ. 1) .AND. (ICHEM .LE. 2)) THEN
C
C         Only dissolved component is transformed to daughter
C
          IMAT = IPROP(NPM1)
          RETN = 1.0 + (PROP(IMAT,3) - 1.0)/SWEL(NPM1)
          QDTERM = QDTERM/RETN
          TRTERM(NP,NUMT,ICHEM) = TRTERM(NP,NUMT,ICHEM) + QDTERM
        ENDIF
C
        HMID1=THETA*DIS(1)-THETM1*PINT(1)
        HMIDN=THETA*DIS(NP)-THETM1*PINT(NP)
        VGRAD1= (VDAR(1)-VDAR1)
        VGRADN= (VDARN-VDAR(NPM1))
        ADD1=0.0
        ADD2=0.0
        IF (IBTND1.EQ.0) ADD1=-VGRAD1*HMID1
        IF (IBTNDN.EQ.0) ADD2=-VGRADN*HMIDN
        QACCU=QACCU+ADD1+ADD2    
        QACCAB = QACCAB + DABS(ADD1) + DABS(ADD2)
        QCHAIN = 0.0
        QCHAAB = 0.0
C
        IF ((ICHAIN.EQ.1) .AND. (PARENT .NE. 0)) THEN
C
C         Account for accumulation by decay of parent compound
C
          IMAT = IPROP(1)
          TERM = FRACMP(IMAT) * TRTERM(1,NUMT,PARENT)
          QCHAIN = QCHAIN + TERM
          DO 36 I = 2, NPM1
            IMAT1 = IPROP(I-1)
            IMAT2 = IPROP(I)
            EL1 = EL(I-1)
            EL2 = EL(I)
            FRACM = (FRACMP(IMAT1)*EL1+FRACMP(IMAT2)*EL2)/(EL1+EL2)
            TERM = FRACM*TRTERM(I,NUMT,PARENT)
            QCHAIN = QCHAIN + TERM
 36       CONTINUE
          IMAT = IPROP(NPM1)
          TERM = FRACMP(IMAT) * TRTERM(NP,NUMT,PARENT)
          QCHAIN = QCHAIN + TERM
          QCHAAB = DABS(QCHAIN)
        ENDIF
C
C       Compute advective solute flux
C
        TERM1=HMID1*VDAR(1)
        TERM2=HMIDN*VDAR(NPM1)
        IF (IBTND1.EQ.0) TERM1=HMID1*VDAR1
        IF (IBTNDN.EQ.0) TERM2=HMIDN*VDARN
        QCSUM=TERM1-TERM2
        QCSUAB=DABS(TERM1)+DABS(TERM2)
        QERR = DFLSUM + QCSUM - QACCU - QDECAY + QCHAIN
        DENOM = DFLSAB + QCSUAB + QACCAB + QDECAB + QCHAAB
        IF(DENOM.LE.0.0) DENOM = 1.0
        QERRN = QERR/DENOM
        QACCJ = (DFLUXN - (DABS(TERM2))*2.)
        QACCM = QACCU+QACCJ
        TMACCP=TMACCP+QACCM*TIN
        TMDCYV=TMDCYV+QDECAY*TIN
C
C       Store chemical fluxes for Monte Carlo
C
        ADVMC(ICHEM,IPZONE) = QCSUM
        DISMC(ICHEM,IPZONE) = DFLSUM
        DKMC(ICHEM,IPZONE) = QDECAY
C
      END IF
C
      IF(IMODL.EQ.1) THEN
        BALSTO(1)=DFLUX1
        BALSTO(2)=DFLUXN
        BALSTO(3)=DFLSUM
        BALSTO(4)=QACCU
        BALSTO(5)=QERR
        BALSTO(6)=TMACCW
        BALSTO(7)=CUWVIF
        BALSTO(8)=CUWVEF
C
C       these values for annual water summary
C       in order outflow,inflow,storage
C
        IF (NUMT .EQ. NTS ) THEN
          ANNWEF(IPZONE) = ANNWEF(IPZONE) + CUWVEF
          ANNWIF(IPZONE) = ANNWIF(IPZONE) + CUWVIF
          ANNCCW(IPZONE) = ANNCCW(IPZONE) + TMACCW
        ENDIF
C
C       Store water fluxes for Monte Carlo
C
        WFLXMC = DFLSUM
C
C       next code for printing daily,cumulative, and yearly flow
        IF (OUTF .EQ.' DAY' .AND. ECHOLV .GE. 4) THEN
          IF (MCOFLG .EQ. 0) THEN
            WRITE(OUTFL,2) TMVECX,TIN
            IF(OUTF .EQ. ' DAY') THEN
              WRITE(OUTFL,3) (BALSTO(I),I=1,5),QERRN
              IF(NUMT .EQ. NTS) THEN
                WRITE(OUTFL,33) (BALSTO(I),I=6,8)
                IF(SIMTYM(17:19).EQ.'Dec') THEN
                  WRITE(OUTFL,34) ANNCCW(IPZONE),ANNWIF(IPZONE),
     1                            ANNWEF(IPZONE)
                  ANNCCW(IPZONE) = 0.0
                  ANNWIF(IPZONE) = 0.0
                  ANNWEF(IPZONE) = 0.0
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF(OUTF .EQ. 'MNTH' .AND. NUMT .EQ. NTS) THEN
          IF (MCOFLG .EQ. 0) THEN
            WRITE(OUTFL,33) (BALSTO(I),I=6,8)
            IF(SIMTYM(17:19).EQ.'Dec') THEN
              WRITE(OUTFL,34) ANNCCW(IPZONE),ANNWIF(IPZONE),
     1                        ANNWEF(IPZONE)
              ANNCCW(IPZONE) = 0.0
              ANNWIF(IPZONE) = 0.0
              ANNWEF(IPZONE) = 0.0
            ENDIF
          ENDIF
        ENDIF
        IF(OUTF .EQ. 'YEAR' .AND. SIMTYM(17:19).EQ.'Dec') THEN
          IF(NUMT .EQ. NTS .AND. MCOFLG .EQ. 0) THEN
            WRITE(OUTFL,34) ANNCCW(IPZONE),ANNWIF(IPZONE),ANNWEF(IPZONE)
            ANNCCW(IPZONE) = 0.0
            ANNWIF(IPZONE) = 0.0
            ANNWEF(IPZONE) = 0.0
          ENDIF
        ENDIF
      ELSE
C
C     Compute nodal values of advective-dispersive flux
C
      SMIFLX = DFLUX1+TERM1
      SMEFLX = DFLUXN-(DABS(TERM2))
      CUSMIF = CUSMIF+SMIFLX*TIN
      CUSMEF = CUSMEF+SMEFLX*TIN
      RTEMP(1) = (DABS(SMIFLX))
      RTEMP(NP) = (DABS(SMEFLX))
      DO 45 I=2,NPM1
        IMAT=IPROP(I)
        IP1=I+1
        IM1=I-1
        ALFAL = PROP(IMAT,1)
        POROS = PROP(IMAT,2)
        DM = PROP(IMAT,4)
        DEP = ALFAL*0.5*(VDAR(I)+VDAR(IP1))+DM*POROS
        RTEMP(I)=(VDAR(I)*DIS(I)+DEP*(DIS(IM1)-DIS(I))/EL(IM1))
  45  CONTINUE
C
C     These values for annual transport summary
C     in order outflow,decay,inflow,storage
C
      IF (NUMT .EQ. NTS) THEN
        ANNMEF(IPZONE,ICHEM) = ANNMEF(IPZONE,ICHEM) + CUSMEF
        ANNCYV(IPZONE,ICHEM) = ANNCYV(IPZONE,ICHEM) + TMDCYV
        ANNMIF(IPZONE,ICHEM) = ANNMIF(IPZONE,ICHEM) + CUSMIF
        ANNCCP(IPZONE,ICHEM) = ANNCCP(IPZONE,ICHEM) + TMACCP
      ENDIF
C
C      Print the summary of mass balance for the transport simulation
C
        IF (OUTT .EQ. ' DAY' .AND. ECHOLV .GE. 4) THEN
          IF (MCOFLG .EQ. 0) THEN
            WRITE(OUTFL,13) ICHEM,TMVECX,TIN,DFLSUM,QCSUM,QACCU,QCHAIN,
     1                      QDECAY,QERR,QERRN
            IF(NUMT .EQ. NTS) THEN
              WRITE(OUTFL,133) ICHEM,TMACCP,TMDCYV,
     1                         CUSMIF,CUSMEF
              IF(SIMTYM(17:19).EQ.'Dec') THEN
                WRITE(OUTFL,135) ICHEM,ANNCCP(IPZONE,ICHEM),
     1                           ANNCYV(IPZONE,ICHEM),
     2                           ANNMIF(IPZONE,ICHEM),
     3                           ANNMEF(IPZONE,ICHEM)
                ANNMEF(IPZONE,ICHEM) = 0.0
                ANNCYV(IPZONE,ICHEM) = 0.0
                ANNMIF(IPZONE,ICHEM) = 0.0
                ANNCCP(IPZONE,ICHEM) = 0.0
              ENDIF
            ENDIF  
          ENDIF
        ENDIF
        IF (OUTT .EQ. 'MNTH' .AND. NUMT .EQ. NTS) THEN
          IF (MCOFLG .EQ. 0) THEN
            WRITE(OUTFL,133) ICHEM,TMACCP,TMDCYV,
     1                       CUSMIF,CUSMEF
            IF(SIMTYM(17:19).EQ.'Dec') THEN
              WRITE(OUTFL,135) ICHEM,ANNCCP(IPZONE,ICHEM),
     1                         ANNCYV(IPZONE,ICHEM),
     2                         ANNMIF(IPZONE,ICHEM),
     3                       ANNMEF(IPZONE,ICHEM)
              ANNMEF(IPZONE,ICHEM) = 0.0
              ANNCYV(IPZONE,ICHEM) = 0.0
              ANNMIF(IPZONE,ICHEM) = 0.0
              ANNCCP(IPZONE,ICHEM) = 0.0
            ENDIF
          ENDIF
        ENDIF
        IF (OUTT .EQ. 'YEAR' .AND. SIMTYM(17:19).EQ.'Dec') THEN
          IF (NUMT .EQ. NTS .AND. MCOFLG .EQ. 0) THEN
            WRITE(OUTFL,135) ICHEM,ANNCCP(IPZONE,ICHEM),
     1                       ANNCYV(IPZONE,ICHEM),
     2                       ANNMIF(IPZONE,ICHEM),
     3                       ANNMEF(IPZONE,ICHEM)
            ANNMEF(IPZONE,ICHEM) = 0.0
            ANNCYV(IPZONE,ICHEM) = 0.0
            ANNMIF(IPZONE,ICHEM) = 0.0
            ANNCCP(IPZONE,ICHEM) = 0.0
          ENDIF
        ENDIF
      ENDIF
C
C
      CALL SUBOUT
      RETURN
      END

