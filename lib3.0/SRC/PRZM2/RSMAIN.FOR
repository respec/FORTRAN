C
C
C
      SUBROUTINE   PRZM2
C
C     + + + PURPOSE + + +
C     main program module for PRZM-2
C     Modification date: 2/14/92 JAM
C
C     + + + PARAMETERS + + +
      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXZON.INC'
      INCLUDE 'PMXNSZ.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CONSTP.INC'
      INCLUDE 'CMCRVR.INC'
      INCLUDE 'CFILEX.INC'
      INCLUDE 'CVMISC.INC'
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL      MCARLO, PRZMON, YRSTEP, VADFON, TRNSIM, FLOSIM,
     $             SEPTON, NITRON
      INTEGER      ISDAY, ISMON, ISTYR, IEDAY, IEMON, IEYR,
     1             NTSAFT, IRUN, NVZONE,
     2             NPZONE, NLDLT, NCHEM, IPARNT(3),
     3             SRNFG, IDNODE(MXZONE),
     4             LLSTS, NSEED, LMXZON, LNCMP2
      REAL         VERSON, CORDND(MXZONE,NCMPP2)
      CHARACTER*3  MODID(NMXFIL)
      CHARACTER*80 MESAGE
C
C     + + + EXTERNAL + + +
      EXTERNAL     INITEM, READM, INITMC, RANDOM, INPREA, PZSCRN,
     1             PZDSPL, EXESUP, STATIS, DONBAR, OUTPUT,
     2             FILCLO, FILINI, INIDAT, INIT
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT ('Monte Carlo simulation [',I5,' ]')
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize common const
      CALL INIT
C     initialize common cmisc
      CALL INIDAT
C     initialize file unit info.
      CALL FILINI
C
      ECHOLV    = 5
      VERSON    = 2.00
      MODID(1)  = '???'
      MODID(2)  = '???'
      MODID(3)  = 'WTR'
      MODID(4)  = 'PST'
      MODID(5)  = 'TSR'
      MODID(6)  = 'CNC'
      MODID(7)  = 'OUT'
      MODID(8)  = 'SNS'
      MODID(10) = 'IRG'
      MODID(11) = 'RST'
      MODID(12) = 'NCO'
      MODID(13) = 'NIT'
C
C     read supervisor file and determine options
      CALL INITEM(
     I            VERSON,
     O            MCARLO,PRZMON,YRSTEP,VADFON,TRNSIM,NVZONE,
     O            ISDAY,ISMON,ISTYR,IEDAY,IEMON,IEYR,
     O            NLDLT,NTSAFT,LLSTS,NPZONE,
     O            NCHEM,IPARNT,SEPTON,NITRON)
C
      IF (MCARLO) THEN
C       Monte Carlo on, first input
        CALL READM(
     I             FMCIN,FMCOUT,FMCOU2,MCMAX,NMAX,NEMP,
     I             NRMAX,NCMAX,NPMAX,
     M             LARR,
     O             VAR,SNAME,NDAT,MCVAR,BBTRNS,PNAME,NVAR,
     O             NRUNS,DIST,PALPH,IND1,NAVG,INDZ)
C
        NSEED = 123999457
C       Monte Carlo initialization
        CALL INITMC(
     I              MCMAX,NVAR,NMAX,BBTRNS,MCVAR,
     M              STAT,CORR,
     O              DECOM)
      ELSE
C       single run
        NRUNS = 1
      END IF
C
      DO 100 IRUN= 1,NRUNS
C       Monte Carlo loop
        IF (MCARLO) THEN
C         generate random numbers
          CALL RANDOM(
     I                VAR,MCVAR,MCMAX,NEMP,NDAT,DIST,
     O                RMC,DECOM,
     M                CNMC,NSEED)
        END IF
C
C       read data from PRZM and VADOFT
        LMXZON = MXZONE
        LNCMP2 = NCMPP2
        CALL INPREA(
     I              MCARLO,PRZMON,VADFON,SEPTON,NITRON,
     I              NCHEM,NLDLT,TRNSIM,FLOSIM,NPZONE,NVZONE,
     I              ISDAY,ISMON,ISTYR,IEDAY,IEMON,IEYR,
     I              LMXZON,LNCMP2,
     I              MODID(3),IRUN,
     O              CORDND,SRNFG,IDNODE)
C
        IF (MCARLO .AND. (ECHOLV .GT. 0)) THEN
C         display Monte Carlo to screen
          WRITE(MESAGE,2000) IRUN
          IF (ECHOLV .EQ. 1) THEN
C           status screen update only
            ECHOLV = 2
            CALL PZSCRN(1,MESAGE)
            ECHOLV = 1
          ELSE
C           status screen and status file
            CALL PZDSPL(FECHO,MESAGE)
          ENDIF
        ENDIF
C
C       simulation begins after all options have been determined
        CALL EXESUP(
     I              MCARLO,PRZMON,YRSTEP,VADFON,TRNSIM,
     I              SEPTON,NITRON,SRNFG,IDNODE,MODID,
     I              NPZONE,NVZONE,
     I              ISDAY,ISMON,ISTYR,
     I              NLDLT,NTSAFT,LLSTS,
     I              NCHEM,IPARNT,IRUN,NRUNS)
C
        IF (MCARLO) THEN
C         calculate Monte Carlo statistical summations:
          CALL STATIS(
     I                NVAR,NMAX,XMC,SNAME,IRUN,NRMAX,NCMAX,FMCOU2,
     M                XCDF,STAT,CORR)
C
C         donbar displays percent of simulation complete to screen
          CALL DONBAR(1,NRUNS,IRUN)
        ENDIF
C
C       end Monte Carlo loop:
 100  CONTINUE
C
      IF (MCARLO) THEN
C       Monte Carlo output:
        CALL OUTPUT(
     I              NVAR,NMAX,NRUNS,NRMAX,NCMAX,FMCOUT,SNAME,IND1,NAVG,
     I              STAT,CORR,XCDF,PALPH,INDZ)
      END IF
C
C     clean up, display to user that program completed sucessfully
      MESAGE = 'Normal completion'
      CALL PZDSPL(FECHO,MESAGE)
      MESAGE = 'Returning to PATRIOT system'
      CALL PZSCRN(7,MESAGE)
C
C     close files
      CALL FILCLO
C
      RETURN
      END

