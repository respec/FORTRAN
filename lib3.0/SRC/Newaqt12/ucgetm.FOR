C
C
C
      SUBROUTINE   UCGGEN
     O                    (OOUTLE,ORESMF,ORUNFG,OSPOUT,OUNIT,
     O                     OSDATI,OEDATI,ORNINF,OWDMFL,OSCEN,OWNAME)
C
C     + + + PURPOSE + + +
C     get general info from common for making a uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       OOUTLE,ORESMF,ORUNFG,OSPOUT,
     1              OSDATI(5),OEDATI(5),OUNIT,OWDMFL
      CHARACTER*8   OSCEN
      CHARACTER*64  OWNAME
      CHARACTER*80  ORNINF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OOUTLE - hspf output level
C     ORESMF - resume flag
C     ORUNFG - run flag
C     OSPOUT - special actions output level
C     OUNIT  - unit system
C     OSDATI - start date
C     OEDATI - end date
C     ORNINF - run info
C     OWDMFL - wdm file unit number
C     OSCEN  - new scenario name
C     OWNAME - wdm file name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      OOUTLE = OUTLEV
      ORESMF = RESMFG
      ORUNFG = RUNFG
      OSPOUT = SPOUT
      DO 10 I = 1,5
        OSDATI(I) = SDATIM(I)
        OEDATI(I) = EDATIM(I)
 10   CONTINUE
      OUNIT  = UNIT
      ORNINF = RNINFO
      OWDMFL = WDMSFL
      OSCEN  = CSCEN
      OWNAME = WDMNAM
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGNEX
     O                   (ONUMEX)
C
C     + + + PURPOSE + + +
C     get number of external source flow records from common
C     * default values filled in GISREA *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ONUMEX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ONUMEX - number of external source flows
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ONUMEX = NUMEXT
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGEXT
     I                   (INUMEX,
     O                    OEXTSR,ORAEXT,OCEXTS)
C
C     + + + PURPOSE + + +
C     get external source flow info from common
C     * default values filled in GISREA *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INUMEX,OEXTSR(INUMEX,2)
      REAL          ORAEXT(INUMEX)
      CHARACTER*8   OCEXTS(INUMEX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUMEX - number of external source flows
C     OEXTSR - array of data set number and reach it connects to
C     ORAEXT - array of areas of each external source
C     OCEXTS - array of names of exernal source locations
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NUMEXT
        OEXTSR(I,1) = EXTSRC(I,1)
        OEXTSR(I,2) = EXTSRC(I,2)
        OCEXTS(I)   = CEXTSR(I)
        ORAEXT(I)   = RAEXT(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGNLI
     I                    (ONDSNI)
C
C     + + + PURPOSE + + +
C     get number of local inflow candidates to common
C     * default values can be filled using UCIDSN *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ONDSNI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ONDSNI - number of local inflow data set candidates
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ONDSNI = NDSNIN
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGLIN
     I                    (INDSNI,
     O                     ODSNIN,OAREAI)
C
C     + + + PURPOSE + + +
C     get local inflow candidate info from common
C     * default values can be filled using UCIDSN *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INDSNI,ODSNIN(INDSNI)
      REAL          OAREAI(INDSNI)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDSNI - number of local inflow data set candidates
C     ODSNIN - array of data set number for each local inflow candidates
C     OAREAI - array of areas of each local inflow candidate
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NDSNIN
        ODSNIN(I) = DSNIN(I)
        OAREAI(I) = AREAIN(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGOUT
     I                    (INR,
     O                     OOUT)
C
C     + + + PURPOSE + + +
C     put output site info to common
C     * default values can be filled using UCIDSN *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INR,OOUT(INR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INR    - number of reaches
C     OOUT   - flag indicating if output is desired here (-999 no, otherwise
C              base data set for numbering output data sets)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,INR
        OOUT(I) = OUTSIT(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGNRC
     O                    (ONR)
C
C     + + + PURPOSE + + +
C     get number of reaches from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ONR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ONR    - number of reaches
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ONR = NREACH
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGRCH
     I                    (INR,
     O                     OCRCH,ORCHME,OLEN,ODELTH,OUPELE,ODNELE,
     O                     OUPLAT,OUPLON,ODNLAT,ODNLON)
C
C     + + + PURPOSE + + +
C     get reach table info from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INR,ORCHME(INR)
      REAL          OLEN(INR),ODELTH(INR),OUPELE(INR),ODNELE(INR),
     1              OUPLAT(INR),OUPLON(INR),ODNLAT(INR),ODNLON(INR)
      CHARACTER*12  OCRCH(INR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INR    - number of reaches
C     OCRCH  - reach name
C     ORCHME - array of met segments assoc with each reach
C     OLEN   - (hspf parm) length of each reach
C     ODELTH - (hspf parm) diff in elevation for each reach
C     OUPELE - elevation at upstream end of each reach
C     ODNELE - elevation at downstream end of each reach
C     OUPLAT - latitude of upstream end of each reach
C     OUPLON - longitude of upstream end of each reach
C     ODNLAT - latitude of downstream end of each reach
C     ODNLON - longitude of downstream end of each reach
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NREACH
        OCRCH(I)  = CRCH(I)
        ORCHME(I) = RCHMET(I)
        OLEN(I)   = LEN(I)
        ODELTH(I) = DELTH(I)
        OUPELE(I) = UPELEV(I)
        ODNELE(I) = DNELEV(I)
        OUPLAT(I) = UPLAT(I)
        OUPLON(I) = UPLON(I)
        ODNLAT(I) = DNLAT(I)
        ODNLON(I) = DNLON(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGIRC
     I                    (IR,
     O                     OCRCH)
C
C     + + + PURPOSE + + +
C     get name of this reach
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IR
      CHARACTER*12  OCRCH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IR     - reach number
C     OCRCH  - reach name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      OCRCH = CRCH(IR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGNCO
     O                    (ONRC)
C
C     + + + PURPOSE + + +
C     get number of reach connections from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ONRC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ONRC   - number of reach to reach connections
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ONRC = NRCONN
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGCON
     I                    (INRC,
     O                     OR2R)
C
C     + + + PURPOSE + + +
C     put reach connection table info to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INRC,OR2R(INRC,2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INRC   - number of reach to reach connections
C     OR2R   - array of reach to reach source/target pairs
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NRCONN
        OR2R(I,1) = R2RCON(I,1)
        OR2R(I,2) = R2RCON(I,2)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGNLA
     O                    (ONLU)
C
C     + + + PURPOSE + + +
C     get number of land uses from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ONLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ONLU   - number of land uses
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ONLU = NLANDU
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGLAN
     I                    (INLU,
     O                     OCLAND,OLANDP)
C
C     + + + PURPOSE + + +
C     get land use table info from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INLU,OLANDP(INLU)
      CHARACTER*12  OCLAND(INLU)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INLU   - number of land uses
C     OCLAND - array of land use names
C     OLANDP - array of type of each land use 1-perv, 2-imperv
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NLANDU
        OCLAND(I) = CLAND(I)
        OLANDP(I) = LANDPI(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGARE
     I                    (INLU,INR,
     O                     OAREA)
C
C     + + + PURPOSE + + +
C     get area table info from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INLU,INR
      REAL          OAREA(INLU,INR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INLU   - number of land uses
C     INR    - number of reaches
C     OAREA  - array of areas of each land use contributing to each reach
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 20 J = 1,INR
        DO 10 I = 1,INLU
          OAREA(I,J) = AREALR(I,J)
 10     CONTINUE
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGNME
     O                    (ONMET,ONEVAP)
C
C     + + + PURPOSE + + +
C     get number of met stations and evap stations from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ONMET,ONEVAP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ONMET  - number of met segments
C     ONEVAP - number of evap data sets
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ONMET = NMETSG
      ONEVAP= NEVAP
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCGMET
     I                    (INMET,INEVAP,
     O                     OPE,OCMETS)
C
C     + + + PURPOSE + + +
C     get met table info from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INMET,INEVAP,OPE(INMET+INEVAP)
      CHARACTER*8   OCMETS(INMET+INEVAP)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INMET  - number of met segments
C     INEVAP - number of evap data sets
C     OPE    - array of flags indicating if this is a precip or evap segment
C     OCMETS - array of met station names
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,INMET+INEVAP
        OPE(I)    = PRCEVP(I)
        OCMETS(I) = CMETS(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE UCGNRA
     O                  (ONRAT)
C
C     + + + PURPOSE + + +
C     get number of rating curves from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  ONRAT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ONRAT  - number of rating curves
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ONRAT = NUMRAT
C
      RETURN
      END
C
C
C
      SUBROUTINE UCGRAT
     I                  (IMXROW,INRAT,
     O                   ONUMR,OAREA,ORAT)
C
C     + + + PURPOSE + + +
C     get rating curve info from common block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  IMXROW,INRAT,ONUMR(INRAT)
      REAL     OAREA(INRAT),ORAT(INRAT,IMXROW,4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMXROW - max number of rows in a rating curve
C     INRAT  - number of rating curves
C     ONUMR  - number of rows in each rating curve
C     OAREA  - area associated with each rating curve
C     ORAT   - rating curve data
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + +
      INTEGER I, J, K
C
C     + + + END SPECIFICATIONS + + +
C
      DO 30 I = 1,NUMRAT
        ONUMR(I) = NUMROW(I)
        OAREA(I) = AREA(I)
        DO 20 J = 1,NUMROW(I)
          DO 10 K = 1,4
            ORAT(I,J,K) = RAT(I,J,K)
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE
C
      RETURN
      END
