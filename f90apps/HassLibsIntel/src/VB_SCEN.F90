      !scnmod
      SUBROUTINE F90_SCNDBG (LEV)
        USE SCENMOD, ONLY : M_SETDBG
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SCNDBG

        INTEGER,         INTENT(IN)  ::LEV

        CALL M_SETDBG(LEV)

      END SUBROUTINE F90_SCNDBG

      SUBROUTINE F90_SPIPH (HIN,HOUT)
        ! HIN is handle to pipe sending input to the fort applic
        ! HOUT is handle to pipe sending output from the fort applic
        USE SCENMOD, ONLY : M_SPIPH
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SPIPH

        INTEGER,  INTENT(IN) :: HIN, HOUT

        CALL M_SPIPH(HIN,HOUT)

      END SUBROUTINE F90_SPIPH

      SUBROUTINE F90_ACTSCN_XX (MKFILS,WDMFL,MSGFL,RETCOD,CSLEN,CSCENI)
        USE SCENMOD, ONLY : M_ACTSCN
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_ACTSCN_XX
    
        INTEGER, INTENT(IN)  :: MKFILS
        INTEGER, INTENT(IN)  :: CSLEN
        INTEGER, INTENT(IN)  :: CSCENI(CSLEN)
        INTEGER, INTENT(IN)  :: WDMFL(4),MSGFL
        INTEGER, INTENT(OUT) :: RETCOD
 
        CHARACTER(LEN=24)    :: CSCEN 
        
        DO I = 1,CSLEN
          CSCEN(I:I) = CHAR(CSCENI(I))
        END DO
        write(99,*) 'f90_actscnx',MKFILS,WDMFL,CSLEN,CSCEN(1:CSLEN)
        CALL M_ACTSCN (MKFILS,CSCEN(1:CSLEN),WDMFL,MSGFL,RETCOD)
 
      END SUBROUTINE F90_ACTSCN_XX
        
      SUBROUTINE F90_ACTSCN (MKFILS,WDMFL,MSGFL,RETCOD,CSCEN)
!       activate scenario
        USE SCENMOD, ONLY : M_ACTSCN
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_ACTSCN

        INTEGER,         INTENT(IN)  :: MKFILS
        CHARACTER(LEN=*),INTENT(IN)  :: CSCEN
        INTEGER,         INTENT(IN)  :: WDMFL(4),MSGFL
        INTEGER,         INTENT(OUT) :: RETCOD
        INTEGER                      :: CSLEN 
       
        CSLEN = LEN(CSCEN)
        write(99,*) 'f90_actscn',MKFILS,CSLEN,CSCEN(1:CSLEN)
        CALL M_ACTSCN (MKFILS,CSCEN,WDMFL,MSGFL,RETCOD)

      END SUBROUTINE F90_ACTSCN

      !scnmod
      SUBROUTINE   F90_SIMSCN (RETCOD)
!       simulate scenario
        USE SCENMOD, ONLY : M_SIMSCN
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SIMSCN

        INTEGER, INTENT(OUT)  :: RETCOD

        CALL M_SIMSCN(RETCOD)

      END SUBROUTINE F90_SIMSCN

      !hspf:hringen
      SUBROUTINE F90_GLOBLK_XX (SDATIM,EDATIM,OUTLEV, &
                                SPOUT,RUNFG,EMFG,INFO)
!       get info from global block
        USE SCENMOD, ONLY : M_GLOBLK
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_GLOBLK_XX

        INTEGER, INTENT(OUT)    :: SDATIM(5),EDATIM(5),OUTLEV
        INTEGER, INTENT(OUT)    :: SPOUT,RUNFG,EMFG,INFO(80)

        INTEGER                 :: IHMFG

        CALL M_GLOBLK (SDATIM,EDATIM,OUTLEV, &
                       SPOUT,RUNFG,EMFG,IHMFG,INFO)

      END SUBROUTINE F90_GLOBLK_XX

      !hspf:hringen
      SUBROUTINE F90_GLOPRMI (IVAL, PARMNAME)
!       get a single integer parameter value from global block
        USE SCENMOD, ONLY : M_GLOBLK
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_GLOPRMI

        CHARACTER(LEN=*), INTENT(IN) :: PARMNAME
        INTEGER, INTENT(OUT)         :: IVAL

        INTEGER                 :: SDATIM(5),EDATIM(5),OUTLEV
        INTEGER                 :: SPOUT,RUNFG,EMFG,INFO(80)
        INTEGER                 :: IHMFG

        CALL M_GLOBLK (SDATIM,EDATIM,OUTLEV, &
                       SPOUT,RUNFG,EMFG,IHMFG,INFO)

        IF (PARMNAME .EQ. 'IHMFG') THEN
          IVAL = IHMFG
        ELSE
          IVAL = -999
        END IF

      END SUBROUTINE F90_GLOPRMI

      !newaqt:scnutl
      SUBROUTINE F90_PUTGLO (SDATIM,EDATIM,OUTLEV, &
                             SPOUT,RUNFG,EMFG,INFO)
!       put info to global block

        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_PUTGLO

        INTEGER, INTENT(IN)          :: SDATIM(5),EDATIM(5),OUTLEV
        INTEGER, INTENT(IN)          :: SPOUT,RUNFG,EMFG
        CHARACTER(LEN=*), INTENT(IN) :: INFO

        CHARACTER*80            :: RNINFO
        INTEGER                 :: RESMFG

        RESMFG = 0
        RNINFO = INFO
        CALL PUTGLO (SDATIM,EDATIM,OUTLEV,SPOUT, &
                     RESMFG,RUNFG,EMFG,RNINFO)

      END SUBROUTINE F90_PUTGLO

      !newaqt:scnutl
      SUBROUTINE F90_GTNXKW_XX (INIT,ID,IKWD,KWDFG,CONTFG,RETID)
!       get next keyword and check for keyword's existance in uci file
        USE SCENMOD, ONLY : M_GTNXKW
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_GTNXKW_XX

        INTEGER, INTENT(IN)  :: INIT,ID
        INTEGER, INTENT(OUT) :: IKWD(12),KWDFG,CONTFG,RETID

        CALL M_GTNXKW(INIT,ID,IKWD,KWDFG,CONTFG,RETID)

      END SUBROUTINE F90_GTNXKW_XX

      !newaqt:scnutl
      SUBROUTINE F90_XBLOCK_XX (BLKNO,INIT,RETKEY,CBUFF,RETCOD)
!       return one line of selected block
        USE SCENMOD, ONLY : M_XBLOCK
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_XBLOCK_XX

        INTEGER, INTENT(IN)  :: BLKNO,INIT
        INTEGER, INTENT(OUT) :: CBUFF(80),RETCOD,RETKEY

        CALL M_XBLOCK(BLKNO,INIT,RETKEY,CBUFF,RETCOD)

      END SUBROUTINE F90_XBLOCK_XX

      !newaqt:scnutl
      SUBROUTINE F90_XBLOCKEX_XX (BLKNO,INIT,RETKEY,CBUFF,RECTYP,RETCOD)
!       return one line of selected block
        USE SCENMOD, ONLY : M_XBLOCKEX
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_XBLOCKEX_XX

        INTEGER, INTENT(IN)    :: BLKNO,INIT
        INTEGER, INTENT(INOUT) :: RETKEY
        INTEGER, INTENT(OUT)   :: CBUFF(80),RETCOD,RECTYP

        CALL M_XBLOCKEX(BLKNO,INIT,RETKEY,CBUFF,RECTYP,RETCOD)

      END SUBROUTINE F90_XBLOCKEX_XX

      !newaqt:scnutl
      SUBROUTINE F90_XTABLE_XX (OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR, &
                                RETKEY,CBUFF,RETCOD)
!       return one line of selected table
        USE SCENMOD, ONLY : M_XTABLE
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_XTABLE_XX

        INTEGER, INTENT(IN)  :: OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR
        INTEGER, INTENT(OUT) :: CBUFF(80),RETCOD,RETKEY

        CALL M_XTABLE(OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,       &
                      RETKEY,CBUFF,RETCOD)

      END SUBROUTINE F90_XTABLE_XX

      !newaqt:scnutl
      SUBROUTINE F90_XTABLEEX_XX (OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR, &
                                  RETKEY,CBUFF,RECTYP,RETCOD)
!       return one line of selected table
        USE SCENMOD, ONLY : M_XTABLEEX
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_XTABLEEX_XX

        INTEGER, INTENT(IN)    :: OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR
        INTEGER, INTENT(INOUT) :: RETKEY
        INTEGER, INTENT(OUT)   :: CBUFF(80),RETCOD,RECTYP

        CALL M_XTABLEEX(OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,       &
                      RETKEY,CBUFF,RECTYP,RETCOD)

      END SUBROUTINE F90_XTABLEEX_XX

      !newaqt:scnutl
      SUBROUTINE F90_XTINFO_XX (OMCODE,TABNO,UUNITS,CHKEST,       &
                    LNFLDS,LSCOL,LFLEN,ILFTYP,LAPOS,LIMIN,LIMAX,  &
                    LIDEF,LRMIN,LRMAX,LRDEF,LNMHDR,IHDRBUF,       &
                    IFDNAM,ISECT,IREPT,RETCOD)
!       return info about specified table
        USE SCENMOD, ONLY : M_XTINFO
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_XTINFO_XX

        INTEGER,INTENT(IN)  :: OMCODE,TABNO,UUNITS,CHKEST
        INTEGER,INTENT(OUT) :: LNFLDS,LSCOL(30),LFLEN(30),        &
                               LAPOS(30),LIMIN(30),LIMAX(30),     &
                               LIDEF(30),LNMHDR,RETCOD,ISECT
        REAL,   INTENT(OUT) :: LRMIN(30),LRMAX(30),LRDEF(30)
        INTEGER,INTENT(OUT) :: ILFTYP(30),IHDRBUF(78,10),         &
                               IFDNAM(12,30),IREPT

        CALL M_XTINFO (OMCODE,TABNO,UUNITS,CHKEST,                &
                    LNFLDS,LSCOL,LFLEN,ILFTYP,LAPOS,LIMIN,LIMAX,  &
                    LIDEF,LRMIN,LRMAX,LRDEF,LNMHDR,IHDRBUF,IFDNAM,&
                    ISECT,IREPT,RETCOD)

        IF (OMCODE.EQ.8) THEN
          !special handling for external targets
          LFLEN(15) = 4
          LFLEN(16) = 4
          LSCOL(16) = 77
        END IF

      END SUBROUTINE F90_XTINFO_XX

      !newaqt:ztwdmf
      SUBROUTINE F90_WMSGTW_XX (ID,IWINAM)
!       returns window name
        USE SCENMOD, ONLY : M_WMSGTW
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_WMSGTW_XX

        INTEGER,INTENT(IN)  :: ID
        INTEGER,INTENT(OUT) :: IWINAM(*) 

        CALL M_WMSGTW(ID,IWINAM)

      END SUBROUTINE F90_WMSGTW_XX

      !newaqt:ztwdmf
      SUBROUTINE F90_WMSGTH (GHELP,FHELP)
!       returns help pointers
        USE SCENMOD, ONLY : M_WMSGTH
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_WMSGTH

        INTEGER,INTENT(OUT) :: GHELP,FHELP(64)

        CALL M_WMSGTH(GHELP,FHELP)

      END SUBROUTINE F90_WMSGTH

      !newaqt:scnutl
      SUBROUTINE F90_DELSCN (CSCEN)
!       delete specified scenario
        USE SCENMOD, ONLY : M_DELSCN
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_DELSCN

        CHARACTER(LEN=*) :: CSCEN

        CALL M_DELSCN(CSCEN)

      END SUBROUTINE F90_DELSCN

      !newaqt:scnutl
      SUBROUTINE F90_COPSCN (DSNID,RELABS,CASCEN,CNSCEN)
!       copy specified scenario
        USE SCENMOD, ONLY : M_COPSCN
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_COPSCN

        CHARACTER(LEN=*),INTENT(IN)  :: CASCEN,CNSCEN
        INTEGER,         INTENT(IN)  :: DSNID,RELABS

        CALL M_COPSCN(DSNID,RELABS,CASCEN,CNSCEN)

      END SUBROUTINE F90_COPSCN

      !newaqt:scnutl
      SUBROUTINE F90_NEWFIL (CASCEN,CNSCEN)
!       change files block for new scenario
        USE SCENMOD, ONLY : M_NEWFIL
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_NEWFIL

        CHARACTER(LEN=*),INTENT(IN)  :: CASCEN,CNSCEN

        CALL M_NEWFIL(CASCEN,CNSCEN)

      END SUBROUTINE F90_NEWFIL

      !newaqt:scnutl
      SUBROUTINE F90_NEWDSN (WDMID,ODSN,NDSN)
!       change dsn in ext targets block for new scenario
        USE SCENMOD, ONLY : M_NEWDSN
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_NEWDSN

        INTEGER,         INTENT(IN)  :: WDMID,ODSN,NDSN

        CALL M_NEWDSN(WDMID,ODSN,NDSN)

      END SUBROUTINE F90_NEWDSN

      !newaqt:scnutl
      SUBROUTINE F90_UCISAV
!       Write out a UCI file to disk
        USE SCENMOD, ONLY : M_UCISAV
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_UCISAV

        CALL M_UCISAV

      END SUBROUTINE F90_UCISAV

      !hspf:hiouci
      SUBROUTINE F90_REPUCI (KEY,UCIBF)
!       replace a UCI line, no change to assoc info
        USE SCENMOD, ONLY : M_REPUCI
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_REPUCI

        INTEGER,         INTENT(IN)  :: KEY
        CHARACTER(LEN=*),INTENT(IN)  :: UCIBF

        CALL M_REPUCI(KEY,UCIBF)

      END SUBROUTINE F90_REPUCI

      !hspf:hiouci
      SUBROUTINE F90_DELUCI (KEY)
!       delete a UCI line and assoc info
        USE SCENMOD, ONLY : M_DELUCI
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_DELUCI

        INTEGER,         INTENT(IN)  :: KEY

        CALL M_DELUCI(KEY)

      END SUBROUTINE F90_DELUCI

      !hspf:hiouci
      SUBROUTINE F90_PUTUCI (KEY,TYPE,UCIBF)
!       Save a UCI line and assoc info
        USE SCENMOD, ONLY : M_PUTUCI
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_PUTUCI

        INTEGER,         INTENT(IN)  :: KEY,TYPE
        CHARACTER(LEN=*),INTENT(IN)  :: UCIBF

        CALL M_PUTUCI(KEY,TYPE,UCIBF)

      END SUBROUTINE F90_PUTUCI

      !newaqt:hget
      SUBROUTINE F90_HGETI (ITMNAM,IDNO,IVAL)
!       get one integer item from hspf card image data structure
        USE SCENMOD, ONLY : M_HGETI
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_HGETI

        CHARACTER(LEN=*),INTENT(IN)  :: ITMNAM
        INTEGER,         INTENT(IN)  :: IDNO
        INTEGER,         INTENT(OUT) :: IVAL

        CALL M_HGETI(ITMNAM,IDNO,IVAL)

      END SUBROUTINE F90_HGETI

      !newaqt:hget
      SUBROUTINE F90_HGETC_XX (ITMNAM,IDNO,ICTXT)
!       get one character item from hspf card image data structure
        USE SCENMOD, ONLY : M_HGETC
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_HGETC_XX

        CHARACTER(LEN=*),INTENT(IN)  :: ITMNAM
        INTEGER,INTENT(IN)           :: IDNO
        INTEGER,INTENT(OUT)          :: ICTXT(*)

        CALL M_HGETC(ITMNAM,IDNO,ICTXT)

      END SUBROUTINE F90_HGETC_XX

      !newaqt:hget
      SUBROUTINE F90_HPUTC (ITMNAM,IDNO,CTXT)
!       put one character item to hspf card image data structure
        USE SCENMOD, ONLY : M_HPUTC
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_HPUTC

        INTEGER,         INTENT(IN)  :: IDNO
        CHARACTER(LEN=*),INTENT(IN)  :: ITMNAM,CTXT

        CALL M_HPUTC(ITMNAM,IDNO,CTXT)

      END SUBROUTINE F90_HPUTC

      !newaqt:ucibat
      SUBROUTINE F90_GTINS_XX (INIT,IDNO,RORB,ICTXT,RAREA)
!       get an input segment into this reach or bmprac from schematic block
        USE SCENMOD, ONLY : M_GTINS
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_GTINS_XX

        INTEGER,INTENT(IN)           :: INIT,IDNO,RORB
        INTEGER,INTENT(OUT)          :: ICTXT(*)
        REAL,INTENT(OUT)             :: RAREA

        CALL M_GTINS(INIT,IDNO,RORB,ICTXT,RAREA)

      END SUBROUTINE F90_GTINS_XX

      !newaqt:ucibat
      SUBROUTINE F90_PBMPAR (CIN,INID,RAREA,COUT,OUTID,RETCOD)
!       put bmp info to uci file in memory schematic and opn seq block
        USE SCENMOD, ONLY : M_PBMPAR
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_PBMPAR

        INTEGER,         INTENT(IN)  :: INID,OUTID
        CHARACTER(LEN=*),INTENT(IN)  :: CIN,COUT
        REAL,            INTENT(IN)  :: RAREA
        INTEGER,         INTENT(OUT) :: RETCOD

        CALL M_PBMPAR (CIN,INID,RAREA,COUT,OUTID,RETCOD)

      END SUBROUTINE F90_PBMPAR

      !newaqt:ucibat
      SUBROUTINE F90_DELBMP (BMPID)
!       delete a bmp
        USE SCENMOD, ONLY : M_DELBMP
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_DELBMP

        INTEGER,         INTENT(IN)  :: BMPID

        CALL M_DELBMP(BMPID)

      END SUBROUTINE F90_DELBMP

      SUBROUTINE F90_MSGUNIT (MSGFL)
!       send message file unit number to module
        USE SCENMOD, ONLY : M_MSGUNIT
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_MSGUNIT

        INTEGER, INTENT(IN)  :: MSGFL

        CALL M_MSGUNIT (MSGFL)

      END SUBROUTINE F90_MSGUNIT

      SUBROUTINE F90_FILSTA (MSG)
        USE SCENMOD, ONLY : M_FILSTA
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_FILSTA

        CHARACTER(LEN=*),INTENT(IN)  :: MSG     
        
        CALL M_FILSTA (MSG)

      END SUBROUTINE F90_FILSTA

      SUBROUTINE   F90_SET_DRIVER (INTEG_FLAG,INTEG_TIMESTEP, &
                                   INTEG_FILENAME)
        USE SCENMOD, ONLY : M_SET_DRIVER
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SET_DRIVER

        INTEGER, INTENT(IN)  :: INTEG_FLAG
        REAL, INTENT(IN)     :: INTEG_TIMESTEP
        CHARACTER(LEN=*),INTENT(IN)  :: INTEG_FILENAME

        CALL M_SET_DRIVER(INTEG_FLAG,INTEG_TIMESTEP,INTEG_FILENAME)
    
      END SUBROUTINE F90_SET_DRIVER
