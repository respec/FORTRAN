C     from CRHSH.INC   
C     RCHRES module common
C            SHADE section
C
C     + + + PARAMETERS + + +
      INTEGER    MXSP,MXPL
      PARAMETER (MXSP = 250) 
      PARAMETER (MXPL = 10) 
C
      COMMON/SCRTCH/HYDRFG,ADFG,CONSFG,HTFG,SEDFG,GQFG,OXFG,NUTFG,
     $              PLKFG,PHFG,ACIDFG,DUMZ,PFLAG,BFLAG,DUMY,PIVL,BIVL,
     $              PYREND,BYREND,RCHPFG,RCHBFG,UUNITS,IUNITS,OUNITS,
     $              PUNIT,BUNIT,MESSU,MSGFL,RCHNO,RCHID,DELT,DELT60,
     $              DELTS,NDAY,NDAYS,LKFG,NEXITS,HRFG,DAYFG,STFG,
     $              EDAYFG,EMONFG,EPYRFG,EBYRFG,YR,MON,DAY,HR,MIN,IVL,
     $              IVL1,PIVLNO,BIVLNO,NXTMON,SPAFP,SPAKND,SPIVL,
     $              SPOPNO,SPOUT,SPNUND,DUMGEN,DUM1,TWID,DUM2,SOLRAD,
     $              DUM3,DSOLAR,DUM4,SHDFAC,DUM5,TOPFL,VEGFL,NSSP,TSA,
     $              DISL,DISR,WIDL,WIDR,HABSL,HABSR,HDEML,HDEMR,
     $              LATDEG,LONDEG,LONSTD,DENL,DENR,NVPL,NVPR,JDAY,
     $              LATRAD,SO,TSAL,TSAR,SKOP,VSAL,VSAR,DLT1,DEC,HFDLT,
     $              DF,DUMRCH
      INTEGER       HYDRFG,ADFG,CONSFG,HTFG,SEDFG,GQFG,OXFG,NUTFG,
     $              PLKFG,PHFG,ACIDFG,DUMZ(2),PFLAG(11),BFLAG(11),
     $              DUMY(1),PIVL,BIVL,PYREND,BYREND,RCHPFG,RCHBFG,
     $              UUNITS,IUNITS,OUNITS,PUNIT(2),BUNIT(2),MESSU,
     $              MSGFL,RCHNO,RCHID(5),NDAY(12),NDAYS,LKFG,NEXITS,
     $              HRFG,DAYFG,STFG,EDAYFG,EMONFG,EPYRFG,EBYRFG,YR,
     $              MON,DAY,HR,MIN,IVL,IVL1,PIVLNO,BIVLNO,NXTMON,
     $              SPAFP,SPAKND,SPIVL,SPOPNO,SPOUT,SPNUND,DUMGEN(13),
     $              TOPFL,VEGFL,NSSP,TSA(MXSP,12),DISL(MXSP,MXPL),
     $              DISR(MXSP,MXPL),WIDL(MXSP,MXPL),WIDR(MXSP,MXPL),
     $              HABSL(MXSP,MXPL),HABSR(MXSP,MXPL),
     $              HDEML(MXSP,MXPL),HDEMR(MXSP,MXPL),NVPL(MXSP),
     $              NVPR(MXSP),JDAY
      REAL          DELT,DELT60,DELTS,TWID,SOLRAD,DSOLAR,SHDFAC,
     $              LATDEG,LONDEG,LONSTD,DENL(MXSP,MXPL),
     $              DENR(MXSP,MXPL),LATRAD,SO(MXSP),TSAL(MXSP),
     $              TSAR(MXSP),SKOP(MXSP),VSAL(MXSP),VSAR(MXSP),DLT1,
     $              DEC,HFDLT,DF
      INTEGER       DUM1(4482),DUM2(9909),DUM3(4),DUM4(277),
     $              DUM5(7806),DUMRCH(1395)
C
      EQUIVALENCE   (ASVEC,HYDRFG),(UNIT,UUNITS),(TIMFG,HRFG),
     $              (DATIM,YR)
      INTEGER       ASVEC(11),UNIT(7),TIMFG(6),DATIM(5)
C     end byte: 216000 word:54000
