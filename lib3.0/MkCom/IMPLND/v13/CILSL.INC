C     from CILSL.INC   
C     IMPLND module common
C            SOLIDS section
      COMMON/SCRTCH/AIRTFG,SNOWFG,IWATFG,SLDFG,IWGFG,IQALFG,DUMZ,
     $              PFLAG,BFLAG,DUMY,PIVL,BIVL,PYREND,BYREND,IMPPFG,
     $              IMPBFG,UUNITS,IUNITS,OUNITS,PUNIT,BUNIT,MESSU,
     $              MSGFL,LSNO,LSID,DELT,DELT60,DUMX,NDAY,NDAYS,DUMW,
     $              HRFG,DAYFG,STFG,EDAYFG,EMONFG,EPYRFG,EBYRFG,YR,
     $              MON,DAY,HR,MIN,IVL,IVL1,PIVLNO,BIVLNO,NXTMON,
     $              SPAFP,SPAKND,SPIVL,SPOPNO,SPOUT,SPNUND,DUMGEN,
     $              DUM1,PRECFP,DUM2,SNIF,DUM3,SNOCOV,DUM4,SNOCFP,
     $              DUM5,RAINF,DUM6,RNFFP,DUM7,SURS,DUM8,SSFP,DUM9,
     $              IWCF1,DUM10,SOFP,DUM11,SLDIF,SLSDFP,VASDFG,VRSDFG,
     $              SDOPFG,ACCSDM,REMSDM,KEIM,JEIM,DRYDFG,SLDS,SLDSFP,
     $              ACCSDP,REMSDP,SDCF1,SOSDFP,DUMIMP
      INTEGER       AIRTFG,SNOWFG,IWATFG,SLDFG,IWGFG,IQALFG,DUMZ(7),
     $              PFLAG(6),BFLAG(6),DUMY(12),PIVL,BIVL,PYREND,
     $              BYREND,IMPPFG,IMPBFG,UUNITS,IUNITS,OUNITS,
     $              PUNIT(2),BUNIT(2),MESSU,MSGFL,LSNO,LSID(5),
     $              DUMX(1),NDAY(12),NDAYS,DUMW(2),HRFG,DAYFG,STFG,
     $              EDAYFG,EMONFG,EPYRFG,EBYRFG,YR,MON,DAY,HR,MIN,IVL,
     $              IVL1,PIVLNO,BIVLNO,NXTMON,SPAFP,SPAKND,SPIVL,
     $              SPOPNO,SPOUT,SPNUND,DUMGEN(13),PRECFP,SNOCFP,
     $              RNFFP,SSFP,SOFP,SLSDFP,VASDFG,VRSDFG,SDOPFG,
     $              DRYDFG,SLDSFP,SOSDFP
      REAL          DELT,DELT60,SNIF(5),SNOCOV,RAINF,SURS,IWCF1(4,5),
     $              SLDIF(5),ACCSDM(12),REMSDM(12),KEIM,JEIM,SLDS,
     $              ACCSDP,REMSDP,SDCF1(5)
      INTEGER       DUM1(3),DUM2(28),DUM3(92),DUM4(29),DUM5(40),
     $              DUM6(5),DUM7(45),DUM8(12),DUM9,DUM10(13),DUM11(3),
     $              DUMIMP(17538)
C
      EQUIVALENCE   (ASVEC,AIRTFG),(UNIT,UUNITS),(TIMFG,HRFG),
     $              (DATIM,YR),(PREC,SNIF),(SURO,IWCF1(2,1)),
     $              (SLSLD,SLDIF(1)),(SLDPM1,VASDFG),(SOSLD,SDCF1(1))
      INTEGER       ASVEC(6),UNIT(5),TIMFG(6),DATIM(5),SLDPM1(3)
      REAL          PREC,SURO,SLSLD,SOSLD
C     end byte:  72000 word:18000
