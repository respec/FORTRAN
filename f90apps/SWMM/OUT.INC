      COMMON/OUT1/YPLT(201,NPO)
      COMMON/OUT2/QPLT(201,NPO)
      COMMON/OUT3/TPLT(201),PRTH(NPO),PRGEL(NPO),
     1            PRTY(NPO),PRTV(NPO),PRTQ(NPO),QQI(NEE)
change dimensions of idum to NCHN
      COMMON/OUT4/JPRT(NPO),IDUM(NCHN),CPRT(NPO),ICOL(10),KPLT(NPO),
     1            JPLT(NPO),JSURF(NPO),JREDO,LTIME,NPLT,LPLT,NPTOT,
     2            NSTART,JNTER,INTER,NSURF,NPRT,IPRT,NHPRT,NQPRT,JPRINT
      COMMON/OUT5/AOUT(NPO,5)
      COMMON/EXSTAT/VMAXX(NEE),QMAXX(NEE),DEPMAX(NEE),
     1              SURLEN(NEE),FLDLEN(NEE),ASMAXX(NEE),YTOT(NEE),
     2              CTIME(NEE,4),AOVERB(NEE),QTOT(NEE),YDEV(NEE),
cim INTCON
CIM SUMOUT and SUMQIN DO NOT NEED TO BE IN COMMON
CIM ELIMINATE TO AVOID CONFUSION
cim     3              HMAX(NEE),AMAX(NEE),SUPLEN(NEE),QDEV(NEE),SUMOUT,
cim     4              SUMQIN,VLEFT,VINIT,IVMIN(NEE),IQHR(NEE),IQMIN(NEE),
     3              HMAX(NEE),AMAX(NEE),SUPLEN(NEE),QDEV(NEE),
     4                     VLEFT,VINIT,IVMIN(NEE),IQHR(NEE),IQMIN(NEE),
     5              IDHR(NEE),IVHR(NEE),IDMIN(NEE),LINK(NEE)
      INTEGER      CPRT
cim no longer character      CHARACTER*3  LINK
cim speedy add four variables to new common
cim      common/codes/DR1,SUB,CR1,CR2
      integer dr1,sub,cr1,cr2
      parameter(dr1=1,sub=2,cr1=3,cr2=4)
cim
      CHARACTER*10 AOUT
