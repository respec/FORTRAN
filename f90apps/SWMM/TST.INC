CIMT  change dimension of PTC0 from 4 to MQUAL 
      COMMON/TST/QINST,STORL(NTSE),STORE(NTSE),TSTORE(NTSE,17),
     1           TSQOU(NTSE,17),TSDEP(NTSE,17),TQPUMP(NTSE,2),
     2           TDSTOP(NTSE),TDSTAR(NTSE,2),LOUT(NTSE),
     3           PTC0(NTSE,MQUAL),KSTOR,QOUST1,QOUST2,QINSTL(NTSE),
     4           TSAREA(NTSE,17),QOUSTL(NTSE),MINTS(NTSE),
     5           B1(NTSE),B2(NTSE),D0(NTSE),
C#### WCH (Steve Merrill), 3/28/94.  Add to Common.
cimt note that DEPTHL also appears in S1.INC, not good programming practice
     5           DEPTHL(NTSE)
