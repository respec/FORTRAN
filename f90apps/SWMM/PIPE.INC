      REAL LEN
      CHARACTER*10 ACOND,KJUNC
      COMMON/PIPE/LEN(NEE),AFULL(NEE),DEEP(NEE),WIDE(NEE),STHETA(NEE),
     1            SPHI(NEE),A(NEE),AT(NEE),VT(NEE),RFULL(NEE),Q(NEE),
     2            QO(NEE),ZD(NEE),QFULL(NEE),PMAX(NEE,2),ZU(NEE),
     3            QT(NEE),H(NEE,2),ROUGH(NEE),V(NEE),NJUNC(NEE,2),
     4            NCOND(NEE),NKLASS(NEE),ACOND(NEE),KJUNC(NEE,2),
     5            ENTK(NEE),EXITK(NEE),OTHERK(NEE)
c common related to one-way pipes and pipes with capacity limits
c note that INGATE was moved from JUNC.INC to here.
      COMMON/PIPE2/ INGATE(NEE)
