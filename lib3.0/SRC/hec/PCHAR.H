C -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C
C
      INTEGER    IMXMCP
      INTEGER    IFUNLN
C     PARAMETER (IMXMCP = 300, IFUNLN=80, MAXCHR=200, MAXFUN=50)        H
      PARAMETER (IMXMCP = 100, IFUNLN=50, MAXCHR=200, MAXFUN=40)        uL
C     PARAMETER (IMXMCP = 50, IFUNN=25, MAXCHR=133, MAXFUN=20)          ML
C
      CHARACTER*(MAXCHR) C133
      CHARACTER*(MAXCHR)  CLBUFM(IMXMCP+2)
      CHARACTER*(MAXCHR)  CLINE, CKBLIN, CLINSV, CTEMP
      CHARACTER CFUNCT(MAXFUN)*(IFUNLN), CKEY(MAXFUN)
      CHARACTER*4   CSPL
      CHARACTER*60  CPROMT
C
      COMMON /PCHAR/ CLBUFM, CKEY,   CFUNCT, CLINE, CSPL, C133,
     +               CPROMT, CLINSV, CKBLIN
C
C
C -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
