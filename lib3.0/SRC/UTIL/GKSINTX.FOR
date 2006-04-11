C
C
C
      SUBROUTINE   GINST
     I                  (WKID,STDNR,LSTR,ISTR,PET,
     I                   XMIN,XMAX,YMIN,YMAX,
     I                   BUFLEN,INIPOS,LDR,DATREC)
C
C     + + + purpose + + +
C     initialize string (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WKID,STDNR,LSTR,PET,BUFLEN,INIPOS,LDR
      REAL         XMIN,XMAX,YMIN,YMAX
      CHARACTER*80 ISTR,DATREC(LDR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     STDNR  - string device number
C     LSTR   - length of initial string
C     ISTR   - initial string
C     PET    - prompt echo type
C     XMIN   - echo area in device coordinates
C     XMAX   - echo area in device coordinates
C     YMIN   - echo area in device coordinates
C     YMAX   - echo area in device coordinates
C     BUFLEN - initial buffer size
C     INIPOS - initial cursor position
C     LDR    - dimension of data record array
C     DATREC - data record
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GINST:'
C
      RETURN
      END
C
C
C
      SUBROUTINE   GQSTS
     I                  (WKID,STDNR,MLDR,
     O                   ERRIND,MODE,ESW,LOSTR,ISTR,PET,EAREA,
     O                   BUFLEN,INIPOS,LDR,DATREC)
C
C     + + + PURPOSE + + +
C     inquire choice device state (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WKID,STDNR,MLDR,
     1             ERRIND,MODE,ESW,LOSTR,PET,BUFLEN,INIPOS,LDR
      REAL         EAREA(4)
      CHARACTER*80 ISTR,DATREC(MLDR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     STDNR  - string device number
C     MLDR   - dimension of data record array
C     ERRIND - error indicator
C     MODE   - operating mode (GREQU, GSAMPL, GEVENT)
C     ESW    - echo switch (GNECHO, GECHO)
C     LOSTR  - number of characters returned
C     ISTR   - initial string
C     PET    - prompt echo type
C     EAREA  - echo area in device coordinates - XMIN, XMAX, YMIN, YMAX
C     BUFLEN - initial buffer size
C     INIPOS - initial cursor position
C     LDR    - number of array elements used in data record
C     DATREC - data record
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GQSDS:'
C
      RETURN
      END
C
C
C
      SUBROUTINE   GQLCS
     I                  (WKID,LCDNR,TYPE,MLDR,
     O                   ERRIND,MODE,ESW,TNR,IPX,IPY,PET,EAREA,
     O                   LDR,DATREC)
C
C     + + + PURPOSE + + +
C     inquire locator device state (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WKID,LCDNR,TYPE,MLDR,
     1             ERRIND,MODE,ESW,TNR,PET,LDR
      REAL         IPX,IPY,EAREA(4)
      CHARACTER*80 DATREC(MLDR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     LCDNR  - locator device number
C     TYPE   - type of returned values (GSET, GREALI)
C     MLDR   - dimension of data record array
C     ERRIND - error indicator
C     MODE   - operating mode (GREQU, GSAMPL, GEVENT)
C     ESW    - echo switch (GNECHO, GECHO)
C     TNR    - initial normalization transformation number
C     IPX    - initial locator position (wc) X
C     IPY    - initial locator position (wc) Y
C     PET    - prompt echo type
C     EAREA  - echo area in device coordinates - XMIN, XMAX, YMIN, YMAX
C     LDR    - number of array elements used in data record
C     DATREC - data record
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GQLCS:'
C
      RETURN
      END
C
C
C
      SUBROUTINE   GQDLC
     I                  (WTYPE,DEVNO,N,MLDR,
     O                   ERRIND,DPX,DPY,OL,PET,EAREA,
     O                   LDR,DATREC)
C
C     + + + PURPOSE + + +
C     inquire default locator device data (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WTYPE,DEVNO,N,MLDR,
     1             ERRIND,OL,PET,LDR
      REAL         DPX,DPY,EAREA(4)
      CHARACTER*80 DATREC(MLDR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WTYPE  - workstation type
C     DEVNO  - logical input device number
C     N      - list element requested
C     MLDR   - dimension of data record array
C     ERRIND - error indicator
C     DPX    - default initial locator position X
C     DPY    - default initial locator position Y
C     OL     - number of available prompt/echo types
C     PET    - Nth element of prompt echo type
C     EAREA  - echo area in device coordinates - XMIN, XMAX, YMIN, YMAX
C     LDR    - number of array elements used in data record
C     DATREC - data record
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GQDLC:',WTYPE,DEVNO,N,MLDR
C
      ERRIND = 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSLCM
     I                  (WKID,LCDNR,MODE,ESW)
C
C     + + + PURPOSE + + +
C     set locator mode (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WKID,LCDNR,MODE,ESW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     LCDNR  - locator device number
C     MODE   - operating mode (GREQU, GSAMPL, GEVENT)
C     ESW    - echo switch (GNECHO, GECHO)
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GSLCM:',WKID,LCDNR,MODE,ESW
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSDS
     I                  (WKID,DEFMOD,REGMOD)
C
C     + + + PURPOSE + + +
C     set deferral state (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WKID,DEFMOD,REGMOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     DEFMOD - deferral mode (GASAP,GBNIG,GBNIL,GASTI)
C     REGMOD - implicit regeneration mode
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GSDS :',WKID,DEFMOD,REGMOD
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSPKM
     I                  (WKID,PKDNR,MODE,ESW)
C
C     + + + PURPOSE + + +
C     set pick mode (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WKID,PKDNR,MODE,ESW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     PKDNR  - pick device number
C     MODE   - operating mode (GREQU, GSAMPL, GEVENT)
C     ESW    - echo switch (GNECHO, GECHO)
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GSPKM:',WKID,PKDNR,MODE,ESW
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSSTM
     I                  (WKID,STDNR,MODE,ESW)
C
C     + + + PURPOSE + + +
C     set string mode (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WKID,STDNR,MODE,ESW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     STDNR  - string device number
C     MODE   - operating mode (GREQU, GSAMPL, GEVENT)
C     ESW    - echo switch (GNECHO, GECHO)
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GSSTM:',WKID,STDNR,MODE,ESW
C
      RETURN
      END
C
C
C
      SUBROUTINE   GRQST
     I                  (WKID,STDNR,
     O                   STAT,LOSTR,STR)
C
C     + + + PURPOSE + + +
C     request string (DOES NOT WORK)
C
C     + + + DUMMY ARGUEMTS + + +
      INTEGER      WKID,STDNR,STAT,LOSTR
      CHARACTER*80 STR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKID   - workstation identifier
C     STDNR  - string device number
C     STAT   - status (GNONE, GOK)
C     LOSTR  - number of cahracters returned
C     STR    - character string
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GRQST:'
C
      RETURN
      END
C
C
C
      SUBROUTINE   GQNT
     I                 (NTNR,
     O                  ERRIND,WINDOW,VIEWPT)
C
C     + + + PURPOSE + + +
C     inquire normalization transformation (DOES NOT WORK)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NTNR,ERRIND
      REAL      WINDOW(4),VIEWPT(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NTNR   - normalization transformation number
C     ERRIND - error indicator
C     WINDOW - window limits in world coords (XMIN,XMAX,YMIN,YMAX)
C     VIEWPT - viewprt lim in normalized dev coord(XMIN,XMAX,YMIN,YMAX)
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GQNT :'
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSVPIP
     I                   (TNR,RTNR,RELPRI)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TNR,RTNR,RELPRI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TNR    - transformation number
C     RTNR   - reference transformation number
C     RELPRI - relative priority (GHIGHR,GLOWER)
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GSVPIP:',TNR,RTNR,RELPRI
C
      RETURN
      END
C
C
C
      SUBROUTINE   GUREC
     I                  (LDR,DATREC,IIL,IRL,ISL,
     O                   ERRIND,IL,IA,RL,RA,SL,LSTR,STR)
C
C     + + + PURPOSE + + +
C     unpack data record (DOES NOT WORK)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LDR,IIL,IRL,ISL,ERRIND,IL,IA(IIL),RL,SL,LSTR(ISL)
      REAL         RA(IRL)
      CHARACTER*80 DATREC(LDR),STR(ISL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LDR    - number of array elements used in DATREC
C     DATREC - data record
C     IIL    - dimension of integer array
C     IRL    - dimension of real array
C     ISL    - dimension of character array
C     ERRIND - error indicator (zero if no error)
C     IL     - number of integer entries
C     IA     - array containing integer entries
C     RL     - number of real entries
C     RA     - array containing real entries
C     SL     - number of character string entries
C     LSTR   - length of each character string entry
C     STR    - character string entries
C
C     + + + END SPECICIFICATIONS + + +
C
      WRITE (99,*) 'GKS:GUREC:',LDR,DATREC,IIL,IRL,ISL
C
      RETURN
      END

