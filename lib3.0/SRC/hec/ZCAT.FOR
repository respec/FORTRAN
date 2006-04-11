C     INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]                        M
C    * (STRING[REFERENCE])                                              M
C     CHARACTER*1 STRING                                                M
C     END                                                               M
      SUBROUTINE ZCAT (IFLTAB, ICUNIT, ICDUNT, INUNIT, CINSTR,
     *  LABREV, LDOSRT, LCDCAT, NORECS)
C
C     Z - CATALOG,  Generates a listing of record pathnames in a
C     DSS File.  This subroutine will sort the pathnames on Harris
C     Computers (takes significantly longer), or Selectively chose
C     pathnames based on their pathname parts.
C
C     Written by Bill Charley at HEC, 1982.
C
C
      CHARACTER CINSTR*(*)
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*), NORECS, NUMREC, IFSIZ                        ML
      INTEGER IMXPRT(6)
C     INTEGER IFLTAB(*)                                                 u
C     INTEGER*2 SYSTEM                                                  M
      LOGICAL LABREV, LDOSRT, LSELCA, LCDCAT
C
      INTEGER JORDER(6)
      CHARACTER CINST*132, CSCRAT*16
C     INTEGER IPACK(4), JFTYPE(4)                                       H
C     CHARACTER CSFILE(4)*20                                            H
      CHARACTER CNAME*80, CDNAME*80
      LOGICAL LERR
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSCA                                                           H
      INCLUDE 'zdssca.h'                                                MLu
C
CADD C.ZDSSCC                                                           H
      INCLUDE 'zdsscc.h'                                                MLu
C
CADD C.ZDSSCM                                                           H
      INCLUDE 'zdsscm.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
C     IFIELD are the fields to sort with.
C     (e.g., IFIELD 1 and 2 are the starting and ending
C     positions of the first part to sort.
C
C     DATA CSFILE(1) /'W2'/, IPACK(1) /1/, JFTYPE(1) /0/                H
C     DATA CSFILE(2) /'W3'/, IPACK(2) /1/, JFTYPE(2) /0/                H
C     DATA CSFILE(3) /'U1'/, IPACK(3) /2/, JFTYPE(3) /-1/               H
C     DATA CSFILE(4) /'U2'/, IPACK(4) /3/, JFTYPE(4) /-1/               H
C
C
C
C
      CINST = CINSTR
      CALL CHRLNB (CINST, NINST)
C
      IF (MLEVEL.GE.11) THEN                                            D
      N = NINST                                                         D
      IF (N.EQ.0) N = 1                                                 D
      WRITE (MUNIT,20) ICUNIT, ICDUNT, INUNIT, LABREV, LDOSRT, LCDCAT,  D
     * NINST, CINST(1:N)                                                D
 20   FORMAT (T6,'-----DSS---Debug:  Enter ZCAT'/,                      D
     * T11,'ICUNIT:',I4,',  ICDUNT:',I4,',  INUNIT:',I4,/,              D
     * T11,'LABREV:',L4,',  LDOSRT:',L4,',  LCDCAT:',L4,/,              D
     * T11,'NINST: ',I4,',  Instructions: ',A)                          D
      ENDIF                                                             D
C
C
C
C     IF (IFLTAB(1).EQ.4) THEN                                          H
C     CALL ZCATL4 (IFLTAB, ICUNIT, INUNIT, CINSTR, 1, NINST,            H
C    *  LABREV, LDOSRT, NORECS)                                         H
C     GO TO 820                                                         H
C     ENDIF                                                             H
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZCAT',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C     Find out how many records are in the file
      NUMREC = IFLTAB(KNRECS)
C     If no records in the file, error out.
      IF (NUMREC.EQ.0) THEN
      NORECS = 0
      GO TO 900
      ENDIF
C
C
      CNAME = ' '
      CDNAME = ' '
      LEXTND = .TRUE.
      LSORT = .TRUE.
      IF (LABREV) LEXTND = .FALSE.
      IF (.NOT.LDOSRT) LSORT = .FALSE.
      NOPTHS = 0
      JCUNIT = ICUNIT
      JNUNIT = INUNIT
      IF (INUNIT.NE.0) THEN
      REWIND INUNIT
      LEXTND = .FALSE.
      LSORT = .FALSE.
      ENDIF
      LSELCA = .FALSE.
C
C     **** NON-HARRIS Sites:  No Sort Flag ****
      LSORT = .FALSE.                                                   CPIL
C     *****************************************
C
C
C     Determine the length of the sort input file
      IF (LSORT) THEN
      MTOTAL = 0
      DO 40 I=1,6
C     Get the maximum length of each part.  If less than 4, set to 4.
      CALL GETHOL (IFLTAB(KMXPRT), I, IMXPRT(I))
      IF (IMXPRT(I).LT.4) IMXPRT(I) = 4
C     Time series records must have a minimum of 6 for the D and E parts
      IF (((I.EQ.4).OR.(I.EQ.5)).AND.(IMXPRT(I).LT.6)) IMXPRT(I) = 6
      MTOTAL = MTOTAL + IMXPRT(I)
 40   CONTINUE
C
C     DOS Sort has a limit of 64,000 bytes.  Is this file too big?
C     IFSIZ = INT4(MTOTAL+6+2) * NUMREC                                 M
C     IF ((IFSIZ.GE.63950).AND.(NINST.EQ.0)) THEN                       M
C     WRITE (MUNIT, 400) -2                                             M
C     LSORT = .FALSE.                                                   M
C     ENDIF                                                             M
      ENDIF
C
C
C
      ISUNIT(1) = 66
      ISUNIT(2) = 67
      ISUNIT(3) = 68
      ISUNIT(4) = 69
C
C
C     Make an exclusive assignment, so that
C     no one else can access the catalog while we are writing
C     to it, and so that we will know that no one is
C     currently using it.
C     IF (INUNIT.EQ.0) THEN                                             HM
C     CALL GETNAM ( ICUNIT, CNAME, IERR)                                HM
C     IF (IERR.EQ.-1) GO TO 920                                         HM
C     IF (IERR.EQ.0) THEN                                               HM
C     CLOSE (UNIT=ICUNIT)                                               HM
C     CALL CHRLNB (CNAME, ILAST)                                        M
C     OPEN ( UNIT=ICUNIT, FILE=CNAME(1:ILAST), IOSTAT=JERR,             M
C    * SHARE='DENYWR')                                                  M
C     CALL ASSIGX ( ICUNIT, CNAME, JERR)                                H
C     IF (JERR.NE.0) GO TO 930                                          HM
C     OPEN (UNIT=ICUNIT)                                                H
C     ENDIF                                                             HM
C     ENDIF                                                             HM
C
C     Determine if a condensed version of the catalog is to be produced
      IF (LSORT.AND.(ICDUNT.GT.0)) THEN
      LCDCAT = .TRUE.
      ELSE
      LCDCAT = .FALSE.
      ENDIF
C
C
C     Initialize Variables
      DO 60 I=1,6
      JORDER(I) = 0
 60   CONTINUE
C     Default Order of pathname parts for sorting
      IORDER(1) = 1
      IORDER(2) = 2
      IORDER(3) = 3
      IORDER(4) = 6
      IORDER(5) = 5
      IORDER(6) = 4
C
C
C     Decipher any Instructions passed in:
C        Is a sort order specified (e.g., O=CB)
C        Is a Selective Catalog Deisired (e.g., C=FLOW)
C
      IF (LSORT) THEN
C
      IF (NINST.GT.0) THEN
C
C     Check for a sort order specified
      IPOS = INDEX (CINST, 'O=')
      IF (IPOS.NE.0) THEN
C     Yes, a sort order was given.  Deactivate the condensed version
      LCDCAT = .FALSE.
C     Blank O= for catalog title
      CINST(IPOS:IPOS+1) = '   '
      IPOS = IPOS + 2
      IF (IPOS+2.LE.NINST) THEN
C     Look for 'OFF' as a sort parameter
      IF (CINST(IPOS:IPOS+2).EQ.'OFF') THEN
      CINST(IPOS:IPOS+2) = '   '
      LSORT = .FALSE.
      GO TO 300
      ENDIF
      ENDIF
C
C     Get the sort order
      DO 100 I=1,6
      M = INDEX ( 'ABCDEF', CINST(IPOS:IPOS))
      IF (M.EQ.0) GO TO 120
      JPOS = I
      JORDER(I) = M
      CINST(IPOS:IPOS) = ' '
      IPOS = IPOS + 1
      IF (IPOS.GT.NINST) GO TO 120
 100  CONTINUE
C
C     Fill in any remainding order not specified
C     First remove from IORDER any parts given by zeroing them out.
 120  CONTINUE
      IF (JPOS.GT.0) THEN
      DO 140 I=1,JPOS
      DO 140 J=1,6
      IF (JORDER(I).EQ.IORDER(J)) IORDER(J) = 0
 140  CONTINUE
      ENDIF
C
C     Now fill in any remaining parts
      JPOS = JPOS + 1
      IF (JPOS.LE.6) THEN
      DO 180 I=JPOS,6
      DO 160 J=1,6
C     Has this part been specified yet
      IF (IORDER(J).GT.0) THEN
C     No - Use it then zero it out.
      JORDER(I) = IORDER(J)
      IORDER(J) = 0
      GO TO 180
      ENDIF
 160  CONTINUE
 180  CONTINUE
      ENDIF
C
C     COPY BACK COMPLETED ORDER TO IORDER
      DO 190 I=1,6
      IORDER(I) = JORDER(I)
 190  CONTINUE
C
      ENDIF
      ENDIF
C
C     Attach Work Files needed for sort
C     DO 240 I=1,4                                                      H
C     CALL CCREAT (CSFILE(I), 400, IPACK(I), JFTYPE(I), JERR)           H
C     CALL CASSIG (ISUNIT(I), CSFILE(I), IERR)                          H
C     IF (IERR.NE.0) THEN                                               H
C     WRITE (MUNIT,200) IERR, ISUNIT(I), CSFILE(I)                      H
 200  FORMAT(' *** ERROR - ZCAT:  Unable to Access Needed Scratch',
     * ' Files for Sort ***',/,' The Catalog is NOT Sorted;  Error:',I5,
     * /,' Unit:',I5,'  File: ',A,/)
C     LSORT = .FALSE.                                                   H
C     DO 220 J=1,I                                                      H
C     CLOSE (UNIT=ISUNIT(J))                                            H
C220  CONTINUE                                                          H
C     ENDIF                                                             H
C240  CONTINUE                                                          H
C
C
C     Open the sort input file.  This file will contain the pathname
C     parts (in sort order), followed by an integer record number.
C     That record number corresponds to a catalog line in DSSSORT.TMP
C     OPEN (UNIT=ISUNIT(1), FILE='DSSSORT.IN', IOSTAT=IERR)             Mu
C     IF (IERR.NE.0) THEN                                               Mu
C     WRITE (MUNIT, 200) IERR, ISUNIT(1), 'DSSSORT.IN'                  Mu
C     LSORT = .FALSE.                                                   Mu
C     ENDIF                                                             Mu
C
C     This file will contain the catalog lines.  Once the file has been
C     sorted, the catalog lines will be read from this file to create
C     the final catalog.
C     OPEN (UNIT=ISUNIT(3), FILE='DSSSORT.TMP', ACCESS='DIRECT',        Mu
C    * RECL=128, IOSTAT=IERR)                                           Mu
C     IF (IERR.NE.0) THEN                                               Mu
C     WRITE (MUNIT, 200) IERR, ISUNIT(3), 'DSSSORT.TMP'                 Mu
C     LSORT = .FALSE.                                                   Mu
C     ENDIF                                                             Mu
C
C     Get the beginning and ending locations of the maximum parts
C     This is so we can construct a minimum size sort input file
C     containing the A part followed by the B part, etc.
      IBPMAX(1) = 1
      J = IORDER(1)
      IEPMAX(1) =  IMXPRT(J)
      DO 260 I=2,6
      IBPMAX(I) = IEPMAX(I-1) + 1
      J = IORDER(I)
      IEPMAX(I) = IBPMAX(I) + IMXPRT(J) - 1
 260  CONTINUE
C
      ENDIF
C
C
C     Look for Selective Catalog Parameters
 300  CONTINUE
      IF (NINST.GT.0) THEN
      CALL ZSETCA (CINST, LSELCA)
      IF (ILWFLG.EQ.-1) GO TO 960
      ENDIF
C
C
      IF (.NOT.LSORT) LCDCAT = .FALSE.
C
      IF (LCDCAT) THEN
C     Initialize MAXPRT
      DO 320 I=1,6
      MAXPRT(I) = 6
 320  CONTINUE
      MAXPRT(7) = 3
C     CALL GETNAM ( ICDUNT, CDNAME, IERR)                               HM
C     IF (IERR.EQ.-1) GO TO 920                                         HM
C     IF (IERR.EQ.0) THEN                                               HM
C     CLOSE (UNIT=ICDUNT)                                               HM
C     CALL CHRLNB (CDNAME, ILAST)                                       M
C     OPEN ( UNIT=ICDUNT, FILE=CDNAME(1:ILAST), IOSTAT=JERR,            M
C    * SHARE='DENYWR')                                                  M
C     CALL ASSIGX ( ICDUNT, CDNAME, JERR)                               H
C     IF (JERR.NE.0) GO TO 930                                          HM
C     OPEN (UNIT=ICDUNT)                                                H
C     ENDIF                                                             HM
      ENDIF
C
C
C     Write the title to the catalog
      CALL ZCATIT (IFLTAB, ICUNIT, LSELCA, .FALSE., CINST, LERR)
      IF (LERR) GO TO 910
C
      IF (INUNIT.EQ.0) THEN
C     Now obtain a list of the pathnames from the DSS File
      CALL ZCATFI (IFLTAB, LEXTND, LSELCA, LCDCAT, .FALSE., LERR)
      ELSE
C     Obtain a list of pathnames from the current catalog file
      CALL ZORDPN (LSELCA, LERR)
      ENDIF
      IF (LERR) GO TO 800
C
      NORECS = NOPTHS
C
C     Sorting Software interface
C     This sort uses the Harris Computer Utility sort software
      IF (LSORT) THEN
C
      IF (MLEVEL.GT.2) WRITE (MUNIT, *)'Sorting...'
C
C     ENDFILE ISUNIT(1)                                                 H
C     REWIND ISUNIT(1)                                                  H
C     CLOSE (UNIT=ISUNIT(1))                                            Mu
C
C     Is the input file to large to sort?)
C     CALL FILEN ('DSSSORT.IN'//CHAR(0), 0, 'F', CSCRAT,                M
C    * IFSIZ, CSCRAT, CSCRAT, IDUM, ISTAT)                              M
C     IF (IFSIZ.GT.63950) THEN                                          M
C     IERR = -1                                                         M
C     GO TO 340                                                         M
C     ENDIF                                                             M
C
C     I = SYSTEM ('SORT <DSSSORT.IN >DSSSORT.OUT'C)                     M
C     CALL system ('sort <DSSSORT.IN >DSSSORT.OUT'//CHAR(0))            u
C     Check to see if the sort occurred (was there an error?)
C     CALL FILEN ('DSSSORT.OUT'//CHAR(0), 0, 'F', CSCRAT,               M
C    * IFSIZ, CSCRAT, CSCRAT, IDUM, ISTAT)                              M
C
C     IF ((IFSIZ.GT.50).AND.(ISTAT.EQ.0)) THEN                          M
C     OPEN (UNIT=ISUNIT(2), FILE='DSSSORT.OUT', STATUS='OLD',           Mu
C    * IOSTAT=IERR)                                                     Mu
C     ELSE                                                              M
C     IERR = -1                                                         M
C     ENDIF                                                             M
C
 340  CONTINUE
C     IF (IERR.NE.0) THEN                                               Mu
C     WRITE (MUNIT, 400) IERR                                           Mu
C400  FORMAT (' ZCAT:  Insufficient Space Available for Sort:',I6,/)    Mu
C     LCDCAT = .FALSE.                                                  Mu
C     OPEN (UNIT=ISUNIT(2), FILE='DSSSORT.IN', STATUS='OLD',            Mu
C    * IOSTAT=IERR)                                                     Mu
C     IF (IERR.NE.0) GO TO 910                                          Mu
C     Rewrite the title to the catalog
C     REWIND (ICUNIT)                                                   Mu
C     LSORT = .FALSE.                                                   Mu
C     CALL ZCATIT (IFLTAB, ICUNIT, LSELCA, .FALSE., CINST, LERR)        Mu
C     LSORT = .TRUE.                                                    Mu
C     IF (LERR) GO TO 910                                               Mu
C     ELSE                                                              Mu
C     OPEN (UNIT=ISUNIT(1), FILE='DSSSORT.IN', IOSTAT=IERR)             Mu
C     ENDIF                                                             Mu
C
C     Call the sort initializer
C     CALL MSINIT(4HSORT)                                               H
C
C     Indicate the reocrd length
C     CALL MSREC(MTOTAL+128)                                            H
C
C     Identify scratch Files Available (already assigned)
C     CALL MSFILE(2HIN,ISUNIT(1))                                       H
C     CALL MSFILE(3HOUT,ISUNIT(2))                                      H
C     CALL MSFILE(4HWORK,ISUNIT(3))                                     H
C     CALL MSFILE(4HWORK,ISUNIT(4))                                     H
C
C     The sort is based on ASCII characters
C     CALL MSMODE(8HSYMBOLIC)                                           H
C
C     Define the sort sequence
C     CALL MSFLD (1, MTOTAL, 9HASCENDING)                               H
C
C     Now do the sort
C     CALL MS                                                           H
C
C     Transform the sorted output file into a catalog
C
C     ENDFILE ISUNIT(2)                                                 H
C     REWIND ISUNIT(2)                                                  H
C
      IF (LCDCAT) CALL ZCATIT (IFLTAB, ICDUNT,
     * LSELCA, .TRUE., CINST, LERR)
C
      CALL ZCAOUT (IFLTAB, ICUNIT, ICDUNT, LCDCAT, NORECS)
C
C
C     DONE!
      GO TO 740
      ENDIF
C
C     Reached the end of file without getting all the pathnames
C     IF ((LSORT).AND.(MLEVEL.GE.1)) WRITE (MUNIT, 720)                 Mu
C720  FORMAT (' WARNING:  Incomplete Catalog (Insufficient Disk'        Mu
C    * ' Space to Complete Sorted Catalog)')                            Mu
C
 740  CONTINUE
C     IF (LDOSRT) CLOSE (UNIT=ISUNIT(1), STATUS='DELETE')               Mu
C     IF (LDOSRT) CLOSE (UNIT=ISUNIT(2), STATUS='DELETE')               Mu
C     IF (LDOSRT) CLOSE (UNIT=ISUNIT(3), STATUS='DELETE')               Mu
C     IF (LDOSRT) THEN                                                  H
C     DO 760 I=1,4                                                      H
C     CLOSE (UNIT=ISUNIT(I))                                            H
C760  CONTINUE                                                          H
C     ENDIF                                                             H
C
C
C     Re-attach the Catalog File with a Normal Assignment
 800  CONTINUE
C     ENDFILE (UNIT=ICUNIT)                                             g
C     IF (INUNIT.EQ.0) THEN                                             HM
C     CLOSE (UNIT=ICUNIT)                                               HM
C     OPEN (UNIT=ICUNIT,FILE=CNAME,IOSTAT=IERR)                         H
C     CALL CHRLNB (CNAME, ILAST)                                        M
C     OPEN ( UNIT=ICUNIT, FILE=CNAME(1:ILAST), IOSTAT=IERR,             M
C    * SHARE='DENYNONE')                                                M
C     IF (IERR.NE.0) GO TO 940                                          HM
C     ENDIF                                                             HM
C
C     IF ((CDNAME(1:2).NE.'  ').AND.(ICDUNT.GT.0)) THEN                 HM
C     CLOSE (UNIT=ICDUNT)                                               HM
C     OPEN (UNIT=ICDUNT,FILE=CDNAME,IOSTAT=IERR)                        H
C     CALL CHRLNB (CDNAME, ILAST)                                       M
C     OPEN ( UNIT=ICDUNT, FILE=CDNAME(1:ILAST), IOSTAT=IERR,            M
C    * SHARE='DENYNONE')                                                M
C     IF (IERR.NE.0) GO TO 950                                          HM
C     ENDIF                                                             HM
C
 820  CONTINUE
      LMAP = .FALSE.
      IF (MLEVEL.GE.11) WRITE (MUNIT,840)                               D
 840  FORMAT (T6,'-----DSS---Debug:  Exit ZCAT')                        D
      RETURN
C
C
C     Error Conditions
C     No Records in the DSS File
 900  CONTINUE
      WRITE (MUNIT,901)
 901  FORMAT (/' **** ERROR - ZCAT:  No Records in DSS ',
     * 'File (Catalog not Created) ****',/)
      GO TO 820
C
C     Error During Write
 910  CONTINUE
      WRITE (MUNIT,911)
 911  FORMAT(/' **** ERROR - ZCAT:  Error During Write to',
     * ' the Catalog File ***',/,' Unable to Complete Catalog',/)
      GO TO 740
C
C     Catalog File Not Attached
C920  CONTINUE                                                          HM
C     WRITE (MUNIT,921)                                                 HM
C921  FORMAT (/' **** ERROR - ZCAT:  Catalog File Not ',                HM
C    * 'Attached (New Catalog Not Created)',/)                          HM
C     GO TO 800                                                         HM
C
C     Error during Exclusive Assign
C930  CONTINUE                                                          HM
C     WRITE (MUNIT,931)                                                 HM
C931  FORMAT (/' **** ERROR - ZCAT:  Catalog file Currently in use;',   HM
C    * /,' Cannot Create New Catalog at this Time.',/)                  HM
C     GO TO 800                                                         HM
C
C     Could not reopen the catalog file
C940  CONTINUE                                                          HM
C     WRITE (MUNIT,941) CNAME                                           HM
C941  FORMAT (/' **** ERROR - ZCAT:  Cannot Re-open Catalog File',/     HM
C    * ' Catalog File: ',A,/)                                           HM
C     GO TO 820                                                         HM
C
C     Could not reopen the catalog file
C950  CONTINUE                                                          HM
C     WRITE (MUNIT,951) CDNAME                                          HM
C951  FORMAT (/' ****CAUTION - ZCAT:  Cannot Re-open Condensed Catalog',HM
C    * /,' Condensed Catalog File: ',A,/)                               HM
C     GO TO 820                                                         HM
C
C     Error in selective parameter
 960  CONTINUE
      WRITE (MUNIT,961)
 961  FORMAT (' **** ERROR - ZCAT:  Invalid Selective Catalog',
     * ' Parameter ***')
      GO TO 800
C
      END
