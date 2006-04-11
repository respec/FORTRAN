      SUBROUTINE PUTHOL ( ISTR, IPOS, ICH)
C
C     Takes the rightmost byte of integer ICH, and places
C     it in position IPOS of hollerith ISTR.
C     May cross work boundaries
C
C     ***** THIS IS A MACHINE DEPENDENT ROUTINE ******
C
      INTEGER ISTR(*)                                                   HMlg
C     For Lahey INT*4 code, we must use an INT*2 word!!
C     INTEGER*2 ISTR(*)                                                 L
      CHARACTER CVAR*10                                                 HMlg
      INTEGER IVAR                                                      HMlg
C     Best way is to 'cheat' by equivalencing an integer and character
      EQUIVALENCE (IVAR,CVAR)                                           HMlg
C
      EXTERNAL BKDATW
      COMMON /WORDS/ IWORD(10)
C
C
C
C     CHECK THAT WORDS HAS BEEN SET
      NCMW = IWORD(7)
      IF ((NCMW.LT.2).OR.(NCMW.GT.10)) THEN
C     WRITE ( 3, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'             H
      WRITE ( 6, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'             LPMAClg
      CALL ABORT
      ENDIF
C
C     THIS CODE IS FOR MACHINES WHERE EQUIVALENCING IS LEGAL
C     CALCULATE THE WORD AND BYTE POSTION
      IWD = (IPOS-1)/NCMW + 1                                           HMlg
      IBYTE = IPOS - ((IWD-1)*NCMW)                                     HMlg
C
C     MOVE THE CHARACTER
      IVAR = ISTR(IWD)                                                  HMlg
      CVAR(IBYTE:IBYTE) = CHAR (ICH)                                    HMlg
      ISTR(IWD) = IVAR                                                  HMlg
C
C
C     THIS CODE IS FOR MACHINES WHERE INTEGER AND CHARACTER
C     CAN BE PASSED AS SAME ARGUMENT
C     ISTR(IPOS) = CHAR(ICH)
C
C
C     This code is where Boolean functions are available.
C     An INTEGER*2 word must be legal
C
C     KWORD = (IPOS-1)/2 + 1                                            L
C     K = MOD(IPOS,2)                                                   L
C
C     IF (K.NE.0) THEN                                                  L
C     Move into the lowest byte
C     ISTR: 11111111 11111111 to 11111111 00000000
C     JCH = IAND (ISTR(KWORD), -256)                                    L
C     ICH: 11111111 11111111 to 00000000 11111111
C     KCH = IAND (ICH, 255)                                             L
C     ISTR: JCH .OR. KCH
C     ISTR(KWORD) = IOR (JCH, KCH)                                      L
C
C     ELSE                                                              L
C     Move into the upper most byte
C     ISTR: 11111111 11111111 to 00000000 11111111
C     JCH = IAND (ISTR(KWORD), 255)                                     L
C     ICH: 00000000 11111111 to 11111111 00000000
C     KCH = ISHFT (ICH, 8)                                              L
C     ISTR: JCH .OR. KCH
C     ISTR(KWORD) = IOR (JCH, KCH)                                      L
C
C     ENDIF                                                             L
C
C     Use math when no other method is available.
C     This procedure will not work when the high order (8th) bit
C     is set.  However, no ANSI characters use this bit, so it is ok
C     as long as the hollerith have been preset to zeros!!
C     KWORD = (IPOS-1)/2 + 1
C     K = MOD(IPOS,2)
C
C     IF (K.NE.0) THEN
C     MOVE INTO LOWEST MOST BYTE
C     JCH = ISTR(KWORD)/256
C     JCH = JCH*256
C     ISTR(KWORD) = JCH + ICH
C
C     ELSE
C     MOVE INTO THE UPPER MOST BYTE
C     JCH = ISTR(KWORD)/256
C     JCH = JCH*256
C     KCH = ISTR(KWORD) - JCH
C     ISTR(KWORD) = KCH + (ICH*256)
C
C     ENDIF
C
      RETURN
      END
C     BLOCK DATA BKDAT                                                  PICALlg
      SUBROUTINE BKDATW                                                 HM
C
C     BLOCK DATA TO SET COMMON BLOCK WORDS
C
C
      COMMON /WORDS/ IWORD(10)
C
C
C     IWORD(1) - NCPW - NUMBER OF CHAR PER PHYSICAL WORD
C     IWORD(2) - NCLW - NUMBER OF CHAR PER LOGICAL WORD
C     IWORD(3) - NBCH - NUMBER OF BITS PER CHARACTER
C     IWORD(4) - MASKA - CHARACTER MASK
C     IWORD(5) - MAS - COMPLEMENT OF MASKA
C     IWORD(6) - NBMW - NUMBER OF BITS PER MACHINE WORD
C     IWORD(7) - NCMW - NUMBER OF CHARACTERS PER MACHINE WORD
C     IWORD(8) - INTEGD - NUMBER OF SINGLE INTEGER WORDS IN
C                         DOUBLE INTEGER WORD
C     IWORD(9) - IREALS - NUMBER OF SINGLE INTEGER WORDS IN
C                         SINGLE REAL WORD
C     IWORD(10) - IREALD - NUMBER OF SINGLE INTEGER WORDS IN
C                          DOUBLE REAL WORD
C
C
C     DATA FOR HARRIS
C     DATA IWORD/3,6,8,'377,'77777400,24,3,2,2,4/                       H
C     DATA FOR CDC
C     DATA IWORD/10,10,6,77B,77777777777777777700B,60,10,1,1,2/         C
C     DATA FOR Unix 32 bit machines
      DATA IWORD/4,4,8,0,0,32,4,1,1,2/                                  Plg
C     DATA FOR IBM-PC USING MS DOS WITH 4 BYTE WORDS
C     DATA IWORD/4,4,8,0,0,32,4,1,1,2/
C     DATA FOR IBM-PC USING MS FORTRAN WITH 2 BYTE WORDS
C     DATA IWORD/2,4,8,0,0,16,2,2,2,4/                                  ML
C     DATA FOR IBM
C     DATA IWORD/4,4,8,ZFF,ZFFFFFF00,32,4,1,1,2/                        I
C
      RETURN                                                            HM
      END
