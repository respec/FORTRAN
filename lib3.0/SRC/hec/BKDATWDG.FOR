C     BLOCK DATA BKDATW                                                 PICALlg
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
