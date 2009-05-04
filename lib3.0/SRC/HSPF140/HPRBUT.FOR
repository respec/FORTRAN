C
C
C
      MODULE HMMSG
C
C     + + + PURPOSE + + +
C     module containing utility modules for reporting HSPF messages
C
C     + + + HISTORY + + +
C     2009/04/30 JLK - initial implementation of module version  
C
      IMPLICIT NONE
C
C     + + + MODULE VARIABLES
      INTEGER                  :: ICNT,RCNT,DATIM(5),CCNT
      INTEGER                  :: MXMSI,MXMSR,MXMSC
      INTEGER, ALLOCATABLE     :: IMSVL(:)
      REAL, ALLOCATABLE        :: RMSVL(:)
      CHARACTER*1, ALLOCATABLE :: CMSVL(:)
      INTEGER, PRIVATE         :: MAXCNT,MAXGRP,MAXACT,RECCNT
      INTEGER, PRIVATE         :: ILEN,RLEN,SDIG,DECP,SCLU,SGRP
      LOGICAL, PRIVATE         :: INIT_NEEDED = .TRUE.
C     TODO: make size of message dynamic      
      CHARACTER*80, PRIVATE    :: MESSAGE(40)
C
      CONTAINS
C
C
C
      SUBROUTINE   INITMSGQ 
C      
C     + + + PURPOSE + + +
C     quick init of module arrays with devault parms
C  
C     + + + PARAMETERS + + +    
      INCLUDE 'phmsg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INIT_NEEDED) THEN
        CALL INITMSG(MXMSI,MXMSR,MXMSC)
      END IF
C      
      RETURN
      END SUBROUTINE      
C
C
C      
      SUBROUTINE   INITMSG
     M                    (AMXMSI,AMXMSR,AMXMSC)
C
C     + + + PURPOSE + + +
C     allocate message arrays
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   AMXMSI, AMXMSR, AMXMSC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AMXMSI - size of integer value array
C     AMXMSR - size of real value array
C     AMXMSC - size of character value array
C
C     + + + END SPECIFICATIONS + + +
C     
      IF (INIT_NEEDED) THEN
        ALLOCATE (IMSVL(AMXMSI))
        ALLOCATE (RMSVL(AMXMSR))
        ALLOCATE (CMSVL(AMXMSC))
        MXMSI = AMXMSI
        MXMSR = AMXMSR
        MXMSC = AMXMSC
        INIT_NEEDED = .FALSE.
      ELSE
        AMXMSI = UBOUND(IMSVL,1)
        AMXMSR = UBOUND(RMSVL,1)
        AMXMSC = UBOUND(CMSVL,1)
      END IF
C      
      END SUBROUTINE
C
C
C
      SUBROUTINE   OMSGX (MESSU,
     M                    COUNT)
C           
C     + + + PURPOSE + + +
C     output an error or warning message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,COUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write message to
C     COUNT  - count of messages written
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MAXCLU,I0,JLEN,IREC,
     #             INITFG,OLEN,CONT,IPOS,RPOS,CPOS,TPOS
      INTEGER      I6
      CHARACTER*80 TBUFF
      CHARACTER*64 FNAME
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TBUFF,TBUF1)
      CHARACTER*1  TBUF1(80)
C      
C     + + + EXTERNALS + + +
      EXTERNAL     DECCHX,INTCHR,HDMES3
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' **************************************************',
     $        '**********************************')
 2010 FORMAT (' ')
 2020 FORMAT (' * ',A80,' *')
 2040 FORMAT (' ERROR/WARNING ID:  ',2I4)
 2050 FORMAT (' DATE/TIME: ',I4,'/',I2,'/',I2,' ',I2,':',I2)
 2060 FORMAT (' *',82X,'*')
C
C     + + + END SPECIFICATIONS + + +
C
      I6 = 6 
C      
C     increment counter for this message
      COUNT= COUNT+ 1
C      
      IF (COUNT .LE. MAXCNT) THEN
C       write detailed error message
        WRITE (MESSU,2010)
        WRITE (MESSU,2010)
C       first write line of asterisks as separator
        WRITE (MESSU,2000)
        WRITE (MESSU,2060)
C       message id
C       WRITE (MESSU,2040) SCLU,SGRP
        WRITE (TBUFF,2040) SCLU, SGRP
        WRITE (MESSU,2020) TBUFF
        CALL HDMES3(I6,TBUFF)
        WRITE (MESSU,2060)
C       initialize position in output arrays
        IPOS= 0
        RPOS= 0
        CPOS= 0
        IREC= 0
 20     CONTINUE
          IREC = IREC + 1
          TBUFF= MESSAGE(IREC)
          IF (TBUFF(1:2) .EQ. '&D') THEN
C           include a date, use standard format
            WRITE(TBUFF,2050) DATIM
          ELSE
C           may include some user spec info
            TPOS= 0
 30         CONTINUE
              TPOS= TPOS+ 1
              IF (TBUFF(TPOS:TPOS+1) .EQ. '&I') THEN
C               an integer
                IPOS= IPOS+ 1
                IF (IPOS .LE. ICNT) THEN
C                 have an integer to write
                  CALL INTCHR (IMSVL(IPOS),ILEN,I0,
     O                         JLEN,TBUF1(TPOS))
                END IF
              ELSE IF (TBUFF(TPOS:TPOS+1) .EQ. '&R') THEN
C               a real
                RPOS= RPOS+ 1
                IF (RPOS .LE. RCNT) THEN
C                 have a real to write
                  DECP= -DECP
                  CALL DECCHX (RMSVL(RPOS),RLEN,SDIG,DECP,
     O                         TBUF1(TPOS))
                END IF
              ELSE IF (TBUFF(TPOS:TPOS+1) .EQ. '&C') THEN
C               include some characters
 40             CONTINUE
                  CPOS= CPOS+ 1
                  TBUF1(TPOS)= CMSVL(CPOS)
                  TPOS= TPOS+ 1
C                 loop back with more characters
                IF (TBUF1(TPOS).EQ.'C' .AND. TPOS.LT.80) GO TO 40
              END IF
C             TODO: use actual length of string              
            IF (TPOS .LT. 79) GO TO 30         
          END IF
C         write the message
          WRITE(MESSU,2020) TBUFF
C         CALL HDMES3(I6,TBUFF)
C         loop back if more text available
        IF (IREC .LT. RECCNT) GO TO 20
C       bottom separator
        WRITE (MESSU,2060)
        WRITE (MESSU,2000)
        WRITE (MESSU,2010)
      END IF
C
      IF (COUNT .EQ. MAXCNT) THEN
C       last time to print, write two lines of asterisks as separator
        WRITE (MESSU,2010)
        WRITE (MESSU,2010)
        WRITE (MESSU,2000)
        WRITE (MESSU,2000)
        WRITE (MESSU,2060)
C       print last time message
        IF (MAXGRP .EQ. 1) THEN
          TBUFF = " The count for the ERROR printed " //
     1            "above has reached its maximum."
          WRITE(MESSU,2020) TBUFF
          TBUFF = ""
          WRITE(MESSU,2020) TBUFF
          TBUFF = " The RUN has been terminated."
        ELSEIF (MAXGRP .EQ. 2) THEN
          TBUFF = " The count for ERRORs generated by the " //
     1            "RUN INTERPRETER is at its maximum."
          WRITE(MESSU,2020) TBUFF
          TBUFF = ""
          WRITE(MESSU,2020) TBUFF
          TBUFF = " The RUN has been terminated."
        ELSEIF (MAXGRP .EQ. 11) THEN
          TBUFF = " The count for the WARNING printed " //
     1            "above has reached its maximum."
          WRITE(MESSU,2020) TBUFF
          TBUFF = ""
          WRITE(MESSU,2020) TBUFF
          TBUFF = " If the condition is encountered again " //
     1            "the message will not be repeated."
          WRITE(MESSU,2020) TBUFF
        END IF
C       write two lines of asterisks as separator to messu
        WRITE (MESSU,2060)
        WRITE (MESSU,2000)
        WRITE (MESSU,2000)
        IF (MAXACT .EQ. 1) THEN
C         this is fatal!
          CALL HDMES3(I6,'Fatal HSPF Error')
          INQUIRE(UNIT=MESSU,NAME=FNAME)
          TBUFF= '  See file ' // FNAME
          CALL HDMES3(I6,TBUFF)
          STOP
        END IF
      END IF
C
C     reset storages
      CALL OMSINI
C
      RETURN
      END SUBROUTINE
C
C
C     
      SUBROUTINE   OMSGMSG
     I                    (MESSU,ASCLU,ASGRP,
     I                     AMAXCNT,AMAXGRP,AMAXACT,
     I                     AILEN,ARLEN,ASDIG,ADECP,
     I                     AMSGLEN,AMSG,
     M                     COUNT)
C
C     + + +  PURPOSE + + +
C     output an error or warning message using argument information
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,ASCLU,ASGRP,AMAXCNT,AMAXGRP,AMAXACT,
     1             AILEN,ARLEN,ASDIG,ADECP,AMSGLEN,COUNT
      CHARACTER*80 AMSG(*)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I      
C  
C     + + + END SPECIFICATIONS + + +
C
      SCLU = ASCLU
      SGRP = ASGRP
      MAXCNT = AMAXCNT
      MAXGRP = AMAXGRP
      MAXACT = AMAXACT
      ILEN  = AILEN
      RLEN  = ARLEN
      SDIG  = ASDIG
      DECP  = ADECP
      DO 10 I = 1, AMSGLEN
        MESSAGE(I) = AMSG(I)
 10   CONTINUE       
C     
      CALL OMSGX (MESSU,COUNT)
C    
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   OMSG
     I                 (MESSU,MESSFL,ASCLU,ASGRP,
     M                  COUNT)
C
C     + + + PURPOSE + + +
C     output an error or warning message with text from message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSU,MESSFL,ASCLU,ASGRP,COUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write message to
C     MESSFL - unit number containing text of message
C     SCLU   - cluster on message file containing message text
C     SGRP   - group on message file containing message text
C     COUNT  - count of messages written
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MAXCLU,JLEN,INITFG,OLEN,CONT 
      CHARACTER*80 TBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TBUFF,TBUF1)
      CHARACTER*1  TBUF1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT 
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (7I5)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = ASCLU
      SGRP = ASGRP
C      
      IF (COUNT .LT. 50) THEN
C       how many will we accept and what do we do when max is reached
        INITFG= 1
        OLEN  = 80
        CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M               OLEN,
     O               TBUF1,CONT)
        INITFG= 0
        READ(TBUFF,1000) MAXCNT,MAXGRP,MAXACT,ILEN,RLEN,SDIG,DECP
        IF (ILEN .EQ. 0) THEN
C         default
          ILEN= 10
        END IF
        IF (RLEN .EQ. 0) THEN
C         default
          RLEN= 10
        END IF
        IF (SDIG .EQ. 0) THEN
C         default
          SDIG= 5
        END IF
        IF (DECP .EQ. 0) THEN
C         default
          DECP= 2
        END IF
      ELSE
C       assume we dont want this again
        MAXCNT= 1
      END IF
C
      RECCNT= 0
 20   CONTINUE
        RECCNT= RECCNT + 1
        OLEN  = 80
        CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M               OLEN,
     O               TBUF1,CONT)
        MESSAGE(RECCNT) = TBUFF
      IF (CONT .EQ. 1) GO TO 20
C     
      CALL OMSGX (MESSU,COUNT)
C            
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   OMSINI
C
C     + + + PURPOSE + + +
C     reset assoc parms to don't write
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      ICNT= 0
      RCNT= 0
      CCNT= 0
      DO 10 I= 1,5
        DATIM(I)= 0
 10   CONTINUE
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   OMSTI
     I                  (IVAL)
C
C     + + + PURPOSE + + +
C     save an integer value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL   - value to save
C
C     + + + END SPECIFICATIONS + + +
C
C     increment counter of values saved
      ICNT= ICNT+ 1
      IF (ICNT .LE. MXMSI) THEN
C       save value
        IMSVL(ICNT)= IVAL
      END IF
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   OMSTR
     I                  (RVAL)
C
C     + + + PURPOSE + + +
C     save an real value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RVAL   - value to save
C
C     + + + END SPECIFICATIONS + + +
C
C     increment counter of values saved
      RCNT= RCNT+ 1
      IF (RCNT .LE. MXMSR) THEN
C       save value
        RMSVL(RCNT)= RVAL
      END IF
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   OMSTC
     I                  (CLEN,CVAL)
C
C     + + + PURPOSE + + +
C     save character value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLEN
      CHARACTER*1 CVAL(CLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLEN   - length of character string
C     CVAL   - character string
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,CLEN
C       increment counter of values saved
        CCNT= CCNT+ 1
        IF (CCNT .LE. MXMSC) THEN
C         save value
          CMSVL(CCNT)= CVAL(I)
        END IF
 10   CONTINUE
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   OMSTD
     I                  (DATE)
C
C     + + + PURPOSE + + +
C     save a date to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date to save
C
C     + + + EXTERNALS + + +
      EXTERNAL   EXDATE
C
C     + + + END SPECIFICATIONS + + +
C
C     save date in external format
      CALL EXDATE (DATE,
     O             DATIM)
C
      RETURN
      END SUBROUTINE
C    
      END MODULE HMMSG
