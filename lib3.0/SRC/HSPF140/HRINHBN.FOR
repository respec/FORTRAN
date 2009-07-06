C
C     
C
      SUBROUTINE   HBNDS
     I                  (MSGFL,MESSU,HBNFL,VOLNO,MEMN,MEMSB,GAPST,
     I                   GAPKW1,SYST,SYSKW1,AMDKW1,AMDST,SDATIM,
     I                   EDATIM,TRFLAG,OUTLEV,MAXTTB,IHMFG,
     M                   ECOUNT,
     O                   NUM,DELT,UNT,NTS,GAPCD,AMDCD,
     O                   TABL,TABLR)
C
C     + + + PURPOSE + + +
C     process a reference to a time series dataset in a hbn file
C
C     + + + MODULES + + +
      USE HMMSG, ONLY : OMSTI,OMSG
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MSGFL,MESSU,HBNFL,VOLNO,MEMSB(2),
     $            SDATIM(5),EDATIM(5),TRFLAG,OUTLEV,MAXTTB,IHMFG,
     $            ECOUNT,NUM,DELT,UNT,NTS,GAPCD,AMDCD,TABL(10,MAXTTB)
      REAL        TABLR(10,MAXTTB)
      CHARACTER*6 MEMN
      CHARACTER*4 GAPST,SYST,AMDST
      CHARACTER*1 AMDKW1(12),SYSKW1(8),GAPKW1(8)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - fortran unit number of HSPF message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     HBNFL  - hspf binary file unit number
C     VOLNO  - ???
C     MEMN   - ???
C     MEMSB  - ???
C     GAPST  - ???
C     GAPKW1 - ???
C     SYST   - ???
C     SYSKW1 - ???
C     AMDKW1 - access mode keyword library
C     AMDST  - ???
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TRFLAG - ???
C     OUTLEV - run interpreter output level
C     ECOUNT - count(s) of specific errors
C     MAXTTB - ???
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     NUM    - ???
C     DELT   - simulation time interval in minutes
C     UNT    - ???
C     NTS    - ???
C     GAPCD  - ???
C     AMDCD  - ???
C     TABL   - ???
C     TABLR  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,KIND,QLFG,LREC,RETCOD,SCLU,SGRP,aConstituentIndex
C
C     + + + EXTERNALS + + +
      EXTERNAL  PROHBN
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4,A2)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT(' BEGIN CHECKING/EXPANDING A HSPF BINARY FILE REFERENCE')
 2020 FORMAT(' END CHECKING/EXPANDING A HSPF BINARY FILE REFERENCE')
C
C     + + + HISTORY + + +
C     2009/06/29 - Initial implementation
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 217
      IF (OUTLEV .GT. 6) WRITE(MESSU,2010)
C
      NTS = 0
C     initialize unt in case dataset not found in hspf binary file
C     subroutine memts requires a valid value
      UNT = 0
C      
      CALL PROHBN (MSGFL,MESSU,HBNFL,VOLNO,MEMN,MEMSB,SYST,GAPST,
     I             AMDST,SDATIM,EDATIM,TRFLAG,
     I             SYSKW1,AMDKW1,GAPKW1,IHMFG,
     M             ECOUNT,
     O             DELT,UNT,KIND,GAPCD,AMDCD,QLFG,
     O             aConstituentIndex)
C
      NUM = VOLNO
      NTS = 1
      READ(MEMN,1000) (TABL(I,NTS),I=1,2)
      TABL(3,NTS)  = QLFG
      TABL(4,NTS)  = 1
      TABL(5,NTS)  = 0
      TABL(6,NTS)  = KIND
      TABLR(8,NTS) = 0.0
      TABLR(9,NTS) = 1.0
C        
      IF (OUTLEV .GT. 6) WRITE(MESSU,2020)
C
C
      RETURN
      END
C
C
C
      SUBROUTINE PROHBN
     I                  (MSGFL,MESSU,HBNFL,VOLNO,MEMN,MEMSB,SYST,
     I                   GAPST,AMDST,SDATIM,EDATIM,TRFLAG,
     I                   SYSKW1,AMDKW1,GAPKW1,IHMFG,
     M                   ECOUNT,
     O                   DELT,UNT,KIND,GAPCD,AMDCD,QLFG,
     O                   aConstituentIndex)
C
C     + + + PURPOSE + + +
C     check attributes for a reference to a time series dataset
C     in a hbn file
C
C     + + + MODULES + + +
      USE HMMSG, ONLY : OMSTI,OMSG,OMSTC,OMSGMSG
      USE HSPF_BINARY_INPUT, ONLY : CheckMember
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MSGFL,MESSU,HBNFL,VOLNO,MEMSB(2),
     $            SDATIM(5),EDATIM(5),TRFLAG,IHMFG,
     $            ECOUNT,DELT,UNT,KIND,GAPCD,AMDCD,QLFG,
     $            aConstituentIndex 
      CHARACTER*6 MEMN
      CHARACTER*4 GAPST,SYST,AMDST
      CHARACTER*1 AMDKW1(12),SYSKW1(8),GAPKW1(8)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - fortran unit number of HSPF message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     HBNFL  - hspf binary file unit number
C     VOLNO  - ???
C     MEMN   - ???
C     MEMSB  - ???
C     SYST   - ???
C     GAPST  - ???
C     AMDST  - ???
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TRFLAG - ???
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     SYSKW1 - ???
C     AMDKW1 - access mode keyword library
C     GAPKW1 - ???
C     ECOUNT - count(s) of specific errors
C     DELT   - dataset time interval in minutes
C     UNT    - ???
C     KIND   - ???
C     GAPCD  - ???
C     AMDCD  - ???
C     QLFG   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SAIND,RNSTRT(6),RNEND(6),RETCOD,RETCD1,
     $            RETCD2,DSSTRT(6),DSEND(6),TCODE,TSSTEP,GPFLG,TDSFRC,
     $            RWFLAG,STRTFG,ENDFG,UNTDS,SCLU,SGRP,
     $            I,I1,I2,I3,I4,OK,ERRCOD,ITMP(1),
     $            UNTUSR,TIMFCT(4),AGGRCD,VBTIME,
     $            EMSGCT
      REAL        TSFILL,RTMP(1)
      CHARACTER*4 TSTYP
      CHARACTER*80 EMSG(5)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TSTYP,TSTYP1)
      CHARACTER*1 TSTYP1(4)
      CHARACTER*4  CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    TIMHTW,WDBSGC,WDBSGI,WTFNDT,TIMADD
      EXTERNAL    CHKSTR,CKDATE,WDBSGR
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        TIMFCT/1,60,1440,43200/
C
C     + + + HISTORY + + +
C     2009/06/29 - JLK - initial implementation
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= -999
      I1  = 1
      I2  = 2
      I3  = 3
      I4  = 4
C
C     convert run times from hspf internal format to wdm
      DO 10 I = 1,5
        RNSTRT(I) = SDATIM(I)
        RNEND(I)  = EDATIM(I)
 10   CONTINUE
      RNSTRT(6) = 0
      RNEND(6)  = 0
      CALL TIMHTW (MESSU,MSGFL,
     M             RNSTRT)
      CALL TIMHTW (MESSU,MSGFL,
     M             RNEND)
C
C     need to use BINARY file index - not just hard code 1
      CALL CheckMember(I1, VOLNO, MEMN,
     O                 aConstituentIndex, 
     O                 TCODE, DSSTRT, DSEND, UNTDS, KIND)   
C
      IF (aConstituentIndex .lt. 0) THEN
C       error - dataset tstyp does not match input memn
        WRITE(EMSG(1),*) ' HBN DATASET PROBLEM1',aConstituentIndex
        EMSGCT = 1
        CALL OMSGMSG(MESSU,SCLU,I1,EMSGCT,EMSG,
     M               ECOUNT)
      ELSEIF (aConstituentIndex .eq. 0) THEN
C       error - attribute tstyp not found
        WRITE(EMSG(1),*) ' HBN DATASET PROBLEM2',VOLNO,' ',MEMN
        EMSGCT = 1
        CALL OMSGMSG(MESSU,SCLU,I1,EMSGCT,EMSG,
     M               ECOUNT)
      END IF
C
      IF (TCODE .GE. 2 .AND. TCODE .LE. 4) THEN
C       valid tcode
        DELT = TIMFCT(TCODE)
      ELSE
C       error - invalid tcode for hspf; must be min., hr., or day
C       month ok for source
C       assign dummy time step for subsequent checks
        TCODE  = 2
        TSSTEP = 60
        WRITE(EMSG(1),*) ' HBN DATASET PROBLEM3',VOLNO,' ',MEMN
        EMSGCT = 1
        CALL OMSGMSG(MESSU,SCLU,I1,EMSGCT,EMSG,
     M               ECOUNT)
      END IF
C
C     check time span
      IF (TRFLAG .EQ. 0) THEN
C       source dataset - compare time span of dataset to time span of run
        OK = 0
        IF (aConstituentIndex .gt. 0) THEN
C         move run start time forward 1 'data' interval
C          write(99,*) 'rnstrt',rnstrt
          CALL TIMADD (RNSTRT,TCODE,TSSTEP,I1,
     O                 RNSTRT)
          CALL CKDATE (DSSTRT,RNSTRT,
     O                 STRTFG)
          CALL CKDATE (RNEND,DSEND,
     O                 ENDFG)
          IF (STRTFG .LE. 0 .AND. ENDFG .LE. 0) OK = 1
        END IF
        IF (OK .EQ. 0) THEN
C         error - run time span not within dataset time span
C          write(99,*) dsstrt
C          write(99,*) rnstrt
C          write(99,*) strtfg
C          write(99,*) rnend
C          write(99,*) dsend
C          write(99,*) endfg
          WRITE(EMSG(1),*) ' HBN DATE PROBLEM'
          WRITE(EMSG(2),*) ' START DATE ', DSSTRT
          WRITE(EMSG(3),*) ' END DATA   ', DSEND
          EMSGCT = 3
          CALL OMSGMSG(MESSU,SCLU,I1,EMSGCT,EMSG,
     M                 ECOUNT)
C         missing data not fatal for ihm operation
          IF (IHMFG .EQ. 1) THEN
            WRITE(99,*) 'PROHBN:missing data IGNORE!',HBNFL,VOLNO
            ECOUNT = ECOUNT-1
          END IF
        END IF
      ELSE
C       target dataset - not allowed - TODO: should it be?
        WRITE(EMSG(1),*) ' HBN DATA FORMAT NOT SUPPORTED FOR OUTPUT'
        EMSGCT = 1
        CALL OMSGMSG(MESSU,SCLU,I1,EMSGCT,EMSG,
     M               ECOUNT)
      END IF
C
      CHSTR = SYST
      UNTUSR= CHKSTR(I4,I2,CHSTR1,SYSKW1)
C
      ERRCOD = 0
      IF (RETCOD .EQ. 0 .AND. UNTUSR .NE. 0) THEN
C       dataset units valid & user units valid; error if different
        UNT = UNTDS
        IF (UNTDS .NE. UNTUSR) ERRCOD = 1
      ELSE IF (RETCOD .EQ. 0 .AND. UNTUSR .EQ. 0) THEN
C       dataset units valid & user units invalid; error if user not blank
        UNT = UNTDS
        IF (SYST .NE. '    ') THEN
C         user string was invalid and not blank
          ERRCOD = 2
        END IF
      ELSE IF (RETCOD .NE. 0 .AND. UNTUSR .NE. 0) THEN
C       dataset units invalid & user units valid; no error
        UNT = UNTUSR
      ELSE IF (RETCOD .NE. 0 .AND. UNTUSR .EQ. 0) THEN
C       dataset units invalid & user units invalid; error if user not blank
        UNT    = 1
        IF (SYST .NE. '    ') THEN
C         user string was invalid and not blank
          ERRCOD = 3
        END IF
      END IF
      IF (ERRCOD .NE. 0) THEN
C       unit system error
        WRITE(EMSG(1),*) ' HBN UNITS PROBLEM'
        WRITE(EMSG(2),*) ' HBN FILE UNITS       ', UNT
        WRITE(EMSG(3),*) ' USER SPECIFIED UNITS ', SYST
        EMSGCT = 3
        CALL OMSGMSG(MESSU,SCLU,I1,EMSGCT,EMSG,
     M               ECOUNT)
      END IF
C
C     check kind of dataset - if attribute not found, set
      IF (KIND .LT. 1 .OR. KIND .GT. 3) THEN
C       error, tsform is not found or not valid for hspf
        WRITE(EMSG(1),*) ' HBN DATASET KIND PROBLEM ',KIND
        EMSGCT = 1
        CALL OMSGMSG(MESSU,SCLU,I1,EMSGCT,EMSG,
     M               ECOUNT)
C       set to undefined     
        KIND= 3
      END IF
C
      IF (TRFLAG .EQ. 0) THEN
C       check user-input for gapst; set gapcd
        IF (GAPST .EQ. '    ') THEN
C         default to gap value of undefined
          GAPCD= 1
        ELSE
          CHSTR= GAPST
          GAPCD= CHKSTR(I4,I2,CHSTR1,GAPKW1)
          IF (GAPCD .EQ. 0) THEN
C           error - invalid gap value requested - undefined assumed
            WRITE(EMSG(1),*) ' HBN GAP CODE PROBLEM ',GAPST
            EMSGCT = 1
            CALL OMSGMSG(MESSU,SCLU,I1,EMSGCT,EMSG,
     M                   ECOUNT)
          ELSE
            GAPCD= GAPCD - 1
          END IF
        END IF
      END IF
C
      RETURN
      END
      