      SUBROUTINE ASCRESA
C	EXTRAN BLOCK
      INCLUDE 'TAPES.INC'
      INCLUDE 'ASCOUT1.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'ORF.INC'
C=======================================================================
C
C     WRITE RESULT TRANSFER INFORMATION TO DETAILED ASCII INTERFACE FILE
C
C     CREATED 9/12/97 by Charles I. Moore
C                        Camp Dresser & McKee
C                        Annandale, Va.
C
C=======================================================================
      IREC   = 0
C FIRST WRITE TIME IN HOURS
      IF(ICOUNT.EQ.20) THEN
      ICOUNT=0
      WRITE(IASCII,7000)
      WRITE(IASCII,7020) 10,' Result record'
      ENDIF
      ICOUNT = ICOUNT + 1
      IREC = IREC + 1
      WRITE(IASCII,7040) TIME/3600.0,IREC,ICOUNT
C Write Junction Elevations
      DO J=1,NJ
      IF(ICOUNT.EQ.20) THEN
      ICOUNT=0
      WRITE(IASCII,7000)
      WRITE(IASCII,7020) 10,' Result record'
      ENDIF
      IREC=IREC+1
      ICOUNT=ICOUNT+1
      WRITE(IASCII,7040) Y(J)+Z(J),IREC,ICOUNT
      ENDDO
C   write pump flows
      DO J = 1,NPUMP
      LPNK = LPUMP(J)
      IF(ICOUNT.EQ.20) THEN
      ICOUNT=0
      WRITE(IASCII,7000)
      WRITE(IASCII,7020) 10,' Result record'
      ENDIF
      IREC=IREC+1
      ICOUNT=ICOUNT+1
      WRITE(IASCII,7040) Q(LPNK),IREC,ICOUNT
      ENDDO
C   write orifice flows
      DO J = 1,NORIF
      LPNK = LORIF(J)
      IF(ICOUNT.EQ.20) THEN
      ICOUNT=0
      WRITE(IASCII,7000)
      WRITE(IASCII,7020) 10,' Result record'
      ENDIF
      IREC=IREC+1
      ICOUNT=ICOUNT+1
      WRITE(IASCII,7040) Q(LPNK),IREC,ICOUNT
      ENDDO
C   write weir flows
      DO J = 1,NWEIR
      LWNK = LWEIR(J)
      IF(ICOUNT.EQ.20) THEN
      ICOUNT=0
      WRITE(IASCII,7000)
      WRITE(IASCII,7020) 10,' Result record'
      ENDIF
      IREC=IREC+1
      ICOUNT=ICOUNT+1
      WRITE(IASCII,7040) Q(LWNK),IREC,ICOUNT
      ENDDO
C now do conduit el 1 , flow, el2 for each
C note:  I tried using H(J,1) and H(J,2) arrays, but values when
C        this is called are the half-step values.
      DO J = 1,NC
      J1 = NJUNC(J,1)
      J2 = NJUNC(J,2)
      EL1 = Y(J1)+Z(J1)
      EINV1 = ZU(J)
cim these checks are somewhat simplified from those in head
c   it may be necessary to update later.
      IF (((EL1-EINV1).LT.0.0).AND.(Q(J).LT.0.0)) THEN
      CALL DEPTHX(J,NKLASS(J),Q(J),YC,YNORM)
      EL1 = EINV1 + AMIN1(YC,YNORM)
      ENDIF
      EL1 = AMAX1(EL1,EINV1)
      EL2 = Y(J2)+Z(J2)
      EINV2 = ZD(J)
cim these checks are somewhat simplified from those in head
c   it may be necessary to update later.
      IF (((EL2-EINV2).LT.0.0).AND.(Q(J).GT.0.0)) THEN
      CALL DEPTHX(J,NKLASS(J),Q(J),YC,YNORM)
      EL2 = EINV2 + AMIN1(YC,YNORM)
      ENDIF
      EL2 = AMAX1(EL2,EINV2)
      IF(ICOUNT.EQ.20) THEN
      ICOUNT=0
      WRITE(IASCII,7000)
      WRITE(IASCII,7020) 10,' Result record'
      ENDIF
      IREC=IREC+1
      ICOUNT=ICOUNT+1
      WRITE(IASCII,7040) EL1,IREC,ICOUNT
      IF(ICOUNT.EQ.20) THEN
      ICOUNT=0
      WRITE(IASCII,7000)
      WRITE(IASCII,7020) 10,' Result record'
      ENDIF
      IREC=IREC+1
      ICOUNT=ICOUNT+1
      WRITE(IASCII,7040) Q(J),IREC,ICOUNT
      IF(ICOUNT.EQ.20) THEN
      ICOUNT=0
      WRITE(IASCII,7000)
      WRITE(IASCII,7020) 10,' Result record'
      ENDIF
      IREC=IREC+1
      ICOUNT=ICOUNT+1
      WRITE(IASCII,7040) EL2,IREC,ICOUNT
      ENDDO
      RETURN
7000  FORMAT('* =    RESULT DESC.                         ==')
7020  FORMAT(I10,' * RecSel     <<< ',A50)
7040  FORMAT(1PE20.10,'   <<< ',2I3,A50)
      END


      SUBROUTINE ASCRESF
      INCLUDE 'TAPES.INC'
      INCLUDE 'ASCOUT1.INC'
C=======================================================================
C
C     WRITE RESULT TRANSFER INFORMATION TO DETAILED ASCII INTERFACE FILE
C
C     THIS PADS OUT REMAINING ICOUNTS AND CLOSES FILE
C
C     CREATED 9/12/97 by Charles I. Moore
C                        Camp Dresser & McKee
C                        Annandale, Va.
C
C=======================================================================
      IREC   = 0
      WRITE(IASCII,6090)
      DO I = ICOUNT+1,20
      IREC=IREC+1
      WRITE(IASCII,7000) 0.0,IREC,I
      ENDDO
      CLOSE(IASCII)
      RETURN
6090  FORMAT('* = PAD REMAINING FIELDS OF 20 LINE RECORD WITH ZEROS')
7000  FORMAT(1PE20.10,'   <<< ',2I3,A50)
      END
