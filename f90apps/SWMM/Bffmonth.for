      FUNCTION BFFMONTH(ICLL,ISET)
C	TRANSPORT AND EXTRAN BLOCK
CIM
CIM Subroutine written by C. Moore 2/6/96
CIM to compute the monthly base flow factors
CIM
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'MOBFF.INC'
cim 12/98  if iset is zero set factor to 1 and return
      IF (ISET.EQ.0) THEN
      BFFMONTH = 1.0
      RETURN
      ENDIF
cim   ICLL determines if factor is for base flow or dry weather flow (transport)
      SELECT CASE (ICLL)
      CASE (1)
cim if called for constant base wastewater flow from E1 lines and IBFF = 2 set to 1 and return
      IF (IBFF(ISET).EQ.2) THEN
      BFFMONTH = 1.0
      RETURN
      ENDIF
      CASE (2)
cim if called for base wastewater flow and IBFF = 1 set to 1 and return
      IF (IBFF(ISET).EQ.1) THEN
      BFFMONTH = 1.0
      RETURN
      ENDIF
      END SELECT
CIM IF 12 VALUES, JUST USE MONTH TO INDICATE WHICH TO USE
      IF (NUMBFF(ISET).EQ.12) THEN
      BFFMONTH = BFFMO(ISET,MONTH)
      RETURN
      ENDIF
CIM FIRST CHECK IF WE ENTERED A NEW MONTH
CIM NOTE MONTHOLD AND INBFF EQUAL ZERO FIRST TIME, SO ALSO INITIALIZES
      IF (MONTH.NE.MONTHOLD) THEN
      MONTHOLD = MONTH
      DO 10 I=1,NUMSETS
      INBFF(I) = INBFF(I) + 1
CIM THIS NEXT LINE WILL CAUSE FACTORS TO REPEAT
   10 IF (INBFF(I).GT.NUMBFF(I)) INBFF(I)=1
      END IF
      if (iset.le.0) write(n6,*) 'iset is less than zero iset = ',iset
      if (iset.gt.maxsets) write(n6,*) 'iset is greater than maximum ',
     .'number allowed iset = ',iset,' maxsets = ',maxsets
      if (inbff(iset).le.0) write(n6,*) 'inbff(iset) = ',inbff(iset),
     .ISET
      if (inbff(iset).gt.maxbff) write(n6,*) 'inbff(iset) = ',
     .inbff(iset),ISET
      BFFMONTH=BFFMO(ISET,INBFF(ISET))
      RETURN
      END

