*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vwrtpt.f,v 1.1 1998/07/07 20:10:45 grogers Exp $
*  vwrtpt.f
*
      SUBROUTINE vWRTPT (ITYPE, NCONV, NPOINT, VRSION, NAME,
     +                  IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                  GHT, IPAGE, PAGE, SCREEN)
*
*  Purpose:  Write the computations to output file (and screen).
****************************************************************
*
*  $Log: vwrtpt.f,v $
*  Revision 1.1  1998/07/07 20:10:45  grogers
*  PR#0, initial add of vertcon_lib
*
*
      REAL VRSION
      REAL SLA, SLO
      INTEGER ITYPE, IPAGE
      INTEGER IDLA, IMLA, IDLO, IMLO
      CHARACTER*40 NAME
      LOGICAL PAGE, SCREEN
      CHARACTER*80 CARD
      CHARACTER*96 B96
      COMMON /vCURNT/ B96
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
      EQUIVALENCE(CARD,B96)
* PAGE NUMBER COUNTER
* this is where you change how many on a page
        IF ( (MOD(NPOINT,13) .EQ. 0) .OR. (NPOINT.EQ. 1)) THEN
          PAGE = .TRUE.
          IPAGE = IPAGE + 1
        ENDIF
** WRITE TO OUTPUT FILE
        IF (ITYPE .EQ. 0) THEN
* ONLY INTERACTIVE USE - NO INPUT FILE
          CALL PRINT (NOUT, NPOINT, NAME, VRSION, 
     +     IDLA, IMLA, SLA, IDLO, IMLO, SLO, GHT, IPAGE, PAGE)
        ELSEIF (ITYPE .EQ. 1) THEN
* FOR FREE FORMAT TYPE 1
          CALL PRINT (NOUT, NPOINT, NAME, VRSION, 
     +     IDLA, IMLA, SLA, IDLO, IMLO, SLO, GHT, IPAGE, PAGE)
        ELSEIF (ITYPE .EQ. 2) THEN
* FOR FREE FORMAT TYPE 2
          CALL vPRINT2 (NOUT, NCONV, VRSION, GHT)
        ELSEIF (ITYPE .EQ. 3) THEN
* NGS BLUE BOOK FORMAT TYPE 3
          CALL vPRINT3 (NOUT, NCONV, VRSION, GHT)
        ELSEIF (ITYPE .EQ. 4) THEN
* NGS BM FORMAT TYPE 4
          CALL PRINT4 (NOUT, NCONV, VRSION, GHT)
        ENDIF
* FOR SCREEN OUTPUT
        IF (SCREEN) THEN
          CALL PRINT (LDUMP, NPOINT, NAME, VRSION, 
     +      IDLA, IMLA, SLA, IDLO, IMLO, SLO, GHT, IPAGE, PAGE)
        ENDIF
      RETURN
      END
