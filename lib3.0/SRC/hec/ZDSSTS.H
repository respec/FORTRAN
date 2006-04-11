C     ---------------------------------------
C
C     DSS Time-Series Buffer Common Block
C     PARAMETER (KLBUFF=6000,NIBUFF=100)                                Hu
      PARAMETER (KLBUFF=2400,NIBUFF=100)                                ML
C     PARAMETER (KLBUFF=4000,NIBUFF=100)
      REAL BUFF(KLBUFF)
C     INTEGER*6 ILBUFF(KLBUFF), INTBUF(NIBUFF)                          H
      INTEGER*4 ILBUFF(KLBUFF), INTBUF(NIBUFF)                          ML
C     INTEGER ILBUFF(KLBUFF), INTBUF(NIBUFF)                            u
      EQUIVALENCE (ILBUFF,BUFF)
      COMMON /ZDSSTS/ BUFF, INTBUF
C
C     ---------------------------------------
C
