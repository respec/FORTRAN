C     ---------------------------------------
C
C     Internal record buffers
      PARAMETER (MXBUFF=6)
C     INTEGER*6 IBUFF(NBSIZE,MXBUFF)                                    H
C     INTEGER IBUFF(NBSIZE,MXBUFF)                                      u
C     INTEGER*4 IBUFF(NBSIZE,MXBUFF), JCREC                             M
      INTEGER*4 IBUFF(NBSIZE+1,MXBUFF), JCREC                           L
      COMMON /ZDSSBZ/ IBUFF, JCREC(MXBUFF), JBUNIT(MXBUFF),
     * JWRITE(MXBUFF), LSBUFF(MXBUFF), LOCKBF(MXBUFF), JBUFF,
     * JMXREC(MXBUFF)
      LOGICAL LSBUFF, LOCKBF
C
C     ---------------------------------------
