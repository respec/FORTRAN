      INTEGER FUNCTION IBCLR (IW,IBIT)                                  HLlg
C
C     SET OF MIL-STD-1753 EXTENTION TO F77
C
C
C     I=MOD(IBIT,24)                                                    H
      I=MOD(IBIT,16)                                                    L
C     I=MOD(IBIT,32)                                                    lg
      IX=1                                                              HLlg
      IX=ISHFT(IX,I)                                                    HLlg
      IX=NOT(IX)                                                        HLlg
      IBCLR=IAND(IW,IX)                                                 HLlg
      RETURN                                                            HLlg
C
      END                                                               HLlg
