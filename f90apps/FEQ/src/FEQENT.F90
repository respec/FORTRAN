      FUNCTION F90_MJD (YR, MN, DY) RESULT(JD)
        dll_export F90_MJD

        INTEGER,  INTENT(IN)   :: YR, MN, DY
        INTEGER                :: JD
        INTEGER                :: MJD

        JD = MJD(YR, MN, DY)

      END

      SUBROUTINE F90_INVMJD (MJD, YR, MN, DY)
        DLL_EXPORT F90_INVMJD

        INTEGER,  INTENT(IN)   :: MJD
        INTEGER,  INTENT(OUT)  :: YR, MN, DY

        CALL INVMJD (MJD, YR, MN, DY)

      END


      SUBROUTINE F90_LKTAB (ADRS, ARGA, SKLT, FUNC, NTAB, PDV)
        DLL_EXPORT F90_LKTAB

        INTEGER, INTENT(IN)   :: ADRS, SKLT
        REAL,    INTENT(IN)   :: ARGA
        INTEGER, INTENT(OUT)  :: NTAB
        REAL,    INTENT(OUT)  :: FUNC, PDV


        CALL LKTAB(ADRS, ARGA, SKLT, FUNC, NTAB, PDV)

      END

      SUBROUTINE F90_XLKT20 (ADRS, YA, A, T, DT, K, DK, B, DB)
        DLL_EXPORT F90_XLKT20

        INTEGER,INTENT(IN)    :: ADRS
        REAL,   INTENT(INOUT) :: YA
        REAL,   INTENT(OUT)   :: A, T, DT, K, DK, B, DB

        CALL XLKT20 (ADRS, YA, A, T, DT, K, DK, B, DB)

      END

      SUBROUTINE F90_XLKT21 (ADRS, YA, A, T, DT, J, K, DK, B, DB)
        DLL_EXPORT F90_XLKT21

        INTEGER,INTENT(IN)    :: ADRS
        REAL,   INTENT(INOUT) :: YA
        REAL,   INTENT(OUT)   :: A, T, DT, J, K, DK, B, DB

        CALL XLKT21 (ADRS, YA, A, T, DT, J, K, DK, B, DB)

      END

      SUBROUTINE F90_XLKT22 (ADRS,YA,A,T,DT,J,K,DK,B,DB,ALP,DALP,QC)
        DLL_EXPORT F90_XLKT22

        INTEGER,INTENT(IN)   :: ADRS
        REAL,   INTENT(INOUT):: YA
        REAL,   INTENT(OUT)  :: A, T, DT, J, K, DK, B, DB, ALP, DALP, QC

        CALL XLKT22 (ADRS, YA, A, T, DT, J, K, DK, B, DB, ALP, DALP, QC)

      END

      SUBROUTINE F90_XLKT23 (ADRS,YA,A,T,DT,K,DK,B,DB,MA,DMA,MQ,DMQ)
        DLL_EXPORT F90_XLKT23

        INTEGER,INTENT(IN)    :: ADRS
        REAL,   INTENT(INOUT):: YA
        REAL,   INTENT(OUT)  :: A, B, DB, DK, DMA, DMQ, DT, K, MA, MQ, T

        CALL XLKT23 (ADRS,YA,A,T,DT,K,DK,B,DB,MA,DMA,MQ,DMQ)

      END

      SUBROUTINE F90_XLKT24 (ADRS,YA,A,T,DT,J,K,DK,B,DB,MA,DMA,MQ,DMQ)
        DLL_EXPORT F90_XLKT24

        INTEGER,INTENT(IN)    :: ADRS
        REAL,   INTENT(INOUT) :: YA
        REAL,   INTENT(OUT)   :: A,B,DB,DK,DMA,DMQ,DT,J,K,MA,MQ,T

        CALL XLKT24 (ADRS,YA,A,T,DT,J,K,DK,B,DB,MA,DMA,MQ,DMQ)

      END

      SUBROUTINE F90_XLKT25 (ADRS,YA,
     O                     A,T,DT,J,K,DK,B,DB,ALP,DALP,QC,MA,DMA,MQ,DMQ)
        DLL_EXPORT F90_XLKT25

        INTEGER,INTENT(IN)    :: ADRS
        REAL,   INTENT(INOUT) :: YA
        REAL,   INTENT(OUT)   :: A,ALP,B,DALP,DB,DK,DMA,DMQ,DT
        REAL,   INTENT(OUT)   :: J,K,MA,MQ,QC,T

        CALL XLKT25 (ADRS,YA,
     O               A,T,DT,J,K,DK,B,DB,ALP,DALP,QC,MA,DMA,MQ,DMQ)

      END

      SUBROUTINE F90_FILTAB (FNAME,NFREC,IPR,LFTREC)
        DLL_EXPORT F90_FILTAB

        CHARACTER(LEN=*),INTENT(IN)   :: FNAME
        INTEGER,         INTENT(IN)   :: NFREC,IPR,LFTREC

        CHARACTER*64                  :: LNAME

        LNAME = FNAME

        CALL FILTAB (LNAME,NFREC,IPR,LFTREC)

      END

      SUBROUTINE F90_TABTYP (ADDR,TYP)
        DLL_EXPORT F90_TABTYP

        INTEGER           :: ADDR,TYP

        CALL TABTYP(ADDR,TYP)

      END
