c     SUBROUTINE VB_SLREG (Y,X,N,SLOPE,CEPT)
c       dll_export VB_SLREG
c       INTEGER  :: N
c       REAL     :: Y(N),X(N),SLOPE,CEPT
c
c       CALL SLREG (Y,X,N,SLOPE,CEPT)
c
c     END SUBROUTINE VB_SLREG
c
c     SUBROUTINE VB_SLREG2 (Y,X,N,B0,B1,B2)
c       dll_export VB_SLREG2
c       INTEGER  :: N
c       REAL     :: Y(N),X(N),B0,B1,B2
c
c       CALL SLREG2 (Y,X,N,B0,B1,B2)
c
c     END SUBROUTINE VB_SLREG2

      REAL FUNCTION VB_GAUSEX (EXPROB)
      dll_export VB_GAUSEX
      REAL       :: EXPROB

      VB_GAUSEX = GAUSEX (EXPROB)

      END FUNCTION VB_GAUSEX

      REAL FUNCTION VB_HARTRG (R)
      dll_export VB_HARTRG
      REAL       :: R

      VB_HARTRG = HARTRG (R)

      END FUNCTION VB_HARTRG

      REAL FUNCTION VB_WILFRT (SKU,ZETA,
     O                         ERRFLG)
      dll_export VB_WILFRT
      INTEGER    :: ERRFLG
      REAL       :: SKU,ZETA

      VB_WILFRT = WILFRT (SKU,ZETA,
     O                    ERRFLG)

      END FUNCTION VB_WILFRT

      SUBROUTINE VB_SCALIT (ITYPE,MMIN,MMAX,
     O                      PLMN,PLMX)
        dll_export VB_SCALIT
        INTEGER  :: ITYPE
        REAL     :: MMIN,MMAX,PLMN,PLMX

        CALL SCALIT (ITYPE,MMIN,MMAX,PLMN,PLMX)

      END SUBROUTINE VB_SCALIT

      REAL FUNCTION VB_RNDLOW (PX)
      dll_export VB_RNDLOW

      VB_RNDLOW = RNDLOW (PX)

      END FUNCTION VB_RNDLOW

      SUBROUTINE VB_GPDATR (INUM,IPOS,NV,ARRA,
     O                      RETCOD)
        dll_export VB_GPDATR
        INTEGER  :: INUM,IPOS,NV,RETCOD
        REAL     :: ARRA(NV)

        CALL GPDATR (INUM,IPOS,NV,ARRA,
     O               RETCOD)

      END SUBROUTINE VB_GPDATR

      SUBROUTINE VB_GPNCRV (ICRV,IVAR)
        dll_export VB_GPNCRV
        INTEGER  :: ICRV,IVAR

        CALL GPNCRV (ICRV,IVAR)

      END SUBROUTINE VB_GPNCRV
