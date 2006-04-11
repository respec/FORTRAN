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
