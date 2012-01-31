      !scnmod
      SUBROUTINE F90_SCNDBG (LEV)
        USE SCENMOD, ONLY : M_SETDBG
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SCNDBG

        INTEGER,         INTENT(IN)  ::LEV

        CALL M_SETDBG(LEV)

      END SUBROUTINE F90_SCNDBG

      SUBROUTINE F90_SPIPH (HIN,HOUT)
        ! HIN is handle to pipe sending input to the fort applic
        ! HOUT is handle to pipe sending output from the fort applic
        USE SCENMOD, ONLY : M_SPIPH
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SPIPH

        INTEGER  :: HIN, HOUT

        CALL M_SPIPH(HIN,HOUT)

      END SUBROUTINE F90_SPIPH
