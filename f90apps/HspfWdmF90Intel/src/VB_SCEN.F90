      !used by HSPFEngineNet and WinHSPFLt
      SUBROUTINE F90_SCNDBG (LEV)
        USE SCENMOD, ONLY : M_SETDBG
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SCNDBG

        INTEGER,         INTENT(IN)  ::LEV

        CALL M_SETDBG(LEV)

      END SUBROUTINE F90_SCNDBG

      !used by HSPFEngineNet and WinHSPFLt
      SUBROUTINE F90_SPIPH (HIN,HOUT)
        ! HIN is handle to pipe sending input to the fort applic
        ! HOUT is handle to pipe sending output from the fort applic
        USE SCENMOD, ONLY : M_SPIPH
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SPIPH

        INTEGER,  INTENT(IN) :: HIN, HOUT

        CALL M_SPIPH(HIN,HOUT)

      END SUBROUTINE F90_SPIPH

      !used by HSPFEngineNet and WinHSPFLt
      SUBROUTINE F90_ACTSCN (MKFILS,WDMFL,MSGFL,RETCOD,CSCEN)
!       activate scenario
        USE SCENMOD, ONLY : M_ACTSCN
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_ACTSCN

        INTEGER,         INTENT(IN)  :: MKFILS
        CHARACTER(LEN=*),INTENT(IN)  :: CSCEN
        INTEGER,         INTENT(IN)  :: WDMFL(4),MSGFL
        INTEGER,         INTENT(OUT) :: RETCOD
        INTEGER                      :: CSLEN 
       
        CSLEN = LEN(CSCEN)
        write(99,*) 'f90_actscn',MKFILS,CSLEN,CSCEN(1:CSLEN)
        CALL M_ACTSCN (MKFILS,CSCEN,WDMFL,MSGFL,RETCOD)
        write(99,*) 'wdmfl',wdmfl(1),wdmfl(2),wdmfl(3),wdmfl(4)

      END SUBROUTINE F90_ACTSCN

      !used by HSPFEngineNet and WinHSPFLt
      SUBROUTINE   F90_SIMSCN (RETCOD)
!       simulate scenario
        USE SCENMOD, ONLY : M_SIMSCN
        !DEC$ ATTRIBUTES DLLEXPORT ::  F90_SIMSCN

        INTEGER, INTENT(OUT)  :: RETCOD

        CALL M_SIMSCN(RETCOD)

      END SUBROUTINE F90_SIMSCN
  
      !used by HSPFEngineNet
      SUBROUTINE F90_FILSTA (MSG)
        USE SCENMOD, ONLY : M_FILSTA
        !DEC$ ATTRIBUTES DLLEXPORT :: f90_filsta
        !DEC$ ATTRIBUTES STDCALL   :: f90_filsta
        !DEC$ ATTRIBUTES REFERENCE :: msg
        CHARACTER(LEN=132),INTENT(IN)  :: msg     
        
        CALL M_FILSTA (MSG)

      END SUBROUTINE F90_FILSTA


