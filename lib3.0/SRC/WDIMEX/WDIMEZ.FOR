C
C
C
      SUBROUTINE   PRWMXO
     I                    (MESSFL,WDMSFL,DSN,LTBNAM,LTBIND,ALLFG,
     I                     OUTFL,LSTTYP,ASSCFG,
     M                     AMSFIL,ILIN,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Dummy stub in WDIMEX for the routine that:
C     routine to either export table dataset data
C     (called from PRWMXE) or to list data (called from PRWMXL)
C     to terminal or file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,DSN,LTBIND,ALLFG,
     1            OUTFL,LSTTYP,ASSCFG,AMSFIL,ILIN,RETCOD
      CHARACTER*1 LTBNAM(16)
C
C     + + + ARGUEMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - dataset number on WDM file
C     LTBNAM - table name to be used
C     LTBIND - table index to be used
C     ALLFG  - table names selected indicator flag
C              0 - listing 1 table
C              1 - listing all tables
C     OUTFL  - Fortran unit number to output data to
C     LSTTYP - indicates listing characteristics
C              -2 - exporting all datasets
C              -1 - export one dataset
C               0 - listing to file
C               1 - listing to screen
C     ASSCFG - flag indicating all data (0) or associated only (1)
C     AMSFIL - auxilary message file for table data
C     ILIN   - count of number of lines printed
C     RETCOD - return code
C
C     + + + END SPECIFICATIONS
C
      WRITE (*,*)
      WRITE (*,*) ' ERROR, subroutine PRWMXO has been called,'
      WRITE (*,*) '        but is not included in the program.'
      WRITE (*,*) '        This is probably a fatal error!'
      RETURN
      END
