      PROGRAM HSPTDRV

      USE TREE

      CHARACTER*64 FILNAM
      CHARACTER*24 RNAME
      CHARACTER*1  BDIR
      INTEGER      RETCOD
      INTEGER      LEVUSE,LEVCNT(MXLEV),LEVPOSMX(MXLEV)

      ! need OUTLEV from COMMON
      INCLUDE 'CRIN1.INC'

      INTEGER      OFL,OPT

      ! get input file name - this is a system-specific routine
      FILNAM= 'HSPFUCI.INP'
      CALL GETCL
     M            (FILNAM)

      CALL HSPF_TREE_INIT (FILNAM,
     O                     RETCOD)
      IF (RETCOD .EQ. 0 .OR. RETCOD .EQ. 1) THEN
        ! run data set interpreted without error
        OFL = 98
        OPEN (UNIT=OFL,FILE='TREE.LIS',RECL=256)
        CALL TREE_SET(NDBG=OUTLEV)

        DO OPT = 1,4
          CALL HSPF_TREE_BUILD(OPT,OUTLEV)
          CALL TREE_ROOT(RNAME)
          WRITE(OFL,*) 'list:"',RNAME(1:LEN_TRIM(RNAME)),'"'
          CALL TREE_SUM(LEVUSE,LEVCNT,LEVPOSMX,RNAME,BDIR)
          WRITE(OFL,*) '^^^^^^^^^^^^^^^^^^^^ TREE UP'
          !CALL TREE_LIS_ALL('U',OFL)
          WRITE(OFL,*) '^^^^^^^^^^^^^^^^^^^^ TREE DOWN'
          !CALL TREE_LIS_ALL('D',OFL)

          CALL TREE_INI

        END DO
      ELSE
        WRITE(*,*) 'HSPTDRV:run interp RETCOD:',RETCOD
      END IF

      END PROGRAM HSPTDRV
