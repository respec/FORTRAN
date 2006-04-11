      SUBROUTINE QFCLOS (IFL,I)
        INTEGER, INTENT(IN) :: IFL,I

        IF (I >= 0) THEN
          CLOSE(IFL,ERR=10)
        END IF
 10     CONTINUE

      END SUBROUTINE QFCLOS
