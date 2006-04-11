      SUBROUTINE QFCLOS (IFL,I)
        INTEGER, INTENT(IN) :: IFL,I

        IF (I >= 0) THEN
          CLOSE(IFL,ERR=10)
        END IF
 10     CONTINUE

      END SUBROUTINE QFCLOS

      SUBROUTINE GETIND (I,J)
        INTEGER :: I,J

        IF (I .EQ. 0) THEN
          J = 0
        ELSE
          J = 0
        END IF
      END SUBROUTINE GETIND
