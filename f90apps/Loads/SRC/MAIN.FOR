C
C
C
      PROGRAM   MAIN
C
C     + + + PURPOSE + + +
C     do character based screen interaction for estimator program
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        IEASY,IYRBEG,IYREND,FUNIT,CUNIT,QUNIT,PUNIT,
     1               CMUNIT,NVARS,IPVARS(1000),IPCODE,K,NP,IVAR(200)
      INTEGER        NPVARS,ERRCOD,MESSFG,MESCOD
      REAL           RBEGIN,REND,VERSION
      CHARACTER*20   VNAME(200),VAR_NAME
      CHARACTER*100  FNAME,CNAME,DLNAME,QNAME,PNAME,CMNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL     ESTIMATE,GPCODE,DPLOTI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA VERSION/95.01/
      DATA NPVARS/174/
      DATA VNAME/
     1    'CONSTANT', 'LOG-FLOW','LOG-FLOW SQUARED','SQRT FLOW',
     2    'DEC_TIME', 'DEC_TIME SQUARED','SIN(2*PI*T)','COS(2*PI*T)',
     *    'SIN(4*PI*T)','COS(4*PI*T)','SIN(6*PI*T)','COS(6*PI*T)',
     *    'I-JAN', 'I-FEB', 'I-MAR', 'I-APR', 'I-MAY', 'I-JUN',
     *    'I-JUL', 'I-AUG', 'I-SEP', 'I-OCT', 'I-NOV', 'I-DEC',
     *    'I-1901', 'I-1902', 'I-1903', 'I-1904', 'I-1905',
     *    'I-1906', 'I-1907', 'I-1908', 'I-1909', 'I-1910',
     *    'I-1911', 'I-1912', 'I-1913', 'I-1914', 'I-1915',
     *    'I-1916', 'I-1917', 'I-1918', 'I-1919', 'I-1920',
     *    'I-1921', 'I-1922', 'I-1923', 'I-1924', 'I-1925',
     *    'I-1926', 'I-1927', 'I-1928', 'I-1929', 'I-1930',
     *    'I-1931', 'I-1932', 'I-1933', 'I-1934', 'I-1935',
     *    'I-1936', 'I-1937', 'I-1938', 'I-1939', 'I-2040',
     *    'I-1941', 'I-1942', 'I-1943', 'I-1944', 'I-1945',
     *    'I-1946', 'I-1947', 'I-1948', 'I-1949', 'I-1950',
     *    'I-1951', 'I-1952', 'I-1953', 'I-1954', 'I-1955',
     *    'I-1956', 'I-1957', 'I-1958', 'I-1959', 'I-1960',
     *    'I-1961', 'I-1962', 'I-1963', 'I-1964', 'I-1965',
     *    'I-1966', 'I-1967', 'I-1968', 'I-1969', 'I-1970',
     *    'I-1971', 'I-1972', 'I-1973', 'I-1974', 'I-1975',
     *    'I-1976', 'I-1977', 'I-1978', 'I-1979', 'I-1980',
     *    'I-1981', 'I-1982', 'I-1983', 'I-1984', 'I-1985',
     *    'I-1986', 'I-1987', 'I-1988', 'I-1989', 'I-1990',
     *    'I-1991', 'I-1992', 'I-1993', 'I-1994', 'I-1995',
     *    'I-1996', 'I-1997', 'I-1998', 'I-1999', 'I-2000',
     *    'I-2001', 'I-2002', 'I-2003', 'I-2004', 'I-2005',
     *    'I-2006', 'I-2007', 'I-2008', 'I-2009', 'I-2010',
     *    'I-2011', 'I-2012', 'I-2013', 'I-2014', 'I-2015',
     *    'I-2016', 'I-2017', 'I-2018', 'I-2019', 'I-2020',
     *    'I-2021', 'I-2022', 'I-2023', 'I-2024', 'I-2025',
     *    'I-2026', 'I-2027', 'I-2028', 'I-2029', 'I-2030',
     *    'I-2031', 'I-2032', 'I-2033', 'I-2034', 'I-2035',
     *    'I-2036', 'I-2037', 'I-2038', 'I-2039', 'I-2040',
     *    'I-2041', 'I-2042', 'I-2043', 'I-2044', 'I-2045',
     *    'I-2046', 'I-2047', 'I-2048', 'I-2049', 'I-2050',
     *    26*'NOTHING'/
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(*,*) ' ESTIMATOR PROGRAM FOR CONSTITUENT LOADS'
      WRITE(*,*) ' BASED ON ADJUSTED MLE ESTIMATORS'
      WRITE(*,*) ' '
      WRITE(*,'('' ****  VERSION '',F5.2,''   ****'')') VERSION
      WRITE(*,*) ' TIM COHN @ 703/648-5711'
      WRITE(*,*) ' '
      WRITE(*,*) ' ENTER THE OUTPUT FILE NAME'
      READ(*,'(A100)') FNAME
C
      WRITE(*,*) ' ENTER 0 FOR EXACT VARIANCES, 1 FOR SPEED (0/1)'
      READ(*,*) IEASY
C
      WRITE(*,*) 'ENTER THE ESTIMATION (Q) FILE NAME'
      READ(*,'(A100)') QNAME
C
      WRITE(*,*) ' ENTER THE PSTAT FILE NAME'
      READ(*,'(A100)') PNAME
C
      WRITE(*,*) ' ENTER THE CMND FILE NAME'
      READ(*,'(A100)') CMNAME
C
C     get valid parameter codes
      CMUNIT = 12
      CALL GPCODE (CMUNIT,CMNAME,
     O             NVARS,IPVARS)
      IF (NVARS.GT.0) THEN
        WRITE(*,*)
        WRITE(*,*) ' PARAMETER CODES CONTAINED IN FILE:'
        WRITE(*,*)
        WRITE(*,'(10('' P'',I5.5)/)') (IPVARS(K),K=1,NVARS)
        WRITE(*,*)
        WRITE(*,*) ' ENTER PARAMETER TO USE (I5):'
        READ(*,*) IPCODE
      END IF
C
      WRITE(*,*) ' ENTER BEGIN YEAR FOR CAL. DATA (E.G. 1983.22)'
      READ(*,*) RBEGIN
      WRITE(*,*) ' ENTER ENDING YEAR FOR CALIBRATION DATA'
      READ(*,*) REND
C
      NP  =  0
      WRITE(*,'(//,'' ** DEFINE THE MODEL **'',/)')
      WRITE(*,*) ' ENTER NAMES OF REGRESSORS TO INCLUDE'
      WRITE(*,'(/,4(1X,A20))') (VNAME(I),I=1,12),
     1      VNAME(13),VNAME(14),VNAME(23),VNAME(24),
     2      VNAME(25),VNAME(26),VNAME(173),VNAME(174)
      DO 30 I=1,NPVARS
   33   CONTINUE
          WRITE(*,*) ' ENTER REGRESSOR NAME; BLANK LINE WHEN DONE'
          READ(*,'(A20)') VAR_NAME
          IF(VAR_NAME(1:4) .EQ. '') GOTO 32
          DO 31 J=1,NPVARS
            IF(VAR_NAME .EQ. VNAME(J)) THEN
              NP       =  NP+1
              IVAR(NP) =  J
              GOTO 30
            ENDIF
   31     CONTINUE
          WRITE(*,*) ' RE-ENTER THE NAME OF THE REGRESSOR'
        GOTO 33
   30 CONTINUE
   32 CONTINUE
C
      WRITE(*,'(/,'' ENTER OUTPUT FILE NAME FOR COMPARISON'')')
      WRITE(*,'('' (BLANK TO SKIP) '')')
      READ(*,'(A100)') CNAME
C
      WRITE(*,*) ' ENTER FIRST YEAR FOR ESTIMATION'
      READ(*,*) IYRBEG
      WRITE(*,*) ' ENTER LAST YEAR FOR ESTIMATION'
      READ(*,*) IYREND
C
      WRITE(*,'(/,'' ENTER OUTPUT FILE NAME FOR DAILY LOADS'')')
      WRITE(*,'('' (BLANK TO SKIP) '')')
      READ(*,'(A100)') DLNAME
C
C     set file unit numbers
      FUNIT = 19
      CUNIT = 16
      QUNIT = 15
      PUNIT = 11
C
C     initialize plots
      CALL DPLOTI
C
C     call computational routine
      MESSFG = 1
      CALL ESTIMATE (MESSFG,FNAME,IEASY,CNAME,IYRBEG,IYREND,DLNAME,
     I               FUNIT,CUNIT,QNAME,QUNIT,PNAME,PUNIT,CMUNIT,
     I               IPCODE,RBEGIN,REND,NP,IVAR,CMNAME,
     O               ERRCOD,MESCOD)
C
      IF (ERRCOD .EQ. 1) THEN
        WRITE(*,*) ' ESTIMATION AND CALIBRATION DATA SETS DIFFER'
      ELSE IF (ERRCOD.EQ.2) THEN
        WRITE(*,*) ' PARAMETER CODE VALUE NOT FOUND IN LIST ',IPCODE
      ELSE IF (ERRCOD.EQ.3) THEN
        WRITE(*,*) ' **** MISSING OR NEGATIVE FLOW VALUE ***'
      ELSE IF (ERRCOD.EQ.4) THEN
        WRITE(*,*)  ' YEAR MUST BE BETWEEN 1901 AND 2050'
      END IF
C
      STOP
      END
