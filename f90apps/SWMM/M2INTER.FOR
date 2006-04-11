      FUNCTION RM2INTER(YNL,YNH,YNORM,YC,KLA,DFULL)
c     This is program written to interpolate YMID correction from two-dimensional table
c     Developed by C. Moore 11/97
c     Table developed by Brett Cunningham
c
      PARAMETER (MAXUP=24,MAXDN=24)
      DIMENSION UPEND(MAXUP),DNEND(MAXDN),FARRAY(MAXUP,MAXDN)
      DATA UPEND/0.000,0.050,0.100,0.150,0.200,0.250,0.300,0.350,0.400,
     .0.450,0.500,0.550,0.600,0.650,0.700,0.750,0.800,0.850,0.900,0.950,
     .0.970,0.990,0.995,1.000/
      DATA DNEND/0.000,0.050,0.100,0.150,0.200,0.250,0.300,0.350,0.400,
     .0.450,0.500,0.550,0.600,0.650,0.700,0.750,0.800,0.850,0.900,0.950,
     .0.970,0.990,0.995,1.000/
c     first dimension in FARRAY should be the up end.
      DATA FARRAY/
     .0.500,0.537,0.554,0.557,0.559,0.566,0.567,0.574,0.580,0.582,0.588,
     .0.594,0.601,0.606,0.616,0.628,0.643,0.662,0.687,0.727,0.752,0.797,
     .0.819,0.871,
c
     .0.500,0.500,0.520,0.532,0.541,0.546,0.554,0.561,0.565,0.571,0.579,
     .0.586,0.591,0.599,0.610,0.622,0.638,0.657,0.683,0.723,0.749,0.795,
     .0.817,0.871,
c
     .0.500,0.500,0.500,0.514,0.524,0.533,0.542,0.548,0.554,0.562,0.569,
     .0.576,0.583,0.592,0.603,0.616,0.632,0.652,0.678,0.719,0.746,0.792,
     .0.815,0.871,
c
     .0.500,0.500,0.500,0.500,0.511,0.521,0.529,0.537,0.545,0.552,0.561,
     .0.567,0.575,0.585,0.597,0.610,0.626,0.646,0.673,0.715,0.742,0.790,
     .0.813,0.870,
c
     .0.500,0.500,0.500,0.500,0.500,0.510,0.519,0.527,0.535,0.544,0.551,
     .0.559,0.568,0.578,0.590,0.604,0.620,0.641,0.668,0.711,0.738,0.787,
     .0.811,0.870,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.509,0.518,0.526,0.534,0.542,
     .0.551,0.560,0.571,0.583,0.598,0.614,0.635,0.663,0.706,0.734,0.784,
     .0.809,0.870,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.509,0.517,0.525,0.534,
     .0.543,0.553,0.564,0.576,0.591,0.608,0.629,0.658,0.702,0.730,0.781,
     .0.806,0.870,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.508,0.517,0.526,
     .0.535,0.545,0.557,0.569,0.584,0.601,0.623,0.651,0.697,0.726,0.778,
     .0.803,0.870,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.509,0.518,
     .0.527,0.537,0.549,0.562,0.576,0.594,0.616,0.645,0.691,0.721,0.774,
     .0.800,0.870,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.509,
     .0.519,0.529,0.541,0.553,0.568,0.586,0.609,0.639,0.685,0.716,0.770,
     .0.797,0.870,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.510,0.520,0.532,0.545,0.560,0.578,0.601,0.631,0.679,0.710,0.766,
     .0.794,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.511,0.522,0.535,0.551,0.569,0.592,0.623,0.671,0.703,0.761,
     .0.790,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.512,0.525,0.540,0.559,0.582,0.613,0.663,0.696,0.755,
     .0.785,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.513,0.529,0.547,0.571,0.603,0.654,0.687,0.749,
     .0.779,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.516,0.534,0.558,0.591,0.643,0.677,0.741,
     .0.773,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.519,0.543,0.576,0.629,0.665,0.731,
     .0.765,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.500,0.524,0.558,0.612,0.650,0.719,
     .0.755,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.534,0.590,0.629,0.702,
     .0.741,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.557,0.598,0.677,
     .0.719,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.542,0.629,
     .0.677,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.590,
     .0.642,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.557,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.869,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500/
cim 11/97  new array containing d/Dfull at which Q is at its maximum
cim        at this depth Q is greater than Q full.
cim        for internal close pipe types 1 - 7
cim         1 circular
cim         2 horseshoe
cim         3 eggshaped
cim         4 basket handle
cim         5 horizontal ellipse
cim         6 vertical ellipse
cim         7 arch
cim   could compute for other types
      DIMENSION QQFULL1(7)
      DATA QQFULL1/0.92,0.92,0.92,0.92,0.92,0.96,0.92/
cim
cim   limit ynorm and ycrit to not exceed qqfull1 X Dfull
cim   to avoid abrupt change in interpolation
cim
      IF (KLA.LE.7) THEN
      YNO = AMIN1(YNORM,QQFULL1(KLA)*DFULL)
      YCC = AMIN1(YC,QQFULL1(KLA)*DFULL)
      IF(YNO.EQ.YCC) THEN
      RM2INTER = 0.5
      RETURN
      ENDIF
      ELSE
      YNO = YNORM
      YCC = YC
      ENDIF
      UPRATIO = (YNL-YCC)/(YNO-YCC)
      DNRATIO = (YNH-YCC)/(YNO-YCC)
c first check ratios for valid ranges
      UPRATIO = AMIN1(UPRATIO,1.0)
      UPRATIO = AMAX1(UPRATIO,0.0)
      DNRATIO = AMIN1(DNRATIO,1.0)
      DNRATIO = AMAX1(DNRATIO,0.0)
c find column and row less than ratios
      IUP = 2
      DO WHILE (UPEND(IUP).LT.UPRATIO)
      IUP = IUP + 1
      ENDDO
      IUP = IUP - 1
      IDN = 2
      DO WHILE (DNEND(IDN).LT.DNRATIO)
      IDN = IDN + 1
      ENDDO
      IDN = IDN - 1
c first interolate on rows
      RATIO = (UPRATIO-UPEND(IUP))/(UPEND(IUP+1)-UPEND(IUP))
      VAL1 = FARRAY(IUP,IDN)+RATIO*(FARRAY(IUP+1,IDN)-FARRAY(IUP,IDN))
      VAL2 = FARRAY(IUP,IDN+1)+
     .       RATIO*(FARRAY(IUP+1,IDN+1)-FARRAY(IUP,IDN+1))
      RATIO = (DNRATIO-DNEND(IDN))/(DNEND(IDN+1)-DNEND(IDN))
      RM2INTER = VAL1 + RATIO * (VAL2-VAL1)
      RETURN
      END

      FUNCTION RS2INTER(YNL,YNH,YNORM,YC,KLA,DFULL)
c      program written to interpolate two-dimensional array for S2 condition
c      by C. Moore 11/99
c      Table by Brett Cunningham
      PARAMETER (MAXUP=12,MAXDN=14)
      DIMENSION UPEND(MAXUP),DNEND(MAXDN),FARRAY(MAXUP,MAXDN)
      DATA UPEND /0.00,0.05,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,
     .0.90,1.00/
      DATA DNEND /0.0,0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,
     .0.8,0.9,1.0/
      DATA FARRAY /
     .0.500,0.131,0.131,0.131,0.131,0.131,0.131,0.131,0.131,0.131,
     .0.131,0.131,
c
     .0.500,0.317,0.279,0.245,0.227,0.215,0.207,0.201,0.195,0.191,
     .0.187,0.184,
c
     .0.500,0.359,0.318,0.279,0.258,0.245,0.235,0.227,0.221,0.216,
     .0.211,0.207,
c
     .0.500,0.500,0.413,0.374,0.350,0.333,0.320,0.309,0.301,0.293,
     .0.287,0.281,
c
     .0.500,0.500,0.500,0.416,0.394,0.377,0.364,0.353,0.344,0.336,
     .0.329,0.323,
c
     .0.500,0.500,0.500,0.500,0.435,0.421,0.410,0.400,0.392,0.384,
     .0.377,0.371,
c
     .0.500,0.500,0.500,0.500,0.500,0.445,0.436,0.428,0.420,0.414,
     .0.407,0.402,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.453,0.446,0.440,0.435,
     .0.429,0.424,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.461,0.456,0.451,
     .0.447,0.442,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.468,0.464,
     .0.461,0.457,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.475,
     .0.473,0.470,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.483,0.481,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.491,
c
     .0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,0.500,
     .0.500,0.500/
cim 11/97  new array containing d/Dfull at which Q is at its maximum
cim        at this depth Q is greater than Q full.
cim        for internal close pipe types 1 - 7
cim         1 circular
cim         2 horseshoe
cim         3 eggshaped
cim         4 basket handle
cim         5 horizontal ellipse
cim         6 vertical ellipse
cim         7 arch
cim   could compute for other types
      DIMENSION QQFULL1(7)
      DATA QQFULL1/0.92,0.92,0.92,0.92,0.92,0.96,0.92/
cim
      IF (KLA.LE.7) THEN
      YNO = AMIN1(YNORM,QQFULL1(KLA)*DFULL)
      YCC = AMIN1(YC,QQFULL1(KLA)*DFULL)
      IF(YNO.EQ.YCC) THEN
      RS2INTER = 0.5
      RETURN
      ENDIF
      ELSE
      YNO = YNORM
      YCC = YC
      END IF
      UPRATIO = (YNL-YNO)/(YCC-YNO)
      DNRATIO = (YNH-YNO)/(YCC-YNO)
c first check ratios for valid ranges
      UPRATIO = AMIN1(UPRATIO,1.0)
      UPRATIO = AMAX1(UPRATIO,0.0)
      DNRATIO = AMIN1(DNRATIO,1.0)
      DNRATIO = AMAX1(DNRATIO,0.0)
c find column and row less than ratios
      IUP = 2
      DO WHILE (UPEND(IUP).LT.UPRATIO)
      IUP = IUP + 1
      ENDDO
      IUP = IUP - 1
      IDN = 2
      DO WHILE (DNEND(IDN).LT.DNRATIO)
      IDN = IDN + 1
      ENDDO
      IDN = IDN - 1
c first interolate on rows
      RATIO = (UPRATIO-UPEND(IUP))/(UPEND(IUP+1)-UPEND(IUP))
      VAL1 = FARRAY(IUP,IDN)+RATIO*(FARRAY(IUP+1,IDN)-FARRAY(IUP,IDN))
      VAL2 = FARRAY(IUP,IDN+1)+
     .       RATIO*(FARRAY(IUP+1,IDN+1)-FARRAY(IUP,IDN+1))
      RATIO = (DNRATIO-DNEND(IDN))/(DNEND(IDN+1)-DNEND(IDN))
      RS2INTER = VAL1 + RATIO * (VAL2-VAL1)
      RETURN
      END

c      PROGRAM TEST
c      DO I = 1,100
c      WRITE(*,*)
c      READ(*,*)   UPEND
c      READ(*,*)   DNEND
c      WRITE (*,*) RM2INTER(UPEND,DNEND)
c      write (*,*) RS2INTER(UPEND,DNEND)
c      ENDDO
c      END
