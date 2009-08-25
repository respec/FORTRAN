
c     begin include file cclim.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /clim/     tmin,tmax,tave,radly,radmj,am(mxhill),
     1                  am2(mxhill),tmnavg,tmxavg,tdpt,wind,vwind,
     1                  r5(mxplan,6),rx(5),iwind,hrsnow(24),hrrain(24),
     1                  hradmj,hrdewp,hrtemp,rpoth,ibrkpt,stmstr,
     1                  vercli,densnf

c     + + + VARIABLE DECLARATIONS + + +

      real              tmin,tmax,tave,radly,radmj,am,am2,tmnavg,
     1                  tmxavg,tdpt,wind,vwind,r5,rx,hrsnow,hrrain,
     1                  hradmj,hrdewp,hrtemp,rpoth,stmstr,densnf
      integer           iwind,ibrkpt,vercli

c     + + + VARIABLE DEFINITIONS + + +

c     tmin           : minimum daily temperature (degrees C)
c     tmax           : maximum daily temperature (degrees C)
c     tave           : average daily temperature (degrees C)
c     radly          : daily solar radiation (Langleys/day)
c     radmj          : daily solar radiation (MJ/m^2/day)
c     am             : five day antecedent moisture for surface
c                      residue (m)
c     am2            : five day antecedent moisture for submerged
c                      residue (m)
c     tmnavg         : five day minimum temperature average (deg C).
c     tmxavg         : five day maximum temperature average (deg C).
c     tdpt           : mean daily dew point temperature (degrees C)
c     wind           : wind direction (degrees from North)
c     vwind          : mean daily wind speed (m/s)
c     r5(mxplan,6)   : five day rainfall total (m).
c     rx(5)          : divisor for rainfall
c     iwind          : wind data flag in climate input file
c                      ( 0 - wind data,  1 - no wind data )
c     hrrain         : hourly rainfall amount--meters of rain. (m)
c     hrsnow         : hourly snowfall amount--meters of snow. (m)
c     hradmj         : hourly radiation distributed over a normal curve
c                      on an hourly basis (MJ/m^2)
c     hrdewp         : Calculated average hourly dew point (C)
c     hrtemp         : Calculated average hourly temperature (C)
c     rpoth          : Calculated daily radiation potential (MJ/m^2/day)
c
c     ibrkpt         : Break point data flag in climate input file
c                      ( 0 - no break point data,  1 - break point data )
c     stmstr         : Storm Start time (hr)
c     vercli         : climate data version
c     densnf         : density of new falling snow (kg/m^3)
c
c     end include file cclim.inc

