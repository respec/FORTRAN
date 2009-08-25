      subroutine tmpcft
c
c     + + + PURPOSE + + +
c
c     Fit an air temperature curve form longterm monthly temperature data
c      Codes of Newton method was adapted from Li Wang
c
c     Called from: SR WINIT
c     Author(s): Li Wang, Shuhui Dun, WSU
c     Reference in User Guide: choose minimum square root error
c
c     Version: 2008.
c     Date recoded: January 07, 2008
c     Verified by : Joan Wu, WSU
c

c
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c
c     + + + COMMON BLOCKS + + +
c
      include 'cobclim.inc'
c     Read: obmaxt, obmint
c
      include 'ctcurv.inc'
c     Modify: YavgT,YampT,YpshfT,obavgT
c
c     + + + LOCAL VARIABLES + + +
c
      integer  i,nir
      real Fpcal,Fmin,Mmaxt,Mmint,tol,dpshft,fsdsqe,sddsqe,ThetaT,pai
      real tmpfun,tday
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Fp0, Fp1,Fpcal: function value (mean square error) of corresponding points
c     Fmin: minimum mean square error
c     tol: tolerance
c      Mmaxt: Maximum monthly temperature
c     Mmint: Minimum monthly temperature
c      dpshft: Delta phase shift
c      fsdsqe: First order derivative of the square error
c     sddsqe: second order derivative of the square error
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c
c     + + + DATA INITIALIZATIONS + + +
      tol = 0.00001
      Mmaxt = -100
      Mmint = 100
      pai=acos(-1.0)
c      
c     + + + END SPECIFICATIONS + + +
c
cd      Added by S. Dun, Jan 05, 2008
c      For a long term yearly average temperature in frost simulation
      yavgt = 0
c
      do 10 i = 1, 12
         obavgt(i) = (obmaxt(i) + obmint(i))/2.0
         if (Mmaxt.lt.obavgt(i))  Mmaxt = obavgt(i)
         if (Mmint.gt.obavgt(i))  Mmint = obavgt(i)
         yavgt = yavgt + obavgt(i)
10    continue
c
      yavgt = yavgt/12.
      YampT = (Mmaxt - Mmint)/2.0
c
      if (YpshfT.gt. 0) then
c      check if the Ypshft could be directly used
            if (tmpfun(YpshfT) .lt. 2.0) return
      endif
c
c     Newton method
      dpshft = 1
      nir = 0
      YpshfT = 0

      Do while ((abs(dpshft).gt. tol).and. (nir.lt.20))

         fsdsqe = 0.0
         sddsqe = 0.0
         nir = nir +1

         Do 30 i = 1, 12
             tday = 15. + (i-1)* 30.5
             ThetaT = 2.*pai/365. *(tday-YpshfT)
                        
             fsdsqe = fsdsqe -(YavgT + YampT * sin(thetaT) -obavgt(i))
     1                * cos(thetaT)
c
             sddsqe = sddsqe - 2.*pai/365.* 
     1        ((YavgT - obavgt(i))*sin(thetaT) - YampT*cos(2.*thetaT))
30        continue
c
            if(sddsqe .lt. 0.) then
              YpshfT = YpshfT + 365.0/2.0
            else
c
                dpshft = - fsdsqe/sddsqe
                Ypshft = ypshft + dpshft
c
              Ypshft = mod(Ypshft,365.0)
              if (Ypshft.lt.0) Ypshft = Ypshft + 365.0
          endif
c                  
40      enddo 
c
cd    The alternative method if Newton Mehtod would not converge after 20 iterations
      if(nir.ge.20) then
c      chose minimum square root error
          Fmin = tmpfun(0.)
          YpshfT = 0
c
          do 20 i = 1, 365
              Fpcal = tmpfun(float(i))
                if (Fpcal.lt.Fmin) then
                  Fmin = Fpcal
                  YpshfT = i
              endif
20        continue
      endif

      return
      end