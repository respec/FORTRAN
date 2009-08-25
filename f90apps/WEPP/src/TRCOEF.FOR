      real function trcoef(shrsol)
cWarning from ftnchek
cFunction TRCOEF may modify argument SHRSOL
c
c************************************************************
c                                                           *
c   This function is called from SR PARAM to compute the    *
c   sediment transport coefficient for rill flow. It calls  *
c   SR YALIN.                                               *
c                                                           *
c************************************************************
c                                                           *
c   Argument                                                *
c      shrsol  : flow shear stress                          *
c                                                           *
c************************************************************
c
      real shrsol,tottc
      
      call yalin(shrsol,tottc)
      trcoef = tottc / shrsol ** 1.5
      if (trcoef.eq.0.0) trcoef = 0.000000001
c
      return
      end
