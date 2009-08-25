      subroutine locate(vardp,layern,flyern,tpbtfg)
c
c     +++PURPOSE+++
c     This is a subroutines to locate the soil layer and finer soil layer number
c     of certain depth.
c     Each soil layer was divided into 10 thin layers for frost simulation.
c     In the tillage zone (top two 10cm thick soil layers) the fine layer thickness is 1 cm.
c     While in the untilled zone (20cm thick each layer) the fine layer thickness is 2cm.
c
c     Authors(s):  Shuhui Dun, WSU
c     Date: 02/22/2008
c     Verified by: Joan Wu, WSU
c
c     ----------------------------------------------
c
c     +++ARGUMENT DECLARATIONS+++
      integer layern,flyern,tpbtfg
      real vardp
c
c     +++ARGUMENT DEFINITIONS+++
c     layerN - soil layer number
c     flyerN - finer soil layer number
c     tpbtfg - a flag for  start point or ending point is required,
c              0 for starting point and 1 for ending point.
c     vardp  - depth variable
c
c     +++PARAMETERS+++
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
c
c     +++COMMON BLOCKS+++
      include 'cflgfs.inc'
      include  'cwater.inc'
      include  'cstruc.inc'
c     fine layer for frost simulation
c
c     +++LOCAL VARIABLES+++
c
      real tmpvr1,tmpvr2,tmpvr3,fthick
c
c     + + + DATA INITIALIZATIONS + + +

c     convert depth from cm to mm
      tmpvr1 = vardp *1000
      
c
d     minimum depth is 10mm
      if (tmpvr1 .lt. 10.) then
           layerN = 1
           flyern = 1
           return
      endif
c
c     The top 2 layers (20cm) are each 10cm deep
      if (tmpvr1.lt.200) then
c     In the tillage layer soil layer depth is 10 cm.
           layern = int(tmpvr1/100.) + 1             
           tmpvr2 = mod(tmpvr1,100.)
c          normally soil layer thickness will be 10cm, except 
c          for last layer. nfine() should be set to less fine layers for the last
c          partial layer.        
           fthick = (dg(layern,iplane)*1000)/nfine(layern)      
           flyern = int(tmpvr2/fthick) + 1
           tmpvr3 = mod(tmpvr2,fthick)
      else
c     Below the tillage layer soil layer depth is 20cm
           layern = int(tmpvr1/200.) + 2
           tmpvr2 = mod(tmpvr1,200.)
c          normally soil layer thickness will be 20cm, except 
c          for last layer. nfine() should be set to less fine layers for the last
c          partial layer.            
           fthick = (dg(layern,iplane)*1000)/nfine(layern)
           flyern = int(tmpvr2/fthick) + 1
           tmpvr3 = mod(tmpvr2,fthick)
      endif
c
      if(tpbtfg.eq.1) then
c     Ending point is different than the starting point at soil layer division
          if (abs(tmpvr2).lt.1.0) then
             layern = layern - 1
             flyern = nfine(layern)
           elseif (abs(tmpvr3).lt.1.0) then
             flyern = flyern -1
           endif
      endif
c 
      return
      end
