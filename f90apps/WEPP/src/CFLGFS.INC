
c     begin include file cflgfs.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /flgfs/ fgfrst(10,mxnsl,mxplan),
     1         slfsd(10,mxnsl,mxplan),
     1         slsw(10,mxnsl,mxplan), slsic(10,mxnsl,mxplan),
     1         sltime(10,mxnsl,mxplan),nfine(mxnsl),
     1         fineTop,fineBot,wintRed        
 
c     + + + VARIABLE DECLARATIONS + + +

      integer  fgfrst,nfine,fineTop,fineBot,wintRed
      real     slfsd,slsw,slsic,sltime

c     + + + VARIABLE DEFINITIONS + + +

c     fgfrst: flag for frost or not in each 1 cm or 2cm of the soil in the profile
c              0 -- no frost;
c              1 -- totally frozen
c              2 -- frozen at the top of the thin layer
c              3 -- frozen at the bottom of the thin layer
c 
c     slfsd: thickness of the frozen layer in a thin layer (m)  
c     slsw : liquid soil water content in the thin layer (-)
c     slsic: ice amount in the thin layer (m)
c     sltime: the time of water redistribution has used for a fine layer (s)
c     nfine: number of fine layers for each major soil layer
c     fineTop: number of fine layers in each of top 2 10cm layers
c     fineBot: number of fine layers in remaining layers
c     wintRed: flag to indicate if water redistribution in frost should be done
c 

c     end include file cflgfs.inc

