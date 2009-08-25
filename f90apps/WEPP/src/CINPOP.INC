
c     begin include file cinpop.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /inpop/   rro1(ntype*2),rho1(ntype*2),rint1(ntype*2),
     1                 tdmea1(ntype*2),mfo11(ntype*2),mfo21(ntype*2),
     1                 iop(ntype*2),code1,numof1,cltps1,
     1                 surdi1(ntype*2),iresa1(ntype*2),resad1(ntype*2),
     1                 frmov1(ntype*2),resma1(ntype*2),jdmov1(ntype*2)

c     + + + VARIABLE DECLARATIONS + + +

      real             rro1,rho1,rint1,tdmea1,mfo11,mfo21,surdi1,resad1,
     1                 frmov1
      integer          iop,code1,numof1,cltps1,iresa1,resma1,jdmov1

c     + + + VARIABLE DEFINITIONS + + +
c     (ntype*2)       : maximum number of tillage operation scenarios
c     iop (ntype*2)   : land use category 1=cropland, 2=rangeland,
c                       3=forest, 4=roads
c     rro1(ntype*2)   : ridge roughness for implement (m)
c     rho1(ntype*2)   : ridge height for implement (m)
c     rint1(ntype*2)  : ridge interval for implement (m)
c     tdmea1(ntype*2) : mean tillage depth for implement (m)
c     mfo11(ntype*2)  : tillage intensity for fragile residue (0-1)
c     mfo21(ntype*2)  : tillage intensity for non-fragile residue (0-1)
c     code1           : implement code 1=planter, 2=drill, 3=cultivator
c                       4=other, residue management code for all numbers 10
c                       and above
c     numof1          : number of rows in implement
c     cltps1          : cultivator position 1=front, 2=rear
c     surdi1(ntype*2) : fraction of soil surface disturbed by a tillage
c                       implement pass (0-1)
c     resma1(ntype*2) : residue management option for current operation
c                       10 = residue addition, no disturbance
c                       11 = residue removal, no disturbance
c                       12 = residue addition with disturbance
c                       13 = residue removal with disturbance
c     end include file cinpop.inc