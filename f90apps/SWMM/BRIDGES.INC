cim  new common for bridges routing
c     maxhn = maximum number of mannings roughness/station pairs
c     maxst = maximum number of elevation/station pairs
c     maxpr = maximum number of piers
c     maxbr = maximum number of bridges
c     maxbpnt  = number of points in bridge result curves
c     MAXCELL = maximum total number of bridge cell segments
c     maxpolymaximum number of points in any cell polygon
      parameter(maxhn=50,maxst=99,maxpr=50,maxbr=200,maxbpnt=51,
     amaxcell=70,maxpoly=20)
c 
c  brdep - depth
c  brhyd - hydraulic radius
c  brarea - area
c  brtw - bridge top width
      common /bridge1/ NBRGE(NEE),brhyd(maxbr,maxbpnt),
     + brarea(maxbr,maxbpnt),brtw(maxbr,maxbpnt)
