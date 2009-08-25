
c     begin include file cavloss.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /avloss/   ioutpt,ioutss,ioutas,iroute,
     1                  avsols,avsole,avsolm,
     1                  avsoly,avsolf,avsolc(mxplan),
     1                  dsmon(mxplan,100),dsyear(mxplan,100),
     1                  dsavg(mxplan,100),ofelod(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           ioutpt,ioutss,ioutas,iroute
      real              avsols,avsole,avsolm,avsoly,avsolf,dsmon,
     1                  dsyear,dsavg,ofelod,avsolc

c     + + + VARIABLE DEFINITIONS + + +

c     ioutpt             : output flag for continuous overall
c                          soil loss options
c     ioutss             : output flag for continuous storm
c                          by storm options
c     ioutas             : output flag for continuous
c                          annual storm options
c     iroute             : overland flow routing flag for printing
c                          enrichment output
c     avsols             : storm sediment loss (kg/m^2)
c     avsole             : storm sediment loss (kg/m)
c     avsolc(mxplan)     : storm sediment loss from each OFE
c                          on a contoured hillslope (kg/m)
c     avsolm             : monthly sediment loss (kg/m)
c     avsoly             : annual sediment loss (kg/m)
c     avsolf             : total sediment loss (kg/m)
c     dsmon(mxplan,100)  : monthly sediment loss at each point
c                          for each overland flow element (kg/m^2)
c     dsyear(mxplan,100) : annual sediment loss at each point
c                          for each overland flow element (kg/m^2)
c     dsavg(mxplan,100)  : total sediment loss at each point for each
c                          overland flow element (kg/m^2)
c     ofelod(mxplan)     : Sediment load at last point in OFE from
c                          dslod1 in sloss.for (reset daily) (kg/m)

c     end include file cavloss.inc
