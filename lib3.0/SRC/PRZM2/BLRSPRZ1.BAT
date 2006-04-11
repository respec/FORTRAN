rem 15 subroutines/functions in RSPRZ1.FOR
copy przm.for+khcorr.for+action.for+getmet.for+plgrow.for+irrig.for+furrow.for temp1
copy infil.for+hydrol.for+evpotr.for+hydr1.for+hydr2.for+erosn.for+sltemp.for+farm.for temp2
copy temp1+temp2 rsprz1.for
erase temp1
erase temp2
