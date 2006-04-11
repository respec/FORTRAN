rem 13 subroutines/functions in RSVADO.FOR
copy vadoft.for+vadcal.for+varcal.for+hfintp.for+interp.for+assemt.for temp1
copy assemf.for+tridiv.for+vswcom.for+swfun.for+pkwfun.for+dswfun.for+balchk.for temp2
copy temp1+temp2 rsvado.for
erase temp1
erase temp2
