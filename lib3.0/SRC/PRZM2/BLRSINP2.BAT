rem 15 subroutine/functions in RSINP2.FOR
copy inprea.for+przmrd.for+iniacc.for+kdcalc.for+mcprz.for+fndchm.for+fndhor.for temp1
copy pzchk.for+maxavg.for+initl.for+rstput.for+rstpt1.for+rstget.for+rstgt1.for+przech.for temp2
copy temp1+temp2 rsinp2.for
erase temp1
erase temp2
