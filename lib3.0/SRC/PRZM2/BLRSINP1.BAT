rem 14 subroutines/functions in RSINP1.FOR
copy initem.for+filopn.for+echof.for+valdat.for+echogd.for+przday.for+tdcalc.for temp1
copy readm.for+mcecho.for+initmc.for+decomp.for+inidat.for+filini.for+thcalc.for temp2
copy temp1+temp2 rsinp1.for
erase temp1
erase temp2
