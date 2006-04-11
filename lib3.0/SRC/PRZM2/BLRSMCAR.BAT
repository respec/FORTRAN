rem 14 subroutines/functions in RSMCAR.FOR
copy random.for+nmb.for+unif.for+exprn.for+empcal.for+mtpv.for+transm.for+transb.for temp1
copy output.for+statis.for+stout.for+outfor.for+frqtab.for+frqplt.for temp2
copy temp1+temp2 rsmcar.for
erase temp1
erase temp2
