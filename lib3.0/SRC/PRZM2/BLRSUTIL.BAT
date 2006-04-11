rem 27 subroutines/functions in RSUTIL.FOR
copy addstr.for+bmpchr.for+center.for+clear.for+comrd.for+comrd2.for+comrd3.for temp1
copy dyjdy.for+echord.for+elpse.for+errchk.for+expchk.for+filchk.for+filclo.for temp2
copy lftjus.for+lnchk.for+lngstr.for+logchk.for+namfix.for+opecho.for+pzdspl.for+pzscrn.for temp3
copy reltst.for+sqrchk.for+subin.for+subout.for+trclin.for temp4
copy temp1+temp2+temp3+temp4 rsutil.for
erase temp1
erase temp2
erase temp3
erase temp4
