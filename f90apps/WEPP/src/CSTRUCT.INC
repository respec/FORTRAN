
c     begin include file cstruct.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /struct/   ichan,nchan,ich(0:mxplan),ielmt,nelmt,
     1                  idelmt(0:mxelem),elmt(mxelem),nhleft(mxelem),
     1                  nhrght(mxelem),nhtop(mxelem),ncleft(mxelem),
     1                  ncrght(mxelem),nctop(mxelem),nileft(mxelem),
     1                  nirght(mxelem),nitop(mxelem),hill(0:mxelem),
     1                  ieltyp(mxelem)

c     + + + VARIABLE DECLARATIONS + + +

      integer           ichan,nchan,ich,ielmt,nelmt,idelmt,elmt,
     1                  nhleft,nhrght,nhtop,ncleft,ncrght,nctop,
     1                  nileft,nirght,nitop,hill
      character         ieltyp*11

c     + + + VARIABLE DEFINITIONS + + +

c     ichan             :
c     nchan             :
c     ich(0:mxplan)     :
c     ielmt             :
c     nelmt             :
c     idelmt(0:mxelem)  :
c     elmt(mxelem)      :
c     nhleft(mxelem)    :
c     nhrght(mxelem)    :
c     nhtop(mxelem)     :
c     ncleft(mxelem)    :
c     ncrght(mxelem)    :
c     nctop(mxelem)     :
c     nileft(mxelem)    :
c     nirght(mxelem)    :
c     nitop(mxelem)     :
c     hill(0:mxelem)    :
c     ieltyp(mxelem)    :

c     end include file cstruct.inc

