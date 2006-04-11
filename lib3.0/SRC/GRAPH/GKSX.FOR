c     ** control **
      subroutine   gopks(errfil,bufa)
      integer   errfil,bufa
      return
      end
      subroutine   gclks
      return
      end
      subroutine   gopwk(wkid,conid,wtype)
      integer   wkid,conid,wtype
      return
      end
      subroutine   gclwk(wkid)
      integer   wkid
      return
      end
      subroutine   gclrwk(wkid,cofl)
      integer   wkid,cofl
      return
      end
      subroutine   gacwk(wkid)
      integer   wkid
      return
      end
      subroutine   gdawk(wkid)
      integer   wkid
      return
      end
      subroutine   gesc(fctid,lidr,idr,mlodr,lodr,odr)
      integer   fctid,lidr,mlodr,lodr
      character*80 idr(lidr),odr(mlodr)
      return
      end
c     ** output **
      subroutine   gpl(n,pxa,pya)
      integer   n
      real      pxa(n), pya(n)
      return
      end
      subroutine   gpm(n,pxa,pya)
      integer   n
      real      pxa(n), pya(n)
      return
      end
      subroutine   gtx (px,py,chars)
      real      px,py
      character*(*) chars 
      return
      end
      subroutine   gfa(n,pxa,pya)
      integer   n
      real      pxa(n), pya(n)
      return
      end
c     ** output attributes **
      subroutine   gsln (ltype)
      integer   ltype
      return
      end
      subroutine   gslwsc(lwidth)
      real      lwidth
      return
      end
      subroutine   gsplci(pmi)
      integer   pmi
      return
      end
      subroutine   gsmksc(mszsf)
      real      mszsf
      return
      end
      subroutine   gspmci(coli)
      integer   coli
      return
      end
      subroutine   gspmi(pmi)
      integer   pmi
      return
      end
      subroutine   gsmk(mtype)
      integer   mtype 
      return
      end
      subroutine   gstxfp(font,prec)
      integer   font,prec
      return
      end
      subroutine   gschxp(chxp)
      real      chxp
      return
      end
      subroutine   gschsp(chsp)
      real      chsp
      return
      end
      subroutine   gstxci(coli)
      integer   coli
      return
      end
      subroutine   gschh(chh)
      real      chh
      return
      end
      subroutine   gschup(chux,chuy)
      real      chux,chuy
      return
      end
      subroutine   gstxp(txp)
      integer   txp
      return
      end
      subroutine   gstxal(txalh,txalv)
      integer   txalh,txalv
      return
      end
      subroutine   gsfai(fai)
      integer   fai
      return
      end
      subroutine   gsfais(ints)
      integer   ints
      return
      end
      subroutine   gsfasi(styli)
      integer   styli
      return
      end
      subroutine   gsfaci(coli)
      integer   coli
      return
      end
      subroutine   gsasf(lasf)
      integer   lasf(13)
      return
      end
      subroutine   gspmr(wkid,pmi,mtype,mszsf,coli)
      integer   wkid,pmi,mtype,coli
      real      mszsf
      return
      end
      subroutine   gscr(wkid,ci,cr,cg,cb)
      integer   wkid,ci
      real      cr,cg,cb
      return
      end
c     ** transformations **
      subroutine   gswn(tnr,xmin,xmax,ymin,ymax)
      integer   tnr
      real      xmin,xmax,ymin,ymax
      return
      end
      subroutine   gsvp(tnr,xmin,xmax,ymin,ymax)
      integer   tnr
      real      xmin,xmax,ymin,ymax
      return
      end
      subroutine   gselnt(tnr)
      integer   tnr
      return
      end
      subroutine   gsclip(clsw)
      integer   clsw
      return
      end
      subroutine   gswkwn(wkid,xmin,xmax,ymin,ymax)
      integer   wkid
      real      xmin,xmax,ymin,ymax
      return
      end
      subroutine   gswkvp(wkid,xmin,xmax,ymin,ymax)
      integer   wkid
      real      xmin,xmax,ymin,ymax
      return
      end
c     ** input functions **
      subroutine   ginlc(wkid,lcdnr,tnr,ipx,ipy,pet,
     i                   xmin,xmax,ymin,ymax,ldr,datrec)
      integer   wkid,lcdnr,tnr,pet,ldr
      real      ipx,ipy,xmin,xmax,ymin,ymax
      character*80 datrec(ldr)
      return
      end
      subroutine   grqlc(wkid,lcdnr,
     o                   stat,tnr,px,py)
      integer   wkid,lcdnr,stat,tnr
      real      px,py
      return
      end
c     ** inquiry functions **
      subroutine   gqops(opsta)
      integer   opsta
      return
      end
      subroutine   gqchh(errind,chh)
      integer   errind
      real      chh
      return
      end
      subroutine   gqchb(errind,chbx,chby)
      integer   errind
      real      chbx,chby
      return
      end
      subroutine   gqchxp(errind,chxp)
      integer   errind
      real      chxp
      return
      end
      subroutine   gqchsp(errind,chsp)
      integer   errind
      real      chsp
      return
      end
      subroutine   gqmksc(errind,mszsf)
      integer   errind
      real      mszsf
        errind= -999
        mszsf = -999.
      return
      end
      subroutine   gqtxfp(errind,font,prec)
      integer   errind,font,prec
      return
      end
      subroutine   gqwks(wkid,errind,state)
      integer   wkid,errind,state
      return
      end
      subroutine   gqtxx(wkid,px,py,str,errind,cpx,cpy,txexpx,txexpy)
      integer   wkid,errind
      real      px,py,cpx,cpy,txexpx(4),txexpy(4)
      character*(*) str
      return
      end
      subroutine   gqwkca(wtype,
     o                    errind,wkcat)
      integer   wtype,errind,wkcat
      return
      end
      subroutine   gqdsp(wtype,errind,dcunit,rx,ry,lx,ly)
      integer   wtype,errind,dcunit,lx,ly
      real      rx,ry
      return
      end
      subroutine   gqcf(wtype,errind,ncoli,cola,npci)
      integer   wtype,errind,ncoli,cola,npci
      return
      end
      subroutine   gqli(wtype,
     o                  errind,nlcd,nskd,nvld,nchd,npkd,nstd)
      integer   wtype,errind,nlcd,nskd,nvld,nchd,npkd,nstd
      return
      end
      subroutine   gqacwk(n,
     o                    errind,ol,wkid)
      integer   n,errind,ol,wkid
      return
      end
c     ** utility functions **
      subroutine   gprec(il,ia,rl,ra,sl,lstr,str,mldr,
     o                   errind,ldr,datrec)
      integer   il,ia(*),rl,sl,lstr(*),mldr,errind,ldr
      real      ra(*)
      character*(*) str(*)
      character*80  datrec(mldr)
      return
      end
