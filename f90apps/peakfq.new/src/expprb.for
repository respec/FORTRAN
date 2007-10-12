       double precision function expprob(n,excp)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to compute expected probability
c
c     author.....tim cohn
c     date.......9 Nov 2004
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

       implicit none
       integer n
       double precision excp,nx,fp_z_icdf,fp_tnc_cdf,tp

         nx = n
         tp = fp_z_icdf(1.d0-excp)*sqrt(nx/(nx+1.d0))
         expprob = 1.d0 - fp_tnc_cdf(tp,nx-1.d0,0.d0)

       return
       end
