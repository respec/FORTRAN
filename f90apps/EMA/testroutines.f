        program test
        
        call table1
        
        call appx3
        
        stop
        end
        
        subroutine table1
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c      the following program exactly reproduces table 1 in bulletin 17b
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      double precision skew,answer(10),mseg
    
          write(*,*) 'Table 1 from Bulletin 17B'
          write(*,*) ' '
        do 10 skew = 0.0,3.05,.1
          do 20 i=1,10
             answer(i)  =  mseg(10*i,skew)
20        continue
          write(*,'(f4.1,2x,10f6.3)') skew,answer
10      continue
          return
        end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
        subroutine appx3
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c      the following program exactly reproduces table in bulletin 17b appendix 3
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      double precision skew,answer(10),mseg,excpts(31),kf,goff,sksign
      
      data excpts/0.0001,0.0005,0.001,0.002,0.005,0.01,0.02,0.025,0.04,
     1            0.05,0.1,0.2,0.3,0.4,0.4296,0.5,15*0.0/
     
        do 10 i=17,31
          excpts(i) = 1.d0 - excpts(32-i)
10      continue

          write(*,*) ' '
          write(*,*) 'Table from Bulletin 17B Appendix 3'
          write(*,*) ' '
          goff = 0.d0
        do 20 sksign=1.d0,-1.d0,-2.d0
        do 20 goff = 0.0,8.4,0.7
            write(*,'(/,10x,7f10.1,/)') 
     1           (sksign*(goff+0.1d0*k),k=0,6)    
          do 30 ip = 31,1,-1
            write(*,'(f6.4,4x,7f10.5)') excpts(ip),
     1         (kf(sksign*(goff+0.1d0*k),1.d0-excpts(ip)),k=0,6)    
30      continue
20      continue
          return
        end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
        double precision function kf(skew,prob)
        double precision parms(2),prob,skew,fp_g2_icdf
        
        
        if(skew .eq. 0.d0) then
            kf = fp_z_icdf(prob)
        else
            parms(1)  = 4.d0/skew**2
          if(skew .lt. 0.d0) then
            parms(2) = -1.d0/sqrt(parms(1))
          else
            parms(2) =  1.d0/sqrt(parms(1))
          endif  
            kf = (fp_g2_icdf(prob,parms)-parms(1)*parms(2))
        endif
          return
          end
          
        
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
