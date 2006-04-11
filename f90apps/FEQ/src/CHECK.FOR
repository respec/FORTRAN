       program check

       integer i,k,j(512),ps,pe
       real*4  r(512)

       open(10,file='..\test\test.ftf',form='unformatted')

       do i = 1,10
         read(10) j
         do k = 1, 64
           ps = 1+(k-1)*8
           pe = ps+7
           write(*,'(i4,8(1x,i10))') ps-1,j(ps:pe)
         end do
       end do

       close(10)

       end
