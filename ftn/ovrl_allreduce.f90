program overlap_benchmark
  use mpi
  use cresta_sp_coll
  use crestacoll, only: gettime
  implicit none
  integer, dimension(:), allocatable :: cresta_send, cresta_recv
  real, dimension(200,200) :: A, B, C
  real(8) :: comp_time, coll_time, tot_time, ovl_time, cmpt0, cmpt1, colt0, &
             colt1, tott0, tott1, compave, collave, totave, ovlave, ovlp
  integer :: n, imsg, i, cnt, iter, maxiter
  integer, parameter :: large_msg = 8192
  character(len=25) :: filename 
  integer :: my_id, ntasks, rc, request


  call mpi_init(rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)

  write(filename,'(A15,I0,A4)') 'overl_allreduce', ntasks, '.dat'
  if(my_id==0) open(12,file=filename,form='formatted')

  call random_number(A)
  call random_number(B)
  C = 0.0  

  do imsg = 1,10
     n = 2**imsg
     allocate (cresta_send(n), cresta_recv(n))

     if (n <= large_msg) then
         maxiter = 100
     else
         maxiter = 10
     end if

     do i = 1, n
        cresta_send(i) = my_id*10 + i
     end do
     cresta_recv = 0
     call mpi_barrier(mpi_comm_world, rc)
     comp_time = 0.0d0
     coll_time = 0.0d0
     tot_time = 0.0d0
     ovl_time = 0.0d0
     do iter = 0, maxiter
       cmpt0 = gettime()      
       C = MATMUL(A,B)
       cmpt1 = gettime()
       if (iter > 0) comp_time = comp_time + (cmpt1 - cmpt0)
       colt0 = gettime()
       call CRESTA_Allreduce_begin(cresta_send, cresta_recv, n, mpi_integer, &
                            mpi_sum, mpi_comm_world, request, rc)
       call CRESTA_Coll_end(1, request, rc)
       colt1 = gettime()
       if (iter > 0) coll_time = coll_time + (colt1 - colt0)

       tott0 = gettime()
       call CRESTA_Allreduce_begin(cresta_send, cresta_recv, n, mpi_integer, &
                            mpi_sum, mpi_comm_world, request, rc)
       C = MATMUL(A,B)
       call CRESTA_Coll_end(1, request, rc)
       tott1 = gettime()
       if (iter > 0) tot_time = tot_time + (tott1 - tott0)
     end do
     comp_time = comp_time / real(maxiter,8)
     coll_time = coll_time / real(maxiter,8)
     tot_time = tot_time / real(maxiter,8)
     ovl_time = (comp_time + coll_time) - tot_time
     call mpi_reduce(comp_time, compave, 1, mpi_double_precision, mpi_sum, &
                     0, mpi_comm_world, rc)
     call mpi_reduce(coll_time, collave, 1, mpi_double_precision, mpi_sum, &
                     0, mpi_comm_world, rc)
     call mpi_reduce(tot_time, totave, 1, mpi_double_precision, mpi_sum, &
                     0, mpi_comm_world, rc)
     call mpi_reduce(ovl_time, ovlave, 1, mpi_double_precision, mpi_sum, &
                     0, mpi_comm_world, rc)
    
     if(my_id==0)  then
        compave = compave/dble(ntasks)
        collave = collave/dble(ntasks)
        totave = totave/dble(ntasks)
        ovlave = ovlave / dble(ntasks)
        ovlp = 100.d0*(compave + collave - totave)/(compave +  collave - max(compave, collave))
        write(12,'(I12,G15.6,G15.6,G15.6,G15.6,F10.1)') &
             4*n*ntasks, 1.0d6*compave, 1.0d6*collave, &
             1.0d6*totave, 1.0d6*ovlave, ovlp 
     end if
     deallocate(cresta_send, cresta_recv)
  end do
  if(my_id==0) close(12)


  call mpi_finalize(rc)


end program overlap_benchmark
