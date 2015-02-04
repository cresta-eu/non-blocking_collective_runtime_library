program collectives_benchmark
  use mpi
  use crestacoll
!  use os_collectives
  implicit none
  integer, dimension(:), allocatable :: cresta_send, cresta_recv, &
       test_send, test_recv
  real(8) :: time, t0, t1, tmax, tmin, tave
  integer :: n, imsg, i, cnt, iter, maxiter
  integer, parameter :: large_msg = 8192
  character(len=26) :: filename 
  integer :: my_id, ntasks, rc, tmaxloc, tminloc


  call mpi_init(rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)

!  call cresta_win_init(mpi_comm_world, rc)

  write(filename,'(A16,I0,A4)') 'cresta_allreduce', ntasks, '.dat'
  if(my_id==0) open(12,file=filename,form='formatted')

  do imsg = 0,24,4
     n = 2**imsg
     allocate (cresta_send(n), cresta_recv(n))
     allocate (test_send(n), test_recv(n))

     if (n <= large_msg) then
         maxiter = 100
     else
         maxiter = 10
     end if

     do i = 1, n
        cresta_send(i) = my_id*10 + i
     end do
     test_send = cresta_send
     test_recv = 0
     cresta_recv = 0
     call mpi_barrier(mpi_comm_world, rc)
     time = 0.0d0
     do iter = 1, maxiter
       t0 = gettime()      
       call CRESTA_Allreduce(cresta_send, cresta_recv, n, &
                            mpi_integer, MPI_SUM, &
                            mpi_comm_world, rc)
       t1 = gettime()
       time = time + (t1 - t0)
     end do
     time = time / real(maxiter,8)
! compare against MPI
     call MPI_Allreduce(test_send, test_recv, n, &
                            mpi_integer, MPI_SUM, &
                            mpi_comm_world, rc)

     if (any(cresta_recv(:) /= test_recv(:))) then
        write(*,*) ' Error in CRESTA_Allreduce'
        do i = 1, n
           if (cresta_recv(i) /= test_recv(i)) then
              write(*,'(4I10)') my_id, i, cresta_recv(i), test_recv(i)
           end if
        end do
        call mpi_barrier(mpi_comm_world, rc)
        call mpi_abort(mpi_comm_world, 1, rc)
     end if

     call mpi_reduce(time, tmax, 1, mpi_double_precision, mpi_max, &
           0, mpi_comm_world, rc)
     call mpi_reduce(time, tmin, 1, mpi_double_precision, mpi_min, &
           0, mpi_comm_world, rc)
     call mpi_reduce(time, tave, 1, mpi_double_precision, mpi_sum, &
           0, mpi_comm_world, rc)

     if(my_id==0)  then
        write(12,'(I12,G15.6,G15.6,G15.6)') &
             4*n, tmax*1.0d6, tmin*1.0d6, 1.0d6*tave/real(ntasks,8)
     end if
     deallocate(cresta_send, cresta_recv)
     deallocate(test_send, test_recv)
  end do
  if(my_id==0) close(12)

!  call cresta_win_finalize(rc)

  call mpi_finalize(rc)


end program collectives_benchmark
