program collectives_benchmark
  use mpi
!  use os_collectives, only : cresta_win_init, cresta_win_finalize
  use crestacoll
  implicit none
  integer, dimension(:), allocatable :: cresta_send, cresta_recv, &
       test_send, test_recv
  real(8) :: time, t0, t1, tmax, tmin, tave
  integer :: n, imsg, i, cnt, iter, maxiter
  integer, parameter :: large_msg = 8192
  character(len=25) :: filename 
  integer :: my_id, ntasks, rc, tmaxloc, tminloc, root


  call mpi_init(rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  root = ntasks-1

! this line is needed when using the OS communication with static windows
!  call cresta_win_init(mpi_comm_world, rc)

  write(filename,'(A12,I0,A4)') 'cresta_bcast', ntasks, '.dat'
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
     if (my_id == root) then
        do i = 1, n
            cresta_send(i) = my_id*10 + i
        end do
     end if
     test_send = cresta_send
     test_recv = 0
     cresta_recv = 0
     call mpi_barrier(mpi_comm_world, rc)
     time = 0.0d0
     do iter = 0, maxiter
       t0 = gettime()      
       call CRESTA_Bcast(cresta_send, n, mpi_integer, &
                            root, mpi_comm_world, rc)
       t1 = gettime()
       if (iter > 0) time = time + (t1 - t0)
     end do
     time = time / real(maxiter,8)
! compare against MPI
     call MPI_Bcast(test_send, n, mpi_integer, &
                            root, mpi_comm_world, rc)
     if (any(cresta_send(:) /= test_send(:))) then
        write(*,*) ' Error in CRESTA_Bcast'
        do i = 1, n
           if (cresta_send(i) /= test_send(i)) then
              write(*,'(4I10)') my_id, i, cresta_send(i), test_send(i)
           end if
        end do
        call mpi_barrier(mpi_comm_world, rc)
        call mpi_abort(mpi_comm_world, 1, rc)
     end if

     call mpi_reduce(time, tmax, 1, mpi_double_precision, mpi_max, 0, mpi_comm_world, rc)
     call mpi_reduce(time, tmin, 1, mpi_double_precision, mpi_min, 0, mpi_comm_world, rc)
     call mpi_reduce(time, tave, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, rc)

     if(my_id==0)  then
        write(12,'(I12,G15.6,G15.6,G15.6)') &
             n, tmax*1.0d6, tmin*1.0d6, 1.0d6*tave/real(ntasks,8)
     end if
     deallocate(cresta_send, cresta_recv)
     deallocate(test_send, test_recv)
  end do
  if(my_id==0) close(12)

!  call cresta_win_finalize(rc)
  call mpi_finalize(rc)

end program collectives_benchmark
