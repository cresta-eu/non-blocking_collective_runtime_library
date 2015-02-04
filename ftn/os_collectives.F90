module os_collectives
  use mpi
  use caf_collectives, only : dp
  implicit none
#if defined (_STATIC_OS_WIN)
  integer, dimension(TMP_OS_WIN_SIZE), save :: itmpbuf
  real, dimension(TMP_OS_WIN_SIZE), save :: rtmpbuf
  real(kind=dp), dimension(TMP_OS_WIN_SIZE), save :: dtmpbuf
  integer, save :: itmpwindow, &
                   rtmpwindow, &
                   dtmpwindow
  private ::  itmpbuf, itmpwindow, &
              rtmpbuf, rtmpwindow, &
              dtmpbuf, dtmpwindow
#endif
  interface os_bcast
     module procedure os_bcast_int, os_bcast_real, os_bcast_dble
  end interface

  interface os_gather
     module procedure os_gather_int, os_gather_real, os_gather_dble
  end interface

  interface os_scatter
     module procedure os_scatter_int, os_scatter_real, os_scatter_dble
  end interface

  interface os_reduce
     module procedure os_reduce_int, os_reduce_real, os_reduce_dble
  end interface

  interface os_allreduce
     module procedure os_allreduce_int, os_allreduce_real, os_allreduce_dble
  end interface

  interface os_alltoall
     module procedure os_alltoall_int, os_alltoall_real, os_alltoall_dble
  end interface


contains

#if defined (_STATIC_OS_WIN)
  subroutine cresta_win_init(comm, rc)
    integer, intent(in) :: comm
    integer, intent(out), optional :: rc
    integer :: win_rc
    integer :: i
    real :: r
    real(kind=dp) :: d
    integer(mpi_address_kind) :: ibufsize, rbufsize, dbufsize
    integer :: my_id, bsize, intsize, realsize, dblesize, ierr(3)
    intsize = sizeof(i)
    realsize = sizeof(r)
    dblesize = sizeof(d)
    ibufsize = TMP_OS_WIN_SIZE*intsize
    rbufsize = TMP_OS_WIN_SIZE*realsize
    dbufsize = TMP_OS_WIN_SIZE*dblesize
    call mpi_win_create(itmpbuf, ibufsize, intsize, &
                       mpi_info_null, comm, itmpwindow, ierr(1))
    call mpi_win_create(rtmpbuf, rbufsize, realsize, &
                       mpi_info_null, comm, rtmpwindow, ierr(1))
    call mpi_win_create(dtmpbuf, dbufsize, dblesize, &
                       mpi_info_null, comm, dtmpwindow, ierr(3))
    if (all(ierr(:) == MPI_SUCCESS)) then
       rc = MPI_SUCCESS
    else
       write(*,*) 'Error in Crestacoll (cresta_win_init): ', &
                  ' initialization of one-sided', &
                  ' memory windows failed. Aborting.'
       call mpi_abort(comm, 1, rc)
    end if
  end subroutine cresta_win_init

  subroutine cresta_win_finalize(rc)
     integer, intent(out), optional :: rc
     integer :: ierr
     call mpi_win_free(itmpwindow, ierr)
     call mpi_win_free(rtmpwindow, ierr)
     call mpi_win_free(dtmpwindow, ierr)
  end subroutine cresta_win_finalize
#endif

! BCAST -------------------------------------------------------------

  subroutine os_bcast_int(buffer, root, comm, rc)
! not an optimal one
    integer, intent(inout) :: buffer(:)
    integer, intent(in) :: root, comm
    integer, intent(out), optional :: rc
    integer :: my_id, ierr
    integer(mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer, dimension(size(buffer)) :: itmpbuf
    integer(mpi_address_kind) :: bufsize
    integer :: bsize, intsize, itmpwindow
    intsize = sizeof(root)
    bufsize = size(buffer)*intsize
    call mpi_win_create(buffer, bufsize, intsize, & 
                       mpi_info_null, comm, itmpwindow, ierr)
#else
    itmpbuf(1:size(buffer)) = buffer(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    disp = 0
    call mpi_win_fence(mpi_mode_noprecede, itmpwindow, ierr)
    if (my_id /= root) then
        call mpi_get(buffer, size(buffer), mpi_integer, root, &
                     disp, size(buffer), mpi_integer, itmpwindow, ierr)
    end if
    call mpi_win_fence(mpi_mode_nosucceed, itmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(itmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_bcast_int

  subroutine os_bcast_real(buffer, root, comm, rc)
! not an optimal one
    real, intent(inout) :: buffer(:)
    integer, intent(in) :: root, comm
    integer, intent(out), optional :: rc
    integer :: my_id, ierr
    integer(mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    real, dimension(size(buffer)) :: rtmpbuf
    real :: x
    integer(mpi_address_kind) :: bufsize
    integer :: bsize, realsize, rtmpwindow
    realsize = sizeof(x)
    bufsize = size(buffer)*dblesize
    call mpi_win_create(buffer, bufsize, realsize, &
                       mpi_info_null, comm, rtmpwindow, ierr)
#else
    rtmpbuf(1:size(buffer)) = buffer(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    disp = 0
    call mpi_win_fence(mpi_mode_noprecede, rtmpwindow, ierr)
    if (my_id /= root) then
        call mpi_get(buffer, size(buffer), mpi_real, root, &
                     disp, size(buffer), mpi_real, rtmpwindow, ierr)
    end if
    call mpi_win_fence(mpi_mode_nosucceed, rtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(rtmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_bcast_real

  subroutine os_bcast_dble(buffer, root, comm, rc)
! not an optimal one
    real(kind=dp), intent(inout) :: buffer(:)
    integer, intent(in) :: root, comm
    integer, intent(out), optional :: rc
    integer :: my_id, ierr
    integer(mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    real(kind=dp), dimension(size(buffer)) :: dtmpbuf
    real(kind=dp) :: x
    integer(mpi_address_kind) :: bufsize
    integer :: bsize, dblesize, dtmpwindow
    dblesize = sizeof(x)
    bufsize = size(buffer)*dblesize
    call mpi_win_create(buffer, bufsize, dblesize, &
                       mpi_info_null, comm, dtmpwindow, ierr)
#else
    dtmpbuf(1:size(buffer)) = buffer(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    disp = 0
    call mpi_win_fence(mpi_mode_noprecede, dtmpwindow, ierr)
    if (my_id /= root) then
        call mpi_get(buffer, size(buffer), mpi_double_precision, root, &
                     disp, size(buffer), mpi_double_precision, dtmpwindow, ierr)
    end if
    call mpi_win_fence(mpi_mode_nosucceed, dtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(dtmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_bcast_dble

! GATHER------------------------------------------------------------

  subroutine os_gather_int(sendbuf, sendcount, recvbuf, root, comm, rc)
    ! Gather, put-based, straightforward algorithm, slow
    implicit none
    integer, intent(in) :: sendbuf(:), sendcount, root, comm
    integer, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: bufsize, intsize
    integer :: itmpwindow
    intsize = sizeof(source)
    bufsize = size(recvbuf)
    call mpi_win_create(recvbuf, bufsize*intsize, intsize, mpi_info_null, &
                        comm, itmpwindow, ierr)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    call mpi_win_fence(mpi_mode_noprecede, itmpwindow, ierr)
    call mpi_put(sendbuf, sendcount, mpi_integer, root, disp, sendcount, &
                 mpi_integer, itmpwindow, ierr)
    call mpi_win_fence(mpi_mode_nosucceed, itmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(itmpwindow, ierr)
#else
    if (my_id == root) then
       recvbuf(:) = itmpbuf(1:size(recvbuf))
    end if
#endif
    rc = MPI_SUCCESS
  end subroutine os_gather_int

  subroutine os_gather_real(sendbuf, sendcount, recvbuf, root, comm, rc)
    ! Gather, put-based, straightforward algorithm, slow
    implicit none
    real, intent(in) :: sendbuf(:)
    integer, intent(in) :: sendcount, root, comm
    real, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: bufsize, realsize
    real :: x
    integer :: rtmpwindow
    realsize = sizeof(x)
    bufsize = size(recvbuf)
    call mpi_win_create(recvbuf, bufsize*realsize, realsize, mpi_info_null, &
                        comm, rtmpwindow, ierr)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    call mpi_win_fence(mpi_mode_noprecede, rtmpwindow, ierr)
    call mpi_put(sendbuf, sendcount, mpi_real, root, disp, sendcount, &
                 mpi_real, rtmpwindow, ierr)
    call mpi_win_fence(mpi_mode_nosucceed, rtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(rtmpwindow, ierr)
#else
    if (my_id == root) then
       recvbuf(:) = rtmpbuf(1:size(recvbuf))
    end if
#endif
    rc = MPI_SUCCESS
  end subroutine os_gather_real


  subroutine os_gather_dble(sendbuf, sendcount, recvbuf, root, comm, rc)
    ! Gather, put-based, straightforward algorithm, slow
    implicit none
    real(dp), intent(in) :: sendbuf(:)
    integer, intent(in) :: sendcount, root, comm
    real(dp), intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: bufsize, dblesize
    real(dp) :: x
    integer :: dtmpwindow
    dblesize = sizeof(source)
    bufsize = size(x)
    call mpi_win_create(recvbuf, bufsize*dblesize, dblesize, mpi_info_null, &
                        comm, dtmpwindow, ierr)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    call mpi_win_fence(mpi_mode_noprecede, dtmpwindow, ierr)
    call mpi_put(sendbuf, sendcount, mpi_double_precision, root, disp, sendcount, &
                 mpi_double_precision, dtmpwindow, ierr)
    call mpi_win_fence(mpi_mode_nosucceed, dtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(dtmpwindow, ierr)
#else
    if (my_id == root) then
       recvbuf(:) = dtmpbuf(1:size(recvbuf))
    end if
#endif
    rc = MPI_SUCCESS
  end subroutine os_gather_dble


! SCATTER------------------------------------------------------------

  subroutine os_scatter_int(sendbuf, sendcount, recvbuf, root, comm, rc)
    ! Scatter, get-based, straightforward algorithm, slow
    implicit none
    integer, intent(in) :: sendbuf(:), sendcount, root, comm
    integer, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: sbufsize, intsize
    integer :: itmpwindow
    intsize = sizeof(source)
    sbufsize = size(sendbuf)
    call mpi_win_create(sendbuf, sbufsize*intsize, intsize, mpi_info_null, &
                        comm, itmpwindow, ierr)
#else
    itmpbuf(1:size(sendbuf)) = sendbuf(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    call mpi_win_fence(mpi_mode_noprecede+mpi_mode_noput, itmpwindow, ierr)
    call mpi_get(recvbuf, sendcount, mpi_integer, root, disp, sendcount, &
                 mpi_integer, itmpwindow, ierr)
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, itmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(itmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_scatter_int

  subroutine os_scatter_real(sendbuf, sendcount, recvbuf, root, comm, rc)
    ! Scatter, get-based, straightforward algorithm, slow
    implicit none
    real, intent(in) :: sendbuf(:)
    integer, intent(in) :: sendcount, root, comm
    real, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: sbufsize, realsize
    real :: x
    integer :: rtmpwindow
    realsize = sizeof(x)
    sbufsize = size(sendbuf)
    call mpi_win_create(sendbuf, sbufsize*intsize, intsize, mpi_info_null, &
                        comm, rtmpwindow, ierr)
#else
    rtmpbuf(1:size(sendbuf)) = sendbuf(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    call mpi_win_fence(mpi_mode_noprecede+mpi_mode_noput, rtmpwindow, ierr)
    call mpi_get(recvbuf, sendcount, mpi_integer, root, disp, sendcount, &
                 mpi_integer, rtmpwindow, ierr)
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, rtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(rtmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_scatter_real


  subroutine os_scatter_dble(sendbuf, sendcount, recvbuf, root, comm, rc)
    ! Scatter, get-based, straightforward algorithm, slow
    implicit none
    real(kind=dp), intent(in) :: sendbuf(:)
    integer, intent(in) :: sendcount, root, comm
    real(kind=dp), intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: sbufsize, dblesize
    integer :: dtmpwindow
    real(kind=dp) :: x
    real(kind=dp), dimension(size(sendbuf)) :: dtmpbuf
    dblesize = sizeof(x)
    sbufsize = size(sendbuf)
    call mpi_win_create(sendbuf, sbufsize*dblesize, dblesize, mpi_info_null, &
                        comm, dtmpwindow, ierr)
#else
    dtmpbuf(1:size(sendbuf)) = sendbuf(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    call mpi_win_fence(mpi_mode_noprecede+mpi_mode_noput, dtmpwindow, ierr)
    call mpi_get(recvbuf, sendcount, mpi_double_precision, root, disp, sendcount, &
                 mpi_double_precision, dtmpwindow, ierr)
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, dtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(dtmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_scatter_dble

! REDUCE -----------------------------------------------------------

  subroutine os_reduce_int(sendbuf, recvbuf, op, root, comm, rc)
    implicit none
    integer, intent(in) :: sendbuf(:)
    integer, intent(in) :: op, root, comm
    integer, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, sbufsize, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: intsize, rbufsize
    integer :: itmpwindow
    intsize = sizeof(ierr)
    rbufsize = size(recvbuf)
    call mpi_win_create(recvbuf, rbufsize*intsize, intsize, mpi_info_null, &
                        comm, itmpwindow, ierr)
    recvbuf = 0
#endif
    sbufsize = size(sendbuf)
    disp = 0
    call mpi_win_fence(mpi_mode_noprecede, itmpwindow, ierr)
    call mpi_accumulate(sendbuf, sbufsize, mpi_integer, root, disp, &
                        sbufsize, mpi_integer, op, itmpwindow, rc)
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, itmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(itmpwindow, ierr)
#else
    recvbuf(:) = itmpbuf(1:size(recvbuf))
#endif
    rc = MPI_SUCCESS
  end subroutine os_reduce_int

  subroutine os_reduce_real(sendbuf, recvbuf, op, root, comm, rc)
    implicit none
    real, intent(in) :: sendbuf(:)
    integer, intent(in) :: op, root, comm
    real, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, sbufsize, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    real :: x
    integer(kind=mpi_address_kind) :: realsize, rbufsize
    integer :: rtmpwindow
    realsize = sizeof(x)
    rbufsize = size(recvbuf)
    call mpi_win_create(recvbuf, rbufsize*dblesize, realsize, mpi_info_null, &
                        comm, rtmpwindow, ierr)
    recvbuf = 0
#endif
    sbufsize = size(sendbuf)
    disp = 0
    call mpi_win_fence(mpi_mode_noprecede, rtmpwindow, ierr)
    call mpi_accumulate(sendbuf, sbufsize, mpi_double_precision, root, disp, &
                        sbufsize, mpi_double_precision, op, rtmpwindow, rc)
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, rtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(rtmpwindow, ierr)
#else
    recvbuf(:) = rtmpbuf(1:size(recvbuf))
#endif
    rc = MPI_SUCCESS
  end subroutine os_reduce_real

  subroutine os_reduce_dble(sendbuf, recvbuf, op, root, comm, rc)
    implicit none
    real(dp), intent(in) :: sendbuf(:)
    integer, intent(in) :: op, root, comm
    real(dp), intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, sbufsize, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    real(dp) :: x
    integer(kind=mpi_address_kind) :: dblesize, rbufsize
    integer :: dtmpwindow
    dblesize = sizeof(x)
    rbufsize = size(recvbuf)
    call mpi_win_create(recvbuf, rbufsize*dblesize, dblesize, mpi_info_null, &
                        comm, dtmpwindow, ierr)
    recvbuf = 0
#endif
    sbufsize = size(sendbuf)
    disp = 0
    call mpi_win_fence(mpi_mode_noprecede, dtmpwindow, ierr)
    call mpi_accumulate(sendbuf, sbufsize, mpi_double_precision, root, disp, &
                        sbufsize, mpi_double_precision, op, dtmpwindow, rc)
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, dtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(dtmpwindow, ierr)
#else
    recvbuf(:) = dtmpbuf(:)
#endif
    rc = MPI_SUCCESS
  end subroutine os_reduce_dble

! ALLREDUCE -----------------------------------------------------------

  subroutine os_allreduce_int(sendbuf, recvbuf, op, comm, rc)
    implicit none
    integer, intent(in) :: sendbuf(:)
    integer, intent(in) :: op, comm
    integer, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, target, sbufsize, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: intsize, rbufsize
    integer :: itmpwindow
    intsize = sizeof(ierr)
    rbufsize = size(recvbuf)
    call mpi_win_create(recvbuf, rbufsize*intsize, intsize, mpi_info_null, &
                        comm, itmpwindow, ierr)
    recvbuf = 0
#endif
    sbufsize = size(sendbuf)
    disp = 0
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    call mpi_win_fence(mpi_mode_noprecede, itmpwindow, ierr)
    do target = my_id+1, ntasks-1
      call mpi_accumulate(sendbuf, sbufsize, mpi_integer, target, disp, &
                        sbufsize, mpi_integer, op, itmpwindow, rc)
    end do
    do target = 0, my_id
      call mpi_accumulate(sendbuf, sbufsize, mpi_integer, target, disp, &
                        sbufsize, mpi_integer, op, itmpwindow, rc)
    end do
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, itmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(itmpwindow, ierr)
#else
    recvbuf(:) = itmpbuf(1:size(recvbuf))
#endif
    rc = MPI_SUCCESS
  end subroutine os_allreduce_int

  subroutine os_allreduce_real(sendbuf, recvbuf, op, comm, rc)
    implicit none
    real, intent(in) :: sendbuf(:)
    integer, intent(in) :: op, comm
    real, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, target, sbufsize, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: realsize, rbufsize
    integer :: rtmpwindow
    real :: x
    realsize = sizeof(x)
    rbufsize = size(recvbuf)
    call mpi_win_create(recvbuf, rbufsize*realsize, realsize, mpi_info_null, &
                        comm, rtmpwindow, ierr)
    recvbuf = 0
#endif
    sbufsize = size(sendbuf)
    disp = 0
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    call mpi_win_fence(mpi_mode_noprecede, rtmpwindow, ierr)
    do target = my_id+1, ntasks-1
      call mpi_accumulate(sendbuf, sbufsize, mpi_real, target, disp, &
                        sbufsize, mpi_real, op, rtmpwindow, rc)
    end do
    do target = 0, my_id
      call mpi_accumulate(sendbuf, sbufsize, mpi_real, target, disp, &
                        sbufsize, mpi_real, op, rtmpwindow, rc)
    end do
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, rtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(rtmpwindow, ierr)
#else
    recvbuf(:) = rtmpbuf(1:size(recvbuf))
#endif
    rc = MPI_SUCCESS
  end subroutine os_allreduce_real

  subroutine os_allreduce_dble(sendbuf, recvbuf, op, comm, rc)
    implicit none
    real(dp), intent(in) :: sendbuf(:)
    integer, intent(in) :: op, comm
    real(dp), intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, target, sbufsize, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: dblesize, rbufsize
    integer :: dtmpwindow
    real(dp) :: x
    dblesize = sizeof(x)
    rbufsize = size(recvbuf)
    call mpi_win_create(recvbuf, rbufsize*dblesize, dblesize, mpi_info_null, &
                        comm, dtmpwindow, ierr)
    recvbuf = 0
#endif
    sbufsize = size(sendbuf)
    disp = 0
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    call mpi_win_fence(mpi_mode_noprecede, dtmpwindow, ierr)
    do target = my_id+1, ntasks-1
      call mpi_accumulate(sendbuf, sbufsize, mpi_double_precision, target, disp, &
                        sbufsize, mpi_double_precision, op, dtmpwindow, rc)
    end do
    do target = 0, my_id
      call mpi_accumulate(sendbuf, sbufsize, mpi_double_precision, target, disp, &
                        sbufsize, mpi_double_precision, op, dtmpwindow, rc)
    end do
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, dtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(dtmpwindow, ierr)
#else
    recvbuf(:) = dtmpbuf(1:size(recvbuf))
#endif
    rc = MPI_SUCCESS
  end subroutine os_allreduce_dble

  
! ALLTOALL-----------------------------------------------------------

  subroutine os_alltoall_int(sendbuf, sendcount, recvbuf, comm, rc)
    ! Alltoall, all get, straightforward algorithm, slow
    implicit none
    integer, intent(in) :: sendbuf(:), sendcount, comm
    integer, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, r_low, r_up, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    integer(kind=mpi_address_kind) :: sbufsize, intsize
    integer :: itmpwindow
    intsize = sizeof(source)
    sbufsize = size(sendbuf)
    call mpi_win_create(sendbuf, sbufsize*intsize, intsize, mpi_info_null, &
                        comm, itmpwindow, ierr)
#else
    itmpbuf(1:size(sendbuf)) = sendbuf(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    r_low = (my_id+1)*sendcount + 1
    r_up = r_low + sendcount - 1
    call mpi_win_fence(mpi_mode_noprecede+mpi_mode_noput, itmpwindow, ierr)
    do source = my_id+1, ntasks-1
       call mpi_get(recvbuf(r_low:r_up), sendcount, mpi_integer, source, disp, sendcount, &
            mpi_integer, itmpwindow, ierr)
       r_low = r_up + 1
       r_up = r_low + sendcount - 1
    end do
    r_low = 1
    r_up = sendcount
    do source = 0, my_id
         call mpi_get(recvbuf(r_low:r_up), sendcount, mpi_integer, source, disp, sendcount, &
              mpi_integer, itmpwindow, ierr)
         r_low = r_up + 1
         r_up = r_low + sendcount - 1
    end do
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, itmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(itmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_alltoall_int

  subroutine os_alltoall_real(sendbuf, sendcount, recvbuf, comm, rc)
    ! Alltoall, all get, straightforward algorithm, slow
    implicit none
    real, intent(in) :: sendbuf(:)
    integer, intent(in) :: sendcount, comm
    real, intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, r_low, r_up, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    real :: x
    integer(kind=mpi_address_kind) :: sbufsize, realsize
    integer :: rtmpwindow
    realsize = sizeof(x)
    sbufsize = size(sendbuf)
    call mpi_win_create(sendbuf, sbufsize*realsize, realsize, mpi_info_null, &
                        comm, rtmpwindow, ierr)
#else
    rtmpbuf(1:size(sendbuf)) = sendbuf(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    r_low = (my_id+1)*sendcount + 1
    r_up = r_low + sendcount - 1
    call mpi_win_fence(mpi_mode_noprecede+mpi_mode_noput, rtmpwindow, ierr)
    do source = my_id+1, ntasks-1
       call mpi_get(recvbuf(r_low:r_up), sendcount, mpi_real, source, disp, sendcount, &
            mpi_real, rtmpwindow, ierr)
       r_low = r_up + 1
       r_up = r_low + sendcount - 1
    end do
    r_low = 1
    r_up = sendcount
    do source = 0, my_id
         call mpi_get(recvbuf(r_low:r_up), sendcount, mpi_real, source, disp, sendcount, &
              mpi_real, rtmpwindow, ierr)
         r_low = r_up + 1
         r_up = r_low + sendcount - 1
    end do
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, rtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(rtmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_alltoall_real

  subroutine os_alltoall_dble(sendbuf, sendcount, recvbuf, comm, rc)
    ! Alltoall, all get, straightforward algorithm, slow
    implicit none
    real(dp), intent(in) :: sendbuf(:)
    integer, intent(in) :: sendcount, comm
    real(dp), intent(out) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: my_id, ntasks, source, r_low, r_up, ierr
    integer(kind=mpi_address_kind) :: disp
#if !defined (_STATIC_OS_WIN)
    real(dp) :: x
    integer(kind=mpi_address_kind) :: sbufsize, dblesize
    integer :: dtmpwindow
    dblesize = sizeof(x)
    sbufsize = size(sendbuf)
    call mpi_win_create(sendbuf, sbufsize*dblesize, dblesize, mpi_info_null, &
                        comm, dtmpwindow, ierr)
#else
    dtmpbuf(1:size(sendbuf)) = sendbuf(:)
#endif
    call mpi_comm_rank(comm, my_id, ierr)
    call mpi_comm_size(comm, ntasks, ierr)
    disp = my_id*sendcount
    r_low = (my_id+1)*sendcount + 1
    r_up = r_low + sendcount - 1
    call mpi_win_fence(mpi_mode_noprecede+mpi_mode_noput, dtmpwindow, ierr)
    do source = my_id+1, ntasks-1
       call mpi_get(recvbuf(r_low:r_up), sendcount, mpi_real, source, disp, sendcount, &
            mpi_real, dtmpwindow, ierr)
       r_low = r_up + 1
       r_up = r_low + sendcount - 1
    end do
    r_low = 1
    r_up = sendcount
    do source = 0, my_id
         call mpi_get(recvbuf(r_low:r_up), sendcount, mpi_double_precision, source, disp, sendcount, &
              mpi_double_precision, dtmpwindow, ierr)
         r_low = r_up + 1
         r_up = r_low + sendcount - 1
    end do
    call mpi_win_fence(mpi_mode_nostore+mpi_mode_nosucceed, dtmpwindow, ierr)
#if !defined (_STATIC_OS_WIN)
    call mpi_win_free(dtmpwindow, ierr)
#endif
    rc = MPI_SUCCESS
  end subroutine os_alltoall_dble



end module os_collectives
