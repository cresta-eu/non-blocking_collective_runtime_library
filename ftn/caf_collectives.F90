module caf_collectives
  use mpi
  implicit none
  integer, parameter :: dp=selected_real_kind(8)
#if !defined NROOTS
#define NROOTS 8
#endif
#if defined (_STATIC_COARRAYS)
  integer, dimension(TMP_COARRAY_SIZE), save :: itmpbuf[*]
  real, dimension(TMP_COARRAY_SIZE), save :: rtmpbuf[*]
  real(dp), dimension(TMP_COARRAY_SIZE), save :: dtmpbuf[*]
#endif
  interface caf_bcast
     module procedure caf_bcast_int,  caf_bcast_real, caf_bcast_dble
  end interface

  interface caf_scatter
     module procedure caf_scatter_int, caf_scatter_real, caf_scatter_dble
  end interface

  interface caf_gather
     module procedure caf_gather_int, caf_gather_real, caf_gather_dble
  end interface

  interface caf_reduce
     module procedure caf_reduce_int, caf_reduce_real, caf_reduce_dble
  end interface

  interface caf_allgather
     module procedure caf_allgather_int, caf_allgather_real, caf_allgather_dble
  end interface

  interface caf_allreduce
     module procedure caf_allreduce_int, caf_allreduce_real, caf_allreduce_dble
  end interface

  interface caf_reduce_scatter
     module procedure caf_reduce_scatter_int, caf_reduce_scatter_real, caf_reduce_scatter_dble
  end interface
  
  interface caf_alltoall
     module procedure caf_alltoall_int,  caf_alltoall_real, caf_alltoall_dble
  end interface


contains

  subroutine caf_comm_check(comm, rc)
     integer, intent(in) :: comm
     integer, intent(out) :: rc
     integer :: ab_rc
     if (comm == MPI_COMM_WORLD) then
        rc = MPI_SUCCESS
     else
        if (this_image() == 1) then
          write (*,*) 'Error in CRESTA collectives: only MPI_COMM_WORLD accepted as a communicator'
        end if
        rc = MPI_ERR_COMM
        call mpi_abort(mpi_comm_world, 5, ab_rc)
     end if
  end subroutine caf_comm_check

  subroutine caf_op_check(op, rc)
     integer, intent(in) :: op
     integer, intent(out) :: rc
     integer :: ab_rc
     if (op == MPI_SUM .or. op == MPI_MAX .or. op == MPI_MIN) then
        rc = MPI_SUCCESS
     else
        if (this_image() == 1) then
          write (*,*) 'Error in CRESTA collectives: only MPI_SUM, MPI_MIN and MPI_MAX accepted as an operation'
        end if
        rc = MPI_ERR_TYPE
        call mpi_abort(mpi_comm_world, 5, ab_rc)
     end if
  end subroutine caf_op_check

! BCAST -------------------------------------------------------------

  subroutine caf_bcast_int(buffer, root, rc)
    integer, intent(inout) :: buffer(:)
    integer, intent(in) :: root
    integer, intent(out), optional :: rc
    integer :: myim, modroot, locroot
#if !defined (_STATIC_COARRAYS)
    integer, dimension(:), allocatable :: itmpbuf[:]
    allocate (itmpbuf(size(buffer))[*])
#else
    if (size(buffer) > TMP_COARRAY_SIZE) then
       if (this_image() == 1) then
           write(*,*) 'CRESTA_bcast (caf_bcast_int): message larger than TMP_COARRAY_SIZE'
       end if
    end if 
#endif 
    myim = this_image()
    modroot = mod(myim, NROOTS)
    locroot = myim + (NROOTS-modroot)
    if (myim == (root+1)) then
        itmpbuf(1:size(buffer)) = buffer(:)
    end if
    sync all
    if (modroot == 0) then
       itmpbuf(1:size(buffer)) = itmpbuf(1:size(buffer))[root+1]
    end if
    sync all
    if (modroot /= 0) then
       buffer(:) = itmpbuf(1:size(buffer))[locroot]
    else 
       buffer(:) = itmpbuf(1:size(buffer))
    end if
    rc = MPI_SUCCESS
#if !defined (_STATIC_COARRAYS)
    deallocate (itmpbuf)
#endif
  end subroutine caf_bcast_int

  subroutine caf_bcast_real(buffer, root, rc)
    real, intent(inout) :: buffer(:)
    integer, intent(in) :: root
    integer, intent(out), optional :: rc
    integer :: myim, modroot, locroot
#if !defined (_STATIC_COARRAYS)
    real, dimension(:), allocatable :: rtmpbuf[:]
    allocate (rtmpbuf(size(buffer))[*])
#else
    if (size(buffer) > TMP_COARRAY_SIZE) then
       if (this_image() == 1) then
           write(*,*) 'CRESTA_bcast (caf_bcast_dble): message larger than TMP_COARRAY_SIZE'
       end if
    end if
#endif
    myim = this_image()
    modroot = mod(myim, NROOTS)
    locroot = myim + (NROOTS-modroot)
    if (myim == (root+1)) then
        rtmpbuf(1:size(buffer)) = buffer(:)
    end if
    sync all
    if (modroot == 0) then
        rtmpbuf(1:size(buffer)) = rtmpbuf(1:size(buffer))[root+1]
    end if
    sync all
    if (modroot /= 0) then
       buffer(:) = rtmpbuf(1:size(buffer))[locroot]
    else
       buffer(:) = rtmpbuf(1:size(buffer))
    end if
    rc = MPI_SUCCESS
#if !defined (_STATIC_COARRAYS)
    deallocate (rtmpbuf)
#endif
  end subroutine caf_bcast_real

  subroutine caf_bcast_dble(buffer, root, rc)
    real(dp), intent(inout) :: buffer(:)
    integer, intent(in) :: root
    integer, intent(out), optional :: rc
    integer :: myim, modroot, locroot
#if !defined (_STATIC_COARRAYS)
    real(dp), dimension(:), allocatable :: dtmpbuf[:]
    allocate (dtmpbuf(size(buffer))[*])
#else
    if (size(buffer) > TMP_COARRAY_SIZE) then
       if (this_image() == 1) then
           write(*,*) 'CRESTA_bcast (caf_bcast_dble): message larger than TMP_COARRAY_SIZE'
       end if
    end if
#endif
    myim = this_image()
    modroot = mod(myim, NROOTS)
    locroot = myim + (NROOTS-modroot)
    if (myim == (root+1)) then
        dtmpbuf(1:size(buffer)) = buffer(:)
    end if
    sync all
    if (modroot == 0) then
        dtmpbuf(1:size(buffer)) = dtmpbuf(1:size(buffer))[root+1]
    end if
    sync all
    if (modroot /= 0) then
       buffer(:) = dtmpbuf(1:size(buffer))[locroot]
    else
       buffer(:) = dtmpbuf(1:size(buffer))
    end if
    rc = MPI_SUCCESS
#if !defined (_STATIC_COARRAYS)
    deallocate (dtmpbuf)
#endif
  end subroutine caf_bcast_dble

! SCATTER ---------------------------------------------------------

  subroutine caf_scatter_int(sendbuf, sendcount, recvbuf, root, rc)
    integer, dimension(:), intent(in) :: sendbuf
    integer, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: root, sendcount
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    integer, dimension(:), allocatable :: itmpbuf[:]
    allocate (itmpbuf(size(recvbuf))[*])
#endif
    if (this_image() == (root+1)) then
       low = 1
       up = sendcount
       do im = 1, num_images()
          itmpbuf(1:sendcount)[im] = sendbuf(low:up)
          low = up + 1
          up = low + sendcount - 1
       end do
    end if
    sync all
    recvbuf(:) = itmpbuf(1:sendcount)
#if !defined (_STATIC_COARRAYS)
    deallocate (itmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_scatter_int

  subroutine caf_scatter_real(sendbuf, sendcount, recvbuf, root, rc)
    real, dimension(:), intent(in) :: sendbuf
    real, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: root, sendcount
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    real, dimension(:), allocatable :: rtmpbuf[:]
    allocate (itmpbuf(size(recvbuf))[*])
#endif

    if (this_image() == (root+1)) then
       low = 1
       up = sendcount
       do im = 1, num_images()
          rtmpbuf(1:sendcount)[im] = sendbuf(low:up)
          low = up + 1
          up = low + sendcount - 1
       end do
    end if
    sync all
    recvbuf(:) = rtmpbuf(1:sendcount)
#if !defined (_STATIC_COARRAYS)
    deallocate (rtmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_scatter_real

  subroutine caf_scatter_dble(sendbuf, sendcount, recvbuf, root, rc)
    real(dp), dimension(:), intent(in) :: sendbuf
    real(dp), dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: root, sendcount
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    real(dp), dimension(:), allocatable :: dtmpbuf[:]
    allocate (itmpbuf(size(recvbuf))[*])
#endif

    if (this_image() == (root+1)) then
       low = 1
       up = sendcount
       do im = 1, num_images()
          dtmpbuf(1:sendcount)[im] = sendbuf(low:up)
          low = up + 1
          up = low + sendcount - 1
       end do
    end if
    sync all
    recvbuf(:) = dtmpbuf(1:sendcount)
#if !defined (_STATIC_COARRAYS)
    deallocate (dtmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_scatter_dble

! GATHER -----------------------------------------------------------

  subroutine caf_gather_int(sendbuf, recvbuf, recvcount, root, rc)
    implicit none
    integer, dimension(:), intent(in) :: sendbuf
    integer, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: recvcount, root
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    integer, dimension(:), allocatable :: itmpbuf[:]
    allocate (itmpbuf(recvcount)[*])
#endif
    itmpbuf(1:recvcount) = sendbuf(1:recvcount)
    sync all
    if (this_image() == (root+1)) then
       low = 1
       up = recvcount
       do im = 1, num_images()
          recvbuf(low:up) = itmpbuf(1:recvcount)[im]
          low = up + 1
          up = low + recvcount - 1
       end do
    end if
#if !defined (_STATIC_COARRAYS)
    deallocate (itmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_gather_int

  subroutine caf_gather_real(sendbuf, recvbuf, recvcount, root, rc)
    implicit none
    real, dimension(:), intent(in) :: sendbuf
    real, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: recvcount, root
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    real, dimension(:), allocatable :: rtmpbuf[:]
    allocate (rtmpbuf(recvcount)[*])
#endif

    rtmpbuf(1:recvcount) = sendbuf(1:recvcount)
    sync all
    if (this_image() == (root+1)) then
       low = 1
       up = recvcount
       do im = 1, num_images()
          recvbuf(low:up) = rtmpbuf(1:recvcount)[im]
          low = up + 1
          up = low + recvcount - 1
       end do
    end if
#if !defined (_STATIC_COARRAYS)
    deallocate (rtmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_gather_real

  subroutine caf_gather_dble(sendbuf, recvbuf, recvcount, root, rc)
    implicit none
    real(kind=dp), dimension(:), intent(in) :: sendbuf
    real(kind=dp), dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: recvcount, root
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    real(kind=dp), dimension(:), allocatable :: dtmpbuf[:]
    allocate (dtmpbuf(recvcount)[*])
#endif
    dtmpbuf(1:recvcount) = sendbuf(1:recvcount)
    sync all
    if (this_image() == (root+1)) then
       low = 1
       up = recvcount
       do im = 1, num_images()
          recvbuf(low:up) = dtmpbuf(1:recvcount)[im]
          low = up + 1
          up = low + recvcount - 1
       end do
    end if
#if !defined (_STATIC_COARRAYS)
    deallocate (dtmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_gather_dble


! REDUCE --------------------------------------------------------
! these are equal to allreduces until the optional result_image
! argument in co_{sum,min,max} becomes supported by compilers

  subroutine caf_reduce_int(sendbuf, recvbuf, op, root, rc)
    implicit none
    integer, dimension(:), intent(in) :: sendbuf
    integer, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op, root
    integer, intent(out), optional :: rc
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), recvbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), recvbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), recvbuf(:))
    end select
    rc = MPI_SUCCESS
  end subroutine caf_reduce_int

  subroutine caf_reduce_real(sendbuf, recvbuf, op, root, rc)
    implicit none
    real, dimension(:), intent(in) :: sendbuf
    real, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op, root
    integer, intent(out), optional :: rc
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), recvbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), recvbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), recvbuf(:))
    end select
    rc = MPI_SUCCESS
  end subroutine caf_reduce_real

  subroutine caf_reduce_dble(sendbuf, recvbuf, op, root, rc)
    implicit none
    real(dp), dimension(:), intent(in) :: sendbuf
    real(dp), dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op, root
    integer, intent(out), optional :: rc
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), recvbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), recvbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), recvbuf(:))
    end select
    rc = MPI_SUCCESS
  end subroutine caf_reduce_dble


! ALLGATHER --------------------------------------------------------

  subroutine caf_allgather_int(sendbuf, recvbuf, recvcount, rc)
    implicit none
    integer, dimension(:), intent(in) :: sendbuf
    integer, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: recvcount
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    integer, dimension(:), allocatable :: itmpbuf[:]
    allocate (itmpbuf(recvcount)[*])
#endif
    itmpbuf(1:recvcount) = sendbuf(1:recvcount)
    sync all
    low = this_image()*recvcount+1
    up = low + recvcount - 1
    do im = this_image()+1, num_images()
        recvbuf(low:up) = itmpbuf(1:recvcount)[im]
        low = up + 1
        up = low + recvcount - 1
    end do    
    low = 1
    up = recvcount
    do im = 1, this_image()
       recvbuf(low:up) = itmpbuf(1:recvcount)[im]
       low = up + 1
       up = low + recvcount - 1
    end do
#if !defined (_STATIC_COARRAYS)
    deallocate (itmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_allgather_int

  subroutine caf_allgather_real(sendbuf, recvbuf, recvcount, rc)
    real, dimension(:), intent(in) :: sendbuf
    real, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: recvcount
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    real, dimension(:), allocatable :: rtmpbuf[:]
    allocate (rtmpbuf(recvcount)[*])
#endif
    rtmpbuf(1:recvcount) = sendbuf(1:recvcount)
    sync all
    low = this_image()*recvcount+1
    up = low + recvcount - 1
    do im = this_image()+1, num_images()
        recvbuf(low:up) = rtmpbuf(1:recvcount)[im]
        low = up + 1
        up = low + recvcount - 1
    end do
    low = 1
    up = recvcount
    do im = 1, this_image()
       recvbuf(low:up) = rtmpbuf(1:recvcount)[im]
       low = up + 1
       up = low + recvcount - 1
    end do
#if !defined (_STATIC_COARRAYS)
    deallocate (rtmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_allgather_real

  subroutine caf_allgather_dble(sendbuf, recvbuf, recvcount, rc)
    implicit none
    real(dp), dimension(:), intent(in) :: sendbuf
    real(dp), dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: recvcount
    integer, intent(out), optional :: rc
    integer :: im, low, up
#if !defined (_STATIC_COARRAYS)
    real(dp), dimension(:), allocatable :: dtmpbuf[:]
    allocate (dtmpbuf(recvcount)[*])
#endif
    dtmpbuf(1:recvcount) = sendbuf(1:recvcount)
    sync all
    low = this_image()*recvcount+1
    up = low + recvcount - 1
    do im = this_image()+1, num_images()
        recvbuf(low:up) = dtmpbuf(1:recvcount)[im]
        low = up + 1
        up = low + recvcount - 1
    end do
    low = 1
    up = recvcount
    do im = 1, this_image()
       recvbuf(low:up) = dtmpbuf(1:recvcount)[im]
       low = up + 1
       up = low + recvcount - 1
    end do
#if !defined (_STATIC_COARRAYS)
    deallocate (dtmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_allgather_dble


! ALLREDUCE --------------------------------------------------------

  subroutine caf_allreduce_int(sendbuf, recvbuf, op, rc)
    integer, dimension(:), intent(in) :: sendbuf
    integer, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op
    integer, intent(out), optional :: rc
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), recvbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), recvbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), recvbuf(:))
    end select
    rc = MPI_SUCCESS
  end subroutine caf_allreduce_int

  subroutine caf_allreduce_real(sendbuf, recvbuf, op, rc)
    real, dimension(:), intent(in) :: sendbuf
    real, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op
    integer, intent(out), optional :: rc
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), recvbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), recvbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), recvbuf(:))
    end select
    rc = MPI_SUCCESS
  end subroutine caf_allreduce_real

  subroutine caf_allreduce_dble(sendbuf, recvbuf, op, rc)
    real(dp), dimension(:), intent(in) :: sendbuf
    real(dp), dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op
    integer, intent(out), optional :: rc
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), recvbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), recvbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), recvbuf(:))
    end select
    rc = MPI_SUCCESS
  end subroutine caf_allreduce_dble

! REDUCE_SCATTER ----------------------------------------------------

  subroutine caf_reduce_scatter_int(sendbuf, recvbuf, recvcnts, op, rc)
    integer, dimension(:), intent(in) :: sendbuf
    integer, dimension(1:), intent(in) :: recvcnts
    integer, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op
    integer, intent(out), optional :: rc
    integer :: i, im, low, up
    integer, dimension(size(sendbuf)) :: tmpbuf
    low = 1
    im = this_image()
    do i = 1, im-1
       low = low + recvcnts(i)
    end do
    up = low + recvcnts(im) - 1
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), tmpbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), tmpbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), tmpbuf(:))
    end select
    recvbuf(1:recvcnts(im)) = tmpbuf(low:up)
    rc = MPI_SUCCESS
  end subroutine caf_reduce_scatter_int

  subroutine caf_reduce_scatter_real(sendbuf, recvbuf, recvcnts, op, rc)
    real, dimension(:), intent(in) :: sendbuf
    integer, dimension(1:), intent(in) :: recvcnts
    real, dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op
    integer, intent(out), optional :: rc
    integer :: i, im, low, up
    real, dimension(size(sendbuf)) :: tmpbuf
    low = 1
    im = this_image()
    do i = 1, im-1
       low = low + recvcnts(i)
    end do
    up = low + recvcnts(im) - 1
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), tmpbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), tmpbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), tmpbuf(:))
    end select
    recvbuf(1:recvcnts(im)) = tmpbuf(low:up)
    rc = MPI_SUCCESS
  end subroutine caf_reduce_scatter_real

  subroutine caf_reduce_scatter_dble(sendbuf, recvbuf, recvcnts, op, rc)
    real(dp), dimension(:), intent(in) :: sendbuf
    integer, dimension(1:), intent(in) :: recvcnts
    real(dp), dimension(:), intent(out) :: recvbuf
    integer, intent(in) :: op
    integer, intent(out), optional :: rc
    integer :: i, im, low, up
    real(dp), dimension(size(sendbuf)) :: tmpbuf
    low = 1
    im = this_image()
    do i = 1, im-1
       low = low + recvcnts(i)
    end do
    up = low + recvcnts(im) - 1
    select case (op)
       case (MPI_SUM)
          call co_sum(sendbuf(:), tmpbuf(:))
       case (MPI_MIN)
           call co_min(sendbuf(:), tmpbuf(:))
       case (MPI_MAX)
          call co_max(sendbuf(:), tmpbuf(:))
    end select
    recvbuf(1:recvcnts(im)) = tmpbuf(low:up)
    rc = MPI_SUCCESS
  end subroutine caf_reduce_scatter_dble
  
! ALLTOALL-----------------------------------------------------------

  subroutine caf_alltoall_int(sendbuf, sendcount, recvbuf, rc)
    integer, intent(in) :: sendbuf(:), sendcount
    integer, intent(inout) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: im, r_low, r_up, s_low, s_up
#if !defined (_STATIC_COARRAYS)
    integer, dimension(:), allocatable :: itmpbuf[:]
    allocate (itmpbuf(size(recvbuf))[*])
#else
    if (size(sendbuf) > TMP_COARRAY_SIZE) then
       if (this_image() == 1) then
           write(*,*) 'CRESTA_Alltoall (caf_alltoall): message larger than TMP_COARRAY_SIZE'
           write(*,*) 'recompile the CRESTA library without -D_STATIC_COARRAYS or with ', &
                      'a larger value N for -DTMP_COARRAY_SIZE=N'
       end if
       rc = MPI_ERR_COMM
    end if
#endif
    itmpbuf(1:size(sendbuf)) = sendbuf(:)
    r_low = this_image()*sendcount+1
    r_up = r_low + sendcount - 1
    s_low = (this_image()-1)*sendcount + 1
    s_up = s_low + sendcount - 1
    do im = this_image()+1, num_images()
        recvbuf(r_low:r_up) = itmpbuf(s_low:s_up)[im]
        r_low = r_up + 1
        r_up = r_low + sendcount - 1
    end do
    r_low = 1
    r_up = sendcount
    do im = 1, this_image()
        recvbuf(r_low:r_up) = itmpbuf(s_low:s_up)[im]
        r_low = r_up + 1
        r_up = r_low + sendcount - 1
    end do
#if !defined(_STATIC_COARRAYS)
    deallocate(itmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_alltoall_int

  subroutine caf_alltoall_real(sendbuf, sendcount, recvbuf, rc)
    real, intent(in) :: sendbuf(:)
    integer, intent(in) :: sendcount
    real, intent(inout) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: im, r_low, r_up, s_low, s_up
#if !defined (_STATIC_COARRAYS)
    real, dimension(:), allocatable :: rtmpbuf[:]
    allocate (rtmpbuf(size(recvbuf))[*])
#else
    if (size(sendbuf) > TMP_COARRAY_SIZE) then
       if (this_image() == 1) then
           write(*,*) 'CRESTA_Alltoall (caf_alltoall): message larger than TMP_COARRAY_SIZE'
           write(*,*) 'recompile the CRESTA library without -D_STATIC_COARRAYS or with ', &
                      'a larger value N for -DTMP_COARRAY_SIZE=N'
       end if
       rc = MPI_ERR_COMM
    end if
#endif
    rtmpbuf(1:size(sendbuf)) = sendbuf(:)
    r_low = this_image()*sendcount+1
    r_up = r_low + sendcount - 1
    s_low = (this_image()-1)*sendcount + 1
    s_up = s_low + sendcount - 1
    do im = this_image()+1, num_images()
        recvbuf(r_low:r_up) = rtmpbuf(s_low:s_up)[im]
        r_low = r_up + 1
        r_up = r_low + sendcount - 1
    end do
    r_low = 1
    r_up = sendcount
    do im = 1, this_image()
        recvbuf(r_low:r_up) = rtmpbuf(s_low:s_up)[im]
        r_low = r_up + 1
        r_up = r_low + sendcount - 1
    end do
#if !defined(_STATIC_COARRAYS)
    deallocate(rtmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_alltoall_real

  subroutine caf_alltoall_dble(sendbuf, sendcount, recvbuf, rc)
    real(dp), intent(in) :: sendbuf(:)
    integer, intent(in) :: sendcount
    real(dp), intent(inout) :: recvbuf(:)
    integer, intent(out), optional :: rc
    integer :: im, r_low, r_up, s_low, s_up
#if !defined (_STATIC_COARRAYS)
    real(dp), dimension(:), allocatable :: dtmpbuf[:]
    allocate (dtmpbuf(size(recvbuf))[*])
#else
    if (size(sendbuf) > TMP_COARRAY_SIZE) then
       if (this_image() == 1) then
           write(*,*) 'CRESTA_Alltoall (caf_alltoall): message larger than TMP_COARRAY_SIZE'
           write(*,*) 'recompile the CRESTA library without -D_STATIC_COARRAYS or with ', &
                      'a larger value N for -DTMP_COARRAY_SIZE=N'
       end if
       rc = MPI_ERR_COMM
    end if
#endif
    dtmpbuf(1:size(sendbuf)) = sendbuf(:)
    r_low = this_image()*sendcount+1
    r_up = r_low + sendcount - 1
    s_low = (this_image()-1)*sendcount + 1
    s_up = s_low + sendcount - 1
    do im = this_image()+1, num_images()
        recvbuf(r_low:r_up) = dtmpbuf(s_low:s_up)[im]
        r_low = r_up + 1
        r_up = r_low + sendcount - 1
    end do
    r_low = 1
    r_up = sendcount
    do im = 1, this_image()
        recvbuf(r_low:r_up) = dtmpbuf(s_low:s_up)[im]
        r_low = r_up + 1
        r_up = r_low + sendcount - 1
    end do
#if !defined(_STATIC_COARRAYS)
    deallocate(dtmpbuf)
#endif
    rc = MPI_SUCCESS
  end subroutine caf_alltoall_dble

end module caf_collectives
