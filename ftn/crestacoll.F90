module crestacoll
! CRESTA collectives: a wrapper library for different implementations of collective
!    communication operations (CRESTA project deliverable D4.5.3)
!   Copyright 2013 Pekka Manninen, Cray Inc. // manninen@cray.com
!   This is an experimental software, use with caution and on your own risk (= no warranty!)

   use mpi
   use caf_collectives
   use os_collectives
   implicit none

   interface CRESTA_Bcast
      module procedure cresta_bcast_int, cresta_bcast_real, cresta_bcast_dble
   end interface

  interface CRESTA_Gather
     module procedure cresta_gather_int, cresta_gather_real, cresta_gather_dble
  end interface

  interface CRESTA_Gatherv
     module procedure cresta_gatherv_int, cresta_gatherv_real, cresta_gatherv_dble
  end interface

  interface CRESTA_Scatter
     module procedure cresta_scatter_int,  cresta_scatter_real, cresta_scatter_dble
  end interface

  interface CRESTA_Scatterv
     module procedure cresta_scatterv_int, cresta_scatterv_real,  cresta_scatterv_dble
  end interface

  interface CRESTA_Reduce
     module procedure cresta_reduce_int,  cresta_reduce_real, cresta_reduce_dble
  end interface

  interface CRESTA_Allgather
     module procedure cresta_allgather_int,  cresta_allgather_real, cresta_allgather_dble
  end interface

  interface CRESTA_Allgatherv
     module procedure cresta_allgatherv_int, cresta_allgatherv_real, cresta_allgatherv_dble
  end interface

  interface CRESTA_Allreduce
     module procedure cresta_allreduce_int,  cresta_allreduce_real, cresta_allreduce_dble
  end interface

  interface CRESTA_Reduce_scatter
     module procedure cresta_reduce_scatter_int, cresta_reduce_scatter_real, cresta_reduce_scatter_dble
  end interface

  interface CRESTA_Alltoall
     module procedure cresta_alltoall_int,  cresta_alltoall_real, cresta_alltoall_dble
  end interface

  interface CRESTA_Alltoallv
     module procedure cresta_alltoallv_int, cresta_alltoallv_real, cresta_alltoallv_dble
  end interface

contains

!-- Begin BCAST interfaces -------------------------------------------------

   subroutine cresta_bcast_int(buffer, count, datatype, root, comm, rc)
      integer, intent(inout) :: buffer(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: count, datatype, root, comm
#if defined (_CAF_BCAST)
      call caf_comm_check(comm, rc)
      call caf_bcast(buffer, root, rc)
#elif defined (_OS_BCAST)
      call os_bcast(buffer, root, comm, rc)
#elif defined (_NONBLOCK_BCAST) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ibcast(buffer, count, datatype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Bcast(buffer, count, datatype, root, comm, rc)
#endif
   end subroutine cresta_bcast_int

   subroutine cresta_bcast_real(buffer, count, datatype, root, comm, rc)
      real, intent(inout) :: buffer(:)
      integer, intent(inout) :: rc
      integer, intent(in) :: count, datatype, root, comm
#if defined (_CAF_BCAST)
      call caf_comm_check(comm, rc)
      call caf_bcast(buffer, root, rc)
#elif defined (_OS_BCAST)
      call os_bcast(buffer, root, comm, rc)
#elif defined (_NONBLOCK_BCAST) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ibcast(buffer, count, datatype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Bcast(buffer, count, datatype, root, comm, rc)
#endif
   end subroutine cresta_bcast_real

   subroutine cresta_bcast_dble(buffer, count, datatype, root, comm, rc)
      real(dp), intent(inout) :: buffer(:)
      integer, intent(inout) :: rc
      integer, intent(in) :: count, datatype, root, comm
#if defined (_CAF_BCAST)
      call caf_comm_check(comm, rc)
      call caf_bcast(buffer, root, rc)
#elif defined (_OS_BCAST)
      call os_bcast(buffer, root, comm, rc)
#elif defined (_NONBLOCK_BCAST) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ibcast(buffer, count, datatype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Bcast(buffer, count, datatype, root, comm, rc)
#endif
   end subroutine cresta_bcast_dble


!-- Begin GATHER interfaces -------------------------------------------------

   subroutine cresta_gather_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
#if defined (_CAF_GATHER)
      call caf_comm_check(comm, rc)
      call caf_gather(sendbuf, recvbuf, recvcnt, root, rc)
#elif defined (_OS_GATHER)
      call os_gather(sendbuf, sendcnt, recvbuf, root, comm, rc)
#elif defined (_NONBLOCK_GATHER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Igather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Gather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
#endif
   end subroutine cresta_gather_int

   subroutine cresta_gather_real(sendbuf, sendcnt, sendtype, recvbuf, &
                                recvcnt, recvtype, root, comm, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
#if defined (_CAF_GATHER)
      call caf_comm_check(comm, rc)
      call caf_gather(sendbuf, recvbuf, recvcnt, root, rc)
#elif defined (_OS_GATHER)
      call os_gather(sendbuf, sendcnt, recvbuf, root, comm, rc)
#elif defined (_NONBLOCK_GATHER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Igather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, &
               recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Gather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype,  &
                      root, comm, rc)
#endif
   end subroutine cresta_gather_real

   subroutine cresta_gather_dble(sendbuf, sendcnt, sendtype, recvbuf, &
                                recvcnt, recvtype, root, comm, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
#if defined (_CAF_GATHER)
      call caf_comm_check(comm, rc)
      call caf_gather(sendbuf, recvbuf, recvcnt, root, rc)
#elif defined (_OS_GATHER)
      call os_gather(sendbuf, sendcnt, recvbuf, root, comm, rc)
#elif defined (_NONBLOCK_GATHER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Igather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, &
               recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Gather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype,  &
                      root, comm, rc)
#endif
   end subroutine cresta_gather_dble


!-- Begin GATHERV interfaces -------------------------------------------------

   subroutine cresta_gatherv_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, comm, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, root, comm
#if defined (_NONBLOCK_GATHERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Igatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Gatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, comm, rc)
#endif
   end subroutine cresta_gatherv_int

   subroutine cresta_gatherv_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, &
                                 root, comm, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, root, comm
#if defined (_NONBLOCK_GATHERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Igatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, comm, &
                        request,rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Gatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, comm, &
                       rc)
#endif
   end subroutine cresta_gatherv_real


   subroutine cresta_gatherv_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, &
                                 root, comm, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, root, comm
#if defined (_NONBLOCK_GATHERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Igatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, comm, &
                        request,rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Gatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, comm, &
                       rc)
#endif
   end subroutine cresta_gatherv_dble


!-- Begin SCATTER interfaces -------------------------------------------------

   subroutine cresta_scatter_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
#if defined (_CAF_SCATTER)
      call caf_comm_check(comm, rc)
      call caf_scatter(sendbuf, sendcnt, recvbuf, root, rc)
#elif defined (_OS_SCATTER)
      call os_scatter(sendbuf, sendcnt, recvbuf, root, comm, rc)
#elif defined (_NONBLOCK_SCATTER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iscatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Scatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
#endif
   end subroutine cresta_scatter_int

   subroutine cresta_scatter_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
#if defined (_CAF_SCATTER)
      call caf_comm_check(comm, rc)
      call caf_scatter(sendbuf, sendcnt, recvbuf, root, rc)
#elif defined (_OS_SCATTER)
      call os_scatter(sendbuf, sendcnt, recvbuf, root, comm, rc)
#elif defined (_NONBLOCK_SCATTER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iscatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Scatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
#endif
   end subroutine cresta_scatter_real

   subroutine cresta_scatter_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
#if defined (_CAF_SCATTER)
      call caf_comm_check(comm, rc)
      call caf_scatter(sendbuf, sendcnt, recvbuf, root, rc)
#elif defined (_OS_SCATTER)
      call os_scatter(sendbuf, sendcnt, recvbuf, root, comm, rc)
#elif defined (_NONBLOCK_SCATTER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iscatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Scatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
#endif
   end subroutine cresta_scatter_dble


!-- Begin SCATTERV interfaces -------------------------------------------------

   subroutine cresta_scatterv_int(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnts(:), displs(:), sendtype, recvcnt, recvtype, root, comm
#if defined (_NONBLOCK_SCATTERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iscatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Scatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
#endif
   end subroutine cresta_scatterv_int

   subroutine cresta_scatterv_real(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnts(:), displs(:), sendtype, recvcnt, recvtype, root, comm
#if defined (_NONBLOCK_SCATTERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iscatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Scatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
#endif
   end subroutine cresta_scatterv_real

   subroutine cresta_scatterv_dble(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnts(:), displs(:), sendtype, recvcnt, recvtype, root, comm
#if defined (_NONBLOCK_SCATTERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iscatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Scatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, rc)
#endif
   end subroutine cresta_scatterv_dble


!-- Begin REDUCE interfaces -------------------------------------------------

   subroutine cresta_reduce_int(sendbuf, recvbuf, count, datatype, op, root, comm, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) ::  recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: count, datatype, op, root, comm
#if defined (_CAF_REDUCE)
      call caf_comm_check(comm, rc)
      call caf_reduce(sendbuf, recvbuf, op, root, rc)
#elif defined (_OS_REDUCE)
      call os_reduce(sendbuf, recvbuf, op, root, comm, rc)
#elif defined (_NONBLOCK_REDUCE) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ireduce(sendbuf, recvbuf, count, datatype, op, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, rc)
#endif
   end subroutine cresta_reduce_int

   subroutine cresta_reduce_real(sendbuf, recvbuf, count, datatype, op, root, comm, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: count, datatype, op, root, comm
#if defined (_CAF_REDUCE)
      call caf_comm_check(comm, rc)
      call caf_reduce(sendbuf, recvbuf, op, root, rc)
#elif defined (_OS_REDUCE)
      call os_reduce(sendbuf, recvbuf, op, root, comm, rc)
#elif defined (_NONBLOCK_REDUCE) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ireduce(sendbuf, recvbuf, count, datatype, op, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, rc)
#endif
   end subroutine cresta_reduce_real

   subroutine cresta_reduce_dble(sendbuf, recvbuf, count, datatype, op, root, comm, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: count, datatype, op, root, comm
#if defined (_CAF_REDUCE)
      call caf_comm_check(comm, rc)
      call caf_reduce(sendbuf, recvbuf, op, root, rc)
#elif defined (_OS_REDUCE)
      call os_reduce(sendbuf, recvbuf, op, root, comm, rc)
#elif defined (_NONBLOCK_REDUCE) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ireduce(sendbuf, recvbuf, count, datatype, op, root, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, rc)
#endif
   end subroutine cresta_reduce_dble


!-- Begin ALLGATHER interfaces -------------------------------------------------

   subroutine cresta_allgather_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, comm
#if defined (_CAF_ALLGATHER)
      call caf_comm_check(comm, rc)
      call caf_allgather(sendbuf, recvbuf, recvcnt, rc)
#elif defined (_NONBLOCK_ALLGATHER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, rc)
#endif
   end subroutine cresta_allgather_int

   subroutine cresta_allgather_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, comm
#if defined (_CAF_ALLGATHER)
      call caf_comm_check(comm, rc)
      call caf_allgather(sendbuf, recvbuf, recvcnt, rc)
#elif defined (_NONBLOCK_ALLGATHER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, rc)
#endif
   end subroutine cresta_allgather_real

   subroutine cresta_allgather_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, comm
#if defined (_CAF_ALLGATHER)
      call caf_comm_check(comm, rc)
      call caf_allgather(sendbuf, recvbuf, recvcnt, rc)
#elif defined (_NONBLOCK_ALLGATHER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, rc)
#endif
   end subroutine cresta_allgather_dble


!-- Begin ALLGATHERV interfaces -------------------------------------------------

   subroutine cresta_allgatherv_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, comm
#if defined (_NONBLOCK_ALLGATHERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, rc)
#endif
   end subroutine cresta_allgatherv_int

   subroutine cresta_allgatherv_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, comm
#if defined (_NONBLOCK_ALLGATHERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, rc)
#endif
   end subroutine cresta_allgatherv_real

   subroutine cresta_allgatherv_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, comm
#if defined (_NONBLOCK_ALLGATHERV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, rc)
#endif
   end subroutine cresta_allgatherv_dble


!-- Begin ALLREDUCE interfaces -------------------------------------------------

   subroutine cresta_allreduce_int(sendbuf, recvbuf, count, datatype, op, comm, rc)
      integer, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: count, datatype, op, comm
      integer, intent(out), optional :: rc
#if defined (_CAF_ALLREDUCE)
      call caf_comm_check(comm, rc)
      call caf_allreduce(sendbuf, recvbuf, op, rc)
#elif defined (_OS_ALLREDUCE)
      call os_allreduce(sendbuf, recvbuf, op, comm, rc)
#elif defined (_NONBLOCK_ALLREDUCE) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, rc)
#endif
   end subroutine cresta_allreduce_int

   subroutine cresta_allreduce_real(sendbuf, recvbuf, count, datatype, op, comm, rc)
      real, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: count, datatype, op, comm
      integer, intent(out), optional :: rc
#if defined (_CAF_ALLREDUCE)
      call caf_comm_check(comm, rc)
      call caf_allreduce(sendbuf, recvbuf, op, rc)
#elif defined (_OS_ALLREDUCE)
      call os_allreduce(sendbuf, recvbuf, op, comm, rc)
#elif defined (_NONBLOCK_ALLREDUCE) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, rc)
#endif
   end subroutine cresta_allreduce_real

   subroutine cresta_allreduce_dble(sendbuf, recvbuf, count, datatype, op, comm, rc)
      real(dp), intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: count, datatype, op, comm
      integer, intent(out), optional :: rc
#if defined (_CAF_ALLREDUCE)
      call caf_comm_check(comm, rc)
      call caf_allreduce(sendbuf, recvbuf, op, rc)
#elif defined (_OS_ALLREDUCE)
      call os_allreduce(sendbuf, recvbuf, op, comm, rc)
#elif defined (_NONBLOCK_ALLREDUCE) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, rc)
#endif
   end subroutine cresta_allreduce_dble


!-- Begin REDUCE_SCATTER interfaces -------------------------------------------------

   subroutine cresta_reduce_scatter_int(sendbuf, recvbuf, recvcnts, datatype, op, comm, rc)
      integer, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: recvcnts(:), datatype, op, comm
      integer, intent(out), optional :: rc
#if defined (_CAF_REDUCE_SCATTER)
      call caf_comm_check(comm, rc)
      call caf_reduce_scatter(sendbuf, recvbuf, recvcnts, op, rc)
#elif defined (_NONBLOCK_REDUCE_SCATTER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ireduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Reduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, rc)
#endif
   end subroutine cresta_reduce_scatter_int

   subroutine cresta_reduce_scatter_real(sendbuf, recvbuf, recvcnts, datatype, op, comm, rc)
      real, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: recvcnts(:), datatype, op, comm
      integer, intent(out), optional :: rc
#if defined (_CAF_REDUCE_SCATTER)
      call caf_comm_check(comm, rc)
      call caf_reduce_scatter(sendbuf, recvbuf, recvcnts, op, rc)
#elif defined (_NONBLOCK_REDUCE_SCATTER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ireduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Reduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, rc)
#endif
   end subroutine cresta_reduce_scatter_real

   subroutine cresta_reduce_scatter_dble(sendbuf, recvbuf, recvcnts, datatype, op, comm, rc)
      real(dp), intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: recvcnts(:), datatype, op, comm
      integer, intent(out), optional :: rc
#if defined (_CAF_REDUCE_SCATTER)
      call caf_comm_check(comm, rc)
      call caf_reduce_scatter(sendbuf, recvbuf, recvcnts, op, rc)
#elif defined (_NONBLOCK_REDUCE_SCATTER) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ireduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Reduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, rc)
#endif
   end subroutine cresta_reduce_scatter_dble

!-- Begin ALLTOALL interfaces -------------------------------------------------

   subroutine cresta_alltoall_int(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, rc)
      integer, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcount, sendtype, recvcount, recvtype, comm
#if defined (_CAF_ALLTOALL)
      call caf_comm_check(comm, rc)
      call caf_alltoall(sendbuf, sendcount, recvbuf, rc)
#elif defined (_OS_ALLTOALL)
      call os_alltoall(sendbuf, sendcount, recvbuf, comm, rc)
#elif defined (_NONBLOCK_ALLTOALL) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ialltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, rc)
#endif
   end subroutine cresta_alltoall_int

   subroutine cresta_alltoall_real(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, rc)
      real, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcount, sendtype, recvcount, recvtype, comm
#if defined (_CAF_ALLTOALL)
      call caf_comm_check(comm, rc)
      call caf_alltoall(sendbuf, sendcount, recvbuf, rc)
#elif defined (_OS_ALLTOALL)
      call os_alltoall(sendbuf, sendcount, recvbuf, comm, rc)
#elif defined (_NONBLOCK) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ialltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, rc)
#endif
   end subroutine cresta_alltoall_real

   subroutine cresta_alltoall_dble(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, rc)
      real(dp), intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcount, sendtype, recvcount, recvtype, comm
#if defined (_CAF_ALLTOALL)
      call caf_comm_check(comm, rc)
      call caf_alltoall(sendbuf, sendcount, recvbuf, rc)
#elif defined (_OS_ALLTOALL)
      call os_alltoall(sendbuf, sendcount, recvbuf, comm, rc)
#elif defined (_NONBLOCK) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ialltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, rc)
#endif
   end subroutine cresta_alltoall_dble

!-- End ALLTOALL interfaces

!-- Begin ALLTOALLV interfaces -------------------------------------------------

   subroutine cresta_alltoallv_int(sendbuf, sendcnts, sdispls, sendtype, &
                                   recvbuf, recvcnts, rdispls, recvtype, comm, rc)
      integer, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: sendcnts(:), sdispls(:), sendtype, recvcnts(:), rdispls(:), recvtype, comm
      integer, intent(out), optional :: rc
#if defined (_NONBLOCK_ALLTOALLV)
      integer :: request, wait_rc
      call MPI_Ialltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Alltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                         recvbuf, recvcnts, rdispls, recvtype, comm, rc)
#endif
   end subroutine cresta_alltoallv_int

   subroutine cresta_alltoallv_real(sendbuf, sendcnts, sdispls, sendtype, &
                                   recvbuf, recvcnts, rdispls, recvtype, comm, rc)
      real, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnts(:), sdispls(:), sendtype, recvcnts(:), rdispls(:), recvtype, comm
#if defined (_NONBLOCK_ALLTOALLV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ialltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Alltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                         recvbuf, recvcnts, rdispls, recvtype, comm, rc)
#endif
   end subroutine cresta_alltoallv_real

   subroutine cresta_alltoallv_dble(sendbuf, sendcnts, sdispls, sendtype, &
                                   recvbuf, recvcnts, rdispls, recvtype, comm, rc)
      real(dp), intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnts(:), sdispls(:), sendtype, recvcnts(:), rdispls(:), recvtype, comm
#if defined (_NONBLOCK_ALLTOALLV) || defined (_ALL_NONBLOCK)
      integer :: request, wait_rc
      call MPI_Ialltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
      call MPI_Wait(request, mpi_status_ignore, wait_rc)
#else
      call MPI_Alltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                         recvbuf, recvcnts, rdispls, recvtype, comm, rc)

#endif
   end subroutine cresta_alltoallv_dble

!-- End ALLTOALLV interfaces

  real(dp) function gettime()
! timing utility for CAF collectives
    implicit none
    integer(kind=8) :: c, r
    call system_clock(count=c, count_rate=r)
    gettime=real(c,8)/real(r,8)
  end function gettime

end module crestacoll
