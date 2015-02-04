module cresta_sp_coll
! CRESTA collectives: a wrapper library for different implementations of collective
!    communication operations (CRESTA project deliverable D4.5.3)
!   Copyright 2013 Pekka Manninen, Cray Inc. // manninen@cray.com
!   This is an experimental software, use with caution and on your own risk (= no warranty!)
! Routines & interfaces for the "Split-phase" API
   use mpi
   use caf_collectives, only: dp
   implicit none

   interface CRESTA_Bcast_begin
      module procedure cresta_bcast_begin_int, cresta_bcast_begin_real, cresta_bcast_begin_dble
   end interface

  interface CRESTA_Gather_begin
     module procedure cresta_gather_begin_int, cresta_gather_begin_real, cresta_gather_begin_dble
  end interface

  interface CRESTA_Gatherv_begin
     module procedure cresta_gatherv_begin_int,  cresta_gatherv_begin_real, cresta_gatherv_begin_dble
  end interface

  interface CRESTA_Scatter_begin
     module procedure cresta_scatter_begin_int, cresta_scatter_begin_real, cresta_scatter_begin_dble
  end interface

  interface CRESTA_Scatterv_begin
     module procedure cresta_scatterv_begin_int, cresta_scatterv_begin_real, cresta_scatterv_begin_dble
  end interface

  interface CRESTA_Reduce_begin
     module procedure cresta_reduce_begin_int, cresta_reduce_begin_real, cresta_reduce_begin_dble
  end interface

  interface CRESTA_Allgather_begin
     module procedure cresta_allgather_begin_int, cresta_allgather_begin_real, cresta_allgather_begin_dble
  end interface

  interface CRESTA_Allgatherv_begin
     module procedure cresta_allgatherv_begin_int, cresta_allgatherv_begin_real, cresta_allgatherv_begin_dble
  end interface

  interface CRESTA_Allreduce_begin
     module procedure cresta_allreduce_begin_int, cresta_allreduce_begin_real, cresta_allreduce_begin_dble
  end interface

  interface CRESTA_Reduce_scatter_begin
     module procedure cresta_reduce_scatter_begin_int, cresta_reduce_scatter_begin_real, cresta_reduce_scatter_begin_dble
  end interface

  interface CRESTA_Alltoall_begin
     module procedure cresta_alltoall_begin_int, cresta_alltoall_begin_real, cresta_alltoall_begin_dble
  end interface

  interface CRESTA_Alltoallv_begin
     module procedure cresta_alltoallv_begin_int,cresta_alltoallv_begin_real, cresta_alltoallv_begin_dble
  end interface

  interface CRESTA_Coll_end
     module procedure cresta_coll_end_single, cresta_coll_end_multiple
  end interface

contains

!-- Begin BCAST interfaces -------------------------------------------------

   subroutine cresta_bcast_begin_int(buffer, count, datatype, root, comm, request, rc)
      integer, intent(inout) :: buffer(:)
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
      integer, intent(in) :: count, datatype, root, comm
      call MPI_Ibcast(buffer, count, datatype, root, comm, request, rc)
   end subroutine cresta_bcast_begin_int

   subroutine cresta_bcast_begin_real(buffer, count, datatype, root, comm, request, rc)
      real, intent(inout) :: buffer(:)
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
      integer, intent(in) :: count, datatype, root, comm
      call MPI_Ibcast(buffer, count, datatype, root, comm, request, rc)
   end subroutine cresta_bcast_begin_real

   subroutine cresta_bcast_begin_dble(buffer, count, datatype, root, comm, request, rc)
      real(dp), intent(inout) :: buffer(:)
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
      integer, intent(in) :: count, datatype, root, comm
      call MPI_Ibcast(buffer, count, datatype, root, comm, request, rc)
   end subroutine cresta_bcast_begin_dble

!-- End BCAST interfaces

!-- Begin GATHER interfaces -------------------------------------------------

   subroutine cresta_gather_begin_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, &
                                     root, comm, request, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Igather(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm,& 
                       request, rc)
#else
      call MPI_Igather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm,& 
                       request, rc)
#endif
   end subroutine cresta_gather_begin_int

   subroutine cresta_gather_begin_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, &
                                       root, comm, request, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Igather(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm,&
                       request, rc)
#else
      call MPI_Igather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
#endif
   end subroutine cresta_gather_begin_real

   subroutine cresta_gather_begin_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, &
                                       root, comm, request, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Igather(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm,&
                       request, rc)
#else
      call MPI_Igather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
#endif
   end subroutine cresta_gather_begin_dble

!-- End GATHER interfaces

!-- Begin GATHERV interfaces -------------------------------------------------

   subroutine cresta_gatherv_begin_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, &
                                       recvtype, root, comm, request, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Igatherv(tmpbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, &
                       comm, request, rc)
#else
      call MPI_Igatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, &
                       comm, request, rc)
#endif
   end subroutine cresta_gatherv_begin_int

   subroutine cresta_gatherv_begin_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, &
                                        recvtype, root, comm, request, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Igatherv(tmpbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, &
                       comm, request, rc)
#else
      call MPI_Igatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, &
                        comm, request, rc)
#endif
   end subroutine cresta_gatherv_begin_real

   subroutine cresta_gatherv_begin_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs,&
                                         recvtype, root, comm, request, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Igatherv(tmpbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, &
                       comm, request, rc)
#else
      call MPI_Igatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, root, &
                        comm, request, rc)
#endif
   end subroutine cresta_gatherv_begin_dble

!-- End GATHERV interfaces


!-- Begin SCATTER interfaces -------------------------------------------------

   subroutine cresta_scatter_begin_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, &
                                       root, comm, request, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iscatter(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, &
                       request, rc)
#else
      call MPI_Iscatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, &
                       request, rc)
#endif
   end subroutine cresta_scatter_begin_int

   subroutine cresta_scatter_begin_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, &
                                       root, comm, request, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iscatter(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, &
                       request, rc)
#else
      call MPI_Iscatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, &
                       request, rc)
#endif
   end subroutine cresta_scatter_begin_real

   subroutine cresta_scatter_begin_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, &
                                       root, comm, request, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iscatter(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, &
                       request, rc)
#else
      call MPI_Iscatter(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, &
                       request, rc)
#endif
   end subroutine cresta_scatter_begin_dble


!-- End SCATTER interfaces

!-- Begin SCATTERV interfaces -------------------------------------------------

   subroutine cresta_scatterv_begin_int(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, &
                                        root, comm, request, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnts(:), displs(:), sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iscatterv(tmpbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
#else
      call MPI_Iscatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
#endif
   end subroutine cresta_scatterv_begin_int

   subroutine cresta_scatterv_begin_real(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, &
                                        root, comm, request, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnts(:), displs(:), sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iscatterv(tmpbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
#else
      call MPI_Iscatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
#endif
   end subroutine cresta_scatterv_begin_real

   subroutine cresta_scatterv_begin_dble(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, &
                                        root, comm, request, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnts(:), displs(:), sendtype, recvcnt, recvtype, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iscatterv(tmpbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
#else
      call MPI_Iscatterv(sendbuf, sendcnts, displs, sendtype, recvbuf, recvcnt, recvtype, root, comm, request, rc)
#endif
   end subroutine cresta_scatterv_begin_dble

!-- End SCATTERV interfaces


!-- Begin REDUCE interfaces -------------------------------------------------

   subroutine cresta_reduce_begin_int(sendbuf, recvbuf, count, datatype, op, root, &
                                      comm, request, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) ::  recvbuf(:)
      integer, intent(in) :: count, datatype, op, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ireduce(tmpbuf, recvbuf, count, datatype, op, root, comm, request, rc)
#else
      call MPI_Ireduce(sendbuf, recvbuf, count, datatype, op, root, comm, request, rc)
#endif
   end subroutine cresta_reduce_begin_int

   subroutine cresta_reduce_begin_real(sendbuf, recvbuf, count, datatype, op, root, &
                                      comm, request, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) ::  recvbuf(:)
      integer, intent(in) :: count, datatype, op, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ireduce(tmpbuf, recvbuf, count, datatype, op, root, comm, request, rc)
#else
      call MPI_Ireduce(sendbuf, recvbuf, count, datatype, op, root, comm, request, rc)
#endif
   end subroutine cresta_reduce_begin_real

   subroutine cresta_reduce_begin_dble(sendbuf, recvbuf, count, datatype, op, root, &
                                      comm, request, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) ::  recvbuf(:)
      integer, intent(in) :: count, datatype, op, root, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ireduce(tmpbuf, recvbuf, count, datatype, op, root, comm, request, rc)
#else
      call MPI_Ireduce(sendbuf, recvbuf, count, datatype, op, root, comm, request, rc)
#endif
   end subroutine cresta_reduce_begin_dble

!-- End REDUCE interfaces

!-- Begin ALLGATHER interfaces -------------------------------------------------

   subroutine cresta_allgather_begin_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, &
                                         recvtype, comm, request, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, comm
      integer, intent(out)  :: request
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallgather(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
#else
      call MPI_Iallgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
#endif
   end subroutine cresta_allgather_begin_int

   subroutine cresta_allgather_begin_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, &
                                         recvtype, comm, request, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, comm
      integer, intent(out)  :: request
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallgather(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
#else
      call MPI_Iallgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
#endif
   end subroutine cresta_allgather_begin_real

   subroutine cresta_allgather_begin_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, &
                                         recvtype, comm, request, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(out), optional :: rc
      integer, intent(in) :: sendcnt, sendtype, recvcnt, recvtype, comm
      integer, intent(out) :: request
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallgather(tmpbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
#else
      call MPI_Iallgather(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, request, rc)
#endif
   end subroutine cresta_allgather_begin_dble

!-- End ALLGATHER interfaces

!-- Begin ALLGATHERV interfaces -------------------------------------------------

   subroutine cresta_allgatherv_begin_int(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, &
                                          recvtype, comm, request, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallgatherv(tmpbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
#else
      call MPI_Iallgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
#endif
   end subroutine cresta_allgatherv_begin_int

   subroutine cresta_allgatherv_begin_real(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, &
                                          recvtype, comm, request, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallgatherv(tmpbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
#else
      call MPI_Iallgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
#endif
   end subroutine cresta_allgatherv_begin_real

   subroutine cresta_allgatherv_begin_dble(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, &
                                          recvtype, comm, request, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(in) :: sendcnt, sendtype, recvcnts(:), displs(:), recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallgatherv(tmpbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
#else
      call MPI_Iallgatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts, displs, recvtype, comm, request, rc)
#endif
   end subroutine cresta_allgatherv_begin_dble

!-- End ALLGATHERV interfaces

!-- Begin ALLREDUCE interfaces -------------------------------------------------

   subroutine cresta_allreduce_begin_int(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
      integer, intent(in) :: sendbuf(:)
      integer, intent(out) :: recvbuf(:)
      integer, intent(in) :: count, datatype, op, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallreduce(tmpbuf, recvbuf, count, datatype, op, comm, request, rc)
#else
      call MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
#endif
   end subroutine cresta_allreduce_begin_int

   subroutine cresta_allreduce_begin_real(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
      real, intent(in) :: sendbuf(:)
      real, intent(out) :: recvbuf(:)
      integer, intent(in) :: count, datatype, op, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallreduce(tmpbuf, recvbuf, count, datatype, op, comm, request, rc)
#else
      call MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
#endif
   end subroutine cresta_allreduce_begin_real

   subroutine cresta_allreduce_begin_dble(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
      real(dp), intent(in) :: sendbuf(:)
      real(dp), intent(out) :: recvbuf(:)
      integer, intent(in) :: count, datatype, op, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Iallreduce(tmpbuf, recvbuf, count, datatype, op, comm, request, rc)
#else
      call MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request, rc)
#endif
   end subroutine cresta_allreduce_begin_dble

!-- End ALLREDUCE interfaces

!-- Begin REDUCE_SCATTER interfaces -------------------------------------------------

   subroutine cresta_reduce_scatter_begin_int(sendbuf, recvbuf, recvcnts, datatype, op, &
                                              comm, request, rc)
      integer, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: recvcnts(:), datatype, op, comm
      integer, intent(out), optional :: rc
      integer, intent(out), optional :: request
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ireduce_scatter(tmpbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
#else
      call MPI_Ireduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
#endif
   end subroutine cresta_reduce_scatter_begin_int

   subroutine cresta_reduce_scatter_begin_real(sendbuf, recvbuf, recvcnts, datatype, op, &
                                              comm, request, rc)
      real, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: recvcnts(:), datatype, op, comm
      integer, intent(out), optional :: rc
      integer, intent(out), optional :: request
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ireduce_scatter(tmpbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
#else
      call MPI_Ireduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
#endif
   end subroutine cresta_reduce_scatter_begin_real

   subroutine cresta_reduce_scatter_begin_dble(sendbuf, recvbuf, recvcnts, datatype, op, &
                                              comm, request, rc)
      real(dp), intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: recvcnts(:), datatype, op, comm
      integer, intent(out), optional :: rc
      integer, intent(out), optional :: request
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ireduce_scatter(tmpbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
#else
      call MPI_Ireduce_scatter(sendbuf, recvbuf, recvcnts, datatype, op, comm, request, rc)
#endif
   end subroutine cresta_reduce_scatter_begin_dble

!-- End REDUCE_SCATTER interfaces

!-- Begin ALLTOALL interfaces -------------------------------------------------

   subroutine cresta_alltoall_begin_int(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, &
                                        comm, request, rc)
      integer, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: sendcount, sendtype, recvcount, recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ialltoall(tmpbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
#else
      call MPI_Ialltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
#endif
   end subroutine cresta_alltoall_begin_int

   subroutine cresta_alltoall_begin_real(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, &
                                        comm, request, rc)
      real, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: sendcount, sendtype, recvcount, recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ialltoall(tmpbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
#else
      call MPI_Ialltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
#endif
   end subroutine cresta_alltoall_begin_real

   subroutine cresta_alltoall_begin_dble(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, &
                                        comm, request, rc)
      real(dp), intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: sendcount, sendtype, recvcount, recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ialltoall(tmpbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
#else
      call MPI_Ialltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, rc)
#endif
   end subroutine cresta_alltoall_begin_dble

!-- End ALLTOALL interfaces

!-- Begin ALLTOALLV interfaces -------------------------------------------------

   subroutine cresta_alltoallv_begin_int(sendbuf, sendcnts, sdispls, sendtype, &
                                         recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
      integer, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: sendcnts(:), sdispls(:), sendtype, recvcnts(:), rdispls(:), recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      integer, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ialltoallv(tmpbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
#else
      call MPI_Ialltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
#endif
   end subroutine cresta_alltoallv_begin_int

   subroutine cresta_alltoallv_begin_real(sendbuf, sendcnts, sdispls, sendtype, &
                                         recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
      real, intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: sendcnts(:), sdispls(:), sendtype, recvcnts(:), rdispls(:), recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real, dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ialltoallv(tmpbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
#else
      call MPI_Ialltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
#endif
   end subroutine cresta_alltoallv_begin_real

   subroutine cresta_alltoallv_begin_dble(sendbuf, sendcnts, sdispls, sendtype, &
                                         recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
      real(dp), intent(inout) :: sendbuf(:), recvbuf(:)
      integer, intent(in) :: sendcnts(:), sdispls(:), sendtype, recvcnts(:), rdispls(:), recvtype, comm
      integer, intent(out) :: request
      integer, intent(out), optional :: rc
#if defined (_PROTECTED_BUFFER)
      real(dp), dimension(size(sendbuf)) :: tmpbuf
      tmpbuf = sendbuf
      call MPI_Ialltoallv(tmpbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
#else
      call MPI_Ialltoallv(sendbuf, sendcnts, sdispls, sendtype, &
                          recvbuf, recvcnts, rdispls, recvtype, comm, request, rc)
#endif
   end subroutine cresta_alltoallv_begin_dble

!-- End ALLTOALLV interfaces

!-- Begin Cresta_coll_end interfaces

  subroutine cresta_coll_end_single(count, request, rc)
    integer, intent(in) :: count
    integer, intent(in) :: request
    integer, intent(out), optional :: rc
    call MPI_Wait(request, MPI_STATUS_IGNORE, rc)
  end subroutine cresta_coll_end_single


  subroutine cresta_coll_end_multiple(count, requests, rc)
    integer, intent(in) :: count
    integer, dimension(:), intent(in) :: requests
    integer, intent(out), optional :: rc
    call MPI_Waitall(count, requests, MPI_STATUSES_IGNORE, rc)
  end subroutine cresta_coll_end_multiple

!-- End Cresta_coll_end interfaces
  
end module cresta_sp_coll
