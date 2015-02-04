/* 
   CRESTA collectives: a wrapper library for different implementations 
   of collective communication operations 
   (CRESTA project deliverable D4.5.3)

   This is an experimental software, use with caution and on 
   your own risk (=no warranty)!
*/

#include <mpi.h>
#include "os_collectives.h"

int CRESTA_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, 
               MPI_Comm comm )
{
   int rc;
#if defined(_OS_BCAST)
  rc = OS_Bcast(buffer, count, datatype, root, comm);
#elif defined(_NONBLOCK_BCAST) || defined(_ALL_NONBLOCK)
  MPI_Request request;
  rc = MPI_Ibcast(buffer, count, datatype, root, comm, &request );
  MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc = MPI_Bcast(buffer, count, datatype, root, comm);
#endif
  return (rc);
}

int CRESTA_Gather(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
               void *recvbuf, int recvcnt, MPI_Datatype recvtype, 
               int root, MPI_Comm comm)
{
   int rc;
#if defined (_OS_GATHER)
   rc=OS_Gather(sendbuf, sendcnt, recvbuf, sendtype, root, comm);
#elif defined(_NONBLOCK_GATHER) || defined(_ALL_NONBLOCK)
   MPI_Request request;
   rc=MPI_Igather(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm, &request);
   MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Gather(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm);
#endif
  return (rc);
}

int CRESTA_Gatherv(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
                void *recvbuf, int *recvcnts, int *displs, 
                MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  int rc;
#if defined(_NONBLOCK_GATHERV) || defined(_ALL_NONBLOCK)
  MPI_Request request;
  rc=MPI_Igatherv(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnts, displs, recvtype,
               root, comm, &request);
  MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Gatherv(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnts, displs, recvtype,
               root, comm);
#endif
  return (rc);
}

int CRESTA_Scatter(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
               void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, 
               MPI_Comm comm)
{
   int rc;
#if defined(_OS_SCATTER)
   rc=OS_Scatter(sendbuf, sendcnt, recvbuf, sendtype, root, comm);
#elif defined(_NONBLOCK_SCATTER) || defined(_ALL_NONBLOCK)
   MPI_Request request;
   rc=MPI_Iscatter(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm, &request);
   MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Scatter(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm);
#endif
  return (rc);
}

int CRESTA_Scatterv( void *sendbuf, int *sendcnts, int *displs, 
                 MPI_Datatype sendtype, void *recvbuf, int recvcnt,
                 MPI_Datatype recvtype,
                 int root, MPI_Comm comm)
{
  int rc;
#if defined(_NONBLOCK_SCATTERV) || defined(_ALL_NONBLOCK)
  MPI_Request request;
  rc=MPI_Iscatterv(sendbuf, sendcnts, displs, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm, &request);
  MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Scatterv(sendbuf, sendcnts, displs, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm);
#endif
  return (rc);
}

int CRESTA_Reduce(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, 
               MPI_Op op, int root, MPI_Comm comm)
{
  int rc;
#if defined(_OS_REDUCE)
  rc=OS_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm);
#elif defined(_NONBLOCK_REDUCE) || defined(_ALL_NONBLOCK)
  MPI_Request request;
  rc=MPI_Ireduce(sendbuf, recvbuf, count, datatype,
               op, root, comm, &request);
  MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Reduce(sendbuf, recvbuf, count, datatype,
               op, root, comm);
#endif
 return (rc);
}

int CRESTA_Allgather(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
               void *recvbuf, int recvcnt, MPI_Datatype recvtype,
               MPI_Comm comm)
{
   int rc;
#if defined(_NONBLOCK_ALLGATHER) || defined(_ALL_NONBLOCK)
   MPI_Request request;
   rc=MPI_Iallgather(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               comm, &request);
   MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Allgather(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               comm);
#endif
  return (rc);
}

int CRESTA_Allgatherv(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
                void *recvbuf, int *recvcnts, int *displs,
                MPI_Datatype recvtype, MPI_Comm comm)
{
  int rc;
#if defined(_NONBLOCK_ALLGATHERV) || defined(_ALL_NONBLOCK)
  MPI_Request request;
  rc=MPI_Iallgatherv(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnts, displs, recvtype,
               comm, &request);
  MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Allgatherv(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnts, displs, recvtype,
               comm);
#endif
  return (rc);
}

int CRESTA_Allreduce(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
               MPI_Op op, MPI_Comm comm)
{
  int rc;
#if defined (_OS_ALLREDUCE)
  rc=OS_Allreduce(sendbuf, recvbuf, count, datatype, op, comm);
#elif defined(_NONBLOCK_ALLREDUCE) || defined(_ALL_NONBLOCK)
  MPI_Request request;
  rc=MPI_Iallreduce(sendbuf, recvbuf, count, datatype,
               op, comm, &request);
  MPI_Wait(&request, MPI_STATUS_IGNORE);
  return (rc);
#else
  rc=MPI_Allreduce(sendbuf, recvbuf, count, datatype,
               op, comm);
#endif
  return (rc);
}

int CRESTA_Reduce_scatter(void *sendbuf, void *recvbuf, int *recvcnts, 
                      MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  int rc;
#if defined(_NONBLOCK_REDUCE_SCATTER) || defined(_ALL_NONBLOCK)
  MPI_Request request;
  rc=MPI_Ireduce_scatter(sendbuf, recvbuf, recvcnts, datatype,
               op, comm, &request);
  MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Reduce_scatter(sendbuf, recvbuf, recvcnts, datatype,
               op, comm);
#endif
  return (rc);
}

int CRESTA_Alltoall(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
                 void *recvbuf, int recvcnt, MPI_Datatype recvtype, 
                 MPI_Comm comm)
{
   int rc;
#if defined (_OS_ALLTOALL)
   rc=OS_Alltoall(sendbuf, sendcnt, recvbuf, sendtype, comm);
#elif defined(_NONBLOCK_ALLTOALL) || defined(_ALL_NONBLOCK)
   MPI_Request request;
   rc=MPI_Ialltoall(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               comm, &request);
   MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
   rc=MPI_Alltoall(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               comm);
#endif
   return (rc);
}

int CRESTA_Alltoallv( void *sendbuf, int *sendcnts, int *sdispls,
                 MPI_Datatype sendtype, void *recvbuf, int *recvcnts,
                 int *rdispls, MPI_Datatype recvtype,
                 MPI_Comm comm)
{
  int rc;
#if defined(_NONBLOCK_ALLTOALLV) || defined(_ALL_NONBLOCK)
  MPI_Request request;
  rc=MPI_Ialltoallv(sendbuf, sendcnts, sdispls, sendtype,
               recvbuf, recvcnts, rdispls, recvtype,
               comm, &request);
  MPI_Wait(&request, MPI_STATUS_IGNORE);
#else
  rc=MPI_Alltoallv(sendbuf, sendcnts, sdispls, sendtype,
               recvbuf, recvcnts, rdispls, recvtype,
               comm);
#endif
  return (rc);
}


