/* 
   CRESTA collectives: a wrapper library for different implementations 
   of collective communication operations 
   (CRESTA project deliverable D4.5.3)

   This is an experimental software, use with caution and on 
   your own risk (=no warranty)!
*/

#include <mpi.h>

int CRESTA_Bcast_begin(void *buffer, int count, MPI_Datatype datatype, int root, 
               MPI_Comm comm, MPI_Request request )
{
   int rc;
   rc = MPI_Ibcast(buffer, count, datatype, root, comm, &request );
   return (rc);
}

int CRESTA_Gather_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
               void *recvbuf, int recvcnt, MPI_Datatype recvtype, 
               int root, MPI_Comm comm, MPI_Request request)
{
   int rc;
   rc=MPI_Igather(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm, &request);
   return (rc);
}

int CRESTA_Gatherv_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
                void *recvbuf, int *recvcnts, int *displs, 
                MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request request)
{
  int rc;
  rc=MPI_Igatherv(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnts, displs, recvtype,
               root, comm, &request);
  return (rc);
}

int CRESTA_Scatter_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
               void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, 
               MPI_Comm comm, MPI_Request request)
{
   int rc;
   rc=MPI_Iscatter(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm, &request);
   return (rc);
}

int CRESTA_Scatterv_begin( void *sendbuf, int *sendcnts, int *displs, 
                 MPI_Datatype sendtype, void *recvbuf, int recvcnt,
                 MPI_Datatype recvtype,
                 int root, MPI_Comm comm, MPI_Request request)
{
  int rc;
  rc=MPI_Iscatterv(sendbuf, sendcnts, displs, sendtype,
               recvbuf, recvcnt, recvtype,
               root, comm, &request);
  return (rc);
}

int CRESTA_Reduce_begin(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, 
               MPI_Op op, int root, MPI_Comm comm, MPI_Request request)
{
  int rc;
  rc=MPI_Ireduce(sendbuf, recvbuf, count, datatype,
               op, root, comm, &request);
  return (rc);
}

int CRESTA_Allgather_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
               void *recvbuf, int recvcnt, MPI_Datatype recvtype,
               MPI_Comm comm, MPI_Request request)
{
   int rc;
   rc=MPI_Iallgather(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               comm, &request);
   return (rc);
}

int CRESTA_Allgatherv_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
                void *recvbuf, int *recvcnts, int *displs,
                MPI_Datatype recvtype, MPI_Comm comm, MPI_Request request)
{
  int rc;
  rc=MPI_Iallgatherv(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnts, displs, recvtype,
               comm, &request);
  return (rc);
}

int CRESTA_Allreduce_begin(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
               MPI_Op op, MPI_Comm comm, MPI_Request request)
{
  int rc;
  rc=MPI_Iallreduce(sendbuf, recvbuf, count, datatype,
               op, comm, &request);
  return (rc);
}

int CRESTA_Reduce_scatter_begin(void *sendbuf, void *recvbuf, int *recvcnts, 
                      MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request request)
{
  int rc;
  rc=MPI_Ireduce_scatter(sendbuf, recvbuf, recvcnts, datatype,
               op, comm, &request);
  return (rc);
}

int CRESTA_Alltoall_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
                 void *recvbuf, int recvcnt, MPI_Datatype recvtype, 
                 MPI_Comm comm, MPI_Request request)
{
   int rc;
   rc=MPI_Ialltoall(sendbuf, sendcnt, sendtype,
               recvbuf, recvcnt, recvtype,
               comm, &request);
   return (rc);
}

int CRESTA_Alltoallv_begin( void *sendbuf, int *sendcnts, int *sdispls,
                 MPI_Datatype sendtype, void *recvbuf, int *recvcnts,
                 int *rdispls, MPI_Datatype recvtype,
                 MPI_Comm comm, MPI_Request request)
{
  int rc;
  rc=MPI_Ialltoallv(sendbuf, sendcnts, sdispls, sendtype,
               recvbuf, recvcnts, rdispls, recvtype,
               comm, &request);
  return (rc);
}

int CRESTA_Coll_end(int count, MPI_Request requests[])
{
  int rc;
  rc=MPI_Waitall(count, requests, MPI_STATUSES_IGNORE);
}
