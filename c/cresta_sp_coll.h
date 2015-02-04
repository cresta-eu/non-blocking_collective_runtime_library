/* CRESTA collectives: a wrapper library for different implementations of collective
    communication operations (CRESTA deliverable D4.5.3)

   This is experimental software, use with caution and on your own risk (=no warranty)!
*/

int CRESTA_Bcast_begin(void *buffer, int count, MPI_Datatype datatype, int root, 
               MPI_Comm comm, MPI_Request request );

int CRESTA_Gather_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
               void *recvbuf, int recvcnt, MPI_Datatype recvtype, 
               int root, MPI_Comm comm, MPI_Request request);

int CRESTA_Gatherv_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
                void *recvbuf, int *recvcnts, int *displs, 
                MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request request);

int CRESTA_Scatter_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
               void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, 
               MPI_Comm comm, MPI_Request request);

int CRESTA_Scatterv_begin( void *sendbuf, int *sendcnts, int *displs, 
                 MPI_Datatype sendtype, void *recvbuf, int recvcnt,
                 MPI_Datatype recvtype,
                 int root, MPI_Comm comm, MPI_Request request);

int CRESTA_Reduce_begin(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, 
               MPI_Op op, int root, MPI_Comm comm, MPI_Request request);

int CRESTA_Allgather_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
               void *recvbuf, int recvcnt, MPI_Datatype recvtype,
               MPI_Comm comm, MPI_Request request);

int CRESTA_Allgatherv_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
                void *recvbuf, int *recvcnts, int *displs,
                MPI_Datatype recvtype, MPI_Comm comm, MPI_Request request);

int CRESTA_Allreduce_begin(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
               MPI_Op op, MPI_Comm comm, MPI_Request request);

int CRESTA_Reduce_scatter_begin(void *sendbuf, void *recvbuf, int *recvcnts, 
                      MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request request);

int CRESTA_Alltoall_begin(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
                 void *recvbuf, int recvcnt, MPI_Datatype recvtype, 
                 MPI_Comm comm, MPI_Request request);

int CRESTA_Alltoallv_begin( void *sendbuf, int *sendcnts, int *sdispls,
                 MPI_Datatype sendtype, void *recvbuf, int *recvcnts,
                 int *rdispls, MPI_Datatype recvtype,
                 MPI_Comm comm, MPI_Request request);

int CRESTA_Coll_end(int count, MPI_Request requests[]);
