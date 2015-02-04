/* CRESTA collectives: a wrapper library for different implementations of collective
    communication operations (CRESTA deliverable D4.5.3)

   This is experimental software, use with caution and on your own risk (=no warranty)!
*/

int CRESTA_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, 
               MPI_Comm comm );

int CRESTA_Gather(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
               void *recvbuf, int recvcnt, MPI_Datatype recvtype, 
               int root, MPI_Comm comm);

int CRESTA_Gatherv(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
                void *recvbuf, int *recvcnts, int *displs, 
                MPI_Datatype recvtype, int root, MPI_Comm comm);

int CRESTA_Scatter(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
               void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, 
               MPI_Comm comm);

int CRESTA_Scatterv( void *sendbuf, int *sendcnts, int *displs, 
                 MPI_Datatype sendtype, void *recvbuf, int recvcnt,
                 MPI_Datatype recvtype,
                 int root, MPI_Comm comm);

int CRESTA_Reduce(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, 
               MPI_Op op, int root, MPI_Comm comm);

int CRESTA_Allgather(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
               void *recvbuf, int recvcnt, MPI_Datatype recvtype,
               MPI_Comm comm);

int CRESTA_Allgatherv(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
                void *recvbuf, int *recvcnts, int *displs,
                MPI_Datatype recvtype, MPI_Comm comm);

int CRESTA_Allreduce(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
               MPI_Op op, MPI_Comm comm);

int CRESTA_Reduce_scatter(void *sendbuf, void *recvbuf, int *recvcnts, 
                      MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);

int CRESTA_Alltoall(void *sendbuf, int sendcnt, MPI_Datatype sendtype, 
                 void *recvbuf, int recvcnt, MPI_Datatype recvtype, 
                 MPI_Comm comm);

int CRESTA_Alltoallv( void *sendbuf, int *sendcnts, int *sdispls,
                 MPI_Datatype sendtype, void *recvbuf, int *recvcnts,
                 int *rdispls, MPI_Datatype recvtype,
                 MPI_Comm comm);
