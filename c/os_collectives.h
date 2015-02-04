int OS_Bcast(void *buffer, int sendcount, MPI_Datatype datatype, int root, MPI_Comm comm);
int OS_Gather(void *sendbuf, int sendcount, void *recvbuf, MPI_Datatype datatype, int root, 
              MPI_Comm comm);
int OS_Scatter(void *sendbuf, int sendcount, void *recvbuf, MPI_Datatype datatype, int root, 
               MPI_Comm comm);
int OS_Reduce(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, int root,
              MPI_Comm comm);
int OS_Allreduce(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
              MPI_Comm comm);
int OS_Alltoall(void *sendbuf, int sendcount, void *recvbuf, MPI_Datatype datatype,
                MPI_Comm comm);
int typesize(MPI_Datatype datatype);

