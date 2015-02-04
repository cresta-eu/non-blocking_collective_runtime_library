#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include "os_collectives.h"

int OS_Bcast(void *buffer, int sendcount, MPI_Datatype datatype, int root, MPI_Comm comm)
{
   int my_id;
   MPI_Aint disp;   
   MPI_Win sendwin;
   MPI_Win_create(buffer, sendcount*typesize(datatype), typesize(datatype), 
                  MPI_INFO_NULL, comm,  &sendwin);  
   MPI_Comm_rank(comm, &my_id);
   disp = 0;
   MPI_Win_fence(MPI_MODE_NOPRECEDE, sendwin);
   if (my_id != root) {
      MPI_Get(buffer, sendcount, datatype, root, disp, sendcount, datatype,
              sendwin);
   }
   MPI_Win_fence(MPI_MODE_NOSUCCEED, sendwin);
   MPI_Win_free(&sendwin);
   return (MPI_SUCCESS);
}

int OS_Gather(void *sendbuf, int sendcount, void *recvbuf, MPI_Datatype datatype, 
              int root, MPI_Comm comm)
{
   int my_id;
   MPI_Aint disp;
   MPI_Win recvwin;
   MPI_Win_create(recvbuf, sendcount*typesize(datatype), typesize(datatype), MPI_INFO_NULL,
                  comm, &recvwin);
   MPI_Comm_rank(comm, &my_id);
   disp = my_id*sendcount;
   MPI_Win_fence(MPI_MODE_NOPRECEDE, recvwin);
   MPI_Put(sendbuf, sendcount, datatype, root, disp, sendcount, datatype,
           recvwin);
   MPI_Win_fence(MPI_MODE_NOSUCCEED, recvwin);
   MPI_Win_free(&recvwin);
   return (MPI_SUCCESS);
}

int OS_Scatter(void *sendbuf, int sendcount, void *recvbuf, MPI_Datatype datatype, 
               int root, MPI_Comm comm)
{
   int my_id;
   MPI_Aint disp;
   MPI_Win sendwin;
   MPI_Win_create(sendbuf, sendcount*typesize(datatype), typesize(datatype), MPI_INFO_NULL, 
                 comm, &sendwin);
   MPI_Comm_rank(comm, &my_id);
   disp = my_id*sendcount;
   MPI_Win_fence(MPI_MODE_NOPRECEDE|MPI_MODE_NOPUT, sendwin);
   MPI_Get(recvbuf, sendcount, datatype, root, disp, sendcount, datatype,
              sendwin);
   MPI_Win_fence(MPI_MODE_NOSUCCEED|MPI_MODE_NOSTORE, sendwin);
   MPI_Win_free(&sendwin);
   return (MPI_SUCCESS);
}

int OS_Reduce(void *sendbuf, void *recvbuf, int count,  
              MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm)
{
   int my_id;
   MPI_Aint disp;
   MPI_Win recvwin;
   MPI_Win_create(recvbuf, count*typesize(datatype), typesize(datatype), MPI_INFO_NULL, 
                  comm, &recvwin);
   MPI_Comm_rank(comm, &my_id);
   disp = 0;
   MPI_Win_fence(MPI_MODE_NOPRECEDE, recvwin);
   if (my_id == root)
      MPI_Put(sendbuf, count, datatype, root, disp, count, datatype, recvwin);
   MPI_Win_fence(0, recvwin);
   if (my_id != root)
      MPI_Accumulate(sendbuf, count, datatype, root, disp, count, datatype,
             op, recvwin);
   MPI_Win_fence(MPI_MODE_NOSUCCEED, recvwin);
   MPI_Win_free(&recvwin);
   return (MPI_SUCCESS);
}

int OS_Allreduce(void *sendbuf, void *recvbuf, int count,
              MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
   int my_id, ntask, target;
   MPI_Aint disp;
   MPI_Win recvwin;
   MPI_Win_create(recvbuf, count*typesize(datatype), typesize(datatype), MPI_INFO_NULL,
                  comm, &recvwin);
   MPI_Comm_rank(comm, &my_id);
   MPI_Comm_size(comm, &ntask);
   disp = 0;
   MPI_Win_fence(MPI_MODE_NOPRECEDE, recvwin);
   MPI_Put(sendbuf, count, datatype, my_id, disp, count, datatype, recvwin);
   MPI_Win_fence(0, recvwin);
   for (target = my_id+1; target < ntask; target++){
      MPI_Accumulate(sendbuf, count, datatype, target, disp, count, datatype,
                     op, recvwin);
   }
   for (target = 0; target < my_id; target++){
      MPI_Accumulate(sendbuf, count, datatype, target, disp, count, datatype,
                     op, recvwin);      
   }
   MPI_Win_fence(MPI_MODE_NOSUCCEED, recvwin);
   MPI_Win_free(&recvwin);
   return (MPI_SUCCESS);
}

int typesize(MPI_Datatype datatype)
{
   int size;
   if (datatype == MPI_INT) {
     size = sizeof(int);
   } else if (datatype == MPI_FLOAT) {
     size = sizeof(float);
   } else if (datatype == MPI_DOUBLE) {
     size = sizeof(double);
   } else if (datatype == MPI_FLOAT) {
     size = sizeof(float);
   } else { 
     printf("Unsupported datatype (user-defined type?) in CRESTA collective (one-sided)\n");
     MPI_Abort(MPI_COMM_WORLD, 1);
   }
   return (size);
}
