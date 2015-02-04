#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <math.h>
#include "crestacoll.h"

int main(int argc, char *argv[])
{
   int *sendbuf, *testbuf;
   double t0, t1, time, tave, tmax, tmin;
   int ntask, my_id, root;
   int n, imsg, i, iter, maxiter, large_msg=8192;
   char filename[30];
   FILE *fp;

   MPI_Init(&argc, &argv);
   MPI_Comm_rank(MPI_COMM_WORLD, &my_id);
   MPI_Comm_size(MPI_COMM_WORLD, &ntask);
   root = ntask-1;
  
   if (my_id==0) {
      sprintf(filename, "cresta_bcast%i.dat", ntask);
      fp=fopen(filename,"a");
      fprintf(fp, "# Msg [B]    t_max [us]  t_min [us]  t_ave [us] \n");
   }
   
/* benchmark loop over message sizes */

   for(imsg=0; imsg<=16; imsg+=2){ 
     n = pow(2, imsg); 
     sendbuf = (int*) malloc(n*sizeof(int));
     testbuf = (int*) malloc(n*sizeof(int)); 
     if (n <= large_msg) {
       maxiter = 50;
     } else {
       maxiter = 10;
     }
     if (my_id==root) {
       for (i=0; i<n; i++) {
          sendbuf[i] = my_id*10 + i;
          testbuf[i] = sendbuf[i];
       }
     } else {
       for (i=0; i<n; i++) {
          sendbuf[i] = 0;
          testbuf[i] = 0;
       }
     }
     MPI_Barrier(MPI_COMM_WORLD);
     /* time the collective, repeated by maxiter times */
     time = 0.0;
     for (iter=1; iter<=maxiter; iter++) {
        t0 = MPI_Wtime();
        CRESTA_Bcast(sendbuf, n, MPI_INT, root, MPI_COMM_WORLD); 
        t1 = MPI_Wtime() - t0;
        time += t1;
     } 
     time = time / (double) maxiter;
     /* verify correctness by comparing with MPI */
     MPI_Bcast(testbuf, n, MPI_INT, root, MPI_COMM_WORLD);
     MPI_Barrier(MPI_COMM_WORLD);
     for (i=0; i<n; i++) {
        if (sendbuf[i] != testbuf[i]) {
           printf("Error in CRESTA_Bcast, n=%i, my_id=%i, %i %i \n",
                   sizeof(n)*n, my_id, sendbuf[i], testbuf[i]);
           MPI_Barrier(MPI_COMM_WORLD);
           MPI_Abort(MPI_COMM_WORLD, 1);
        }
     }   
     /* gather and print timings */
     MPI_Reduce(&time, &tmax, 1, MPI_DOUBLE, MPI_MAX, 0, 
                MPI_COMM_WORLD);
     MPI_Reduce(&time, &tmin, 1, MPI_DOUBLE, MPI_MIN, 0,
                MPI_COMM_WORLD);
     MPI_Reduce(&time, &tave, 1, MPI_DOUBLE, MPI_SUM, 0,
                MPI_COMM_WORLD); 
     if (my_id==0) {
        fprintf(fp, "%i %15.3f %15.3f %15.3f \n", 
                sizeof(n)*n, 1.0e6*tmax, 1.0e6*tmin, 
                1.0e6*tave/((double) ntask));
     } 
     free(sendbuf);
     free(testbuf);
     sendbuf=NULL;
     testbuf=NULL;
   }  /* end benchmark loop */
      
   if (my_id==0) fclose(fp); 
   MPI_Finalize(); 
   return 0; 
}
