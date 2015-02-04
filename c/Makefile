# Makefile for the CRESTA Collectives library
#
# Insert proper compiler and compiler flags.

CC	= cc
AR	= ar rs
CFLAGS_LIB	=
CFLAGS_TEST	= 

# Control how to compile the CRESTA Collectives wrapper library itself.
#
# 1. MPI 3.0 nonblocking collectives: -D_NONBLOCK_<ROUTINE NAME>, e.g., -D_NONBLOCK_ALLTOALL
#    o all collectives available
#    o wildcard -D_ALL_NONBLOCK makes all collectives nonblocking
# 2. One-sided implementations: -D_OS_<ROUTINE NAME>, e.g. -D_OS_ALLTOALL
#    o only Bcast, Gather, Scatter, Reduce, Allreduce and Alltoall currently available
# 3. If no flags are given here, all CRESTA_Xxxx <=> MPI_Xxxx.

DFLAGS_LIB = -D_OS_BCAST -D_OS_GATHER -D_OS_SCATTER -D_OS_REDUCE -D_OS_ALLREDUCE -D_OS_ALLTOALL

# control how to compile the collectives implemented with
# MPI_Put/Get/Accumulate operations. Recognized flags are
# -D_STATIC_OS_WIN : use statically allocated common blocks & related memory windows that
#                      exist throughout the program execution.
#                      They are usually much faster than
#                      creating windows during every single call.
#                      Use these if the collective data is not too large to handle.
# -DTMP_OS_WIN_SIZE=N  : size of the statically allocated temporary arrays. Not referred if
#                         -D_STATIC_OS_WIN is not set.


DFLAGS_OS = 

# No changes should be needed hereafter ------------------------

LIB	= libcrestacoll.a
LIB_SHR	= libcrestacoll.so
OBJ_LIB	= os_collectives.o sp_collectives.o crestacoll.o
OBJ_SHR = os_collectives_s.o sp_collectives_s.o crestacoll_s.o
TEST_BIN = test_bcast test_gather test_scatter test_reduce test_allreduce test_alltoall

default: lib

help:
	@echo " lib (default) - build the library"
	@echo " os - build just the MPI one-sided part"
	@echo " sp - build the split-phase communication API"
	@echo " test - build testcases (requires compiled library)"
clean:
	@/bin/rm -f *.o *.a $(TEST_BIN) $(LIB) $(LIB_SHR)


lib	: $(OBJ_LIB)
	@/bin/rm -f $(LIB)
	@$(AR) $(LIB) $(OBJ_LIB)
shared	: $(OBJ_SHR)
	$(CC) -shared -o $(LIB_SHR) $(OBJ_SHR) 
os	: os_collectives.o
sp	: sp_collectives.o
test	: $(TEST_BIN)

crestacoll.o	: crestacoll.c os_collectives.o sp_collectives.o
	$(CC) -c $(CFLAGS_LIB) $(DFLAGS_LIB) crestacoll.c
os_collectives.o       : os_collectives.c
	$(CC) -c $(CFLAGS_LIB) $(DFLAGS_OS) os_collectives.c
sp_collectives.o       : sp_collectives.c
	$(CC) -c $(CFLAGS_LIB)  sp_collectives.c
crestacoll_s.o    : crestacoll.c os_collectives.o sp_collectives.o
	$(CC) -fpic -c $(CFLAGS_LIB) $(DFLAGS_LIB) -o crestacoll_s.o crestacoll.c
os_collectives_s.o       : os_collectives.c
	$(CC) -fpic -c $(CFLAGS_LIB) $(DFLAGS_OS) -o os_collectives_s.o os_collectives.c
sp_collectives_s.o       : sp_collectives.c
	$(CC) -fpic -c $(CFLAGS_LIB) -o sp_collectives_s.o sp_collectives.c

test_bcast : test_bcast.c
	$(CC) $(CFLAGS_TEST) -o test_bcast test_bcast.c -L. -lcrestacoll
test_gather	: test_gather.c
	$(CC) $(CFLAGS_TEST) -o test_gather test_gather.c -L. -lcrestacoll
test_scatter	: test_scatter.c
	$(CC) $(CFLAGS_TEST) -o test_scatter test_scatter.c -L. -lcrestacoll
test_reduce	: test_reduce.c
	$(CC) $(CFLAGS_TEST) -o test_reduce test_reduce.c -L. -lcrestacoll
test_allreduce     : test_allreduce.c
	$(CC) $(CFLAGS_TEST) -o test_allreduce test_allreduce.c -L. -lcrestacoll
test_alltoall	: test_alltoall.c
	$(CC) $(CFLAGS_TEST) -o test_alltoall test_alltoall.c -L. -lcrestacoll


