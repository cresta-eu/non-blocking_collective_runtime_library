# Makefile for the CRESTA Collective Communication Library
#
# Insert the proper compiler wrapper and compiler flags.

F95	= ftn
FFLAGS  = -O3 
AR	= ar rs
LD	= ld

# DFLAGS_LIB:
# Control how to compile the CRESTA Collectives wrapper library itself.
# 1. MPI 3.0 nonblocking collectives: -D_NONBLOCK_<ROUTINE NAME>, e.g., -D_NONBLOCK_ALLTOALL
#    o all collectives available
#    o wildcard -D_ALL_NONBLOCK will make all collectives nonblocking, which have not been
#      specified to be CAF or one-sided
# 2. CAF implementations: -D_CAF_<ROUTINE NAME>, e.g. -D_CAF_ALLTOALL
#    o only non-vector collectives currently available
# 3. One-sided implementations: -D_OS_<ROUTINE NAME>, e.g. -D_OS_ALLTOALL
#    o only Bcast, Gather, Scatter, Reduce, Allreduce and Alltoall curretly available, 
#       and they are clearly slower than other alternatives
# 4. If no flags are given here, all CRESTA_Xxxx <=> MPI_Xxxx.

DFLAGS_LIB = 

# DFLAGS_CAF:
# Control how to compile the collectives implemented with
# Fortran coarrays. Recognized flags are 
# -D_STATIC_COARRAYS : use statically allocated coarrays (common areas) that 
#                      exist throughout the program execution.
#                      They are usually much faster than
#                      allocating own coarrays for every single call.
#                      Use these if the collective data is not too large to handle.
# -DTMP_COARRAY_SIZE=N  : size of the statically allocated arrays. Not referred if
#                         -D_STATIC_COARRAYS is not set.
# -DNROOTS=N : number of intermediate roots for rooted collectives. The best value
#              depends on the number of cores.           

DFLAGS_CAF = -D_STATIC_COARRAYS -DTMP_COARRAY_SIZE=1000000

# DFLAGS_OS:
# Control how to compile the collectives implemented with
# MPI_Put/Get/Accumulate operations. Recognized flags are
# -D_STATIC_OS_WIN : use statically allocated common blocks & related memory windows that
#                      exist throughout the program execution.
#                      They are usually much faster than
#                      creating windows during every single call.
#                      Use these if the collective data is not too large to handle.
# -DTMP_OS_WIN_SIZE=N  : size of the statically allocated temporary arrays. Not referred if
#                         -D_STATIC_OS_WIN is not set.

DFLAGS_OS = -D_STATIC_OS_WIN -DTMP_OS_WIN_SIZE=1000000

# DFLAGS_SP:
# Control how to build the split-phase API
# -D_PROTECTED_BUFFER : make an extra memory copy such that the send/receive buffer
#                       of the overlapped collective can be read also before
#                       the return of CRESTA_Coll_end

DFLAGS_SP =

# Remove object files if necessary (e.g. your compiler does not support coarrays)

LIB	= libcrestacoll.a
LIB_SHR	= libcrestacoll.so
OBJ_LIB	= caf_collectives.o os_collectives.o sp_collectives.o crestacoll.o
OBJ_SHR = caf_collectives_s.o os_collectives_s.o sp_collectives_s.o crestacoll_s.o
OBJ_CAF = caf_collectives.o
OBJ_OS	= os_collectives.o caf_collectives.o
OBJ_SP	= sp_collectives.o caf_collectives.o
TEST_BIN = test_bcast test_gather test_scatter test_allgather test_allreduce test_alltoall test_reduce_scatter test_reduce
OVL_BIN	= ovrl_alltoall ovrl_allreduce

default: static

help:
	@echo " static - build a static (.a) version the library"
	@echo " shared - build a shared (.so) version of the library"
	@echo " caf - build just the Fortran coarrays part"
	@echo " os - build just the MPI one-sided part"
	@echo " sp - build the split-phase API"
	@echo " test - build testcases (requires compiled library)"
	@echo " ovl - build overlap benchmarks (requires compiled library)"
clean:
	@/bin/rm -f *.mod $(TEST_BIN) $(LIB) $(LIB_SHR) $(OBJ_LIB) $(OBJ_SHR)


static	: $(OBJ_LIB)
	@rm -f $(LIB)
	@$(AR) $(LIB) $(OBJ_LIB)
shared	: $(OBJ_SHR)
	@rm -f $(LIB_SHR)
	@$(LD) -shared -o $(LIB_SHR) $(OBJ_SHR)
caf	: $(OBJ_CAF)
os	: $(OBJ_OS)
sp	: $(OBJ_SP)
test	: $(TEST_BIN)
ovl	: $(OVL_BIN)

crestacoll.o	: caf_collectives.o os_collectives.o sp_collectives.o
	$(F95) -c $(FFLAGS) $(DFLAGS_LIB) crestacoll.F90
caf_collectives.o	: caf_collectives.F90
	$(F95) -c $(FFLAGS) $(DFLAGS_CAF) caf_collectives.F90
os_collectives.o       : os_collectives.F90
	$(F95) -c $(FFLAGS) $(DFLAGS_OS) os_collectives.F90
sp_collectives.o       : sp_collectives.F90
	$(F95) -c $(FFLAGS) $(DFLAGS_SP) sp_collectives.F90
crestacoll_s.o    : caf_collectives_s.o os_collectives_s.o sp_collectives_s.o
	$(F95) -c -fPIC $(FFLAGS) $(DFLAGS_LIB) -o crestacoll_s.o crestacoll.F90
caf_collectives_s.o       : caf_collectives.F90
	$(F95) -c -fPIC $(FFLAGS) $(DFLAGS_CAF) -o caf_collectives_s.o caf_collectives.F90
os_collectives_s.o       : os_collectives.F90
	$(F95) -c -fPIC $(FFLAGS) $(DFLAGS_OS) -o os_collectives_s.o os_collectives.F90
sp_collectives_s.o       : sp_collectives.F90
	$(F95) -c -fPIC $(FFLAGS) $(DFLAGS_SP) -o sp_collectives_s.o os_collectives.F90
test_bcast : test_bcast.f90
	$(F95) -o test_bcast test_bcast.f90 -L. -lcrestacoll
test_gather : test_gather.f90
	$(F95) -o test_gather test_gather.f90 -L. -lcrestacoll
test_reduce : test_reduce.f90
	$(F95) -o test_reduce test_reduce.f90 -L. -lcrestacoll
test_scatter : test_scatter.f90
	$(F95) -o test_scatter test_scatter.f90 -L. -lcrestacoll
test_allgather : test_allgather.f90
	$(F95) -o test_allgather test_allgather.f90 -L. -lcrestacoll
test_allreduce : test_allreduce.f90
	$(F95) -o test_allreduce test_allreduce.f90 -L. -lcrestacoll
test_alltoall : test_alltoall.f90
	$(F95) -o test_alltoall test_alltoall.f90 -L. -lcrestacoll
test_reduce_scatter	: test_reduce_scatter.f90
	$(F95) -o test_reduce_scatter test_reduce_scatter.f90 -L. -lcrestacoll
ovrl_allreduce : ovrl_allreduce.f90
	$(F95) -o ovrl_allreduce ovrl_allreduce.f90 -L. -lcrestacoll
ovrl_alltoall : ovlr_alltoall.f90
	$(F95) -o ovrl_alltoall ovrl_alltoall.f90 -L. -lcrestacoll



