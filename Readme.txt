############################################################
####CRESTA Collective Communication Library#################
############################################################

The purpose of the CRESTA Collective Communication Library is to provide an easy
interface for the application programmer to experiment with alternative approaches to
possibly alleviate the bottleneck collectives, with minimal changes to the source code
and fully without having to change algorithms or data structures.
The design is such that the programmer can convert some or all of the collectives to
the CRESTA collectives, and is always able to return to the original MPI collectives with
a simple library recompilation. The implementation is selected separately for different
collectives, but such that all (e.g.) CRESTA_Bcast in the program are using the same
implementation. The user needs to the change of the "MPI_" sentinel in a collective
operation to "CRESTA_", keeping the same call arguments.

The library is available as C and Fortran versions for easier interoperability with the
program, but they differ in available implementations. In its initial scope, the library may
implement a collective with:
- Original, "blocking" collective operations as defined in the MPI standard (all
operations, both languages).
- Non-blocking collective operations introduced in the MPI standard version 3.0-
(all operations, both languages). The completion of the operation is taken care
of by the library - i.e. still no changes to the call arguments.
-Implementations employing the PGAS languages – i.e. coarrays feature of the
Fortran 2008 standard (all non-vector collectives, Fortran only) and the Unified
Parallel C extension of the C language [3] (not available yet).
- Implementations using one-sided operations as defined in the MPI standard
(only few routines available, both languages).

For a detailed description of the library see the CRESTA document:
http://cresta-project.eu/images/docs/deliverables/d4.5.3_non-blocking_collectives_runtime_library.pdf

CRESTA Consortium Partners 2011
