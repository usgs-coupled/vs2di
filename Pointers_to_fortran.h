// adapted from pointers_to_fortran.h file of PHAST program
#if !defined(POINTERS_TO_FORTRAN_H_INCLUDED)
#define POINTERS_TO_FORTRAN_H_INCLUDED

// Pointers to Fortran arrays

// 7xnxz initial condition arrays
EXTERNAL int *initial_conditions1_c, *initial_conditions2_c;
EXTERNAL double *mxfrac_c;

// Grid info nxz
EXTERNAL double *x_node_c, *z_node_c;
EXTERNAL int nxz_c;

#endif // POINTERS_TO_FORTRAN_H_INCLUDED
