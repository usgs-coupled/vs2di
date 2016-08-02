/*! @file FileWriter.h
	@brief C/Fortran Documentation
*/
#ifndef FileWriter_H
#define FileWriter_H

#if defined(_WINDLL)
#define IPQ_DLL_EXPORT __declspec(dllexport)
#else
#define IPQ_DLL_EXPORT
#endif

#if defined(_MSC_VER) && !defined(CMAKE_FC)
#define FC_FUNC_(name,NAME) NAME
#endif

#if defined(CMAKE_FC)
#include "FC.h"
#endif

#if defined(__cplusplus)
extern "C" {
#endif
IPQ_DLL_EXPORT void FH_FinalizeFiles();
IPQ_DLL_EXPORT void FH_SetPointers(double *x_node,double *z_node, int * x_index, int * z_index, int *ic, double *saturation = NULL, int *mapping = NULL);
IPQ_DLL_EXPORT void FH_WriteFiles(int *id, int *print_xz, int *print_obs, int *xz_mask, int *obs_mask);
#if defined(__cplusplus)
}
#endif

#endif // FileWriter_H
