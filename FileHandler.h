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

#if defined(FC_FUNC_)
// Called from Fortran or C++
#define FH_FinalizeFiles                FC_FUNC_ (fh_finalizefiles,                 FH_FINALIZEFILES)
#define FH_ProcessRestartFiles          FC_FUNC_ (fh_processrestartfiles,           FH_PROCESSRESTARTFILES)
#define FH_SetPointers                  FC_FUNC_ (fh_setpointers,                   FH_SETPOINTERS)
#define FH_SetRestartName               FC_FUNC_ (fh_setrestartname,                FH_SETRESTARTNAME)
#define FH_WriteFiles                   FC_FUNC_ (fh_writefiles,                    FH_WRITEFILES)
#define FH_WriteBcRaw                   FC_FUNC_ (fh_writebcraw,                    FH_WRITEBCRAW)
#endif

#if defined(CMAKE_FC)
#include "FC.h"
#endif

#if defined(__cplusplus)
extern "C" {
#endif
IPQ_DLL_EXPORT void FH_FinalizeFiles();
//void FH_ProcessRestartFiles(int *id, int *initial_conditions1_in, int *initial_conditions2_in, 
//	double *fraction1_in);
IPQ_DLL_EXPORT void FH_SetPointers(double *x_node, double *y_node, double *z_node, int *ic, double *saturation = NULL, int *mapping = NULL);
IPQ_DLL_EXPORT void FH_SetRestartName(const char *name, long nchar);
IPQ_DLL_EXPORT void FH_WriteFiles(int *id, int *print_hdf, int *print_media, int *print_xyz, int *xyz_mask, int *print_restart);
IPQ_DLL_EXPORT void FH_WriteBcRaw(int *id, double *c, int *solution_list, int * bc_solution_count, int * solution_number, char *prefix, int prefix_l);
#if defined(__cplusplus)
}
#endif

#endif // FileWriter_H
