/*  adapted from phastproto.h file of PHAST program */
//#include <iostream>				// std::cout std::cerr
/*
 * vsrt.cxx
 */


int int_compare(const void *ptr1, const void *ptr2);

/*
 *  vsrtsubs.c
 */
#include <iostream>
#define COMP_FIRST_FORTRAN_DIMENSION
void add_all_components(void);
void buffer_print(const char *ptr, int n);
void buffer_to_vsrt(double *first);
void buffer_to_vsrt(double *first,int dim);
void moles_to_vsrt(double *first,int dim);
void moles_to_vsrt(double *first);
void buffer_to_mass_fraction(void);
void buffer_to_moles(void);
void buffer_to_solution(struct solution *solution_ptr);
void buffer_scale_moles(double f);
void vsrt_to_buffer(double *first);
void vsrt_to_buffer(double *first, int dim);
void vsrt_to_tempbuffer(double *first);
void vsrt_to_waterbuffer(double *first);
void vsrt_moles_to_buffer(double *first);
void vsrt_moles_to_buffer(double *first, int dim);
void set_use_vsrt(int i);
int xexchange_save_vsrt(int n);
int xgas_save_vsrt(int n);
int xpp_assemblage_save_vsrt(int n);
int xsolution_save_vsrt(int n);
int xsurface_save_vsrt(int n);
int xs_s_assemblage_save_vsrt(int n);
int calc_dummy_kinetic_reaction(struct kinetics *kinetics_ptr);
int print_using_vsrt(int cell_number);
int file_exists(const char *name);
int file_rename(const char *temp_name, const char *name,
				const char *backup_name);

/*
 * cxxVsrtSubs.cxx
 */
#include "Solution.h"
void buffer_to_cxxsolutionwheat(int n,int *heat);
void buffer_to_cxxsolution(int n);
void cxxsolution_to_buffer(cxxSolution * solution_ptr);
void unpackcxx_from_vsrtwheat(double *fraction,int *dim,int *heat);
void unpackcxx_from_vsrt(double *fraction,int *dim);
//void unpackcxx_from_hst_confined(double *fraction, int *dim, double *pv0,
//								 double *pv);
void system_cxxInitialize(int i, int n_user_new, int *initial_conditions1,
					 int *initial_conditions2, double *fraction1);
int write_restart(double hst_time);
int scale_cxxsystem(int iphrq,LDBLE frac);
int partition_uz(int iphrq, int ihst, LDBLE *new_vMoistureContent);
