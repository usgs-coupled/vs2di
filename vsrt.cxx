/* adapted from hst.cxx file of PHAST program */
#define EXTERNAL extern
#define MAIN
#include <fstream>
#include <iostream>				// std::cout std::cerr
#include "StorageBin.h"
#include "cxxMix.h"
#include "phreeqc/global.h"
#include "phreeqc/output.h"
#include "vsrt.h"
#include "phreeqc/phqalloc.h"
#include "phreeqc/phrqproto.h"
#include "phreeqc/input.h"
#include "vsrt_files.h"
#include "vsrtproto.h"
#include "Dictionary.h"
#include "gzstream.h"
#include "Pointers_to_fortran.h"
#include <vector>
#include "KDtree/Point.h"
#include "KDtree/KDtree.h"
#include "Phreeqc_class.h"
extern int xsurface_save(int n_user);
static int n_to_ij(int n_cell, int *i, int *j);
static char const svnid[] = "$Id: vsrt.cxx,v 1.1 2011 modified by Sosina  $";
static void EQUILIBRATE_SERIALWHEAT(double *fraction,double *hxshc, double *ttemp,int *dim, int *heat, int *print_sel,
							   double *x_vsrt, double *z_vsrt,
							   double *time_vsrt, double *time_step_vsrt,
							   int *prslm, double *cnvtmi,int *printzone_chem,
							   int *printzone_xyz,int *print_out,double *theta,
							   int *print_restart);
static void EQUILIBRATE_SERIAL(double *fraction,double *hxshc, int *dim, int *print_sel,
							   double *x_vsrt, double *z_vsrt,
							   double *time_vsrt, double *time_step_vsrt,
							   int *prslm, double *cnvtmi,int *printzone_chem,
							   int *printzone_xyz,int *print_out,double *theta,
							   int *print_restart);                               
/*..............................................................*/
#if defined(FC_FUNC_)

#define CALCULATE_WELL_PH                FC_FUNC_ (calculate_well_ph,             CALCULATE_WELL_PH)
#define CALCULATE_WELL_PHWHEAT           FC_FUNC_ (calculate_well_phwheat        CALCULATE_WELL_PHWHEAT)
#define COLLECT_FROM_NONROOT             FC_FUNC_ (collect_from_nonroot,          COLLECT_FROM_NONROOT)
#define COUNT_ALL_COMPONENTS             FC_FUNC_ (count_all_components,          COUNT_ALL_COMPONENTS)
#define CONVERT_TO_MOLAL                 FC_FUNC_ (convert_to_molal,              CONVERT_TO_MOLAL)
#define CONVERT_TO_MASS_FRACTION         FC_FUNC_ (convert_to_mass_fraction,      CONVERT_TO_MASS_FRACTION)
#define DISTRIBUTE_INITIAL_CONDITIONS    FC_FUNC_ (distribute_initial_conditions, DISTRIBUTE_INITIAL_CONDITIONS)
#define EQUILIBRATEWHEAT                 FC_FUNC_ (equilibratewheat,              EQUILIBRATEWHEAT)
#define EQUILIBRATE                      FC_FUNC_ (equilibrate,                   EQUILIBRATE)
#define ERRPRT_C                         FC_FUNC_ (errprt_c,                      ERRPRT_C)
#define FORWARD_AND_BACK                 FC_FUNC_ (forward_and_back,              FORWARD_AND_BACK)
#define LOGPRT_C                         FC_FUNC_ (logprt_c,                      LOGPRT_C)
#define ON_ERROR_CLEANUP_AND_EXIT        FC_FUNC_ (on_error_cleanup_and_exit,     ON_ERROR_CLEANUP_AND_EXIT)
#define PACK_FOR_VSRT                    FC_FUNC_ (pack_for_vsrt,                 PACK_FOR_VSRT)
#define PHREEQC_FREE                     FC_FUNC_ (phreeqc_free,                  PHREEQC_FREE)
#define PHREEQC_MAIN                     FC_FUNC_ (phreeqc_main,                  PHREEQC_MAIN)
#define SCREENPRT_C                      FC_FUNC_ (screenprt_c,                   SCREENPRT_C)
#define SEND_RESTART_NAME                FC_FUNC_ (send_restart_name,             SEND_RESTART_NAME)
#define STORE_C_POINTERS                 FC_FUNC_ (store_c_pointers,              STORE_C_POINTERS)
#define SETUP_BOUNDARY_CONDITIONS        FC_FUNC_ (setup_boundary_conditions,     SETUP_BOUNDARY_CONDITIONS)
#define WARNPRT_C                        FC_FUNC_ (warnprt_c,                     WARNPRT_C)
#define UZ_INIT                          FC_FUNC_ (uz_init,                       UZ_INIT)
#define COLLECT_COMP                     FC_FUNC_ (collect_comp,                  COLLECT_COMP)            

#else /* defined(FC_FUNC_) */

#if defined(NO_UNDERSCORES)
#define CALCULATE_WELL_PH calculate_well_ph
#define CALCULATE_WELL_PHWHEAT calculate_well_phwheat
#define COLLECT_FROM_NONROOT collect_from_nonroot
#define COUNT_ALL_COMPONENTS count_all_components
#define CONVERT_TO_MOLAL convert_to_molal
#define CONVERT_TO_MASS_FRACTION convert_to_mass_fraction
#define DISTRIBUTE_INITIAL_CONDITIONS distribute_initial_conditions
#define EQUILIBRATEWHEAT equilibratewheat
#define EQUILIBRATE equilibrate
#define ERRPRT_C errprt_c
#define FORWARD_AND_BACK forward_and_back
#define LOGPRT_C logprt_c
#define ON_ERROR_CLEANUP_AND_EXIT on_error_cleanup_and_exit
#define PACK_FOR_HST pack_for_vsrt
#define PHREEQC_FREE phreeqc_free
#define PHREEQC_MAIN phreeqc_main
#define SCREENPRT_C screenprt_c
#define SEND_RESTART_NAME send_restart_name
#define STORE_C_POINTERS store_c_pointers
#define SETUP_BOUNDARY_CONDITIONS setup_boundary_conditions
#define WARNPRT_C warnprt_c
#define UZ_INIT uz_init
#define COLLECT_COMP collect_comp 
#define STORE_C_POINTERS store_c_pointers
#elif defined(_MSC_VER)
#define CALCULATE_WELL_PH CALCULATE_WELL_PH
#define CALCULATE_WELL_PHWHEAT CALCULATE_WELL_PHWHEAT
#define COLLECT_FROM_NONROOT COLLECT_FROM_NONROOT
#define COUNT_ALL_COMPONENTS COUNT_ALL_COMPONENTS
#define CONVERT_TO_MOLAL CONVERT_TO_MOLAL
#define CONVERT_TO_MASS_FRACTION CONVERT_TO_MASS_FRACTION
#define DISTRIBUTE_INITIAL_CONDITIONS DISTRIBUTE_INITIAL_CONDITIONS
#define EQUILIBRATEWHEAT EQUILIBRATEWHEAT
#define EQUILIBRATE EQUILIBRATE
#define ERRPRT_C ERRPRT_C
#define FORWARD_AND_BACK FORWARD_AND_BACK
#define LOGPRT_C LOGPRT_C
#define ON_ERROR_CLEANUP_AND_EXIT ON_ERROR_CLEANUP_AND_EXIT
#define PACK_FOR_HST PACK_FOR_VSRT
#define PHREEQC_FREE PHREEQC_FREE
#define PHREEQC_MAIN PHREEQC_MAIN
#define SCREENPRT_C SCREENPRT_C
#define SEND_RESTART_NAME SEND_RESTART_NAME
#define STORE_C_POINTERS STORE_C_POINTERS
#define SETUP_BOUNDARY_CONDITIONS SETUP_BOUNDARY_CONDITIONS
#define WARNPRT_C WARNPRT_C
#define UZ_INIT UZ_INIT
#define COLLECT_COMP COLLECT_COMP
#define STORE_C_POINTERS STORE_C_POINTERS
#else 
#define CALCULATE_WELL_PH calculate_well_ph_
#define CALCULATE_WELL_PHWHEAT calculate_well_phwheat_
#define COLLECT_FROM_NONROOT collect_from_nonroot_
#define COUNT_ALL_COMPONENTS count_all_components_
#define CONVERT_TO_MOLAL convert_to_molal_
#define CONVERT_TO_MASS_FRACTION convert_to_mass_fraction_
#define DISTRIBUTE_INITIAL_CONDITIONS distribute_initial_conditions_
#define EQUILIBRATEWHEAT equilibratewheat_
#define ERRPRT_C errprt_c_
#define FORWARD_AND_BACK forward_and_back_
#define LOGPRT_C logprt_c_
#define ON_ERROR_CLEANUP_AND_EXIT on_error_cleanup_and_exit_
#define PACK_FOR_HST pack_for_vsrt_
#define PHREEQC_FREE phreeqc_free_
#define SCREENPRT_C screenprt_c_
#define SEND_RESTART_NAME send_restart_name_
#define STORE_C_POINTERS store_c_pointers_
#define SETUP_BOUNDARY_CONDITIONS setup_boundary_conditions_
#define WARNPRT_C warnprt_c_
#define UZ_INIT uz_init_
#define STORE_C_POINTERS store_c_pointers_
#endif

#endif /* defined(FC_FUNC_) */
/*......................................................*/

cxxStorageBin uzBin;
cxxStorageBin szBin;
cxxStorageBin phreeqcBin;
std::map < std::string, int >
	FileMap;
cxxDictionary
	dictionary;

extern
	"C"
{
    void
	CALCULATE_WELL_PH(double *c, LDBLE * ph, LDBLE * alkalinity);
    void
	CALCULATE_WELLPHWHEAT(double *c, LDBLE * ph, LDBLE * alkalinity,int *heat);
	void
	COLLECT_FROM_NONROOT(double *fraction, int *dim);
	void
	CONVERT_TO_MASS_FRACTION(double *c, int *n, int *dim);
	void
	CONVERT_TO_MOLAL(double *c, int *n, int *dim);
	void
	COUNT_ALL_COMPONENTS(int *n_comp, char *names, int length);
	void
	DISTRIBUTE_INITIAL_CONDITIONS(int *initial_conditions1, 
							  int *initial_conditions2, double *fraction1);
	void 
	EQUILIBRATEWHEAT(double *fraction,double *hxshc, double *ttemp, int *dim, int *heat, int *print_sel,double *x_vsrt, 
				double *z_vsrt, double *time_vsrt, double *time_step_vsrt, int *prslm,
				double *cnvtmi, int *printzone_chem,int *printzone_xyz,int *print_out,
				int *stop_msg,double *theta,int *print_restart );
    void 
	EQUILIBRATE(double *fraction,double *hxshc, int *dim, int *print_sel,double *x_vsrt, 
				double *z_vsrt, double *time_vsrt, double *time_step_vsrt, int *prslm,
				double *cnvtmi, int *printzone_chem,int *printzone_xyz,int *print_out,
				int *stop_msg,double *theta,int *print_restart );            
	void
	ERRPRT_C(char *err_str, long l);
	void
	FORWARD_AND_BACK(int *initial_conditions,int *axes,int *nx,
					 int *nz);
	void
	LOGPRT_C(char *err_str, long l);
	void
	ON_ERROR_CLEANUP_AND_EXIT(void);
	void
	PACK_FOR_VSRT(double *fraction,int *dim);
	void
	PHREEQC_FREE(int *solute);
	void
	PHREEQC_MAIN(int *solute, char *chemistry_name, char *database_name,char *prefix,
				   int chemistry_l, int database_l, int prefix_l);
	void
	SCREENPRT_C(char *err_str, long l);
	void
	SEND_RESTART_NAME(char *name, int nchar);
	void
	SETUP_BOUNDARY_CONDITIONS( int *boundary_solution1,int *boundary_solution2,
				   double *fraction1, double *boundary_fraction);
	void
	STORE_C_POINTERS(int *indx_sol1_ic, double *x_node, double *z_node);
	void
	WARNPRT_C(char *err_str, long l);
	void 
	COLLECT_COMP(double *concs);
	int
	UZ_INIT(void);
	
}
/* ---------------------------------------------------------------------- */
void
PHREEQC_FREE(int *solute)
/* ---------------------------------------------------------------------- */
/*
 *   free space
 */
{
	//int i;
	if (svnid == NULL)
		fprintf(stderr, " ");
	free_check_null(file_prefix);
	if (*solute)
	{
		free_model_allocs();
		free_check_null(buffer);
		free_check_null(activity_list);
		free_check_null(forward1);
		free_check_null(back);
		//free_check_null(file_prefix);
		free_check_null(old_vMoistureContent);
		clean_up();
	}
	else
	close_output_files();
	return;
}

/* ---------------------------------------------------------------------- */
void
PHREEQC_MAIN(int *solute, char *chemistry_name, char *database_name,
			 char *prefix, int chemistry_l, int database_l, int prefix_l)
/* ---------------------------------------------------------------------- */
/*
 *   Main program for PHREEQC
 */
{
	int
		errors;
	void *
		db_cookie = NULL;
	void *
		input_cookie = NULL;
	/*
	 * Set the debug-heap flag to keep freed blocks in the
	 * heap's linked list - This will allow us to catch any
	 * inadvertent use of freed memory
	 */

//#if defined(WIN32_MEMORY_DEBUG)
#if defined(_DEBUG)
	int
		tmpDbgFlag;
	tmpDbgFlag = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
	tmpDbgFlag |= _CRTDBG_LEAK_CHECK_DF;
    //  tmpDbgFlag |= _CRTDBG_DELAY_FREE_MEM_DF;
	//  tmpDbgFlag |= _CRTDBG_CHECK_ALWAYS_DF;
	//  tmpDbgFlag |= _CRTDBG_ALLOC_MEM_DF;
	_CrtSetDbgFlag(tmpDbgFlag);
	//_crtBreakAlloc = 26298;
	setbuf(stderr, NULL);

#ifdef SKIP
	// Send all reports to STDOUT, since this example is a console app
	_CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
	_CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDOUT);
	_CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE);
	_CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDOUT);
	_CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
	_CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDOUT);
#endif

#endif
	vs2drt = TRUE;                              
	chemistry_name[chemistry_l - 1] = '\0';
	prefix[prefix_l - 1] = '\0';
	database_name[database_l - 1] = '\0';
	file_prefix = string_duplicate(prefix);
	input_file_name[0] = '\0';
	output_file_name[0] = '\0';
	database_file_name[0] = '\0';
	/*
	 *   Add callbacks for echo_file
	 */
	if (add_output_callback(vsrt_handler, NULL) != OK)
	{
		fprintf(stderr, "ERROR: %s\n",
				"NULL pointer returned from malloc or realloc.");
		fprintf(stderr, "ERROR: %s\n", "Program terminating.");
		clean_up();
		exit(1);
		/* return(-1); */
	}

	/* Open error (screen) file */
	if (output_open(OUTPUT_ERROR, "Dummy") != OK)
		exit(4);

	/*
	 *   Set jump for errors
	 */
	errors = setjmp(mark);
	if (errors != 0)
	{
		clean_up();
		exit(1);
		/* return errors; */
	}

	open_output_file(prefix, *solute);

	if (errors != 0)
	{
		clean_up();
		exit(1);
		/*return errors; */
	}
		output_msg(OUTPUT_ECHO, "Running VS2DRT.\n\n");

	/*
	 *  Return if flow only simulation
	 */
	if (*solute == FALSE)
	{
		return /*(0) */ ;
	}
	/*
	 *   Open input files
	 */
	errors =
		open_input_files_vsrt(chemistry_name, database_name, &db_cookie,
							   &input_cookie);
	if (errors != 0)
	{
		clean_up();
		exit(1);
		/*return errors; */
	}
		output_msg(OUTPUT_ECHO, "Output file:    %s\n", output_file_name);
		output_msg(OUTPUT_ECHO, "Chemistry file: %s\n", input_file_name);
		output_msg(OUTPUT_ECHO, "Database file:  %s\n\n", database_file_name);
		output_msg(OUTPUT_MESSAGE, "Output file:    %s\n", output_file_name);
		output_msg(OUTPUT_MESSAGE, "Chemistry file: %s\n", input_file_name);
		output_msg(OUTPUT_MESSAGE, "Database file:  %s\n\n",
				   database_file_name);

	/*
	 *   Initialize arrays
	 */
	errors = do_initialize();
	
	if (errors != 0)
	{
		clean_up();
		exit(1);
		/*return errors; */
	}
	
		output_msg(OUTPUT_ECHO,
				   "Running PHREEQC for initial conditions.\n\n");

	/*
	 *   Read data base
	 */

		output_msg(OUTPUT_ECHO, "Processing database file.\n");

	errors = read_database(getc_callback, db_cookie);
	if (errors != 0)
	{
		clean_up();
		exit(1);
		/* return errors; */
	}

		output_msg(OUTPUT_LOG, "\nSuccessfully processed database file.\n");

	/*
	 *   Read input data for simulation
	 */

		output_msg(OUTPUT_ECHO, "\nProcessing chemical data file.\n");

	errors = run_simulations(getc_callback, input_cookie);
	if (errors != 0)
	{
		clean_up();
		exit(1);
		/*return errors; */
	}

		output_msg(OUTPUT_LOG,
				   "\nSuccessfully processed chemistry data file.\n");

	/*
	 *   Close input files and selected_output file
	 */
	close_input_files();
	output_close(OUTPUT_PUNCH);

	open_punch_file(prefix, *solute);

		output_msg(OUTPUT_ECHO, "PHREEQC done.\n");

	return;
}

/* ---------------------------------------------------------------------- */
void
COUNT_ALL_COMPONENTS(int *n_comp, char *names, int length)
/* ---------------------------------------------------------------------- */
{
/*
 *   Counts components in any defined solution, gas_phase, exchanger,
 *   surface, or pure_phase_assemblage
 *
 *   Returns n_comp, which is total, including H, O, elements, and Charge
 *           names contains character strings with names of components
 */
	int
		i,
		j,
		k;
/*
 *   Accumulate all aqueous components
 */
	add_all_components();
	
/*
 *   Count components, 2 for hydrogen, oxygen,  + others,
 */
	count_component = 2;
    
	for (i = 0; i < count_master; i++)
	{
		if (master[i]->total > 0.0 && master[i]->s->type == AQ)
		{
			count_component++;
		}
	}
	
	if (transport_charge == TRUE)
	{
		count_total = count_component++;
	}
	else
	{
		count_total = count_component;
	}
/*
 *   Put information in buffer.
 *   Buffer contains an entry for every primary master
 *   species that can be used in the transport problem.
 *   Each entry in buffer is sent to HST for transort.
 */
	buffer =
		(struct buffer *) PHRQ_malloc((size_t) count_component *
									  sizeof(struct buffer));
	if (buffer == NULL)
		malloc_error();
	buffer_dbg = buffer;
	buffer[0].name = string_hsave("H");
	if (s_h2 != NULL)
	{
		buffer[0].gfw = s_h2->secondary->elt->primary->elt->gfw;
	}
	else
	{
		buffer[0].gfw = 1.;
	}
	buffer[0].master = s_eminus->primary;
	buffer[1].name = string_hsave("O");
	if (s_o2 != NULL)
	{
		buffer[1].gfw = s_o2->secondary->elt->primary->elt->gfw;
	}
	else
	{
		buffer[1].gfw = 16.;
	}
	buffer[1].master = s_h2o->primary;
	j = 2;
	for (i = 0; i < count_master; i++)
	{
		if (master[i]->total > 0.0 && master[i]->s->type == AQ)
		{
			buffer[j].name = master[i]->elt->name;
			buffer[j].master = master[i];
			buffer[j].gfw = master[i]->elt->gfw;
			buffer[j].first_master = -1;
			buffer[j].last_master = -1;
			j++;
		}
	}
/* Bogus component used if surface reactions are included */
	if (transport_charge == TRUE)
	{
		buffer[j].name = string_hsave("Charge");
		if (s_h2 != NULL)
		{
			buffer[j].gfw = s_h2->secondary->elt->primary->elt->gfw;
		}
		else
		{
			buffer[j].gfw = 1.0;
		}
		buffer[j].master = s_eminus->primary;
	}
	output_msg(OUTPUT_MESSAGE, "List of Components:\n");
	for (i = 0; i < count_component; i++)
	{
		output_msg(OUTPUT_MESSAGE, "\t%d\t%s\n", i + 1, buffer[i].name);
		for (j = 0; buffer[i].name[j] != '\0'; j++)
		{
			names[i * length + j] = buffer[i].name[j];
		}
	}
/*
 *   Make list of all master species, one for each non redox element
 *   one for each secondary master of redox elements
 */
	count_activity_list = 0;
	for (i = 0; i < count_master; i++)
	{
		if (master[i]->total > 0.0 && master[i]->s->type == AQ)
		{
			if ((i + 1 < count_master) && (master[i + 1]->primary == FALSE))
			{
				for (k = i + 1; k < count_master; k++)
				{
					if (master[k]->primary == FALSE)
					{
						count_activity_list++;
					}
					else
					{
						break;
					}
				}
			}
			else
			{
				count_activity_list++;
			}
		}
	}
/*
 *   malloc space
 */
	activity_list =
		(struct activity_list *) PHRQ_malloc((size_t) count_activity_list *
											 sizeof(struct activity_list));
	if (activity_list == NULL)
		malloc_error();
	activity_list_dbg = activity_list;

	count_activity_list = 0;
	for (i = 0; i < count_master; i++)
	{
		if (master[i]->total > 0.0 && master[i]->s->type == AQ)
		{
			if ((i + 1 < count_master) && (master[i + 1]->primary == FALSE))
			{
				for (k = i + 1; k < count_master; k++)
				{
					if (master[k]->primary == FALSE)
					{
						activity_list[count_activity_list].master = master[k];
						activity_list[count_activity_list].name =
							master[k]->elt->name;
						count_activity_list++;
					}
					else
					{
						break;
					}
				}
			}
			else
			{
				activity_list[count_activity_list].master = master[i];
				activity_list[count_activity_list].name =
					master[i]->elt->name;
				count_activity_list++;
			}
		}
	}
	output_msg(OUTPUT_MESSAGE, "List of master species:\n");
	for (i = 0; i < count_activity_list; i++)
	{
		output_msg(OUTPUT_MESSAGE, "\t%d\t%s\n", i + 1,
				   activity_list[i].name);
	}
/*
 *   Associate buffer master species with activity_list master species
 */
	j = 0;
	for (i = 0; i < count_activity_list; i++)
	{
		while (activity_list[i].master->elt->primary != buffer[j].master)
			j++;
		if (buffer[j].first_master < 0)
			buffer[j].first_master = i;
		if (i > buffer[j].last_master)
			buffer[j].last_master = i;
	}
/*
 *   Realloc space for totals and activities for all solutions to make
 *   enough room during hst simulation, put array in standard form
 */

	for (i = 0; i < count_solution; i++)
	{
		xsolution_zero();
		add_solution(solution[i], 1.0 / solution[i]->mass_water, 1.0);
		solution[i]->totals =
			(struct conc *) PHRQ_realloc(solution[i]->totals,
										 (size_t) (count_total -
												   1) * sizeof(struct conc));
		if (solution[i]->totals == NULL)
			malloc_error();
		solution[i]->master_activity =
			(struct master_activity *) PHRQ_realloc(solution[i]->
													master_activity,
													(size_t)
													(count_activity_list +
													 1) *
													sizeof(struct
														   master_activity));
		if (solution[i]->master_activity == NULL)
			malloc_error();
		solution[i]->count_master_activity = count_activity_list;
		/*solution[i]->species_gamma = PHRQ_realloc (solution[i]->species_gamma, (size_t) (count_activity_list + 1) * sizeof(struct master_activity));
		   if (solution[i]->species_gamma == NULL) malloc_error(); */
		solution[i]->species_gamma = NULL;
		solution[i]->count_species_gamma = 0;

		for (j = 2; j < count_total; j++)
		{
			buffer[j].master->total_primary = buffer[j].master->total;
		}
	xsolution_save_vsrt(i);
	}
/*
 *   Make sure solution -1 is defined
 */
	if (count_solution > 0)
	{
		solution_duplicate(solution[0]->n_user, -1);
	}
	else
	{
		error_msg("No solutions have been defined.", STOP);
	}
	if (count_exchange > 0)
	{
		exchange_duplicate(exchange[0].n_user, -1);
	}
	if (count_gas_phase > 0)
	{
		gas_phase_duplicate(gas_phase[0].n_user, -1);
	}
	if (count_pp_assemblage > 0)
	{
		pp_assemblage_duplicate(pp_assemblage[0].n_user, -1);
	}
	if (count_surface > 0)
	{
		surface_duplicate(surface[0].n_user, -1);
	}
	if (count_s_s_assemblage > 0)
	{
		s_s_assemblage_duplicate(s_s_assemblage[0].n_user, -1);
	}
	if (count_kinetics > 0)
	{
		kinetics_duplicate(kinetics[0].n_user, -1);
	}

/*
 *   Set pe data structrure for all calculations
 */
	pe_data_free(pe_x);
	pe_x = pe_data_alloc();
/*
 *   Beginning of stored data for HST
 */
	first_solution = count_solution;
	first_gas_phase = count_gas_phase;
	first_exchange = count_exchange;
	first_pp_assemblage = count_pp_assemblage;
	first_surface = count_surface;
	first_s_s_assemblage = count_s_s_assemblage;
	first_kinetics = count_kinetics;

	*n_comp = count_component;
	/* mass_water_switch = TRUE; */
	delay_mass_water = FALSE;
	last_model.force_prep = TRUE;
	simulation = -1;
	/*
	 *  set up C++ storage
	 */

	phreeqcBin.import_phreeqc();
	dictionary.add_phreeqc();
	/*
	   fprintf(stderr, "Size of dictionary %d\n", dictionary.size());
	   for (i = 0; i < 10; i++) {
	   std::cerr << *dictionary.int2string(i) << std::endl;
	   }
	 */
	//std::ostringstream oss;
	//phreeqcBin.dump_raw(oss,0);
	//std::cerr << oss.str();
	return;
}



/* ---------------------------------------------------------------------- */
void
DISTRIBUTE_INITIAL_CONDITIONS(int *initial_conditions1,
							  int *initial_conditions2, double *fraction1)
/* ---------------------------------------------------------------------- */
{
	/*
	 *      ixyz - number of cells
	 *      initial_conditions1 - Fortran, 7 x n_cell integer array, containing
	 *           solution number
	 *           pure_phases number
	 *           exchange number
	 *           surface number
	 *           gas number
	 *           solid solution number
	 *           kinetics number
	 *      initial_conditions2 - Fortran, 8 x n_cell integer array, containing
	 *      fraction for 1 - Fortran, 8 x n_cell integer array, containing
	 *
	 *      Routine mixes solutions, pure_phase assemblages,
	 *      exchangers, surface complexers, gases, solid solution assemblages,
	 *      and kinetics for each cell.
	 */
	int
		i,
		j;
	//struct system *system_ptr;
	/*
	 *  Copy solution, exchange, surface, gas phase, kinetics, solid solution for each active cell.
	 *  Does nothing for indexes less than 0 (i.e. restart files)
	 */
	 cout<<"Intial distribution1 ="<<"\n";
	for (i = 0; i < ixz; i++)
	{							/* i is ixyz number */
		j = forward1[i];			/* j is count_chem number */
		if (j < 0)
			continue;
		assert(forward1[i] >= 0);	
		system_cxxInitialize(i, j, initial_conditions1, initial_conditions2,
			fraction1);
	}
	/*
	 * Read any restart files
	 */
	for (std::map < std::string, int >::iterator it = FileMap.begin();
		 it != FileMap.end(); it++)
	{
		int
			ifile = -100 - it->second;

		// use gsztream
		igzstream
			myfile;
		myfile.open(it->first.c_str());
		if (!myfile.good())

		{
			sprintf(error_string, "File could not be opened: %s.",
					it->first.c_str());
			input_error++;
			error_msg(error_string, CONTINUE);
			break;
		}
       cout<<"Intial distribution2 ="<<"\n";
		std::ostringstream oss;
		CParser
		cparser(myfile, oss, std::cerr);
		cparser.set_echo_file(CParser::EO_NONE);
		cparser.set_echo_stream(CParser::EO_NONE);

		// skip headers
		while (cparser.check_line("restart", false, true, true, false) ==
			   CParser::LT_EMPTY);

		// read number of lines of index
		int
			n = -1;
		if (!(cparser.get_iss() >> n) || n < 3)
		{
			sprintf(error_string,
					"File does not have node locations: %s.\nPerhaps it is an old format restart file.",
					it->first.c_str());
			input_error++;
			myfile.close();
			error_msg(error_string, CONTINUE);
			break;
		}

		// points are x, z, cell_no
		std::vector < Point > pts;
		// index:
		// 0 solution
		// 1 ppassemblage setup_boundary_conditions
		// 2 exchange
		// 3 surface
		// 4 gas phase
		// 5 ss_assemblage
		// 6 kinetics
		int * index = (int *) PHRQ_malloc((size_t) (n * 7 * sizeof(int)));

		for (i = 0; i < n; i++)
		{
			cparser.check_line("restart", false, false, false, false);
			double
				x,
				z,
				v;
			cparser.get_iss() >> x;
			cparser.get_iss() >> z;
			cparser.get_iss() >> v;
			pts.push_back(Point(x, z, v));

			cparser.get_iss() >> index[i * 7];
			cparser.get_iss() >> index[i * 7 + 1];
			cparser.get_iss() >> index[i * 7 + 2];
			cparser.get_iss() >> index[i * 7 + 3];
			cparser.get_iss() >> index[i * 7 + 4];
			cparser.get_iss() >> index[i * 7 + 5];
			cparser.get_iss() >> index[i * 7 + 6];
		}
		KDtree
		index_tree(pts);

		cxxStorageBin
			tempBin;
		tempBin.read_raw(cparser);

		for (j = 0; j < count_chem; j++)	/* j is count_chem number */
		{
			i = back[j].list;	/* i is ixyz number */
			Point p(x_node_c[i], z_node_c[i]);
			int k = (int) index_tree.Interpolate3d(p);	// k is index number in tempBin

			// solution
			if (initial_conditions1[i * 7] == ifile)
			{
				if (index[k * 7] != -1)	// entity k should be defined in tempBin
				{
					if (tempBin.getSolution(k) != NULL)
					{
						szBin.setSolution(j, tempBin.getSolution(k));
					}
					else
					{
						initial_conditions1[7 * i] = -1;
					}
				}
			}

			// PPassemblage
			if (initial_conditions1[i * 7 + 1] == ifile)
			{
				if (index[k * 7 + 1] != -1)	// entity k should be defined in tempBin
				{
					if (tempBin.getPPassemblage(k) != NULL)
					{
						szBin.setPPassemblage(j, tempBin.getPPassemblage(k));
					}
					else
					{
						initial_conditions1[7 * i + 1] = -1;
					}
				}
			}

			// Exchange
			if (initial_conditions1[i * 7 + 2] == ifile)
			{
				if (index[k * 7 + 2] != -1)	// entity k should be defined in tempBin
				{
					if (tempBin.getExchange(k) != NULL)
					{
						szBin.setExchange(j, tempBin.getExchange(k));
					}
					else
					{
						initial_conditions1[7 * i + 2] = -1;
					}
				}
			}

			// Surface
			if (initial_conditions1[i * 7 + 3] == ifile)
			{
				if (index[k * 7 + 3] != -1)	// entity k should be defined in tempBin
				{
					if (tempBin.getSurface(k) != NULL)
					{
						szBin.setSurface(j, tempBin.getSurface(k));
					}
					else
					{
						initial_conditions1[7 * i + 3] = -1;
					}
				}
			}

			// Gas phase
			if (initial_conditions1[i * 7 + 4] == ifile)
			{
				if (index[k * 7 + 4] != -1)	// entity k should be defined in tempBin
				{
					if (tempBin.getGasPhase(k) != NULL)
					{ 
						szBin.setGasPhase(j, tempBin.getGasPhase(k));
					}
					else
					{
						initial_conditions1[7 * i + 4] = -1;
					}
				}
			}

			// Solid solution
			if (initial_conditions1[i * 7 + 5] == ifile)
			{
				if (index[k * 7 + 5] != -1)	// entity k should be defined in tempBin
				{
					if (tempBin.getSSassemblage(k) != NULL)
					{
						szBin.setSSassemblage(j, tempBin.getSSassemblage(k));
					}
					else
					{
						initial_conditions1[7 * i + 5] = -1;
					}
				}
			}

			// Kinetics
			if (initial_conditions1[i * 7 + 6] == ifile)
			{
				if (index[k * 7 + 6] != -1)	// entity k should be defined in tempBin
				{
					if (tempBin.getKinetics(k) != NULL)
					{ 
						szBin.setKinetics(j, tempBin.getKinetics(k));
					}
					else
					{
						initial_conditions1[7 * i + 6] = -1;
					}
				}
			}
		}
		myfile.close();
		index = (int *) free_check_null(index);
	}
	if (input_error > 0)
	{
		error_msg("Terminating in distribute_initial_conditions.\n", STOP);
	}
/*	write_restart(0.0);
     cout<<"Intial distribution1 ="<<"\n";*/
}

/* ---------------------------------------------------------------------- */
void
SETUP_BOUNDARY_CONDITIONS( int *boundary_solution1,int *boundary_solution2, 
			   double *fraction1,double *boundary_fraction)
/* ---------------------------------------------------------------------- */
{
/*
 *   Routine takes a list of solution numbers and returns a set of
 *   mass fractions
 *   Input: n_boundary - number of boundary conditions in list
 *          boundary_solution1 - list of first solution numbers to be mixed
 *          boundary_solution2 - list of second solution numbers to be mixed
 *          fraction1 - fraction of first solution 0 <= f <= 1
 *          dim - leading dimension of array boundary mass fractions
 *                must be >= to n_boundary
 *
 *   Output: boundary_fraction - mass fractions for boundary conditions
 *                             - dimensions must be >= n_boundary x n_comp
 *
 */
	int
		i,
		n_old1,
		n_old2;
	double
		f1,
		f2;
		cxxMix
			mixmap;
		n_old1 = *boundary_solution1;
		n_old2 = *boundary_solution2;
		f1 = *fraction1;
		f2 = 1 - f1;
		mixmap.add(n_old1, f1);
		if (f2 > 0.0)
		{
			mixmap.add(n_old2, f2);
		}
		//cxxSolution *cxxsoln_ptr = phreeqcBin.mix_cxxSolutions(mixmap);
		cxxSolution
		cxxsoln(phreeqcBin.getSolutions(), mixmap, 0);
		cxxsolution_to_buffer(&cxxsoln);
		buffer_to_mass_fraction();
		buffer_to_vsrt(boundary_fraction);
	return;
}

/* ---------------------------------------------------------------------- */
void
PACK_FOR_VSRT(double *fraction,int *dim)
/* ---------------------------------------------------------------------- */
{
/*
 *   Routine takes solution data and makes array of mass fractions for HST
 *   Input: n_cell - number of cells in model
 *          dim - leading dimension of 2-d array fraction
 *
 *   Output: fraction - mass fractions of all components in all solutions
 *                      dimensions must be >= n_cell x n_comp
 */
       int i, j,k;
      
	cxxSolution *
		cxxsoln_ptr;

	for (i = 0; i < count_chem; i++)
	{
		cxxsoln_ptr = szBin.getSolution(i);
		cxxsolution_to_buffer(cxxsoln_ptr);
                buffer_to_mass_fraction();
      for (j = 0; j <  count_component; j++)
	{
		k=count_component*i +j;
		fraction[k] = buffer[j].moles;
	}     
                
     }
     for (i=0; i < count_chem; i++)
     {
     	for(j=0; j <  count_component; j++)
     	{
        	k=	k=count_component*i +j;
            cout<<"fraction["<<k<<"]="<<fraction[k]<<endl;
     	}
     }
	return;
}

/*--------------------------------------------------------------------------*/
static void 
EQUILIBRATE_SERIALWHEAT(double *fraction,double *hxshc,double *ttemp, int *dim, int *heat, int *print_sel,
							   double *x_vsrt, double *z_vsrt,
							   double *time_vsrt, double *time_step_vsrt,
							   int *prslm, double *cnvtmi,int *printzone_chem,
							   int *printzone_xyz,int *print_out,double *theta,int *print_restart)
/*--------------------------------------------------------------------------*/
{
  /*
 *   Routine takes mass fractions from HST, equilibrates each cell,
 *   and returns new mass fractions to HST
 *
 *   Input: ixz - number of cells in model
 *          dim - leading dimension of 2-d array fraction
 *          temp - temperature at each cell  
 *          theta- volumetric moisture content at each cell
 *
 *   Output: fraction - mass fractions of all components in all solutions
 *                      dimensions must be >= ixz x n_comp
 */
	int
		i,
		j,
		tot_same,
		tot_iter,
		tot_zero,
		max_iter;
	int
		active;
	int
		n_user;
	LDBLE
		kin_time;
	static int
		write_headings = 1;

	pr.all = *print_out;
/*
 *   Update solution compositions
 */
  //      vsrt_to_waterbuffer(theta);
        vsrt_to_tempbuffer(ttemp);
    unpackcxx_from_vsrtwheat(fraction,dim,heat);
/*
 *   Calculate equilibrium
 */
	kin_time = *time_step_vsrt;
	timest = kin_time;

	state = VS2DRT;
	tot_same = 0;
	tot_iter = 0;
	tot_zero = 0;
	max_iter = 0;
	if (*print_sel == TRUE)
	{
		simulation++;
		pr.punch = TRUE;
	}
	else
	{
		pr.punch = FALSE;
	}

	if (punch.in == TRUE && write_headings)
	{
		int
			pr_punch = pr.punch;
		pr.punch = TRUE;
		/*
		 * write headings
		 */
		punch.new_def = TRUE;
		output_msg(OUTPUT_PUNCH, "%15s\t%15s\t%15s\t%2s\t", "x", 
				   "z", "time", "in");
		tidy_punch();
		write_headings = 0;
		pr.punch = pr_punch;
	}

	rate_sim_time_start = *time_vsrt - *time_step_vsrt;
	rate_sim_time_end = *time_vsrt;
	initial_total_time = 0;
	// free all c structures
	reinitialize();
	for (i = 0; i < count_chem; i++)
	{							/* i is count_chem number */
		j = back[i].list;	/* j is nxyz number */
/*		partition_uz(i, j, &theta[j]);*/
		if (theta[j] <= 1e-10 )
			theta[j] = 0.0;
		// set flags
		active = FALSE;
		if (theta[j] > 0.0)
		{
			active = TRUE;
		}
		pr.all = FALSE;
		if (*print_out == TRUE && printzone_chem[j] == TRUE)
			pr.all = TRUE;
		pr.punch = FALSE;
		if (*print_sel == TRUE && printzone_xyz[j] == TRUE
			&& punch.in == TRUE)
			pr.punch = TRUE;
		if (pr.punch == TRUE)
		{
			output_msg(OUTPUT_PUNCH, "%15g\t%15g\t%15g\t%2d\t",
					   x_vsrt[j],  z_vsrt[j], (*time_vsrt) * (*cnvtmi),
					   active);
			if (active == FALSE)
			{
				output_msg(OUTPUT_PUNCH, "\n");
			}
		}
		if (active)
		{
			cell_no = i;
          
			szBin.cxxStorageBin2phreeqc(i);
			set_use_vsrt(i);
			n_user = i;
			set_initial_moles(n_user);
			run_reactions(n_user, kin_time, FALSE, 1.0);
			if (iterations == 0)
				tot_zero++;
			if (iterations > max_iter)
				max_iter = iterations;
			tot_same += same_model;
			tot_iter += iterations;
			sum_species();
			if (pr.all == TRUE)
			{
				output_msg(OUTPUT_MESSAGE,
						   "Time %g. Cell %d: x=%15g\ttz=%15g\n",
						   (*time_vsrt) * (*cnvtmi), j + 1, x_vsrt[j], 
						   z_vsrt[j]);
				print_using_vsrt(j + 1);
			}
			print_all();
			punch_all();
			/*
			 *   Save data
			 */
			solution_bsearch(i, &n_solution, TRUE);
			if (n_solution == -999)
			{
				error_msg("How did this happen?", STOP);
			}
			xsolution_save_vsrt(n_solution);
			if (save.exchange == TRUE)
			{
				exchange_bsearch(i, &n_exchange);
				xexchange_save_vsrt(n_exchange);
			}
			if (save.gas_phase == TRUE)
			{
				gas_phase_bsearch(i, &n_gas_phase);
				xgas_save_vsrt(n_gas_phase);
			}
			if (save.pp_assemblage == TRUE)
			{
				pp_assemblage_bsearch(i, &n_pp_assemblage);
				xpp_assemblage_save_vsrt(n_pp_assemblage);
			}
			if (save.surface == TRUE)
			{
				surface_bsearch(i, &n_surface);
                //xsurface_save_vsrt(n_surface);
                xsurface_save(i);
			}
			if (save.s_s_assemblage == TRUE)
			{
				s_s_assemblage_bsearch(i, &n_s_s_assemblage);
				xs_s_assemblage_save_vsrt(n_s_s_assemblage);
			}
				
			szBin.phreeqc2cxxStorageBin(i);
	

		}
		else
		{
			if (pr.all == TRUE)
			{
				output_msg(OUTPUT_MESSAGE,
						   "Time %g. Cell %d: x=%15g\tz=%15g\n",
						   (*time_vsrt) * (*cnvtmi), j + 1, x_vsrt[j], 
						   z_vsrt[j]);
				output_msg(OUTPUT_MESSAGE, "Cell is dry.\n");
			}
		}
		// free phreeqc structures
		reinitialize();
        }
		/*
	 *  Write restart data
	 */
	if (*print_restart == 1)
		write_restart((*time_vsrt) * (*cnvtmi));
/*
 *   Put values back for VSRT
 */
	PACK_FOR_VSRT(fraction,dim);
/*
 *   Write screen and log messages
 */
	if (*prslm == TRUE)
	{
		sprintf(error_string, "          Total cells: %d", count_chem);
		output_msg(OUTPUT_SCREEN, "%s\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n", error_string);
		sprintf(error_string,
				"          Number of cells with same aqueous model: %d",
				tot_same);
		output_msg(OUTPUT_SCREEN, "%s\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n", error_string);
		sprintf(error_string, "          Total iterations all cells: %d",
				tot_iter)

;
		output_msg(OUTPUT_SCREEN, "%s\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n", error_string);
		sprintf(error_string,
				"          Number of cells with zero iterations: %d",
				tot_zero);
		output_msg(OUTPUT_SCREEN, "%s\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n", error_string);
		sprintf(error_string, "          Maximum iterations for one cell: %d",
				max_iter);
		output_msg(OUTPUT_SCREEN, "%s\n\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n\n", error_string);
	}
	return;
}
/*------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
static void 
EQUILIBRATE_SERIAL(double *fraction,double *hxshc,int *dim, int *print_sel,
							   double *x_vsrt, double *z_vsrt,
							   double *time_vsrt, double *time_step_vsrt,
							   int *prslm, double *cnvtmi,int *printzone_chem,
							   int *printzone_xyz,int *print_out,double *theta,int *print_restart)
/*--------------------------------------------------------------------------*/
{
  /*
 *   Routine takes mass fractions from HST, equilibrates each cell,
 *   and returns new mass fractions to HST
 *
 *   Input: ixz - number of cells in model
 *          dim - leading dimension of 2-d array fraction
 *          temp - temperature at each cell  
 *          theta- volumetric moisture content at each cell
 *
 *   Output: fraction - mass fractions of all components in all solutions
 *                      dimensions must be >= ixz x n_comp
 */
	int
		i,
		j,
		tot_same,
		tot_iter,
		tot_zero,
		max_iter;
	int
		active;
	int
		n_user;
	LDBLE
		kin_time;
	static int
		write_headings = 1;

	pr.all = *print_out;
/*
 *   Update solution compositions
 */
  //     vsrt_to_waterbuffer(theta);

	unpackcxx_from_vsrt(fraction,dim);
/*
 *   Calculate equilibrium
 */
	kin_time = *time_step_vsrt;
	timest = kin_time;

	state = VS2DRT;
	tot_same = 0;
	tot_iter = 0;
	tot_zero = 0;
	max_iter = 0;
	if (*print_sel == TRUE)
	{
		simulation++;
		pr.punch = TRUE;
	}
	else
	{
		pr.punch = FALSE;
	}

	if (punch.in == TRUE && write_headings)
	{
		int
			pr_punch = pr.punch;
		pr.punch = TRUE;
		/*
		 * write headings
		 */
		punch.new_def = TRUE;
		output_msg(OUTPUT_PUNCH, "%15s\t%15s\t%15s\t%2s\t", "x", 
				   "z", "time", "in");
		tidy_punch();
		write_headings = 0;
		pr.punch = pr_punch;
	}

	rate_sim_time_start = *time_vsrt - *time_step_vsrt;
	rate_sim_time_end = *time_vsrt;
	initial_total_time = 0;
	// free all c structures
	reinitialize();
	for (i = 0; i < count_chem; i++)
	{							/* i is count_chem number */
		j = back[i].list;	/* j is nxyz number */
/*		partition_uz(i, j, &theta[j]);*/
		if (theta[j] <= 1e-10 )
			theta[j] = 0.0;
		// set flags
		active = FALSE;
		if (theta[j] > 0.0)
		{
			active = TRUE;
		}
		pr.all = FALSE;
		if (*print_out == TRUE && printzone_chem[j] == TRUE)
			pr.all = TRUE;
		pr.punch = FALSE;
		if (*print_sel == TRUE && printzone_xyz[j] == TRUE
			&& punch.in == TRUE)
			pr.punch = TRUE;
		if (pr.punch == TRUE)
		{
			output_msg(OUTPUT_PUNCH, "%15g\t%15g\t%15g\t%2d\t",
					   x_vsrt[j],  z_vsrt[j], (*time_vsrt) * (*cnvtmi),
					   active);
			if (active == FALSE)
			{
				output_msg(OUTPUT_PUNCH, "\n");
			}
		}
		if (active)
		{
			cell_no = i;

			szBin.cxxStorageBin2phreeqc(i);
			set_use_vsrt(i);
			n_user = i;
			set_initial_moles(n_user);
			run_reactions(n_user, kin_time, FALSE, 1.0);
			if (iterations == 0)
				tot_zero++;
			if (iterations > max_iter)
				max_iter = iterations;
			tot_same += same_model;
			tot_iter += iterations;
			sum_species();
			if (pr.all == TRUE)
			{
				output_msg(OUTPUT_MESSAGE,
						   "Time %g. Cell %d: x=%15g\ttz=%15g\n",
						   (*time_vsrt) * (*cnvtmi), j + 1, x_vsrt[j], 
						   z_vsrt[j]);
				print_using_vsrt(j + 1);
			}
			print_all();
			punch_all();
			/*
			 *   Save data
			 */
			solution_bsearch(i, &n_solution, TRUE);
			if (n_solution == -999)
			{
				error_msg("How did this happen?", STOP);
			}
			xsolution_save_vsrt(n_solution);
			if (save.exchange == TRUE)
			{
				exchange_bsearch(i, &n_exchange);
				xexchange_save_vsrt(n_exchange);
			}
			if (save.gas_phase == TRUE)
			{
				gas_phase_bsearch(i, &n_gas_phase);
				xgas_save_vsrt(n_gas_phase);
			}
			if (save.pp_assemblage == TRUE)
			{
				pp_assemblage_bsearch(i, &n_pp_assemblage);
				xpp_assemblage_save_vsrt(n_pp_assemblage);
			}
			if (save.surface == TRUE)
			{
				surface_bsearch(i, &n_surface);
                //xsurface_save_vsrt(n_surface);
                xsurface_save(i);
			}
			if (save.s_s_assemblage == TRUE)
			{
				s_s_assemblage_bsearch(i, &n_s_s_assemblage);
				xs_s_assemblage_save_vsrt(n_s_s_assemblage);
			}
			szBin.phreeqc2cxxStorageBin(i);
		

		}
		else
		{
			if (pr.all == TRUE)
			{
				output_msg(OUTPUT_MESSAGE,
						   "Time %g. Cell %d: x=%15g\tz=%15g\n",
						   (*time_vsrt) * (*cnvtmi), j + 1, x_vsrt[j], 
						   z_vsrt[j]);
				output_msg(OUTPUT_MESSAGE, "Cell is dry.\n");
			}
		}
		// free phreeqc structures
		reinitialize();
        }
		/*
	 *  Write restart data
	 */
	if (*print_restart == 1)
		write_restart((*time_vsrt) * (*cnvtmi));
/*
 *   Put values back for VSRT
 */
	PACK_FOR_VSRT(fraction,dim);
/*
 *   Write screen and log messages
 */
	if (*prslm == TRUE)
	{
		sprintf(error_string, "          Total cells: %d", count_chem);
		output_msg(OUTPUT_SCREEN, "%s\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n", error_string);
		sprintf(error_string,
				"          Number of cells with same aqueous model: %d",
				tot_same);
		output_msg(OUTPUT_SCREEN, "%s\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n", error_string);
		sprintf(error_string, "          Total iterations all cells: %d",
				tot_iter)

;
		output_msg(OUTPUT_SCREEN, "%s\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n", error_string);
		sprintf(error_string,
				"          Number of cells with zero iterations: %d",
				tot_zero);
		output_msg(OUTPUT_SCREEN, "%s\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n", error_string);
		sprintf(error_string, "          Maximum iterations for one cell: %d",
				max_iter);
		output_msg(OUTPUT_SCREEN, "%s\n\n", error_string);
		output_msg(OUTPUT_ECHO, "%s\n\n", error_string);
	}
	return;
}

/* ---------------------------------------------------------------------- */
 void
EQUILIBRATEWHEAT(double *fraction, double *hxshc,double *ttemp,int *dim,int *heat,int *print_sel, double *x_vsrt, double *z_vsrt,
				   double *time_vsrt, double *time_step_vsrt, int *prslm,
				   double *cnvtmi, int *printzone_chem,int *printzone_xyz,
				    int *print_out,int *stop_msg,double *theta, int *print_restart)
/* ---------------------------------------------------------------------- */
{
	if (!(*stop_msg == 1))
	{
	   EQUILIBRATE_SERIALWHEAT(fraction,hxshc,ttemp,dim,heat,print_sel,x_vsrt,z_vsrt,
					time_vsrt,time_step_vsrt,
					prslm,cnvtmi,printzone_chem,
					printzone_xyz,print_out,theta,
					print_restart);
	}
  return;
}
/* ---------------------------------------------------------------------- */
 void
EQUILIBRATE(double *fraction, double *hxshc,int *dim,int *print_sel, double *x_vsrt, double *z_vsrt,
				   double *time_vsrt, double *time_step_vsrt, int *prslm,
				   double *cnvtmi, int *printzone_chem,int *printzone_xyz,
				    int *print_out,int *stop_msg,double *theta, int *print_restart)
/* ---------------------------------------------------------------------- */
{
	if (!(*stop_msg == 1))
	{
	   EQUILIBRATE_SERIAL(fraction,hxshc,dim,print_sel,x_vsrt,z_vsrt,
					time_vsrt,time_step_vsrt,
					prslm,cnvtmi,printzone_chem,
					printzone_xyz,print_out,theta,
					print_restart);
	}
  return;
}

/* ---------------------------------------------------------------------- */

void
FORWARD_AND_BACK(int *initial_conditions,int *axes,int *nx, int *nz)
/* ---------------------------------------------------------------------- */
{
/*
 *   calculate mapping from full set of cells to subset needed for chemistry
 */
	int
		i,
		n,
		ii,
		jj;

	count_chem = 1;
	ix = *nx;
	iz = *nz;
	ixz = ix*iz;
	if (axes[0] == FALSE && axes[1] == FALSE )
	{
		error_msg("No active coordinate direction in DIMENSIONS keyword.",
				  STOP);
	}
	
		count_chem = ixz;
/*
 *   malloc space
 */
	forward1 = (int *) PHRQ_malloc((size_t) ixz * sizeof(int));
	if (forward1 == NULL)
		malloc_error();
	back =
		(back_list *) PHRQ_malloc((size_t) count_chem *sizeof(back_list)); 
	if (back == NULL)
		malloc_error();
		n = 0;
        if ((axes[0] == TRUE) && (axes[1] == TRUE))
	{
		n = 0;
		for (i = 0; i < ixz; i++)
		{
                  if(initial_conditions[7 * i] >= 0
					|| initial_conditions[7 * i] <= -100)
                        
			{
				forward1[i] = n;
				back[n].list = i;
				n++;
			}
			else
			{
				forward1[i] = -1;
			}
			cout<<"XZforward ["<<i<<"] ="<<forward1[i]<<"\n";	
		}
		count_chem = n;
/*
 *   Copy line x
 */
	}
else if ((axes[0] == TRUE) && (axes[1] == FALSE))
	{
		if (iz != 2)
		{
			sprintf(error_string,
					"z direction should contain only three nodes for this 1D problem.");
			error_msg(error_string, STOP);
		}
		
		n = 0;
		for (i = 0; i < ixz; i++)
		{
			if (initial_conditions[i * 7] < 0
				&& initial_conditions[7 * i] > -100)
			{
				input_error++;
				sprintf(error_string,
						"Can not have inactive cells in a 1D simulation.");
				error_msg(error_string, STOP);
			}
			n_to_ij(i, &ii, &jj);
			if (jj == 0)
			{
				forward1[i] = n;
				back[n].list = i;
				n++;
			}
			else
			{
				forward1[i] = -1;
			}
			cout<<"Xforward ["<<i<<"] ="<<forward1[i]<<"\n";	
		}
		count_chem = n;
/*
 *   Copy z line
 */}
	else if ((axes[0] == FALSE) && (axes[1] == TRUE))
	{
		n = 0;
		for (i = 0; i < ixz; i++)
		{
			if (initial_conditions[i * 7] < 0
				&& initial_conditions[7 * i] > -100)
			{
				input_error++;
				sprintf(error_string,
						"Can not have inactive cells in a 1D simulation.");
				error_msg(error_string, STOP);
			}
			n_to_ij(i, &ii, &jj);
			if (ii == 0)
			{
				forward1[i] = n;
				back[n].list = i;
				n++;
			}
			else
			{
				forward1[i] = -1;
			}
				cout<<"Zforward ["<<i<<"] ="<<forward1[i]<<"\n";
		}
		count_chem = n;
	}
	
	count_chem = n;
		cout<<"count_chem ="<<count_chem<<"\n";
	return;
}

/* ---------------------------------------------------------------------- */
int
n_to_ij(int n, int *i, int *j)
/* ---------------------------------------------------------------------- */
{
	int
		return_value;

	return_value = OK;

	*i = n % ix;
	*j = n/ ix;
	
	if (*i < 0 || *i >= ix)
	{
		error_msg("X index out of range", CONTINUE);
		return_value = ERROR;
	}
	if (*j < 0 || *j >= iz)
	{
		error_msg("z index out of range", CONTINUE);
		return_value = ERROR;
	}
	return (return_value);
}

/* ---------------------------------------------------------------------- */
void
CONVERT_TO_MOLAL(double *c, int *n, int *dim)
/* ---------------------------------------------------------------------- */
{
	int
		i;
/*
 *  convert c from mass fraction to moles
 *  The c array is dimensioned c(dim,ns).
 *  n is the number of rows that are used.
 *  In f90 dim = n and is often the number of
 *    cells in the domain.
 */
	for (i = 0; i < *n; i++)
	{
		vsrt_to_buffer(&c[i],*dim);
		buffer_to_moles();
		moles_to_vsrt(&c[i],*dim);
	}
	return;
}

/* ---------------------------------------------------------------------- */
void
CONVERT_TO_MASS_FRACTION(double *c, int *n, int *dim)
/* ---------------------------------------------------------------------- */
{
	int
		i;
/*
 *  convert c from mass fraction to moles
 */
	for (i = 0; i < *n; i++)
	{
		vsrt_moles_to_buffer(&c[i]);
		buffer_to_mass_fraction();
		buffer_to_vsrt(&c[i],*dim);
	}
	return;
}

/* ---------------------------------------------------------------------- */
void
ERRPRT_C(char *err_str, long l)
/* ---------------------------------------------------------------------- */
{
	char *
		e_string;

	e_string = (char *) PHRQ_malloc((size_t) (l + 1) * sizeof(char));
	strncpy(e_string, err_str, (size_t) (l));
	e_string[l] = '\0';
	string_trim_right(e_string);
	output_msg(OUTPUT_ECHO, "ERROR: %s\n", e_string);
	output_msg(OUTPUT_SCREEN, "ERROR: %s\n", e_string);
	free_check_null(e_string);
	return;
}

/* ---------------------------------------------------------------------- */
void
WARNPRT_C(char *err_str, long l)
/* ---------------------------------------------------------------------- */
{
	char *
		e_string;

	e_string = (char *) PHRQ_malloc((size_t) (l + 1) * sizeof(char));
	strncpy(e_string, err_str, (size_t) (l));
	e_string[l] = '\0';
	string_trim_right(e_string);
	output_msg(OUTPUT_ECHO, "WARNING: %s\n", e_string);
	output_fflush(OUTPUT_ECHO);
	output_msg(OUTPUT_SCREEN, "WARNING: %s\n", e_string);
	output_fflush(OUTPUT_SCREEN);
	free_check_null(e_string);
	return;
}

/* ---------------------------------------------------------------------- */
void
LOGPRT_C(char *err_str, long l)
/* ---------------------------------------------------------------------- */
{
	char *
		e_string;
	e_string = (char *) PHRQ_malloc((size_t) (l + 1) * sizeof(char));
	strncpy(e_string, err_str, (size_t) (l));
	e_string[l] = '\0';
	string_trim_right(e_string);
	output_msg(OUTPUT_ECHO, "%s\n", e_string);
	output_fflush(OUTPUT_ECHO);
	/*
	   fprintf(error_file,"%s\n", e_string);
	   fflush(error_file);
	 */
	free_check_null(e_string);
	return;
}

/* ---------------------------------------------------------------------- */
void
SCREENPRT_C(char *err_str, long l)
/* ---------------------------------------------------------------------- */
{
	char *
		e_string;

	e_string = (char *) PHRQ_malloc((size_t) (l + 1) * sizeof(char));
	strncpy(e_string, err_str, (size_t) (l));
	e_string[l] = '\0';
	string_trim_right(e_string);
	output_msg(OUTPUT_SCREEN, "%s\n", e_string);
	output_fflush(OUTPUT_SCREEN);
	free_check_null(e_string);
	return;
}
/* ---------------------------------------------------------------------- */

/* ---------------------------------------------------------------------- */
void
CALCULATE_WELL_PH(double *c, LDBLE * ph, LDBLE * alkalinity)
/* ---------------------------------------------------------------------- */
{
	struct solution *
		solution_ptr;
	int
		i,
		j,
		n_user;

	/*
	 *  put moles into buffer
	 */
	for (j = 0; j < count_component; j++)
	{
		buffer[j].moles = c[j];
	}
	n_user = -2;
    	buffer_to_cxxsolution(n_user);
	cxxSolution *
		cxxsoln = szBin.getSolution(n_user);
	reinitialize();
	solution[0] = cxxsoln->cxxSolution2solution();
	count_solution++;

	//  solution_duplicate(solution[first_solution]->n_user, n_user);
	//  solution_bsearch(first_user_number, &first_solution, TRUE);
	solution_ptr = solution_bsearch(n_user, &i, FALSE);
	if (solution_ptr == NULL)
	{
		sprintf(error_string,
				"Could not find solution %d in calculate_well_ph\n", n_user);
		error_msg(error_string, STOP);
	}
	/*
	 * Make enough space
	 */
	// solution[i]->totals = (struct conc *) PHRQ_realloc (solution[i]->totals, (size_t) (count_total - 1) * sizeof(struct conc));
	//if (solution[i]->totals == NULL) malloc_error();
	//solution[i]->master_activity = (struct master_activity *) PHRQ_realloc (solution[i]->master_activity, (size_t) (count_activity_list + 1) * sizeof(struct master_activity));
	//solution[i]->count_master_activity = count_activity_list;
	//solution[i]->species_gamma = NULL;
	//solution[i]->count_species_gamma = 0;
	//if (solution[i]->master_activity == NULL) malloc_error();
	/*
	 *  Zero out solution
	 */
	//for (j = 0; j < count_total - 1; j++) {
	//  solution_ptr->totals[j].moles = 0;
	//}
	/*
	 *  copy buffer to solution
	 */
	//buffer_to_solution(solution_ptr);
	/*
	 * set use flags
	 */
	use.temperature_ptr = NULL;
	use.irrev_ptr = NULL;
	use.mix_ptr = NULL;
	/*
	 *   set solution
	 */
	use.solution_ptr = solution[i];
	use.n_solution_user = n_user;
	use.n_solution = i;
	use.solution_in = TRUE;
	save.solution = TRUE;
	save.n_solution_user = n_user;
	save.n_solution_user_end = n_user;
	/*
	 *   Switch out exchange
	 */
	use.exchange_ptr = NULL;
	use.exchange_in = FALSE;
	save.exchange = FALSE;
	/*
	 *   Switch out gas_phase
	 */
	use.gas_phase_ptr = NULL;
	use.gas_phase_in = FALSE;
	save.gas_phase = FALSE;
	/*
	 *   Switch out pp_assemblage
	 */
	use.pp_assemblage_ptr = NULL;
	use.pp_assemblage_in = FALSE;
	save.pp_assemblage = FALSE;
	/*
	 *   Switch out surface
	 */
	use.surface_ptr = NULL;
	use.surface_in = FALSE;
	save.surface = FALSE;
	/*
	 *   Switch out s_s_assemblage
	 */
	use.s_s_assemblage_ptr = NULL;
	use.s_s_assemblage_in = FALSE;
	save.s_s_assemblage = FALSE;
	/*
	 *   Switch out kinetics
	 */
	use.kinetics_ptr = NULL;
	use.kinetics_in = FALSE;
	save.kinetics = FALSE;

	state = REACTION;
	run_reactions(n_user, 0.0, FALSE, 0.0);
	state = PHAST;
	*ph = -(s_hplus->la);
	*alkalinity = total_alkalinity / mass_water_aq_x;
	return;
}
void
CALCULATE_WELLPHWHEAT(double *c, LDBLE * ph, LDBLE * alkalinity,int *heat)
/* ---------------------------------------------------------------------- */
{
	struct solution *
		solution_ptr;
	int
		i,
		j,
		n_user;

	/*
	 *  put moles into buffer
	 */
	for (j = 0; j < count_component; j++)
	{
		buffer[j].moles = c[j];
	}
	n_user = -2;
	buffer_to_cxxsolutionwheat(n_user,heat);
	cxxSolution *
		cxxsoln = szBin.getSolution(n_user);
	reinitialize();
	solution[0] = cxxsoln->cxxSolution2solution();
	count_solution++;

	//  solution_duplicate(solution[first_solution]->n_user, n_user);
	//  solution_bsearch(first_user_number, &first_solution, TRUE);
	solution_ptr = solution_bsearch(n_user, &i, FALSE);
	if (solution_ptr == NULL)
	{
		sprintf(error_string,
				"Could not find solution %d in calculate_well_ph\n", n_user);
		error_msg(error_string, STOP);
	}
	/*
	 * Make enough space
	 */
	// solution[i]->totals = (struct conc *) PHRQ_realloc (solution[i]->totals, (size_t) (count_total - 1) * sizeof(struct conc));
	//if (solution[i]->totals == NULL) malloc_error();
	//solution[i]->master_activity = (struct master_activity *) PHRQ_realloc (solution[i]->master_activity, (size_t) (count_activity_list + 1) * sizeof(struct master_activity));
	//solution[i]->count_master_activity = count_activity_list;
	//solution[i]->species_gamma = NULL;
	//solution[i]->count_species_gamma = 0;
	//if (solution[i]->master_activity == NULL) malloc_error();
	/*
	 *  Zero out solution
	 */
	//for (j = 0; j < count_total - 1; j++) {
	//  solution_ptr->totals[j].moles = 0;
	//}
	/*
	 *  copy buffer to solution
	 */
	//buffer_to_solution(solution_ptr);
	/*
	 * set use flags
	 */
	use.temperature_ptr = NULL;
	use.irrev_ptr = NULL;
	use.mix_ptr = NULL;
	/*
	 *   set solution
	 */
	use.solution_ptr = solution[i];
	use.n_solution_user = n_user;
	use.n_solution = i;
	use.solution_in = TRUE;
	save.solution = TRUE;
	save.n_solution_user = n_user;
	save.n_solution_user_end = n_user;
	/*
	 *   Switch out exchange
	 */
	use.exchange_ptr = NULL;
	use.exchange_in = FALSE;
	save.exchange = FALSE;
	/*
	 *   Switch out gas_phase
	 */
	use.gas_phase_ptr = NULL;
	use.gas_phase_in = FALSE;
	save.gas_phase = FALSE;
	/*
	 *   Switch out pp_assemblage
	 */
	use.pp_assemblage_ptr = NULL;
	use.pp_assemblage_in = FALSE;
	save.pp_assemblage = FALSE;
	/*
	 *   Switch out surface
	 */
	use.surface_ptr = NULL;
	use.surface_in = FALSE;
	save.surface = FALSE;
	/*
	 *   Switch out s_s_assemblage
	 */
	use.s_s_assemblage_ptr = NULL;
	use.s_s_assemblage_in = FALSE;
	save.s_s_assemblage = FALSE;
	/*
	 *   Switch out kinetics
	 */
	use.kinetics_ptr = NULL;
	use.kinetics_in = FALSE;
	save.kinetics = FALSE;

	state = REACTION;
	run_reactions(n_user, 0.0, FALSE, 0.0);
	state = PHAST;
	*ph = -(s_hplus->la);
	*alkalinity = total_alkalinity / mass_water_aq_x;
	return;
}
/* ---------------------------------------------------------------------- */
void
ON_ERROR_CLEANUP_AND_EXIT(void)
/* ---------------------------------------------------------------------- */
{
	int
		errors;
/*
 *   Prepare error handling
 */
	errors = setjmp(mark);
	if (errors != 0)
	{
		clean_up();
		exit(1);
	}
	return;
}

/* ---------------------------------------------------------------------- */
void
SEND_RESTART_NAME(char *name, int nchar)
/* ---------------------------------------------------------------------- */
{
	int
		i = (int) FileMap.size();
	name[nchar - 1] = '\0';
	string_trim(name);
	std::string stdstring(name);
	FileMap[stdstring] = i;
}

/*--------------------------------------------------------------------------*/

void COLLECT_COMP(double *concs)
/*-------------------------------------------------------------------------*/
{
   int
		i,
		j;
	cxxSolution *
		cxxsoln_ptr;
	for (i = 0; i < count_chem; i++)
	{
		cxxsoln_ptr = szBin.getSolution(i);
		cxxsolution_to_buffer(cxxsoln_ptr);
//		buffer_to_moles();
		moles_to_vsrt(concs);
	}
	return;

}
/************************************************************************/
void
STORE_C_POINTERS(int *indx_sol1_ic, double *xnode, double *znode)
/*''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''*/
{
	// Fills in slave Fortran arrays
	initial_conditions1_c = indx_sol1_ic;
	x_node_c = xnode;
	z_node_c = znode;

	
return;
}
/*--------------------------------------------------------------------------*/
int
UZ_INIT()
/* ---------------------------------------------------------------------- */
{
	int
		i;

		old_vMoistureContent = (LDBLE *) PHRQ_malloc((size_t) (ixz * sizeof(LDBLE)));
		if (old_vMoistureContent == NULL)
			malloc_error();
		for (i = 0; i < ixz; i++)
		{
			old_vMoistureContent[i] = 1.0;
		}

	return (OK);
}
