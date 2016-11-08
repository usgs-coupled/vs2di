#include "PhreeqcRM.h"
#ifdef WIN32
#include <windows.h>
#endif
#include <string>
#include <map>
#include <iostream>
#include "FileHandler.h"
#include "RM_interface_C.h"
#include "IPhreeqcPhast.h"
#ifdef USE_GZ
#include "gzstream.h"
#else
#define gzFile FILE*
#define gzclose fclose
#define gzopen fopen
#define gzprintf fprintf
#define igzstream ifstream
#endif
#include "Phreeqc.h"
#include "Solution.h"
#include "IPhreeqc.h"
#ifdef USE_MPI
#include "mpi.h"
#endif
#if defined(_MSC_VER) && !defined(CMAKE_FC)
#define FC_FUNC_(name,NAME) NAME
#endif

#if defined(FC_FUNC_)
// Calls to Fortran
#define HDF_WRITE_INVARIANT         FC_FUNC_ (hdf_write_invariant,       HDF_WRITE_INVARIANT)
#define HDF_BEGIN_TIME_STEP         FC_FUNC_ (hdf_begin_time_step,       HDF_BEGIN_TIME_STEP)
#define HDF_END_TIME_STEP           FC_FUNC_ (hdf_end_time_step,         HDF_END_TIME_STEP)
#endif

class FileHandler: public PHRQ_base
{
public:
	FileHandler();
	~FileHandler(void);	
	IRM_RESULT ProcessRestartFiles(
		int id, 
		int *initial_conditions1_in,
		int *initial_conditions2_in, 
		double *fraction1_in);
	bool GetXYZInitialized(void) {return this->XYZInitialized;}
	void SetXYZInitialized(bool tf) {this->XYZInitialized = tf;}
	std::vector< std::ostream * > &GetXYZOstreams(void) {return this->XYZOstreams;}
	std::vector< std::ostream * > &GetObsOstreams(void) {return this->ObsOstreams;}
	void SetPointers(double *x_node, double *z_node, int * x_index, int * z_index, int *ic, double *saturation = NULL, int *mapping = NULL);
	IRM_RESULT WriteRestartFile(int *id, int *print_restart = NULL, int *indices_ic = NULL);
	IRM_RESULT WriteFiles(int id, int *print_xz = NULL, int *print_obs = NULL, int *xz_mask = NULL, int *obs_mask = NULL);
	IRM_RESULT WriteXYZ(int id, int *print_xyz, int *print_obs, int *xz_mask, int *obs_mask);

protected:
	bool XYZInitialized;
	std::vector < std::ostream * > XYZOstreams;	
	std::vector < std::ostream * > ObsOstreams;	
	double * x_node;
	double * z_node;
	int * x_index;
	int * z_index;
	double * saturation;  // only root
	int    * mapping;     // only root
	int    * ic;
};
FileHandler *ptr_file_handler;
// Constructor
FileHandler::FileHandler()
{
	this->io = new PHRQ_io;
	XYZInitialized = false;
	x_node = NULL;
	z_node = NULL;
	x_index = NULL;
	z_index = NULL;
	saturation = NULL;
	mapping = NULL;
	ic = NULL;
}
// Destructor
FileHandler::~FileHandler()
{
	delete this->io;
}

/* ---------------------------------------------------------------------- */
void
FileHandler::SetPointers(double *x_node_in, double *z_node_in, int * x_index_in, int * z_index_in, int *ic_in,
	double * saturation_in, int *mapping_in)
/* ---------------------------------------------------------------------- */
{
	this->x_node = x_node_in;
	this->z_node = z_node_in;
	x_index = x_index_in;
	z_index = z_index_in;
	this->saturation = saturation_in;
	this->mapping = mapping_in;  // only root
	this->ic = ic_in;
	if (this->x_node == NULL ||
		this->z_node == NULL ||
		x_index == NULL ||
		z_index == NULL ||
		this->ic == NULL)
	{
		error_msg("NULL pointer in FileHandler.SetPointers ", 1);
	}
}

/* ---------------------------------------------------------------------- */
IRM_RESULT
FileHandler::WriteFiles(int id, int *print_xz_in, int *print_obs_in, int *xz_mask, int *obs_mask)
/* ---------------------------------------------------------------------- */
{
	PhreeqcRM * Reaction_module_ptr = PhreeqcRM::GetInstance(id);
	if (Reaction_module_ptr)
	{	
		IRM_RESULT rtn = IRM_OK;
		int print_xz, print_obs;

		if (print_xz_in == 0 ||
			print_obs_in == 0 ||
			obs_mask == 0)
		{
			Reaction_module_ptr->ErrorHandler(IRM_FAIL, "NULL pointer in FileHandler::WriteFiles");
			//RM_Error(id, "Null pointer in WriteFiles");
		}
		print_xz = *print_xz_in;
		print_obs = *print_obs_in;

		if (print_xz != 0)
		{
			IRM_RESULT result = WriteXYZ(id, &print_xz, &print_obs, xz_mask, obs_mask);
			if (result) rtn = result;
		}		

		return rtn;
	}
	return IRM_BADINSTANCE;
}

/* ---------------------------------------------------------------------- */
IRM_RESULT
FileHandler::WriteXYZ(int id, int *print_xz, int *print_obs, int *xz_mask, int *obs_mask)
/* ---------------------------------------------------------------------- */
{
	PhreeqcRM * Reaction_module_ptr = PhreeqcRM::GetInstance(id);
	if (*print_xz == 0 && *print_obs == 0) return IRM_OK;
	if (Reaction_module_ptr)
	{	
		int nso = RM_GetSelectedOutputCount(id);
		int nxyz = RM_GetSelectedOutputRowCount(id); 
		double current_time = RM_GetTimeConversion(id) * RM_GetTime(id);
		//
		// Initialize XYZ
		//
		if (!this->GetXYZInitialized())
		{
			for (int iso = 0; iso < nso; iso++)
			{
				int status;
				int n_user = RM_GetNthSelectedOutputUserNumber(id, iso);
				if (n_user >= 0)
				{
					status = RM_SetCurrentSelectedOutputUserNumber(id, n_user);
					if (status >= 0)
					{
						// open files							
						char prefix[256];
						RM_GetFilePrefix(id, prefix, 256);
						if (*print_xz != 0)
						{
							// chem.xz.tsv
							std::ostringstream filename;
							filename << prefix << "_" << n_user << ".chem.xz.tsv";
							if (!this->Get_io()->punch_open(filename.str().c_str()))
							{
								Reaction_module_ptr->ErrorHandler(IRM_FAIL, "Could not open xyz file.");
							}
							this->GetXYZOstreams().push_back(this->Get_io()->Get_punch_ostream());
							// write first headings
							char line_buff[132];
							sprintf(line_buff, "%15s\t%15s\t%15s\t%15s\t", 
								"TIME",
								"NODE",
								"XR", 
								"Z");
							this->Get_io()->punch_msg(line_buff);

							// create chemistry headings
							int ncol = RM_GetSelectedOutputColumnCount(id);
							std::ostringstream h;
							for (int icol = 0; icol < ncol; icol++)
							{
								char head[100];
								status = RM_GetSelectedOutputHeading(id, icol, head, 100);
								std::string s(head);
								s.append("\t");
								h.width(20);
								h << s;
							}
							h << "\n";
							this->Get_io()->punch_msg(h.str().c_str());
						}
						if (*print_obs != 0)
						{
							// chemobs.xz.tsv
							std::ostringstream filename;
							filename << prefix << "_" << n_user << ".chemobs.xz.tsv";
							if (!this->Get_io()->punch_open(filename.str().c_str()))
							{
								Reaction_module_ptr->ErrorHandler(IRM_FAIL, "Could not open xyz file.");
							}
							this->GetObsOstreams().push_back(this->Get_io()->Get_punch_ostream());
							// write first headings
							char line_buff[132];
							sprintf(line_buff, "%15s\t%15s\t%15s\t%15s\t", 
								"TIME",
								"NODE",
								"XR", 
								"Z");
							this->Get_io()->punch_msg(line_buff);

							// create chemistry headings
							int ncol = RM_GetSelectedOutputColumnCount(id);
							std::ostringstream h;
							for (int icol = 0; icol < ncol; icol++)
							{
								char head[100];
								status = RM_GetSelectedOutputHeading(id, icol, head, 100);
								std::string s(head);
								s.append("\t");
								h.width(20);
								h << s;
							}
							h << "\n";
							this->Get_io()->punch_msg(h.str().c_str());
						}
					}
				}
			}
			this->SetXYZInitialized(true);
		}

		//	
		// Write XYZ file
		//
		std::vector<double> local_selected_out;
		int status;
		for (int iso = 0; iso < nso; iso++)
		{
			int n_user = RM_GetNthSelectedOutputUserNumber(id, iso);
			if (n_user >= 0)
			{
				status = RM_SetCurrentSelectedOutputUserNumber(id, n_user);
				int ncol = RM_GetSelectedOutputColumnCount(id);
				if (status >= 0)
				{
					local_selected_out.resize((size_t) (nxyz*ncol));
					RM_GetSelectedOutput(id, &local_selected_out.front());

					if (*print_xz != 0)
					{

						this->Get_io()->Set_punch_ostream(this->GetXYZOstreams()[iso]);

						// write xyz file

						for (int icell = 0; icell < nxyz; icell++)
						{
							if (xz_mask[icell] <= 0) continue;
							int active = 1;
							if (mapping[icell] < 0 || saturation[icell] <= 0)
							{
								active = 0;
							}

							// write x,y,z
							std::ostringstream ln;

							char line_buff[132];

							sprintf(line_buff, "%15g\t%15d\t%15g\t%15g\t",
								current_time,
								icell + 1,
								x_node[x_index[icell] - 1], 
								z_node[z_index[icell] - 1]);
							ln << line_buff;

							if (active)
							{
								// write chemistry values
								char token[21];
								for (int jcol = 0; jcol < ncol; jcol++)
								{		
									sprintf(token,"%19.10e\t", local_selected_out[jcol * nxyz + icell]);
									ln.width(20);
									ln << token;
								}
							}
							ln << "\n";
							this->Get_io()->punch_msg(ln.str().c_str());
						}
					}
					if (*print_obs != 0)
					{

						this->Get_io()->Set_punch_ostream(this->GetObsOstreams()[iso]);

						// write xyz file

						for (int icell = 0; icell < nxyz; icell++)
						{
							if (obs_mask[icell] <= 0) continue;
							int active = 1;
							if (mapping[icell] < 0 || saturation[icell] <= 0)
							{
								active = 0;
							}

							// write x,y,z
							std::ostringstream ln;

							char line_buff[132];

							sprintf(line_buff, "%15g\t%15d\t%15g\t%15g\t",
								current_time,
								icell + 1,
								x_node[x_index[icell] - 1], 
								z_node[z_index[icell] - 1]);
							ln << line_buff;

							if (active)
							{
								// write chemistry values
								char token[21];
								for (int jcol = 0; jcol < ncol; jcol++)
								{		
									sprintf(token,"%19.10e\t", local_selected_out[jcol * nxyz + icell]);
									ln.width(20);
									ln << token;
								}
							}
							ln << "\n";
							this->Get_io()->punch_msg(ln.str().c_str());
						}
					}
				}
			}
		}

		return IRM_OK;
	}
	return IRM_BADINSTANCE;
}
/* ---------------------------------------------------------------------- */
void
FH_FinalizeFiles()
/* ---------------------------------------------------------------------- */
{
	//HDFFinalize();

	if (ptr_file_handler != NULL)
	{
		ptr_file_handler->Get_io()->Set_punch_ostream(NULL);
		for (int iso = 0; iso < (int) ptr_file_handler->GetXYZOstreams().size(); iso++)
		{
			delete ptr_file_handler->GetXYZOstreams()[iso];
		}
		ptr_file_handler->GetXYZOstreams().clear();

		for (int iso = 0; iso < (int) ptr_file_handler->GetObsOstreams().size(); iso++)
		{
			delete ptr_file_handler->GetObsOstreams()[iso];
		}
		ptr_file_handler->GetObsOstreams().clear();

		assert(ptr_file_handler != NULL);
		delete ptr_file_handler;
		ptr_file_handler = NULL;
	}

}
//
// Wrappers
//

/* ---------------------------------------------------------------------- */
void
FH_SetPointers(double *x_node, double *z_node, int *x_index, int *z_index, int *ic, double *saturation, int *mapping)
/* ---------------------------------------------------------------------- */
{
	assert(ptr_file_handler == NULL);
	ptr_file_handler = new FileHandler;
	ptr_file_handler->SetPointers(x_node, z_node, x_index, z_index, ic, saturation, mapping);
}

/* ---------------------------------------------------------------------- */
void
FH_WriteFiles(int *id_in, int *print_xz, int *print_obs, int *xz_mask, int *obs_mask)
/* ---------------------------------------------------------------------- */
{
	int id = *id_in;
	ptr_file_handler->WriteFiles(id, print_xz, print_obs, xz_mask, obs_mask);
}
