#include "BMIPhreeqcRM.h"
#include "BMIVariant.h"
#include "bmi.hxx"
#include <string>
#include <ostream>
#include <iostream>
#include <iomanip>
#include <sstream>

#include <queue>

#ifdef USE_YAML
#include "yaml-cpp/yaml.h"
#endif

#include "Phreeqc.h"
#include "IPhreeqcPhast.h"
#include "PhreeqcRM.h"
#include "VarManager.h"

std::map<size_t, BMIPhreeqcRM*> BMIPhreeqcRM::Instances;
size_t BMIPhreeqcRM::InstancesIndex = 0;

//// static BMIPhreeqcRM methods
/* ---------------------------------------------------------------------- */
void
BMIPhreeqcRM::CleanupBMIModuleInstances(void)
/* ---------------------------------------------------------------------- */
{
	std::map<size_t, BMIPhreeqcRM*>::iterator it = BMIPhreeqcRM::Instances.begin();
	std::vector<BMIPhreeqcRM*> bmirm_list;
	for (; it != BMIPhreeqcRM::Instances.end(); it++)
	{
		bmirm_list.push_back(it->second);
	}
	for (size_t i = 0; i < bmirm_list.size(); i++)
	{
		delete bmirm_list[i];
	}
}
/* ---------------------------------------------------------------------- */
int
BMIPhreeqcRM::CreateBMIModule()
/* ---------------------------------------------------------------------- */
{
	//_CrtSetDbgbool ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
	//_crtBreakAlloc = 5144;
	int n = IRM_OUTOFMEMORY;
	try
	{
		BMIPhreeqcRM* bmirm_ptr = new BMIPhreeqcRM();
		if (bmirm_ptr)
		{
			n = (int)bmirm_ptr->Index;
			BMIPhreeqcRM::Instances[n] = bmirm_ptr;
			bmirm_ptr->language = "F90";
			return n;
		}
	}
	catch (...)
	{
		return IRM_OUTOFMEMORY;
	}
	return IRM_OUTOFMEMORY;
}
/* ---------------------------------------------------------------------- */
int
BMIPhreeqcRM::CreateBMIModule(int nxyz, MP_TYPE nthreads)
/* ---------------------------------------------------------------------- */
{
	//_CrtSetDbgbool ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
	//_crtBreakAlloc = 5144;
	int n = IRM_OUTOFMEMORY;
	try
	{
		BMIPhreeqcRM* bmirm_ptr = new BMIPhreeqcRM(nxyz, nthreads);
		if (bmirm_ptr)
		{
			n = (int)bmirm_ptr->Index;
			BMIPhreeqcRM::Instances[n] = bmirm_ptr;
			bmirm_ptr->language = "F90";
			return n;
		}
	}
	catch (...)
	{
		return IRM_OUTOFMEMORY;
	}
	return IRM_OUTOFMEMORY;
}
/* ---------------------------------------------------------------------- */
IRM_RESULT
BMIPhreeqcRM::DestroyBMIModule(int id)
/* ---------------------------------------------------------------------- */
{
	IRM_RESULT retval = IRM_BADINSTANCE;
	std::map<size_t, BMIPhreeqcRM*>::iterator it = BMIPhreeqcRM::Instances.find(size_t(id));
	if (it != BMIPhreeqcRM::Instances.end())
	{
		delete (*it).second;
		retval = IRM_OK;
	}
	return retval;
}
/* ---------------------------------------------------------------------- */
BMIPhreeqcRM*
BMIPhreeqcRM::GetInstance(int id)
/* ---------------------------------------------------------------------- */
{
	std::map<size_t, BMIPhreeqcRM*>::iterator it = BMIPhreeqcRM::Instances.find(size_t(id));
	if (it != BMIPhreeqcRM::Instances.end())
	{
		return (*it).second;
	}
	return 0;
}
// Constructor
BMIPhreeqcRM::BMIPhreeqcRM()
: PhreeqcRM(PhreeqcRM::default_nxyz, PhreeqcRM::default_data_for_parallel_processing, nullptr, true)
, var_man{ nullptr }
{
	this->language = "cpp";
#if defined(WITH_PYBIND11)
	this->_initialized = false;
	this->language = "Py";
#endif
}
BMIPhreeqcRM::BMIPhreeqcRM(int nxyz, int nthreads)
: PhreeqcRM(nxyz, nthreads, nullptr, true) 
, var_man{ nullptr }
{
	this->language = "cpp";
#if defined(WITH_PYBIND11)
	this->_initialized = false;
	this->language = "Py";
#endif
}
// Destructor
BMIPhreeqcRM::~BMIPhreeqcRM()
{
}
void BMIPhreeqcRM::AddOutputVars(std::string option, std::string def)
{
	assert(this->var_man);
	this->var_man->AddOutputVars(option, def);
}
void BMIPhreeqcRM::ClearBMISelectedOutput(void)
{
	assert(this->var_man);
	this->var_man->BMISelectedOutput.clear();
}
void BMIPhreeqcRM::Construct(PhreeqcRM::Initializer i)
{
	this->PhreeqcRM::Construct(i);
	//std::map<size_t, BMIPhreeqcRM*>::value_type instance(this->GetWorkers()[0]->Get_Index(), this);
	std::map<size_t, BMIPhreeqcRM*>::value_type instance(this->Index, this);
	BMIPhreeqcRM::Instances.insert(instance);
	this->var_man = new VarManager((PhreeqcRM*)this);
	//this->language = "cpp";
#if defined(WITH_PYBIND11)
	this->_initialized = true;
#endif
}

// Model control functions.
void BMIPhreeqcRM::Initialize(std::string config_file)
{
#ifdef USE_YAML
#if defined(WITH_PYBIND11)
	if (config_file.size() != 0)
	{
#endif
	YAML::Node yaml = YAML::LoadFile(config_file);

	bool found_nxyz = false;
	bool found_threads = false;

	for (auto it = yaml.begin(); it != yaml.end(); it++)
	{
		YAML::Node node1 = *it;
		auto it1 = node1.begin();
		std::string keyword = it1++->second.as<std::string>();
		if (keyword == "SetGridCellCount")
		{
			this->initializer.nxyz_arg = it1++->second.as<int>();
			found_nxyz = true;
		}
		if (keyword == "ThreadCount")
		{
			this->initializer.data_for_parallel_processing = it1++->second.as<int>();
			found_threads = true;
		}
		if (found_threads && found_nxyz) break;
	}

	//if (yaml["SetGridCellCount"].IsDefined())
	//{
	//	this->initializer.nxyz_arg = yaml["SetGridCellCount"].as<int>();
	//}
	//if (yaml["ThreadCount"].IsDefined())
	//{
	//	this->initializer.data_for_parallel_processing = yaml["ThreadCount"].as<int>();
	//}
#if defined(WITH_PYBIND11)
	}
#endif
#endif

	this->Construct(this->initializer);

#ifdef USE_YAML
#if defined(WITH_PYBIND11)
	if (config_file.size() != 0)
	{
#endif
	this->InitializeYAML(config_file);
#if defined(WITH_PYBIND11)
	}
#endif
#endif
}
void BMIPhreeqcRM::Update()
{
	this->RunCells();
	this->SetTime(this->GetTime() + this->GetTimeStep());
	this->UpdateVariables();
}
void BMIPhreeqcRM::UpdateBMI(RMVARS v_enum)
{
	assert(this->var_man);
	this->var_man->RM2BMIUpdate(v_enum);
}
void BMIPhreeqcRM::UpdateVariables()
{
	this->var_man->task = VarManager::VAR_TASKS::Update;
	std::set<RMVARS>& UpdateSet = this->var_man->UpdateSet;
	for (auto it = this->var_man->UpdateSet.begin(); it != UpdateSet.end(); it++)
	{
		VarManager::VarFunction fn = this->var_man->GetFn(*it);
		//		((*this).*f)(); 
		// ((*this->var_man).*fn)();
		((*this->var_man).*fn)();
	}
}
void BMIPhreeqcRM::UpdateUntil(double time)
{
	double time_step = time - this->GetTime();
	if (time_step >= 0)
	{
		this->SetTimeStep(time_step);
		this->RunCells();
		this->SetTime(time);
		this->UpdateVariables();
	}
}
void BMIPhreeqcRM::Finalize()
{
	this->CloseFiles();
#if defined(WITH_PYBIND11)
	delete this->var_man;
	this->var_man = nullptr;
	this->_initialized = false;
#endif
}
void BMIPhreeqcRM::GenerateAutoOutputVars()
{
#ifdef USE_MPI
	if (this->mpi_myself == 0)
	{
		if (var_man != nullptr)
		{
			var_man->GenerateAutoOutputVars();
			this->SetCurrentSelectedOutputUserNumber(var_man->BMISelectedOutputUserNumber);
			if (var_man->NeedInitialRun)
			{
				bool current = this->phreeqcrm_io->Get_screen_on();
				this->SetScreenOn(false);
				this->RunCells();
				this->SetScreenOn(current);
			}
			// Initialize BMI variables
			var_man->task = VarManager::VAR_TASKS::Info;
			for (auto it = this->var_man->VariantMap.begin();
				it != this->var_man->VariantMap.end(); it++)
			{
				BMIVariant& bv = it->second;
				bv.SetInitialized(false);
				((*this->var_man).*bv.GetFn())();
			}
		}
	}
#else
	if (var_man != nullptr)
	{ 
		var_man->GenerateAutoOutputVars();
		this->SetCurrentSelectedOutputUserNumber(var_man->BMISelectedOutputUserNumber);
		//if (var_man->NeedInitialRun)
		//{
		//	bool current = this->phreeqcrm_io->Get_screen_on();
		//	this->SetScreenOn(false);
		//	this->RunCells();
		//	this->SetScreenOn(current);
		//}
		// Initialize BMI variables
		var_man->task = VarManager::VAR_TASKS::Info;
		for (auto it = this->var_man->VariantMap.begin();
			it != this->var_man->VariantMap.end(); it++)
		{
			BMIVariant& bv = it->second;
			bv.SetInitialized(false);
			((*this->var_man).*bv.GetFn())();
		}
	}
#endif
}
int BMIPhreeqcRM::GetInputItemCount()
{
	int count = 0;
	for (auto it = this->var_man->VariantMap.begin();
		it != this->var_man->VariantMap.end(); it++)
	{
		BMIVariant& bv = it->second;
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			//((*this->var_man).*bv.GetFn())();
			((*this->var_man).*bv.GetFn())();
		}
		if (bv.GetHasSetter())
		{
			count++;
		}
	}
	return count;
}
int BMIPhreeqcRM::GetOutputItemCount()
{
	int count = 0;
	for (auto it = this->var_man->VariantMap.begin();
		it != this->var_man->VariantMap.end(); it++)
	{
		BMIVariant& bv = it->second;
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}	
		if (bv.GetHasGetter())
		{
			count++;
		}
	}
	count += (int)this->var_man->AutoOutputVars.size();
	return count;
}
int BMIPhreeqcRM::GetPointableItemCount()
{
	int count = 0;
	for (auto it = this->var_man->VariantMap.begin();
		it != this->var_man->VariantMap.end(); it++)
	{
		BMIVariant& bv = it->second;
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		if (bv.GetHasPtr())
		{
			count++;
		}
	}
	return count;
}
std::vector<std::string> BMIPhreeqcRM::GetInputVarNames()
{
	std::vector <std::string> names;
	for (auto it = this->var_man->VariantMap.begin();
		it != this->var_man->VariantMap.end(); it++)
	{
		BMIVariant& bv = it->second;
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		if (bv.GetHasSetter()) 
		{
			names.push_back(bv.GetName());
		}
	}
	return names;
}
std::vector<std::string>  BMIPhreeqcRM::GetOutputVarNames()
{ 
	std::vector <std::string> names;
	for (auto it = this->var_man->VariantMap.begin();
		it != this->var_man->VariantMap.end(); it++)
	{
		BMIVariant& bv = it->second;
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		if (bv.GetHasGetter())
		{
			names.push_back(bv.GetName());
		}
	}
	for(auto it = var_man->AutoOutputVars.begin();
		it != var_man->AutoOutputVars.end(); it++)
	{ 
		names.push_back(it->second.GetName());
	}
	return names;
}
std::vector<std::string> BMIPhreeqcRM::GetPointableVarNames()
{
	std::vector <std::string> names;
	for (auto it = this->var_man->VariantMap.begin();
		it != this->var_man->VariantMap.end(); it++)
	{
		BMIVariant& bv = it->second;
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		if (it->first == RMVARS::InputVarNames)
		{
			continue;
		}
		if (it->first == RMVARS::OutputVarNames)
		{
			continue;
		}
		if (bv.GetHasPtr())
		{
			names.push_back(bv.GetName());
		}
	}
	return names;
}
std::string BMIPhreeqcRM::GetVarType(const std::string name)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		if (this->language == "cpp")
		{
			return bv.GetCType();
		}
		else if (this->language == "F90")
		{
			return bv.GetFType();
		}
		else if (this->language == "Py")
		{
			return bv.GetPType();
		}
	}
	{
		auto it = var_man->AutoOutputVars.find(name);
		if (it != var_man->AutoOutputVars.end())
		{
			if (this->language == "cpp")
			{
				return it->second.GetCType();
			}
			else if (this->language == "F90")
			{
				return it->second.GetFType();
			}
			else if (this->language == "Py")
			{
				return it->second.GetPType();
			}
		}
	}
	assert(false);
	return "Failed in GetVarType.";
}
std::string BMIPhreeqcRM::GetVarUnits(const std::string name)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		return bv.GetUnits();
	}
	{
		auto it = var_man->AutoOutputVars.find(name);
		if (it != var_man->AutoOutputVars.end())
		{
			return it->second.GetUnits();
		}
	}
	assert(false);
	return "Failed in GetVarUnits.";
}

int BMIPhreeqcRM::GetVarItemsize(const std::string name)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		return bv.GetItemsize();
	}
	{
		auto it = var_man->AutoOutputVars.find(name);
		if (it != var_man->AutoOutputVars.end())
		{
			return it->second.GetItemsize();
		}
	}
	assert(false);
	return 0;
}

int BMIPhreeqcRM::GetVarNbytes(const std::string name)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		return bv.GetNbytes();
	}
	{
		auto it = var_man->AutoOutputVars.find(name);
		if (it != var_man->AutoOutputVars.end())
		{
			return it->second.GetNbytes();
		}
	}
	assert(false);
	return 0;
}
double BMIPhreeqcRM::GetCurrentTime()
{
	return this->GetTime();
}
double BMIPhreeqcRM::GetStartTime()
{
	return this->GetTime();
}
double BMIPhreeqcRM::GetEndTime()
{
	return this->GetTime() + this->GetTimeStep();
}
//double BMIPhreeqcRM::GetTimeStep()
//{
//	return this->GetTimeStep();
//}
void BMIPhreeqcRM::GetValue(const std::string name, void* dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		int Nbytes = this->var_man->VarExchange.GetNbytes();
		int dim = this->var_man->VarExchange.GetDim();
		if (this->var_man->VarExchange.GetCType() == "bool" && dim == 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetBVarPtr(), Nbytes);
			return;
		}
		if (this->var_man->VarExchange.GetCType() == "int" && dim == 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetIVarPtr(), Nbytes);
			return;
		}
		if (this->var_man->VarExchange.GetCType() == "double" && dim == 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetDVarPtr(), Nbytes);
			return;
		}
		if (this->var_man->VarExchange.GetCType() == "std::vector<std::string>")
		{
			int itemsize = this->GetVarItemsize(name);
			//int nbytes = this->GetVarNbytes(name);
			std::stringstream all;
			for (size_t i = 0; i < this->var_man->VarExchange.GetStringVectorRef().size(); i++)
			{
				all << std::left << std::setfill(' ') << std::setw(itemsize) << this->var_man->VarExchange.GetStringVectorRef()[i];
			}
			memcpy( dest, all.str().data(), all.str().size());
			return;
		}
		if (this->var_man->VarExchange.GetCType() == "std::string" && dim == 0)
		{
			memcpy(dest, this->var_man->VarExchange.GetStringRef().data(), Nbytes);
			return;
		}
		if (this->var_man->VarExchange.GetCType() == "std::string" && dim == 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetStringRef().data(), Nbytes);
			return;
		}
		if (this->var_man->VarExchange.GetCType() == "double" && dim > 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetDoubleVectorPtr(), Nbytes);
			return;
		}
		if (this->var_man->VarExchange.GetCType() == "int" && dim > 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetIntVectorPtr(), Nbytes);
			return;
		}
	}
	{
		auto it = var_man->AutoOutputVars.find(name);
		if (it != var_man->AutoOutputVars.end())
		{
			if (var_man->BMISelectedOutput.size() == 0)
			{
				int n_user = GetCurrentSelectedOutputUserNumber();
				SetCurrentSelectedOutputUserNumber(var_man->BMISelectedOutputUserNumber);
				this->GetSelectedOutput(var_man->BMISelectedOutput);
				SetCurrentSelectedOutputUserNumber(n_user);
			}
			int column = it->second.GetColumn();
			int nxyz = GetGridCellCount();
			void* ptr = (void*)&(var_man->BMISelectedOutput[column * nxyz]);
			memcpy(dest, ptr, it->second.GetNbytes());
			return;
		}
	}	
	std::ostringstream oss;
	oss << "BMI GetValue void* failed for variable " << name << std::endl;
	this->ErrorMessage(oss.str(), true);
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, bool& dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		assert(this->var_man->VarExchange.GetCType() == "bool");
		dest = this->var_man->VarExchange.GetBVar();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, bool* dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		assert(this->var_man->VarExchange.GetCType() == "bool");
		int dim = this->var_man->VarExchange.GetDim();
		int nbytes = this->var_man->VarExchange.GetNbytes();
		if (dim == 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetBVarPtr(), nbytes);
			return;
		}
	}
	std::ostringstream oss;
	oss << "BMI GetValue bool* failed for variable " << name << std::endl;
	this->ErrorMessage(oss.str(), true);
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, double& dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		assert(this->var_man->VarExchange.GetCType() == "double");
		dest = this->var_man->VarExchange.GetDVar();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, double* dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		assert(this->var_man->VarExchange.GetCType() == "double");
		int dim = this->var_man->VarExchange.GetDim();
		int nbytes = this->var_man->VarExchange.GetNbytes();
		if (dim == 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetDVarPtr(), nbytes);
			return;
		}
		else if (dim > 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetDoubleVectorPtr(), nbytes);
			return;
		}

	}
	{
		auto it = var_man->AutoOutputVars.find(name);
		if (it != var_man->AutoOutputVars.end())
		{
			if (var_man->BMISelectedOutput.size() == 0)
			{
				int n_user = GetCurrentSelectedOutputUserNumber();
				SetCurrentSelectedOutputUserNumber(var_man->BMISelectedOutputUserNumber);
				this->GetSelectedOutput(var_man->BMISelectedOutput);
				SetCurrentSelectedOutputUserNumber(n_user);
			}
			int column = it->second.GetColumn();
			int nxyz = GetGridCellCount();
			void* ptr = (void*)&(var_man->BMISelectedOutput[column * nxyz]);
			memcpy(dest, ptr, it->second.GetNbytes());
			return;
		}
	}
	std::ostringstream oss;
	oss << "BMI GetValue double* failed for variable " << name << std::endl;
	this->ErrorMessage(oss.str(), true);
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, int& dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		dest = this->var_man->VarExchange.GetIVar();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, int* dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		assert(this->var_man->VarExchange.GetCType() == "int");
		int dim = this->var_man->VarExchange.GetDim();
		int nbytes = this->var_man->VarExchange.GetNbytes();
		if (dim == 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetIVarPtr(), nbytes);
			return;
		}
		else if (dim > 1)
		{
			memcpy(dest, this->var_man->VarExchange.GetIntVectorPtr(), nbytes);
			return;
		}
		std::ostringstream oss;
		oss << "BMI GetValue int* failed for variable " << name << std::endl;
		this->ErrorMessage(oss.str(), true);
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, std::string& dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		assert(this->var_man->VarExchange.GetCType() == "std::string");
		dest = this->var_man->VarExchange.GetStringVar();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, std::vector<double>& dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		assert(this->var_man->VarExchange.GetCType() == "double");
		dest = this->var_man->VarExchange.GetDoubleVectorRef();
		return;
	}
	{
		auto it = var_man->AutoOutputVars.find(name);
		if (it != var_man->AutoOutputVars.end())
		{
			if (var_man->BMISelectedOutput.size() == 0)
			{
				int n_user = GetCurrentSelectedOutputUserNumber();
				SetCurrentSelectedOutputUserNumber(var_man->BMISelectedOutputUserNumber);
				this->GetSelectedOutput(var_man->BMISelectedOutput);
				SetCurrentSelectedOutputUserNumber(n_user);
			}
			int column = it->second.GetColumn();
			int nxyz = GetGridCellCount();
			void* ptr = (void*)&(var_man->BMISelectedOutput[column * nxyz]);
			dest.resize(nxyz);
			memcpy(dest.data(), ptr, it->second.GetNbytes());
			return;
		}
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, std::vector<int>& dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		dest = this->var_man->VarExchange.GetIntVectorRef();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::GetValue(const std::string name, std::vector<std::string>& dest)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->task = VarManager::VAR_TASKS::GetVar;
		((*this->var_man).*bv.GetFn())();
		dest = this->var_man->VarExchange.GetStringVectorRef();
		return;
	}
	assert(false);
	return;
}
void* BMIPhreeqcRM::GetValuePtr(const std::string name)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (bv.GetVoidPtr() == NULL)
		{
			this->var_man->task = VarManager::VAR_TASKS::GetPtr;
			((*this->var_man).*bv.GetFn())();
		}
		return bv.GetVoidPtr();
	}
	assert(false);
	return NULL;
}
void BMIPhreeqcRM::SetValue(const std::string name, void* src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		// Store the variable in var_man->VarExchange
		int Nbytes = bv.GetNbytes();
		int itemsize = bv.GetItemsize();
		int dim = Nbytes / itemsize;
		if (bv.GetCType() == "bool" && dim == 1)
		{
			memcpy(this->var_man->VarExchange.GetBVarPtr(), src, Nbytes);
		}
		else if (bv.GetCType() == "int" && dim == 1)
		{
			memcpy(this->var_man->VarExchange.GetIVarPtr(), src, Nbytes);
		}
		else if (bv.GetCType() == "double" && dim == 1)
		{
			memcpy(this->var_man->VarExchange.GetDVarPtr(), src, Nbytes);
		}
		else if (bv.GetCType() == "std::string")
		{
			this->var_man->VarExchange.GetStringRef() = (char*)src;
		}
		else if (bv.GetCType() == "double" && dim > 1)
		{
			this->var_man->VarExchange.GetDoubleVectorRef().resize(dim);
			memcpy(this->var_man->VarExchange.GetDoubleVectorPtr(), src, Nbytes);
		}
		else if (bv.GetCType() == "int" && dim > 1)
		{
			this->var_man->VarExchange.GetIntVectorRef().resize(dim);
			memcpy(this->var_man->VarExchange.GetIntVectorPtr(), src, Nbytes);
		}
		//if (this->var_man->VarExchange.GetType() == "StringVector")
		//{
		//	// Don't think this is possible
		//	//int itemsize = this->GetVarItemsize(name);
		//	//int nbytes = this->GetVarNbytes(name);
		//	//std::stringstream all;
		//	//for (size_t i = 0; i < this->var_man->VarExchange.GetStringVectorRef().size(); i++)
		//	//{
		//	//	all << std::left << std::setfill(' ') << std::setw(itemsize) << this->var_man->VarExchange.GetStringVectorRef()[i];
		//	//}
		//	//memcpy( src, all.str().size());
		//}
		else
		{
			std::ostringstream oss;
			oss << "BMI failed in SetValue void* for variable " << name << std::endl;
			this->ErrorMessage(oss.str(), true);
			throw PhreeqcRMStop();
		}
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::SetValue(const std::string name, bool src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		// Store in var_man->VarExchange
		this->var_man->VarExchange.SetCType("bool");
		this->var_man->VarExchange.SetBVar(src);
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::SetValue(const std::string name, const char* src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		// Store in var_man->VarExchange
		this->var_man->VarExchange.SetStringVar(src);
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::SetValue(const std::string name, double src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		// Store in var_man->VarExchange
		this->var_man->VarExchange.SetDVar(src);
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::SetValue(const std::string name, int src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		// Store in var_man->VarExchange
		this->var_man->VarExchange.SetIVar(src);
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::SetValue(const std::string name, const std::string src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		// Store in var_man->VarExchange
		this->var_man->VarExchange.SetStringVar(src);
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::SetValue(const std::string name, std::vector<double> src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}	
		// Check dimension
		int dim = bv.GetDim(); 
		if (dim != src.size())
		{
			std::ostringstream oss;
			oss << "Dimension error in SetValue: " << name;
			this->ErrorMessage(oss.str());
			return;
		}
		// Store in var_man->VarExchange
		this->var_man->VarExchange.GetDoubleVectorRef().resize(bv.GetDim());
		this->var_man->VarExchange.SetDoubleVector(src);
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::SetValue(const std::string name, std::vector<int> src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}	// Store in var_man->VarExchange
		this->var_man->VarExchange.GetIntVectorRef().resize(bv.GetDim());
		this->var_man->VarExchange.SetIntVector(src);
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}
void BMIPhreeqcRM::SetValue(const std::string name, std::vector<std::string> src)
{
	RMVARS v_enum = this->var_man->GetEnum(name);
	if (v_enum != RMVARS::NotFound)
	{
		BMIVariant& bv = this->var_man->VariantMap[v_enum];
		//VarManager::VarFunction fn = this->var_man->GetFn(v_enum);
		if (!bv.GetInitialized())
		{
			this->var_man->task = VarManager::VAR_TASKS::Info;
			((*this->var_man).*bv.GetFn())();
		}
		this->var_man->VarExchange.SetCType("std::vector<std::string>");
		this->var_man->VarExchange.SetStringVector(src);
		// Set the variable
		this->var_man->task = VarManager::VAR_TASKS::SetVar;
		((*this->var_man).*bv.GetFn())();
		return;
	}
	assert(false);
	return;
}

int BMIPhreeqcRM::GetGridRank(const int grid)
{
	if (grid == 0)
	{
		return 1;
	}
	return 0;
};

int BMIPhreeqcRM::GetGridSize(const int grid)
{
	if (grid == 0)
	{
		return this->GetGridCellCount();
	}
	return 0;
};

std::string BMIPhreeqcRM::GetGridType(const int grid)
{
	if (grid == 0)
	{
		return "points";
	}
	return "Undefined grid identifier";
};


//////////////////

