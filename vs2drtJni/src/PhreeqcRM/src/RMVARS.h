#if !defined(RMVARS_H)
#define RMVARS_H
enum class RMVARS {
	ComponentCount,
	Components,
	Concentrations,
	DensityUser,
	DensityCalculated,
	ErrorString,
	FilePrefix,
	Gfw,
	GridCellCount,
	InputVarNames,
	NthSelectedOutput,
	OutputVarsAddSolutionProperties,
	OutputVarsAddSolutionTotalMolalities,
	OutputVarsAddSolutionMolalities,
	OutputVarsAddSolutionActivities,
	OutputVarsAddExchangeMolalities,
	OutputVarsAddSurfaceMolalities,
	OutputVarsAddEquilibriumPhases,
	OutputVarsAddSaturationIndices,
	OutputVarsAddGases,
	OutputVarsAddKineticReactants,
	OutputVarsAddSolidSolutions,
	OutputVarsAddCalculateValues,
	OutputVarsFinalize,
	OutputVarNames,
	SaturationCalculated,
	SaturationUser,
	SelectedOutput,
	SelectedOutputColumnCount,
	SelectedOutputCount,
	SelectedOutputHeadings,
	SelectedOutputRowCount,
	SolutionVolume,
	Time,
	TimeStep,
	CurrentSelectedOutputUserNumber,
	Porosity,
	Pressure,
	SelectedOutputOn,
	Temperature,
#if defined(WITH_PYBIND11)
	Temperature_as_strings,
#endif
	Viscosity,
	NotFound
};
enum class OUTPUTVARS {
	AddOutputVars,
	SolutionProperties,
	SolutionTotalMolalities,
	ExchangeMolalities,
	SurfaceMolalities,
	EquilibriumPhases,
	Gases,
	KineticReactants,
	SolidSolutions,
	CalculateValues,
	SolutionActivities,
	SolutionMolalities,
	SaturationIndices,
	NotFound
};
#endif