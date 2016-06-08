/*
 * vs2Contents.java
 */
package vs2;

public interface vs2Constants {

    //public static final String VS2_VERSION = "1.3";
    public static final String VS2_VERSION = "1.4";

    //public static final int MAX_OUTPUT_TIMES = 500;
    //public static final int MAX_OBSERVATION_POINTS = 150;

    public static final int TEXTURAL_CLASS = 4101;
    public static final int EVAPOTRANSPIRATION = 4102;
    public static final int RECHARGE_PERIOD = 4103;
    public static final int TEXTURAL_MAP = 4104;
    public static final int INITIAL_FLOW = 4105;
    public static final int INITIAL_TEMPERATURE = 4109;
    public static final int FLUID_SOURCE = 4110;

    // Don't change values below
    public static final int INITIAL_PRESSURE_HEAD = 206;
    public static final int INITIAL_MOISTURE_CONTENT = 207;
    public static final int INITIAL_EQUILIBRIUM_PROFILE = 208;

    public static final int ARITHMETIC_MEAN_WEIGHTING = 0;
    public static final int GEOMETRIC_MEAN_WEIGHTING = 1;
    public static final int UPSTREAM_WEIGHTING = 2;

    public static final int BROOKS_COREY = 0;
    public static final int VAN_GENUCHTEN = 1;
    public static final int HAVERKAMP = 2;
    public static final int TABULAR_DATA = 3;
    public static final int ROSSI_NIMMO = 4;
    public static final int DEFAULT_SOIL_MODEL = VAN_GENUCHTEN;

    public static final int N0_ADSORPTION_NO_ION_EXCHANGE = 0;
    public static final int LINEAR_ADSORPTION = 1;
    public static final int LANGMUIR = 2;
    public static final int FREUNDLICH = 3;
    public static final int MONO_MONOVALENT_ION_EXCHANGE = 4;
    public static final int MONO_DIVALENT_ION_EXCHANGE = 5;
    public static final int DI_MONOVALENT_ION_EXCHANGE = 6;
    public static final int DI_DIVALENT_ION_EXCHANGE = 7;

    // Do not change bc codes. Values must match those in VS2DT.
    public static final int NO_FLOW_BC = 0;
    public static final int PRESSURE_HEAD_BC = 1;
    public static final int NORMAL_FLUID_FLUX_BC = 2;
    public static final int SEEPAGE_FACE_BC = 3;
    public static final int TOTAL_HEAD_BC = 4;
    public static final int EVAPORATION_BC = 5;
    public static final int VOLUMETRIC_FLOW_BC = 6;
    public static final int VERTICAL_FLUID_FLUX_BC = 7;
    public static final int GRAVITY_DRAIN_BC = 8;

    public static final int DEFAULT_CONC_BC = 0;
    public static final int SPECIFIED_CONC_BC = 1;
    public static final int DIFFUSIVE_FLUX_BC = 2;

    public static final int SOLUTE_TRANSPORT = 0;
    public static final int ENERGY_TRANSPORT = 1;
    public static final int SOLUTE_AND_ENERGY_TRANSPORT = 2;       // new in Version 1.4
    public static final int USAGE_UNDEFINED = -1;
    
    // IPRNTCHE options
    public static final int IPRNTCHE_NO_PHREEQC_OUTPUT = 0;        // new in Version 1.4
    public static final int IPRNTCHE_SELECTED_OUTPUT   = 1;        // new in Version 1.4

    // IPOUT options
    public static final int IPOUT_NO_PHREEQC_OUTPUT        = 0;    // new in Version 1.4
    public static final int IPOUT_EXTENSIVE_PHREEQC_OUTPUT = 1;    // new in Version 1.4
}
