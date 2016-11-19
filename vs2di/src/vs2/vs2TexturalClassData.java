/*
 * vs2TexturalClassData.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

/**
 * Holds the textural class data in a table
 */
public class vs2TexturalClassData extends mp2TableData implements
        vs2Constants, Serializable {

    static final long serialVersionUID = -3614085714246550385L;

    protected int idCounter;

    protected static final String [] COLUMN_NAME = {
        // Common hydraulic parameters  (Note that "Id" and "Generic" are never
        // used by table but required for spacing)
        "Id", "Color", "Name", "Kzz/Khh", "Ss", "Porosity", "RMC", "Generic",
        //0      1        2        3       4        5         6        7

        // Brooks-Corey
        "Sat Khh", "hb", "lambda",
        //  8        9       10

        // Van Genuchten
        "Sat Khh", "alpha", "beta",
        //  11       12       13

        // Haverkamp
        "Sat Khh", "A'", "B'", "alpha", "beta",
        //  14      15    16      17      18

        // Tabular Data  (Note that "Hkm Data" is never used, but required for
        // spacing)
        "Sat Khh", "Hkm Data",
        //  19         20

        // Common Transport parameters
        "alpha-L", "alpha-T", "mole. diff", "decay", "density",
        //  21         22          23          24       25

        // Linear Adsorption isotherm
        "Kd",
        //26

        // Langmuir isotherm
        "K1", "Q",
        //27  28

        // Freundlich isotherm
        "Kf", "n",
        //29   30

        // Ion Exchange
        "Km", "Q^", "C0",
        //31   32    33

        // Energy transport parameters
        "Cs", "KTr", "KTs", "Cw",
        //34   35     36     37

        // Rossi-Nimmo
        "Sat Khh", "psi0", "psiD", "lambda",
        // 38        39      40       41
        
        // Solute parameters
        "alpha-L", "alpha-T",
        // 42         43
        
        // INSOL(7)
        "Solution", "Eq phases", "Exchange", "Surface", "Gas phase", "Solid solns", "Kinetics"
        // 44          45            46         47          48            49            50
    };

    protected static final Class [] COLUMN_CLASS = {
        Integer.class, ImageIcon.class,
        String.class, Double.class, Double.class, Double.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class,
        // new for 1.4 Long disp, Trans disp, INSOL(7)
        Double.class, Double.class,
        Integer.class, Integer.class, Integer.class, Integer.class, Integer.class, 
        Integer.class, Integer.class
    };

    protected static final String [] TOOL_TIP_TEXT = {
        // Common hydraulic parameters  (Note that "Id" and "generic" are never
        // used by table but required for spacing)
        "Id",
        "color code",
        "name of textural class",
        "vertical to horizontal anisotropy",
        "specific storage",
        "porosity",
        "residual mositure content",
        "generic",

        // Brooks-Corey
        "saturated hydraulic conductivity",
        "air entry pressure head in Brooks-Corey model",
        "pore-size distribution index",

        // Van Genuchten
        "saturated hydraulic conductivity",
        "alpha paremeter in van Genuchten model",
        "beta parameter in van Genuchten model",

        // Haverkamp
        "saturated hydraulic conductivity",
        "A' paremeter in Haverkamp model",
        "B' paremeter in Haverkamp model",
        "alpha parameter in Haverkamp model",
        "beta parameter in Haverkamp model",

        // Tabular Data  (Note that "Hkm Data" is never used, but required for
        // spacing)
        "saturated hydraulic conductivity",
        "Hkm Data",

        // Common Transport parameters
        "longitudinal dispersivity",
        "transverse dispersivity",
        "cofficient of molecular diffusion",
        "decay constant",
        "bulk density of solid phase",

        // Linear Adsorption isotherm
        "equilibrium distribution coefficient",

        // Langmuir isotherm
        "Langmuir adsorption constant",
        "Maximum number of adsorption sites",

        // Freundlich isotherm
        "Freundlich adsorption constant",
        "Freunclich exponent",

        // Ion Exchange
        "ion-exchange selectivity coefficient",
        "ion-exchange capacity",
        "total-solution concentration",

        // Energy transport parameters
        "Heat capacity of dry solids",
        "Thermal cond of water-sediment at residual moisture content",
        "Thermal cond of water-sediment at full saturation",
        "Heat capacity of water",

        // Rossi-Nimmo parameters
        "saturated hydraulic conductivity",
        "psi0 parameter in Rossi-Nimmo model",
        "psiD parameter in Rossi-Nimmo model",
        "lambda parameter in Rossi-Nimmo model",
        
        // SOLUTE
        "longitudinal dispersivity",
        "transverse dispersivity",
        
        // INSOL(7)
        "SOLUTION number",
        "EQUILIBRIUM_PHASES number",
        "EXCHANGE number",
        "SURFACE number",
        "GAS_PHASE number",
        "SOLID_SOLUTIONS number",
        "KINETICS number"
    };

    /**
     * Constructor
     */
    public vs2TexturalClassData() {
        super ();
        assert(COLUMN_NAME.length == 51);
        assert(COLUMN_NAME.length == COLUMN_CLASS.length);
        assert(COLUMN_NAME.length == TOOL_TIP_TEXT.length);
        idCounter = 0;
        // Start off with a default row in the data set
        Object [] aRow = createDefaultRow();
        // Put 2 rows of dummy values in the tabular data
        // Actual values don't matter because sat K is zero.
        Vector hkmData = (Vector) aRow[20];
        Object [] hkm = new Object [3];
        hkm[0] = new Double(-1);
        hkm[1] = new Double(.2);
        hkm[2] = new Double(.2);
        hkmData.addElement(hkm);
        hkm = new Object [3];
        hkm[0] = new Double(-2);
        hkm[1] = new Double(.1);
        hkm[2] = new Double(.1);
        hkmData.addElement(hkm);
        dataRows.addElement(aRow);
    }

    public void convertToCurrentVersion() {
        Object [] oldRow;
        Object [] newRow;
        for (int i=0; i<dataRows.size(); i++) {
            oldRow = (Object []) dataRows.elementAt(i);
            newRow = createDefaultRow();
            for (int j=0; j<oldRow.length; j++) {
                newRow[j] = oldRow[j];
            }
            dataRows.setElementAt(newRow, i);
        }
    }

    /**
     * Get the maximum number of tabular data rows among textural classes
     */
    public int getMaxTabularDataRows() {
        int result = 0;
        for (int i=0; i<dataRows.size(); i++) {
            Object [] aRow = (Object []) dataRows.elementAt(i);
            int size = ((Vector) aRow[20]).size();
            if (size > result) {
                result = size;
            }
        }
        return result;
    }

    public void init(mp2Doc doc) {
        super.init(doc);
        clearUndoOnDelete = true;
    }

    /**
     * Export data
     */
    public void exportData(PrintWriter pw, vs2ModelOptions modelOptions) {
        final int commentOffset = 23;
        String s;
        Object [] aRow;
        int maxTabDataSize = getMaxTabularDataRows();
        for (int i=0; i<dataRows.size(); i++) {
            aRow = (Object []) dataRows.elementAt(i);
            // Card B-8
            s = String.valueOf((i+1));
            pw.println(s + vs2App.tab(s, commentOffset)
                    + "/B-8 -- ITEX. B-9 to begin next line: HK");
            // Card B-9
            switch (modelOptions.soilModel) {
            case BROOKS_COREY:
                pw.println(((Double) aRow[3]).doubleValue() + " " +
                           ((Double) aRow[8]).doubleValue() + " " +
                           ((Double) aRow[4]).doubleValue() + " " +
                           ((Double) aRow[5]).doubleValue() + " " +
                           ((Double) aRow[9]).doubleValue() + " " +
                           ((Double) aRow[6]).doubleValue() + " " +
                           ((Double) aRow[10]).doubleValue());
                break;
            case VAN_GENUCHTEN:
                pw.println(((Double) aRow[3]).doubleValue() + " " +
                           ((Double) aRow[11]).doubleValue() + " " +
                           ((Double) aRow[4]).doubleValue() + " " +
                           ((Double) aRow[5]).doubleValue() + " " +
                           ((Double) aRow[12]).doubleValue() + " " +
                           ((Double) aRow[6]).doubleValue() + " " +
                           ((Double) aRow[13]).doubleValue());
                break;
            case HAVERKAMP:
                pw.println(((Double) aRow[3]).doubleValue() + " " +
                           ((Double) aRow[14]).doubleValue() + " " +
                           ((Double) aRow[4]).doubleValue() + " " +
                           ((Double) aRow[5]).doubleValue() + " " +
                           ((Double) aRow[15]).doubleValue() + " " +
                           ((Double) aRow[6]).doubleValue() + " " +
                           ((Double) aRow[16]).doubleValue() + " " +
                           ((Double) aRow[17]).doubleValue() + " " +
                           ((Double) aRow[18]).doubleValue());
                break;
            case ROSSI_NIMMO:
                pw.println(((Double) aRow[3]).doubleValue() + " " +
                           ((Double) aRow[38]).doubleValue() + " " +
                           ((Double) aRow[4]).doubleValue() + " " +
                           ((Double) aRow[5]).doubleValue() + " " +
                           ((Double) aRow[39]).doubleValue() + " " +
                           ((Double) aRow[40]).doubleValue() + " " +
                           ((Double) aRow[41]).doubleValue());
                break;
            case TABULAR_DATA:
                pw.println(((Double) aRow[3]).doubleValue() + " " +
                           ((Double) aRow[19]).doubleValue() + " " +
                           ((Double) aRow[4]).doubleValue() + " " +
                           ((Double) aRow[5]).doubleValue());
                Vector hkmData = (Vector) aRow[20];
                for (int j=0; j<3; j++) {
                    for (int k=0; k<hkmData.size(); k++) {
                        Object [] hkm = (Object []) hkmData.elementAt(k);
                        pw.print(((Double) hkm[j]).doubleValue() + " ");
                    }
                    pw.println("99");
                }
                // Padding
                if (hkmData.size() < maxTabDataSize) {
                    for (int j=0; j<3*(maxTabDataSize - hkmData.size()); j++) {
                        pw.print("99 ");
                    }
                    pw.println();
                }
                break;
            }
            
            // Card B-10 (was B-7A - VS2DH)
            if (modelOptions.doEnergyTransport) {
                s = String.valueOf(((Double) aRow[21]).doubleValue() + " " +
                                   ((Double) aRow[22]).doubleValue() + " " +
                                   ((Double) aRow[34]).doubleValue() + " " +
                                   ((Double) aRow[35]).doubleValue() + " " +
                                   ((Double) aRow[36]).doubleValue() + " " +
                                   ((Double) aRow[37]).doubleValue() );
                pw.println(s + vs2App.tab(s, commentOffset)
                        + "/B-10 -- HT");
            }
            // Card B-11 (was B-7A - VS2DT)
            if (modelOptions.doSoluteTransport) {
                s = String.valueOf(((Double) aRow[42]).doubleValue() + " " +
                                   ((Double) aRow[43]).doubleValue() + " " +
                                   ((Double) aRow[23]).doubleValue() );
                pw.println(s + vs2App.tab(s, commentOffset)
                        + "/B-11 -- HS");
            }
        }
    }

    /**
     * Set the column name, column class, and tool tip text.
     */
    protected void setColumnAttributes() {
        columnName = COLUMN_NAME;
        columnClass = COLUMN_CLASS;
        toolTipText = TOOL_TIP_TEXT;
    }

    /**
     * Creates a default row and return it
     */
    public Object [] createDefaultRow() {
        Object [] aRow = new Object[COLUMN_NAME.length];

        // Common flow parameters
        aRow[0] = new Integer(idCounter);  // a unique id for the textural class
        aRow[1] = new Color(255, 255, 255);   // color
        aRow[2] = new String("inactive");  // name
        aRow[3] = new Double(1.0);         // anisotropy
        aRow[4] = new Double(0.0);         // specific storage
        aRow[5] = new Double(0.0);         // porosity
        aRow[6] = new Double(0.0);         // residual moisture content
        aRow[7] = new Integer(-1);         // generic data index

        // Brooks-Corey parameters
        aRow[8] = new Double(0.0);         // saturated hydraulic conductivity
        aRow[9] = new Double(0.0);         // h_b
        aRow[10] = new Double(0.0);        // pore-size distribution index

        // Van Genuchten parameters
        aRow[11] = new Double(0.0);        // saturated hydraulic conductivity
        aRow[12] = new Double(0.0);        // alpha prime
        aRow[13] = new Double(0.0);        // beta prime

        // Haverkamp parameters
        aRow[14] = new Double(0.0);        // saturated hydraulic conductivity
        aRow[15] = new Double(0.0);        // A prime
        aRow[16] = new Double(0.0);        // B prime
        aRow[17] = new Double(0.0);        // alpha
        aRow[18] = new Double(0.0);        // beta

        // Tabular data
        aRow[19] = new Double(0.0);        // saturated hydraulic conductivity
        aRow[20] = new Vector();           // head-conductivity-moisture data

        // Common Transport parameters
        aRow[21] = new Double(0.0);        // longitudinal dispersivity
        aRow[22] = new Double(0.0);        // transverse dispersivity
        aRow[23] = new Double(0.0);        // coefficient of molecular diffusion
        aRow[24] = new Double(0.0);        // decay constant
        aRow[25] = new Double(0.0);        // bulk density of solid phase

        // Linear isotherm
        aRow[26] = new Double(0.0);        // Kd for linear adsorption isotherm

        // Langmuir isotherm
        aRow[27] = new Double(0.0);        // K1 for Langmuir isotherm
        aRow[28] = new Double(0.0);        // Q for Langmuir isotherm

        // Freundlich isotherm
        aRow[29] = new Double(0.0);        // Kf for Freundlich isotherm
        aRow[30] = new Double(1.0);        // n for Freundlich isotherm

        // Ion exchange
        aRow[31] = new Double(0.0);        // Km for ion exchange
        aRow[32] = new Double(0.0);        // Q^ for ion exchange
        aRow[33] = new Double(0.0);        // C0 for ion exchange

        // Energy transport parameters
        aRow[34] = new Double(0.0);        // heat capacity of dry solid
        aRow[35] = new Double(0.0);        // thermal conductivity at residual moisture
        aRow[36] = new Double(0.0);        // thermal conductivity at saturation
        aRow[37] = new Double(0.0);        // heat capacity of water

        // Rossi-Nimmo parameters
        aRow[38] = new Double(0.0);        // saturated hydraulic conductivity
        aRow[39] = new Double(0.0);        // psi0
        aRow[40] = new Double(0.0);        // psiD
        aRow[41] = new Double(0.0);        // lambda
        
        // Chemistry
        aRow[42] = new Double(0.0);        // longitudinal dispersivity (solute)
        aRow[43] = new Double(0.0);        // transverse dispersivity (solute)
        
        aRow[44] = new Integer(-1);        // solution
        aRow[45] = new Integer(-1);        // equilibrium_phases
        aRow[46] = new Integer(-1);        // exchange
        aRow[47] = new Integer(-1);        // surface
        aRow[48] = new Integer(-1);        // gas_phase
        aRow[49] = new Integer(-1);        // solid_solutions
        aRow[50] = new Integer(-1);        // kinetics

        // Increment idCounter for next time
        idCounter++;

        return aRow;
    }

    /**
     * Override the superclass method to wrap the Color by an ImageIcon.
     */
    public Object getObjectAt (int r, int c) {
        if (c == 1) {
            return new mp2ColorRectIcon((Color) super.getObjectAt(r, 1),
                40, 10);
        } else {
            return super.getObjectAt(r, c);
        }
    }

    public boolean isFreundlichNonlinear() {
        if (dataRows.size() == 0) {
            return false;
        }
        for (int i=0; i<dataRows.size(); i++) {
            Object [] aRow = (Object []) dataRows.elementAt(i);
            double n = ((Double) aRow[30]).doubleValue();
            if (n != 1) {
            return true;
            }
        }
        return false;
    }

    /**
     * Gets the row index of the specified texture class id
     */
    public int getRowIndexOfId(int id) {
        for (int i=0; i<dataRows.size(); i++) {
            Object [] aRow = (Object []) dataRows.elementAt(i);
            if (((Integer) aRow[0]).intValue() == id) {
                return i;
            }
        }
        return -1;
    }

    public void convertGenericProperties() {
        for (int i=0; i<dataRows.size(); i++) {
            Object [] aRow = (Object []) dataRows.elementAt(i);
            if (((Integer) aRow[7]).intValue() != -1) {
                aRow[2] = "*" + ((String) aRow[2]);
                aRow[7] = new Integer(-1);
            }
        }
        for (int i=0; i<registeredTableModels.size(); i++) {
            ((mp2TableModel) registeredTableModels.elementAt(i)).
                    notifyDataChanged();
        }
    }

    /**
     * Changes the sign of the alpha prime parameter in the van Genuchten function
     * from the 1.0beta definition to 1.0(final) definition.
     * Returns true if any change is made, return false if all alpha parameters
     * are zero, meaning that the van Genuchten function was not used.
     */
    public boolean updateVanGenuchtenParametersFrom1p0betaTo1p0() {
        Object [] aRow;
        boolean changed = false;
        double alpha;
        for (int i=1; i<dataRows.size(); i++) {
            aRow = (Object []) dataRows.elementAt(i);
            alpha = ((Double) aRow[12]).doubleValue();
            if (alpha != 0) {
                aRow[12] = new Double( - alpha);
                changed = true;
            }
        }
        return changed;
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public void openAndAddFileData (String file, int soilModel, int insertPos) {

	int                      i, j, mats;
        int                      dataColumns = 0;
        int                      currentRows = getNumberOfRows ();
	vs2TexturalClassFileData tt = null;

        try {
	    switch (soilModel)
                {
	    case BROOKS_COREY:
	        tt          = new vs2TexturalClassFileData (file, "BC", "Brooks-Corey");
	        dataColumns = vs2TexturalClassWindow.BROOKS_COREY_MASK.length - 1;
		break;
	    case HAVERKAMP:
	        tt          = new vs2TexturalClassFileData (file, "HK", "Haverkamp");
	        dataColumns = vs2TexturalClassWindow.HAVERKAMP_MASK.length - 1;
		break;
	    case TABULAR_DATA:
	        tt          = new vs2TexturalClassFileData (file, "TD", "Tabular Data");
	        dataColumns = vs2TexturalClassWindow.TABULAR_DATA_MASK.length - 1;
		break;
	    case VAN_GENUCHTEN:
	        tt          = new vs2TexturalClassFileData (file, "VG", "van Genuchten");
	        dataColumns = vs2TexturalClassWindow.VAN_GENUCHTEN_MASK.length - 1;
		break;
	    case ROSSI_NIMMO:
	        tt          = new vs2TexturalClassFileData (file, "RN", "Rossi-Nimmo");
	        dataColumns = vs2TexturalClassWindow.ROSSI_NIMMO_MASK.length - 1;
		break;
	    default:
		return;
		}

            j    = insertPos;
            i    = 0;
            mats = 0;
            while (i<tt.data.size ()) {
                Object [] aRow = createDefaultRow();

		mats++;

		switch (soilModel)
                    {
                case BROOKS_COREY:  i += evaluateBrooksCoreyData  (tt.data, i, aRow, dataColumns);  break;
                case HAVERKAMP:     i += evaluateHaverkampData    (tt.data, i, aRow, dataColumns);  break;
                case TABULAR_DATA:  i += evaluateTabularDataData  (tt.data, i, aRow, dataColumns);  break;
                case VAN_GENUCHTEN: i += evaluateVanGenuchtenData (tt.data, i, aRow, dataColumns);  break;
                case ROSSI_NIMMO:   i += evaluateRossiNimmoData   (tt.data, i, aRow, dataColumns);  break;
		    }

		i += readHeatAndConcData (tt.data, i, tt.heat, tt.conc, aRow);

                addRow (j++, aRow);
            }
	    assignMaterialColors (mats, insertPos + 1);

        } catch (FileNotFoundException e) {
            mp2MessageBox.showMessageDialog ("Error: Could not find " + file + ".", "File Not Found!");
        } catch (IOException e) {
            mp2MessageBox.showMessageDialog ("Error: Line format problem, check data file syntax.",
                                             "I/O Exception Error");
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog ("Error: Number format problem, check data file syntax.",
                                             "Number Format Error");
        }
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public int evaluateBrooksCoreyData (Vector data, int pos, Object aRow[], int dataColumns) throws IOException,
								  			             NumberFormatException {
	int      i = 0;
        String[] fields;

	fields   = new String[dataColumns];
        fields   = split ((String) data.elementAt (pos + i++), dataColumns);

        aRow[2]  = trimQuotes (new String (fields[0]));         // material name
        aRow[3]  = new Double (fields[1]);                      // anisotropy
        aRow[8]  = new Double (fields[2]);                      // saturatedHydraulicConductivity
        aRow[4]  = new Double (fields[3]);                      // specificStorage
        aRow[5]  = new Double (fields[4]);                      // porosity
        aRow[6]  = new Double (fields[5]);                      // residualMoistureContent
        aRow[9]  = new Double (fields[6]);                      // h_b
        aRow[10] = new Double (fields[7]);                      // pore-size distribution index

	return i;
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public int evaluateHaverkampData (Vector data, int pos, Object aRow[], int dataColumns) throws IOException,
                                                                                                   NumberFormatException {
        int      i = 0;
        String[] fields;

        fields   = new String[dataColumns];
        fields   = split ((String) data.elementAt (pos + i++), dataColumns);

        aRow[2]  = trimQuotes (new String (fields[0]));         // material name
        aRow[3]  = new Double (fields[1]);                      // anisotropy
        aRow[14] = new Double (fields[2]);                      // saturatedHydraulicConductivity
        aRow[4]  = new Double (fields[3]);                      // specificStorage
        aRow[5]  = new Double (fields[4]);                      // porosity
        aRow[6]  = new Double (fields[5]);                      // residualMoistureContent
        aRow[15] = new Double (fields[6]);                      // A'
        aRow[16] = new Double (fields[7]);                      // B'
        aRow[17] = new Double (fields[8]);                      // alpha
        aRow[18] = new Double (fields[9]);                      // beta

        return i;
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public int evaluateRossiNimmoData (Vector data, int pos, Object aRow[], int dataColumns) throws IOException,
                                                                                                   NumberFormatException {
        int      i = 0;
        String[] fields;

        fields   = new String[dataColumns];
        fields   = split ((String) data.elementAt (pos + i++), dataColumns);

        aRow[2]  = trimQuotes (new String (fields[0]));         // material name
        aRow[3]  = new Double (fields[1]);                      // anisotropy
        aRow[38] = new Double (fields[2]);                      // saturatedHydraulicConductivity
        aRow[4]  = new Double (fields[3]);                      // specificStorage
        aRow[5]  = new Double (fields[4]);                      // porosity
        aRow[39] = new Double (fields[5]);                      // psi0
        aRow[40] = new Double (fields[6]);                      // psiD
        aRow[41] = new Double (fields[7]);                      // lambda

        return i;
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public int evaluateTabularDataData (Vector data, int pos, Object aRow[], int dataColumns) throws IOException,
                                                                                                     NumberFormatException {
        int        i = 0;
	int        jj, k;
        int        samples;
        String[]   fields;
	vs2HkmData hkmData;

        fields   = new String[dataColumns];
        fields   = split ((String) data.elementAt (pos + i++), dataColumns);

        aRow[2]  = trimQuotes (new String (fields[0]));         // material name
        aRow[3]  = new Double (fields[1]);                      // anisotropy
        aRow[19] = new Double (fields[2]);                      // saturatedHydraulicConductivity
        aRow[4]  = new Double (fields[3]);                      // specificStorage
        aRow[5]  = new Double (fields[4]);                      // porosity

        // Read Number of Samples
        hkmData = new vs2HkmData ();
        fields  = split ((String) data.elementAt (pos + i++), 1);
        samples = Integer.parseInt (fields[0]);
        jj      = hkmData.getNumberOfRows () - 1;
        for (k=0; k<samples; k++) {
            fields  = split ((String) data.elementAt (pos + i++), hkmData.COLUMN_NAME.length);
            Object [] bRow = hkmData.createDefaultRow ();

            bRow[0] = new Double (fields[0]);      // pressure head
            bRow[1] = new Double (fields[1]);      // relative hydraulic conductivity
            bRow[2] = new Double (fields[2]);      // moisture content

            hkmData.addRow (jj++, bRow);
        }

        aRow[20] = hkmData.getData();

        return i;
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public int evaluateVanGenuchtenData (Vector data, int pos, Object aRow[], int dataColumns) throws IOException,
                                                                                                      NumberFormatException {
        int      i = 0;
        String[] fields;

        fields   = new String[dataColumns];
        fields   = split ((String) data.elementAt (pos + i++), dataColumns);

        aRow[2]  = trimQuotes (new String (fields[0]));         // material name
        aRow[3]  = new Double (fields[1]);                      // anisotropy
        aRow[11] = new Double (fields[2]);                      // saturatedHydraulicConductivity
        aRow[4]  = new Double (fields[3]);                      // specificStorage
        aRow[5]  = new Double (fields[4]);                      // porosity
        aRow[6]  = new Double (fields[5]);                      // residualMoistureContent
        aRow[12] = new Double (fields[6]);                      // alpha
        aRow[13] = new Double (fields[7]);                      // beta

        return i;
    }

    /** *************************************************************************************
     */ /********************************************************************************* */
    void assignMaterialColors (int mats, int previousRows) {
	int i, cinc, cpos;

        cinc = 255 / (Math.max ((mats - 1), 1));
        cpos = 0;
        for (i=0; i<mats;i++) {
            Object [] aRow = getRow (previousRows + i);

            aRow[1]  = spectrumColor (cpos);                        // color
            cpos += cinc;
        }
    }

    /** *************************************************************************************
     */ /********************************************************************************* */
    int readHeatAndConcData (Vector data, int pos, boolean heat, boolean conc, Object aRow[]) throws IOException {

        boolean  linear, freundlich, langmuir, valent;
        int      i;
	String[] fields = new String[6];

	i = 0;
        if (heat || conc) {
	    fields   = split ((String) data.elementAt (pos + i++), 2);
            aRow[21] = new Double (fields[0]);              // alpha-L
            aRow[22] = new Double (fields[1]);              // alpha-T
            if (heat) {
	        fields   = split ((String) data.elementAt (pos + i++), 4);
                aRow[34] = new Double (fields[0]);          // Cs
                aRow[35] = new Double (fields[1]);          // KTr
                aRow[36] = new Double (fields[2]);          // KTs
                aRow[37] = new Double (fields[3]);          // Cw
            }
            if (conc) {
	        fields     = split ((String) data.elementAt (pos + i++), 7);
                aRow[23]   = new Double (fields[0]);          // molecular diff.
                aRow[24]   = new Double (fields[1]);          // decay
                aRow[25]   = new Double (fields[2]);          // bulk density of solid phase

		linear     = (Integer.parseInt (fields[3]) == 0) ? false : true;
		langmuir   = (Integer.parseInt (fields[4]) == 0) ? false : true;
		freundlich = (Integer.parseInt (fields[5]) == 0) ? false : true;
		valent     = (Integer.parseInt (fields[6]) == 0) ? false : true;

		if (linear) {
	            fields   = split ((String) data.elementAt (pos + i++), 1);
                    aRow[26] = new Double (fields[0]);
		}
		if (langmuir) {
	            fields   = split ((String) data.elementAt (pos + i++), 2);
                    aRow[27] = new Double (fields[0]);
                    aRow[28] = new Double (fields[1]);
		}
		if (freundlich) {
	            fields   = split ((String) data.elementAt (pos + i++), 2);
                    aRow[29] = new Double (fields[0]);
                    aRow[30] = new Double (fields[1]);
		}
		if (valent) {
	            fields   = split ((String) data.elementAt (pos + i++), 3);
                    aRow[31] = new Double (fields[0]);
                    aRow[32] = new Double (fields[1]);
                    aRow[33] = new Double (fields[2]);
		}
            }
        }

	return i;
    }

    /** *************************************************************************************
     */ /********************************************************************************* */
    Color spectrumColor (int pos) {
        int r, g, b;

	if (pos < 64) {
	    r = 0;
            g = (256 * (pos + 1)) / 64 - 1;
            b = 255;
        }
	else if (pos < 128) {
	    r = 0;
            g = 255;
            b = 255 - ((256 * ((pos - 64) + 1)) / 64 - 1);
        }
	else if (pos < 192) {
            r = (256 * ((pos - 128) + 1)) / 64 - 1;
            g = 255;
	    b = 0;
        }
	else {
            r = 255;
            g = 255 - ((256 * ((pos - 192) + 1)) / 64 - 1);
	    b = 0;
        }

	return (new Color (r, g, b));
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public void saveFileData (String file, int soilModel) {

	int i, j;

	try {
            File             outFile = new File (file);
            FileOutputStream fos     = new FileOutputStream (outFile);
            PrintWriter      pw      = new PrintWriter (fos, true);

            switch (soilModel)
                {
            case BROOKS_COREY:  pw.println ("BC+HC");  break;
            case HAVERKAMP:     pw.println ("HK+HC");  break;
            case TABULAR_DATA:  pw.println ("TD+HC");  break;
            case VAN_GENUCHTEN: pw.println ("VG+HC");  break;
            case ROSSI_NIMMO:   pw.println ("RN+HC");  break;
            default:            return;
                }

	    // Loop over all materials - don't save first "inactive" textural class
	    for (i=1; i<getNumberOfRows (); i++) {
		Object aRow[] = getRow (i);

		pw.println ("# ---- Textural Class #" + i + " ------------------------------------");
		switch (soilModel)
		    {
		case BROOKS_COREY:  saveBrooksCoreyData  (pw, aRow);  break;
		case HAVERKAMP:     saveHaverkampData    (pw, aRow);  break;
		case TABULAR_DATA:  saveTabularDataData  (pw, aRow);  break;
		case VAN_GENUCHTEN: saveVanGenuchtenData (pw, aRow);  break;
		case ROSSI_NIMMO:   saveRossiNimmoData   (pw, aRow);  break;
		    }

		saveHeatAndConcData (pw, aRow);
	    }

	    fos.close();
        } catch (IOException e) {
            mp2MessageBox.showMessageDialog ("Error: There was a problem saving " + file + ".  ",
                                             "I/O Exception Error");
	}
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public void saveBrooksCoreyData (PrintWriter pw, Object aRow[]) throws IOException {

        pw.print   ("\"" + aRow[2]  + "\" ");
        pw.print   (aRow[3]  + " ");
        pw.print   (aRow[8]  + " ");
        pw.print   (aRow[4]  + " ");
        pw.print   (aRow[5]  + " ");
        pw.print   (aRow[6]  + " ");
        pw.print   (aRow[6]  + " ");
        pw.print   (aRow[9]  + " ");
        pw.println (aRow[10]);
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public void saveHaverkampData (PrintWriter pw, Object aRow[]) throws IOException {

        pw.print   ("\"" + aRow[2]  + "\" ");
        pw.print   (aRow[3]  + " ");
        pw.print   (aRow[14]  + " ");
        pw.print   (aRow[4]  + " ");
        pw.print   (aRow[5]  + " ");
        pw.print   (aRow[6]  + " ");
        pw.print   (aRow[15]  + " ");
        pw.print   (aRow[16]  + " ");
        pw.print   (aRow[17]  + " ");
        pw.println (aRow[18]);
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public void saveTabularDataData (PrintWriter pw, Object aRow[]) throws IOException {

        int    j, k;
	Vector hkmData = (Vector) aRow[20];

        pw.print   ("\"" + aRow[2]  + "\" ");
        pw.print   (aRow[3]  + " ");
        pw.print   (aRow[19]  + " ");
        pw.print   (aRow[4]  + " ");
        pw.println (aRow[5]);

        pw.println (hkmData.size());
        for (k=0; k<hkmData.size(); k++) {
            Object [] hkm = (Object []) hkmData.elementAt(k);
            for (j=0; j<3; j++)
                pw.print (((Double) hkm[j]).doubleValue() + " ");
	    pw.println ("");
	}
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public void saveVanGenuchtenData (PrintWriter pw, Object aRow[]) throws IOException {

        pw.print   ("\"" + aRow[2]  + "\" ");
        pw.print   (aRow[3]  + " ");
        pw.print   (aRow[11] + " ");
        pw.print   (aRow[4]  + " ");
        pw.print   (aRow[5]  + " ");
        pw.print   (aRow[6]  + " ");
        pw.print   (aRow[12] + " ");
        pw.println (aRow[13]);
    }

    /** ***********************************************************************
     */ /*********************************************************************/
    public void saveRossiNimmoData (PrintWriter pw, Object aRow[]) throws IOException {

        pw.print   ("\"" + aRow[2]  + "\" ");
        pw.print   (aRow[3]  + " ");
        pw.print   (aRow[38] + " ");
        pw.print   (aRow[4]  + " ");
        pw.print   (aRow[5]  + " ");
        pw.print   (aRow[39]  + " ");
        pw.print   (aRow[40] + " ");
        pw.println (aRow[41]);
    }

    /** *************************************************************************************
     */ /********************************************************************************* */
    public void saveHeatAndConcData (PrintWriter pw, Object aRow[]) throws IOException {

        boolean  linear, freundlich, langmuir, valent;
        int      i;
	String[] fields = new String[6];

	pw.print   (aRow[21] + " ");		// alpha-L
        pw.println (aRow[22]);			// alpha-T

	pw.print   (aRow[34] + " ");  		// Heat Cs
	pw.print   (aRow[35] + " ");  		// Heat KTr
	pw.print   (aRow[36] + " ");  		// Heat KTs
        pw.println (aRow[37]);        		// Heat Cw

	pw.print   (aRow[23] + " ");		// molecular diff.
	pw.print   (aRow[24] + " ");            // decay
	pw.print   (aRow[25] + " ");		// bulk density of solid phase
        pw.println ("1 1 1 1");			// linear, langmuir, freundlich, valent

	pw.println (aRow[26]);			// linear

	pw.print   (aRow[27] + " ");		// langmuir
	pw.println (aRow[28]);			// langmuir

	pw.print   (aRow[29] + " ");		// freundlich
	pw.println (aRow[30]);			// freundlich

	pw.print   (aRow[31] + " ");		// valent
	pw.print   (aRow[32] + " ");		// valent
	pw.println (aRow[33]);			// valent
    }

    /***************************************************************************************/
    /*** MISC FILE / STRING TOOLS WHICH SHOULD TO PUT SOMEWHERE MORE GENERAL ***************/
    /***************************************************************************************/

    /** *************************************************************************************
     * Parses an input line from an preparation file
     */ /********************************************************************************* */
    public int getFirstWhiteSpace (String line) {

        int pos, posSpace, posTab;

        // KEEP SEGMENTS IN QUOTES TOGETHER
        pos = line.indexOf ("\"");
        if (pos > -1)
            pos = line.indexOf ("\"", ++pos);
        if (pos == -1) pos = 0;

        posSpace = line.indexOf (" ", pos);
        posTab   = line.indexOf ("\t", pos);
        if ((posSpace > -1) && (posTab > -1)) {
            pos = Math.min (posSpace, posTab);
        }
        else {
            pos = Math.max (posSpace, posTab);
        }

        return (pos);
    }

    /** *************************************************************************************
     * Parses an input line from an preparation file
     */ /********************************************************************************* */
    public String[] split (String line, int size) throws IOException {

        int      i;
        int      pos, posSpace, posTab;
        String[] fields = new String[size];

        size--;
        for (i=0; i<size; i++) {
            pos = getFirstWhiteSpace (line);

            if (pos != -1) {
                fields[i] = new String (line.substring (0, pos));
                line      = line.substring (pos);
                line      = line.trim ();
            }
            else {
                throw new IOException ();
            }
        }
        fields[size] = line.trim ();

        return fields;
    }

    /** *************************************************************************************
     */ /********************************************************************************* */
    public String trimQuotes (String label) {

        int pos;

        if (label.startsWith ("\""))
            label = label.substring (1);
        if (label.endsWith ("\""))
            label = label.substring (0, label.length () - 1);

        return label;
    }
}

/*******************************************************************************************/
class vs2TexturalClassFileData {

    protected boolean heat, conc;
    protected Vector  data = new Vector (5);

    protected vs2TexturalClassFileData (String file, String abrev, String name)
		throws FileNotFoundException, IOException {

        boolean first;
	String  line;

	heat = false;
        conc = false;

        BufferedReader in  = new BufferedReader (new FileReader (file));
        first = true;
        do {
            line = in.readLine ();
            if ((line != null) && !(line.startsWith ("#")) && (line.length () > 0)) {
                if (first) {
                    if (line.equals (abrev + "+H"))
                        heat = true;
                    else if (line.equals (abrev + "+C"))
                        conc = true;
                    else if (line.equals (abrev + "+HC")) {
                        conc = true;
                        heat = true;
                    }
                    else if (!line.equals (abrev)) {
                        String label = new String (file + " not a valid " + name + " library file.");
                        mp2MessageBox.showMessageDialog (label, "File Error");
                        return;
                    }
                    first = false;
                }
                else
                    data.addElement (line);
            }
        } while (line != null);
        in.close ();
    }
}
