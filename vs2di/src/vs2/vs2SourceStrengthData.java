/*
 * vs2SourceStrengthData.java
 */
package vs2;

import mp2.*;
import java.io.*;

public class vs2SourceStrengthData extends mp2TableData 
            implements vs2Constants, Serializable {
    
    static final long serialVersionUID = -4127687646305316051L;
    
    protected static final String [] COLUMN_NAME = {"Period No.", "Variable", "Value", "Variable", "Value", "Variable", "Value"};

    protected static final Class [] COLUMN_CLASS = {Integer.class, String.class, Double.class, String.class, Double.class, String.class, Double.class};

    protected static final String [] TOOL_TIP_TEXT = {
        "recharge period number",
        "The type of hydraulic variable specified for this source/sink point",
        "The value of the hydraulic variable",
        "The type of concentration variable, Ci or C, specified for this source/sink point",
        "The value of the concentration variable",
        "The type of temperature variable, Ti or T, specified for this source/sink point",
        "The value of the temperature variable"};

    protected void setColumnAttributes() {
        columnName = COLUMN_NAME;
        columnClass = COLUMN_CLASS;
        toolTipText = TOOL_TIP_TEXT;
    }

    public Object [] createDefaultRow() {
        Object [] aRow = new Object[COLUMN_NAME.length];
        aRow[0] = null;                               // Not used but needed for spacing
        aRow[1] = new Integer(NORMAL_FLUID_FLUX_BC);  // The type of hydraulic variable specified for this source.
        aRow[2] = new Double(0.0);                    // The value of the hydraulic variable
        aRow[3] = new Integer(DEFAULT_CONC_BC);       // The type of concentration variable, Ci or C, specified for this source
        aRow[4] = new Double(0.0);                    // The value of the concentration variable
        aRow[5] = new Integer(DEFAULT_CONC_BC);       // The type of temperature variable, Ti or T, specified for this source
        aRow[6] = new Double(0.0);                    // The value of the temperature variable
        return aRow;
    }

    public Object getObjectAt (int r, int c) {
        switch (c) {
        case 0:
            return (new Integer(r+1));
        case 1:
            switch (((Integer)super.getObjectAt(r, c)).intValue()) {
            case NORMAL_FLUID_FLUX_BC:   
                return "Flow";
            case PRESSURE_HEAD_BC:
                return "Press. Head";
            case TOTAL_HEAD_BC:
                return "Total Head";
            default:
                return "unknown";
            }
        case 3:
            switch (((Integer)super.getObjectAt(r, c)).intValue()) {
            case DEFAULT_CONC_BC:   
                return "Ci";
            case SPECIFIED_CONC_BC:
                return "C";
            case DIFFUSIVE_FLUX_BC:
                return "M";
            default:
                return "unknown";
            }
        case 5:
            switch (((Integer)super.getObjectAt(r, c)).intValue()) {
            case DEFAULT_CONC_BC:   
                return "Ti";
            case SPECIFIED_CONC_BC:
                return "T";
            case DIFFUSIVE_FLUX_BC:
                return "H";
            default:
                return "unknown";
            }
        default:
            return super.getObjectAt(r, c);
        }
    }

    public void init(mp2Doc doc) {
        super.init(doc);
        clearUndoOnDelete = true;
        clearUndoOnAdd = true;
    }
    
    public void insertPeriodAt(int p) {
        dataRows.insertElementAt(createDefaultRow(), p);
    }
}
