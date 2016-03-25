/*
 * vs2RechargePeriodData.java
 */
package vs2;

import mp2.*;
import java.io.*;

/**
 * Holds the recharge period data in a table
 */
public class vs2RechargePeriodData extends mp2TableData implements Serializable {

    static final long serialVersionUID = -3886246462909542598L;
    
    protected static final String [] COLUMN_NAME = {"No.", "Length", "DELT", 
        "TMLT", "DLTMX", "DLTMIN", "TRED", "DSMAX", "STERR", "Pond", "Print", 
        "Evap", "Plant"};

    protected static final Class [] COLUMN_CLASS = {Integer.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class, 
        Double.class, Double.class, Double.class, Boolean.class, Boolean.class, 
        Boolean.class};

    protected static final String [] TOOL_TIP_TEXT = {
        "recharge period number",
        "length of recharge period",
        "initial time step",
        "time step multiplier",
        "maximum time step",
        "minimum time step",
        "time step reduction factor",
        "maximum head change",
        "steady-state head criterion",
        "maximum height of ponding",
        "print result for each time step",
        "simulate evaporation",
        "simulate plane transpiration"};

    public vs2RechargePeriodData() {
        super ();
    }

    protected void setColumnAttributes() {
        columnName = COLUMN_NAME;
        columnClass = COLUMN_CLASS;
        toolTipText = TOOL_TIP_TEXT;
    }

    public Object [] createDefaultRow() {
        Object [] aRow = new Object[COLUMN_NAME.length];

        aRow[0] = null;                 // Not used but needed for spacing
        aRow[1] = new Double(0.0);      // length of recharge period
        aRow[2] = new Double(0.0);      // initial time step
        aRow[3] = new Double(0.0);      // time step multiplier
        aRow[4] = new Double(0.0);      // maximum time step
        aRow[5] = new Double(0.0);      // minimum time step
        aRow[6] = new Double(0.0);      // time step reduction factor
        aRow[7] = new Double(0.0);      // maximum head change
        aRow[8] = new Double(0.0);      // steady-state head criterion
        aRow[9] = new Double(0.0);      // maximum height of ponding
        aRow[10] = new Boolean(false);    // print result for each time step
        aRow[11] = new Boolean(false);    // simulate evaporation
        aRow[12] = new Boolean(false);    // simulate plant transpiration

        return aRow;
    }

    public Object getObjectAt (int r, int c) {
        if (c == 0) {
            return (new Integer(r+1));
        } else {
            return super.getObjectAt(r, c);
        }
    }

    public boolean isColumnEditable(int c) {
        if (c >= 10 && c <= 12) {
            return true;
        } else {
            return false;
        }
    }

    public double getMaximumSimulationTime() {
        double maxSimTime = 0;
        for (int i=0; i<dataRows.size(); i++) {
            Object [] aRow = (Object []) dataRows.elementAt(i);
            maxSimTime += ((Double) aRow[1]).doubleValue();
        }
        return maxSimTime;
    }

    public void init(mp2Doc doc) {
        super.init(doc);
        clearUndoOnDelete = true;
        clearUndoOnAdd = true;
    }

    public boolean isEvaporationSimulated() {
        for (int i=0; i<dataRows.size(); i++) {
            Object [] aRow = (Object []) dataRows.elementAt(i);
            if (((Boolean) aRow[11]).booleanValue()) {
                return true;
            }
        }
        return false;
    }

    public boolean isTranspirationSimulated() {
        for (int i=0; i<dataRows.size(); i++) {
            Object [] aRow = (Object []) dataRows.elementAt(i);
            if (((Boolean) aRow[12]).booleanValue()) {
                return true;
            }
        }
        return false;
    }

    public void exportPeriod(PrintWriter pw, int i, vs2ModelOptions modelOptions) {
        if (i >= dataRows.size()) {
            return;
        }
        Object [] aRow = (Object []) dataRows.elementAt(i);
        // Card C-1
        pw.println(((Double) aRow[1]).doubleValue() + " "
                + ((Double) aRow[2]).doubleValue()
                + "     /C1 -- TPER, DELT (Recharge Period " + (i+1) + ")");
        // Card C-2
        pw.println(((Double) aRow[3]).doubleValue() + " "
                + ((Double) aRow[4]).doubleValue() + " "
                + ((Double) aRow[5]).doubleValue() + " "
                + ((Double) aRow[6]).doubleValue()
                + "     /C2 -- TMLT, DLTMX, DLTMIN, TRED");
        // Card C-3
        pw.println(((Double) aRow[7]).doubleValue() + " "
                + ((Double) aRow[8]).doubleValue()
                + "     /C3 -- DSMAX, STERR");
        // Card C-4
        pw.println(((Double) aRow[9]).doubleValue()
                + "     /C4 -- POND");
        // Card C-5
                pw.println(((((Boolean) aRow[10]).booleanValue()) ? "T" : "F")
                + "     /C5 -- PRNT");
        // Card C-6
        pw.print(((((Boolean) aRow[11]).booleanValue() && modelOptions.doEvaporation) ? "T " : "F ")
                + ((((Boolean) aRow[12]).booleanValue() && modelOptions.doTranspiration) ? "T " : "F "));
        // rest of line continued in boundary conditions
    }
}
