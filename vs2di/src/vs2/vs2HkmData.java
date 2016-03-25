/*
 * vs2HkmData.java
 */
package vs2;

import mp2.*;
import java.util.Vector;

/**
 * Hold pressure head, relative hydraulic conductivity,
 * and moisture content data.
 */
public class vs2HkmData extends mp2TableData {

    protected static final String [] COLUMN_NAME = {
            "Pressure Head", 
            "Relative K", 
            "Moisture Cont."};

    protected static final Class [] COLUMN_CLASS = {
            Double.class, 
            Double.class, 
            Double.class};

    protected static final String [] TOOL_TIP_TEXT = {
            "pressure head",
            "relative hydraulic conductivity",
            "moisture content"};

    /**
     * Constructor
     */
    public vs2HkmData() {        
        super ();
    }

    /**
     * Create a default row and return it
     */
    public Object [] createDefaultRow() {
        Object [] aRow = new Object[COLUMN_NAME.length];

        aRow[0] = new Double(0);      // pressure head
        aRow[1] = new Double(0);      // relative hydraulic conductivity
        aRow[2] = new Double(0);      // moisture content

        return aRow;
    }

    /**
     * Implementation of abstract method in superclass
     * Set the column name, column class, and tool tip text.
     */
    protected void setColumnAttributes() {
        columnName = COLUMN_NAME;
        columnClass = COLUMN_CLASS;
        toolTipText = TOOL_TIP_TEXT;
    }
}
