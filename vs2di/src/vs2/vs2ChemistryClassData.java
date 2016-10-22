/*
 * vs2ChemistryClassData.java
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
public class vs2ChemistryClassData extends mp2TableData implements
        vs2Constants, Serializable {

    private static final long serialVersionUID = -4353205783802737424L;

    protected int idCounter;
    
    static final int AS_IS_VALUE = -2;

    protected static final String [] COLUMN_NAME = {
        // Common hydraulic parameters  (Note that "Id" is never
        // used by table)
        "Id", "Color", "Name",
        //0      1        2   
        
        // INSOL(7)
        "Solution", "Eq phases", "Exchange", "Surface", "Gas phase", "Solid solns", "Kinetics"
        //  3           4             5          6           7             8             9
    };

    protected static final Class [] COLUMN_CLASS = {
        Integer.class, ImageIcon.class, String.class,
        Integer.class, Integer.class, Integer.class, Integer.class, Integer.class, 
        Integer.class, Integer.class
    };

    protected static final String [] TOOL_TIP_TEXT = {
        // Common hydraulic parameters  (Note that "Id" is never
        // used by table)
        "Id",
        "color code",
        "name of chemistry class",
        
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
    public vs2ChemistryClassData() {
        super ();
        assert(COLUMN_NAME.length == 10);
        assert(COLUMN_NAME.length == COLUMN_CLASS.length);
        assert(COLUMN_NAME.length == TOOL_TIP_TEXT.length);
        idCounter = 0;
        // Start off with a default row in the data set
        Object [] aRow = createDefaultRow();
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
        assert(false);
        Object [] aRow;
        int maxTabDataSize = getMaxTabularDataRows();
        for (int i=0; i<dataRows.size(); i++) {
            aRow = (Object []) dataRows.elementAt(i);
            // Card B-8
            pw.println((i+1) + "     /B8 -- ITEX. B9 to begin next line: HK");
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
            
            // Card B-9A (was B-7A - VS2DH)
            if (modelOptions.doEnergyTransport) {
                pw.println(((Double) aRow[21]).doubleValue() + " " +
                           ((Double) aRow[22]).doubleValue() + " " +
                           ((Double) aRow[34]).doubleValue() + " " +
                           ((Double) aRow[35]).doubleValue() + " " +
                           ((Double) aRow[36]).doubleValue() + " " +
                           ((Double) aRow[37]).doubleValue() + " " + "     /B9A -- HT");
            }
            // Card B-9B (was B-7A - VS2DT)
            if (modelOptions.doSoluteTransport) {
                pw.println(((Double) aRow[21]).doubleValue() + " " +
                           ((Double) aRow[22]).doubleValue() + " " +
                           ((Double) aRow[23]).doubleValue() + " " +
                           ((Integer)aRow[44]).intValue()    + " " +
                           ((Integer)aRow[45]).intValue()    + " " +
                           ((Integer)aRow[46]).intValue()    + " " +
                           ((Integer)aRow[47]).intValue()    + " " +
                           ((Integer)aRow[48]).intValue()    + " " +
                           ((Integer)aRow[49]).intValue()    + " " +
                           ((Integer)aRow[50]).intValue()    + " " + "     /B9B -- HS INSOL");
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
        
        // Chemistry
        aRow[3] = new Integer(-1);        // solution
        aRow[4] = new Integer(-1);        // equilibrium_phases
        aRow[5] = new Integer(-1);        // exchange
        aRow[6] = new Integer(-1);        // surface
        aRow[7] = new Integer(-1);        // gas_phase
        aRow[8] = new Integer(-1);        // solid_solutions
        aRow[9] = new Integer(-1);        // kinetics

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
        } else if (c > 2) {
            int n = ((Integer) super.getObjectAt(r, c)).intValue();
            if (n == AS_IS_VALUE) {
                return "As is";
            }            
            return super.getObjectAt(r, c);
        } else {
            return super.getObjectAt(r, c);
        }
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
        assert(false);
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
    
    
    private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException {
        // unserialize default
        in.defaultReadObject();
    }
    
    private void writeObject(java.io.ObjectOutputStream out) throws IOException {
        out.defaultWriteObject();
    }
    
    public boolean isAsIs(int row, int column) {
        if (column > 2 && column < 10) {
            int n = ((Integer) super.getObjectAt(row, column)).intValue();
            return (n == vs2ChemistryClassData.AS_IS_VALUE);            
        }
        return false;
    }
    
    public static boolean isAsIs(Object obj) {
        if (obj instanceof Integer) {
            Integer i = (Integer)obj;
            if (i.intValue() == AS_IS_VALUE) {
                return true;
            }
        }
        return false;
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
class vs2ChemistryClassFileData {

    protected boolean heat, conc;
    protected Vector  data = new Vector (5);

    protected vs2ChemistryClassFileData (String file, String abrev, String name)
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
