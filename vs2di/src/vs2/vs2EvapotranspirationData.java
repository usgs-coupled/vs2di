/*
 * vs2EvapotranspirationData.java
 */
package vs2;

import mp2.*;
import java.io.*;
import java.util.*;

/**
 * Holds evapotranspiration data in a table
 */
public class vs2EvapotranspirationData extends mp2TableData
                implements Serializable {

    static final long serialVersionUID = 929345942203869983L;

    protected double periodLength;

    protected static final String [] COLUMN_NAME = {
        // Common
        "Period",

        // Evaporation parameters
        "PEVAL", "SRES", "HA",

        // Transpiration parameters
        "PTVAL", "RD", "RA base", "RA top", "HROOT"
    };

    protected static final Class [] COLUMN_CLASS = {
        Integer.class,
        Double.class, Double.class, Double.class,
        Double.class, Double.class, Double.class, Double.class, Double.class
        };

        protected static final String [] TOOL_TIP_TEXT = {
        // Common
        "ET period number",

        // Evaporation parameters
        "potential evaporation rate",
        "surface resistance to evaporation",
        "pressure potential of atmosphere",

        // Transpiration parameters
        "potential evapotranspiration rate",
        "root depth",
        "root activity at base of root zone",
        "root activity at top of root zone",
        "pressure head in roots"
    };

    /**
    * Constructor
    */
    public vs2EvapotranspirationData() {
        super ();
        periodLength = 1;
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
    * Create a default row and return it
    */
    public Object [] createDefaultRow() {
        Object [] aRow = new Object[COLUMN_NAME.length];

        // common
        aRow[0] = null;                 // Not used but needed for spacing

        // evaporation parameters
        aRow[1] = new Double(0.0);      // potential evaporation rate (PEV)
        aRow[2] = new Double(0.0);      // surface resistance to evaporation (SRES)
        aRow[3] = new Double(0.0);      // pressure potential of atmosphere (HA)

        // evapotranspiration parameters
        aRow[4] = new Double(0.0);      // potential evapotranspiration rate (PET)
        aRow[5] = new Double(0.0);      // root depth
        aRow[6] = new Double(0.0);      // root activity at base of root zone
        aRow[7] = new Double(0.0);      // root activity at top of root zone
        aRow[8] = new Double(0.0);      // pressure head in roots

        return aRow;
    }

    public void exportData(PrintWriter pw, boolean doEvaporation,
                      boolean doTranspiration) {
        String s;
        final int commentOffset = 23;
        Object [] aRow;
        // Card B-19
        s = String.valueOf(dataRows.size() + " " + periodLength);
        pw.println(s + vs2App.tab(s, commentOffset)
                + "/B-19 -- NPV, ETCYC");
        
        // Cards B-20 through B-22
        if (doEvaporation) {
            s = "";
            for (int j=1; j<=3; j++) {
                for (int i=0; i<dataRows.size(); i++) {
                    aRow = (Object []) dataRows.elementAt(i);
                    s += String.valueOf(((Double) aRow[j]).doubleValue() + " ");
                }
                if (j==1) pw.println(s + vs2App.tab(s, commentOffset) + "/B-20 -- PEVAL");
                if (j==2) pw.println(s + vs2App.tab(s, commentOffset) + "/B-21 -- RDC(1,J)");
                if (j==3) pw.println(s + vs2App.tab(s, commentOffset) + "/B-22 -- RDC(2,J)");
                s = "";
            }
        }
        // Cards B-23 through B-27
        if (doTranspiration) {
            s = "";
            for (int j=4; j<=8; j++) {
                for (int i=0; i<dataRows.size(); i++) {
                    aRow = (Object []) dataRows.elementAt(i);
                    s += String.valueOf(((Double) aRow[j]).doubleValue() + " ");
                }
                if (j==4) pw.println(s + vs2App.tab(s, commentOffset) + "/B-23 -- PTVAL");
                if (j==5) pw.println(s + vs2App.tab(s, commentOffset) + "/B-24 -- RDC(3,J)");
                if (j==6) pw.println(s + vs2App.tab(s, commentOffset) + "/B-25 -- RDC(4,J)");
                if (j==7) pw.println(s + vs2App.tab(s, commentOffset) + "/B-26 -- RDC(5,J)");
                if (j==8) pw.println(s + vs2App.tab(s, commentOffset) + "/B-27 -- RDC(6,J)");
                s = "";
            }
        }
    }

    /**
     * Return the row number for the first column of the data set.
     * Row counting starts from 1.
     */
    public Object getObjectAt (int r, int c) {
        if (c == 0) {
            return (new Integer(r+1));
        } else {
            return super.getObjectAt(r, c);
        }
    }

    /**
     * Gets the period length
     */
    public double getPeriodLength() {
        return periodLength;
    }

    /**
     * Sets the period length
     */
    public void setPeriodLength(double len) {
        periodLength = len;
    }

    public void loadDataFromFile (String file) {
		int n = 0;
		try {
			BufferedReader in  = new BufferedReader (new FileReader (file));
			Vector newDataRows = new Vector();
			String  aLine;
			String delims = "[\\s]+";
			int p1, p2;
			while (true) {
				n++;
				aLine = in.readLine ();
				if (aLine == null) break;
				String[] tokens = aLine.trim().split(delims);
				if (tokens[0].length() == 0) {
					break;
				} else if (tokens.length < 8) {
            		mp2MessageBox.showMessageDialog ("Error encountered while reading line " + n + ".  Please Check the data file.",
                                             "Error" + tokens[0] + ".");
                    in.close();
					return;
				} else {
					Object [] aRow = new Object[COLUMN_NAME.length];
					aRow[0] = null;
					for (int i=0; i<8; i++) {
						aRow[i+1] = Double.valueOf(tokens[i]);
					}
					newDataRows.addElement(aRow);
				}
			}
			in.close ();
			if (newDataRows.size() > 0) {
				setData(newDataRows);
			} else {
            	mp2MessageBox.showMessageDialog ("Error: File is either empty or does not contain evapotranspiration data.",
                                             "Error");
			}
        } catch (FileNotFoundException e) {
            mp2MessageBox.showMessageDialog ("Error: Could not find " + file + ".", "File Not Found!");
        } catch (IOException e) {
            mp2MessageBox.showMessageDialog ("Error: Line format problem, check data file syntax.",
                                             "I/O Exception Error");
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog ("Error encountered while reading line " + n + ". Please Check the data file.",
                                             "Error");
        }

	}

    public void saveDataToFile (String file) {
		int i, j;

		try {
			File             outFile = new File (file);
			FileOutputStream fos     = new FileOutputStream (outFile);
			PrintWriter      pw      = new PrintWriter (fos, true);

			for (i=0; i<getNumberOfRows (); i++) {
				Object aRow[] = getRow (i);
				for (j=1; j<8; j++) {
        			pw.print(aRow[j]  + " ");
				}
				pw.println(aRow[8]);
			}

			fos.close();
			} catch (IOException e) {
				mp2MessageBox.showMessageDialog ("Error: There was a problem saving " + file + ".  ",
												 "I/O Exception Error");
		}

	}

}
