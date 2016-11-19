/*
 * vs2InitialData.java
 */
package vs2;

import mp2.*;
import java.io.*;

public class vs2InitialData extends mp2ContourMapData 
        implements Serializable {

    static final long serialVersionUID = 2058370151363540275L;

    protected transient double nullValue = 0;

    public void setNullValue(double nullValue) {
        this.nullValue = nullValue;
    }

    public void exportData(PrintWriter pw, String label1, String label2) {
        exportData(pw, label1, label2, false);
    }    

    public void exportData(PrintWriter pw, String label1, String label2, boolean skipIUandIFMT) {

        String s;
        final int commentOffset = 23;
        
        // If there is only one contour, then the initial condition is
        // a uniform value
        if (shapes.size() == 1) {
            mp2Shape aContour = (mp2Shape) shapes.firstElement();
            s = String.valueOf("0 " + (float) aContour.getValue());
            pw.println(s + vs2App.tab(s, commentOffset)
                    + "/" + label1 + " -- IREAD, FACTOR");
            return;
        }

        discretize();

        int c, r, i;
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int numColNoBorder = rectGridData.getXCoords().length - 1;
        int numRowNoBorder = rectGridData.getYCoords().length - 1;
        // Card B-XX  Always read ic from file, with mult factor set to 1.
        // XX=15 for initial pressure head and intial moisture content
        // XX=28 for initial temperature
        // XX=30 for phreeqc initial chemistry
        if (skipIUandIFMT) {
            s = String.valueOf("1 1.0");
            pw.println(s + vs2App.tab(s, commentOffset)
                    + "/" + label1 + " -- IREAD, FACTOR. Initial values to follow.");
        } else {
            s = String.valueOf("1 1.0");
            pw.println(s + vs2App.tab(s, commentOffset)
                    + "/" + label1 + " -- IREAD, FACTOR");
        }
        // Card B-XX  Assume unit number is 5, use free format
        // XX=17 initial head or moisture content
        // XX=29 initial temperature
        // for solute transport IU, IFMT isn't read
        if (!skipIUandIFMT) {
            s = String.valueOf("5 'free'");
            pw.println(s + vs2App.tab(s, commentOffset)
                    + "/" + label2 + " -- IU, IFMT. Initial values to follow.");
        }
        // first, write the concentration of cells on top border
        for (c=0; c<numColNoBorder+2; c++) {
            pw.print(nullValue + " ");
            if ((c+1)%20 == 0) {
                pw.println();
            }
        }
        if ((numColNoBorder+2)%20 != 0) {
            pw.println();
        }
        // next, write the interior cells plus left and right borders
        i = 0;
        for (r=0; r<numRowNoBorder; r++) {
            pw.print(nullValue + " ");   // left border cell
            for (c=0; c<numColNoBorder; c++, i++) {
                if (valueArray[i] == Double.NEGATIVE_INFINITY) {
                    pw.print(nullValue + " ");
                } else {
                    pw.print(((float) valueArray[i]) + " ");
                }
                if (((c+2)%20) == 0) {
                    pw.println();
                }
            }
            pw.println(nullValue);   // right border cell
        }
        // finally, write the bottom edge
        for (c=0; c<numColNoBorder+2; c++) {
            pw.print(nullValue + " ");
            if ((c+1)%20 == 0) {
                pw.println();
            }
        }
        if ((numColNoBorder+2)%20 != 0) {
            pw.println();
        }
    }
}
