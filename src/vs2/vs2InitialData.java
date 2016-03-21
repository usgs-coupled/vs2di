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

        // If there is only one contour, then the initial condition is
        // a uniform value
        if (shapes.size() == 1) {
            mp2Shape aContour = (mp2Shape) shapes.firstElement();
            pw.println("0 " + (float) aContour.getValue()
                    + "     /" + label1 + " -- IREAD, FACTOR");
            return;
        }

        discretize();

        int c, r, i;
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int numColNoBorder = rectGridData.getXCoords().length - 1;
        int numRowNoBorder = rectGridData.getYCoords().length - 1;
        // Card B-24  Always read ic from file, with mult factor set to 1.
        pw.println("1 1.0" + "     /" + label1 + " -- IREAD, FACTOR");
        // Card B-25   Assume unit number is 5, use free format
        pw.println("5 'free'" + "     /" + label2 + " -- IU, IFMT. Initial values to follow.");
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
