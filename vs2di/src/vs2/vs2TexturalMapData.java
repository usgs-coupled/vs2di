/*
 * vs2TexturalMapData.java
 */
package vs2;

import mp2.*;
import java.io.*;

public class vs2TexturalMapData extends mp2ColorCodedMapData implements Serializable {

    static final long serialVersionUID = -8567766856406075333L;

    /**
     * Exports data to VS2DT model input file
     */
    public void exportData(PrintWriter pw, 
                   vs2TexturalClassData texturalClassData) {
        discretize();
        int c, r, i;
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int numColNoBorder = rectGridData.getXCoords().length - 1;
        int numRowNoBorder = rectGridData.getYCoords().length - 1;
        // Card B-10 (Always read texture class for each row)
        pw.println("0" + "     /B10 -- IROW. B11 begins next line: JTEX");
        // Card B-11
        // first, write the top border
        for (c=0; c<numColNoBorder+2; c++) {
            pw.print("1 ");
        }
        pw.println();
        // next, write the interior cells plus left and right borders
        i = 0;
        for (r=0; r<numRowNoBorder; r++) {
            pw.print("1 ");   // left border cell
            for (c=0; c<numColNoBorder; c++, i++) {
                int texturalClassIndex = 
                    texturalClassData.getRowIndexOfId(zoneArray[i]);
                pw.print((texturalClassIndex+1) + " ");
            }
            pw.println("1");   // right border cell
        }
        // finally, write the bottom edge
        for (c=0; c<numColNoBorder+2; c++) {
            pw.print("1 ");
        }
        pw.println();
        // Note that Card B-12 is not used.
    }
}