/*
 * vs2ObservationPointsData.java
 */
package vs2;

import mp2.*;
import java.io.*;

public class vs2ObservationPointsData extends mp2ObservationPointsData 
                                        implements Serializable {
    static final long serialVersionUID = -2733415543650572830L;
        
    /**
     * Exports data to VS2DT model input file
     */
    public void exportData(PrintWriter pw, boolean outputToAuxFilesEveryTimeStep) {
        discretize();

        if (observationCellIndices == null ||
            observationCellIndices.size() == 0) {
            return;
        }

        // Card A-15
        int nobs = observationCellIndices.size();
        if (!outputToAuxFilesEveryTimeStep) {
            pw.print("-");
        }
        pw.println(nobs + "     /A15 -- NOBS. A16 begines next line: J, N");

        // Card A-16
        int i, index, row, col;
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int numCol = rectGridData.getXCoords().length - 1;
        int numRow = rectGridData.getYCoords().length - 1;
        for (i=0; i<observationCellIndices.size(); i++) {
            index = ((Integer) observationCellIndices.elementAt(i)).intValue();
            row = index/numCol;
            col = index - row*numCol;
            pw.println((row+2) + " " + (col+2)); // add one for border cell and one for start index counting form 1.
        }
    }
}