/*
 * vs2ChemistryMapData.java
 */
package vs2;

import mp2.*;
import java.io.*;
import static vs2.vs2Constants.*;

public class vs2ChemistryMapData extends mp2ColorCodedMapData implements Serializable {

    static final long serialVersionUID = -8567766856406075333L;
    
    static final int NUMBER_OF_ARRAYS = 7;
    
    
    /**
     * The discretized form of the zones.
     */
    protected transient int[][] zoneArrays;
    
    /**
     * Index to the active zoneArray
     */
    protected transient int currentIndex;
    
    /**
     * Constructor
     */
    public vs2ChemistryMapData() {
        currentIndex = 0;
    }
    
    public int getCurrentIndex() {
        return currentIndex;        
    }    
    
    public void setCurrentIndex(int index) {
        currentIndex = index;        
    }
    
    /**
     * Discretize the zones
     */
    public boolean discretize() {

        if (!(gridData instanceof mp2RectilinearGridData)) {
            return false;
        }
        
        // exportData needs zoneArrays to allocated
        if (zoneArrays != null) {
            // Don't do this if neither the grid nor the data (zones) 
            // have changed since the last discretization.
            if ((!activeCellsHaveChanged) && (!dataHaveChanged)) {
                zoneArray = zoneArrays[currentIndex];
                return false;
            }
        }

        // Get the active cell array and the grid line coordinates 
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int [] active = rectGridData.getActiveCellArray();
        double [] xCoord = rectGridData.getXCoords();
        double [] yCoord = rectGridData.getYCoords();
        int numCol = xCoord.length - 1;
        int numRow = yCoord.length - 1;
        int i, j, k, index;

        // Compute the x and y coordinates of the cell centers.
        double [] xCenter = new double[numCol];
        double [] yCenter = new double[numRow];
        for (i=0; i<numCol; i++) {
            xCenter[i] = (xCoord[i] + xCoord[i+1])/2;
        }
        for (i=0; i<numRow; i++) {
            yCenter[i] = (yCoord[i] + yCoord[i+1])/2;
        }

        // Create the zone array and initially set all cells
        // to the default type (id 0).
        zoneArrays = new int[NUMBER_OF_ARRAYS][numCol*numRow];
        for (int n=0; n < NUMBER_OF_ARRAYS; ++n) {
            for (i=0; i<numCol*numRow; i++) {
                zoneArrays[n][i] = 0;
            }
        }
        
        
        vs2ChemistryClassData chemistryClassData = (vs2ChemistryClassData)doc.getData(CHEMISTRY_CLASS);

        // For now, use a simple minded scheme for discretization.
        // Traverse all zones from "back" (most covered)
        // "front" (not covered). For each zone, assign the zone id
        // to those cells whose centers lie inside the zone and
        // inside the domain boundary. When this has be done from 
        // backmost zone to frontmost zone, discretization is
        // done. Cells whose centers do not lie in any zone and
        // cells whose centers lie outside the domain are 
        // assigned the default id (0).
        for (k=0; k<shapes.size(); k++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(k);
            int row = chemistryClassData.getRowIndexOfId(shape.getId());
            for (j=0; j<numRow; j++) {
                for (i=0; i<numCol; i++) {
                    index = j*numCol+i;
                    if (active[index] > -2) {
                        if (shape.contains(xCenter[i], yCenter[j])) {
                            for (int n = 0; n < 7; n++) {
                                if (!chemistryClassData.isAsIs(row, n+3)) {
                                    zoneArrays[n][index] = shape.getId();
                                }
                            }
                        }
                    }
                }
            }
        }
        zoneArray = zoneArrays[currentIndex];
        activeCellsHaveChanged = false;
        dataHaveChanged = false;
        return true;
    }    

    /**
     * Exports data to VS2DT model input file
     */
    public void exportData(PrintWriter pw, 
                   vs2ChemistryClassData chemistryClassData) {
        discretize();
        int c, r, index;
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int numColNoBorder = rectGridData.getXCoords().length - 1;
        int numRowNoBorder = rectGridData.getYCoords().length - 1;
        pw.println("1" + "     /B28 -- IREAD. B30 begins next line: INDSOL");
        // Note that Card B-29 is not used.
        for (int n = 0; n < 7; ++n) {
            // Card B-30
            // first, write the top border
            for (c=0; c<numColNoBorder+2; c++) {
                pw.print("-1 ");
            }
            pw.println();
            // next, write the interior cells plus left and right borders
            for (r=0; r<numRowNoBorder; r++) {
                pw.print("-1 ");   // left border cell
                for (c=0; c<numColNoBorder; c++) {
                    index = r*numColNoBorder+c;
                    int row = chemistryClassData.getRowIndexOfId(zoneArrays[n][index]);
                    Integer val = (Integer)chemistryClassData.getObjectAt(row, n+3);
                    pw.print(val.intValue() + " ");
                }
                pw.println("-1");   // right border cell
            }
            // finally, write the bottom edge
            for (c=0; c<numColNoBorder+2; c++) {
                pw.print("-1 ");
            }
            pw.println();
        }
    }
}