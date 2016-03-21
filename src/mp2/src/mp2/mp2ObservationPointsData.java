/*
 * mp2ObservationPointsData
 */
package mp2;

import java.awt.*;
import java.io.*;
import java.util.Vector;

public class mp2ObservationPointsData extends mp2ShapesData 
        implements Serializable {

    static final long serialVersionUID = -5122705188670573718L;

    /**
     * The discretized form of the zones.
     */
    protected transient Vector observationCellIndices;

    public boolean discretize() {

        if (!(gridData instanceof mp2RectilinearGridData)) {
            return false;
        }

        // Don't do this if neither the grid nor the data
        // have changed since the last discretization.
        if ((!activeCellsHaveChanged) && (!dataHaveChanged)) {
            return false;
        }

        // Get the active cell array and the grid line coordinates 
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int [] active = rectGridData.getActiveCellArray();
        double [] xCoord = rectGridData.getXCoords();
        double [] yCoord = rectGridData.getYCoords();
        int numCol = xCoord.length - 1;
        int numRow = yCoord.length - 1;

        // Create the observation point array and set all cells 
        // initially to false (no observation point at cell);
        observationCellIndices = new Vector();

        // Find the cell corresponding to each observation point,
        // and set observation point array element to true
        for (int i=0; i<shapes.size(); i++) {
            double [] v = ((mp2Point) shapes.elementAt(i)).getCoord();
            int col = -1;
            int row = -1;
            if (v[0] > xCoord[0] && v[1] > yCoord[0]) {
                for (int j=1; j<xCoord.length && col == -1; j++) {
                    if (v[0] < xCoord[j]) {
                        col = j-1;
                    }
                }
                for (int j=1; j<yCoord.length && row == -1; j++) {
                    if (v[1] < yCoord[j]) {
                        row = j-1;
                    }
                }
            }
            if (col > -1 && row > -1) {
                int index = row*numCol + col;
                if (active[index] > -2) {
                    observationCellIndices.addElement(new Integer(index));
                }
            }
        }
        activeCellsHaveChanged = false;
        dataHaveChanged = false;
        return true;
    }

    public int getNumberOfObservationCells() {
        discretize();
        if (observationCellIndices == null) {
            return 0;
        } else {
            return observationCellIndices.size();
        }
    }

    public Vector getObservationCellIndices() {
        return observationCellIndices;
    }

    /**
     * Initializes this object
     */
    public void init(mp2Doc doc) {
        super.init(doc);
        observationCellIndices = null;
    }
}
