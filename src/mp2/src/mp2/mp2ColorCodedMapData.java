/*
 * mp2ColorCodedMapData.java
 */
package mp2;

import java.awt.*;
import java.io.*;
import java.util.Vector;

/**
 * Holds the data for a color coded map. This class consists of
 * a collection of "zones" each being a mp2Shape object. 
 * Each zone has a unique id that distinguishes 
 * it from all other zones. The discretized form
 * the data is a zone array.
 *
 * <P>This class implements the Serializable interface so
 * objects can be serialized and deserialized. Therefore,
 * object creation is a two-step procedure in which
 * the <code>init</code> method is called after construction
 * or deserialization. This is similar to how document objects
 * are created.</P>
 *
 * @see mp2.mp2Doc
 */
public class mp2ColorCodedMapData extends mp2ShapesData 
        implements Serializable {

    static final long serialVersionUID = 6034582131743420042L;

    /**
     * The discretized form of the zones.
     */
    protected transient int [] zoneArray;

    /**
     * Constructor
     */
    public mp2ColorCodedMapData() {
        super();
    }

    /**
     * Discretize the zones
     */
    public boolean discretize() {

        if (!(gridData instanceof mp2RectilinearGridData)) {
            return false;
        }

        // Don't do this if neither the grid nor the data (zones) 
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
        zoneArray = new int[numCol*numRow];
        for (i=0; i<numCol*numRow; i++) {
            zoneArray[i] = 0;
        }

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
            for (j=0; j<numRow; j++) {
                for (i=0; i<numCol; i++) {
                    index = j*numCol+i;
                    if (shape.contains(xCenter[i], yCenter[j]) &&
                            active[index] > -2) {
                        zoneArray[index] = shape.getId();
                    }
                }
            }
        }
        activeCellsHaveChanged = false;
        dataHaveChanged = false;
        return true;
    }

    /**
     * Gets the zone array, which is the discretized form
     * of the zones.
     */
    public int [] getZoneArray() {
        return zoneArray;
    }

    /**
     * Initializes this object
     */
    public void init(mp2Doc doc) {
        super.init(doc);
        zoneArray = null;
    }
}
