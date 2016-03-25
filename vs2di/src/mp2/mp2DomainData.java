/*
 * mp2DomainData.java
 */
package mp2;

import java.awt.*;
import java.io.*;
import java.util.*;

/**
 * Encapsulates the model domain. The domain is defined
 * by its external and internal boundaries. 
 * The discretized form is represented by a 
 * collection of line segments that runs along the grid
 * lines, and thus making 90 degree turns.
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
public class mp2DomainData extends mp2ShapesData 
        implements Serializable {

    static final long serialVersionUID = -1778388243800084433L;

    /**
     * Array of line segments that make up the discretized form
     * of the domain
     */
    protected transient Vector discretizedDomain;

    /**
     * Creates a data object to hold domain boundaries.
     */
    public mp2DomainData() {
        super();
    }

    /**
     * Deselects all the boundaries in the domain
     */
    public void deselectAllBoundaries() {
        deselectAllShapes();
    }

    /**
     * Discretizes the domain. This is done by sweeping each
     * column and each row to find boundaries between active and
     * inactive cells.
     */
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
        int i, j, k;
        double [] v;

        // Create the discretized domain. First sweep in the row 
        // direction
        discretizedDomain = new Vector();
        boolean findLeft;
        for (j=0; j<numRow; j++) {
            findLeft = true;
            for (i=0; i<numCol; i++) {
                k = j*numCol + i;
                if ((active[k] > -2 && findLeft) || ((active[k] == -2) && (!findLeft))) {
                    v = new double[2];
                    v[0] = xCoord[i];
                    v[1] = yCoord[j];
                    discretizedDomain.addElement(v);
                    v = new double[2];
                    v[0] = xCoord[i];
                    v[1] = yCoord[j+1];
                    discretizedDomain.addElement(v);
                    findLeft = !findLeft;
                }
                if (active[k] > -2 && (i == numCol-1)) {
                    v = new double[2];
                    v[0] = xCoord[i+1];
                    v[1] = yCoord[j];
                    discretizedDomain.addElement(v);
                    v = new double[2];
                    v[0] = xCoord[i+1];
                    v[1] = yCoord[j+1];
                    discretizedDomain.addElement(v);
                }
            }
        }
        // Then sweep in the column direction
        boolean findTop;
        for (i=0; i<numCol; i++) {
            findTop = true;
            for (j=0; j<numRow; j++) {
                k = j*numCol + i;
                if ((active[k] > -2 && findTop) || ((active[k] == -2) && (!findTop))) {
                    v = new double[2];
                    v[0] = xCoord[i];
                    v[1] = yCoord[j];
                    discretizedDomain.addElement(v);
                    v = new double[2];
                    v[0] = xCoord[i+1];
                    v[1] = yCoord[j];
                    discretizedDomain.addElement(v);
                    findTop = !findTop;
                }
                if (active[k] > -2 && (j == numRow-1)) {
                    v = new double[2];
                    v[0] = xCoord[i];
                    v[1] = yCoord[j+1];
                    discretizedDomain.addElement(v);
                    v = new double[2];
                    v[0] = xCoord[i+1];
                    v[1] = yCoord[j+1];
                    discretizedDomain.addElement(v);
                    findTop = true;
                }
            }
        }
        activeCellsHaveChanged = false;
        dataHaveChanged = false;
        return true;
    }

    /**
     * Gets the discretized domain
     */
    public Vector getDiscretizedDomain() {
        return discretizedDomain;
    }

    /**
     * Gets the specified boundary. The index zero refers
     * to the exterior boundary. All indices greater than
     * zero refer to interior boundaries.
     */
    public mp2Polygon getBoundary(int i) {
        if (i>=0 && i<shapes.size()) {
            return (mp2Polygon) shapes.elementAt(i);
        } else {
            return null;
        }
    }

    /**
     * Gets the number of boundaries.
     */
    public int getNumberOfBoundaries() {
        return super.getNumberOfShapes();
    }

    /**
     * Gets the nearest boundary segment to the specified point,
     * searching in the x direction from the point
     * (in model coordinates).
     *
     * @return a two-element int array, the first element is the
     *           index of the boundary, and the second element
     *           is the index of the segment within the boundary
     */
    public int [] getNearestSegmentInXDirection(double x, double y) {
        if (shapes.size() == 0) {
            return null;
        }
        double distance, nearestDistance;
        mp2Polygon boundary;
        Object [] seginfo;
        int [] result = new int[2];
        result[0] = -1;
        result[1] = -1;
        nearestDistance = -1;

        for (int i=0; i<shapes.size(); i++) {
            boundary = (mp2Polygon) shapes.elementAt(i);
            seginfo = boundary.getNearestSegmentInXDirection(x, y);
            if (seginfo != null) {
                distance = ((Double) seginfo[1]).doubleValue();
                if (result[0] == -1 || distance < nearestDistance) {
                    result[0] = i;
                    result[1] = ((Integer) seginfo[0]).intValue();
                    nearestDistance = distance;
                }
            }
        }
        return result;
    }
    /**
     * Gets the nearest boundary segment to the specified point
     * searching in the x direction from the point
     * (in model coordinates).
     *
     * @return a two-element int array, the first element is the
     *           index of the boundary, and the second element
     *           is the index of the segment within the boundary
     */
    public int [] getNearestSegmentInYDirection(double x, double y) {
        if (shapes.size() == 0) {
            return null;
        }
        double distance, nearestDistance;
        mp2Polygon boundary;
        Object [] seginfo;
        int [] result = new int[2];
        result[0] = -1;
        result[1] = -1;
        nearestDistance = -1;

        for (int i=0; i<shapes.size(); i++) {
            boundary = (mp2Polygon) shapes.elementAt(i);
            seginfo = boundary.getNearestSegmentInYDirection(x, y);
            if (seginfo != null) {
                distance = ((Double) seginfo[1]).doubleValue();
                if (result[0] == -1 || distance < nearestDistance) {
                    result[0] = i;
                    result[1] = ((Integer) seginfo[0]).intValue();
                    nearestDistance = distance;
                }
            }
        }
        return result;
    }

    /**
     * Initializes this object
     */
    public void init(mp2Doc doc) {
        super.init(doc);
        discretizedDomain = null;
    }

    public void setDataHaveChanged() {
        super.setDataHaveChanged();
        gridData.onDomainChanged();
    }

}
