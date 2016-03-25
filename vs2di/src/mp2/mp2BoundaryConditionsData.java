/*
 * mp2BoundaryConditionsData.java
 */
package mp2;

import java.io.*;
import java.util.*;

/**
 * Encapsulates the boundary conditions. Boundary conditions are
 * specified by model periods. A model period is a simulation time 
 * interval during which all model parameters remain constant.
 */
public abstract class mp2BoundaryConditionsData extends mp2DiscretizableData 
            implements Serializable {

    static final long serialVersionUID = 6273369052507456108L;

    /**
     * Boundary conditions for all boundary segments for all 
     * model periods
     */
    protected Vector periods;

    protected boolean verticalBoundaryControlsCorner;

    /**
     * The domain data
     */
    protected transient mp2DomainData domainData;

    /**
     * The selected period for which the boundary conditions are
     * shown on the view.
     */
    protected transient int selectedPeriod;

    protected transient Vector boundaryCellMasterList;

    /**
     * Create a data set containing boundary conditions.
     */
    public mp2BoundaryConditionsData() {
        super();
        periods = new Vector();
        verticalBoundaryControlsCorner = true;
    }

    /**
     * Add a new boundary for all periods
     */
    public void addBoundary(int numSegment) {
        for (int i=0; i<periods.size(); i++) {
            Vector boundaries = (Vector) periods.elementAt(i);
            Vector newSegments = new Vector();
            for (int j=0; j<numSegment; j++) {
                newSegments.addElement(createBoundaryCondition());
            }
            boundaries.addElement(newSegments);
        }
        setDataHaveChanged();
    }

    /**
     * Add a new period after the specified period.
     */
    public void addPeriodAfter(int p) {

        int i, j, k;
        // Create a set of boundaries for the new period
        Vector newBoundaries = new Vector();
        for (i=0; i<domainData.getNumberOfBoundaries(); i++) {
            // For each boundary, create segments
            mp2Polygon boundary = domainData.getBoundary(i);
            Vector newSegments = new Vector();
            for (j=0; j<boundary.getNumberOfVertices(); j++) {
                newSegments.addElement(createBoundaryCondition());
            }
            newBoundaries.addElement(newSegments);
        }
        periods.insertElementAt(newBoundaries, p+1);

        // copy bc from pervious period
        if (p > -1) {
            Vector boundaries = (Vector) periods.elementAt(p);
            for (i=0; i<boundaries.size(); i++) {
                Vector segments = (Vector) boundaries.elementAt(i);
                Vector newSegments = (Vector) newBoundaries.elementAt(i);
                for (j=0; j<segments.size(); j++) {
                    mp2BoundaryCondition bc = 
                            (mp2BoundaryCondition) segments.elementAt(j);
                    mp2BoundaryCondition newBC = 
                            (mp2BoundaryCondition) newSegments.elementAt(j);
                    newBC.copy(bc);
                }
            }
        }
        setDataHaveChanged();
    }

    /**
     * Copy the BC of the selected period to the specified period
     */
    public void copySelectedPeriodBCTo(int p) {
        if (selectedPeriod < 0 || selectedPeriod == p) {
            return;
        }
        Vector selectedBoundaries = (Vector) periods.elementAt(selectedPeriod);
        Vector boundaries = (Vector) periods.elementAt(p);
        for (int j=0; j<boundaries.size(); j++) {
            Vector selectedSegments = (Vector) selectedBoundaries.elementAt(j);
            Vector segments = (Vector) boundaries.elementAt(j);
            for (int k=0; k<segments.size(); k++) {
                mp2BoundaryCondition selectedBC = 
                        (mp2BoundaryCondition) selectedSegments.elementAt(k);
                mp2BoundaryCondition bc = 
                        (mp2BoundaryCondition) segments.elementAt(k);
                bc.copy(selectedBC);
            }
        }
        setDataHaveChanged();
    }

    /**
     * Creates a boundary condition
     */
    abstract public mp2BoundaryCondition createBoundaryCondition();

    /**
     * Deletes the BC for the specified boundary.
     */
    public void deleteBoundary(int b) {
        for (int i=0; i<periods.size(); i++) {
            Vector boundaries = (Vector) periods.elementAt(i);
            if (b>=0 && b<boundaries.size()) {
                boundaries.removeElementAt(b);
            }
        }
        setDataHaveChanged();
    }

    /**
     * Deletes the BC for the specified model period.
     */
    public void deletePeriod(int p) {
        if (p>=0 && p<periods.size()) {
            periods.removeElementAt(p);
        }
        setDataHaveChanged();
    }

    /**
     * Discretize the boundary conditions
     */
    public boolean discretize() {

        if (!(gridData instanceof mp2RectilinearGridData)) {
            return false;
        }
        // Create the boundary cell master list
        int i, j;
        Vector segments;
        Vector cells;
        boundaryCellMasterList = new Vector();
        for (i=0; i<domainData.getNumberOfBoundaries(); i++) {
            segments = new Vector();
            boundaryCellMasterList.addElement(segments);
            int n = domainData.getBoundary(i).getNumberOfVertices();
            for (j=0; j<n; j++) {
                cells = new Vector();
                segments.addElement(cells);
            }
        }

        // Get grid coordinates, and find cell centers
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        double [] xCoord = rectGridData.getXCoords();
        double [] yCoord = rectGridData.getYCoords();
        int numCol = xCoord.length-1;
        int numRow = yCoord.length-1;
        double [] xCenter = new double[numCol];
        double [] yCenter = new double[numRow];
        for (i=0; i<numCol; i++) {
            xCenter[i] = (xCoord[i] + xCoord[i+1])/2;
        }
        for (j=0; j<numRow; j++) {
            yCenter[j] = (yCoord[j] + yCoord[j+1])/2;
        }
        
        // For 1D case, set corner policy
        if (numCol == 1) {
            // one vertical column
            verticalBoundaryControlsCorner = false;
        } else if (numRow == 1) {
            // one horizontal row
            verticalBoundaryControlsCorner = true;
        }

        // Make a copy of the active cell array for use here.
        int [] originalActiveCellArray = 
                ((mp2RectilinearGridData)gridData).getActiveCellArray();
        int [] activeCellArray = new int[originalActiveCellArray.length];
        for (i=0; i<originalActiveCellArray.length; i++) {
            activeCellArray[i] = originalActiveCellArray[i];
        }

        // Find boundary cells
        if (verticalBoundaryControlsCorner) {
            sweepForBoundaryCellsByRow(numCol, numRow, xCenter, yCenter, 
                                            activeCellArray);
            sweepForBoundaryCellsByColumn(numCol, numRow, xCenter, yCenter, 
                                            activeCellArray);
        } else {
            sweepForBoundaryCellsByColumn(numCol, numRow, xCenter, yCenter, 
                                            activeCellArray);
            sweepForBoundaryCellsByRow(numCol, numRow, xCenter, yCenter, 
                                            activeCellArray);
        }

        // Sort the boundary cells by segment
        int m, n, size;
        for (i=0; i<boundaryCellMasterList.size(); i++) {
            segments = (Vector) boundaryCellMasterList.elementAt(i);
            for (j=0; j<segments.size(); j++) {
                cells = (Vector) segments.elementAt(j);
                // Copy data from Vector into array, use bubble sort
                // on array, and save the array
                size = cells.size();
                int [] a = new int[size];
                for (m=0; m<size; m++) {
                    a[m] = ((Integer) cells.elementAt(m)).intValue();
                }
                for (m=1; m<size; m++) {
                    for (n=size-1; n>=m; n--) {
                        if (a[n-1] > a[n]) {
                            int t = a[n-1];
                            a[n-1] = a[n];
                            a[n] = t;
                        }
                    }
                }
                segments.setElementAt(a, j);
            }
        }


        return true;
    }

    /**
     * Extends the BC of the specified segments of the selected
     * period to all future periods.
     */
    public void extendSelectedPeriodBCToFuture(boolean [][] segmentMask) {
        Vector selectedBoundaries = (Vector) periods.elementAt(selectedPeriod);
        for (int i=selectedPeriod+1; i<periods.size(); i++) {
            Vector boundaries = (Vector) periods.elementAt(i);
            for (int j=0; j<boundaries.size(); j++) {
                Vector selectedSegments = (Vector) selectedBoundaries.elementAt(j);
                Vector segments = (Vector) boundaries.elementAt(j);
                for (int k=0; k<segments.size(); k++) {
                    if (segmentMask[j][k]) {
                        mp2BoundaryCondition selectedBC = 
                            (mp2BoundaryCondition) selectedSegments.elementAt(k);
                        mp2BoundaryCondition bc = 
                            (mp2BoundaryCondition) segments.elementAt(k);
                        bc.copy(selectedBC);
                    }
                }
            }
        }
        setDataHaveChanged();
    }

    /**
     * Gets the boundary conditions for the selected period
     */
    public Vector getBCForSelectedPeriod() {
        if (selectedPeriod > -1 
                && selectedPeriod < periods.size()) {
            return (Vector) periods.elementAt(selectedPeriod);
        } else {
            return null;
        }
    }

    public Vector getBoundaryCellListForPeriod(int p) {
        return boundaryCellMasterList;
    }

    /**
     * Gets a clone of this BC
     */
    public Vector getClonedBC() {
        Vector clonedPeriods = new Vector();
        for (int i=0; i<periods.size(); i++) {
            Vector boundaries = (Vector) periods.elementAt(i);
            Vector clonedBoundaries = new Vector();
            for (int j=0; j<boundaries.size(); j++) {
                Vector segments = (Vector) boundaries.elementAt(j);
                Vector clonedSegments = new Vector();
                for (int k=0; k<segments.size(); k++) {
                    mp2BoundaryCondition bc = 
                            (mp2BoundaryCondition) segments.elementAt(k);
                    mp2BoundaryCondition clonedBC = createBoundaryCondition();
                    clonedBC.copy(bc);
                    clonedSegments.addElement(clonedBC);
                }
                clonedBoundaries.addElement(clonedSegments);
            }
            clonedPeriods.addElement(clonedBoundaries);
        }
        return clonedPeriods;
    }

    /**
     * Gets the number of periods
     */
    public int getNumberOfPeriods() {
        return periods.size();
    }

    /**
     * Gets the selected period, which corresponds to the selected
     * row in the recharge period table.
     */
    public int getSelectedPeriod() {
        return selectedPeriod;
    }

    /**
     * Gets the sweep policy
     */
    public boolean getVerticalBoundaryControlsCorner() {
        return verticalBoundaryControlsCorner;
    }

    /**
     * Initializes this object. This methods initialized the
     * transient data members of this object.
     */
    public void init(mp2Doc doc) {
        super.init(doc);
        selectedPeriod = -1;
    }

    public void setBC(Vector p) {
        this.periods = p;
        setDataHaveChanged();
    }

    /**
     * Sets the domain data
     */
    public void setDomainData(mp2DomainData domainData) {
        this.domainData = domainData;
    }

    /**
     * Sets the selected period. This is the period for which the
     * boundary conditions are shown in the view.
     */
    public void setSelectedPeriod(int p) {
        if (selectedPeriod != p) {
            selectedPeriod = p;
            doc.getView().repaint();
        }
    }

    /**
     * Sets the sweep policy
     */
    public void setVerticalBoundaryControlsCorner(boolean b) {
        verticalBoundaryControlsCorner = b;
        setDataHaveChanged();
    }

    /**
     * Split the specified segment on the specified boundary into
     * two segments
     */
    public void splitSegment(int boundaryIndex, int segmentIndex) {
        for (int i=0; i<periods.size(); i++) {
            Vector boundaries = (Vector) periods.elementAt(i);
            Vector segments = (Vector) boundaries.elementAt(boundaryIndex);
            mp2BoundaryCondition bc = 
                    (mp2BoundaryCondition) segments.elementAt(segmentIndex);
            mp2BoundaryCondition newBC = createBoundaryCondition();
            newBC.copy(bc);
            segments.insertElementAt(newBC, segmentIndex+1);
        }
        setDataHaveChanged();
    }

    /**
     * Find boundary cells, sweeping in row direction
     */
    protected void sweepForBoundaryCellsByRow(int numCol, int numRow,
                double [] xCenter, double [] yCenter, int [] activeCellArray) {

        boolean outside;
        int i, j, k;
        int [] bs;
        Vector segments;
        Vector cells;

        for (j=0; j<numRow; j++) {
            outside = true;
            for (i=0; i<numCol; i++) {
                k = j*numCol + i;
                if ((activeCellArray[k] > -2) && outside) {
                    if (activeCellArray[k] == -1) {
                        activeCellArray[k] = 0;
                        bs = domainData.getNearestSegmentInXDirection(
                                                        xCenter[i], yCenter[j]);
                        if (bs != null) {
                            segments = (Vector) boundaryCellMasterList.elementAt(bs[0]);
                            cells = (Vector) segments.elementAt(bs[1]);
                            cells.addElement(new Integer(k));
                        }
                    }
                    outside = !outside;
                }
                else if ((activeCellArray[k] == -2) && (!outside)) {
                    if (activeCellArray[k-1] == -1) {
                        activeCellArray[k-1] = 0;
                        bs = domainData.getNearestSegmentInXDirection(
                                                        xCenter[i], yCenter[j]);
                        if (bs != null) {
                            segments = (Vector) boundaryCellMasterList.elementAt(bs[0]);
                            cells = (Vector) segments.elementAt(bs[1]);
                            cells.addElement(new Integer(k-1));
                        }
                    }
                    outside = !outside;
                }
                else if ((activeCellArray[k] > -2) && (i == numCol-1)) {
                    if (activeCellArray[k] == -1) {
                        activeCellArray[k] = 0;
                        bs = domainData.getNearestSegmentInXDirection(
                                                        xCenter[i], yCenter[j]);
                        if (bs != null) {
                            segments = (Vector) boundaryCellMasterList.elementAt(bs[0]);
                            cells = (Vector) segments.elementAt(bs[1]);
                            cells.addElement(new Integer(k));
                        }
                    }
                }
            }
        }

    }

    /**
     * Find boundary cells, sweep in column direction
     */
    protected void sweepForBoundaryCellsByColumn(int numCol, int numRow,
                double [] xCenter, double [] yCenter, int [] activeCellArray) {

        boolean outside;
        int i, j, k;
        int [] bs;
        Vector segments;
        Vector cells;

        for (i=0; i<numCol; i++) {
            outside = true;
            for (j=0; j<numRow; j++) {
                k = j*numCol + i;
                if (activeCellArray[k] > -2 && outside) {
                    if (activeCellArray[k] == -1) {
                        activeCellArray[k] = 0;
                        bs = domainData.getNearestSegmentInYDirection(
                                                    xCenter[i], yCenter[j]);
                        if (bs != null) {
                            segments = (Vector) boundaryCellMasterList.elementAt(bs[0]);
                            cells = (Vector) segments.elementAt(bs[1]);
                            cells.addElement(new Integer(k));
                        }
                    }
                    outside = !outside;
                }
                else if ((activeCellArray[k] == -2) && (!outside)) {
                    if (activeCellArray[k-numCol] == -1) {
                        activeCellArray[k-numCol] = 0;
                        bs = domainData.getNearestSegmentInYDirection(
                                                    xCenter[i], yCenter[j]);
                        if (bs != null) {
                            segments = (Vector) boundaryCellMasterList.elementAt(bs[0]);
                            cells = (Vector) segments.elementAt(bs[1]);
                            cells.addElement(new Integer(k-numCol));
                        }
                    }
                    outside = !outside;
                }
                else if (activeCellArray[k] > -2 && (j == numRow-1)) {
                    if (activeCellArray[k] == -1) {
                        activeCellArray[k] = 0;
                        bs = domainData.getNearestSegmentInYDirection(
                                                    xCenter[i], yCenter[j]);
                        if (bs != null) {
                            segments = (Vector) boundaryCellMasterList.elementAt(bs[0]);
                            cells = (Vector) segments.elementAt(bs[1]);
                            cells.addElement(new Integer(k));
                        }
                    }
                }
            }
        }
    }
}
