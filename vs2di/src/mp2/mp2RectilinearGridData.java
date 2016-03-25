/*
 * mp2RectilinearGridData.java
 */
package mp2;

import java.io.*;
import java.awt.*;
import java.util.Vector;

/**
 * Holds the coordinates of grid lines
 */
public class mp2RectilinearGridData extends mp2AbstractGridData
        implements Serializable {

    static final long serialVersionUID = -6440041347742231535L;

    protected Vector xCoord;
    protected Vector yCoord;
    protected double rotationAngle;

    protected transient mp2DomainData domainData;
    protected transient boolean updateActiveCellArray;
    protected transient int [] activeCellArray;
    protected transient boolean useRadialCoord;

    /**
     * Constructor.
     */
    public mp2RectilinearGridData() {
        xCoord = new Vector();
        yCoord = new Vector();
        rotationAngle = 0;
    }

    /**
     * Adds an x coordinate (that is, add a vertical grid line).
     * Returns the index of the added coordinate.
     */
    public int addXCoord(double x) {
        int result = 0;
        if (xCoord.size() == 0) {
            xCoord.addElement(new Double(x));
            return result;
        }
        int i;
        for (i=0; i<xCoord.size(); i++) {
            if ((((Double) xCoord.elementAt(i)).doubleValue()) > x) {
                xCoord.insertElementAt(new Double(x), i);
                result = i;
                break;
            }
        }
        if (i==xCoord.size()) {
            xCoord.addElement(new Double(x));
            result = i;
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
        return result;
    }


    /**
     * Adds a y coordinate (that is, add a horizontal grid line).
     * Returns the index of the added coordinate
     */
    public int addYCoord(double y) {
        int result = 0;
        if (yCoord.size() == 0) {
            yCoord.addElement(new Double(y));
            return result;
        }
        int i;
        for (i=0; i<yCoord.size(); i++) {
            if ((((Double) yCoord.elementAt(i)).doubleValue()) > y) {
                yCoord.insertElementAt(new Double(y), i);
                result = i;
                break;
            }
        }
        if (i==yCoord.size()) {
            yCoord.addElement(new Double(y));
            result = i;
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
        return result;
    }

    /**
     * Deletes all the x coordinates
     */
    public void deleteAllXCoords() {
        xCoord.removeAllElements();
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }

    /**
     * Deletes all the y coordinates
     */
    public void deleteAllYCoords() {
        yCoord.removeAllElements();
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }

    /**
     * Delete the x coordinate at the specified index
     * Returns the deleted coordinate
     */
    public double deleteXCoordAt(int index) {
        if (index < 0 || index >= xCoord.size()) {
            return Double.NaN;
        }
        double deletedCoord = ((Double) xCoord.elementAt(index)).doubleValue();
        xCoord.removeElementAt(index);
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
        return deletedCoord;
    }

    /**
     * Deletes the y coordinate at the specified index
     * Returns the deleted coordinate
     */
    public double deleteYCoordAt(int index) {
        if (index < 0 || index >= yCoord.size()) {
            return Double.NaN;
        }
        double deletedCoord = ((Double) yCoord.elementAt(index)).doubleValue();
        yCoord.removeElementAt(index);
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
        return deletedCoord;
    }

    /**
     * Deletes the x coordinates in the specified list.
     * Return an array of the deleted coordinates
     */
    public double [] deleteXCoords(int [] list) {
        if (list == null || list.length == 0) {
            return null;
        }
        double [] deletedCoords = new double[list.length];
        for (int i=0; i<list.length; i++) {
            deletedCoords[i] =
                ((Double) xCoord.elementAt(list[i] - i)).doubleValue();
            xCoord.removeElementAt(list[i] - i);
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
        return deletedCoords;
    }

    /**
     * Deletes the y coordinates in the specified list.
     * Returns an array of the deleted coordinates
     */
    public double [] deleteYCoords(int [] list) {
        if (list == null || list.length == 0) {
            return null;
        }
        double [] deletedCoords = new double[list.length];
        for (int i=0; i<list.length; i++) {
            deletedCoords[i] =
                ((Double) yCoord.elementAt(list[i] - i)).doubleValue();
            yCoord.removeElementAt(list[i] - i);
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
        return deletedCoords;
    }
    
    public double [] getBounds() {
        if (xCoord.size() <=1 || yCoord.size() <= 1) {
            return null;
        }
        double [] bounds = new double[4];
        bounds[0] = ((Double) xCoord.firstElement()).doubleValue();
        bounds[1] = ((Double) yCoord.firstElement()).doubleValue();
        bounds[2] = ((Double) xCoord.lastElement()).doubleValue();
        bounds[3] = ((Double) yCoord.lastElement()).doubleValue();
        return bounds;
    }

    /**
     * Gets the active cell array
     */
    public int [] getActiveCellArray() {
        if (updateActiveCellArray) {
            updateActiveCellArray();
        }
        return activeCellArray;
    }

    /**
     * Gets the rotation angle (not currently used
     */
    public double getRotationAngle() {
        return rotationAngle;
    }

    /**
     * Indicates whether or not radial coordinates are being used
     */
    public boolean getUseRadialCoord() {
        return useRadialCoord;
    }

    /**
     * Gets the x coordinate at the specified index
     */
    public double getXCoordAt(int i) {
        return ((Double) xCoord.elementAt(i)).doubleValue();
    }

    /**
     * Gets the y coordinate at the specified index
     */
    public double getYCoordAt(int i) {
        return ((Double) yCoord.elementAt(i)).doubleValue();
    }

    /**
     * Gets the x coordinates as an array
     */
    public double [] getXCoords() {
        if (xCoord.size() == 0) {
            return null;
        }
        double [] x = new double [xCoord.size()];
        for (int i=0; i<x.length; i++) {
            x[i] = ((Double) xCoord.elementAt(i)).doubleValue();
        }
        return x;
    }

    /**
     * Gets the y coordinates as an array
     */
    public double [] getYCoords() {
        if (yCoord.size() == 0) {
            return null;
        }
        double [] y = new double [yCoord.size()];
        for (int i=0; i<y.length; i++) {
            y[i] = ((Double) yCoord.elementAt(i)).doubleValue();
        }
        return y;
    }

    /**
     * Gets the number of x coordinates (vertical grid lines)
     */
    public int getXCoordCount() {
        return xCoord.size();
    }

    /**
     * Gets the number of y coordinates (horizontal grid lines)
     */
    public int getYCoordCount() {
        return yCoord.size();
    }

    /**
     * Initialization
     */
    public void init(mp2Doc doc) {
        super.init(doc);
        activeCellArray = null;
        // if xCoord and yCoord are not null at init, then
        // the object was created via serialization, in which
        // case we need to update the active cell array and
        if (xCoord != null && yCoord != null) {
            updateActiveCellArray = true;
        } else {
            updateActiveCellArray = false;
        }
    }

    /**
     * Indicates whether or not the grid has been defined
     */
    public boolean isDefined() {
        return ((xCoord.size() > 0) && (yCoord.size() > 0));
    }

    /**
     * Creates a uniform grid, with equal column widths, and
     * equal row heights, but column width may not necessarily
     * equal row height
     */
    public void makeUniformGrid(int numCol, int numRow, boolean shrinkFit) {

        double x0, y0, dx, dy;
        if (xCoord.size() >= 2 && yCoord.size() >=2 && !shrinkFit) {
            x0 = ((Double) xCoord.firstElement()).doubleValue();
            y0 = ((Double) yCoord.firstElement()).doubleValue();
            dx = (((Double) xCoord.lastElement()).doubleValue() - x0)/numCol;
            dy = (((Double) yCoord.lastElement()).doubleValue() - y0)/numRow;
        } else {
            mp2RectBounds domainBounds = domainData.getBoundary(0).getBounds();
            x0 = domainBounds.x;
            y0 = domainBounds.y;
            dx = domainBounds.width/numCol;
            dy = domainBounds.height/numRow;
        }
        xCoord.removeAllElements();
        for (int i=0; i<=numCol; i++) {
            xCoord.addElement(new Double(x0 + i*dx));
        }
        yCoord.removeAllElements();
        for (int i=0; i<=numRow; i++) {
            yCoord.addElement(new Double(y0 + i*dy));
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }

    /**
     * Invoked when the domain is changed
     */
    public void onDomainChanged() {
        super.onDomainChanged();
        updateActiveCellArray = true;
    }

    /**
     * Resize the grid by stretching it in the x and y direction
     * to the new width and height
     */
    public void resize(double newWidth, double newHeight) {
        double x0 = ((Double) xCoord.elementAt(0)).doubleValue();
        double x1 = ((Double) xCoord.lastElement()).doubleValue();
        double y0 = ((Double) yCoord.elementAt(0)).doubleValue();
        double y1 = ((Double) yCoord.lastElement()).doubleValue();
        double scalex = newWidth/(x1 - x0);
        double scaley = newHeight/(y1 - y0);
        for (int i=0; i<xCoord.size(); i++) {
            double x = ((Double) xCoord.elementAt(i)).doubleValue();
            xCoord.setElementAt(new Double(x0 + scalex * (x - x0)), i);
        }
        for (int j=0; j<yCoord.size(); j++) {
            double y = ((Double) yCoord.elementAt(j)).doubleValue();
            yCoord.setElementAt(new Double(y0 + scaley * (y - y0)), j);
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }


    /**
     * Sets the reference to the domain data
     */
    public void setDomainData(mp2DomainData d) {
        domainData = d;
    }

    /**
     * Invoked if the simulation uses radial coordinates
     */
    public void setUseRadialCoord(boolean use) {
        useRadialCoord = use;
        if (useRadialCoord && this.isDefined() 
                && ((Double) xCoord.firstElement()).doubleValue() > 0) {
            xCoord.insertElementAt(new Double(0), 0);
            updateActiveCellArray = true;
            doc.setChanged(true);
            notifyRegisteredData();
        }
    }

    /**
     * Sets the x coordinate at the specified index
     */
    public void setXCoord(double x, int i) {
        if (i<0 || i>=xCoord.size()) {
            return;
        }
        xCoord.setElementAt(new Double(x), i);
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }

    /**
     * Sets the y coordinate at the specified index
     */
    public void setYCoord(double y, int i) {
        if (i<0 || i>=yCoord.size()) {
            return;
        }
        yCoord.setElementAt(new Double(y), i);
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }

    /**
     * Subdivides the column interval defined by the vertical grid
     * lines at the start and end indices. Option for variable spacing
     */
    public void subdivideXInterval(int startIndex, int endIndex,
                int numSubInterval, double multiplier) {
        double start = ((Double) xCoord.elementAt(startIndex)).doubleValue();
        double end = ((Double) xCoord.elementAt(endIndex)).doubleValue();
        double delta;
        if (multiplier == 1) {
            delta = (end - start)/numSubInterval;
        } else {
            delta = (end - start)*(multiplier - 1)/
                    (Math.pow(multiplier, numSubInterval) - 1);
        }
        for (int i=startIndex+1; i<endIndex; i++) {
            xCoord.removeElementAt(startIndex + 1);
        }
        for (int i=1; i<numSubInterval; i++) {
            start += delta;
            xCoord.insertElementAt(new Double(start), startIndex + i);
            delta *= multiplier;
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }

    /**
     * Subdivides the row interval defined by the horizontal grid
     * lines at the start and end indices. Option for variable spacing
     */
    public void subdivideYInterval(int startIndex, int endIndex,
                int numSubInterval, double multiplier) {
        double start = ((Double) yCoord.elementAt(startIndex)).doubleValue();
        double end = ((Double) yCoord.elementAt(endIndex)).doubleValue();
        double delta;
        if (multiplier == 1) {
            delta = (end - start)/numSubInterval;
        } else {
            delta = (end - start)*(multiplier - 1)/
                    (Math.pow(multiplier, numSubInterval) - 1);
        }
        for (int i=startIndex+1; i<endIndex; i++) {
            yCoord.removeElementAt(startIndex + 1);
        }
        for (int i=1; i<numSubInterval; i++) {
            start += delta;
            yCoord.insertElementAt(new Double(start), startIndex + i);
            delta *= multiplier;
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }

    /**
     * translate the upper left corner of the grid to the specified location
     * *****DO NOT USE*****
     * *****Use translateByMovingFirstPointTo instead
     */
    public void translateUpperLeftCornerTo(double newLeft, double newTop) {
        double dx = newLeft - ((Double) xCoord.firstElement()).doubleValue();
        double dy = newTop - ((Double) yCoord.firstElement()).doubleValue();
        for (int i=0; i<xCoord.size(); i++) {
            double x = ((Double) xCoord.elementAt(i)).doubleValue();
            xCoord.setElementAt(new Double(x + dx), i);
        }
        for (int j=0; j<yCoord.size(); j++) {
            double y = ((Double) yCoord.elementAt(j)).doubleValue();
            yCoord.setElementAt(new Double(y + dy), j);
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }
    
    public void translateByMovingFirstPointTo(double newX, double newY) {
        double dx = newX - ((Double) xCoord.firstElement()).doubleValue();
        double dy = newY - ((Double) yCoord.firstElement()).doubleValue();
        for (int i=0; i<xCoord.size(); i++) {
            double x = ((Double) xCoord.elementAt(i)).doubleValue();
            xCoord.setElementAt(new Double(x + dx), i);
        }
        for (int j=0; j<yCoord.size(); j++) {
            double y = ((Double) yCoord.elementAt(j)).doubleValue();
            yCoord.setElementAt(new Double(y + dy), j);
        }
        updateActiveCellArray = true;
        doc.setChanged(true);
        notifyRegisteredData();
    }


    /**
     * Makes the array that indicates which cells are inactive
     * and which cells are active.
     */
    public void updateActiveCellArray() {
        if (!isDefined() || !updateActiveCellArray) {
            return;
        }

        int i, j, k;
        int numCol = xCoord.size()-1;
        int numRow = yCoord.size()-1;
        double [] xCenter = new double[numCol];
        double [] yCenter = new double[numRow];

        // Create the active cell array and set all cells initially to inactive
        activeCellArray = new int[numCol*numRow];
        for (k=0; k<numCol*numRow; k++) {
            activeCellArray[k] = -2;
        }

        // Compute the x coordinates of the cell centers.
        for (i=0; i<numCol; i++) {
            xCenter[i] = (((Double) xCoord.elementAt(i)).doubleValue()
                        + ((Double) xCoord.elementAt(i+1)).doubleValue())/2;
        }

        // Compute the y coordinates of the cell centers.
        for (j=0; j<numRow; j++) {
            yCenter[j] = (((Double) yCoord.elementAt(j)).doubleValue()
                        + ((Double) yCoord.elementAt(j+1)).doubleValue())/2;
        }

        // If the center of a cell lies inside the exterior boundary,
        // then the cell is considered active. Ignore interior
        // boundaries for now.
        mp2Polygon exteriorBoundary = domainData.getBoundary(0);
        for (j=0; j<numRow; j++) {
            for (i=0; i<numCol; i++) {
                k = j*numCol + i;
                if (exteriorBoundary.contains(xCenter[i], yCenter[j])) {
                    activeCellArray[k] = -1;
                }
            }
        }

        // Now exclude all cells inside interior boundaries. If the
        // center of a cell lies inside an interior boundary, then
        // the cell is considered inactive.
        for (int b=1; b<domainData.getNumberOfShapes(); b++) {
            mp2Polygon interiorBoundary = domainData.getBoundary(b);
            for (j=0; j<numRow; j++) {
                for (i=0; i<numCol; i++) {
                    k = j*numCol + i;
                    if (interiorBoundary.contains(xCenter[i], yCenter[j])) {
                        activeCellArray[k] = -2;
                    }
                }
            }
        }
        updateActiveCellArray = false;
    }
}
