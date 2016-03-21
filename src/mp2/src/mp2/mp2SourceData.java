/*
 * mp2SourceData
 */

package mp2;

import java.awt.*;
import java.io.*;
import java.util.Vector;

public abstract class mp2SourceData extends mp2ShapesData implements Serializable {
    
    static final long serialVersionUID = -1570947898353415293L;
    
    protected Vector sourceStrengthAllData;
    protected transient Vector sourceCellIndices;
    protected transient mp2TableData simulationPeriodData;
    
    public mp2SourceData() {
        super();
        sourceStrengthAllData = new Vector();
    }
    
    public void addShape(mp2Shape shape) {
        super.addShape(shape);
        mp2TableData strengths = createStrengthData();
        for (int i=0; i<simulationPeriodData.getNumberOfRows(); i++) {
            strengths.addRow(0, strengths.createDefaultRow());
        }
        sourceStrengthAllData.addElement(strengths);
    }
    
    protected abstract mp2TableData createStrengthData();
    
    public Vector deleteSelectedShapes() {
        Vector newStrengths = new Vector();
        for (int i=0; i<shapes.size(); i++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(i);
            if (!shape.isSelected()) {
                newStrengths.addElement(sourceStrengthAllData.elementAt(i));
            }
        }
        sourceStrengthAllData = newStrengths;
        return super.deleteSelectedShapes();
    }
    
    public void deleteShapeAt(int i) {
        if (i>=0 && i<shapes.size()) {
            sourceStrengthAllData.removeElementAt(i);
        }
        super.deleteShapeAt(i);
    }

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

        // Create the point source array and set all cells 
        // initially to false (no point source at cell);
        sourceCellIndices = new Vector();

        // Find the cell corresponding to each point source,
        // and set point source array element to true
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
                    sourceCellIndices.addElement(new Integer(index));
                }
            }
        }
        activeCellsHaveChanged = false;
        dataHaveChanged = false;
        return true;
    }
    
    public Vector getClonedStrengths() {
        Vector clone = new Vector();
        for (int i=0; i<sourceStrengthAllData.size(); i++) {
            clone.addElement(sourceStrengthAllData.elementAt(i));
        }
        return clone;
    }

    public int getNumberOfSourceCells() {
        discretize();
        if (sourceCellIndices == null) {
            return 0;
        } else {
            return sourceCellIndices.size();
        }
    }

    public Vector getSourceCellIndices() {
        return sourceCellIndices;
    }
    
    public mp2TableData getSourceStrengthData(int i) {
        if (i < sourceStrengthAllData.size()) {
            return (mp2TableData) sourceStrengthAllData.elementAt(i);
        } else {
            return null;
        }
    }
        

    public void init(mp2Doc doc) {
        super.init(doc);
        for (int i=0; i<sourceStrengthAllData.size(); i++) {
            ((mp2TableData) sourceStrengthAllData.elementAt(i)).init(doc);
        }
        sourceCellIndices = null;
    }
    
    public void insertPeriodAt(int p) {
        for (int i=0; i<sourceStrengthAllData.size(); i++) {
            mp2TableData strength = (mp2TableData) sourceStrengthAllData.elementAt(i);
            strength.insertDefaultRowAt(p);
        }
    }
    
    public void deletePeriodAt(int p) {
        for (int i=0; i<sourceStrengthAllData.size(); i++) {
            mp2TableData strength = (mp2TableData) sourceStrengthAllData.elementAt(i);
            strength.deleteRow(p);
        }
    }
    
    public void setSimulationPeriodData(mp2TableData simulationPeriodData) {
        this.simulationPeriodData = simulationPeriodData;
    }
    
    public void setStrengths(Vector strengths) {
        this.sourceStrengthAllData = strengths;
    }
}
