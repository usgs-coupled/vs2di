/*
 * mp2DiscretizableData.java
 */
package mp2;

import java.io.*;

/**
 * Defines the behavior of graphical data that are discretizable. 
 * All discretizable data objects hold a reference to a 
 * grid data object to access the grid line coordinates and
 * the active cell array--both are needed for discretization.
 * In turn, a discretizable data object registers itself with 
 * the grid data object to be notified when the grid has been 
 * modified. If either the grid and/or the data are modified, 
 * rediscretization is necessary. However, rediscretization is
 * postponed to just before the data is painted to screen.
 * This avoid the needless computation if the data is not
 * visible. The instance variables <code>activeCellsHaveChanged</code>
 * and <code>dataHaveChanged</code> keep track of whether
 * rediscretization is necessary.
 *
 * <P>This class implements the Serializable interface so
 * objects can be serialized and deserialized. Therefore,
 * object creation is a two-step procedure in which
 * the <code>init</code> method is called after construction
 * or deserialization. This is similar to how document objects
 * are created.
 *
 * @see mp2.mp2Doc
 */
public abstract class mp2DiscretizableData extends mp2GraphicalData
        implements mp2Discretizable, Serializable {

    static final long serialVersionUID = -7993930115355063767L;

    /**
     * Indicates whether or not the grid has changed since the
     * last time this object was discretized.
     */
    protected boolean activeCellsHaveChanged;   // should be tranisent?

    /**
     * Indicates whether or not the data have changed since the
     * list time this object was discretized.
     */
    protected boolean dataHaveChanged;  //should be transient?

    /**
     * Reference to the grid data. This is a transient variable
     * and is not serialized.
     */
    protected transient mp2AbstractGridData gridData;


    /**
     * Used by concrete subclass during construction.
     */
    public mp2DiscretizableData() {
        activeCellsHaveChanged = false;
        dataHaveChanged = false;
    }

    /**
     * Sets a reference to the grid data. This method also
     * registers this object with the grid data's register
     * to be notified when the grid has been modified.
     */
    public void setGridData(mp2AbstractGridData gridData) {
        this.gridData = gridData;
        gridData.register(this);
    }

    /**
     * Takes note of the fact that the active cells have changed.
     * This can be cause by (1) change in grid line position, or
     * (2) change in domain boundaries, which change the active
     * cells.
     */
    public void notifyActiveCellsChanged() {
        activeCellsHaveChanged = true;
    }

    /**
     * Take note of the fact that the data in this object has changed.
     * Also marks the document as changed.
     */
    public void setDataHaveChanged() {
        dataHaveChanged = true;
        doc.setChanged(true);
    }
}
