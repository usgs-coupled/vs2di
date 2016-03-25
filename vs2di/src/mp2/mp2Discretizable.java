/*
 * mp2Discretizable.java
 */
package mp2;

public interface mp2Discretizable {

    /**
     * Sets a reference to the grid data. This method also
     * registers this object with the grid data's register
     * to be notified when the grid has been modified.
     */
    public abstract void setGridData(mp2AbstractGridData gridData);
    
    /**
     * Takes note of the fact that the active cells have changed.
     * This can be cause by (1) change in grid line position, or
     * (2) change in domain boundaries, which change the active
     * cells.
     */
    public abstract void notifyActiveCellsChanged();

    /**
     * Discretize the data in this object
     */
    public abstract boolean discretize();
}
