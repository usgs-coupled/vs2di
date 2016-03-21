/*
 * mp2ColorCodeWindow.java
 */
package mp2;

import java.awt.*;

/**
 * Interface for an object that manipulate color-coded data 
 * contained in a table. Each color-coded data item has 
 * a color and a unique id, and occupies one row in the
 * table.
 */
public interface mp2ColorCodeWindow {

    /**
     * Gets the id of the data item in the selected row in 
     * the table.
     *
     * @return  the id of the data item in the selected row
     *          in the table.
     */
    public abstract int getIdOfSelectedRow();

    /**
     * Gets the color of the data item having the specified id.
     *
     * @param  id  the <code>int</code> value of the id.
     *
     * @return  the color of the data item having the specified id.
     */
    public abstract Color getColorOfId(int id);

    /**
     * Gets the index of the row that holds the data item 
     * having the specified id.
     *
     * @param  id  the <code>int</code> value of the id.
     *
     * @return  the index of the row that holds the data item 
     *          having the specified id.
     */
    public abstract int getRowIndexOfId(Integer id);

    /**
     * Enables or Disables editing of the data.
     *
     * @param  b  if <code>true</code> data can be edited.
     *            if <code>false</code> data cannot be edited.
     */
    public abstract void setEnabled(boolean b);

}