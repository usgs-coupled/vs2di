/*
 * mp2TableModel.java
 */
package mp2;

import javax.swing.table.*;
import javax.swing.event.*;

public class mp2TableModel extends AbstractTableModel {

    protected mp2TableData tableData;
    protected int [] columnMask;
    protected mp2TablePanel registeredTablePanel;

    /**
     * Creates a mp2TableModel for the specified table data.
     */
    public mp2TableModel(mp2TableData tableData) {
        this.tableData = tableData;
        tableData.registerTableModel(this);
    }

    /**
     * Gets the <code>Class</code> of the specified column
     */
    public Class getColumnClass(int c) {
        if (columnMask == null) {
            return tableData.getColumnClass(c);
        }
        try {
            return tableData.getColumnClass(columnMask[c]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return Object.class;
        }
    }

    /**
     * Gets the number of rows
     */
    public int getRowCount() {
        return tableData.getNumberOfRows();
    }

    /**
     * Gets the number of column
     */
    public int getColumnCount() {
        if (columnMask == null) {
            return tableData.getNumberOfColumns();
        }
        else {
            return columnMask.length;
        }
    }

    /**
     * Gets the name of the specified column
     */
    public String getColumnName(int c) {
        if (columnMask == null) {
            return tableData.getColumnName(c);
        }
        try {
            return tableData.getColumnName(columnMask[c]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return "";
        }
    }

    /**
     * Return the value at the specified cell
     */
    public Object getValueAt(int r, int c) {
        if (columnMask == null) {
            return tableData.getObjectAt(r,c);
        }
        try {
            return tableData.getObjectAt(r, columnMask[c]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return null;
        }
    }

    /**
     * Return the tool tip text for the specified column
     */
    public String getToolTipText(int c) {
        if (columnMask == null) {
            return tableData.getToolTipText(c);
        }
        try {
            return tableData.getToolTipText(columnMask[c]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return "";
        }
    }

    /**
     * Indicate if the specifed cell is editable.
     */
    public boolean isCellEditable(int r, int c) {
        if (columnMask == null) {
            return tableData.isColumnEditable(c);
        }
        try {
            return tableData.isColumnEditable(columnMask[c]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return false;
        }
    }

    /**
     * Notify table listeners that the specified row has 
     * been deleted
     */
    public void notifyRowDeleted(int r) {
        fireTableRowsDeleted(r, r);
    }

    /**
     * Notify table listeners that the specified row has 
     * been updated
     */
    public void notifyRowUpdated(int r) {
        fireTableRowsUpdated(r, r);
    }

    /**
     * Notify table listeners that the specified row has 
     * been added
     */
    public void notifyRowAdded(int r) {
        fireTableRowsInserted(r+1, r+1);
    }

    /**
     * Notify table listeners that the table data has changed
     */
    public void notifyDataChanged() {
        fireTableDataChanged();
    }

    /**
     * Notify table listeners that the table structure has changed
     */
    public void notifyStructureChanged() {
        fireTableStructureChanged();
        if (registeredTablePanel != null) {
            registeredTablePanel.makeToolTips();
        }
    }

    /**
     * Register the table panel that uses this table model
     */
    public void registerTablePanel(mp2TablePanel tablePanel) {
        registeredTablePanel = tablePanel;
    }

    /**
     * Sets the column mask
     */
    public void setColumnMask(int [] columnMask) {
        this.columnMask = columnMask;
        fireTableStructureChanged();
        if (registeredTablePanel != null) {
            registeredTablePanel.makeToolTips();
        }
    }

    /**
     * Sets the specified object at the specified row-column
     * location
     */
    public void setValueAt(Object value, int r, int c) {
        if (columnMask == null) {
            tableData.setObjectAt(value, r, c);
            return;
        }
        try {
            tableData.setObjectAt(value, r, columnMask[c]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
        }
    }
}
