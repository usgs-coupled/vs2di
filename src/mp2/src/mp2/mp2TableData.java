/*
 * mp2TableData.java
 */
package mp2;

import java.io.*;
import java.util.*;

/**
 * Abstract class for defining table column headers and column class types
 */
public abstract class mp2TableData implements Serializable {

    static final long serialVersionUID = 8060408355600638805L;

    protected Vector dataRows;

    protected transient mp2Doc doc;
    protected transient Vector registeredTableModels;
    protected transient Class [] columnClass;
    protected transient String [] columnName;
    protected transient String [] toolTipText;
    protected transient boolean clearUndoOnDelete;
    protected transient boolean clearUndoOnAdd;

    /**
     * Creates a mp2TableData
     */
    public mp2TableData() {
        dataRows = new Vector();
        registeredTableModels = new Vector();
        setColumnAttributes();
    }

    public abstract Object [] createDefaultRow();

    /**
     * Define the arrays: columnClass, columnName
     * and toopTipText.
     */
    protected abstract void setColumnAttributes();

    /**
     * Add a row of data. The data will be add to the row AFTER row r.
     * For example, if r is 2, the added data will be in row 3, and the original 
     * data from row 3 onward will be shifted down one row.
     */
    public void addRow(int r, Object[] aRow) {
        // if there are no data, or if r is the last row, then add to the end 
        // of the data set
        if (dataRows.size() == 0 || r == dataRows.size()-1) { 
            dataRows.addElement(aRow);
        }

        // if r is not the last row and r <= -1, then insert the data
        // at row r+1. All original rows from r+1 to end are shifted
        // down one row.
        else if (r>=-1 && r < dataRows.size()-1) {
            dataRows.insertElementAt(aRow, r+1);
        }

        else {
            return;
        }

        // Notify registered table models that a row had been added.
        for (int i=0; i<registeredTableModels.size(); i++) {
            ((mp2TableModel) registeredTableModels.elementAt(i)).
                    notifyRowAdded(r);
        }

        // Mark the document as changed.
        if (doc != null) {
            if (clearUndoOnAdd) {
                doc.setChangedAndClearUndo();
            } else {
                doc.setChangedWithNoUndo();
            }
        }
    }

    /**
     * Delete the specified row of data
     */
    public void deleteRow(int r) {
        // Return if row index is out of bounds
        if (r < 0 || r >= dataRows.size()) {
            return;
        }

        // Remove the row of data
        dataRows.removeElementAt(r);

        // Notify registered table models that a row has been deleted
        for (int i=0; i<registeredTableModels.size(); i++) {
            ((mp2TableModel) registeredTableModels.elementAt(i)).
                                                    notifyRowDeleted(r);
        }

        // Mark the document as changed
        if (doc != null) {
            if (clearUndoOnDelete) {
                doc.setChangedAndClearUndo();
            } else {
                doc.setChangedWithNoUndo();
            }
        }
    }

    /**
     * Gets the table data
     */
    public Vector getData() {
        return dataRows;
    }

    /**
     * Return the column class.
     */
    public Class getColumnClass(int c) {
        try {
            return columnClass[c];
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return Object.class;
        }
    }

    /**
     * Gets the name of the specified column
     */
    public String getColumnName(int c) {
        if (columnName == null) return "";
        try {
            return columnName[c];
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return "";
        }
    }

    /**
     * Gets the number of columns
     */
    public int getNumberOfColumns() {
        if (columnName == null) {
            return 0;
        }
        return columnName.length;
    }

    /**
     * Gets the number of rows
     */
    public int getNumberOfRows() {
        return dataRows.size();
    }

    /**
     * Gets the object at the specified row-column location
     */
    public Object getObjectAt(int r, int c) {
        try {
            Object[] aRow = (Object[]) dataRows.elementAt(r);
            return aRow[c];
        }
        catch (ArrayIndexOutOfBoundsException e) {
            System.out.println ("ArrayIndexOutOfBoundsException in mp2TableData getObjectAt");
            return null;
        }
    }

    /**
     * Gets a row of data
     */
    public Object [] getRow(int r) {
        try {
            return (Object[]) dataRows.elementAt(r);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return null;
        }
    }

    /**
     * Gets the tool tip text for the specified column
     */
    public String getToolTipText(int c) {
        if (toolTipText == null) {
            return "";
        }
        try {
            return toolTipText[c];
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return "";
        }
    }

    /**
     * Initializes the data
     *
     * @param  doc  the document to which this object belongs
     */
    public void init(mp2Doc doc) {
        this.doc = doc;
        registeredTableModels = new Vector();
        setColumnAttributes();
        clearUndoOnDelete = false;
        clearUndoOnAdd = false;
    }
    
    
    public void insertDefaultRowAt(int row) {
        dataRows.insertElementAt(createDefaultRow(), row);
    }

    /**
     * By default the columns are not editable. 
     */
    public boolean isColumnEditable(int c) {
        return false;
    }

    /**
     * Register table models that are using this data
     */
    public void registerTableModel(mp2TableModel tableModel) {
        registeredTableModels.addElement(tableModel);
    }

    /**
     * Replace the specified row of data at the specifiec location
     */
    public void replaceRow(Object[] aRow, int r) {
        // Check that row index is within range, and column index 
        // is non negative
        if (r < 0 || r >= dataRows.size()) return;

        // replace the row of data
        dataRows.setElementAt(aRow, r);

        for (int i=0; i<registeredTableModels.size(); i++) {
            ((mp2TableModel) registeredTableModels.elementAt(i)).
                    notifyRowUpdated(r);
        }

        // mark the doc as changed
        if (doc != null) doc.setChangedWithNoUndo();
    }

    /**
     * Set new data (Use with caution! Make sure new data has the proper number
     * of columns).
     */
    public void setData(Vector dataRows) {
        this.dataRows = dataRows;

        for (int i=0; i<registeredTableModels.size(); i++) {
            ((mp2TableModel) registeredTableModels.elementAt(i)).
                    notifyStructureChanged();
        }
        if (doc != null) doc.setChangedWithNoUndo();
    }

    /**
     * Puts the specified object at the specified row-column location
     */
    public void setObjectAt(Object value, int r, int c) {
        try {
            Object [] aRow = (Object[]) dataRows.elementAt(r);
            aRow[c] = value;

            for (int i=0; i<registeredTableModels.size(); i++) {
                ((mp2TableModel) registeredTableModels.elementAt(i)).
                        notifyRowUpdated(r);
            }
        }
        catch (ArrayIndexOutOfBoundsException e) {
            System.out.println ("ArrayIndexOutOfBoundsException " + r + " " + c);
            return;
        }

        // mark the doc as changed
        if (doc != null) doc.setChangedWithNoUndo();
    }
}
