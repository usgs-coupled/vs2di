/*
 * mp2TablePanel.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.text.*;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.event.*;
import javax.swing.table.*;

/**
 * A panel that holds a scroll pane that in turn holds a table.
 */
public class mp2TablePanel extends JPanel {

    protected mp2TableModel tableModel;
    protected JTable table;
    protected JScrollPane tableScrollPane;
    protected int width;
    protected int height;

    /**
     * Creates a <code>mp2TablePanel</code> applying the specified 
     * table model to the table in the panel.
     *
     * @param  tableModel  table model that applies to the table in
     *                     this panel. 
     *
     * @see mp2TableModel
     */
    public mp2TablePanel(mp2TableModel tableModel) {
        this(tableModel, false);
    }
    
    public mp2TablePanel(mp2TableModel tableModel, boolean singleIntervalSelection) {
        this.tableModel = tableModel;
        tableModel.registerTablePanel(this);
        setLayout(new BorderLayout());

        table = new JTable(tableModel);
        table.setColumnSelectionAllowed(false);
        if (singleIntervalSelection) {
            table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        } else {
            table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        }
        table.getTableHeader().setReorderingAllowed(false);
        TableCellRenderer tcr = new DoubleRenderer();
        table.setDefaultRenderer(Double.class, tcr);

        // Put the table inside a scroll pane and add it to the panel
        tableScrollPane = new JScrollPane(table);
        tableScrollPane.setVerticalScrollBarPolicy(JScrollPane.
                VERTICAL_SCROLLBAR_ALWAYS);
        add(tableScrollPane, BorderLayout.CENTER);

        // Make the tool tips
        makeToolTips();
    }

    /**
     * Gets the table in this panel
     *
     * @return the table in this panel
     */
    public JTable getTable() {
        return table;
    }

    /**
     * Gets the scroll bar of the scroll pane in this panel
     *
     * @return the scroll bar of the scroll pane in this panel
     */
    public JScrollBar getScrollBar() {
        return tableScrollPane.getVerticalScrollBar();
    }

    /**
     * Makes tool tips for the table
     */
    public void makeToolTips() {
        for (int i=0; i<tableModel.getColumnCount(); i++) {
            String toolTipText = tableModel.getToolTipText(i);

            // If the tool tip text is not empty, then make the 
            // tool tip
            if (toolTipText.length()>0) {
                // Get the table column of the i-th column, 
                // identified by its name
                TableColumn tableColumn = 
                        table.getColumn(table.getColumnName(i));
                // Get the renderer for the header
                TableCellRenderer renderer = 
                        tableColumn.getHeaderRenderer();
                // If the renderer is an instance of 
                // DefaultTableCellRenderer, then set the tool tip text
                if (renderer instanceof DefaultTableCellRenderer) {
                    ((DefaultTableCellRenderer)renderer).setToolTipText(
                            toolTipText);
                }
            }
        }
    }

    class DoubleRenderer extends DefaultTableCellRenderer {
        NumberFormat deciformat;
        DecimalFormat sciformat;
        public DoubleRenderer() { super(); }

        public void setValue(Object value) {
            if (deciformat==null) {
                deciformat = NumberFormat.getInstance();
                deciformat.setMaximumFractionDigits(3);
                deciformat.setMinimumFractionDigits(0);
                sciformat = new DecimalFormat("0.###E0");
                setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
            }
            if (value == null) {
                setText("");   
            } else if (value instanceof Double) {
                double v = Math.abs(((Double) value).doubleValue());
                if ((v < 1000.0 && v >= .001) || (v == 0)) {
                    setText(deciformat.format(value));
                } else {
                    setText(sciformat.format(value));
                }
            } else {
                setText(sciformat.format(value));
            }
        }
    }


}