/*
 * mp2RectilinearGridWindowUnix.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.text.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.table.*;
import javax.swing.event.*;

/**
 * Displays a dialog box for user to specify grid properties.
 */
public class mp2RectilinearGridWindowUnix extends JFrame
            implements mp2RectilinearGridWindowInterface {

    protected mp2RectilinearGridView gridView;
    public mp2RectilinearGridData gridData;
    public mp2View view;

    protected JRadioButton editXCoordRadioButton;
    protected CoordinateTableModel xCoordTableModel;
    protected JTable xCoordTable;
    protected JScrollPane xCoordTableScrollPane;

    protected JRadioButton editYCoordRadioButton;
    protected CoordinateTableModel yCoordTableModel;
    protected JTable yCoordTable;
    protected JScrollPane yCoordTableScrollPane;

    protected JButton subdivideButton;
    protected JButton addXCoordButton;
    protected JButton addYCoordButton;
    protected JButton changeCoordButton;
    protected JButton deleteButton;
    protected JButton hideButton;
    protected JButton helpButton;
    protected JTextField xCoordTextField;
    protected JTextField yCoordTextField;
    protected JTextField coordTextField;
    protected String yCoordLabel;

    protected static final int X_COORD = 1;
    protected static final int Y_COORD = 2;
    protected boolean isEnabled;
    protected boolean isActivated;

    /**
     * Constructor
     */
    public mp2RectilinearGridWindowUnix(mp2RectilinearGridView gv,
            mp2RectilinearGridData gridData,
            mp2View view) {
        this(gv, gridData, view, "y");
    }

    /**
     * Constructor
     */
    public mp2RectilinearGridWindowUnix(mp2RectilinearGridView gv,
            mp2RectilinearGridData gridData,
            mp2View view, String yCoordLabel) {
        super("Grid");
        gridView = gv;
        this.gridData = gridData;
        this.view = view;
        this.yCoordLabel = yCoordLabel;
        isEnabled = true;
        isActivated = false;

        addWindowListener (new WindowAdapter() {
            public void windowActivated(WindowEvent e) {
                isActivated = true;
            }
            public void windowDeactivated(WindowEvent e) {
                isActivated = false;
            }
            public void windowClosing (WindowEvent e) {
                gridView.deselectGridWindowButton();
            }
        });

        // Make a center panel to hold all the components.
        JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.setBorder(new EmptyBorder(10, 10, 0, 10));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        // Make a panel to hold both tables
        JPanel tablesPanel = new JPanel(new GridLayout(1, 2, 10, 0));
        centerPanel.add(tablesPanel, BorderLayout.CENTER);

        // Make table to show x coords
        JPanel leftPanel = new JPanel(new BorderLayout());
        leftPanel.setBorder(new CompoundBorder(
            BorderFactory.createEtchedBorder(),
            new EmptyBorder(5, 5, 5, 5)));
        tablesPanel.add(leftPanel);
        xCoordTableModel = new CoordinateTableModel(X_COORD);
        xCoordTable = new JTable(xCoordTableModel);
        xCoordTable.setColumnSelectionAllowed(false);
        xCoordTable.getTableHeader().setReorderingAllowed(false);
        xCoordTable.setPreferredScrollableViewportSize(new Dimension(160, 160));
        xCoordTable.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        xCoordTable.getSelectionModel().addListSelectionListener(
            new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    if (!e.getValueIsAdjusting() && isEnabled && isActivated) {
                        onXCoordSelectionFromTable();
                    }
                }
        });
        xCoordTableScrollPane = new JScrollPane(xCoordTable,
                        JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                        JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        leftPanel.add(xCoordTableScrollPane, BorderLayout.CENTER);
        JPanel panel = new JPanel(new FlowLayout());
        leftPanel.add(panel, BorderLayout.SOUTH);
        panel.add(xCoordTextField = new JTextField(6));
        panel.add(addXCoordButton = new JButton("Add x coord"));
        addXCoordButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onAddXCoord();
            }
        });
        xCoordTextField.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onAddXCoord();
            }
        });

        // Make table to show y coords
        JPanel rightPanel = new JPanel(new BorderLayout());
        rightPanel.setBorder(new CompoundBorder(
            BorderFactory.createEtchedBorder(),
            new EmptyBorder(5, 5, 5, 5)));
        tablesPanel.add(rightPanel);
        yCoordTableModel = new CoordinateTableModel(Y_COORD);
        yCoordTable = new JTable(yCoordTableModel);
        yCoordTable.setColumnSelectionAllowed(false);
        yCoordTable.getTableHeader().setReorderingAllowed(false);
        yCoordTable.setPreferredScrollableViewportSize(new Dimension(160, 160));
        yCoordTable.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        yCoordTable.getSelectionModel().addListSelectionListener(
            new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    if (!e.getValueIsAdjusting() && isEnabled && isActivated) {
                        onYCoordSelectionFromTable();
                    }
                }
        });
        yCoordTableScrollPane = new JScrollPane(yCoordTable,
                        JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                        JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        rightPanel.add(yCoordTableScrollPane, BorderLayout.CENTER);
        panel = new JPanel(new FlowLayout());
        rightPanel.add(panel, BorderLayout.SOUTH);
        panel.add(yCoordTextField = new JTextField(6));
        panel.add(addYCoordButton = new JButton("Add " + yCoordLabel + " coord"));
        addYCoordButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onAddYCoord();
            }
        });
        yCoordTextField.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onAddYCoord();
            }
        });

        // make a panel for buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 10, 0));
        buttonPanel.setBorder(new EmptyBorder(10, 0, 10, 0));
        centerPanel.add(buttonPanel, BorderLayout.SOUTH);

        panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.add(panel);
        panel.setBorder(new CompoundBorder(
            BorderFactory.createEtchedBorder(),
            new EmptyBorder(5, 5, 5, 5)));
        panel.add(coordTextField = new JTextField(6));
        panel.add(changeCoordButton = new JButton("Change"));
        changeCoordButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onChangeCoord();
            }
        });

        panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.add(panel);
        panel.setBorder(new CompoundBorder(
            BorderFactory.createEtchedBorder(),
            new EmptyBorder(5, 5, 5, 5)));
        panel.add(subdivideButton = new JButton("Subdivide"));
        subdivideButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                gridView.onSubdivide();
            }
        });

        panel.add(deleteButton = new JButton("Delete"));
        deleteButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                gridView.onDelete();
            }
        });

        panel.add(helpButton = new JButton("Help"));
        if (mp2App.useJavaHelp()) {
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "gridWindow", null);
        } else {
            helpButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    mp2HelpWindow.showHelpFile("gridWindow.html");
                }
            });
        }

        panel.add(hideButton = new JButton("Hide"));
        hideButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                isActivated = false;
                gridView.deselectGridWindowButton();
            }
        });
        pack();

        Dimension screenSize = getToolkit().getScreenSize();
        Dimension dlgSize = getSize();
        int left = (screenSize.width - dlgSize.width)/2;
        int top = (screenSize.height - dlgSize.height)/2;
        if (left < 0) {
            left = 0;
        }
        if (top < 0) {
            top = 0;
        }
        setLocation(left, top);
        pack();
    }


    /**
     * Deselects all x and y coordinates
     */
    public void deselectAllCoords() {
        xCoordTable.clearSelection();
        yCoordTable.clearSelection();
        coordTextField.setText("");
        coordTextField.setEnabled(false);
        changeCoordButton.setEnabled(false);
    }

    /**
     * Deselects all x coordinates
     */
    public void deselectAllXCoords() {
        xCoordTable.clearSelection();
        if (yCoordTable.getSelectedRowCount() == 0) {
            coordTextField.setText("");
            coordTextField.setEnabled(false);
            changeCoordButton.setEnabled(false);
        }
    }

    /**
     * Deselects all y coordinates
     */
    public void deselectAllYCoords() {
        yCoordTable.clearSelection();
        if (xCoordTable.getSelectedRowCount() == 0) {
            coordTextField.setText("");
            coordTextField.setEnabled(false);
            changeCoordButton.setEnabled(false);
        }
    }
    
    /**
     * Wraps the dispose method so it is accessible via the interface
     */
    public void doDispose() {
        dispose();
    }
    
    
    /**
     * Wraps the repaint method so it is accessible via the interface
     */
    public void doRepaint() {
        repaint();
    }

    /**
     * Notify tables that grid data have changed
     */
    public void fireGridDataChanged() {
        xCoordTableModel.fireTableDataChanged();
        yCoordTableModel.fireTableDataChanged();
    }

    /**
     * Gets the number of selected x coordinates
     */
    public int getSelectedXCoordCount() {
        return xCoordTable.getSelectedRowCount();
    }

    /**
     * Gets an integer array containin the indices of
     * the selected x coordinates
     */
    public int [] getSelectedXCoordIndices() {
        return xCoordTable.getSelectedRows();
    }

    /**
     * Gets the number of selected y coordinates
     */
    public int getSelectedYCoordCount() {
        return yCoordTable.getSelectedRowCount();
    }

    /**
     * Gets an integer array containin the indices of
     * the selected y coordinates
     */
    public int [] getSelectedYCoordIndices() {
        if (gridView.modelVerticalAxisUpward()) {
            int [] rowIndices = yCoordTable.getSelectedRows();
            int [] coordIndices = new int [rowIndices.length];
            int ny = gridData.getYCoordCount();
            for (int i=0; i<coordIndices.length; i++) {
                coordIndices[i] = ny - rowIndices[rowIndices.length - i - 1] - 1;
            }
            return coordIndices;
        } else {
            return yCoordTable.getSelectedRows();
        }
    }
    
    /**
     * Wraps the isVisible method so it is accessible via the interface
     */
    public boolean isWindowVisible() {
        return isVisible();
    }

    /**
     * Invoked when the "Add" button for x coord is clicked.
     * Adds an x coordinate
     */
    protected void onAddXCoord() {
        try {
            double x = Double.valueOf(xCoordTextField.getText()).doubleValue();
            double [] xCoord = gridData.getXCoords();
            for (int i=0; i<xCoord.length; i++) {
                if (x == Double.valueOf(mp2DecimalFormat.format(xCoord[i])).doubleValue()) {
                    mp2MessageBox.showMessageDialog(
                        "This x coordinate already exists",
                        "Error");
                    xCoordTextField.requestFocus();
                    xCoordTextField.selectAll();
                    requestFocus();
                    return;
                }
            }
            int row = gridView.addXCoordWithUndo(x);
            xCoordTableModel.fireTableDataChanged();
            xCoordTable.clearSelection();
            xCoordTable.scrollRectToVisible(xCoordTable.getCellRect(row, 0, true));
            xCoordTextField.setText("");
            xCoordTextField.requestFocus();
            gridView.setAddVLineButtonSelected();
            gridView.evaluateEditMenu();
            view.repaint();
        }
        catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
                    "Input Error");
            xCoordTextField.requestFocus();
            xCoordTextField.selectAll();
            requestFocus();
            return;
        }
    }

    /**
     * Invoked when the "Add" button for y coord is clicked.
     * Adds a y coordinate
     */
    protected void onAddYCoord() {
        try {
            double y = Double.valueOf(yCoordTextField.getText()).doubleValue();
            double [] yCoord = gridData.getYCoords();
            for (int i=0; i<yCoord.length; i++) {
                if (y == Double.valueOf(mp2DecimalFormat.format(yCoord[i])).doubleValue()) {
                    mp2MessageBox.showMessageDialog(
                        "This y coordinate already exists",
                        "Error");
                    yCoordTextField.requestFocus();
                    yCoordTextField.selectAll();
                    requestFocus();
                    return;
                }
            }
            int coordIndex = gridView.addYCoordWithUndo(y);
            yCoordTableModel.fireTableDataChanged();
            yCoordTable.clearSelection();
            int rowIndex;
            if (gridView.modelVerticalAxisUpward()) {
                rowIndex = gridData.getYCoordCount() - coordIndex - 1;
            } else {
                rowIndex = coordIndex;
            }
            yCoordTable.scrollRectToVisible(yCoordTable.getCellRect(rowIndex, 0, true));
            yCoordTextField.setText("");
            yCoordTextField.requestFocus();
            gridView.setAddHLineButtonSelected();
            gridView.evaluateEditMenu();
            view.repaint();
        }
        catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
                    "Input Error");
            yCoordTextField.requestFocus();
            yCoordTextField.selectAll();
            requestFocus();
            return;
        }
    }

    protected void onChangeCoord() {
        int coordIndex, row;
        double coord = 0;
        try {
            coord = Double.valueOf(coordTextField.getText()).doubleValue();
        }
        catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
                    "Input Error");
            coordTextField.requestFocus();
            coordTextField.selectAll();
            requestFocus();
            return;
        }
        row = xCoordTable.getSelectedRow();
        if (row > -1) {
            double [] xCoord = gridData.getXCoords();
            for (int i=0; i<xCoord.length; i++) {
                if (i!=row && coord == Double.valueOf(mp2DecimalFormat.format(xCoord[i])).doubleValue()) {
                    mp2MessageBox.showMessageDialog(
                        "This x coordinate already exists",
                        "Error");
                    coordTextField.requestFocus();
                    coordTextField.selectAll();
                    requestFocus();
                    return;
                }
            }
            row = gridView.changeXCoordWithUndo(coord, row);
            view.repaint();
            xCoordTableModel.fireTableDataChanged();
            xCoordTable.clearSelection();
            xCoordTable.addRowSelectionInterval(row, row);
            xCoordTable.scrollRectToVisible(xCoordTable.getCellRect(row, 0, true));
            return;
        } else {
            row = yCoordTable.getSelectedRow();
            if (gridView.modelVerticalAxisUpward()) {
                coordIndex = gridData.getYCoordCount() - row - 1;
            } else {
                coordIndex = row;
            }
            double [] yCoord = gridData.getYCoords();
            for (int i=0; i<yCoord.length; i++) {
                if (i!=coordIndex && coord == Double.valueOf(mp2DecimalFormat.format(yCoord[i])).doubleValue()) {
                    mp2MessageBox.showMessageDialog(
                        "This y coordinate already exists",
                        "Error");
                    coordTextField.requestFocus();
                    coordTextField.selectAll();
                    requestFocus();
                    return;
                }
            }
            coordIndex = gridView.changeYCoordWithUndo(coord, coordIndex);
            if (gridView.modelVerticalAxisUpward()) {
                row = gridData.getYCoordCount() - coordIndex - 1;
            } else {
                row = coordIndex;
            }
            view.repaint();
            yCoordTableModel.fireTableDataChanged();
            yCoordTable.clearSelection();
            yCoordTable.addRowSelectionInterval(row, row);
            yCoordTable.scrollRectToVisible(yCoordTable.getCellRect(row, 0, true));
            return;
        }
    }

    /**
     * Invoked when user selects x coordinate(s) from the
     * table. Note that this method is not invoked if a
     * vertical grid line is selected on the grid view.
     */
    protected void onXCoordSelectionFromTable() {
        if (xCoordTable.getSelectedRowCount() == 0) {
            return;
        }
        yCoordTable.clearSelection();
        if (xCoordTable.getSelectedRowCount() == 1) {
            coordTextField.setEnabled(true);
            coordTextField.setText(
                mp2DecimalFormat.format(gridData.getXCoordAt(
                xCoordTable.getSelectedRow())));
            changeCoordButton.setEnabled(true);
        } else {
            coordTextField.setText("");
            coordTextField.setEnabled(false);
            changeCoordButton.setEnabled(false);
        }
        gridView.setEditLineButtonSelected();
        gridView.evaluateEditMenu();
        view.repaint();  //Find a more efficient way?
    }

    /**
     * Invoked when user selects y coordinate(s) from the
     * table. Note that this method is not invoked if a
     * horizontal grid line is selected on the grid view.
     */
    protected void onYCoordSelectionFromTable() {
        if (yCoordTable.getSelectedRowCount() == 0) {
            return;
        }
        xCoordTable.clearSelection();
        if (yCoordTable.getSelectedRowCount() == 1) {
            coordTextField.setEnabled(true);
            int i = yCoordTable.getSelectedRow();
            double yValue = ((Double) yCoordTableModel.getValueAt(i, 1)).doubleValue();
            coordTextField.setText(mp2DecimalFormat.format(yValue));
            changeCoordButton.setEnabled(true);
        } else {
            coordTextField.setText("");
            coordTextField.setEnabled(false);
            changeCoordButton.setEnabled(false);
        }
        gridView.setEditLineButtonSelected();
        gridView.evaluateEditMenu();
        view.repaint();
    }

    /**
     * Selects the i-th x coordinate.
     */
    public void selectXCoord(int i) {
        xCoordTable.clearSelection();
        yCoordTable.clearSelection();
        xCoordTableScrollPane.requestFocus();
        xCoordTable.addRowSelectionInterval(i, i);
        xCoordTable.scrollRectToVisible(xCoordTable.getCellRect(i, 0, true));
        coordTextField.setEnabled(true);
        coordTextField.setText(mp2DecimalFormat.format(gridData.getXCoordAt(i)));
        changeCoordButton.setEnabled(true);
    }

    /**
     * Selects an interval of x coordinate values,
     * Starting from the a-th coordinate value and ending
     * at the b-th coordinate value
     */
    public void selectXCoordInterval(int a, int b) {
        xCoordTable.clearSelection();
        yCoordTable.clearSelection();
        xCoordTableScrollPane.requestFocus();
        xCoordTable.addRowSelectionInterval(a, b);
        if (a==b) {
            coordTextField.setEnabled(true);
            coordTextField.setText(mp2DecimalFormat.format(gridData.getXCoordAt(a)));
            changeCoordButton.setEnabled(true);
        } else {
            coordTextField.setText("");
            coordTextField.setEnabled(false);
            changeCoordButton.setEnabled(false);
        }
    }

    /**
     * Selects the i-th y coordinate.
     */
    public void selectYCoord(int coordIndex) {
        xCoordTable.clearSelection();
        yCoordTable.clearSelection();
        yCoordTableScrollPane.requestFocus();
        int row;
        if (gridView.modelVerticalAxisUpward()) {
            row = gridData.getYCoordCount() - coordIndex - 1;
        } else {
            row = coordIndex;
        }
        yCoordTable.addRowSelectionInterval(row, row);
        yCoordTable.scrollRectToVisible(yCoordTable.getCellRect(row, 0, true));
        coordTextField.setEnabled(true);
        double yValue = ((Double) yCoordTableModel.getValueAt(row, 1)).doubleValue();
        coordTextField.setText(mp2DecimalFormat.format(yValue));
        changeCoordButton.setEnabled(true);
    }

    /**
     * Selects an interval of y coordinate values,
     * Starting from the a-th coordinate value and ending
     * at the b-th coordinate value
     */
    public void selectYCoordInterval(int coordIndexMin, int coordIndexMax) {
        xCoordTable.clearSelection();
        yCoordTable.clearSelection();
        xCoordTableScrollPane.requestFocus();
        int rowMin, rowMax;
        if (gridView.modelVerticalAxisUpward()) {
            rowMin = gridData.getYCoordCount() - coordIndexMax - 1;
            rowMax = gridData.getYCoordCount() - coordIndexMin - 1;
        } else {
            rowMin = coordIndexMin;
            rowMax = coordIndexMax;
        }
        yCoordTable.addRowSelectionInterval(rowMin, rowMax);
        if (rowMin==rowMax) {
            coordTextField.setEnabled(true);
            double yValue = ((Double) yCoordTableModel.getValueAt(rowMin, 1)).doubleValue();
            coordTextField.setText(mp2DecimalFormat.format(yValue));
            changeCoordButton.setEnabled(true);
        } else {
            coordTextField.setEnabled(false);
            coordTextField.setText("");
            changeCoordButton.setEnabled(false);
        }
    }

    /**
     * Enables or disables the "Delete" button.
     * Call by the grid view to synchronize the
     * "Delete" button with the "Delete" menu item.
     */
    public void setDeleteButtonEnabled(boolean b) {
        deleteButton.setEnabled(b);
    }

    /**
     * Enables or diables the grid window
     */
    public void setEnabled(boolean b) {
        isEnabled = b;
        if (!b) {
            xCoordTable.clearSelection();
            yCoordTable.clearSelection();
            coordTextField.setText("");
            coordTextField.setEnabled(false);
            changeCoordButton.setEnabled(false);
        }
        xCoordTextField.setText("");
        xCoordTextField.setEnabled(b);
        yCoordTextField.setText("");
        yCoordTextField.setEnabled(b);
        addXCoordButton.setEnabled(b);
        addYCoordButton.setEnabled(b);
        xCoordTable.setRowSelectionAllowed(b);
        yCoordTable.setRowSelectionAllowed(b);
        gridView.evaluateEditMenu();
    }

    /**
     * Enables or disables the "Subdivide" button.
     * Call by the grid view to synchronize the
     * "Subdivide" button with the "Subdivide" menu item.
     */
    public void setSubdivideButtonEnabled(boolean b) {
        subdivideButton.setEnabled(b);
    }
    
    /**
     * Wraps the setVisible method so it is accessible via the interface
     */
    public void setWindowVisible(boolean b) {
        setVisible(b);
    }

    /**
     * Table model for x and y coordinates and spacings
     */
    class CoordinateTableModel extends AbstractTableModel {

        int type;

        CoordinateTableModel(int type) {
            this.type = type;
        }

        public int getColumnCount() {
            return 3;
        }

        public String getColumnName (int c) {
            switch (c) {
            case 0:
                return "Index";
            case 1:
                if (type == X_COORD) {
                    return "x coord";
                }
                else {
                    return yCoordLabel + " coord";
                }
            case 2:
                return "spacing";
            default:
                return null;
            }
        }

        public int getRowCount() {
            if (type == X_COORD) {
                return gridData.getXCoordCount();
            } else {
                return gridData.getYCoordCount();
            }
        }

        public Object getValueAt(int row, int col) {
            int nx = gridData.getXCoordCount();
            int ny = gridData.getYCoordCount();
            boolean vertAxisUpward = 
                (view.getModelVerticalAxisOrientation() ==
                            mp2Drawing.MODEL_VERTICAL_AXIS_UPWARD_POSITIVE);
            switch (col) {
            case 0:
                if (type == Y_COORD && vertAxisUpward) {
                    return new Integer(ny - row);
                } else {
                    return new Integer(row+1);
                }
            case 1:

                if (type == X_COORD) {
                    if (row >= nx) {
                        return null;
                    } else {
                        return Double.valueOf(mp2DecimalFormat.format(
                            gridData.getXCoordAt(row)));
                    }
                }
                else {
                    if (row >= ny) {
                        return null;
                    } else if (vertAxisUpward) {
                        return Double.valueOf(mp2DecimalFormat.format(
                            gridData.getYCoordAt(ny - row - 1)));
                    } else {
                        return Double.valueOf(mp2DecimalFormat.format(
                            gridData.getYCoordAt(row)));
                    }
                }
            case 2:
                if (type == X_COORD) {
                    if (row >= nx-1) {
                        return null;
                    } else {
                        return Double.valueOf(mp2DecimalFormat.format(
                            gridData.getXCoordAt(row+1) - gridData.getXCoordAt(row)));
                    }
                }
                else {
                    if (vertAxisUpward) {
                        if (row <= 0) {
                            return null;
                        } else {
                            return Double.valueOf(mp2DecimalFormat.format(
                                gridData.getYCoordAt(ny - row) - gridData.getYCoordAt(ny - row - 1)));
                        }
                    } else {
                        if (row >= ny-1) {
                            return null;
                        } else {
                            return Double.valueOf(mp2DecimalFormat.format(
                                gridData.getYCoordAt(row+1) - gridData.getYCoordAt(row)));
                        }
                    }
                }
            default:
                return null;
            }
        }

        public void fireTableDataChanged() {
            super.fireTableDataChanged();
        }
    }
}
