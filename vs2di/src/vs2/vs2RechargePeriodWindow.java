/*
 * vs2RechargePeriodWindow.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 * Encapsulates a window to show recharge period data in a table
 */
public class vs2RechargePeriodWindow extends mp2TableWindow
        implements vs2Constants {

    protected vs2ModelOptions modelOptions;
    protected vs2BoundaryConditionsData boundaryConditionsData;
    protected vs2BoundaryConditionsView boundaryConditionsView;
    protected vs2FluidSourceData fluidSourceData;

    protected JPanel centerPanel;
    protected JPanel expandPanel;
    protected JButton shrinkButton;
    protected JButton expandButton;
    protected int shrunkWidth = 200;
    protected int expandedWidth = 600;
    protected boolean isExpanded;
    protected mp2TablePanel fullTablePanel;
    protected mp2TableModel fullTableModel;
    protected mp2TablePanel smallTablePanel;
    protected mp2TableModel smallTableModel;

    protected static final int [] PERIOD_LENGTH_MASK = {0, 1};

    protected static final int [] NO_EVAP_NO_TRAN_MASK =
                                {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    protected static final int [] YES_EVAP_NO_TRAN_MASK =
                                {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};

    protected static final int [] NO_EVAP_YES_TRAN_MASK =
                                {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12};

    protected static final int [] YES_EVAP_YES_TRAN_MASK =
                                {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};

    public vs2RechargePeriodWindow(mp2Frame frame,
                    vs2RechargePeriodData tableData,
                    vs2BoundaryConditionsData boundaryConditionsData,
                    vs2FluidSourceData fluidSourceData,
                    vs2ModelOptions modelOptions) {
        // Call super class constructor
        super(frame, "Recharge Period", new Point(220, 60), tableData);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "rechargePeriods", null);

        this.boundaryConditionsData = boundaryConditionsData;
        this.fluidSourceData = fluidSourceData;

        // At the start, the table is expanded
        isExpanded = true;

        // Create a shrink button and add it to the right panel for buttons
        buttonPanelRight.add(shrinkButton = new JButton("Shrink"));
        shrinkButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onShrink();
            }
        });

        // Create an expand button for use later
        expandPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        expandPanel.add(expandButton = new JButton("Expand"));
        expandButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onExpand();
            }
        });

        // Create the table models and panel
        fullTableModel = new mp2TableModel(tableData);
        fullTablePanel = new mp2TablePanel(fullTableModel, true);
        smallTableModel = new mp2TableModel(tableData);
        smallTableModel.setColumnMask(PERIOD_LENGTH_MASK);
        smallTablePanel = new mp2TablePanel(smallTableModel, false);

        // Put the table panel in the window
        getContentPane().add(fullTablePanel, BorderLayout.CENTER);

        // Add listeners when user selects
        JTable table = fullTablePanel.getTable();
        ListSelectionModel rowSM = table.getSelectionModel();
        rowSM.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                onRowSelection();
            }
        });
        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent me) {
                JTable table = (JTable)me.getSource();
                Point pt = me.getPoint();
                int row = table.rowAtPoint(pt);
                if (me.getClickCount() == 2) {
                    onEdit();                    
                }
            }
        });
        table = smallTablePanel.getTable();
        rowSM = table.getSelectionModel();
        rowSM.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                onRowSelection();
            }
        });
        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent me) {
                JTable table = (JTable)me.getSource();
                Point pt = me.getPoint();
                int row = table.rowAtPoint(pt);
                if (me.getClickCount() == 2) {
                    onEdit();                    
                }
            }
        });

        // Set the active table. Althought there is only one table in the
        // window, this step is needed for the superclass.
        activeTable = fullTablePanel.getTable();

        // select first row (if exists) at beginning
        if (tableData.getNumberOfRows() > 0) {
            activeTable.setRowSelectionInterval(0, 0);
            boundaryConditionsData.setSelectedPeriod(0);
        }

        // Update the table
        UpdateTable(modelOptions);

        // Pack the window.
        pack();
    }

    protected int addRow(int r) {

        // Create a dialog box for user to add a new row of data
        Object [] customObjects = new Object[2];
        customObjects[0] = modelOptions;
        customObjects[1] = new Boolean(false);
        vs2RechargePeriodDialog dlg = new vs2RechargePeriodDialog(
        "Add Recharge Period", customObjects);

        // Create a new row of data and set it in the dialog box
        if (r < 0) {
            dlg.aRow = tableData.createDefaultRow();
        } else {
            dlg.aRow = (Object []) tableData.getRow(r).clone();
        }

        // Show the dialog box for user to enter data
        if (dlg.doModal() == true) {
            // Add the row(s) of data from the dialog box to the table data
            for (int i=0; i<dlg.repetitions; i++) {
                tableData.addRow(r+i, (Object []) dlg.aRow.clone());
                boundaryConditionsData.addPeriodAfter(r+i);
                fluidSourceData.insertPeriodAt(r+i+1);
            }
            boundaryConditionsData.setSelectedPeriod(r+dlg.repetitions);
            frame.setInformationText(RECHARGE_PERIOD,
                                    "Recharge Period " + (r+dlg.repetitions+1));
            boundaryConditionsView.evaluateEditMenu();
            return dlg.repetitions;
        }
        else {
            return 0;
        }
    }

    /**
     * Edit the the specified row.
     */
    protected void editRows(int [] r) {
        // Create dialog box for user to edit data
        Object [] customObjects = new Object[2];
        customObjects[0] = modelOptions;
        customObjects[1] = new Boolean(true);
        String title;
        if (r.length > 1) {
            title = "Edit Recharge Periods " + (r[0]+1) + " - " + (r[0]+r.length);;
        } else {
            title = "Edit Recharge Period " + (r[0]+1);
        }
        vs2RechargePeriodDialog dlg = new vs2RechargePeriodDialog(
            title, customObjects);
        int i, j;

        if (r.length == 1) {
            // Get the selected row of data and put it in the dialog box
            dlg.aRow = tableData.getRow(r[0]);

            // Show the dialog box for user to edit the data
            dlg.multiplePeriods = false;
            if (dlg.doModal()==true) {
                // Set the edited row of data to the table data.
                tableData.replaceRow(dlg.aRow, r[0]);
            }
        } else {
            dlg.aRow = (Object []) tableData.getRow(r[0]).clone();
            dlg.multiplePeriods = true;
            dlg.minPeriodLength = ((Double) dlg.aRow[1]).doubleValue();
            dlg.periodHavingMinPeriodLength = r[0]+1;
            dlg.maxInitialTimeStep = ((Double) dlg.aRow[2]).doubleValue();
            dlg.periodHavingMaxInitialTimeStep = r[0]+1;
            dlg.minMaxTimeStep = ((Double) dlg.aRow[4]).doubleValue();
            dlg.periodHavingMinMaxTimeStep = r[0]+1;
            dlg.maxMinTimeStep = ((Double) dlg.aRow[5]).doubleValue();
            dlg.periodHavingMaxMinTimeStep = r[0]+1;
            for (i=1; i<r.length; i++) {
                Object [] aRow = tableData.getRow(r[i]);
                for (j=1; j<=9; j++)
                {
                    double value = ((Double) dlg.aRow[j]).doubleValue();
                    if (value != Double.MIN_VALUE) {
                        if (value != ((Double) aRow[j]).doubleValue()) {
                            dlg.aRow[j] = new Double(Double.MIN_VALUE);
                        }
                    }
                }
                if (dlg.minPeriodLength > ((Double) aRow[1]).doubleValue()) {
                    dlg.minPeriodLength = ((Double) aRow[1]).doubleValue();
                    dlg.periodHavingMinPeriodLength = r[i]+1;
                }
                if (dlg.maxInitialTimeStep < ((Double) aRow[2]).doubleValue()) {
                    dlg.maxInitialTimeStep = ((Double) aRow[2]).doubleValue();
                    dlg.periodHavingMaxInitialTimeStep = r[i]+1;
                }
                if (dlg.minMaxTimeStep > ((Double) aRow[4]).doubleValue()) {
                    dlg.minMaxTimeStep = ((Double) aRow[4]).doubleValue();
                    dlg.periodHavingMinMaxTimeStep = r[i]+1;
                }
                if (dlg.maxMinTimeStep < ((Double) aRow[5]).doubleValue()) {
                    dlg.maxMinTimeStep = ((Double) aRow[5]).doubleValue();
                    dlg.periodHavingMaxMinTimeStep = r[i]+1;
                }
            }
            if (dlg.doModal()==true) {
                for (i=0; i<r.length; i++)
                {
                    Object [] aRow = (Object []) tableData.getRow(r[i]).clone();
                    for (j=1; j<=9; j++) {
                        double value = ((Double) dlg.aRow[j]).doubleValue();
                        if (value != Double.MIN_VALUE) {
                            aRow[j] = new Double(value);
                        }
                    }
                    for (j=10; j<=12; j++){
                        aRow[j] = new Boolean(((Boolean) dlg.aRow[j]).booleanValue());
                    }
                    tableData.replaceRow(aRow, r[i]);
                }
            }
        }
    }

    /**
     * Invoked when the "Delete" button is clicked
     */
    protected boolean onDelete() {
        int [] r = activeTable.getSelectedRows();
        if (super.onDelete() == true) {
            for (int i=0; i<r.length; i++) {
                boundaryConditionsData.deletePeriod(r[0]);
                fluidSourceData.deletePeriodAt(r[0]);
            }
            if (tableData.getNumberOfRows() == 0) {
                boundaryConditionsData.setSelectedPeriod(-1);
            }
            else {
                boundaryConditionsData.setSelectedPeriod(activeTable.getSelectedRow());
            }
            boundaryConditionsView.evaluateEditMenu();
            return true;
        } else {
            return false;
        }
    }

    /**
     * Expand the window to show flow and transport properties
     */
    protected void onExpand() {
        int r = activeTable.getSelectedRow();
        getContentPane().removeAll();
        getContentPane().add(fullTablePanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        activeTable = fullTablePanel.getTable();
        if (r > -1) {
            activeTable.clearSelection();
            activeTable.addRowSelectionInterval(r, r);
        }
        Dimension dim = getSize();
        shrunkWidth = dim.width;
        width = expandedWidth;
        height = dim.height;
        Point loc = getLocationOnScreen();
        setLocation(loc.x - expandedWidth + shrunkWidth, loc.y);
        getContentPane().validate();
        pack();
        isExpanded = true;
    }

    /**
     * Override the onBrowserHelp() method of the super class
     */
    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile ("rechargePeriods.html");
    }

    /**
     * Implementation of abstract method in super class.
     * Hide the window.
     */
    protected void onHide() {
        // Hide the window by delelecting the Show|Recharge Period Window
        // menu item on the menu bar. The event handling procedure of the
        // menu item takes care of hiding the window.
        frame.getMenuItem(RECHARGE_PERIOD).setSelected(false);;
    }

    /**
     * Invoked when the row selection has changed in the table
     */
    protected void onRowSelection() {
        int r = activeTable.getSelectedRow();
        if (r > -1) {
            boundaryConditionsData.setSelectedPeriod(r);
            frame.setInformationText(RECHARGE_PERIOD,
                                    "Recharge Period " + (r+1));
            if (boundaryConditionsView != null) {
                boundaryConditionsView.clearSelectedSegments();
            }
        }
        if (activeTable.getSelectedRows().length > 1) {
            addButton.setEnabled(false);
        } else {
            if (isEnabled) {
                addButton.setEnabled(true);
            }
        }
    }

    /**
     * Handle the shrink/expand button
     */
    protected void onShrink() {
        // Get the selected row from the previously active table
        int r = activeTable.getSelectedRow();
        getContentPane().removeAll();
        getContentPane().add(smallTablePanel, BorderLayout.CENTER);
        getContentPane().add(expandPanel, BorderLayout.SOUTH);
        activeTable = smallTablePanel.getTable();
        if (r > -1) {
            activeTable.clearSelection();
            activeTable.addRowSelectionInterval(r, r);
        }
        Dimension dim = getSize();
        expandedWidth = dim.width;
        width = shrunkWidth;
        height = dim.height;
        Point loc = getLocationOnScreen();
        setLocation(loc.x + expandedWidth - shrunkWidth, loc.y);
        getContentPane().validate();
        pack();
        isExpanded = false;
    }

    /**
     * Update the table
     */
    public void UpdateTable(vs2ModelOptions modelOptions) {
        // Save the new model options
        this.modelOptions = modelOptions;

        // Remember the selected row
        int r = activeTable.getSelectedRow();

        if (modelOptions.doEvaporation && modelOptions.doTranspiration) {
            fullTableModel.setColumnMask(YES_EVAP_YES_TRAN_MASK);
        }
        else if (modelOptions.doEvaporation && !modelOptions.doTranspiration) {
            fullTableModel.setColumnMask(YES_EVAP_NO_TRAN_MASK);
        }
        else if (!modelOptions.doEvaporation && modelOptions.doTranspiration) {
            fullTableModel.setColumnMask(NO_EVAP_YES_TRAN_MASK);
        }
        else if (!modelOptions.doEvaporation && !modelOptions.doTranspiration) {
            fullTableModel.setColumnMask(NO_EVAP_NO_TRAN_MASK);
        }

        // If a row was previously selected, select it again. This is needed because
        // the above call to setColumnMask has cleared the previous selection.
        if (r > -1) {
            activeTable.clearSelection();
            activeTable.addRowSelectionInterval(r, r);
        }
    }

    public void setBoundaryConditionsView(vs2BoundaryConditionsView bcView) {
        boundaryConditionsView = bcView;
    }

}
