/*
 * vs2TexturalClassWindow.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 * Displays textural class data in a table.
 */
public class vs2TexturalClassWindow extends mp2TableWindow
    implements vs2Constants, mp2ColorCodeWindow {

    protected mp2App               theApp;
    protected vs2ModelOptions modelOptions;
    protected JTabbedPane tabbedPane;
    protected JPanel centerPanel;
    protected JPanel expandPanel;
    protected JButton customButton;
    protected JButton shrinkButton;
    protected JButton expandButton;
    protected int shrunkWidth = 200;
    protected int expandedWidth = 625;
    protected boolean isExpanded;
    protected mp2TablePanel colorAndNamePanel;
    protected mp2TablePanel transportPanel;
    protected mp2TablePanel flowPanel;
    protected mp2TableModel colorAndNameModel;
    protected mp2TableModel transportModel;
    protected mp2TableModel flowModel;

    protected static final int [] COLOR_AND_NAME_MASK = {1, 2};

    protected static final int [] BROOKS_COREY_MASK =
                                {1, 2, 3, 8, 4, 5, 6, 9, 10};

    protected static final int [] VAN_GENUCHTEN_MASK =
                                {1, 2, 3, 11, 4, 5, 6, 12, 13};

    protected static final int [] HAVERKAMP_MASK =
                                {1, 2, 3, 14, 4, 5, 6, 15, 16, 17, 18};

    protected static final int [] ROSSI_NIMMO_MASK =
                                {1, 2, 3, 38, 4, 5, 39, 40, 41};

    protected static final int [] TABULAR_DATA_MASK =
                                {1, 2, 3, 19, 4, 5};

    protected static final int [] NO_ADSORPTION_NO_ION_EXCHANGE_MASK =
                                {1, 2, 21, 22, 23, 24};

    protected static final int [] LINEAR_ADSORPTION_MASK =
                                {1, 2, 21, 22, 23, 24, 25, 26};

    protected static final int [] LANGMUIR_MASK =
                                {1, 2, 21, 22, 23, 24, 25, 27, 28};

    protected static final int [] FREUNDLICH_MASK =
                                {1, 2, 21, 22, 23, 24, 25, 29, 30};

    protected static final int [] ION_EXCHANGE_MASK =
                                {1, 2, 21, 22, 23, 24, 25, 31, 32, 33};

    protected static final int [] ENERGY_TRANSPORT_MASK =
                                {1, 2, 21, 22, 34, 35, 36, 37};

    /**
     * Constructor
     */
    public vs2TexturalClassWindow(mp2Frame frame,
                        vs2TexturalClassData tableData,
                        vs2ModelOptions modelOptions,
                        mp2App app) {
        // Call super class constructor
        super(frame, "Textural Class", new Point(260, 80), tableData);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "texturalClasses", null);
        buttonPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10));

        theApp = app;

        // For Opening and Saving Textural Class data to and from files.
        buttonPanelRight.add(customButton = new JButton("Custom"));
        customButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCustomButton();
            }
        });

        // At the start, the table is expanded
        isExpanded = true;
        width = expandedWidth;

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

        // Create table models and panels
        colorAndNameModel = new mp2TableModel(tableData);
        colorAndNameModel.setColumnMask(COLOR_AND_NAME_MASK);
        colorAndNamePanel = new mp2TablePanel(colorAndNameModel);
        flowModel = new mp2TableModel(tableData);
        flowPanel = new mp2TablePanel(flowModel);
        transportModel = new mp2TableModel(tableData);
        transportPanel = new mp2TablePanel(transportModel);

        // Add listeners when user selects
        JTable table = colorAndNamePanel.getTable();
        ListSelectionModel rowSM = table.getSelectionModel();
        rowSM.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                onRowSelection();
            }
        });
        table = flowPanel.getTable();
        rowSM = table.getSelectionModel();
        rowSM.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                onRowSelection();
            }
        });
        table = transportPanel.getTable();
        rowSM = table.getSelectionModel();
        rowSM.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                onRowSelection();
            }
        });

        // Create a tabbed pane to hold the table panels
        tabbedPane = new JTabbedPane();
        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        tabbedPane.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                OnTabChange();
            }
        });

        // Add the flow panel, which is always present
        tabbedPane.add(flowPanel, 0);

        // Set the minimum row count--There is always a row for the default
        // textural class
        minRowCount = 1;

        // Initially the active table is the flow table, and the default row
        // is selected.
        tabbedPane.setSelectedIndex(0);
        activeTable = flowPanel.getTable();
        activeTable.addRowSelectionInterval(0, 0);
        editButton.setEnabled(false);
        deleteButton.setEnabled(false);

        // Update the tabs
        UpdateTabs(modelOptions);

        // Pack the window.
        validate();
        pack();
    }

    /**
     * Concrete implementation of abstract method in super class.
     * Add a textural class via dialog box
     */
    protected int addRow(int r) {
        // Create a dialog box for user to add a new row of data
        vs2TexturalClassDialog dlg;
        if (modelOptions.soilModel == TABULAR_DATA) {
            dlg = new vs2TabularDataDialog("Add Textural Class",
                                            modelOptions, false);
        } else {
            dlg = new vs2SoilFunctionDialog("Add Textural Class",
                                             modelOptions, false);
        }

        // Create a new row of data and set it in the dialog box
        dlg.aRow = tableData.createDefaultRow();

        // Show the dialog box for user to enter data
        if (dlg.doModal() == true) {
            // Add the row of data from the dialog box to the table data
            tableData.addRow(r, dlg.aRow);
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Implementation of abstract method in super class.
     * Edit the the specified row.
     */
    protected void editRows(int [] r) {
        // Disallow editing the first row
        if (r[0] == 0) {
            mp2MessageBox.showMessageDialog(
                            "Cannot edit default textural class.",
                            "Warning");
            return;
        }

        // Create dialog box for user to edit data
        vs2TexturalClassDialog dlg;
        if (modelOptions.soilModel == TABULAR_DATA) {
            dlg = new vs2TabularDataDialog("Edit Textural Class",
                                            modelOptions, true);
        } else {
            dlg = new vs2SoilFunctionDialog("Edit Textural Class",
                                             modelOptions, true);
        }

        // Put current dat in dialog box and show it
        dlg.aRow = tableData.getRow(r[0]);

        if (dlg.doModal()==true) {
            // Set the edited row of data to the table data.
            tableData.replaceRow(dlg.aRow, r[0]);

            // Repaint the frame (main application window), which should repaint
            // the view within the frame also
            frame.getContentPane().repaint();
        }
    }

    /**
     * Implementation of method required by mp2ColorCodeWindow interface.
     * Return the color corresponding to the textural class id
     */
    public Color getColorOfId(int id) {
        // Search through the textural class ids to find the matching id
        for (int i=0; i<tableData.getNumberOfRows(); i++) {
            Object [] aRow = tableData.getRow(i);
            if (id == ((Integer) aRow[0]).intValue()) {
                return (Color) aRow[1];
            }
        }
        // If the id is not found, return null
        return null;
    }

    /**
     * Implementation of method required by mp2ColorCodeWindow interface.
     * Return the textural class id of the selected row.
     */
    public int getIdOfSelectedRow() {
        Object [] aRow = tableData.getRow(activeTable.getSelectedRow());
        return ((Integer) aRow[0]).intValue();
    }

    /**
     * Implementation of method required by mp2ColorCodeWindow interface.
     * Return the row index corresponding to the textural class id. Returns -1
     * if id is not found.
     */
    public int getRowIndexOfId(Integer id) {
        // Search through the textural class ids to find the matching id
        for (int i=0; i<tableData.getNumberOfRows(); i++) {
            Object [] aRow = tableData.getRow(i);
            if (id == aRow[0]) {
                return i;
            }
        }

        // If the id is not found, return -1
        return -1;
    }

    /**
     * Invoked when the custom button is clicked to load user's textural class data
     */
    protected void onCustomButton() {
		vs2TexturalClassCustomDialog dlg;
		dlg = new vs2TexturalClassCustomDialog (theApp,
                    (vs2TexturalClassData) tableData, modelOptions.soilModel,
                    activeTable.getSelectedRow());
		dlg.setVisible(true);
    }

    /**
     * Override the superclass method to prevent deletion of the first row
     */
    protected boolean onDelete() {
        // Disallow deletion of the first row
        boolean b = false;
        if (activeTable.getSelectedRow() == 0) {
            mp2MessageBox.showMessageDialog(
                            "Cannot delete default textural class.",
                            "Warning");
            return false;
        }
        else {
            b = super.onDelete();
        }

        // Repaint the frame, which should repaint the view within the frame also
        frame.getContentPane().repaint();
        return b;
    }

    /**
     * Expand the window to show flow and transport properties
     */
    protected void onExpand() {
        int r = activeTable.getSelectedRow();
        getContentPane().removeAll();
        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        tabbedPane.setSelectedIndex(0);
        activeTable = flowPanel.getTable();
        activeTable.clearSelection();
        activeTable.addRowSelectionInterval(r, r);
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
        mp2HelpWindow.showHelpFile ("texturalClasses.html");
    }

    /**
     * Implementation of abstract method in super class. Hide the window.
     * This is equivalent to deselecting the Show|Textural Class Window menu
     * item.
     */
    protected void onHide() {
        // Hide the window by delelecting the Show|Textural Class Window
        // menu item on the menu bar. The event handling procedure of the
        // menu item takes care of hiding the window.
        frame.getMenuItem(TEXTURAL_CLASS).setSelected(false);
    }

    /**
     * Invoked when the row selection has changed in the table
     */
    protected void onRowSelection() {
        if (!isEnabled) {
            return;
        }
        int r = activeTable.getSelectedRow();
        editButton.setEnabled(r > 0);
        deleteButton.setEnabled(r > 0);
    }

    /**
     * Handle the shrink/expand button
     */
    protected void onShrink() {
        // Get the selected row from the previously active table
        int r = activeTable.getSelectedRow();
        getContentPane().removeAll();
        getContentPane().add(colorAndNamePanel, BorderLayout.CENTER);
        getContentPane().add(expandPanel, BorderLayout.SOUTH);
        activeTable = colorAndNamePanel.getTable();
        activeTable.clearSelection();
        activeTable.addRowSelectionInterval(r, r);
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
     * Handle selection of tabbed pane. Set the active table, and synchronize
     * selected row and scroll bar value of new and old active tables
     */
    protected void OnTabChange() {
        if (activeTable == null) {
            return;
        }

        // Get the selected row from the previously active table
        int r = activeTable.getSelectedRow();

        // Set the new active table and synchronize the scroll bars
        if (tabbedPane.getSelectedIndex() == 0) {
            activeTable = flowPanel.getTable();
            flowPanel.getScrollBar().setValue(
                        transportPanel.getScrollBar().getValue());
        } else {
            activeTable = transportPanel.getTable();
            transportPanel.getScrollBar().setValue(
                        flowPanel.getScrollBar().getValue());
        }

        // Set the selected row in the new active table
        activeTable.clearSelection();
        activeTable.addRowSelectionInterval(r, r);
    }

    /**
     * Update which tabs are shown
     */
    public void UpdateTabs(vs2ModelOptions modelOptions) {
        // Save the new model options
        this.modelOptions = modelOptions;

        // Get the previously selected panel
        int i = tabbedPane.getSelectedIndex();

        // Remember the selected row so we can select
        // the same row at the end of the update
        int r = activeTable.getSelectedRow();

        // Put the appropriate title in the flow tab
        switch (modelOptions.soilModel) {
        case BROOKS_COREY:
            tabbedPane.setTitleAt(0, "Flow (Brooks-Corey function)");
            flowModel.setColumnMask(BROOKS_COREY_MASK);
            break;
        case VAN_GENUCHTEN:
            tabbedPane.setTitleAt(0, "Flow (van Genuchten function)");
            flowModel.setColumnMask(VAN_GENUCHTEN_MASK);
            break;
        case HAVERKAMP:
            tabbedPane.setTitleAt(0, "Flow (Haverkamp function)");
            flowModel.setColumnMask(HAVERKAMP_MASK);
            break;
        case ROSSI_NIMMO:
            tabbedPane.setTitleAt(0, "Flow (Rossi-Nimmo function)");
            flowModel.setColumnMask(ROSSI_NIMMO_MASK);
            break;
        case TABULAR_DATA:
            tabbedPane.setTitleAt(0, "Flow (tabular data)");
            flowModel.setColumnMask(TABULAR_DATA_MASK);
            break;
        }
        // If simulating transport, add transport panel if not present, and set title
        if (modelOptions.doTransport) {
            if (tabbedPane.getTabCount() == 1) tabbedPane.add(transportPanel, 1);
            if (vs2App.doHeat()) {
                tabbedPane.setTitleAt(1, "Transport");
                transportModel.setColumnMask(ENERGY_TRANSPORT_MASK);
            } else {
                switch (modelOptions.reactionOption)
                {
                case N0_ADSORPTION_NO_ION_EXCHANGE:
                    tabbedPane.setTitleAt(1, "Transport");
                    transportModel.setColumnMask(NO_ADSORPTION_NO_ION_EXCHANGE_MASK);
                    break;
                case LINEAR_ADSORPTION:
                    tabbedPane.setTitleAt(1, "Transport (with linear adsorption)");
                    transportModel.setColumnMask(LINEAR_ADSORPTION_MASK);
                    break;
                case LANGMUIR:
                    tabbedPane.setTitleAt(1, "Transport (with Langmuir isotherm)");
                    transportModel.setColumnMask(LANGMUIR_MASK);
                    break;
                case FREUNDLICH:
                    tabbedPane.setTitleAt(1, "Transport (with Freundlich isotherm)");
                    transportModel.setColumnMask(FREUNDLICH_MASK);
                    break;
                case MONO_MONOVALENT_ION_EXCHANGE:  // fall through
                case MONO_DIVALENT_ION_EXCHANGE:    // fall through
                case DI_MONOVALENT_ION_EXCHANGE:    // fall through
                case DI_DIVALENT_ION_EXCHANGE:
                    tabbedPane.setTitleAt(1, "Transport (with ion exchange)");
                    transportModel.setColumnMask(ION_EXCHANGE_MASK);
                    break;
                }
            }
        }
        // If not simulating transport, make sure transport tab is removed
        else {
            if (tabbedPane.getTabCount() == 2) {
                tabbedPane.removeTabAt(1);
            }
        }

        // If the table is shrunk, then no need to set tabs and active table.
        if (!isExpanded) {
            return;
        }

        // Select the tab and set the active table
        if (modelOptions.doTransport) {
            tabbedPane.setSelectedIndex(i);
            if (i==0) {
                activeTable = flowPanel.getTable();
            } else {
                activeTable = transportPanel.getTable();
            }
        } else {
            tabbedPane.setSelectedIndex(0);
            activeTable = flowPanel.getTable();
        }

        // select the same row as before the update
        activeTable.clearSelection();
        activeTable.addRowSelectionInterval(r, r);

        // validate and repaint the window
        getContentPane().validate();
        getContentPane().repaint();
    }

    /**
     * Enable or disable the editing buttons on the window
     */
    public void setEnabled(boolean b) {
        super.setEnabled(b);
        if (b && activeTable.getSelectedRow() == 0) {
            editButton.setEnabled(false);
            deleteButton.setEnabled(false);
        }
    }
}
