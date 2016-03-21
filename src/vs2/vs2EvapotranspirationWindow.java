/*
 * vs2EvapotranspirationWindow.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;

/**
 * Displays evapotranspiration data in table
 */
public class vs2EvapotranspirationWindow extends mp2TableWindow
                        implements vs2Constants {
    protected mp2App               theApp;
    protected vs2ModelOptions modelOptions;
    protected JTabbedPane tabbedPane;
    protected mp2TablePanel evaporationPanel;
    protected mp2TablePanel transpirationPanel;
    protected JButton customButton;
    protected mp2TableModel evaporationModel;
    protected mp2TableModel transpirationModel;
    protected JTextField periodLengthTextField;
    protected JButton modifyButton;
    protected static final int [] EVAPORATION_MASK = {0, 1, 2, 3};
    protected static final int [] TRANSPIRATION_MASK = {0, 4, 5, 6, 7, 8};

    /**
     * Constructor
     */
    public vs2EvapotranspirationWindow(mp2Frame frame,
                            vs2EvapotranspirationData tableData,
                            vs2ModelOptions modelOptions,
                            mp2App app) {
        // Call super class constructor
        super(frame, "Evapotranspiration", new Point(180, 40), tableData);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "evapotranspirationCycle", null);

        theApp = app;
        width = 500;

        // For Opening and Saving Evapotranspiration data to and from files.
        buttonPanelRight.add(customButton = new JButton("Custom"));
        customButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCustomButton();
            }
        });

        // Create table models and panels
        evaporationModel = new mp2TableModel(tableData);
        evaporationModel.setColumnMask(EVAPORATION_MASK);
        evaporationPanel = new mp2TablePanel(evaporationModel);
        transpirationModel = new mp2TableModel(tableData);
        transpirationModel.setColumnMask(TRANSPIRATION_MASK);
        transpirationPanel = new mp2TablePanel(transpirationModel);

        // Create a tabbed pane to hold the table panels, and put it in the window
        tabbedPane = new JTabbedPane();
        //getContentPane().add(tabbedPane, BorderLayout.CENTER);
        tabbedPane.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                OnTabChange();
            }
        });

        // Add tabs to tabbed pane
        UpdateTabs(modelOptions);

        JPanel mainPanel = new JPanel(new BorderLayout());
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        mainPanel.add(tabbedPane, BorderLayout.CENTER);

        JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        mainPanel.add(panel, BorderLayout.SOUTH);
        panel.setBorder(new EmptyBorder(10, 0, 0, 0));

        panel.add(new JLabel("Period Length:"));
        panel.add(Box.createHorizontalStrut(5));

        periodLengthTextField = new JTextField(String.valueOf(tableData.getPeriodLength()), 8);
        periodLengthTextField.setEditable (false);
        periodLengthTextField.setBackground(Color.lightGray);
        panel.add(periodLengthTextField);

        panel.add(Box.createHorizontalStrut(10));
        modifyButton = new JButton("Modify");
        panel.add(modifyButton);
        modifyButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onModifyPeriodLength();
            }
        });

        // Pack the window.
        pack();
    }

    /**
     * Concrete implementation of abstract method in super class.
     * Add an evapotranspiration period via dialog box
     */
    protected int addRow(int r) {
        // Create a dialog box for user to add a new row of data
        vs2EvapotranspirationDialog dlg = new vs2EvapotranspirationDialog(
        "Add Evapotranspiration Period", modelOptions, false);

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
        // Create dialog box for user to edit data
        vs2EvapotranspirationDialog dlg =
                        new vs2EvapotranspirationDialog(
                        "Edit Evapotranspiration Period",
                        modelOptions, true);

        // Get the selected row of data and put it in the dialog box
        dlg.aRow = tableData.getRow(r[0]);

        // Show the dialog box for user to edit the data
        if (dlg.doModal()==true) {
            // Set the edited row of data to the table data.
            tableData.replaceRow(dlg.aRow, r[0]);
        }
    }

    /**
     * Invoked when the custom button is clicked to load user's evapotranspiration data
     */
    protected void onCustomButton() {
		vs2EvapotranspirationCustomDialog dlg;
		dlg = new vs2EvapotranspirationCustomDialog (theApp, (vs2EvapotranspirationData) tableData);
		dlg.setVisible(true);
    }

    /**
     * Override superclass method
     */
    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile ("evapotranspirationCycle.html");
    }

    /**
     * Implementation of abstract method in super class.
     * Hide the window.
     */
    protected void onHide() {
        // Hide the window by delelecting the Show|Evapotranspiration Window
        // menu item on the menu bar. The event handling procedure of the
        // menu item takes care of hiding the window.
        frame.getMenuItem(EVAPOTRANSPIRATION).setSelected(false);
    }

    protected void onModifyPeriodLength() {
        vs2EvapotranspirationData etData = (vs2EvapotranspirationData) tableData;
        PeriodLengthDialog dlg = new PeriodLengthDialog();
        dlg.length = etData.getPeriodLength();
        if (dlg.doModal() == true) {
            etData.setPeriodLength(dlg.length);
            periodLengthTextField.setText(String.valueOf(dlg.length));
        }
    }

    /**
     * Handle selection of tabbed pane. Set the active table, and synchronize
     * selected row and scroll bar value of new and old active tables
     */
    protected void OnTabChange() {
        if (activeTable == null) {
            return;
        }

        // Remember the selected row from the previously active table
        int r = activeTable.getSelectedRow();

        // Set the new active table and synchronize the scroll bars
        if (tabbedPane.getSelectedIndex() == 0) {
            activeTable = evaporationPanel.getTable();
            evaporationPanel.getScrollBar().setValue(
                        transpirationPanel.getScrollBar().getValue());
        } else {
            activeTable = transpirationPanel.getTable();
            transpirationPanel.getScrollBar().setValue(
                        evaporationPanel.getScrollBar().getValue());
        }

        // Set the selected row in the new active table
        if (r > -1) {
            activeTable.clearSelection();
            activeTable.addRowSelectionInterval(r, r);
        }
    }

    /**
     * Update which tabs are shown, depending on model options
     */
    public void UpdateTabs(vs2ModelOptions modelOptions) {
        // Save the new model options
        this.modelOptions = modelOptions;

        // If a table is showing, remember the selected row so we can select
        // the same row for the new table.
        int r;
        if (activeTable != null) {
            r = activeTable.getSelectedRow();
        }
        // If a table is not showing, set the selected row to -1 which means
        // no row is selected.
        else {
            r = -1;
        }

        // Remove all tabs from tabbed panel
        tabbedPane.removeAll();

        // Put the appropriate tab(s) in the tabbed pane
        if (modelOptions.doEvaporation) {
            tabbedPane.addTab("Evaporation", null, evaporationPanel);
        }
        if (modelOptions.doTranspiration) {
            tabbedPane.addTab("Transpiration", null, transpirationPanel);
        }

        // set the active table
        if (modelOptions.doEvaporation) {
            activeTable = evaporationPanel.getTable();
        }
        else if (modelOptions.doTranspiration) {
            activeTable = transpirationPanel.getTable();
        }

        // If a row was previously selected, then select the same row again
        if (r > -1) {
            activeTable.clearSelection();
            activeTable.addRowSelectionInterval(r, r);
        }

        // validate and repaint the window
        getContentPane().validate();
        getContentPane().repaint();
    }

    public void setEnabled(boolean b) {
        super.setEnabled(b);
        modifyButton.setEnabled(b);
    }


    /**
     * Displays a dialog box for user to specify contour value.
     */
    protected class PeriodLengthDialog extends mp2Dialog {

        public double length;
        protected JTextField lengthTextField;

        /**
         * Creates a dialog box.
         */
        public PeriodLengthDialog() {
            super("Evapotranspiration Period Length", false);
        }

        /**
         * Makes dialog box contents.
         */
        protected void makeContents() {
            JPanel centerPanel = new JPanel(false);
            centerPanel.setLayout(new FlowLayout(FlowLayout.CENTER));
            centerPanel.setBorder(new EmptyBorder(25, 25, 25, 25));
            getContentPane().add(centerPanel, BorderLayout.CENTER);
            centerPanel.add(new JLabel("Period Length"));
            centerPanel.add(lengthTextField = new JTextField(5));
        }

        /**
         * Displays the dialog box with modal behavior.
         */
        public boolean doModal() {
            lengthTextField.setText(String.valueOf(length));
            return super.doModal();
        }

        /**
         * Retrives data from dialog box components and checks validity.
         */
        protected boolean retrieveData() {
            try {
                length = Double.valueOf(lengthTextField.getText()).doubleValue();
            } catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog("Please check your input",
                        "Input Error");
                return false;
            }
            if (!dataCheck(length, "\"Period Length\"", IS_POSITIVE,
                    lengthTextField)) {
                return false;
            }
            return true;
        }
    }
}
