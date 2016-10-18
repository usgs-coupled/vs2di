/*
 * vs2TabularDataDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.*;

public class vs2TabularDataDialog extends vs2TexturalClassDialog 
                                  implements vs2Constants {

    protected vs2HkmData hkmData;
    protected mp2TablePanel hkmTablePanel;
    protected mp2TableModel hkmTableModel;
    protected JTable hkmTable;
    protected JTextField hTextField;
    protected JTextField kTextField;
    protected JTextField mTextField;
    protected JButton insertButton;
    protected JButton modifyButton;
    protected JButton deleteButton;
    protected static final int EDIT_INSERT = 1;
    protected static final int EDIT_MODIFY = 2;

    public vs2TabularDataDialog(String title, 
                vs2ModelOptions modelOptions, boolean doEdit) {
        super(title, modelOptions, doEdit);
        this.doEdit = doEdit;
    }

    /**
     * Make dialog box contents
     */
    protected void makeContents() {
        // get the model options back from the custom object
        modelOptions = (vs2ModelOptions) customObject;

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();

        // Call the superclass method to make the color button
        MakeColorButton();

        // Create a center panel to hold labels, text fields, and table
        JPanel centerPanel = new JPanel(gridbag, false);
        centerPanel.setBorder(new EmptyBorder(10, 20, 10, 20));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        // Create a sub panel to hold hydraulic and transport properties
        JPanel subPanel = new JPanel(new GridLayout(1, 2, 20, 0), false);
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.HORIZONTAL;
        gridbag.setConstraints(subPanel, c);
        centerPanel.add(subPanel);

        // Create panels for hydraulic properties and for transport properties
        JPanel hydraulicPanel = new JPanel(false);
        String type = "Flow properties";
        switch (modelOptions.soilModel) {
            case BROOKS_COREY:
                type = "Flow properties (Brooks-Corey function)";
                break;
            case VAN_GENUCHTEN:
                type = "Flow properties (van Genuchten function)";
                break;
            case HAVERKAMP:
                type = "Flow properties (Haverkamp function)";
                break;
            case TABULAR_DATA:
                type = "Flow properties (tabular data)";
                break;
            case ROSSI_NIMMO:
                type = "Flow properties (Rossi-Nimmo function)";
                break;
            default:
                assert(false);            
        }
        hydraulicPanel.setBorder(new CompoundBorder(
            BorderFactory.createTitledBorder(type),
            new EmptyBorder(4, 10, 10, 10)));
        hydraulicPanel.setLayout(new BoxLayout(hydraulicPanel, BoxLayout.X_AXIS));
        subPanel.add(hydraulicPanel);

        // Within the hydraulic panel, make a left panel to hold the labels
        JPanel hydrLeftPanel = new JPanel(false);
        hydrLeftPanel.setLayout(new GridLayout(0, 1, 0, 10));
        hydraulicPanel.add(hydrLeftPanel);

        // Put space between the left and right panels
        hydraulicPanel.add(Box.createHorizontalStrut(20));

        // Make a right panel to hold the text fields
        JPanel hydrRightPanel = new JPanel(false);
        hydrRightPanel.setLayout(new GridLayout(0, 1, 0, 10));
        hydraulicPanel.add(hydrRightPanel);

        // Initialize the hydraulic row count 
        int hydraulicRows = 0;

        // Use horizontal strut for spacing
        hydrLeftPanel.add(Box.createHorizontalStrut(80));
        hydrRightPanel.add(Box.createHorizontalStrut(80));
        hydraulicRows ++;

        // create labels and text field for hydraulic properties
        hydrLeftPanel.add(new JLabel("name", SwingConstants.RIGHT));
        hydrRightPanel.add(nameTextField = new JTextField(5));
        hydraulicRows ++;

        hydrLeftPanel.add(new JLabel("Kzz/Kxx", SwingConstants.RIGHT));
        hydrRightPanel.add(anisotropyTextField = new JTextField(5));

        hydrLeftPanel.add(new JLabel("saturated K", SwingConstants.RIGHT));
        hydrRightPanel.add(satHydrCondTextField = new JTextField(5));

        hydrLeftPanel.add(new JLabel("specific storage", SwingConstants.RIGHT));
        hydrRightPanel.add(specificStorageTextField = new JTextField(5));

        hydrLeftPanel.add(new JLabel("porosity", SwingConstants.RIGHT));
        hydrRightPanel.add(porosityTextField = new JTextField(5));

        hydraulicRows += 4;

        MakeContentsForTransport(subPanel, hydrLeftPanel, hydrRightPanel, hydraulicRows);        

        // Create the table
        hkmData = new vs2HkmData();
        hkmData.init(null);
        hkmTableModel = new mp2TableModel(hkmData);
        hkmTablePanel = new mp2TablePanel(hkmTableModel);
        c.insets = new Insets(20, 0, 0, 0);
        gridbag.setConstraints(hkmTablePanel, c);
        centerPanel.add(hkmTablePanel);

        hkmTable = hkmTablePanel.getTable();
        hkmTable.setPreferredScrollableViewportSize(
                    new Dimension(200, 100));
        ListSelectionModel rowSM = hkmTable.getSelectionModel();
        rowSM.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                onRowSelection();
            }
        });

        JPanel editPanel = new JPanel(gridbag);
        hkmTablePanel.add(editPanel, BorderLayout.SOUTH);

        JPanel leftPanel = new JPanel(new GridLayout(3, 3, 10, 10));
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(leftPanel, c);
        editPanel.add(leftPanel);
        leftPanel.add(new JLabel("Pressure Head", SwingConstants.RIGHT));
        leftPanel.add(hTextField = new JTextField(5));
        leftPanel.add(new JLabel("Relative K", SwingConstants.RIGHT));
        leftPanel.add(kTextField = new JTextField(5));
        leftPanel.add(new JLabel("Moisture Cont.", SwingConstants.RIGHT));
        leftPanel.add(mTextField = new JTextField(5));

        JPanel rightPanel = new JPanel(new GridLayout(3, 1, 5, 5));
        rightPanel.setBorder(new EmptyBorder(0, 100, 0, 0));
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(rightPanel, c);
        editPanel.add(rightPanel);
        rightPanel.add(insertButton = new JButton("Insert"));
        rightPanel.add(modifyButton = new JButton("Modify"));
        rightPanel.add(deleteButton = new JButton("Delete"));

        insertButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onEdit(EDIT_INSERT);
            }
        });
        modifyButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onEdit(EDIT_MODIFY);
            }
        });
        deleteButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDelete();
            }
        });
    }

    /**
     * Displays the dialog box
     */
    public boolean doModal() {

        // If we are editing data, then put the current data in the text fields
        if (doEdit) {
            colorLabel.setColor((Color) aRow[1]);
            nameTextField.setText((String) aRow[2]);
            anisotropyTextField.setText(String.valueOf(((Double) aRow[3]).doubleValue()));
            specificStorageTextField.setText(String.valueOf(((Double) aRow[4]).doubleValue()));
            porosityTextField.setText(String.valueOf(((Double) aRow[5]).doubleValue()));
            satHydrCondTextField.setText(String.valueOf(((Double) aRow[19]).doubleValue()));

            // put data in table
            hkmData.setData((Vector) (((Vector) aRow[20]).clone()));

            // If the table contains data, select the first row and enable the edit and
            // delete buttons
            if (hkmData.getNumberOfRows() > 0) {
                hkmTable.addRowSelectionInterval(0, 0);
                modifyButton.setEnabled(true);
                deleteButton.setEnabled(true);
            }

            if (modelOptions.doEnergyTransport || modelOptions.doSoluteTransport) {
                SetTextFieldsForTransport();
            }
        } else {
            modifyButton.setEnabled(false);
            deleteButton.setEnabled(false);
        }

        // Request focus for the color button when the dialog box opens
        colorButton.requestFocus();

        // Call superclass method to show the dialog box and return result
        // when the dialog box closes
        return super.doModal();
    }

    /**
     * Inserts or modifies a row of hkm data
     */
    protected void onEdit(int editMode) {
        double h, k, m, h0, k0, m0;
        try {
            h = Double.valueOf(hTextField.getText()).doubleValue();
            k = Double.valueOf(kTextField.getText()).doubleValue();
            m = Double.valueOf(mTextField.getText()).doubleValue();
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
            "Input Error");
            hTextField.requestFocus();
            return;
        }
        if (!dataCheck(k, "Relative K", IS_BETWEEN_INCLUSIVE, 
                                0, "0", 1, "1", kTextField)) {
            return;
        }
        if (!dataCheck(m, "Moisture Cont.", IS_BETWEEN_INCLUSIVE, 
                                0, "0", 1, "1", mTextField)) {
            return;
        }

        // check that if h is nonnegative, k = 1
        if (h >= 0 && k != 1) {
            mp2MessageBox.showMessageDialog("Relative K should equal 1 when"
                + " pressure head is nonnegative", "Error");
            return;
        }
        int numRows = hkmData.getNumberOfRows();
        int selectedRow = hkmTable.getSelectedRow();

        // check that the h-k and h-m functions are monotonic
        for (int i=0; i<numRows; i++) {
            if (!(editMode == EDIT_MODIFY && i == selectedRow)) {
                h0 = ((Double) hkmData.getObjectAt(i, 0)).doubleValue();
                k0 = ((Double) hkmData.getObjectAt(i, 1)).doubleValue();
                m0 = ((Double) hkmData.getObjectAt(i, 2)).doubleValue();
                if (h < h0) {
                    if (k > k0 || m > m0) {
                        mp2MessageBox.showMessageDialog("Please ensure that soil functions"
                            + " are monotonic", "Error");
                        hTextField.requestFocus();
                        hTextField.selectAll();
                        return;
                    }
                } else if (h > h0) {
                    if (k < k0 || m < m0) {
                        mp2MessageBox.showMessageDialog("Please ensure that soil functions"
                            + " are monotonic", "Error");
                        hTextField.requestFocus();
                        hTextField.selectAll();
                        return;
                    }
                } else {  // case of h == h0
                    mp2MessageBox.showMessageDialog("Tabular data cannot contain" +
                        " 2 identical pressure heads", "Error");
                    hTextField.requestFocus();
                    hTextField.selectAll();
                    return;
                }
                if (k==1 && k0==1 && m0!=m) {
                    mp2MessageBox.showMessageDialog("Moisture content should "
                        + "not change when relative K equals 1", "Error");
                    mTextField.requestFocus();
                    mTextField.selectAll();
                    return;
                }

            }
        }

        if (editMode == EDIT_MODIFY) {
            hkmData.deleteRow(selectedRow);
            numRows--;
        }
        Object [] aRow = hkmData.createDefaultRow();
        aRow[0] = new Double(h);
        aRow[1] = new Double(k);
        aRow[2] = new Double(m);

        int r=0;
        int s=0;
        int i;
        if (numRows > 0) {
            for (i=0; i<numRows; i++) {
                if (h > ((Double) hkmData.getObjectAt(i, 0)).doubleValue()) {
                    r = i-1;
                    s = i;
                    break;
                }
            }
            if (i == numRows) {
                r = numRows-1;
                s = numRows;
            }
        }
        hkmData.addRow(r, aRow);
        hkmTable.clearSelection();
        hkmTable.addRowSelectionInterval(s, s);
        hkmTable.scrollRectToVisible(
                hkmTable.getCellRect(s, 0, true));
        hTextField.requestFocus();
        hTextField.selectAll();
        modifyButton.setEnabled(true);
        deleteButton.setEnabled(true);
    }

    /**
     * Deletes a row of hkm data
     */
    protected void onDelete() {

        // Get the selected row
        int r = hkmTable.getSelectedRow();

        // If row is -1, then no row is selected. So just return
        if (r == -1) {
            return;
        }

        // Verify that user really wants to delete the selected row
        int result = mp2MessageBox.showYesNoDialog(
                "Do you want to delete the selected data?", "Confirm");

        if (result == mp2MessageBox.YES_OPTION) {

            // Delete the selected row from the data
            hkmData.deleteRow(r);

            // Determine the revised row count
            int rowCount = hkmData.getNumberOfRows();

            // Set row selection and scroll it into view
            hkmTable.clearSelection();
            if (rowCount > 0) {
                r = Math.min(r, rowCount-1);
                hkmTable.addRowSelectionInterval(r, r);
                hkmTable.scrollRectToVisible(hkmTable.getCellRect(r, 0, true));
            }

            // If the row count is at minimum, then disable the delete and edit buttons
            if (rowCount == 0) {
                modifyButton.setEnabled(false);
                deleteButton.setEnabled(false);
            } else {
                deleteButton.requestFocus();
            }
        }
    }

    /**
     * Invoked when a row is selected in the hkm table
     */
    protected void onRowSelection() {
        int r = hkmTable.getSelectedRow();
        if (r < 0) {
            return;
        }
        Object [] aRow = hkmData.getRow(r);
        if (aRow != null) {
            hTextField.setText(String.valueOf((Double) aRow[0]));
            kTextField.setText(String.valueOf((Double) aRow[1]));
            mTextField.setText(String.valueOf((Double) aRow[2]));
        }
    }

    /**
     * Get data from dialog box components
     */
    protected boolean retrieveData() {
        double aniso=0, satK=0, ss=0, poro=0;
        try {
            aniso = Double.valueOf(anisotropyTextField.getText()).doubleValue();
            satK = Double.valueOf(satHydrCondTextField.getText()).doubleValue();
            ss = Double.valueOf(specificStorageTextField.getText()).doubleValue();
            poro = Double.valueOf(porosityTextField.getText()).doubleValue();
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
                "Input Error");
            return false;
        }

        if (!dataCheck(aniso, "\"Kzz/Kxx\"", IS_POSITIVE, 
                            anisotropyTextField)) {
            return false;
        }
        if (!dataCheck(satK, "\"saturated K\"", IS_NON_NEGATIVE, 
                            satHydrCondTextField)) {
            return false;
        }
        if (!dataCheck(ss, "\"specific storage\"", IS_NON_NEGATIVE, 
                            specificStorageTextField)) {
            return false;
        }
        if (!dataCheck(poro,  "\"porosity\"",  IS_BETWEEN_INCLUSIVE, 
                            0, "0", 1, "1", porosityTextField)) {
            return false;
        }

        if (modelOptions.doEnergyTransport || modelOptions.doSoluteTransport) {
            if (!RetrieveDataForTransport()) {
                return false;
            }
        }
        if (hkmData.getNumberOfRows() == 0) {
            mp2MessageBox.showMessageDialog("Please enter tabular data.",
                "Error");
            return false;
        }

        aRow[1] = colorLabel.getColor();
        aRow[2] = nameTextField.getText();
        aRow[3] = new Double(aniso);
        aRow[4] = new Double(ss);
        aRow[5] = new Double(poro);
        aRow[7] = new Integer(-1);
        aRow[19] = new Double(satK);
        aRow[20] = hkmData.getData();
        return true;
    }
}

