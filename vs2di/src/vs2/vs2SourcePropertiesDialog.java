/*
 * vs2SourcePropertiesDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

public class vs2SourcePropertiesDialog extends mp2Dialog implements vs2Constants {

    protected JComboBox sourceChooser;
    protected JComboBox propertyChooser;
    protected JTextField strengthTextField;
    protected JTextField valueTextField;
    protected JButton updateButton;
    protected mp2TableData tableData;
    protected mp2TableModel tableModel;
    protected JTable table;
    protected String transport;
    protected String diffusiveTransport;
    int [] columnMask;
            
    public vs2SourcePropertiesDialog(Object [] customArray) {
        super("Source/Sink Properties", true, customArray);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "sourceStrength", null);
        tableData = (mp2TableData) customArray[0];
        transport = (String) customArray[1];
        if (transport != null) {
            if (transport.equalsIgnoreCase("Temp.")) {
                diffusiveTransport = "Conductive heat flow (H)";
            } else {
                diffusiveTransport = "Diffusive mass flow (M)";
            }
        }
        if (customArray.length > 3) {
            columnMask = (int []) customArray[2];
        }
    }
    
    protected void makeContents() {
        String usage;
        int [] columnMask = null;
        mp2TableData tableData = (mp2TableData) customArray[0];
        usage = (String) customArray[1];
        if (customArray.length > 3) {
            columnMask = (int []) customArray[2];
        }
        
        JPanel centerPanel = new JPanel(new BorderLayout());
        getContentPane().add(centerPanel, BorderLayout.CENTER);
        
        // Create the table
        tableModel = new mp2TableModel(tableData);
        if (columnMask != null) {
            tableModel.setColumnMask(columnMask);
        }
        mp2TablePanel tablePanel = new mp2TablePanel(tableModel, true);
        tablePanel.setPreferredSize(new Dimension(100, 200));
        table = tablePanel.getTable();
        table.getSelectionModel().addListSelectionListener(
            new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    if (!e.getValueIsAdjusting()) {
                        onRowSelection();
                    }
                }
        });
        centerPanel.add(tablePanel, BorderLayout.NORTH);
        
        // Create controls for editing the table
        sourceChooser = new JComboBox();
        sourceChooser.addItem("Please select");
        sourceChooser.addItem("Flow rate");
        sourceChooser.addItem("Pressure Head");
        sourceChooser.addItem("Total Head");
        sourceChooser.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onSelectedFlowType();
                }
            }
        });

        
        propertyChooser = new JComboBox();
        if (usage != null) {
            propertyChooser.addItem("Please select");
            if (usage.equalsIgnoreCase("Temp.")) {
                propertyChooser.addItem("Temperature of inflow (Ti)");
                propertyChooser.addItem("Temperature at source (T)");
            } else {
                propertyChooser.addItem("Concentration of inflow (Ci)");
                propertyChooser.addItem("Concentration at source (C)");
            }
        }
        strengthTextField = new JTextField(6);
        valueTextField = new JTextField(6);
        updateButton = new JButton("Update");
        updateButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onUpdateButtonClicked();
            }
        });
        
        
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        JPanel controlPanel = new JPanel(gridbag);
        controlPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
        c.anchor = GridBagConstraints.CENTER;
        gridbag.setConstraints(controlPanel, c);
        centerPanel.add(controlPanel, BorderLayout.CENTER);

        JPanel subPanel = new JPanel(new GridLayout(0, 1, 0, 10));
        controlPanel.add(subPanel);
        c.fill = GridBagConstraints.VERTICAL;
        c.insets = new Insets(0, 0, 0, 10);
        gridbag.setConstraints(subPanel, c);
        subPanel.add(sourceChooser);
        if (usage != null) {
            subPanel.add(propertyChooser);
        }
        
        subPanel = new JPanel(new GridLayout(0, 1, 0, 10));
        controlPanel.add(subPanel);
        c.insets = new Insets(0, 0, 0, 10);
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(subPanel, c);
        subPanel.add(new JLabel("="));
        if (usage != null) {
            subPanel.add(new JLabel("="));
        }
        
        subPanel = new JPanel(new GridLayout(0, 1, 0, 10));
        controlPanel.add(subPanel);
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.insets = new Insets(0, 0, 0, 10);
        gridbag.setConstraints(subPanel, c);
        subPanel.add(strengthTextField);
        if (usage != null) {
            subPanel.add(valueTextField);
        }
        
        controlPanel.add(updateButton);
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.anchor = GridBagConstraints.EAST;
        c.insets = new Insets(10, 0, 0, 0);
        gridbag.setConstraints(updateButton, c);

        pack();
    }
    
    public boolean doModal() {
        if (tableData.getNumberOfRows() > 0) {
            table.setRowSelectionInterval(0, 0);
            updateButton.setEnabled(true);
        } else {
            updateButton.setEnabled(false);
        }
        return super.doModal();
    }
    
    protected void onUpdateButtonClicked() {
        int [] r = table.getSelectedRows();
        if (r == null) {
            return;
        }
        int type = NORMAL_FLUID_FLUX_BC; 
        int prop = DEFAULT_CONC_BC;
        double strength = 0;
        double value = 0;
        switch (sourceChooser.getSelectedIndex()) {
        case 1:
            type = NORMAL_FLUID_FLUX_BC;
            break;
        case 2:
            type = PRESSURE_HEAD_BC;
            break;
        case 3:
            type = TOTAL_HEAD_BC;
            break;
        default:
            mp2MessageBox.showMessageDialog("Please select the flow variable.", "Input Error");
            return;
        }
                
        try {
            strength = Double.valueOf(strengthTextField.getText()).doubleValue();
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input", "Input Error");
            return;
        }
        if (transport != null) {
            switch (propertyChooser.getSelectedIndex()) {
            case 1:
                prop = DEFAULT_CONC_BC;
                break;
            case 2:
                prop = SPECIFIED_CONC_BC;
                break;
            case 3:
                prop = DIFFUSIVE_FLUX_BC;
                break;
            default:
                mp2MessageBox.showMessageDialog("Please select the transport variable.", "Input Error");
                return;
            }
            try {
                value = Double.valueOf(valueTextField.getText()).doubleValue();
            } catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog("Please check your input", "Input Error");
                return;
            }
            if (prop==DIFFUSIVE_FLUX_BC && strength!=0) {
                mp2MessageBox.showMessageDialog("Conductive heat flow is allowed only if fluid flow rate is zero.", "Error");
                return;
            }
        }
                
        for (int i=0; i<r.length; i++)
        {
            Object [] aRow = (Object []) tableData.getRow(r[i]);
            aRow[1] = new Integer(type);
            aRow[2] = new Double(strength);
            if (transport != null) {
                aRow[columnMask[3]] = new Integer(prop);
                aRow[columnMask[4]] = new Double(value);
            }
        }
        tableModel.notifyDataChanged();
        table.setRowSelectionInterval(r[0], r[0]);
    }

    protected void onRowSelection() {
        if (tableData.getNumberOfRows() == 0 || table.getSelectedRowCount() == 0) {
            return;
        }
        int [] r = table.getSelectedRows();
        Object [] firstSelectedRow = tableData.getRow(r[0]);
        // check the source type
        int type = ((Integer) firstSelectedRow[1]).intValue();
        for (int i=1; i<r.length; i++) {
            Object [] aRow = tableData.getRow(r[i]);
            if (((Integer) aRow[1]).intValue() != type) {
                type = -1;
                break;
            }
        }
        switch (type) {
        case NORMAL_FLUID_FLUX_BC:
            sourceChooser.setSelectedIndex(1);
            break;
        case PRESSURE_HEAD_BC:
            sourceChooser.setSelectedIndex(2);
            break;
        case TOTAL_HEAD_BC:
            sourceChooser.setSelectedIndex(3);
            break;
        default :
            sourceChooser.setSelectedIndex(0);
            break;
        }
        // check the source strength
        double strength = ((Double) firstSelectedRow[2]).doubleValue();
        String s = String.valueOf(strength);
        for (int i=1; i<r.length; i++) {
            Object [] aRow = tableData.getRow(r[i]);
            if (((Double) aRow[2]).doubleValue() != strength) {
                s = "";
                break;
            }
        }
        strengthTextField.setText(s);
        if (transport != null) {
            int prop = ((Integer) firstSelectedRow[columnMask[3]]).intValue();
            for (int i=1; i<r.length; i++) {
                Object [] aRow = tableData.getRow(r[i]);
                if (((Integer) aRow[columnMask[3]]).intValue() != prop) {
                    prop = -1;
                    break;
                }
            }
            switch (prop) {
            case DEFAULT_CONC_BC:
                propertyChooser.setSelectedIndex(1);
                break;
            case SPECIFIED_CONC_BC:
                propertyChooser.setSelectedIndex(2);
                break;
            case DIFFUSIVE_FLUX_BC:
                propertyChooser.setSelectedIndex(3);
                break;
            default:
                propertyChooser.setSelectedIndex(0);
                break;
            }
            double value = ((Double) firstSelectedRow[columnMask[4]]).doubleValue();
            s = String.valueOf(value);
            for (int i=1; i<r.length; i++) {
                Object [] aRow = tableData.getRow(r[i]);
                if (((Double) aRow[columnMask[4]]).doubleValue() != value) {
                    s = "";
                    break;
                }
            }
            valueTextField.setText(s);
        }
    }
    
    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile ("sourceSinkDialog.html");
    }


    protected void onSelectedFlowType() {
            String strFlowType = (String) sourceChooser.getSelectedItem();
            int propChooserItemCount = propertyChooser.getItemCount();
            int propIndex = propertyChooser.getSelectedIndex();
            if (strFlowType.equalsIgnoreCase("Flow rate")) {
                if (propChooserItemCount == 3) {
                    propertyChooser.addItem(diffusiveTransport);
                }
                propertyChooser.setSelectedIndex(propIndex);
            } else {
                if (propChooserItemCount == 4) {
                    propertyChooser.removeItem(diffusiveTransport);
                }
                if (propIndex == 3) {
                    propertyChooser.setSelectedIndex(0);
                } else {
                    propertyChooser.setSelectedIndex(propIndex);
                }
            }
    }
    
    protected boolean retrieveData() {
        // This is just to check that the text entries are entered into the table
        int [] r = table.getSelectedRows();
        if (r == null) {
            return true;
        }
        boolean updated = true;
        int type = -1;
        int prop = -1;
        double strength = 0;
        double value = 0;
        switch(sourceChooser.getSelectedIndex()) {
        case 0:
            return true;    // if not selected, assume no need to update
        case 1:
            type = NORMAL_FLUID_FLUX_BC;
            break;
        case 2:
            type = PRESSURE_HEAD_BC;
            break;
        case 3:
            type = TOTAL_HEAD_BC;
            break;
        
        }
        if (transport != null) {
            switch (propertyChooser.getSelectedIndex()) {
            case 0:
                return true;    // if not selected, assume no need to update
            case 1:
                prop = DEFAULT_CONC_BC;
                break;
            case 2:
                prop = SPECIFIED_CONC_BC;
                break;
            case 3:
                prop = DIFFUSIVE_FLUX_BC;
                break;
            }
        }
        try {
            strength = Double.valueOf(strengthTextField.getText()).doubleValue();
            if (transport != null) {
                value = Double.valueOf(valueTextField.getText()).doubleValue();
            }
        } catch (NumberFormatException e) {
            // if text field does not contain number, assume no need to update
            return true;
        }
        for (int i=0; i<r.length && updated; i++) {
            Object [] aRow = (Object []) tableData.getRow(r[i]);
            if (((Integer) aRow[1]).intValue() != type) { 
                updated = false;
                break;
            }
            if (((Double) aRow[2]).doubleValue() != strength) { 
                updated = false;
                break;
            }
            if (transport != null) {
                if (((Integer) aRow[columnMask[3]]).intValue() != prop) {
                    updated = false;
                    break;
                }
                if (((Double) aRow[columnMask[4]]).doubleValue() != value) { 
                    updated = false;
                    break;
                }
            }
        }
 
        if (!updated) {
            int result = mp2MessageBox.showYesNoDialog("Data have not been transferred to the table." +
                " Do you still want to quit?", "Warning");
            if (result == mp2MessageBox.NO_OPTION) {
                return false;   // don't close dialog box
            }
        }
        return true;
    }
}
