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
    protected JComboBox energyPropertyChooser;
    protected JComboBox solutePropertyChooser;
    protected JTextField strengthTextField;
    protected JTextField energyValueTextField;
    protected JTextField soluteValueTextField;
    protected JButton updateButton;
    protected mp2TableData tableData;
    protected mp2TableModel tableModel;
    protected JTable table;
    protected String transport;
    protected String diffusiveTransport;
    int [] columnMask;
            
    public vs2SourcePropertiesDialog(Object [] customArray) {
        super("Source/Sink Properties", true, customArray);
        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "sourceSinkDialog", null);
        tableData = (mp2TableData) customArray[0];
        transport = (String) customArray[1];
        diffusiveTransport = "Conductive heat flow (H)";
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
        tablePanel.setPreferredSize(new Dimension(400, 200));
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

        
        energyPropertyChooser = new JComboBox();
        solutePropertyChooser = new JComboBox();
        if (usage != null) {
            if (usage.compareToIgnoreCase("Both") == 0 || usage.compareToIgnoreCase("Temp.") == 0) {
                energyPropertyChooser.addItem("Please select");
                energyPropertyChooser.addItem("Temperature of inflow (Ti)");
                energyPropertyChooser.addItem("Temperature at source (T)");
            }
            if (usage.compareToIgnoreCase("Both") == 0 || usage.compareToIgnoreCase("Conc.") == 0) {
                solutePropertyChooser.addItem("Please select");
                solutePropertyChooser.addItem("Solution no. of inflow (Si)");
                solutePropertyChooser.addItem("Solution no. at source (S)");
            }
        }
        strengthTextField = new JTextField(6);
        energyValueTextField = new JTextField(6);
        soluteValueTextField = new JTextField(6);
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
            if (usage.compareToIgnoreCase("Temp.") == 0) {
                subPanel.add(energyPropertyChooser);                
            }
            else if (usage.compareToIgnoreCase("Conc.") == 0) {
                subPanel.add(solutePropertyChooser);                                
            }
            else {
                assert(usage.compareToIgnoreCase("Both") == 0);
                subPanel.add(energyPropertyChooser);                
                subPanel.add(solutePropertyChooser);                                
            }
        }
        
        subPanel = new JPanel(new GridLayout(0, 1, 0, 10));
        controlPanel.add(subPanel);
        c.insets = new Insets(0, 0, 0, 10);
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(subPanel, c);
        subPanel.add(new JLabel("="));
        if (usage != null) {
            if (usage.compareToIgnoreCase("Temp.") == 0) {
                subPanel.add(new JLabel("="));
            }
            else if (usage.compareToIgnoreCase("Conc.") == 0) {
                subPanel.add(new JLabel("="));
            }
            else {
                assert(usage.compareToIgnoreCase("Both") == 0);
                subPanel.add(new JLabel("="));
                subPanel.add(new JLabel("="));
            }
        }
        
        subPanel = new JPanel(new GridLayout(0, 1, 0, 10));
        controlPanel.add(subPanel);
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.insets = new Insets(0, 0, 0, 10);
        gridbag.setConstraints(subPanel, c);
        subPanel.add(strengthTextField);
        if (usage != null) {
            if (usage.compareToIgnoreCase("Temp.") == 0) {
                subPanel.add(energyValueTextField);
            }
            else if (usage.compareToIgnoreCase("Conc.") == 0) {
                subPanel.add(soluteValueTextField);
            }
            else {
                assert(usage.compareToIgnoreCase("Both") == 0);
                subPanel.add(energyValueTextField);
                subPanel.add(soluteValueTextField);
            }
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
        double energyValue = 0;
        int soluteValue = 0;
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
            // energy transport
            if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Temp.") == 0) {            
                switch (energyPropertyChooser.getSelectedIndex()) {
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
                    energyValue = Double.valueOf(energyValueTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog("Please check your input", "Input Error");
                    return;
                }
                if (prop==DIFFUSIVE_FLUX_BC && strength!=0) {
                    mp2MessageBox.showMessageDialog("Conductive heat flow is allowed only if fluid flow rate is zero.", "Error");
                    return;
                }
            }
            // solute transport
            if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Conc.") == 0) {            
                switch (solutePropertyChooser.getSelectedIndex()) {
                case 1:
                    prop = DEFAULT_CONC_BC;
                    break;
                case 2:
                    prop = SPECIFIED_CONC_BC;
                    break;
                default:
                    mp2MessageBox.showMessageDialog("Please select the transport variable.", "Input Error");
                    return;
                }
                try {
                    soluteValue = Integer.valueOf(soluteValueTextField.getText()).intValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog("Please check your input", "Input Error");
                    return;
                }
            }
        }
                
        for (int i=0; i<r.length; i++)
        {
            Object [] aRow = (Object []) tableData.getRow(r[i]);
            aRow[1] = new Integer(type);
            aRow[2] = new Double(strength);
            if (transport != null) {
                if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Temp.") == 0) {
                    // energy
                    aRow[5] = new Integer(prop);
                    aRow[6] = new Double(energyValue);
                }
                if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Conc.") == 0) {
                    // solute
                    aRow[3] = new Integer(prop);
                    aRow[4] = new Integer(soluteValue);
                }
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
        
        if (transport == null) return;
        
        // energy transport
        if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Temp.") == 0) {
            // temp type
            int prop = ((Integer) firstSelectedRow[5]).intValue();
            for (int i=1; i<r.length; i++) {
                Object [] aRow = tableData.getRow(r[i]);
                if (((Integer) aRow[5]).intValue() != prop) {
                    prop = -1;
                    break;
                }
            }
            switch (prop) {
            case DEFAULT_CONC_BC:
                energyPropertyChooser.setSelectedIndex(1);
                break;
            case SPECIFIED_CONC_BC:
                energyPropertyChooser.setSelectedIndex(2);
                break;
            case DIFFUSIVE_FLUX_BC:
                energyPropertyChooser.setSelectedIndex(3);
                break;
            default:
                energyPropertyChooser.setSelectedIndex(0);
                break;
            } 
            // temp value
            double value = ((Double) firstSelectedRow[6]).doubleValue();
            s = String.valueOf(value);
            for (int i=1; i<r.length; i++) {
                Object [] aRow = tableData.getRow(r[i]);
                if (((Double) aRow[6]).doubleValue() != value) {
                    s = "";
                    break;
                }
            }
            energyValueTextField.setText(s);
        }

        // solute transport
        if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Conc.") == 0) {
            // solute type
            int prop = ((Integer) firstSelectedRow[3]).intValue();
            for (int i=1; i<r.length; i++) {
                Object [] aRow = tableData.getRow(r[i]);
                if (((Integer) aRow[3]).intValue() != prop) {
                    prop = -1;
                    break;
                }
            }
            switch (prop) {
            case DEFAULT_CONC_BC:
                solutePropertyChooser.setSelectedIndex(1);
                break;
            case SPECIFIED_CONC_BC:
                solutePropertyChooser.setSelectedIndex(2);
                break;
            default:
                solutePropertyChooser.setSelectedIndex(0);
                break;
            }
            int value = ((Integer) firstSelectedRow[4]).intValue();
            s = String.valueOf(value);
            for (int i=1; i<r.length; i++) {
                Object [] aRow = tableData.getRow(r[i]);
                if (((Integer) aRow[4]).intValue() != value) {
                    s = "";
                    break;
                }
            }
            soluteValueTextField.setText(s);
        }
    }
    
    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile ("sourceSinkDialog.html");
    }


    protected void onSelectedFlowType() {
            String strFlowType = (String) sourceChooser.getSelectedItem();
            int energyPropChooserItemCount = energyPropertyChooser.getItemCount();
            int energyPropIndex = energyPropertyChooser.getSelectedIndex();
            if (strFlowType.equalsIgnoreCase("Flow rate")) {
                if (energyPropChooserItemCount == 3) {
                    energyPropertyChooser.addItem(diffusiveTransport);
                }
                energyPropertyChooser.setSelectedIndex(energyPropIndex);
            } else {
                if (energyPropChooserItemCount == 4) {
                    energyPropertyChooser.removeItem(diffusiveTransport);
                }
                if (energyPropIndex == 3) {
                    energyPropertyChooser.setSelectedIndex(0);
                } else {
                    energyPropertyChooser.setSelectedIndex(energyPropIndex);
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
        int energyProp = -1;
        int soluteProp = -1;
        double strength = 0;
        double energyValue = 0;
        int soluteValue = 0;
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
            if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Temp.") == 0) {
                switch (energyPropertyChooser.getSelectedIndex()) {
                case 0:
                    return true;    // if not selected, assume no need to update
                case 1:
                    energyProp = DEFAULT_CONC_BC;
                    break;
                case 2:
                    energyProp = SPECIFIED_CONC_BC;
                    break;
                case 3:
                    energyProp = DIFFUSIVE_FLUX_BC;
                    break;
                }
            }
            if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Conc.") == 0) {
                switch (solutePropertyChooser.getSelectedIndex()) {
                case 0:
                    return true;    // if not selected, assume no need to update
                case 1:
                    energyProp = DEFAULT_CONC_BC;
                    break;
                case 2:
                    energyProp = SPECIFIED_CONC_BC;
                    break;
                case 3:
                    assert(false);
                    energyProp = DIFFUSIVE_FLUX_BC;
                    break;
                }
            }
        }
        try {
            strength = Double.valueOf(strengthTextField.getText()).doubleValue();
            if (transport != null) {
                if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Temp.") == 0) {
                    energyValue = Double.valueOf(energyValueTextField.getText()).doubleValue();
                }
                if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Conc.") == 0) {
                    soluteValue = Integer.valueOf(soluteValueTextField.getText()).intValue();
                }
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
                if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Temp.") == 0) {
                    if (((Integer) aRow[5]).intValue() != energyProp) {
                        updated = false;
                        break;
                    }
                    if (((Double) aRow[6]).doubleValue() != energyValue) { 
                        updated = false;
                        break;
                    }
                }
                if (transport.compareToIgnoreCase("Both") == 0 || transport.compareToIgnoreCase("Conc.") == 0) {
                    if (((Integer) aRow[3]).intValue() != soluteProp) {
                        updated = false;
                        break;
                    }
                    if (((Integer) aRow[4]).intValue() != soluteValue) { 
                        updated = false;
                        break;
                    }
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
