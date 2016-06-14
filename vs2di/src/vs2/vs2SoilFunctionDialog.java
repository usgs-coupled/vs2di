/*
 * vs2SoilFunctionDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.text.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2SoilFunctionDialog extends vs2TexturalClassDialog
                implements vs2Constants, vs2VanGenuchtenParameters {

    // Boolean flag that is true if generic soil properties is used, false otherwise
    protected boolean useGenericSoil;

    // The index of the selected generic soil. Equals -1 if generic soil is not used
    protected int genericSoilIndex;

    // Components in the dialog box
    protected JCheckBox genericSoilCheckBox;
    protected JPanel namePanel;
    protected JComboBox genericSoilChooser;
    protected static final double LN10 = Math.log(10.0);

    /**
     * Constructor
     */
    public vs2SoilFunctionDialog(String title,
                    vs2ModelOptions modelOptions, boolean doEdit) {
        super(title, modelOptions, doEdit);
        this.doEdit = doEdit;
    }

    /**
     * Creates the dialog box contents
     */
    protected void makeContents() {
        // get the model options back from the custom object
        modelOptions = (vs2ModelOptions) customObject;

        // Call the superclass method to make the color button
        MakeColorButton();

        // Create a center panel to hold labels, text fields, and table
        JPanel centerPanel = new JPanel(false);
        if (modelOptions.soilModel == TABULAR_DATA) {
            centerPanel.setLayout(new GridLayout(2, 1, 0, 20));
        }
        if (modelOptions.soilModel == VAN_GENUCHTEN) {
            centerPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
        } else {
            centerPanel.setBorder(new EmptyBorder(0, 20, 10, 20));
        }
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        // Create a sub panel to hold hydraulic and transport properties
        JPanel subPanel = new JPanel(false);
        subPanel.setLayout(new GridLayout(1, 3, 20, 0));
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

        // For the van Genuchten case, create an check box to provide the
        // option to use generic data.
        if (modelOptions.soilModel == VAN_GENUCHTEN && modelOptions.useStandardUnits()) {
            hydrLeftPanel.add(new JLabel(" ", SwingConstants.RIGHT));
            hydrRightPanel.add(genericSoilCheckBox = new JCheckBox("Use generic data"));
            genericSoilCheckBox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    OnGenericSoilCheckBox(e);
                }
            });
            hydraulicRows++;
        }

        // Create a name panel that holds the name text field
        hydrLeftPanel.add(new JLabel("name", SwingConstants.RIGHT));
        hydrRightPanel.add(namePanel = new JPanel(new GridLayout(1, 1, 0, 0), false));
        namePanel.add(nameTextField = new JTextField(5));
        hydraulicRows ++;

        // Create the generic soil chooser that will replace the name text field if
        // user wants to use generic properties
        genericSoilChooser = new JComboBox();
        genericSoilChooser.setEditable(false);
        for (int i=0; i<VG_NAME.length; i++) {
            genericSoilChooser.addItem(VG_NAME[i]);
        }
        genericSoilChooser.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                OnGenericSoilChooser(e);
            }
        });
        genericSoilIndex = 0;
        useGenericSoil = false;

        // create labels and text field for hydraulic properties
        hydrLeftPanel.add(new JLabel("Kzz/Khh", SwingConstants.RIGHT));
        hydrRightPanel.add(anisotropyTextField = new JTextField(5));

        hydrLeftPanel.add(new JLabel("saturated Khh", SwingConstants.RIGHT));
        hydrRightPanel.add(satHydrCondTextField = new JTextField(5));

        hydrLeftPanel.add(new JLabel("specific storage", SwingConstants.RIGHT));
        hydrRightPanel.add(specificStorageTextField = new JTextField(5));

        hydrLeftPanel.add(new JLabel("porosity", SwingConstants.RIGHT));
        hydrRightPanel.add(porosityTextField = new JTextField(5));

        hydraulicRows += 4;

        switch (modelOptions.soilModel) {
        case BROOKS_COREY:
            hydrLeftPanel.add(new JLabel("RMC", SwingConstants.RIGHT));
            hydrRightPanel.add(residualMoistureContentTextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("hb", SwingConstants.RIGHT));
            hydrRightPanel.add(flow1TextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("lambda", SwingConstants.RIGHT));
            hydrRightPanel.add(flow2TextField = new JTextField(5));

            hydraulicRows += 3;
            break;
        case VAN_GENUCHTEN:
            hydrLeftPanel.add(new JLabel("RMC", SwingConstants.RIGHT));
            hydrRightPanel.add(residualMoistureContentTextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("alpha", SwingConstants.RIGHT));
            hydrRightPanel.add(flow1TextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("beta", SwingConstants.RIGHT));
            hydrRightPanel.add(flow2TextField = new JTextField(5));

            hydraulicRows += 3;
            break;
        case HAVERKAMP:
            hydrLeftPanel.add(new JLabel("RMC", SwingConstants.RIGHT));
            hydrRightPanel.add(residualMoistureContentTextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("A'", SwingConstants.RIGHT));
            hydrRightPanel.add(flow1TextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("B'", SwingConstants.RIGHT));
            hydrRightPanel.add(flow2TextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("alpha", SwingConstants.RIGHT));
            hydrRightPanel.add(flow3TextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("beta", SwingConstants.RIGHT));
            hydrRightPanel.add(flow4TextField = new JTextField(5));

            hydraulicRows += 5;
            break;
        case ROSSI_NIMMO:
            hydrLeftPanel.add(new JLabel("psi0", SwingConstants.RIGHT));
            hydrRightPanel.add(flow1TextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("psiD", SwingConstants.RIGHT));
            hydrRightPanel.add(flow2TextField = new JTextField(5));

            hydrLeftPanel.add(new JLabel("lambda", SwingConstants.RIGHT));
            hydrRightPanel.add(flow3TextField = new JTextField(5));

            hydraulicRows += 3;
            break;
        }

        MakeContentsForTransport(subPanel, hydrLeftPanel, hydrRightPanel, hydraulicRows);
    }

    /**
     * Show the dialog box
     */
    public boolean doModal() {
        // If we are editing data, then put the current data in the text fields
        if (doEdit) {
            colorLabel.setColor((Color) aRow[1]);
            nameTextField.setText((String) aRow[2]);
            anisotropyTextField.setText(String.valueOf(((Double) aRow[3]).doubleValue()));
            specificStorageTextField.setText(String.valueOf(((Double) aRow[4]).doubleValue()));
            porosityTextField.setText(String.valueOf(((Double) aRow[5]).doubleValue()));
            switch (modelOptions.soilModel) {
            case BROOKS_COREY:
                residualMoistureContentTextField.setText(String.valueOf(((Double) aRow[6]).doubleValue()));
                satHydrCondTextField.setText(String.valueOf(((Double) aRow[8]).doubleValue()));
                flow1TextField.setText(String.valueOf(((Double) aRow[9]).doubleValue()));
                flow2TextField.setText(String.valueOf(((Double) aRow[10]).doubleValue()));
                break;
            case VAN_GENUCHTEN:
                if (((Integer) aRow[7]).intValue() > -1) {
                    useGenericSoil = true;
                    genericSoilCheckBox.setSelected(true);
                    genericSoilChooser.setSelectedIndex(((Integer) aRow[7]).intValue());
                    ReviseDialogBoxComponents();
                    ReviseDialogBoxData();
                } else {
                    useGenericSoil = false;
                    genericSoilIndex = 0;
                    residualMoistureContentTextField.setText(String.valueOf(((Double) aRow[6]).doubleValue()));
                    satHydrCondTextField.setText(String.valueOf(((Double) aRow[11]).doubleValue()));
                    flow1TextField.setText(String.valueOf(((Double) aRow[12]).doubleValue()));
                    flow2TextField.setText(String.valueOf(((Double) aRow[13]).doubleValue()));
                }
                break;
            case HAVERKAMP:
                residualMoistureContentTextField.setText(String.valueOf(((Double) aRow[6]).doubleValue()));
                satHydrCondTextField.setText(String.valueOf(((Double) aRow[14]).doubleValue()));
                flow1TextField.setText(String.valueOf(((Double) aRow[15]).doubleValue()));
                flow2TextField.setText(String.valueOf(((Double) aRow[16]).doubleValue()));
                flow3TextField.setText(String.valueOf(((Double) aRow[17]).doubleValue()));
                flow4TextField.setText(String.valueOf(((Double) aRow[18]).doubleValue()));
                break;
            case ROSSI_NIMMO:
                satHydrCondTextField.setText(String.valueOf(((Double) aRow[38]).doubleValue()));
                flow1TextField.setText(String.valueOf(((Double) aRow[39]).doubleValue()));
                flow2TextField.setText(String.valueOf(((Double) aRow[40]).doubleValue()));
                flow3TextField.setText(String.valueOf(((Double) aRow[41]).doubleValue()));
                break;
            }
            if (modelOptions.doEnergyTransport || modelOptions.doSoluteTransport) {
                SetTextFieldsForTransport();
            }
        }

        // Request focus for the color button when the dialog box opens
        colorButton.requestFocus();

        // Call superclass method to show the dialog box and return result
        // when the dialog box closes
        return super.doModal();
    }

    /**
     * Handle the generic soil check box
     */
    protected void OnGenericSoilCheckBox(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.SELECTED) {
            useGenericSoil = true;
        } else {
            useGenericSoil = false;
        }
        ReviseDialogBoxComponents();
        ReviseDialogBoxData();
    }

    /**
     * Handle the generic soil chooser
     */
    protected void OnGenericSoilChooser(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.SELECTED) {
            genericSoilIndex = genericSoilChooser.getSelectedIndex();
            ReviseDialogBoxData();
        }
    }

    /**
     * Revise the dialog box components
     */
    protected void ReviseDialogBoxComponents() {
        if (useGenericSoil) {
            // Replace the nameTextField with the genericSoilChooser
            namePanel.removeAll();
            namePanel.add(genericSoilChooser);

            // Make the text fields non editable
            anisotropyTextField.setEditable(false);
            satHydrCondTextField.setEditable(false);
            specificStorageTextField.setEditable(false);
            porosityTextField.setEditable(false);
            flow1TextField.setEditable(false);
            residualMoistureContentTextField.setEditable(false);
            flow2TextField.setEditable(false);
        } else {
            // Replace the genericSoilChooser by the nameTextField
            namePanel.removeAll();
            namePanel.add(nameTextField);

            // Make text field editable
            anisotropyTextField.setEditable(true);
            satHydrCondTextField.setEditable(true);
            specificStorageTextField.setEditable(true);
            porosityTextField.setEditable(true);
            flow1TextField.setEditable(true);
            residualMoistureContentTextField.setEditable(true);
            flow2TextField.setEditable(true);
        }
    }

    /**
     * Revise the dialog box data
     */
    protected void ReviseDialogBoxData() {
        if (useGenericSoil) {
            // Set the color and text fields for the generic soil

            int i = genericSoilIndex;
            // default units are m for [L] and m/s for [L/T]
            double factorL = 1;   // conversion for [L]
            double factorV = 1;   // conversion for [L/T]
            if (modelOptions.lengthUnit.equals("mm  ")) {
                factorL = 1000;   // meter to mm
                factorV = 1000;
            } else if (modelOptions.lengthUnit.equals("cm  ")) {
                factorL = 100;    // meter to cm
                factorV = 100;
            } else if (modelOptions.lengthUnit.equals("ft  ")) {
                factorL = 3.28;   // meter to ft
                factorV = 3.28;
            }
            if (modelOptions.timeUnit.equals("hour")) {
                factorV *= 3600;  //
            } else if (modelOptions.timeUnit.equals("day ")) {
                factorV *= 86400;
            } else if (modelOptions.timeUnit.equals("year")) {
                factorV *= 31536000;
            }
            double factor2 = 1;
            anisotropyTextField.setText(String.valueOf(VG_ANISOTROPY[i]));
            satHydrCondTextField.setText(customFormat(
                     VG_SATURATED_HYDRAULIC_CONDUCTIVITY[i] * factorV, 3));
            specificStorageTextField.setText(customFormat(
                        VG_SPECIFIC_STORAGE[i] / factorL, 3));
            porosityTextField.setText(String.valueOf(VG_POROSITY[i]));
            flow1TextField.setText(customFormat(VG_ALPHA[i] / factorL, 3));
            residualMoistureContentTextField.setText(String.valueOf(
                        VG_RESIDUAL_MOISTURE_CONTENT[i]));
            flow2TextField.setText(String.valueOf(VG_BETA[i]));
        } else {
            // If we are editing data, put the current data back into the text fields
            if (doEdit) {
                nameTextField.setText((String) aRow[2]);
                anisotropyTextField.setText(String.valueOf(((Double) aRow[3]).doubleValue()));
                satHydrCondTextField.setText(String.valueOf(((Double) aRow[11]).doubleValue()));
                specificStorageTextField.setText(String.valueOf(((Double) aRow[4]).doubleValue()));
                porosityTextField.setText(String.valueOf(((Double) aRow[5]).doubleValue()));
                flow1TextField.setText(String.valueOf(((Double) aRow[12]).doubleValue()));
                residualMoistureContentTextField.setText(String.valueOf(((Double) aRow[6]).doubleValue()));
                flow2TextField.setText(String.valueOf(((Double) aRow[13]).doubleValue()));
            }
            // If we are adding new data, put blanks into text fields
            else {
                nameTextField.setText("");
                anisotropyTextField.setText("");
                satHydrCondTextField.setText("");
                specificStorageTextField.setText("");
                porosityTextField.setText("");
                flow1TextField.setText("");
                residualMoistureContentTextField.setText("");
                flow2TextField.setText("");
            }
        }
        // Validate and repaint the dialog box
        getContentPane().validate();
        getContentPane().repaint();
    }

    /**
     * Get data from dialog box components
     */
    protected boolean retrieveData() {
        // Check to make sure that there are no errors
        // Don't allow white as a color
        if (colorLabel.getColor() == Color.white) {
            mp2MessageBox.showMessageDialog(parent, "Please select a color that is not white.", "Input error");
            return false;
        }
        double aniso=0, satK=0, ss=0, poro=0, rmc=0, flow1=0, flow2=0, flow3=0,
        flow4=0;
        try {
            aniso = Double.valueOf(anisotropyTextField.getText()).doubleValue();
            satK = Double.valueOf(satHydrCondTextField.getText()).doubleValue();
            ss = Double.valueOf(specificStorageTextField.getText()).doubleValue();
            poro = Double.valueOf(porosityTextField.getText()).doubleValue();
            switch (modelOptions.soilModel) {
            case BROOKS_COREY:
                rmc = Double.valueOf(residualMoistureContentTextField.getText()).doubleValue();
                flow1 = Double.valueOf(flow1TextField.getText()).doubleValue();
                flow2 = Double.valueOf(flow2TextField.getText()).doubleValue();
                break;
            case VAN_GENUCHTEN:
                rmc = Double.valueOf(residualMoistureContentTextField.getText()).doubleValue();
                flow1 = Double.valueOf(flow1TextField.getText()).doubleValue();
                flow2 = Double.valueOf(flow2TextField.getText()).doubleValue();
                break;
            case HAVERKAMP:
                rmc = Double.valueOf(residualMoistureContentTextField.getText()).doubleValue();
                flow1 = Double.valueOf(flow1TextField.getText()).doubleValue();
                flow2 = Double.valueOf(flow2TextField.getText()).doubleValue();
                flow3 = Double.valueOf(flow3TextField.getText()).doubleValue();
                flow4 = Double.valueOf(flow4TextField.getText()).doubleValue();
                break;
            case ROSSI_NIMMO:
                flow1 = Double.valueOf(flow1TextField.getText()).doubleValue();
                flow2 = Double.valueOf(flow2TextField.getText()).doubleValue();
                flow3 = Double.valueOf(flow3TextField.getText()).doubleValue();
                break;
            }
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
            "Input Error");
            return false;
        }

        // Check that hydraulic properties are correct
        if (!dataCheck(aniso, "\"Kzz/Kxx\"",       IS_POSITIVE, anisotropyTextField)) {
            return false;
        }
        if (!dataCheck(satK, "\"saturated K\"",    IS_NON_NEGATIVE, satHydrCondTextField)) {
            return false;
        }
        if (!dataCheck(ss, "\"specific storage\"", IS_NON_NEGATIVE, specificStorageTextField)) {
            return false;
        }
        if (!dataCheck(poro,  "\"porosity\"",  IS_BETWEEN_INCLUSIVE, 0, "0", 1, "1",
                        porosityTextField)) {
            return false;
        }
        if (!dataCheck(rmc,   "\"RMC\"",  IS_BETWEEN_INCLUSIVE, 0, "0", 1, "1",
                        residualMoistureContentTextField)) {
            return false;
        }
        if (!dataCheck(poro,  "\"porosity\"", IS_GREATER_THAN_OR_EQUAL_TO, rmc,   "\"RMC\"",
                        porosityTextField)) {
            return false;
        }
        switch (modelOptions.soilModel) {
        case BROOKS_COREY:
            if (!dataCheck(flow1, "\"h-sub-b\"", IS_NEGATIVE, flow1TextField))
            return false;
            if (!dataCheck(flow2, "\"pore-size dist. index\"", IS_NON_NEGATIVE, flow2TextField))
            return false;
            break;
        case VAN_GENUCHTEN:
            if (!dataCheck(flow1, "\"alpha\"", IS_NON_NEGATIVE, flow1TextField))
            return false;
            if (!dataCheck(flow2, "\"beta\"", IS_GREATER_THAN, 1.0, "1.0", flow2TextField))
            return false;
            break;
        case HAVERKAMP:
            if (!dataCheck(flow1, "\"A'\"", IS_NEGATIVE, flow1TextField)) {
                return false;
            }
            if (!dataCheck(flow2, "\"B'\"", IS_NON_NEGATIVE, flow2TextField)) {
                return false;
            }
            if (!dataCheck(flow3, "\"alpha\"", IS_NEGATIVE, flow3TextField)) {
                return false;
            }
            if (!dataCheck(flow4, "\"beta\"", IS_NON_NEGATIVE, flow4TextField)) {
                return false;
            }
            break;
        case ROSSI_NIMMO:
            if (!dataCheck(flow1, "\"psi0\"", IS_POSITIVE, flow1TextField)) {
                return false;
            }
            if (!dataCheck(flow2, "\"psiD\"", IS_POSITIVE, flow2TextField)) {
                return false;
            }
            if (!dataCheck(flow3, "\"lambda\"", IS_POSITIVE, flow3TextField)) {
                return false;
            }
            break;
        }

        if (modelOptions.doEnergyTransport || modelOptions.doSoluteTransport) {
            if (!RetrieveDataForTransport()) return false;
        }

        // When execution reaches this point, all input data are OK so we put
        // them in aRow and return true
        aRow[1] = colorLabel.getColor();
        if (useGenericSoil) {
            aRow[2] = VG_NAME[genericSoilIndex];
            aRow[7] = new Integer(genericSoilIndex);
        } else {
            aRow[2] = nameTextField.getText();
            aRow[7] = new Integer(-1);
        }
        aRow[3] = new Double(aniso);
        aRow[4] = new Double(ss);
        aRow[5] = new Double(poro);
        switch (modelOptions.soilModel) {
        case BROOKS_COREY:
            aRow[6] = new Double(rmc);
            aRow[8] = new Double(satK);
            aRow[9] = new Double(flow1);
            aRow[10] = new Double(flow2);
            break;
        case VAN_GENUCHTEN:
            aRow[6] = new Double(rmc);
            aRow[11] = new Double(satK);
            aRow[12] = new Double(flow1);
            aRow[13] = new Double(flow2);
            break;
        case HAVERKAMP:
            aRow[6] = new Double(rmc);
            aRow[14] = new Double(satK);
            aRow[15] = new Double(flow1);
            aRow[16] = new Double(flow2);
            aRow[17] = new Double(flow3);
            aRow[18] = new Double(flow4);
            break;
        case ROSSI_NIMMO:
            aRow[38] = new Double(satK);
            aRow[39] = new Double(flow1);
            aRow[40] = new Double(flow2);
            aRow[41] = new Double(flow3);
            break;
        }
        return true;
    }

    protected String customFormat(double value, int figures) {
        if (value == 0 || figures < 1) {
            return new String("0");
        }
        NumberFormat nf = NumberFormat.getInstance();
        int p = (int) Math.floor(Math.log(Math.abs(value))/LN10);
        if (p >= -3 && p < 3) {
            nf.setMaximumFractionDigits(2-p);
            return nf.format(value);
        } else {
            nf.setMaximumFractionDigits(figures-1);
            double a = value/Math.pow(10, p);
            return new String(nf.format(a) + "E" + p);
        }
    }
}

