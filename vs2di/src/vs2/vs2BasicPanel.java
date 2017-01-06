/*
 * vs2BasicPanel.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2BasicPanel extends vs2ModelOptionsPanel {

    public String title;
    public String lengthUnit;
    public String timeUnit;
    public String massUnit;
    public String energyUnit;
    public boolean useRadialCoord;
    public boolean doEnergyTransport;    // new in Version 1.4
    public boolean doSoluteTransport;    // new in Version 1.4   
    public boolean doEvaporation;
    public boolean doTranspiration;
    public boolean radialCoordinatesEnabled;
    public boolean unitChangeWarningEnabled;

    protected JTextField titleTextField;
    protected JTextField otherLengthUnitTextField;
    protected JTextField otherTimeUnitTextField;
    protected JTextField otherMassUnitTextField;
    protected JTextField otherEnergyUnitTextField;
    protected JRadioButton mmRadioButton;
    protected JRadioButton cmRadioButton;
    protected JRadioButton mRadioButton;
    protected JRadioButton ftRadioButton;
    protected JRadioButton otherLengthRadioButton;
    protected JRadioButton secRadioButton;
    protected JRadioButton hourRadioButton;
    protected JRadioButton dayRadioButton;
    protected JRadioButton yearRadioButton;
    protected JRadioButton otherTimeRadioButton;
    protected JRadioButton mgRadioButton;
    protected JRadioButton gRadioButton;
    protected JRadioButton kgRadioButton;
    protected JRadioButton lbRadioButton;
    protected JRadioButton otherMassRadioButton;
    protected JRadioButton jouleRadioButton;
    protected JRadioButton calRadioButton;
    protected JRadioButton otherEnergyRadioButton;
    protected JCheckBox useRadialCoordCheckBox;
    protected JCheckBox doEnergyTransportCheckBox;    // new in Version 1.4
    protected JCheckBox doSoluteTransportCheckBox;    // new in Version 1.4
    protected JCheckBox doEvaporationCheckBox;
    protected JCheckBox doTranspirationCheckBox;

    /**
     * Creates the panel for basic model options
     */
    public vs2BasicPanel(vs2ModelOptionsDialog parentDialog) {

        super(parentDialog);

        unitChangeWarningEnabled = true;
        radialCoordinatesEnabled = true;

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        setBorder(new EmptyBorder(10, 10, 10, 10));
        setLayout(gridbag);

        JPanel panel, subPanel;
        ButtonGroup bg;

        // Create a panel to hold the title
        add(panel = new JPanel(false));
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(10, 20, 5, 20);
        gridbag.setConstraints(panel, c);
        panel.add(new JLabel("Title:"));
        panel.add(titleTextField = new JTextField(40));

        // Create a panel to hold three subpanel for units
        add(panel = new JPanel(new GridLayout(1, 3, 10, 1), false));
        gridbag.setConstraints(panel, c);

        // Length units
        panel.add(subPanel = new JPanel(gridbag, false));
        subPanel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Length Unit"),
                new EmptyBorder(5, 2, 5, 2)));
        c.fill = GridBagConstraints.NONE;
        c.anchor = GridBagConstraints.WEST;
        c.insets = new Insets(0, 0, 0, 0);

        mmRadioButton = new JRadioButton("mm");
        gridbag.setConstraints(mmRadioButton, c);
        subPanel.add(mmRadioButton);

        cmRadioButton = new JRadioButton("cm");
        gridbag.setConstraints(cmRadioButton, c);
        subPanel.add(cmRadioButton);

        mRadioButton = new JRadioButton("m");
        gridbag.setConstraints(mRadioButton, c);
        subPanel.add(mRadioButton);

        ftRadioButton = new JRadioButton("ft");
        gridbag.setConstraints(ftRadioButton, c);
        subPanel.add(ftRadioButton);

        otherLengthRadioButton = new JRadioButton("other");
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(otherLengthRadioButton, c);
        subPanel.add(otherLengthRadioButton);

        otherLengthUnitTextField = new JTextField(4);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(otherLengthUnitTextField, c);
        subPanel.add(otherLengthUnitTextField);

        bg = new ButtonGroup();
        bg.add(mmRadioButton);
        bg.add(cmRadioButton);
        bg.add(mRadioButton);
        bg.add(ftRadioButton);
        bg.add(otherLengthRadioButton);

        // Time units
        panel.add(subPanel = new JPanel(gridbag, false));
        subPanel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Time Unit"),
                    new EmptyBorder(5, 2, 5, 2)));

        secRadioButton = new JRadioButton("sec");
        gridbag.setConstraints(secRadioButton, c);
        subPanel.add(secRadioButton);

        hourRadioButton = new JRadioButton("hour");
        gridbag.setConstraints(hourRadioButton, c);
        subPanel.add(hourRadioButton);

        dayRadioButton = new JRadioButton("day");
        gridbag.setConstraints(dayRadioButton, c);
        subPanel.add(dayRadioButton);

        yearRadioButton = new JRadioButton("year");
        gridbag.setConstraints(yearRadioButton, c);
        subPanel.add(yearRadioButton);

        otherTimeRadioButton = new JRadioButton("other");
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(otherTimeRadioButton, c);
        subPanel.add(otherTimeRadioButton);

        otherTimeUnitTextField = new JTextField(4);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(otherTimeUnitTextField, c);
        subPanel.add(otherTimeUnitTextField);

        bg = new ButtonGroup();
        bg.add(secRadioButton);
        bg.add(hourRadioButton);
        bg.add(dayRadioButton);
        bg.add(yearRadioButton);
        bg.add(otherTimeRadioButton);

        // Mass units
        panel.add(subPanel = new JPanel(gridbag, false));
        bg = new ButtonGroup();
        subPanel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Mass Unit"),
                new EmptyBorder(5, 2, 5, 2)));

        mgRadioButton = new JRadioButton("mg");
        gridbag.setConstraints(mgRadioButton, c);
        subPanel.add(mgRadioButton);

        gRadioButton = new JRadioButton("g");
        gridbag.setConstraints(gRadioButton, c);
        subPanel.add(gRadioButton);

        kgRadioButton = new JRadioButton("Kg");
        gridbag.setConstraints(kgRadioButton, c);
        subPanel.add(kgRadioButton);

        lbRadioButton = new JRadioButton("lb");
        gridbag.setConstraints(lbRadioButton, c);
        subPanel.add(lbRadioButton);

        otherMassRadioButton = new JRadioButton("other");
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(otherMassRadioButton, c);
        subPanel.add(otherMassRadioButton);

        otherMassUnitTextField = new JTextField(4);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(otherMassUnitTextField, c);
        subPanel.add(otherMassUnitTextField);

        bg.add(mgRadioButton);
        bg.add(gRadioButton);
        bg.add(kgRadioButton);
        bg.add(lbRadioButton);
        bg.add(otherMassRadioButton);
        
        // only allow g
        gRadioButton.setSelected(true);
        mgRadioButton.setEnabled(false);
        kgRadioButton.setEnabled(false);
        lbRadioButton.setEnabled(false);
        otherMassRadioButton.setEnabled(false);
        otherMassUnitTextField.setEnabled(false);        
        
        // Energy units
        panel.add(subPanel = new JPanel(gridbag, false));
        bg = new ButtonGroup();
        subPanel.setBorder(new CompoundBorder(
               BorderFactory.createTitledBorder("Energy Unit"),
               new EmptyBorder(5, 2, 5, 2)));

        jouleRadioButton = new JRadioButton("Joule");
        gridbag.setConstraints(jouleRadioButton, c);
        subPanel.add(jouleRadioButton);

        calRadioButton = new JRadioButton("cal");
        gridbag.setConstraints(calRadioButton, c);
        subPanel.add(calRadioButton);

        otherEnergyRadioButton = new JRadioButton("other");
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(otherEnergyRadioButton, c);
        subPanel.add(otherEnergyRadioButton);

        otherEnergyUnitTextField = new JTextField(4);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(otherEnergyUnitTextField, c);
        subPanel.add(otherEnergyUnitTextField);

        bg.add(jouleRadioButton);
        bg.add(calRadioButton);
        bg.add(otherEnergyRadioButton);
        
        // only allow Joule
        jouleRadioButton.setSelected(true);
        calRadioButton.setEnabled(false);
        otherEnergyRadioButton.setEnabled(false);
        otherEnergyUnitTextField.setEnabled(false);        

        // Create a panel to hold misc options
        add(panel = new JPanel(new GridLayout(2, 2, 10, 1), false));
        c.anchor = GridBagConstraints.CENTER;
        c.insets = new Insets(5, 20, 10, 20);
        c.fill = GridBagConstraints.HORIZONTAL;
        gridbag.setConstraints(panel, c);
        panel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Misc Options"),
                new EmptyBorder(5, 20, 5, 20)));
        panel.add(useRadialCoordCheckBox = 
                    new JCheckBox("Use radial coordinates"));
        panel.add(doEvaporationCheckBox = 
                    new JCheckBox("Simulate evaporation"));
        panel.add(doEnergyTransportCheckBox = 
                    new JCheckBox("Simulate heat transport", true));
        panel.add(doSoluteTransportCheckBox = 
                    new JCheckBox("Simulate solute transport", true));
        panel.add(doTranspirationCheckBox = 
                    new JCheckBox("Simulate transpiration"));
        doEnergyTransportCheckBox.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onEnergyTransportCheckBox();
            }
        });
        doSoluteTransportCheckBox.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onSoluteTransportCheckBox();
            }
        });
    }

    /**
     * Puts values in the components in the panel
     */
    public void init() {

        titleTextField.setText(title);
        setSelectedUnits();

        useRadialCoordCheckBox.setSelected(useRadialCoord);
        doEnergyTransportCheckBox.setSelected(doEnergyTransport);  // new in Version 1.4
        doSoluteTransportCheckBox.setSelected(doSoluteTransport);  // new in Version 1.4
        doEvaporationCheckBox.setSelected(doEvaporation);
        doTranspirationCheckBox.setSelected(doTranspiration);

        useRadialCoordCheckBox.setEnabled(radialCoordinatesEnabled);
        
        onEnergyTransportCheckBox();
        onSoluteTransportCheckBox();
    }

    /**
     * Restore or remove the transport panel when the heat transport check box is
     * checked or unchecked.
     */
    protected void onEnergyTransportCheckBox() {
        if (doEnergyTransportCheckBox.isSelected() || doSoluteTransportCheckBox.isSelected()) {
            parentDialog.transportPanel.setEnabled(false);
        } else {
            parentDialog.transportPanel.setEnabled(true);
        }
        
        // allow only meters for length if ANY transport
        if (doEnergyTransportCheckBox.isSelected() || doSoluteTransportCheckBox.isSelected()) {            
            mRadioButton.setSelected(true);
            mmRadioButton.setEnabled(false);
            cmRadioButton.setEnabled(false);
            ftRadioButton.setEnabled(false);
            otherLengthRadioButton.setEnabled(false);
            otherLengthUnitTextField.setEnabled(false);
        } else {
            mmRadioButton.setEnabled(true);
            cmRadioButton.setEnabled(true);
            ftRadioButton.setEnabled(true);
            otherLengthRadioButton.setEnabled(true);            
            otherLengthUnitTextField.setEnabled(true);
        }
        
        // only allow sec for time if heat transport
        if (doEnergyTransportCheckBox.isSelected()) {
            secRadioButton.setSelected(true);
            hourRadioButton.setEnabled(false);
            dayRadioButton.setEnabled(false);
            yearRadioButton.setEnabled(false);
            otherTimeRadioButton.setEnabled(false);
            otherTimeUnitTextField.setEnabled(false);
        } else {
            hourRadioButton.setEnabled(true);
            dayRadioButton.setEnabled(true);
            yearRadioButton.setEnabled(true);
            otherTimeRadioButton.setEnabled(true);            
            otherTimeUnitTextField.setEnabled(true);
        }
        
        parentDialog.solverPanel.doEnergyTransport(
                doEnergyTransportCheckBox.isSelected());
    }
            
    /**
     * Restore or remove the transport panel when the solute transport check box is
     * checked or unchecked.
     */
    protected void onSoluteTransportCheckBox() {
        if (doEnergyTransportCheckBox.isSelected() || doSoluteTransportCheckBox.isSelected()) {
            parentDialog.transportPanel.setEnabled(false);
        } else {
            parentDialog.transportPanel.setEnabled(true);
        }
        
        // allow only meters for length if ANY transport
        if (doEnergyTransportCheckBox.isSelected() || doSoluteTransportCheckBox.isSelected()) {            
            mRadioButton.setSelected(true);
            mmRadioButton.setEnabled(false);
            cmRadioButton.setEnabled(false);
            ftRadioButton.setEnabled(false);
            otherLengthRadioButton.setEnabled(false);
            otherLengthUnitTextField.setEnabled(false);
        } else {
            mmRadioButton.setEnabled(true);
            cmRadioButton.setEnabled(true);
            ftRadioButton.setEnabled(true);
            otherLengthRadioButton.setEnabled(true);            
            otherLengthUnitTextField.setEnabled(true);
        }
        
        parentDialog.solverPanel.doSoluteTransport(
                doSoluteTransportCheckBox.isSelected());

        parentDialog.transportPanel.doSoluteTransport(
                doSoluteTransportCheckBox.isSelected());

        parentDialog.outputPanel.doSoluteTransport(
                doSoluteTransportCheckBox.isSelected());

    }
    
    /**
     * Get values from components.
     */
    public boolean retrieveData() {
        title = titleTextField.getText();
        boolean b = lengthUnit.equals(getSelectedLengthUnit());
        b = b && timeUnit.equals(getSelectedTimeUnit());
        b = b && energyUnit.equals(getSelectedEnergyUnit());
        b = b && massUnit.equals(getSelectedMassUnit());
        
        if (unitChangeWarningEnabled && !b) {
            String [] messageLines = new String [3];
            messageLines[0] = "You are about to change units.";
            messageLines[1] = "All data will keep their current numerical values.";
            messageLines[2] = "Do you want to continue?";
            int result = mp2MessageBox.showConfirmDialog(
                            messageLines, "Warning");
            if (result == mp2MessageBox.NO_OPTION) {
                // Undo unit selection
                setSelectedUnits();
                return false;
            }
            else if (result == mp2MessageBox.CANCEL_OPTION) {
                return false;
            }
        }
        lengthUnit = getSelectedLengthUnit();
        timeUnit = getSelectedTimeUnit();
        energyUnit = getSelectedEnergyUnit();
        massUnit = getSelectedMassUnit();
        useRadialCoord = useRadialCoordCheckBox.isSelected();
        doEnergyTransport = doEnergyTransportCheckBox.isSelected();    // new in Version 1.4
        doSoluteTransport = doSoluteTransportCheckBox.isSelected();    // new in Version 1.4
        doEvaporation = doEvaporationCheckBox.isSelected();
        doTranspiration = doTranspirationCheckBox.isSelected();
        return true;
    }

    protected void setSelectedUnits() {
        // length units
        if (lengthUnit.equals("mm  ")) {
            mmRadioButton.setSelected(true);
        }
        else if (lengthUnit.equals("cm  ")) {
            cmRadioButton.setSelected(true);
        }
        else if (lengthUnit.equals("m   ")) {
            mRadioButton.setSelected(true);
        }
        else if (lengthUnit.equals("ft  ")) {
            ftRadioButton.setSelected(true);
        }
        else {
            otherLengthRadioButton.setSelected(true);
            otherLengthUnitTextField.setText(lengthUnit);
        }

        // time units
        if (timeUnit.equals("sec ")) {
            secRadioButton.setSelected(true);
        }
        else if (timeUnit.equals("hour")) {
            hourRadioButton.setSelected(true);
        }
        else if (timeUnit.equals("day ")) {
            dayRadioButton.setSelected(true);
        }
        else if (timeUnit.equals("year")) {
            yearRadioButton.setSelected(true);
        }
        else {
            otherTimeRadioButton.setSelected(true);
            otherTimeUnitTextField.setText(timeUnit);
        }

        // energy units
        if (energyUnit.equals("J   ")) {
            jouleRadioButton.setSelected(true);
        }
        else if (energyUnit.equals("cal ")) {
            calRadioButton.setSelected(true);
        }
        else {
            otherEnergyRadioButton.setSelected(true);
            otherEnergyUnitTextField.setText(energyUnit);
        }
        
        // mass units
        if (massUnit.equals("mg  ")) {
            mgRadioButton.setSelected(true);
        }
        else if (massUnit.equals("g   ")) {
            gRadioButton.setSelected(true);
        }
        else if (massUnit.equals("Kg  ")) {
            kgRadioButton.setSelected(true);
        }
        else if (massUnit.equals("lb  ")) {
            lbRadioButton.setSelected(true);
        }
        else {
            otherMassRadioButton.setSelected(true);
            otherMassUnitTextField.setText(massUnit);
        }
    }

    protected String getSelectedLengthUnit() {

        if (mmRadioButton.isSelected()) {
            return "mm  ";
        }
        else if (cmRadioButton.isSelected()) {
            return "cm  ";
        }
        else if (mRadioButton.isSelected()) {
            return "m   ";
        }
        else if (ftRadioButton.isSelected()) {
            return "ft  ";
        }
        else if (otherLengthRadioButton.isSelected()) {
            return otherLengthUnitTextField.getText();
        }
        else {
            return "";
        }

    }

    protected String getSelectedTimeUnit() {
        
        if (secRadioButton.isSelected()) {
            return "sec ";
        }
        else if (hourRadioButton.isSelected()) {
            return "hour";
        }
        else if (dayRadioButton.isSelected()) {
            return "day ";
        }
        else if (yearRadioButton.isSelected()) {
            return "year";
        }
        else if (otherTimeRadioButton.isSelected()) {
            return otherTimeUnitTextField.getText();
        }
        else {
            return "";
        }
    }

    protected String getSelectedMassUnit() {
        if (mgRadioButton.isSelected()) {
            return "mg  ";
        }
        else if (gRadioButton.isSelected()) {
            return "g   ";
        }
        else if (kgRadioButton.isSelected()) {
            return "Kg  ";
        }
        else if (lbRadioButton.isSelected()) {
            return "lb  ";
        }
        else if (otherMassRadioButton.isSelected()) {
            return otherMassUnitTextField.getText();
        }
        else {
            return "";
        }
    }
    
    protected String getSelectedEnergyUnit() {
        if (jouleRadioButton.isSelected()) {
            return "J   ";
        }
        else if (calRadioButton.isSelected()) {
            return "cal ";
        }
        else if (otherEnergyRadioButton.isSelected()) {
            return otherEnergyUnitTextField.getText();
        }
        else {
            return "";
        }
    }
}

