/*
 * vs2RechargePeriodDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2RechargePeriodDialog extends mp2Dialog {

    public Object [] aRow;
    public int repetitions;
    public boolean multiplePeriods;
    public double minPeriodLength;
    public int periodHavingMinPeriodLength;
    public double maxInitialTimeStep;
    public int periodHavingMaxInitialTimeStep;
    public double minMaxTimeStep;
    public int periodHavingMinMaxTimeStep;
    public double maxMinTimeStep;
    public int periodHavingMaxMinTimeStep;

    // model options
    protected vs2ModelOptions modelOptions;

    // Boolean flag that is true if this dialog box is for editing data, and
    // false if the dialog box is for adding data
    protected boolean doEdit;

    // Components in the dialog box
    protected JTextField periodLengthTextField;
    protected JTextField initialTimeStepTextField;
    protected JTextField timeStepMultiplierTextField;
    protected JTextField maxTimeStepTextField;
    protected JTextField minTimeStepTextField;
    protected JTextField timeStepReductionFactorTextField;
    protected JTextField maxHeadChangeTextField;
    protected JTextField steadyStateCriterionTextField;
    protected JTextField maxPondHeightTextField;
    protected JTextField repetitionTextField;
    protected JCheckBox printResultsCheckBox;
    protected JCheckBox doEvaporationCheckBox;
    protected JCheckBox doTranspirationCheckBox;

    public vs2RechargePeriodDialog(String title, Object [] customArray)	{
        super(title, true, customArray);
        this.doEdit = ((Boolean) customArray[1]).booleanValue();
        this.multiplePeriods = false;
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "rechargePeriodParameters", null);
    }

    protected void makeContents() {
        // get the model options back from the custom object
        modelOptions = (vs2ModelOptions) customArray[0];
        boolean doEdit = ((Boolean) customArray[1]).booleanValue();

        // Make a center panel to hold all the components.
        JPanel centerPanel = new JPanel(false);
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
        centerPanel.setBorder(new EmptyBorder(25, 25, 10, 25));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        // Make a sub panel to hold labels and text fields
        JPanel subPanel = new JPanel(false);
        subPanel.setLayout(new BoxLayout(subPanel, BoxLayout.X_AXIS));
        centerPanel.add(subPanel);

        // Within the sub panel, make a left panel to hold the labels
        JPanel leftPanel = new JPanel(false);
        leftPanel.setLayout(new GridLayout(0, 1, 0, 10));
        subPanel.add(leftPanel);

        // Put space between the left and right panels
        subPanel.add(Box.createHorizontalStrut(20));

        // Make a right panel to hold the text fields
        JPanel rightPanel = new JPanel(false);
        rightPanel.setLayout(new GridLayout(0, 1, 0, 10));
        subPanel.add(rightPanel);
        
        // Make a right panel to hold the text fields
        JPanel unitsPanel = new JPanel(false);
        unitsPanel.setLayout(new GridLayout(0, 1, 0, 10));
        subPanel.add(unitsPanel);
        
        String T = modelOptions.T();
        String L = modelOptions.L();

        // Add the labels and text fields to left and right panels respectively
        leftPanel.add(new JLabel("Period length", SwingConstants.RIGHT));
        rightPanel.add(periodLengthTextField = new JTextField(5));
        unitsPanel.add(new JLabel(T, SwingConstants.CENTER));

        leftPanel.add(new JLabel("Initial time step", SwingConstants.RIGHT));
        rightPanel.add(initialTimeStepTextField = new JTextField(5));
        unitsPanel.add(new JLabel(T, SwingConstants.CENTER));

        leftPanel.add(new JLabel("Time step multiplier", SwingConstants.RIGHT));
        rightPanel.add(timeStepMultiplierTextField = new JTextField(5));
        unitsPanel.add(new JLabel("-", SwingConstants.CENTER));

        leftPanel.add(new JLabel("Maximum time step", SwingConstants.RIGHT));
        rightPanel.add(maxTimeStepTextField = new JTextField(5));
        unitsPanel.add(new JLabel(T, SwingConstants.CENTER));

        leftPanel.add(new JLabel("Minimum time step", SwingConstants.RIGHT));
        rightPanel.add(minTimeStepTextField = new JTextField(5));
        unitsPanel.add(new JLabel(T, SwingConstants.CENTER));

        leftPanel.add(new JLabel("Time step reduction factor", SwingConstants.RIGHT));
        rightPanel.add(timeStepReductionFactorTextField = new JTextField(5));
        unitsPanel.add(new JLabel("-", SwingConstants.CENTER));

        leftPanel.add(new JLabel("Maximum head change", SwingConstants.RIGHT));
        rightPanel.add(maxHeadChangeTextField = new JTextField(5));
        unitsPanel.add(new JLabel(L, SwingConstants.CENTER));

        leftPanel.add(new JLabel("Steady-state head criterion", SwingConstants.RIGHT));
        rightPanel.add(steadyStateCriterionTextField = new JTextField(5));
        unitsPanel.add(new JLabel(L, SwingConstants.CENTER));

        leftPanel.add(new JLabel("Maximum height of ponding", SwingConstants.RIGHT));
        rightPanel.add(maxPondHeightTextField = new JTextField(5));
        unitsPanel.add(new JLabel(L, SwingConstants.CENTER));

        // Put space below the sub panel
        centerPanel.add(Box.createVerticalStrut(20));

        // Add the check boxes
        centerPanel.add(printResultsCheckBox = new JCheckBox("Print results"));
        if (modelOptions.doEvaporation) {
            centerPanel.add(doEvaporationCheckBox = new JCheckBox(
            "Simulate Evaporation"));
        }
        if (modelOptions.doTranspiration) {
            centerPanel.add(doTranspirationCheckBox = new JCheckBox(
            "Simulate Transpiration"));
        }
        if (!doEdit) {
            centerPanel.add(Box.createVerticalStrut(10));
            subPanel = new JPanel();
            subPanel.add(new JLabel("Repetitions", SwingConstants.RIGHT));
            subPanel.add(repetitionTextField = new JTextField(5));
            centerPanel.add(subPanel);
        }
    }

    public boolean doModal() {
        // If we are editing data, then put the current data in the text fields
        if (doEdit || ((Double) aRow[1]).doubleValue() > 0) {
            if (((Double) aRow[1]).doubleValue() != Double.MIN_VALUE) {
                periodLengthTextField.setText(String.valueOf(
                        ((Double) aRow[1]).doubleValue()));
            } else {
                periodLengthTextField.setText("as is");
            }
            if (((Double) aRow[2]).doubleValue() != Double.MIN_VALUE) {
                initialTimeStepTextField.setText(String.valueOf(
                        ((Double) aRow[2]).doubleValue()));
            } else {
                initialTimeStepTextField.setText("as is");
            }
            if (((Double) aRow[3]).doubleValue() != Double.MIN_VALUE) {
                timeStepMultiplierTextField.setText(String.valueOf(
                        ((Double) aRow[3]).doubleValue()));
            } else {
                timeStepMultiplierTextField.setText("as is");
            }
            if (((Double) aRow[4]).doubleValue() != Double.MIN_VALUE) {
                maxTimeStepTextField.setText(String.valueOf(
                        ((Double) aRow[4]).doubleValue()));
            } else {
                maxTimeStepTextField.setText("as is");
            }
            if (((Double) aRow[5]).doubleValue() != Double.MIN_VALUE) {
                minTimeStepTextField.setText(String.valueOf(
                        ((Double) aRow[5]).doubleValue()));
            } else {
                minTimeStepTextField.setText("as is");
            }
            if (((Double) aRow[6]).doubleValue() != Double.MIN_VALUE) {
                timeStepReductionFactorTextField.setText(String.valueOf(
                        ((Double) aRow[6]).doubleValue()));
            } else {
                timeStepReductionFactorTextField.setText("as is");
            }
            if (((Double) aRow[7]).doubleValue() != Double.MIN_VALUE) {
                maxHeadChangeTextField.setText(String.valueOf(
                        ((Double) aRow[7]).doubleValue()));
            } else {
                maxHeadChangeTextField.setText("as is");
            }
            if (((Double) aRow[8]).doubleValue() != Double.MIN_VALUE) {
                steadyStateCriterionTextField.setText(String.valueOf(
                        ((Double) aRow[8]).doubleValue()));
            } else {
                steadyStateCriterionTextField.setText("as is");
            }
            if (((Double) aRow[9]).doubleValue() != Double.MIN_VALUE) {
                maxPondHeightTextField.setText(String.valueOf(
                        ((Double) aRow[9]).doubleValue()));
            } else {
                maxPondHeightTextField.setText("as is");
            }
            printResultsCheckBox.setSelected(
            ((Boolean) aRow[10]).booleanValue());
            if (modelOptions.doEvaporation) {
                doEvaporationCheckBox.setSelected(
                        ((Boolean) aRow[11]).booleanValue());
            }
            if (modelOptions.doTranspiration) {
                doTranspirationCheckBox.setSelected(
                    ((Boolean) aRow[12]).booleanValue());
            }
        }
        if (!doEdit) {
            repetitionTextField.setText("1");
        }

        // Request focus for the period length text field when the dialog box opens
        periodLengthTextField.requestFocus();

        // Call superclass method to show the dialog box and return result
        // when the dialog box closes
        return super.doModal();
    }

    protected boolean retrieveData() {
        double len=0, step=0, mult=0, maxStep=0, minStep=0, reduct=0, maxDh=0,
        ssCrit=0, maxPond=0;
        int rep = 1;
        try { 
            if (multiplePeriods && periodLengthTextField.getText().trim().equalsIgnoreCase("as is")) {
                len = Double.MIN_VALUE;              
            } else {
                len = Double.valueOf(periodLengthTextField.getText()).doubleValue();
            }
            if (multiplePeriods && initialTimeStepTextField.getText().trim().equalsIgnoreCase("as is")) {
                step = Double.MIN_VALUE;              
            } else {
                step = Double.valueOf(initialTimeStepTextField.getText()).doubleValue();
            }
            if (multiplePeriods && timeStepMultiplierTextField.getText().trim().equalsIgnoreCase("as is")) {
                mult = Double.MIN_VALUE;              
            } else {
                mult = Double.valueOf(timeStepMultiplierTextField.getText()).doubleValue();
            }
            if (multiplePeriods && maxTimeStepTextField.getText().trim().equalsIgnoreCase("as is")) {
                maxStep = Double.MIN_VALUE;              
            } else {
                maxStep = Double.valueOf(maxTimeStepTextField.getText()).doubleValue();
            }
            if (multiplePeriods && minTimeStepTextField.getText().trim().equalsIgnoreCase("as is")) {
                minStep = Double.MIN_VALUE;              
            } else {
                minStep = Double.valueOf(minTimeStepTextField.getText()).doubleValue();
            }
            if (multiplePeriods && timeStepReductionFactorTextField.getText().trim().equalsIgnoreCase("as is")) {
                reduct = Double.MIN_VALUE;              
            } else {
                reduct = Double.valueOf(timeStepReductionFactorTextField.getText()).doubleValue();
            }
            if (multiplePeriods && maxHeadChangeTextField.getText().trim().equalsIgnoreCase("as is")) {
                maxDh = Double.MIN_VALUE;              
            } else {
                maxDh = Double.valueOf(maxHeadChangeTextField.getText()).doubleValue();
            }
            if (multiplePeriods && steadyStateCriterionTextField.getText().trim().equalsIgnoreCase("as is")) {
                ssCrit = Double.MIN_VALUE;              
            } else {
                ssCrit = Double.valueOf(steadyStateCriterionTextField.getText()).doubleValue();
            }
            if (multiplePeriods && maxPondHeightTextField.getText().trim().equalsIgnoreCase("as is")) {
                maxPond = Double.MIN_VALUE;              
            } else {
                maxPond = Double.valueOf(maxPondHeightTextField.getText()).doubleValue();
            }
            if (!doEdit) {
                rep = Integer.parseInt(repetitionTextField.getText());
            }
        }
        catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
            "Input Error");
            return false;
        }

        // Check that input data are correct
        if (len != Double.MIN_VALUE && !dataCheck(len,       "\"Period length\"",     IS_POSITIVE, periodLengthTextField))
            return false;
        if (step != Double.MIN_VALUE && !dataCheck(step,    "\"Initial time step\"",   IS_POSITIVE, initialTimeStepTextField))
            return false;
        if (len != Double.MIN_VALUE && step != Double.MIN_VALUE) {
            if (!dataCheck(step, "\"Initial time step\"", IS_LESS_THAN_OR_EQUAL_TO, len, "\"Period length\"",
                    initialTimeStepTextField)) return false;
        } else if (len == Double.MIN_VALUE && step != Double.MIN_VALUE) {
            if (!dataCheck(step, "\"Initial time step\"", IS_LESS_THAN_OR_EQUAL_TO, minPeriodLength, 
                    String.valueOf(minPeriodLength) + " (Length of recharge period " + periodHavingMinPeriodLength + ")", 
                    initialTimeStepTextField)) return false;
        } else if (len != Double.MIN_VALUE && step == Double.MIN_VALUE) {
            if (!dataCheck(len, "\"Period length\"", IS_GREATER_THAN_OR_EQUAL_TO, maxInitialTimeStep, 
                    String.valueOf(maxInitialTimeStep) + " (Initial time step for recharge period " + periodHavingMaxInitialTimeStep + ")", 
                    periodLengthTextField)) return false;
        } else {
            // no check   
        }
        if (mult != Double.MIN_VALUE && !dataCheck(mult,   "\"Time step multiplier\"", IS_POSITIVE, timeStepMultiplierTextField))
            return false;
        if (maxStep != Double.MIN_VALUE && !dataCheck(maxStep, "\"Maximum time step\"",   IS_POSITIVE, maxTimeStepTextField))
            return false;
        if (minStep != Double.MIN_VALUE && !dataCheck(minStep, "\"Minimum time step\"",   IS_POSITIVE, minTimeStepTextField))
            return false;
        if (maxStep != Double.MIN_VALUE && minStep != Double.MIN_VALUE) {
            if (!dataCheck(maxStep, "\"Maximum time step\"", IS_GREATER_THAN_OR_EQUAL_TO, minStep,
                    "\"Minimum time step\"", maxTimeStepTextField)) return false;
        } else if (maxStep == Double.MIN_VALUE && minStep != Double.MIN_VALUE) {
            if (!dataCheck(minStep, "\"Minimum time step\"", IS_LESS_THAN_OR_EQUAL_TO, minMaxTimeStep, 
                String.valueOf(minMaxTimeStep) + " (Maximum time step for recharge period " + periodHavingMinMaxTimeStep + ")", 
                minTimeStepTextField)) return false;
        } else if (maxStep != Double.MIN_VALUE && minStep == Double.MIN_VALUE) {
            if (!dataCheck(maxStep, "\"Maximum time step\"", IS_GREATER_THAN_OR_EQUAL_TO, maxMinTimeStep, 
                String.valueOf(maxMinTimeStep) + " (Minimum time step for recharge period " + periodHavingMaxMinTimeStep + ")", 
                maxTimeStepTextField)) return false;
        } else {
            // no check   
        }
        if (reduct != Double.MIN_VALUE && !dataCheck(reduct, "\"Time step reduction factor\"", IS_BETWEEN_INCLUSIVE, 0, "0", 1, "1",
            timeStepReductionFactorTextField)) return false;
        if (maxDh != Double.MIN_VALUE && !dataCheck(maxDh,     "\"Maximum head change\"",      IS_POSITIVE, maxHeadChangeTextField))
            return false;
        if (ssCrit != Double.MIN_VALUE && !dataCheck(ssCrit, "\"Steady-state head criterion\"", IS_NON_NEGATIVE, steadyStateCriterionTextField))
            return false;
        if (!doEdit && !dataCheck(rep, "\"Repetition\"",  IS_POSITIVE, repetitionTextField))
            return false;

        // When execution reaches this point, all input data are OK so we put
        // them in aRow and return true
        aRow[1] = new Double(len);
        aRow[2] = new Double(step);
        aRow[3] = new Double(mult);
        aRow[4] = new Double(maxStep);
        aRow[5] = new Double(minStep);
        aRow[6] = new Double(reduct);
        aRow[7] = new Double(maxDh);
        aRow[8] = new Double(ssCrit);
        aRow[9] = new Double(maxPond);
        aRow[10] = new Boolean(printResultsCheckBox.isSelected());
        if (modelOptions.doEvaporation) {
            aRow[11] = new Boolean(doEvaporationCheckBox.isSelected());
        }
        if (modelOptions.doTranspiration) {
            aRow[12] = new Boolean(doTranspirationCheckBox.isSelected());
        }
        repetitions = rep;
        return true;
    }

    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile ("rechargePeriodParameters.html");
    }

}

