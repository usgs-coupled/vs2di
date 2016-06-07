/*
 * vs2SolverPanel.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;
import javax.swing.*;

public class vs2SolverPanel extends vs2ModelOptionsPanel {

    public double hmax;
    public int minit;
    public int itmax;
    public double eps;
    public double eps1;
    public double eps2;
    public double eps3;                         // new in Version 1.4
    public boolean itstop;
    public int maxStep;

    protected JTextField hmaxTextField;
    protected JTextField minitTextField;
    protected JTextField itmaxTextField;
    protected JTextField epsTextField;
    protected JTextField eps1TextField;
    protected JTextField eps2TextField;
    protected JTextField eps3TextField;         // new in Version 1.4
    protected JLabel eps1Label;
    protected JLabel eps2Label;
    protected JLabel eps3Label;                 // new in Version 1.4
    protected JTextField maxStepTextField;
    protected JCheckBox itstopCheckBox;
    
    protected boolean doEnergyTransport;        // new in Version 1.4
    protected boolean doSoluteTransport;        // new in Version 1.4


    /**
     * Constructs the panel for solver options
     */
    public vs2SolverPanel(vs2ModelOptionsDialog parentDialog) {

        super(parentDialog);

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        setBorder(new EmptyBorder(10, 10, 10, 10));
        setLayout(gridbag);

        JPanel panel, leftPanel, rightPanel, subPanel;

        add(panel = new JPanel(gridbag, false));
        c.anchor = GridBagConstraints.CENTER;
        gridbag.setConstraints(panel, c);
        panel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Solver Options"),
                new EmptyBorder(5, 20, 10, 20)));

        panel.add(leftPanel = new JPanel(new GridLayout(3, 1, 0, 10)));
        c.fill = GridBagConstraints.VERTICAL;
        c.insets = new Insets(0, 0, 0, 10);
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(leftPanel, c);
        panel.add(rightPanel = new JPanel(new GridLayout(3, 1, 0, 10)));
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.insets = new Insets(0, 0, 0, 0);
        gridbag.setConstraints(rightPanel, c);

        leftPanel.add(new JLabel("Relaxation parameter", 
                                    SwingConstants.RIGHT));
        rightPanel.add(hmaxTextField = new JTextField(6));

        leftPanel.add(new JLabel("Minimum iterations per time step", 
                                    SwingConstants.RIGHT));
        rightPanel.add(minitTextField = new JTextField(6));

        leftPanel.add(new JLabel("Maximum iterations per time step", 
                                    SwingConstants.RIGHT));
        rightPanel.add(itmaxTextField = new JTextField(6));

        panel.add(itstopCheckBox = new JCheckBox(
                        "Stop simulation at convergence failure"));
        c.fill = GridBagConstraints.NONE;
        c.insets = new Insets(10, 0, 20, 0);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(itstopCheckBox, c);

        //int rows = vs2App.doHeat() ? 4 : 3;
        int rows = 5;
        panel.add(leftPanel = new JPanel(new GridLayout(rows, 1, 0, 10)));
        c.fill = GridBagConstraints.VERTICAL;
        c.insets = new Insets(0, 0, 0, 10);
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(leftPanel, c);
        panel.add(rightPanel = new JPanel(new GridLayout(rows, 1, 0, 10)));
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.insets = new Insets(0, 0, 0, 0);
        gridbag.setConstraints(rightPanel, c);

        leftPanel.add(new JLabel("Maximum number of time steps",
                                    SwingConstants.RIGHT));
        rightPanel.add(maxStepTextField = new JTextField(6));

        leftPanel.add(new JLabel("Closure criterion for head", 
                                    SwingConstants.RIGHT));
        rightPanel.add(epsTextField = new JTextField(6));

        leftPanel.add(eps1Label = new JLabel(
                "Closure criterion for temperature",
                SwingConstants.RIGHT));
        rightPanel.add(eps1TextField = new JTextField(6));

        leftPanel.add(eps2Label = new JLabel(
                "Closure criterion for velocity",
                SwingConstants.RIGHT));
        rightPanel.add(eps2TextField = new JTextField(6));

        leftPanel.add(eps3Label = new JLabel(
                "Closure criterion for concentration",
                SwingConstants.RIGHT));
        rightPanel.add(eps3TextField = new JTextField(6));
    }

    /**
     * Puts values in components
     */
    public void init() {
        hmaxTextField.setText(String.valueOf(hmax));
        minitTextField.setText(String.valueOf(minit));
        itmaxTextField.setText(String.valueOf(itmax));
        itstopCheckBox.setSelected(itstop);
        maxStepTextField.setText(String.valueOf(maxStep));
        epsTextField.setText(String.valueOf(eps));
        eps1TextField.setText(String.valueOf(eps1));
        eps2TextField.setText(String.valueOf(eps2));
        eps3TextField.setText(String.valueOf(eps3));
    }

    public void doEnergyTransport(boolean b) {
        doEnergyTransport = b;
        eps1Label.setEnabled(b);
        eps1TextField.setEnabled(b);
        eps2Label.setEnabled(b);
        eps2TextField.setEnabled(b);
        revalidate();
    }
    
    public void doSoluteTransport(boolean b) {
        doSoluteTransport = b;
        eps3Label.setEnabled(b);
        eps3TextField.setEnabled(b);
        revalidate();
    }

    /**
     * Get values from components
     */
    public boolean retrieveData() {
        itstop = itstopCheckBox.isSelected();
        if (doEnergyTransport) assert(eps1TextField.isEnabled());
        if (doEnergyTransport) assert(eps2TextField.isEnabled());
        if (doSoluteTransport) assert(eps3TextField.isEnabled());
        try {
            hmax = Double.valueOf(hmaxTextField.getText()).doubleValue();
            minit = Integer.parseInt(minitTextField.getText());
            itmax = Integer.parseInt(itmaxTextField.getText());
            maxStep = Integer.parseInt(maxStepTextField.getText());
            eps = Double.valueOf(epsTextField.getText()).doubleValue();
            if (doEnergyTransport && eps1TextField.isEnabled()) {
                eps1 = Double.valueOf(eps1TextField.getText()).doubleValue();
            }
            if (doEnergyTransport && eps2TextField.isEnabled()) {
                eps2 = Double.valueOf(eps2TextField.getText()).doubleValue();
            }
            if (doSoluteTransport && eps3TextField.isEnabled()) {
                eps3 = Double.valueOf(eps3TextField.getText()).doubleValue();
            }
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input",
                "Input Error");
                return false;
        }

        if (!mp2Dialog.dataCheck(hmax, "\"Relaxation parameter\"", 
                    mp2Dialog.IS_POSITIVE, hmaxTextField)) {
            return false;
        }
        if (!mp2Dialog.dataCheck(minit, "\"Minimum iterations...\"", 
                    mp2Dialog.IS_POSITIVE, minitTextField)) {
            return false;
        }
        if (!mp2Dialog.dataCheck(itmax, "\"Maximum iterations...\"", 
                    mp2Dialog.IS_POSITIVE, itmaxTextField)) {
            return false;
        }
        if (!mp2Dialog.dataCheck(itmax, "\"Maximum iterations...\"", 
                    mp2Dialog.IS_GREATER_THAN, minit, "\"Minimum iterations...\"", 
                    itmaxTextField)) {
            return false;
        }
        if (!mp2Dialog.dataCheck(maxStep, "\"Maximum number of time steps\"", 
                    mp2Dialog.IS_POSITIVE, maxStepTextField)) {
            return false;
        }
        if (!mp2Dialog.dataCheck(eps, "\"Closure criterion for head\"", 
                    mp2Dialog.IS_POSITIVE, epsTextField)) {
            return false;
        }
        if (doEnergyTransport && eps1TextField.isEnabled() &&
                    !mp2Dialog.dataCheck(eps1, "\"Closure criterion for temperature\"", 
                     mp2Dialog.IS_POSITIVE, eps1TextField)) {
            return false;
        }
        if (doEnergyTransport && eps2TextField.isEnabled() &&
                    !mp2Dialog.dataCheck(eps2, "\"Closure criterion for velocity\"", 
                     mp2Dialog.IS_POSITIVE, eps2TextField)) {
            return false;
        }
        if (doSoluteTransport && eps3TextField.isEnabled() &&
                    !mp2Dialog.dataCheck(eps3, "\"Closure criterion for concentration\"", 
                     mp2Dialog.IS_POSITIVE, eps2TextField)) {
            return false;
        }

        return true;
    }
}

