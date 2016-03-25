/*
 * mp2VectorDialog.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class mp2VectorDialog extends mp2Dialog implements mp2Constants {

    public float vectorMagnitudePerInch;
    public int vectorColInterval;
    public int vectorRowInterval;
    public int vectorMode;
    public boolean showStems;

    protected JFrame parent;
    protected mp2VectorDialogCaller caller;

    protected JTextField vectorMagnitudePerInchTextField;
    protected JTextField vectorColIntervalTextField;
    protected JTextField vectorRowIntervalTextField;
    protected JRadioButton vectorAsVelocityRadioButton;
    protected JRadioButton vectorAsFluxRadioButton;
    protected JButton applyButton;
    protected JCheckBox showStemsCheckBox;

    public mp2VectorDialog(JFrame parent, mp2VectorDialogCaller caller,
                           String title, boolean doVectorOption) {
        super(title, false, new Boolean(doVectorOption), parent);
        this.parent = parent;
        this.caller = caller;
    }

    protected void makeContents() {
        
        boolean doVectorOption = ((Boolean) customObject).booleanValue();
        
        JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        JPanel mainPanel = new JPanel(gridbag);
        centerPanel.add(mainPanel, BorderLayout.CENTER);

        JPanel panel = new JPanel(new GridLayout(3, 1));
        c.fill = GridBagConstraints.VERTICAL;
        c.insets = new Insets(0, 0, 0, 20);
        gridbag.setConstraints(panel, c);
        if (doVectorOption) {
            mainPanel.add(panel);
        }

        panel.add(new JLabel("Show vector as:"));
        panel.add(vectorAsVelocityRadioButton = new JRadioButton("Velocity"));
        panel.add(vectorAsFluxRadioButton = new JRadioButton("Flux"));

        ButtonGroup bg = new ButtonGroup();
        bg.add(vectorAsVelocityRadioButton);
        bg.add(vectorAsFluxRadioButton);

        panel = new JPanel(new GridLayout(3, 1, 5, 5));
        c.fill = GridBagConstraints.VERTICAL;
        c.insets = new Insets(0, 0, 0, 0);
        gridbag.setConstraints(panel, c);
        mainPanel.add(panel);

        panel.add(new JLabel("Scale: 1 inch =", SwingConstants.RIGHT));
        panel.add(new JLabel("Column interval", SwingConstants.RIGHT));
        panel.add(new JLabel("Row interval", SwingConstants.RIGHT));

        panel = new JPanel(new GridLayout(3, 1, 5, 5));
        c.insets = new Insets(0, 10, 0, 0);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(panel, c);
        mainPanel.add(panel);

        panel.add(vectorMagnitudePerInchTextField = new JTextField(8));
        panel.add(vectorColIntervalTextField = new JTextField(8));
        panel.add(vectorRowIntervalTextField = new JTextField(8));
        
        panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel.add(showStemsCheckBox = new JCheckBox("Show stems"));
        centerPanel.add(panel, BorderLayout.SOUTH);

        applyButton = new JButton("Apply");
        applyButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onApply();
            }
        });
    }

    public boolean doModal() {
        vectorMagnitudePerInchTextField.setText(String.valueOf(vectorMagnitudePerInch));
        vectorColIntervalTextField.setText(String.valueOf(vectorColInterval));
        vectorRowIntervalTextField.setText(String.valueOf(vectorRowInterval));
        if (vectorMode == VECTOR_AS_VELOCITY) {
            vectorAsVelocityRadioButton.setSelected(true);
        } else {
            vectorAsFluxRadioButton.setSelected(true);
        }
        showStemsCheckBox.setSelected(showStems);
        buttonPanel.add(applyButton, 1);
        return super.doModal();
    }

    protected boolean retrieveData() {
        try {
            vectorMagnitudePerInch = Float.valueOf(vectorMagnitudePerInchTextField.getText()).floatValue();
            vectorColInterval = Integer.parseInt(vectorColIntervalTextField.getText());
            vectorRowInterval = Integer.parseInt(vectorRowIntervalTextField.getText());
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog(parent, "Please check your input.",
                                "Input Error");
            return false;
        }
        if (!dataCheck(vectorMagnitudePerInch, "\"Scale\"", IS_POSITIVE,
                        vectorMagnitudePerInchTextField)) {
            return false;
        }
        if (!dataCheck(vectorColInterval, "\"Column interval\"", IS_POSITIVE,
                        vectorColIntervalTextField)) {
            return false;
        }
        if (!dataCheck(vectorRowInterval, "\"Row interval\"", IS_POSITIVE,
                        vectorRowIntervalTextField)) {
            return false;
        }
        if (vectorAsVelocityRadioButton.isSelected()) {
            vectorMode = VECTOR_AS_VELOCITY;
        } else {
            vectorMode = VECTOR_AS_FLUX;
        }
        showStems = showStemsCheckBox.isSelected();
        return true;
    }

    protected void onApply() {
        if (retrieveData()) {
            caller.applyVectorProperties(vectorMagnitudePerInch, vectorColInterval,
                                        vectorRowInterval, vectorMode, showStems); 
        }
    }
}
