/*
 * mp2ColorScaleDialog.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;


public class mp2ColorScaleDialog extends mp2Dialog {

    public double valueRed;
    public double valueBlue;
    public double colorInterval;
    public double labelInterval;

    protected JFrame parent;
    protected mp2ColorScaleDialogCaller caller;

    protected JTextField valueRedTextField;
    protected JTextField valueBlueTextField;
    protected JTextField colorIntervalTextField;
    protected JTextField labelIntervalTextField;
    protected JButton applyButton;

    /**
     * Constructor. Note that the calling object must implement the ColorDialogCaller
     * interface to enable callback from the dialog when the "Applied" button is clicked.
     */
    public mp2ColorScaleDialog(JFrame parent, mp2ColorScaleDialogCaller caller) {
        super("Color Scale", false, parent);
        this.parent = parent;
        this.caller = caller;
    }

    protected void makeContents() {

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();

        JPanel centerPanel = new JPanel(gridbag);
        centerPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        JPanel panel = new JPanel(new GridLayout(4, 1, 5, 5));
        c.fill = GridBagConstraints.VERTICAL;
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);

        panel.add(new JLabel("Value at red limit", SwingConstants.RIGHT));
        panel.add(new JLabel("Value at blue limit", SwingConstants.RIGHT));
        panel.add(new JLabel("Contour color interval", SwingConstants.RIGHT));
        panel.add(new JLabel("Label interval", SwingConstants.RIGHT));

        panel = new JPanel(new GridLayout(4, 1, 5, 5));
        c.insets = new Insets(0, 10, 0, 0);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);

        panel.add(valueRedTextField = new JTextField(8));
        panel.add(valueBlueTextField = new JTextField(8));
        panel.add(colorIntervalTextField = new JTextField(8));
        panel.add(labelIntervalTextField = new JTextField(8));

        applyButton = new JButton("Apply");
        applyButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onApply();
            }
        });
    }

    public boolean doModal() {
        valueRedTextField.setText(String.valueOf(valueRed));
        valueBlueTextField.setText(String.valueOf(valueBlue));
        colorIntervalTextField.setText(String.valueOf(colorInterval));
        labelIntervalTextField.setText(String.valueOf(labelInterval));
        buttonPanel.add(applyButton, 1);
        return super.doModal();
    }

    protected boolean retrieveData() {
        try {
            valueRed = Double.valueOf(valueRedTextField.getText()).doubleValue();
            valueBlue = Double.valueOf(valueBlueTextField.getText()).doubleValue();
            colorInterval = Double.valueOf(colorIntervalTextField.getText()).doubleValue();
            labelInterval = Double.valueOf(labelIntervalTextField.getText()).doubleValue();
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog(parent, "Please check your input.",
                                "Input Error");
            return false;
        }
        if (!dataCheck(colorInterval, "\"Color Interval\"", IS_POSITIVE,
                        colorIntervalTextField)) {
            return false;
        }
        if (!dataCheck(labelInterval, "\"Label Interval\"", IS_POSITIVE,
                        labelIntervalTextField)) {
            return false;
        }
        int numInterval = (int) (Math.abs(valueRed-valueBlue)/colorInterval);
        if (numInterval > 20) {
            int result = mp2MessageBox.showYesNoDialog(parent, 
                "This will create contour plots with " + numInterval + " intervals." +
                "Do you want to continue?", "Warning");
            if (result == mp2MessageBox.NO_OPTION) {
                return false;
            }
        }
        return true;
    }

    protected void onApply() {
        if (retrieveData()) {
            caller.applyColorScale(valueBlue, valueRed, colorInterval, labelInterval); 
        }
    }
}
