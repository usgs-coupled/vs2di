/*
 * mp2SimulationDialog
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class mp2SimulationDialog extends mp2Dialog {

    public float secPerStep;
    public float saveInterval;
    public boolean saveBinary;
    protected JTextField secPerStepTextField;
    protected JTextField saveIntervalTextField;
    protected JCheckBox saveBinaryCheckBox;
    protected JPanel dataListPanel;
    protected boolean computing;

    public mp2SimulationDialog(JFrame parent, boolean computing) {
        super("Simulation", true, new Boolean(computing), parent);
        this.computing = computing;
        if (computing) {
	         mp2JavaHelp.hb.enableHelpOnButton(helpButton, "saveSimulation", null);
        } else {
	         mp2JavaHelp.hb.enableHelpOnButton(helpButton, "animationSpeed", null);
        }
    }

    protected void makeContents() {
        boolean computing = ((Boolean) customObject).booleanValue();

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
    
        JPanel centerPanel = new JPanel(gridbag);
        centerPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        JPanel panel = new JPanel(new GridLayout(0, 1, 5, 5));
        c.fill = GridBagConstraints.VERTICAL;
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);

        panel.add(new JLabel("Seconds per time step", SwingConstants.RIGHT));
        if (computing) {
            panel.add(new JLabel("Target time interval between save", SwingConstants.RIGHT));
        }

        panel = new JPanel(new GridLayout(0, 1, 5, 5));
        c.insets = new Insets(0, 10, 0, 0);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);

        panel.add(secPerStepTextField = new JTextField(8));
        if (computing) {
            saveIntervalTextField = new JTextField(8);
            panel.add(saveIntervalTextField);
            panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
            saveBinaryCheckBox = new JCheckBox("Save simulation");
            panel.add(saveBinaryCheckBox);
            c.insets = new Insets(15, 0, 0, 0);
            gridbag.setConstraints(panel, c);
            centerPanel.add(panel);
        }

        dataListPanel = new JPanel(new BorderLayout());
        c.insets = new Insets(10, 0, 0, 0);
        gridbag.setConstraints(dataListPanel, c);
        if (computing) {
            centerPanel.add(dataListPanel);
        }
    }

    public boolean doModal() {
        secPerStepTextField.setText(String.valueOf(secPerStep));
        if (computing) {
            saveIntervalTextField.setText(String.valueOf(saveInterval));
            saveBinaryCheckBox.setSelected(saveBinary);
        }
        return super.doModal();
    }
    
    protected void onBrowserHelp() {
       if (computing) {
            mp2HelpWindow.showHelpFile ("saveSimulation.html");
       } else {
            mp2HelpWindow.showHelpFile ("animationSpeed.html");
       }
    }

    protected boolean retrieveData() {
        try {
            secPerStep = Float.valueOf(secPerStepTextField.getText()).floatValue();
            if (computing) {
                saveInterval = Float.valueOf(saveIntervalTextField.getText()).floatValue();
            }
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog(parent, "Please check your input.",
                                "Input Error");
            return false;
        }
        if (!dataCheck(secPerStep, "\"Seconds per time step\"", IS_NON_NEGATIVE,
                        secPerStepTextField)) {
            return false;
        }
        if (computing) {
            if (!dataCheck(saveInterval, "\"Time interval between save & refresh\"", IS_NON_NEGATIVE,
                            saveIntervalTextField)) {
                return false;
            }
            saveBinary = saveBinaryCheckBox.isSelected();
        }
        return true;
    }
}
