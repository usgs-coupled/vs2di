/*
 * vs2FlowPanel.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2FlowPanel extends vs2ModelOptionsPanel implements vs2Constants {

    public int weightingOption;
    public int soilModel;
    public int initialFlowType;

    protected JRadioButton arithmeticMean;
    protected JRadioButton geometricMean;
    protected JRadioButton upstreamWeighting;
    protected JRadioButton vanGenuchten;
    protected JRadioButton haverkamp;
    protected JRadioButton brooksCorey;
    protected JRadioButton rossiNimmo;
    protected JRadioButton tabularData;
    protected JRadioButton pressureHeadIC;
    protected JRadioButton moistureContentIC;
    protected JRadioButton equilibriumProfileIC;
    protected JTextField minInitPressHeadTextField;

    /**
     * Constructs the panel for flow options
     */
    public vs2FlowPanel(vs2ModelOptionsDialog parentDialog) {

        super(parentDialog);

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        setBorder(new EmptyBorder(10, 10, 10, 10));
        setLayout(gridbag);

        JPanel panel, subPanel;
        ButtonGroup bg;

        // Create a panel to hold 2 sub panels
        add(panel = new JPanel(new GridLayout(1, 2, 20, 0), false));
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.HORIZONTAL;
        gridbag.setConstraints(panel, c);

        // flow initial conditions
        panel.add(subPanel = new JPanel(new GridLayout(5, 1), false));
        subPanel.setBorder(new CompoundBorder(
                BorderFactory.createEtchedBorder(),
                new EmptyBorder(10, 10, 10, 10)));
        subPanel.add(new JLabel("Specify initial hydraulic"));
        subPanel.add(new JLabel("condition as"));
        subPanel.add(equilibriumProfileIC =
                    new JRadioButton("Equilibrium profile"));
        subPanel.add(pressureHeadIC =
                    new JRadioButton("Pressure head"));
        subPanel.add(moistureContentIC =
                    new JRadioButton("Moisture content"));
        moistureContentIC.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    tabularData.setEnabled(false);
                } else {
                    tabularData.setEnabled(true);
                }
            }
        });
        bg = new ButtonGroup();
        bg.add(pressureHeadIC);
        bg.add(moistureContentIC);
        bg.add(equilibriumProfileIC);

        // intercell weighting options
        panel.add(subPanel = new JPanel(new GridLayout(5, 1), false));
        subPanel.setBorder(new CompoundBorder(
                BorderFactory.createEtchedBorder(),
                new EmptyBorder(10, 10, 10, 10)));
        subPanel.add(new JLabel("Compute intercell relative "));
        subPanel.add(new JLabel("hydraulic conductivity by"));
        subPanel.add(arithmeticMean =
                new JRadioButton("Arithmetic mean"));
        subPanel.add(geometricMean =
                new JRadioButton("Geometric mean"));
        subPanel.add(upstreamWeighting =
                new JRadioButton("Upstream weighting"));
        bg = new ButtonGroup();
        bg.add(geometricMean);
        bg.add(arithmeticMean);
        bg.add(upstreamWeighting);

        // hydraulic property model
        add(panel = new JPanel(gridbag, false));
        c.insets = new Insets(20, 0, 0, 0);
        gridbag.setConstraints(panel, c);
        panel.setBorder(new CompoundBorder(
                BorderFactory.createEtchedBorder(),
                new EmptyBorder(10, 10, 10, 10)));
        JLabel label = new JLabel("Represent hydraulic characteristic"
                    + " functions by");
        c.insets = new Insets(0, 0, 10, 0);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(label, c);
        panel.add(label);
        panel.add(subPanel = new JPanel(new GridLayout(3, 2, 10, 4), false));
        c.insets = new Insets(0, 0, 0, 0);
        gridbag.setConstraints(subPanel, c);
        subPanel.add(vanGenuchten =
                new JRadioButton("van Genuchten model"));
        subPanel.add(haverkamp =
                new JRadioButton("Haverkamp model"));
        subPanel.add(brooksCorey =
                new JRadioButton("Brooks-Corey model"));
        subPanel.add(rossiNimmo =
                new JRadioButton("Rossi-Nimmo model"));
        subPanel.add(tabularData =
                new JRadioButton("Tabular Data"));
        tabularData.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    moistureContentIC.setEnabled(false);
                } else {
                    moistureContentIC.setEnabled(true);
                }
            }
        });
        bg = new ButtonGroup();
        bg.add(vanGenuchten);
        bg.add(haverkamp);
        bg.add(brooksCorey);
        bg.add(rossiNimmo);
        bg.add(tabularData);
    }

    /**
     * Puts values in components
     */
    public void init() {
        pressureHeadIC.setSelected(initialFlowType
                                    == INITIAL_PRESSURE_HEAD);
        moistureContentIC.setSelected(initialFlowType
                                    == INITIAL_MOISTURE_CONTENT);
        equilibriumProfileIC.setSelected(initialFlowType
                                    == INITIAL_EQUILIBRIUM_PROFILE);

        arithmeticMean.setSelected(weightingOption
                                    == ARITHMETIC_MEAN_WEIGHTING);
        geometricMean.setSelected(weightingOption
                                    == GEOMETRIC_MEAN_WEIGHTING);
        upstreamWeighting.setSelected(weightingOption
                                    == UPSTREAM_WEIGHTING);

        vanGenuchten.setSelected(soilModel == VAN_GENUCHTEN);
        haverkamp.setSelected(soilModel == HAVERKAMP);
        brooksCorey.setSelected(soilModel == BROOKS_COREY);
        rossiNimmo.setSelected(soilModel == ROSSI_NIMMO);
        tabularData.setSelected(soilModel == TABULAR_DATA);
    }

    /**
     * Get values from components. This method always returns true because
     * there is no possibility for input error
     */
    public boolean retrieveData() {
        if (equilibriumProfileIC.isSelected()) {
            initialFlowType = INITIAL_EQUILIBRIUM_PROFILE;
        }
        else if (pressureHeadIC.isSelected()) {
            initialFlowType = INITIAL_PRESSURE_HEAD;
        }
        else if (moistureContentIC.isSelected()) {
            initialFlowType = INITIAL_MOISTURE_CONTENT;
        }

        if (arithmeticMean.isSelected()) {
            weightingOption = ARITHMETIC_MEAN_WEIGHTING;
        }
        else if (geometricMean.isSelected()) {
            weightingOption = GEOMETRIC_MEAN_WEIGHTING;
        }
        else if (upstreamWeighting.isSelected()) {
            weightingOption = UPSTREAM_WEIGHTING;
        }

        if (brooksCorey.isSelected()) {
            soilModel = BROOKS_COREY;
        }
        else if (vanGenuchten.isSelected()) {
            soilModel = VAN_GENUCHTEN;
        }
        else if (haverkamp.isSelected()) {
            soilModel = HAVERKAMP;
        }
        else if (rossiNimmo.isSelected()) {
            soilModel = ROSSI_NIMMO;
        }
        else if (tabularData.isSelected()) {
            soilModel = TABULAR_DATA;
        }

        return true;
    }
}

