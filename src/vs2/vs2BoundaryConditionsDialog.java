/*
 * vs2BoundaryConditionsDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2BoundaryConditionsDialog extends mp2Dialog
                        implements vs2Constants {

    public int flowBCType;
    public int transportBCType;
    public double flowBCValue;
    public double transportBCValue;
    public boolean applyToThisPeriodOnly;
    public boolean getFromFile;

    protected vs2ModelOptions modelOptions;
    protected JComboBox flowBCChooser;
    protected JComboBox concChooser;
    protected JPanel flowBCValuePanel;
    protected JPanel cards;
    protected JTextField flowBCValueTextField;
    protected JRadioButton noSoluteFluxRadioButton;
    protected JRadioButton boundaryConcRadioButton;
    protected JRadioButton diffusiveFluxRadioButton;
    protected JRadioButton inflowRadioButton;
    protected JRadioButton boundaryRadioButton;
    protected JRadioButton seepOutflowRadioButton;
    protected JRadioButton seepTemperatureRadioButton;
    protected JRadioButton gdrnOutflowRadioButton;
    protected JRadioButton gdrnTemperatureRadioButton;
    protected JRadioButton evapOutflowRadioButton;
    protected JRadioButton evapTemperatureRadioButton;
    protected JLabel blankLabel;
    protected JLabel blankSeepLabel;
    protected JLabel blankGdrnLabel;
    protected JLabel blankEvapLabel;
    protected JTextField boundaryConcTextField;
    protected JTextField diffusiveFluxTextField;
    protected JTextField concTextField;
    protected JTextField seepTemperatureTextField;
    protected JTextField gdrnTemperatureTextField;
    protected JTextField evapTemperatureTextField;
    protected JRadioButton thisPeriodOnlyRadioButton;
    protected JRadioButton thisPeriodAndFutureRadioButton;
    protected JRadioButton getFromFileRadioButton;

    protected final static String noFlow = "No flow across boundary (q = 0)";
    protected final static String pressureHead = "Specified pressure head (p)";
    protected final static String totalHead = "Specified total head (h)";
    protected final static String normalFluidFluxIn = "Specified flux into domain - normal (qn)";
    protected final static String verticalFluidFluxIn = "Specified flux into domain - vertical (qv)";
    protected final static String normalFluidFluxOut = "Specified flux out of domain - normal (-qn)";
    protected final static String verticalFluidFluxOut = "Specified flux out of domain - vertical (-qv)";
    protected final static String volumeFlowIn = "Specified volumetric flow into domain (Q)";
    protected final static String volumeFlowOut = "Specified volumetric flow out of domain (-Q)";
    protected final static String evapotrans = "Evaporation/transpiration (ET)";
    protected final static String seepageFace = "Possible seepage face (seep)";
    protected final static String gravityDrain = "Gravity drain (grv_drn)";
    protected static String noSoluteFlux = "No solute flux (J = 0)";
    protected static String inflowConc = "Specified concentration of inflow (Ci)";
    protected static String boundaryConc = "Specified concentration at boundary (Cb)";
    protected static String promptConc = "Specified concentration";
    protected final static String diffusiveFlux = "Specified diffusive flux at boundary (Jd)";
    protected final static String conductiveFlux = "Specified conductive flux at boundary (Jc)";


    public vs2BoundaryConditionsDialog(int rp, vs2ModelOptions modelOptions) {
        super("BC for Recharge Period " + rp, true, modelOptions);
        this.modelOptions = modelOptions;
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "flowBoundaryConditions", null);
    }

    protected void makeContents() {

        vs2ModelOptions modOpt = (vs2ModelOptions) customObject;
        if (vs2App.doHeat()) {
            noSoluteFlux = "No energy flux (J = 0)";
            inflowConc = "Specified temperature of inflow (Ti)";
            boundaryConc = "Specified temperature at boundary (Tb)";
            promptConc = "Specified temperature";
        }

        // Make a center panel to hold all the components.
        JPanel centerPanel = new JPanel(false);
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.insets = new Insets(10, 0, 0, 0);
        centerPanel.setLayout(gridbag);
        centerPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        // Make panel for flow BC
        JPanel flowBCPanel = new JPanel();
        flowBCPanel.setLayout(new GridLayout(2, 1, 5, 5));
        flowBCPanel.setBorder(new TitledBorder (new EtchedBorder (),
                "Flow BC"));
        gridbag.setConstraints(flowBCPanel, c);
        centerPanel.add(flowBCPanel);

        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        panel.add(new JLabel("Type: "));
        panel.add(flowBCChooser = new JComboBox());
        flowBCChooser.addItem(noFlow);
        flowBCChooser.addItem(pressureHead);
        flowBCChooser.addItem(totalHead);
        flowBCChooser.addItem(normalFluidFluxIn);
        flowBCChooser.addItem(verticalFluidFluxIn);
        flowBCChooser.addItem(normalFluidFluxOut);
        //vertical flux out not implemented
        //flowBCChooser.addItem(verticalFluidFluxOut);
        if (modOpt.useRadialCoord) {
            flowBCChooser.addItem(volumeFlowIn);
            flowBCChooser.addItem(volumeFlowOut);
        }
        if (modOpt.doEvaporation || modOpt.doTranspiration) {
            flowBCChooser.addItem(evapotrans);
        }
        flowBCChooser.addItem(seepageFace);
        flowBCChooser.addItem(gravityDrain);
        flowBCPanel.add(panel);

        flowBCChooser.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onSelectedFlowBC();
                }
            }
        });

        flowBCValuePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        flowBCValuePanel.add(new JLabel("Value: "));
        flowBCValuePanel.add(flowBCValueTextField = new JTextField(8));
        flowBCPanel.add(flowBCValuePanel);
        flowBCValuePanel.setVisible(false);

        // Make panel for transport BC
        if (modOpt.doTransport) {

            // Use a card layout
            cards = new JPanel(new CardLayout());
            cards.setBorder(new TitledBorder (new EtchedBorder (),
                    "Transport BC"));
            gridbag.setConstraints(cards, c);
            centerPanel.add(cards);

            // Make the panel for no flow bc (card 1)
            JPanel p1 = new JPanel(gridbag);

            JPanel leftPanel = new JPanel(new GridLayout(3, 1));
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.insets = new Insets(0, 5, 5, 0);
            c.fill = GridBagConstraints.VERTICAL;
            gridbag.setConstraints(leftPanel, c);
            p1.add(leftPanel);
            JPanel rightPanel = new JPanel(new GridLayout(3, 1));
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(0, 10, 5, 5);
            gridbag.setConstraints(rightPanel, c);
            p1.add(rightPanel);

            leftPanel.add(noSoluteFluxRadioButton = new JRadioButton(noSoluteFlux, true));
            leftPanel.add(boundaryConcRadioButton = new JRadioButton(boundaryConc, false));
            if (vs2App.doHeat()) {
                leftPanel.add(diffusiveFluxRadioButton = new JRadioButton(conductiveFlux, false));
            } else {
                leftPanel.add(diffusiveFluxRadioButton = new JRadioButton(diffusiveFlux, false));
            }
            rightPanel.add(blankLabel = new JLabel("      "));
            rightPanel.add(boundaryConcTextField = new JTextField(5));
            rightPanel.add(diffusiveFluxTextField = new JTextField(5));
            boundaryConcTextField.setVisible(false);
            diffusiveFluxTextField.setVisible(false);


            noSoluteFluxRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankLabel.setVisible(true);
                        boundaryConcTextField.setVisible(false);
                        diffusiveFluxTextField.setVisible(false);
                        if (getFromFileRadioButton.isSelected()) {
                            getFromFileRadioButton.setSelected(false);
                            thisPeriodOnlyRadioButton.setSelected(true);
                        }
                        getFromFileRadioButton.setEnabled(false);
                    }
                }
            });
            boundaryConcRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankLabel.setVisible(false);
                        boundaryConcTextField.setVisible(true);
                        diffusiveFluxTextField.setVisible(false);
                        getFromFileRadioButton.setEnabled(true);
                    }
                }
            });
            diffusiveFluxRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankLabel.setVisible(false);
                        boundaryConcTextField.setVisible(false);
                        diffusiveFluxTextField.setVisible(true);
                        getFromFileRadioButton.setEnabled(true);
                    }
                }
            });
            ButtonGroup bg = new ButtonGroup();
            bg.add(noSoluteFluxRadioButton);
            bg.add(boundaryConcRadioButton);
            bg.add(diffusiveFluxRadioButton);

            // Make a panel for specified head and flux bc
            JPanel p2 = new JPanel(new FlowLayout(FlowLayout.LEFT));
            p2.add(new JLabel(promptConc));
            p2.add(concChooser = new JComboBox());
            concChooser.addItem("of inflow");
            concChooser.addItem("at boundary");
            p2.add(new JLabel(" = "));
            p2.add(concTextField = new JTextField(5));

            // Make a panel for default
            JPanel p3 = new JPanel(new FlowLayout(FlowLayout.LEFT));
            p3.add(new JRadioButton("Default outflow (sole option)", true));

            // For energy transport, make a panel for seepage face that
            // allows user to specify temperature at seepage face
            JPanel p4 = new JPanel(gridbag);

            leftPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.insets = new Insets(0, 5, 5, 0);
            c.fill = GridBagConstraints.VERTICAL;
            gridbag.setConstraints(leftPanel, c);
            p4.add(leftPanel);
            rightPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(0, 10, 5, 5);
            gridbag.setConstraints(rightPanel, c);
            p4.add(rightPanel);

            leftPanel.add(seepOutflowRadioButton = new JRadioButton("Default outflow", true));
            leftPanel.add(seepTemperatureRadioButton = new JRadioButton("Temperature", false));
            rightPanel.add(blankSeepLabel = new JLabel("      "));
            rightPanel.add(seepTemperatureTextField = new JTextField(5));
            seepTemperatureTextField.setVisible(false);

            seepOutflowRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankSeepLabel.setVisible(true);
                        seepTemperatureTextField.setVisible(false);
                        if (getFromFileRadioButton.isSelected()) {
                            getFromFileRadioButton.setSelected(false);
                            thisPeriodOnlyRadioButton.setSelected(true);
                        }
                        getFromFileRadioButton.setEnabled(false);
                    }
                }
            });
            seepTemperatureRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankSeepLabel.setVisible(false);
                        seepTemperatureTextField.setVisible(true);
                        getFromFileRadioButton.setEnabled(true);
                    }
                }
            });
            bg = new ButtonGroup();
            bg.add(seepOutflowRadioButton);
            bg.add(seepTemperatureRadioButton);


            // For energy transport, make a panel for gravity drain that
            // allows user to specify temperature at gravity drain
            JPanel p4a = new JPanel(gridbag);

            leftPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.insets = new Insets(0, 5, 5, 0);
            c.fill = GridBagConstraints.VERTICAL;
            gridbag.setConstraints(leftPanel, c);
            p4a.add(leftPanel);
            rightPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(0, 10, 5, 5);
            gridbag.setConstraints(rightPanel, c);
            p4a.add(rightPanel);

            leftPanel.add(gdrnOutflowRadioButton = new JRadioButton("Default outflow", true));
            leftPanel.add(gdrnTemperatureRadioButton = new JRadioButton("Temperature", false));
            rightPanel.add(blankGdrnLabel = new JLabel("      "));
            rightPanel.add(gdrnTemperatureTextField = new JTextField(5));
            gdrnTemperatureTextField.setVisible(false);

            gdrnOutflowRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankGdrnLabel.setVisible(true);
                        gdrnTemperatureTextField.setVisible(false);
                        if (getFromFileRadioButton.isSelected()) {
                            getFromFileRadioButton.setSelected(false);
                            thisPeriodOnlyRadioButton.setSelected(true);
                        }
                        getFromFileRadioButton.setEnabled(false);
                    }
                }
            });
            gdrnTemperatureRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankGdrnLabel.setVisible(false);
                        gdrnTemperatureTextField.setVisible(true);
                        getFromFileRadioButton.setEnabled(true);
                    }
                }
            });
            bg = new ButtonGroup();
            bg.add(gdrnOutflowRadioButton);
            bg.add(gdrnTemperatureRadioButton);

            // For energy transport, make a panel for evapotranspiration boundary that
            // allows user to specify temperature at evapotranspiration boundary
            JPanel p5 = new JPanel(gridbag);

            leftPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.insets = new Insets(0, 5, 5, 0);
            c.fill = GridBagConstraints.VERTICAL;
            gridbag.setConstraints(leftPanel, c);
            p5.add(leftPanel);
            rightPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(0, 10, 5, 5);
            gridbag.setConstraints(rightPanel, c);
            p5.add(rightPanel);

            leftPanel.add(evapOutflowRadioButton = new JRadioButton("Default outflow", true));
            leftPanel.add(evapTemperatureRadioButton = new JRadioButton("Temperature", false));
            rightPanel.add(blankEvapLabel = new JLabel("      "));
            rightPanel.add(evapTemperatureTextField = new JTextField(5));
            evapTemperatureTextField.setVisible(false);

            evapOutflowRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankEvapLabel.setVisible(true);
                        evapTemperatureTextField.setVisible(false);
                        if (getFromFileRadioButton.isSelected()) {
                            getFromFileRadioButton.setSelected(false);
                            thisPeriodOnlyRadioButton.setSelected(true);
                        }
                        getFromFileRadioButton.setEnabled(false);
                    }
                }
            });
            evapTemperatureRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankEvapLabel.setVisible(false);
                        evapTemperatureTextField.setVisible(true);
                        getFromFileRadioButton.setEnabled(true);
                    }
                }
            });
            bg = new ButtonGroup();
            bg.add(evapOutflowRadioButton);
            bg.add(evapTemperatureRadioButton);


            cards.add("Card 1", p1);
            cards.add("Card 2", p2);
            cards.add("Card 3", p3);
            cards.add("Card 4", p4);
            cards.add("Card 4a", p4a);
            cards.add("Card 5", p5);

            CardLayout cl = (CardLayout)(cards.getLayout());
            cl.show(cards, "Card 1");

        }

        // Make the "Assignment Options" panel
        JPanel applyToPanel = new JPanel();
        applyToPanel.setLayout(new GridLayout(3, 1));
        applyToPanel.setBorder(new TitledBorder(new EtchedBorder(),
                "Assignment Option"));
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.insets = new Insets(10, 0, 0, 0);
        gridbag.setConstraints(applyToPanel, c);
        centerPanel.add(applyToPanel);

        panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        panel.add(thisPeriodOnlyRadioButton = new JRadioButton(
                                "Apply to this recharge period only", true));
        applyToPanel.add(panel);

        panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        panel.add(thisPeriodAndFutureRadioButton = new JRadioButton(
                        "Apply to this and all future recharge periods", false));
        applyToPanel.add(panel);

        panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        panel.add(getFromFileRadioButton = new JRadioButton(
                        "Import BC data starting this recharge period", false));
        applyToPanel.add(panel);

        ButtonGroup bg = new ButtonGroup();
        bg.add(thisPeriodOnlyRadioButton);
        bg.add(thisPeriodAndFutureRadioButton);
        bg.add(getFromFileRadioButton);

        thisPeriodOnlyRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    flowBCValueTextField.setEnabled(true);
                    if (modelOptions.doTransport) {
                        boundaryConcTextField.setEnabled(true);
                        diffusiveFluxTextField.setEnabled(true);
                        concTextField.setEnabled(true);
                        seepTemperatureTextField.setEnabled(true);
                        gdrnTemperatureTextField.setEnabled(true);
                        evapTemperatureTextField.setEnabled(true);
                    }
                }
            }
        });

        thisPeriodAndFutureRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    flowBCValueTextField.setEnabled(true);
                    if (modelOptions.doTransport) {
                        boundaryConcTextField.setEnabled(true);
                        diffusiveFluxTextField.setEnabled(true);
                        concTextField.setEnabled(true);
                        seepTemperatureTextField.setEnabled(true);
                        gdrnTemperatureTextField.setEnabled(true);
                        evapTemperatureTextField.setEnabled(true);
                    }
                }
            }
        });

        getFromFileRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    flowBCValueTextField.setEnabled(false);
                    if (modelOptions.doTransport) {
                        boundaryConcTextField.setEnabled(false);
                        diffusiveFluxTextField.setEnabled(false);
                        concTextField.setEnabled(false);
                        seepTemperatureTextField.setEnabled(false);
                        gdrnTemperatureTextField.setEnabled(false);
                        evapTemperatureTextField.setEnabled(false);
                    }
                }
            }
        });
    }

    public boolean doModal() {
        if (flowBCType == NO_FLOW_BC) {
            flowBCChooser.setSelectedItem(noFlow);
        }
        else if (flowBCType == PRESSURE_HEAD_BC) {
            flowBCChooser.setSelectedItem(pressureHead);
            if (flowBCValue != Double.MIN_VALUE) {
                flowBCValueTextField.setText(String.valueOf(flowBCValue));
            }
        }
        else if (flowBCType == NORMAL_FLUID_FLUX_BC) {
            if (flowBCValue >= 0) {
                flowBCChooser.setSelectedItem(normalFluidFluxIn);
                if (flowBCValue != Double.MIN_VALUE) {
                    flowBCValueTextField.setText(String.valueOf(flowBCValue));
                }
            } else {
                flowBCChooser.setSelectedItem(normalFluidFluxOut);
                if (flowBCValue != Double.MIN_VALUE) {
                    flowBCValueTextField.setText(String.valueOf(-flowBCValue));
                }
            }
        }
        else if (flowBCType == VERTICAL_FLUID_FLUX_BC) {
            if (flowBCValue >= 0) {
                flowBCChooser.setSelectedItem(verticalFluidFluxIn);
                if (flowBCValue != Double.MIN_VALUE) {
                    flowBCValueTextField.setText(String.valueOf(flowBCValue));
                }
            } else {
                flowBCChooser.setSelectedItem(verticalFluidFluxOut);
                if (flowBCValue != Double.MIN_VALUE) {
                    flowBCValueTextField.setText(String.valueOf(-flowBCValue));
                }
            }
        }
        else if (flowBCType == SEEPAGE_FACE_BC) {
            flowBCChooser.setSelectedItem(seepageFace);
        }
        else if (flowBCType == GRAVITY_DRAIN_BC) {
            flowBCChooser.setSelectedItem(gravityDrain);
        }
        else if (flowBCType == TOTAL_HEAD_BC) {
            flowBCChooser.setSelectedItem(totalHead);
            if (flowBCValue != Double.MIN_VALUE) {
                flowBCValueTextField.setText(String.valueOf(flowBCValue));
            }
        }
        else if (flowBCType == EVAPORATION_BC) {
            flowBCChooser.setSelectedItem(evapotrans);
        }
        else if (flowBCType == VOLUMETRIC_FLOW_BC) {
            if (flowBCValue >= 0) {
                flowBCChooser.setSelectedItem(volumeFlowIn);
                if (flowBCValue != Double.MIN_VALUE) {
                    flowBCValueTextField.setText(String.valueOf(flowBCValue));
                }
            } else {
                flowBCChooser.setSelectedItem(volumeFlowOut);
                if (flowBCValue != Double.MIN_VALUE) {
                    flowBCValueTextField.setText(String.valueOf(-flowBCValue));
                }
            }
        }

        if (modelOptions.doTransport) {
            if (flowBCType == NO_FLOW_BC) {
                if (transportBCType == SPECIFIED_CONC_BC) {
                    boundaryConcRadioButton.setSelected(true);
                    if (transportBCValue != Double.MIN_VALUE) {
                        boundaryConcTextField.setText(String.valueOf(transportBCValue));
                    }
                }
                else if (transportBCType == DIFFUSIVE_FLUX_BC) {
                    diffusiveFluxRadioButton.setSelected(true);
                    if (transportBCValue != Double.MIN_VALUE) {
                        diffusiveFluxTextField.setText(String.valueOf(transportBCValue));
                    }
                }
            }
            else if (flowBCType == PRESSURE_HEAD_BC
                    || flowBCType == TOTAL_HEAD_BC
                    || (flowBCType == NORMAL_FLUID_FLUX_BC && flowBCValue > 0)
                    || (flowBCType == VERTICAL_FLUID_FLUX_BC && flowBCValue > 0)
                    || (flowBCType == VOLUMETRIC_FLOW_BC && flowBCValue > 0)) {
                if (transportBCValue != Double.MIN_VALUE) {
                    concTextField.setText(String.valueOf(transportBCValue));
                }
                if (transportBCType == DEFAULT_CONC_BC) {
                    concChooser.setSelectedIndex(0);
                }
                else if (transportBCType == SPECIFIED_CONC_BC) {
                    concChooser.setSelectedIndex(1);
                }
            }
            else if (flowBCType == EVAPORATION_BC && vs2App.doHeat()) {
                if (transportBCType == SPECIFIED_CONC_BC) {
                    evapTemperatureRadioButton.setSelected(true);
                    if (transportBCValue != Double.MIN_VALUE) {
                        evapTemperatureTextField.setText(String.valueOf(transportBCValue));
                    }
                } else {
                    evapOutflowRadioButton.setSelected(true);
                }
            }
            else if (flowBCType == SEEPAGE_FACE_BC && vs2App.doHeat()) {
                if (transportBCType == SPECIFIED_CONC_BC) {
                    seepTemperatureRadioButton.setSelected(true);
                    if (transportBCValue != Double.MIN_VALUE) {
                        seepTemperatureTextField.setText(String.valueOf(transportBCValue));
                    }
                } else {
                    seepOutflowRadioButton.setSelected(true);
                }
            }
            else if (flowBCType == GRAVITY_DRAIN_BC && vs2App.doHeat()) {
                if (transportBCType == SPECIFIED_CONC_BC) {
                    gdrnTemperatureRadioButton.setSelected(true);
                    if (transportBCValue != Double.MIN_VALUE) {
                        gdrnTemperatureTextField.setText(String.valueOf(transportBCValue));
                    }
                } else {
                    gdrnOutflowRadioButton.setSelected(true);
                }
            }
        }

        if ((flowBCType == NO_FLOW_BC && !modelOptions.doTransport)
            || (flowBCType == NO_FLOW_BC && modelOptions.doTransport && transportBCType == DEFAULT_CONC_BC)
            || flowBCType == EVAPORATION_BC || flowBCType == GRAVITY_DRAIN_BC
            || flowBCType == SEEPAGE_FACE_BC) {
            getFromFileRadioButton.setEnabled(false);
        }

        return super.doModal();
    }

    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile("flowBoundaryConditions.html");
    }

    protected void onSelectedFlowBC() {
        String flowBC = (String) flowBCChooser.getSelectedItem();

        if (flowBC.equalsIgnoreCase(noFlow)) {
            flowBCValuePanel.setVisible(false);
            if (modelOptions.doTransport) {
                CardLayout cl = (CardLayout)(cards.getLayout());
                cl.show(cards, "Card 1");
            }

        }
        else if (flowBC.equalsIgnoreCase(pressureHead)
                || flowBC.equalsIgnoreCase(totalHead)
                || flowBC.equalsIgnoreCase(normalFluidFluxIn)
                || flowBC.equalsIgnoreCase(verticalFluidFluxIn)
                || flowBC.equalsIgnoreCase(volumeFlowIn)) {
            flowBCValuePanel.setVisible(true);
            if (modelOptions.doTransport) {
                CardLayout cl = (CardLayout)(cards.getLayout());
                cl.show(cards, "Card 2");
            }
        }
        else if (flowBC.equalsIgnoreCase(normalFluidFluxOut)
                || flowBC.equalsIgnoreCase(verticalFluidFluxOut)
                || flowBC.equalsIgnoreCase(volumeFlowOut)) {
            flowBCValuePanel.setVisible(true);
            if (modelOptions.doTransport) {
                CardLayout cl = (CardLayout)(cards.getLayout());
                cl.show(cards, "Card 3");
            }
        }
        else if (flowBC.equalsIgnoreCase(seepageFace)) {
            flowBCValuePanel.setVisible(false);
            if (modelOptions.doTransport) {
                CardLayout cl = (CardLayout)(cards.getLayout());
                if (modelOptions.doTransport && vs2App.doHeat()) {
                    cl.show(cards, "Card 4");
                } else {
                    cl.show(cards, "Card 3");
                }
            }
        }
        else if (flowBC.equalsIgnoreCase(gravityDrain)) {
            flowBCValuePanel.setVisible(false);
            if (modelOptions.doTransport) {
                CardLayout cl = (CardLayout)(cards.getLayout());
                if (modelOptions.doTransport && vs2App.doHeat()) {
                    cl.show(cards, "Card 4a");
                } else {
                    cl.show(cards, "Card 3");
                }
            }
        }
        else if (flowBC.equalsIgnoreCase(evapotrans)) {
            flowBCValuePanel.setVisible(false);
            if (modelOptions.doTransport) {
                CardLayout cl = (CardLayout)(cards.getLayout());
                if (modelOptions.doTransport && vs2App.doHeat()) {
                    cl.show(cards, "Card 5");
                } else {
                    cl.show(cards, "Card 3");
                }
            }
        }
        if ((flowBC.equalsIgnoreCase(noFlow) && !modelOptions.doTransport)
            || (flowBC.equalsIgnoreCase(noFlow) && modelOptions.doTransport && noSoluteFluxRadioButton.isSelected())
            || flowBC.equalsIgnoreCase(evapotrans) || flowBC.equalsIgnoreCase(gravityDrain)
            || flowBC.equalsIgnoreCase(seepageFace)) {
            if (getFromFileRadioButton.isSelected()) {
                getFromFileRadioButton.setSelected(false);
                thisPeriodOnlyRadioButton.setSelected(true);
            }
            getFromFileRadioButton.setEnabled(false);
        } else {
            getFromFileRadioButton.setEnabled(true);
        }
        repaint();
    }

    protected boolean retrieveData() {

        flowBCType = NO_FLOW_BC;
        flowBCValue = 0;
        transportBCType = DEFAULT_CONC_BC;
        transportBCValue = 0;

        double vf, v1, v2;
        boolean bf, b1, b2;

        String quantity = vs2App.doHeat() ? "the temperature" : "the concentration";

        getFromFile = getFromFileRadioButton.isSelected();

        applyToThisPeriodOnly = thisPeriodOnlyRadioButton.isSelected();

        String flowBC = (String) flowBCChooser.getSelectedItem();

        if (flowBC.equalsIgnoreCase(evapotrans)) {
            flowBCType = EVAPORATION_BC;
            if (!modelOptions.doTransport || !vs2App.doHeat() || evapOutflowRadioButton.isSelected()){
                return true;
            }
        }
        else if (flowBC.equalsIgnoreCase(seepageFace)) {
            flowBCType = SEEPAGE_FACE_BC;
            if (!modelOptions.doTransport || !vs2App.doHeat() || seepOutflowRadioButton.isSelected()){
                return true;
            }
        }
        else if (flowBC.equalsIgnoreCase(gravityDrain)) {
            flowBCType = GRAVITY_DRAIN_BC;
            if (!modelOptions.doTransport || !vs2App.doHeat() || gdrnOutflowRadioButton.isSelected()){
                return true;
            }
        }
        else if (flowBC.equalsIgnoreCase(noFlow)) {
            flowBCType = NO_FLOW_BC;
        }
        else {
            if (getFromFile) {
                flowBCValue = 1;  // used as a flag
            } else {
                try {
                    flowBCValue = Double.valueOf(
                            flowBCValueTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog(
                            "Please check your input.", "Input Error");
                    return false;
                }
            }
            if (flowBC.equalsIgnoreCase(pressureHead)) {
                flowBCType = PRESSURE_HEAD_BC;
            }
            else if (flowBC.equalsIgnoreCase(totalHead)) {
                flowBCType = TOTAL_HEAD_BC;
            }
            else {
                if (!getFromFile && !dataCheck(flowBCValue, "the flow BC value",
                            IS_POSITIVE, flowBCValueTextField)) {
                    return false;
                }
                if (flowBC.equalsIgnoreCase(normalFluidFluxIn)) {
                    flowBCType = NORMAL_FLUID_FLUX_BC;
                }
                else if (flowBC.equalsIgnoreCase(verticalFluidFluxIn)) {
                    flowBCType = VERTICAL_FLUID_FLUX_BC;
                }
                else if (flowBC.equalsIgnoreCase(normalFluidFluxOut)) {
                    flowBCType = NORMAL_FLUID_FLUX_BC;
                    flowBCValue = -flowBCValue;
                    return true;
                }
                else if (flowBC.equalsIgnoreCase(verticalFluidFluxOut)) {
                    flowBCType = VERTICAL_FLUID_FLUX_BC;
                    flowBCValue = -flowBCValue;
                    return true;
                }
                else if (flowBC.equalsIgnoreCase(volumeFlowIn)) {
                    flowBCType = VOLUMETRIC_FLOW_BC;
                }
                else if (flowBC.equalsIgnoreCase(volumeFlowOut)) {
                    flowBCType = VOLUMETRIC_FLOW_BC;
                    flowBCValue = -flowBCValue;
                    return true;
                }
            }
        }

        if (!modelOptions.doTransport) {
            return true;
        }

        if (flowBCType == NO_FLOW_BC) {
            if (noSoluteFluxRadioButton.isSelected()) {
                transportBCType = DEFAULT_CONC_BC;
                transportBCValue = 0;
                return true;
            }
            else if (boundaryConcRadioButton.isSelected()) {
                if (!getFromFile) {
                    try {
                        transportBCValue = Double.valueOf(
                                boundaryConcTextField.getText()).doubleValue();
                    } catch (NumberFormatException e) {
                        mp2MessageBox.showMessageDialog(
                                "Please check your input.", "Input Error");
                        return false;
                    }
                    if (!dataCheck(transportBCValue, quantity,
                                IS_NON_NEGATIVE, boundaryConcTextField)) {
                        return false;
                    }
                }
                transportBCType = SPECIFIED_CONC_BC;
                return true;
            } else {
                if (!getFromFile) {
                    try {
                        transportBCValue = Double.valueOf(
                                diffusiveFluxTextField.getText()).doubleValue();
                    } catch (NumberFormatException e) {
                        mp2MessageBox.showMessageDialog(
                                "Please check your input.", "Input Error");
                        return false;
                    }
                    if (!vs2App.doHeat() && !dataCheck(transportBCValue, "the diffusive flux",
                                IS_NON_NEGATIVE, diffusiveFluxTextField)) {
                        return false;
                    }
                }
                transportBCType = DIFFUSIVE_FLUX_BC;
                return true;
            }
        }

        else if (vs2App.doHeat() && flowBCType == EVAPORATION_BC) {
            if (!getFromFile) {
                try {
                    transportBCValue = Double.valueOf(
                            evapTemperatureTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog(
                            "Please check your input.", "Input Error");
                    return false;
                }

                if (!dataCheck(transportBCValue, quantity,
                            IS_NON_NEGATIVE, evapTemperatureTextField)) {
                    return false;
                }
            }
            transportBCType = SPECIFIED_CONC_BC;
            return true;
        }

        else if (vs2App.doHeat() && flowBCType == SEEPAGE_FACE_BC) {
            if (!getFromFile) {
                try {
                    transportBCValue = Double.valueOf(
                            seepTemperatureTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog(
                            "Please check your input.", "Input Error");
                    return false;
                }

                if (!dataCheck(transportBCValue, quantity,
                            IS_NON_NEGATIVE, seepTemperatureTextField)) {
                    return false;
                }
            }
            transportBCType = SPECIFIED_CONC_BC;
            return true;
        }

        else if (vs2App.doHeat() && flowBCType == GRAVITY_DRAIN_BC) {
            if (!getFromFile) {
                try {
                    transportBCValue = Double.valueOf(
                            gdrnTemperatureTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog(
                            "Please check your input.", "Input Error");
                    return false;
                }

                if (!dataCheck(transportBCValue, quantity,
                            IS_NON_NEGATIVE, gdrnTemperatureTextField)) {
                    return false;
                }
            }
            transportBCType = SPECIFIED_CONC_BC;
            return true;
        }

        else {
            if (!getFromFile) {
                try {
                    transportBCValue = Double.valueOf(
                            concTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog(
                            "Please check your input.", "Input Error");
                    return false;
                }

                if (!dataCheck(transportBCValue, quantity,
                            IS_NON_NEGATIVE, concTextField)) {
                    return false;
                }
            }
            if (concChooser.getSelectedIndex() == 1) {
                transportBCType = SPECIFIED_CONC_BC;
            } else {
                transportBCType = DEFAULT_CONC_BC;
            }
            return true;
        }

    }
}

