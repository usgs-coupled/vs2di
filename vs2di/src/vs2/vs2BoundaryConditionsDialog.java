/*
 * vs2BoundaryConditionsDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2BoundaryConditionsDialog extends vs2Dialog
                        implements vs2Constants {

    public int flowBCType;
    public int energyTransportBCType;
    public int soluteTransportBCType;
    public double flowBCValue;
    public double energyTransportBCValue;
    public int soluteTransportBCValue;
    public boolean applyToThisPeriodOnly;
    public boolean getFromFile;
    
    protected vs2ModelOptions modelOptions;
    protected JComboBox flowBCChooser;
    protected JComboBox tempChooser;
    protected JComboBox concChooser;
    protected JPanel flowBCValuePanel;
    protected JPanel energyCards;
    protected JPanel soluteCards;
    protected JTextField flowBCValueTextField;
    protected JRadioButton noEnergyFluxRadioButton;           // new for Version 1.4
    protected JRadioButton noSoluteFluxRadioButton;
    protected JRadioButton boundaryTempRadioButton;
    protected JRadioButton boundaryConcRadioButton;
    protected JRadioButton diffEnergyFluxRadioButton;          // energy (heat) only
    
    protected JRadioButton seepOutflowRadioButton;
    protected JRadioButton seepTemperatureRadioButton;
    protected JRadioButton gdrnOutflowRadioButton;
    protected JRadioButton gdrnTemperatureRadioButton;
    protected JRadioButton evapOutflowRadioButton;
    protected JRadioButton evapTemperatureRadioButton;
    protected JLabel blankEnergyLabel;
    protected JLabel blankSoluteLabel;
    protected JLabel blankSeepLabel;
    protected JLabel blankGdrnLabel;
    protected JLabel blankEvapLabel;
    protected JTextField boundaryTempTextField;
    protected JTextField boundaryConcTextField;
    protected JTextField diffEnergyFluxTextField;
    protected JTextField tempTextField;
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
    protected static String noEnergyFlux = "No energy flux (J = 0)";
    protected static String noSoluteFlux = "No solute flux (J = 0)";
    protected static String boundaryTemp = "Specified temperature at boundary (Tb)";
    protected static String boundaryConc = "Specified solution at boundary (Sb)";
    protected static String promptTemp = "Specified temperature";
    protected static String promptConc = "Specified solution";
    protected final static String conductiveFlux = "Specified conductive flux at boundary (Jc)";


    public vs2BoundaryConditionsDialog(int rp, vs2ModelOptions modelOptions) {
        super("BC for Recharge Period " + rp, true, modelOptions);
        this.modelOptions = modelOptions;
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "flowBoundaryConditions", null);
    }

    @Override
    protected void makeContents() {

        vs2ModelOptions modOpt = (vs2ModelOptions) customObject;

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
            @Override
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

        // Make panel for energy transport BC
        if (modOpt.doEnergyTransport) {

            // Use a card layout
            energyCards = new JPanel(new CardLayout());
            energyCards.setBorder(new TitledBorder (new EtchedBorder (),
                    "Heat Transport BC"));
            c.fill = GridBagConstraints.HORIZONTAL;
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(10, 0, 0, 0);            
            gridbag.setConstraints(energyCards, c);
            centerPanel.add(energyCards);

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

            leftPanel.add(noEnergyFluxRadioButton = new JRadioButton(noEnergyFlux, true));
            leftPanel.add(boundaryTempRadioButton = new JRadioButton(boundaryTemp, false));
            leftPanel.add(diffEnergyFluxRadioButton = new JRadioButton(conductiveFlux, false));
            rightPanel.add(blankEnergyLabel = new JLabel("      "));
            rightPanel.add(boundaryTempTextField = new JTextField(5));
            rightPanel.add(diffEnergyFluxTextField = new JTextField(5));
            boundaryTempTextField.setVisible(false);
            diffEnergyFluxTextField.setVisible(false);


            noEnergyFluxRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankEnergyLabel.setVisible(true);
                        boundaryTempTextField.setVisible(false);
                        diffEnergyFluxTextField.setVisible(false);
                        if (getFromFileRadioButton.isSelected()) {
                            getFromFileRadioButton.setSelected(false);
                            thisPeriodOnlyRadioButton.setSelected(true);
                        }
                        getFromFileRadioButton.setEnabled(false);
                    }
                }
            });
            boundaryTempRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankEnergyLabel.setVisible(false);
                        boundaryTempTextField.setVisible(true);
                        diffEnergyFluxTextField.setVisible(false);
                        getFromFileRadioButton.setEnabled(true);
                    }
                }
            });
            diffEnergyFluxRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankEnergyLabel.setVisible(false);
                        boundaryTempTextField.setVisible(false);
                        diffEnergyFluxTextField.setVisible(true);
                        getFromFileRadioButton.setEnabled(true);
                    }
                }
            });
            ButtonGroup bg = new ButtonGroup();
            bg.add(noEnergyFluxRadioButton);
            bg.add(boundaryTempRadioButton);
            bg.add(diffEnergyFluxRadioButton);

            // Make a panel for specified head and flux bc
            JPanel p2 = new JPanel(new FlowLayout(FlowLayout.LEFT));
            p2.add(new JLabel(promptTemp));
            p2.add(tempChooser = new JComboBox());
            tempChooser.addItem("of inflow");
            tempChooser.addItem("at boundary");
            p2.add(new JLabel(" = "));
            p2.add(tempTextField = new JTextField(5));

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


            energyCards.add("Card 1", p1);
            energyCards.add("Card 2", p2);
            energyCards.add("Card 3", p3);
            energyCards.add("Card 4", p4);
            energyCards.add("Card 4a", p4a);
            energyCards.add("Card 5", p5);

            CardLayout cl = (CardLayout)(energyCards.getLayout());
            cl.show(energyCards, "Card 1");

        }

        // Make panel for solute transport BC
        if (modOpt.doSoluteTransport) {

            // Use a card layout
            soluteCards = new JPanel(new CardLayout());
            soluteCards.setBorder(new TitledBorder (new EtchedBorder (),
                    "Solute Transport BC"));
            c.fill = GridBagConstraints.HORIZONTAL;
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(10, 0, 0, 0);
            gridbag.setConstraints(soluteCards, c);
            centerPanel.add(soluteCards);

            // Make the panel for no flow bc (card 1)
            JPanel p1 = new JPanel(gridbag);

            ///JPanel leftPanel = new JPanel(new GridLayout(3, 1));
            JPanel leftPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.insets = new Insets(0, 5, 5, 0);
            c.fill = GridBagConstraints.VERTICAL;
            gridbag.setConstraints(leftPanel, c);
            p1.add(leftPanel);
            ///JPanel rightPanel = new JPanel(new GridLayout(3, 1));
            JPanel rightPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(0, 10, 5, 5);
            gridbag.setConstraints(rightPanel, c);
            p1.add(rightPanel);

            leftPanel.add(noSoluteFluxRadioButton = new JRadioButton(noSoluteFlux, true));
            leftPanel.add(boundaryConcRadioButton = new JRadioButton(boundaryConc, false));
            rightPanel.add(blankSoluteLabel = new JLabel("      "));
            rightPanel.add(boundaryConcTextField = new JTextField(5));
            boundaryConcTextField.setVisible(false);


            noSoluteFluxRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankSoluteLabel.setVisible(true);
                        boundaryConcTextField.setVisible(false);
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
                        blankSoluteLabel.setVisible(false);
                        boundaryConcTextField.setVisible(true);
                        getFromFileRadioButton.setEnabled(true);
                    }
                }
            });
            ButtonGroup bg = new ButtonGroup();
            bg.add(noSoluteFluxRadioButton);
            bg.add(boundaryConcRadioButton);

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
            rightPanel.add(blankSeepLabel = new JLabel("      "));
            seepOutflowRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        blankSeepLabel.setVisible(true);
                        if (getFromFileRadioButton.isSelected()) {
                            getFromFileRadioButton.setSelected(false);
                            thisPeriodOnlyRadioButton.setSelected(true);
                        }
                        getFromFileRadioButton.setEnabled(false);
                    }
                }
            });
            bg = new ButtonGroup();
            bg.add(seepOutflowRadioButton);


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


            soluteCards.add("Card 1", p1);
            soluteCards.add("Card 2", p2);
            soluteCards.add("Card 3", p3);
            soluteCards.add("Card 4", p4);
            soluteCards.add("Card 4a", p4a);
            soluteCards.add("Card 5", p5);

            CardLayout cl = (CardLayout)(soluteCards.getLayout());
            cl.show(soluteCards, "Card 1");

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
                    if (modelOptions.doEnergyTransport) {
                        boundaryTempTextField.setEnabled(true);
                        diffEnergyFluxTextField.setEnabled(true);                        
                        tempTextField.setEnabled(true);
                        seepTemperatureTextField.setEnabled(true);
                        gdrnTemperatureTextField.setEnabled(true);
                        evapTemperatureTextField.setEnabled(true);
                    }
                    if (modelOptions.doSoluteTransport) {
                        boundaryConcTextField.setEnabled(true);
                        concTextField.setEnabled(true);
                    }
                }
            }
        });

        thisPeriodAndFutureRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    flowBCValueTextField.setEnabled(true);
                    if (modelOptions.doEnergyTransport) {
                        boundaryTempTextField.setEnabled(true);
                        diffEnergyFluxTextField.setEnabled(true);
                        tempTextField.setEnabled(true);
                        seepTemperatureTextField.setEnabled(true);
                        gdrnTemperatureTextField.setEnabled(true);
                        evapTemperatureTextField.setEnabled(true);
                    }
                    if (modelOptions.doSoluteTransport) {
                        boundaryConcTextField.setEnabled(true);
                        concTextField.setEnabled(true);
                    }
                }
            }
        });

        getFromFileRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    flowBCValueTextField.setEnabled(false);
                    if (modelOptions.doEnergyTransport) {
                        boundaryTempTextField.setEnabled(false);
                        diffEnergyFluxTextField.setEnabled(false);                        
                        tempTextField.setEnabled(false);
                        seepTemperatureTextField.setEnabled(false);
                        gdrnTemperatureTextField.setEnabled(false);
                        evapTemperatureTextField.setEnabled(false);
                    }
                    if (modelOptions.doSoluteTransport) {
                        boundaryConcTextField.setEnabled(false);
                        concTextField.setEnabled(false);
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
        
        if (modelOptions.doEnergyTransport) {
            if (flowBCType == NO_FLOW_BC) {
                if (energyTransportBCType == SPECIFIED_CONC_BC) {
                    boundaryConcRadioButton.setSelected(true);
                    if (energyTransportBCValue != Double.MIN_VALUE) {
                        boundaryConcTextField.setText(String.valueOf(energyTransportBCValue));
                    }
                }
                else if (energyTransportBCType == DIFFUSIVE_FLUX_BC) {
                    diffEnergyFluxRadioButton.setSelected(true);
                    if (energyTransportBCValue != Double.MIN_VALUE) {
                        diffEnergyFluxTextField.setText(String.valueOf(energyTransportBCValue));
                    }
                }
            }
            else if (flowBCType == PRESSURE_HEAD_BC
                    || flowBCType == TOTAL_HEAD_BC
                    || (flowBCType == NORMAL_FLUID_FLUX_BC   && flowBCValue > 0)
                    || (flowBCType == VERTICAL_FLUID_FLUX_BC && flowBCValue > 0)
                    || (flowBCType == VOLUMETRIC_FLOW_BC     && flowBCValue > 0)) {
                if (energyTransportBCValue != Double.MIN_VALUE) {
                    tempTextField.setText(String.valueOf(energyTransportBCValue));
                }
                if (energyTransportBCType == DEFAULT_CONC_BC) {
                    tempChooser.setSelectedIndex(0);
                }
                else if (energyTransportBCType == SPECIFIED_CONC_BC) {
                    tempChooser.setSelectedIndex(1);
                }
            }
            else if (flowBCType == EVAPORATION_BC && modelOptions.doEnergyTransport) {
                if (energyTransportBCType == SPECIFIED_CONC_BC) {
                    evapTemperatureRadioButton.setSelected(true);
                    if (energyTransportBCValue != Double.MIN_VALUE) {
                        evapTemperatureTextField.setText(String.valueOf(energyTransportBCValue));
                    }
                } else {
                    evapOutflowRadioButton.setSelected(true);
                }
            }
            else if (flowBCType == SEEPAGE_FACE_BC && modelOptions.doEnergyTransport) {
                if (energyTransportBCType == SPECIFIED_CONC_BC) {
                    seepTemperatureRadioButton.setSelected(true);
                    if (energyTransportBCValue != Double.MIN_VALUE) {
                        seepTemperatureTextField.setText(String.valueOf(energyTransportBCValue));
                    }
                } else {
                    seepOutflowRadioButton.setSelected(true);
                }
            }
            else if (flowBCType == GRAVITY_DRAIN_BC && modelOptions.doEnergyTransport) {
                if (energyTransportBCType == SPECIFIED_CONC_BC) {
                    gdrnTemperatureRadioButton.setSelected(true);
                    if (energyTransportBCValue != Double.MIN_VALUE) {
                        gdrnTemperatureTextField.setText(String.valueOf(energyTransportBCValue));
                    }
                } else {
                    gdrnOutflowRadioButton.setSelected(true);
                }
            }
        }
        
        if (modelOptions.doSoluteTransport) {
            if (flowBCType == NO_FLOW_BC) {
                if (soluteTransportBCType == SPECIFIED_CONC_BC) {
                    boundaryConcRadioButton.setSelected(true);
                    if (soluteTransportBCValue != Integer.MIN_VALUE) {
                        boundaryConcTextField.setText(String.valueOf(soluteTransportBCValue));
                    }
                }
            }
            else if (flowBCType == PRESSURE_HEAD_BC
                    || flowBCType == TOTAL_HEAD_BC
                    || (flowBCType == NORMAL_FLUID_FLUX_BC && flowBCValue > 0)
                    || (flowBCType == VERTICAL_FLUID_FLUX_BC && flowBCValue > 0)
                    || (flowBCType == VOLUMETRIC_FLOW_BC && flowBCValue > 0)) {
                if (soluteTransportBCValue != Integer.MIN_VALUE) {
                    concTextField.setText(String.valueOf(soluteTransportBCValue));
                }
                if (soluteTransportBCType == DEFAULT_CONC_BC) {
                    concChooser.setSelectedIndex(0);
                }
                else if (soluteTransportBCType == SPECIFIED_CONC_BC) {
                    concChooser.setSelectedIndex(1);
                }
            }
        }

        if ((flowBCType == NO_FLOW_BC && !(modelOptions.doSoluteTransport || modelOptions.doEnergyTransport))
            || (flowBCType == NO_FLOW_BC && modelOptions.doSoluteTransport && soluteTransportBCType == DEFAULT_CONC_BC)
            || (flowBCType == NO_FLOW_BC && modelOptions.doEnergyTransport && energyTransportBCType == DEFAULT_CONC_BC)
            || flowBCType == EVAPORATION_BC || flowBCType == GRAVITY_DRAIN_BC
            || flowBCType == SEEPAGE_FACE_BC) {
            getFromFileRadioButton.setEnabled(false);
        }

        return super.doModal();
    }

    @Override
    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile("flowBoundaryConditions.html");
    }

    protected void onSelectedFlowBC() {
        String flowBC = (String) flowBCChooser.getSelectedItem();

        if (flowBC.equalsIgnoreCase(noFlow)) {
            flowBCValuePanel.setVisible(false);
            if (modelOptions.doEnergyTransport) {
                CardLayout cl = (CardLayout)(energyCards.getLayout());
                cl.show(energyCards, "Card 1");                
            }
            if (modelOptions.doSoluteTransport) {
                CardLayout cl = (CardLayout)(soluteCards.getLayout());
                cl.show(soluteCards, "Card 1");                
            }
            
        }
        else if (flowBC.equalsIgnoreCase(pressureHead)
                || flowBC.equalsIgnoreCase(totalHead)
                || flowBC.equalsIgnoreCase(normalFluidFluxIn)
                || flowBC.equalsIgnoreCase(verticalFluidFluxIn)
                || flowBC.equalsIgnoreCase(volumeFlowIn)) {
            flowBCValuePanel.setVisible(true);
            if (modelOptions.doEnergyTransport) {
                CardLayout cl = (CardLayout)(energyCards.getLayout());
                cl.show(energyCards, "Card 2");                
            }
            if (modelOptions.doSoluteTransport) {
                CardLayout cl = (CardLayout)(soluteCards.getLayout());
                cl.show(soluteCards, "Card 2");                
            }
        }
        else if (flowBC.equalsIgnoreCase(normalFluidFluxOut)
                || flowBC.equalsIgnoreCase(verticalFluidFluxOut)
                || flowBC.equalsIgnoreCase(volumeFlowOut)) {
            flowBCValuePanel.setVisible(true);
            if (modelOptions.doEnergyTransport) {
                CardLayout cl = (CardLayout)(energyCards.getLayout());
                cl.show(energyCards, "Card 3");                
            }
            if (modelOptions.doSoluteTransport) {
                CardLayout cl = (CardLayout)(soluteCards.getLayout());
                cl.show(soluteCards, "Card 3");                
            }
        }
        else if (flowBC.equalsIgnoreCase(seepageFace)) {
            flowBCValuePanel.setVisible(false);
            if (modelOptions.doEnergyTransport) {
                CardLayout cl = (CardLayout)(energyCards.getLayout());
                cl.show(energyCards, "Card 4");                
            }
            if (modelOptions.doSoluteTransport) {
                CardLayout cl = (CardLayout)(soluteCards.getLayout());
                cl.show(soluteCards, "Card 3");                
            }
        }
        else if (flowBC.equalsIgnoreCase(gravityDrain)) {
            flowBCValuePanel.setVisible(false);
            if (modelOptions.doEnergyTransport) {
                CardLayout cl = (CardLayout)(energyCards.getLayout());
                cl.show(energyCards, "Card 4a");                
            }
            if (modelOptions.doSoluteTransport) {
                CardLayout cl = (CardLayout)(soluteCards.getLayout());
                cl.show(soluteCards, "Card 3");                
            }
        }
        else if (flowBC.equalsIgnoreCase(evapotrans)) {
            flowBCValuePanel.setVisible(false);
            if (modelOptions.doEnergyTransport) {
                CardLayout cl = (CardLayout)(energyCards.getLayout());
                cl.show(energyCards, "Card 5");                
            }
            if (modelOptions.doSoluteTransport) {
                CardLayout cl = (CardLayout)(soluteCards.getLayout());
                cl.show(soluteCards, "Card 3");                
            }
        }
        if ((flowBC.equalsIgnoreCase(noFlow) && !(modelOptions.doEnergyTransport || modelOptions.doSoluteTransport))
            || (flowBC.equalsIgnoreCase(noFlow) && (modelOptions.doEnergyTransport || modelOptions.doSoluteTransport) && noSoluteFluxRadioButton.isSelected())
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

    @Override
    protected boolean retrieveData() {

        flowBCType = NO_FLOW_BC;
        flowBCValue = 0;

        // solute transport
        soluteTransportBCType = DEFAULT_CONC_BC;
        soluteTransportBCValue = 0;

        // energy transport
        energyTransportBCType = DEFAULT_CONC_BC;
        energyTransportBCValue = 0;

        double vf, v1, v2;
        boolean bf, b1, b2;

        String energyQuantity = "the temperature";
        String solutionQuantity = "the solution";

        getFromFile = getFromFileRadioButton.isSelected();               // "Import BC data starting this recharge period"

        applyToThisPeriodOnly = thisPeriodOnlyRadioButton.isSelected();  // "Apply to this recharge period only"

        String flowBC = (String) flowBCChooser.getSelectedItem();

        if (flowBC.equalsIgnoreCase(evapotrans)) {
            flowBCType = EVAPORATION_BC;
            if (!modelOptions.doEnergyTransport || evapOutflowRadioButton.isSelected()){
                return true;
            }
        }
        else if (flowBC.equalsIgnoreCase(seepageFace)) {
            flowBCType = SEEPAGE_FACE_BC;
            if (!modelOptions.doEnergyTransport || seepOutflowRadioButton.isSelected()){
                return true;
            }
        }
        else if (flowBC.equalsIgnoreCase(gravityDrain)) {
            flowBCType = GRAVITY_DRAIN_BC;
            if (!modelOptions.doEnergyTransport || gdrnOutflowRadioButton.isSelected()){
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
                assert(flowBCValueTextField.isVisible());
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

        if (!modelOptions.doEnergyTransport && !modelOptions.doSoluteTransport) {
            return true;
        }
        
        if (flowBCType == NO_FLOW_BC) {
            // solute transport
            if (modelOptions.doSoluteTransport) {
                if (noSoluteFluxRadioButton.isSelected()) {
                    soluteTransportBCType = DEFAULT_CONC_BC;
                    soluteTransportBCValue = 0;
                }
                else if (boundaryConcRadioButton.isSelected()) {
                    if (!getFromFile) {
                        try {
                            soluteTransportBCValue = Integer.valueOf(
                                    boundaryConcTextField.getText()).intValue();
                        } catch (NumberFormatException e) {
                            mp2MessageBox.showMessageDialog(
                                    "Please check your input.", "Input Error");
                            return false;
                        }
                        if (!dataCheck(soluteTransportBCValue, solutionQuantity,
                                    IS_NON_NEGATIVE, boundaryConcTextField)) {
                            return false;
                        }
                    }
                    soluteTransportBCType = SPECIFIED_CONC_BC;
                }
            }
            
            // energy/heat transport
            if (modelOptions.doEnergyTransport) {
                if (noEnergyFluxRadioButton.isSelected()) {
                    energyTransportBCType = DEFAULT_CONC_BC;
                    energyTransportBCValue = 0;
                }
                else if (boundaryTempRadioButton.isSelected()) {
                    if (!getFromFile) {
                        try {
                            energyTransportBCValue = Double.valueOf(
                                    boundaryTempTextField.getText()).intValue();
                        } catch (NumberFormatException e) {
                            mp2MessageBox.showMessageDialog(
                                    "Please check your input.", "Input Error");
                            return false;
                        }
                        if (!dataCheck(energyTransportBCValue, energyQuantity,
                                    IS_NON_NEGATIVE, boundaryTempTextField)) {
                            return false;
                        }
                    }
                    energyTransportBCType = SPECIFIED_CONC_BC;
                    return true;
                } else {
                    if (!getFromFile) {
                        try {
                            energyTransportBCValue = Double.valueOf(
                                    diffEnergyFluxTextField.getText()).doubleValue();
                        } catch (NumberFormatException e) {
                            mp2MessageBox.showMessageDialog(
                                    "Please check your input.", "Input Error");
                            return false;
                        }
                        if (!dataCheck(energyTransportBCValue, "the diffusive flux",
                                IS_NON_NEGATIVE, diffEnergyFluxTextField)) {
                            return false;
                        }
                    }
                    energyTransportBCType = DIFFUSIVE_FLUX_BC;
                }
            }
            return true;
        }

        else if (modelOptions.doEnergyTransport && flowBCType == EVAPORATION_BC) {
            if (!getFromFile) {
                try {
                    energyTransportBCValue = Double.valueOf(
                            evapTemperatureTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog(
                            "Please check your input.", "Input Error");
                    return false;
                }

                if (!dataCheck(energyTransportBCValue, energyQuantity,
                            IS_NON_NEGATIVE, evapTemperatureTextField)) {
                    return false;
                }
            }
            energyTransportBCType = SPECIFIED_CONC_BC;
            return true;
        }

        else if (modelOptions.doEnergyTransport && flowBCType == SEEPAGE_FACE_BC) {
            if (!getFromFile) {
                try {
                    energyTransportBCValue = Double.valueOf(
                            seepTemperatureTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog(
                            "Please check your input.", "Input Error");
                    return false;
                }

                if (!dataCheck(energyTransportBCValue, energyQuantity,
                            IS_NON_NEGATIVE, seepTemperatureTextField)) {
                    return false;
                }
            }
            energyTransportBCType = SPECIFIED_CONC_BC;
            return true;
        }

        else if (modelOptions.doEnergyTransport && flowBCType == GRAVITY_DRAIN_BC) {
            if (!getFromFile) {
                try {
                    energyTransportBCValue = Double.valueOf(
                            gdrnTemperatureTextField.getText()).doubleValue();
                } catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog(
                            "Please check your input.", "Input Error");
                    return false;
                }

                if (!dataCheck(energyTransportBCValue, energyQuantity,
                            IS_NON_NEGATIVE, gdrnTemperatureTextField)) {
                    return false;
                }
            }
            energyTransportBCType = SPECIFIED_CONC_BC;
            return true;
        }

        else {
            assert(flowBCType  == PRESSURE_HEAD_BC      ||
                    flowBCType == TOTAL_HEAD_BC         ||
                    flowBCType == NORMAL_FLUID_FLUX_BC  ||
                    flowBCType == VERTICAL_FLUID_FLUX_BC );
            // solute transport
            if (modelOptions.doSoluteTransport) {                    
                if (!getFromFile) {
                    try {
                        soluteTransportBCValue = Integer.valueOf(
                                concTextField.getText()).intValue();
                    } catch (NumberFormatException e) {
                        mp2MessageBox.showMessageDialog(
                                "Please check your input.", "Input Error");
                        return false;
                    }

                    if (!dataCheck(soluteTransportBCValue, solutionQuantity,
                            IS_NON_NEGATIVE, concTextField)) {
                        return false;
                    }
                }
                if (concChooser.getSelectedIndex() == 1) {
                    soluteTransportBCType = SPECIFIED_CONC_BC;
                } else {
                    soluteTransportBCType = DEFAULT_CONC_BC;
                }
            }
            
            // energy/heat transport
            if (modelOptions.doEnergyTransport) {                    
                if (!getFromFile) {
                    try {
                        energyTransportBCValue = Double.valueOf(
                                tempTextField.getText());
                    } catch (NumberFormatException e) {
                        mp2MessageBox.showMessageDialog(
                                "Please check your input.", "Input Error");
                        return false;
                    }

                    if (!dataCheck(energyTransportBCValue, energyQuantity,
                            IS_NON_NEGATIVE, tempTextField)) {
                        return false;
                    }
                }
                if (tempChooser.getSelectedIndex() == 1) {
                    energyTransportBCType = SPECIFIED_CONC_BC;
                } else {
                    energyTransportBCType = DEFAULT_CONC_BC;
                }
            }
            return true;
        }
    }
}

