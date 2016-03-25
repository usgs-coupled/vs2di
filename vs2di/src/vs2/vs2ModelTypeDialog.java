/*
 * vs2ModelTypeDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2ModelTypeDialog extends mp2Dialog implements vs2Constants {

    public int usage;
    public boolean useOldVersion;
    public int hydraulicFunctionType;
    public int adsorptionType;
    
    protected JRadioButton soluteRadioButton;
    protected JRadioButton solute2p5RadioButton;
    protected JRadioButton energyRadioButton;
    protected JRadioButton energy1p0RadioButton;
    protected JComboBox hydraulicFunctionChooser;
    protected JComboBox adsorptionChooser;
    
    public vs2ModelTypeDialog(JFrame frame, String filename) {
        super("Model Type", true, (Object) filename, frame);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "modelType", null);
    }

    protected void makeContents() {
        String filename = (String) customObject;
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        JPanel centerPanel = new JPanel(gridbag);
        centerPanel.setBorder(new EmptyBorder(25, 25, 25, 25));
        getContentPane().add(centerPanel, BorderLayout.CENTER);
        
        JPanel panel = new JPanel(new GridLayout(0, 1, 5, 5));
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);
        panel.add(new JLabel("You have selected to load the file \"" + filename + "\""));
        panel.add(new JLabel("Please specify the type of model to run:"));
        panel.add(soluteRadioButton = new JRadioButton("solute transport (VS2DT-GUI)", true));
        panel.add(energyRadioButton = new JRadioButton("energy transport (VS2DH-GUI)"));
        panel.add(solute2p5RadioButton = new JRadioButton("solute transport (VS2DT ver 2.5)"));
        panel.add(energy1p0RadioButton = new JRadioButton("energy transport (VS2DH ver 1.0)"));

        panel = new JPanel(gridbag);
        c.insets = new Insets(20, 0, 0, 0);
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);
        
        JPanel leftPanel = new JPanel(new GridLayout(0, 1, 10, 10));
        c.insets = new Insets(0, 0, 0, 0);
        c.gridwidth = GridBagConstraints.RELATIVE;
        c.fill = GridBagConstraints.VERTICAL;
        gridbag.setConstraints(leftPanel, c);
        panel.add(leftPanel);
        leftPanel.add(new JLabel("Hydraulic Function:", SwingConstants.RIGHT));
        leftPanel.add(new JLabel("Adsorption/Ion Exchange:", SwingConstants.RIGHT));
        
        JPanel rightPanel = new JPanel(new GridLayout(0, 1, 10, 10));
        c.insets = new Insets(0, 10, 0, 0);
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(rightPanel, c);
        panel.add(rightPanel);
        rightPanel.add(hydraulicFunctionChooser = new JComboBox());
        rightPanel.add(adsorptionChooser = new JComboBox());
        hydraulicFunctionChooser.addItem("Brooks Corey");
        hydraulicFunctionChooser.addItem("Van Genuchten");
        hydraulicFunctionChooser.addItem("Haverkamp");
        hydraulicFunctionChooser.addItem("Tabular Data");
        adsorptionChooser.addItem("None");
        adsorptionChooser.addItem("Langmuir isotherm");
        adsorptionChooser.addItem("Freundlich isotherm");
        adsorptionChooser.addItem("Monovalent-monovalent ion exch");
        adsorptionChooser.addItem("Monovalent-divalent ion exch");
        adsorptionChooser.addItem("Divalent-monovalent ion exch");
        adsorptionChooser.addItem("Divalent-divalent ion exch");
        
        ButtonGroup bg = new ButtonGroup();
        bg.add(soluteRadioButton);
        bg.add(energyRadioButton);
        bg.add(solute2p5RadioButton);
        bg.add(energy1p0RadioButton);
        
        soluteRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    hydraulicFunctionChooser.setEnabled(false);
                    adsorptionChooser.setEnabled(false);
                }
            }
        });
        energyRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    hydraulicFunctionChooser.setEnabled(false);
                    adsorptionChooser.setEnabled(false);
                }
            }
        });
        solute2p5RadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    hydraulicFunctionChooser.setEnabled(true);
                    adsorptionChooser.setEnabled(true);
                }
            }
        });
        energy1p0RadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    hydraulicFunctionChooser.setEnabled(true);
                    adsorptionChooser.setEnabled(false);
                }
            }
        });
    }

    public boolean doModal() {
        soluteRadioButton.setSelected(usage == SOLUTE_TRANSPORT && !useOldVersion);
        solute2p5RadioButton.setSelected(usage == SOLUTE_TRANSPORT && useOldVersion);
        energyRadioButton.setSelected(usage == ENERGY_TRANSPORT && !useOldVersion);
        energy1p0RadioButton.setSelected(usage == ENERGY_TRANSPORT && useOldVersion);
        if (useOldVersion) {
            hydraulicFunctionChooser.setSelectedIndex(hydraulicFunctionType);
            if (usage == SOLUTE_TRANSPORT) {
                adsorptionChooser.setSelectedIndex(adsorptionType - 1);
            } else {
                adsorptionChooser.setEnabled(false);
            }
        } else {
            hydraulicFunctionChooser.setEnabled(false);
            adsorptionChooser.setEnabled(false);
        }
        return super.doModal();
    }

    protected boolean retrieveData() {
        if (energyRadioButton.isSelected()) {
            usage = ENERGY_TRANSPORT;
            useOldVersion = false;
        } else if (energy1p0RadioButton.isSelected()) {
            usage = ENERGY_TRANSPORT;
            useOldVersion = true;
        } else if (solute2p5RadioButton.isSelected()) {
            usage = SOLUTE_TRANSPORT;
            useOldVersion = true;
        } else {
            usage = SOLUTE_TRANSPORT;
            useOldVersion = false;
        }
        if (useOldVersion) {
            hydraulicFunctionType = hydraulicFunctionChooser.getSelectedIndex();
            if (usage == SOLUTE_TRANSPORT) {
                adsorptionType = adsorptionChooser.getSelectedIndex() + 1;
            }
        }
        return true;
    }

    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile("modelType.html");
    }

}

