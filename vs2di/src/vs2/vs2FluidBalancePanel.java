/*
 * vs2FluidBalancePanel.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;
import javax.swing.*;

public class vs2FluidBalancePanel extends vs2ModelOptionsPanel {

    public boolean inFlowSpecifiedHead;
    public boolean outFlowSpecifiedHead;
    public boolean inFlowSpecifiedFlux;
    public boolean outFlowSpecifiedFlux;
    public boolean inFlowTotal;
    public boolean outFlowTotal;
    public boolean evaporation;
    public boolean transpiration;
    public boolean evapoTranspiration;
    public boolean changeInStorage;
    public boolean fluidBalance;

    // Check boxes for options
    protected JCheckBox inFlowSpecifiedHeadCheckBox;
    protected JCheckBox outFlowSpecifiedHeadCheckBox;
    protected JCheckBox inFlowSpecifiedFluxCheckBox;
    protected JCheckBox outFlowSpecifiedFluxCheckBox;
    protected JCheckBox inFlowTotalCheckBox;
    protected JCheckBox outFlowTotalCheckBox;
    protected JCheckBox evaporationCheckBox;
    protected JCheckBox transpirationCheckBox;
    protected JCheckBox evapoTranspirationCheckBox;
    protected JCheckBox changeInStorageCheckBox;
    protected JCheckBox fluidBalanceCheckBox;

    /**
     * Creates the panel for fluid mass balance options
     */
    public vs2FluidBalancePanel(vs2ModelOptionsDialog parentDialog) {

        super(parentDialog);

        JPanel panel, leftPanel, rightPanel;
        ButtonGroup buttonGroup;

        // Put an empty border around the panel
        setBorder(new EmptyBorder(20, 50, 20, 50));
        GridBagLayout gridbag = new GridBagLayout();
        setLayout(gridbag);

        // Define gridbag constraints
        GridBagConstraints c = new GridBagConstraints();
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(10, 20, 10, 20);

        // Create a panel to hold the flow mass balance parameters
        add(panel = new JPanel(new GridLayout(1, 2, 10, 1), false));
        panel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Output Options for Fluid Balance"),
                new EmptyBorder(5, 5, 5, 5)));
        panel.add(leftPanel = new JPanel(new GridLayout(6, 1), false));
        leftPanel.add(inFlowSpecifiedHeadCheckBox = new JCheckBox("Specified Head, In"));
        leftPanel.add(outFlowSpecifiedHeadCheckBox = new JCheckBox("Specified Head, Out"));
        leftPanel.add(inFlowSpecifiedFluxCheckBox = new JCheckBox("Specified Flux, In"));
        leftPanel.add(outFlowSpecifiedFluxCheckBox = new JCheckBox("Specified Flux, Out"));
        leftPanel.add(inFlowTotalCheckBox = new JCheckBox("Total Fluid, In"));
        leftPanel.add(outFlowTotalCheckBox = new JCheckBox("Total Fluid, Out"));

        panel.add(rightPanel = new JPanel(new GridLayout(6, 1), false));
        rightPanel.add(evaporationCheckBox = new JCheckBox("Evaporation"));
        rightPanel.add(transpirationCheckBox = new JCheckBox("Transpiration"));
        rightPanel.add(evapoTranspirationCheckBox = new JCheckBox("Evaporation + Transpiration"));
        rightPanel.add(changeInStorageCheckBox = new JCheckBox("Change in Fluid Storage"));
        rightPanel.add(fluidBalanceCheckBox = new JCheckBox("Fluid Balance"));
    }

    /**
     * Put data in components
     */
    public void init() {
        inFlowSpecifiedHeadCheckBox.setSelected(inFlowSpecifiedHead);
        outFlowSpecifiedHeadCheckBox.setSelected(outFlowSpecifiedHead);
        inFlowSpecifiedFluxCheckBox.setSelected(inFlowSpecifiedFlux);
        outFlowSpecifiedFluxCheckBox.setSelected(outFlowSpecifiedFlux);
        inFlowTotalCheckBox.setSelected(inFlowTotal);
        outFlowTotalCheckBox.setSelected(outFlowTotal);
        evaporationCheckBox.setSelected(evaporation);
        transpirationCheckBox.setSelected(transpiration);
        evapoTranspirationCheckBox.setSelected(evapoTranspiration);
        changeInStorageCheckBox.setSelected(changeInStorage);
        fluidBalanceCheckBox.setSelected(fluidBalance);
    }

    /**
     * Get data from components
     */
    public boolean retrieveData() {
        inFlowSpecifiedHead = inFlowSpecifiedHeadCheckBox.isSelected();
        outFlowSpecifiedHead = outFlowSpecifiedHeadCheckBox.isSelected();
        inFlowSpecifiedFlux = inFlowSpecifiedFluxCheckBox.isSelected();
        outFlowSpecifiedFlux = outFlowSpecifiedFluxCheckBox.isSelected();
        inFlowTotal = inFlowTotalCheckBox.isSelected();
        outFlowTotal = outFlowTotalCheckBox.isSelected();
        evaporation = evaporationCheckBox.isSelected();
        transpiration = transpirationCheckBox.isSelected();
        evapoTranspiration = evapoTranspirationCheckBox.isSelected();
        changeInStorage = changeInStorageCheckBox.isSelected();
        fluidBalance = fluidBalanceCheckBox.isSelected();
        return true;
    }

}
