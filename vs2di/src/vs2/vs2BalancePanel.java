/*
 * vs2EnergyBalancePanel.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;
import javax.swing.*;

public class vs2BalancePanel extends vs2ModelOptionsPanel {

    public boolean inEnergySpecifiedHead;
    public boolean outEnergySpecifiedHead;
    public boolean inEnergySpecifiedFlux;
    public boolean outEnergySpecifiedFlux;
    public boolean inEnergyDispersion;
    public boolean outEnergyDispersion;
    public boolean inEnergyTotal;
    public boolean outEnergyTotal;
    public boolean outEnergyEvapoTranspiration;
    public boolean changeInEnergyStorage;
    public boolean energyBalance;

    protected JCheckBox inEnergySpecifiedHeadCheckBox;
    protected JCheckBox outEnergySpecifiedHeadCheckBox;
    protected JCheckBox inEnergySpecifiedFluxCheckBox;
    protected JCheckBox outEnergySpecifiedFluxCheckBox;
    protected JCheckBox inEnergyDispersionCheckBox;
    protected JCheckBox outEnergyDispersionCheckBox;
    protected JCheckBox inEnergyTotalCheckBox;
    protected JCheckBox outEnergyTotalCheckBox;
    protected JCheckBox outEnergyEvapoTranspirationCheckBox;
    protected JCheckBox changeInEnergyStorageCheckBox;
    protected JCheckBox energyBalanceCheckBox;

    /**
     * Creates the panel for output options
     */
    public vs2BalancePanel(vs2ModelOptionsDialog parentDialog) {

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
                BorderFactory.createTitledBorder("Output Options for Energy/Solute Balance"),
                new EmptyBorder(5, 5, 5, 5)));
        panel.add(leftPanel = new JPanel(new GridLayout(6, 1), false));
        panel.add(rightPanel = new JPanel(new GridLayout(6, 1), false));
        leftPanel.add(inEnergySpecifiedHeadCheckBox = new JCheckBox("Specified Head, Advection In"));
        leftPanel.add(outEnergySpecifiedHeadCheckBox = new JCheckBox("Specified Head, Advection Out"));
        leftPanel.add(inEnergySpecifiedFluxCheckBox = new JCheckBox("Specified Flux, Advection In"));
        leftPanel.add(outEnergySpecifiedFluxCheckBox = new JCheckBox("Specified Flux, Advection Out"));
        leftPanel.add(inEnergyDispersionCheckBox = new JCheckBox("Dispersion/Conduction, In"));
        leftPanel.add(outEnergyDispersionCheckBox = new JCheckBox("Dispersion/Conduction, Out"));
        rightPanel.add(outEnergyEvapoTranspirationCheckBox = new JCheckBox("Evapotranspiration"));
        rightPanel.add(inEnergyTotalCheckBox = new JCheckBox("Total Energy/Solute, In"));
        rightPanel.add(outEnergyTotalCheckBox = new JCheckBox("Total Energy/Solute, Out"));
        rightPanel.add(changeInEnergyStorageCheckBox = new JCheckBox("Change in Energy/Solute Storage"));
        rightPanel.add(energyBalanceCheckBox = new JCheckBox("Energy/Solute Balance"));
        
    }

    /**
     * Put data in components
     */
    public void init() {
        inEnergySpecifiedHeadCheckBox.setSelected(inEnergySpecifiedHead);
        outEnergySpecifiedHeadCheckBox.setSelected(outEnergySpecifiedHead);
        inEnergySpecifiedFluxCheckBox.setSelected(inEnergySpecifiedFlux);
        outEnergySpecifiedFluxCheckBox.setSelected(outEnergySpecifiedFlux);
        inEnergyDispersionCheckBox.setSelected(inEnergyDispersion);
        outEnergyDispersionCheckBox.setSelected(outEnergyDispersion);
        inEnergyTotalCheckBox.setSelected(inEnergyTotal);
        outEnergyTotalCheckBox.setSelected(outEnergyTotal);
        outEnergyEvapoTranspirationCheckBox.setSelected(outEnergyEvapoTranspiration);
        changeInEnergyStorageCheckBox.setSelected(changeInEnergyStorage);
        energyBalanceCheckBox.setSelected(energyBalance);
    }

    /**
     * Get data from components
     */
    public boolean retrieveData() {
        inEnergySpecifiedHead = inEnergySpecifiedHeadCheckBox.isSelected();
        outEnergySpecifiedHead = outEnergySpecifiedHeadCheckBox.isSelected();
        inEnergySpecifiedFlux = inEnergySpecifiedFluxCheckBox.isSelected();
        outEnergySpecifiedFlux = outEnergySpecifiedFluxCheckBox.isSelected();
        inEnergyDispersion = inEnergyDispersionCheckBox.isSelected();
        outEnergyDispersion = outEnergyDispersionCheckBox.isSelected();
        inEnergyTotal = inEnergyTotalCheckBox.isSelected();
        outEnergyTotal = outEnergyTotalCheckBox.isSelected();
        outEnergyEvapoTranspiration = outEnergyEvapoTranspirationCheckBox.isSelected();
        changeInEnergyStorage = changeInEnergyStorageCheckBox.isSelected();
        energyBalance = energyBalanceCheckBox.isSelected();
        return true;
    }

}
