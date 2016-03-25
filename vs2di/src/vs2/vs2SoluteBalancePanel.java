/*
 * vs2SoluteBalancePanel.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;
import javax.swing.*;

public class vs2SoluteBalancePanel extends vs2ModelOptionsPanel {

    public boolean inSoluteSpecifiedHead;
    public boolean outSoluteSpecifiedHead;
    public boolean inSoluteSpecifiedFlux;
    public boolean outSoluteSpecifiedFlux;
    public boolean inSoluteDispersion;
    public boolean outSoluteDispersion;
    public boolean inSoluteTotal;
    public boolean outSoluteTotal;
    public boolean outSoluteEvapoTranspiration;
    public boolean outSoluteDecay;
    public boolean outSoluteAdsorption;
    public boolean changeInSoluteStorage;
    public boolean soluteBalance;

    protected JCheckBox inSoluteSpecifiedHeadCheckBox;
    protected JCheckBox outSoluteSpecifiedHeadCheckBox;
    protected JCheckBox inSoluteSpecifiedFluxCheckBox;
    protected JCheckBox outSoluteSpecifiedFluxCheckBox;
    protected JCheckBox inSoluteDispersionCheckBox;
    protected JCheckBox outSoluteDispersionCheckBox;
    protected JCheckBox inSoluteTotalCheckBox;
    protected JCheckBox outSoluteTotalCheckBox;
    protected JCheckBox outSoluteEvapoTranspirationCheckBox;
    protected JCheckBox outSoluteDecayCheckBox;
    protected JCheckBox outSoluteAdsorptionCheckBox;
    protected JCheckBox changeInSoluteStorageCheckBox;
    protected JCheckBox soluteBalanceCheckBox;

    /**
     * Creates the panel for output options
     */
    public vs2SoluteBalancePanel(vs2ModelOptionsDialog parentDialog) {

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
                BorderFactory.createTitledBorder("Output Options for Solute Balance"),
                new EmptyBorder(5, 5, 5, 5)));
        panel.add(leftPanel = new JPanel(new GridLayout(8, 1), false));
        leftPanel.add(inSoluteSpecifiedHeadCheckBox = new JCheckBox("Specified Head, Advection In"));
        leftPanel.add(outSoluteSpecifiedHeadCheckBox = new JCheckBox("Specified Head, Advection Out"));
        leftPanel.add(inSoluteSpecifiedFluxCheckBox = new JCheckBox("Specified Flux, Advection In"));
        leftPanel.add(outSoluteSpecifiedFluxCheckBox = new JCheckBox("Specified Flux, Advection Out"));
        leftPanel.add(inSoluteDispersionCheckBox = new JCheckBox("Dispersion/Diffusion, In"));
        leftPanel.add(outSoluteDispersionCheckBox = new JCheckBox("Dispersion/Diffusion, Out"));
        leftPanel.add(inSoluteTotalCheckBox = new JCheckBox("Total Solute, In"));
        leftPanel.add(outSoluteTotalCheckBox = new JCheckBox("Total Solute, Out"));
 
        panel.add(rightPanel = new JPanel(new GridLayout(8, 1), false));
        rightPanel.add(outSoluteDecayCheckBox = new JCheckBox("Decay"));
        rightPanel.add(outSoluteAdsorptionCheckBox = new JCheckBox("Adsorption or Ion Exchange"));
        rightPanel.add(outSoluteEvapoTranspirationCheckBox = new JCheckBox("Transpiration"));
        rightPanel.add(changeInSoluteStorageCheckBox = new JCheckBox("Change in Solute Storage"));
        rightPanel.add(soluteBalanceCheckBox = new JCheckBox("Solute Mass Balance"));
        
    }

    /**
     * Put data in components
     */
    public void init() {
        inSoluteSpecifiedHeadCheckBox.setSelected(inSoluteSpecifiedHead);
        outSoluteSpecifiedHeadCheckBox.setSelected(outSoluteSpecifiedHead);
        inSoluteSpecifiedFluxCheckBox.setSelected(inSoluteSpecifiedFlux);
        outSoluteSpecifiedFluxCheckBox.setSelected(outSoluteSpecifiedFlux);
        inSoluteDispersionCheckBox.setSelected(inSoluteDispersion);
        outSoluteDispersionCheckBox.setSelected(outSoluteDispersion);
        inSoluteTotalCheckBox.setSelected(inSoluteTotal);
        outSoluteTotalCheckBox.setSelected(outSoluteTotal);
        outSoluteDecayCheckBox.setSelected(outSoluteDecay);
        outSoluteAdsorptionCheckBox.setSelected(outSoluteAdsorption);
        outSoluteEvapoTranspirationCheckBox.setSelected(outSoluteEvapoTranspiration);
        changeInSoluteStorageCheckBox.setSelected(changeInSoluteStorage);
        soluteBalanceCheckBox.setSelected(soluteBalance);
    }

    /**
     * Get data from components
     */
    public boolean retrieveData() {
        inSoluteSpecifiedHead = inSoluteSpecifiedHeadCheckBox.isSelected();
        outSoluteSpecifiedHead = outSoluteSpecifiedHeadCheckBox.isSelected();
        inSoluteSpecifiedFlux = inSoluteSpecifiedFluxCheckBox.isSelected();
        outSoluteSpecifiedFlux = outSoluteSpecifiedFluxCheckBox.isSelected();
        inSoluteDispersion = inSoluteDispersionCheckBox.isSelected();
        outSoluteDispersion = outSoluteDispersionCheckBox.isSelected();
        inSoluteTotal = inSoluteTotalCheckBox.isSelected();
        outSoluteTotal = outSoluteTotalCheckBox.isSelected();
        outSoluteDecay = outSoluteDecayCheckBox.isSelected();
        outSoluteAdsorption = outSoluteAdsorptionCheckBox.isSelected();
        outSoluteEvapoTranspiration = outSoluteEvapoTranspirationCheckBox.isSelected();
        changeInSoluteStorage = changeInSoluteStorageCheckBox.isSelected();
        soluteBalance = soluteBalanceCheckBox.isSelected();
        return true;
    }

}
