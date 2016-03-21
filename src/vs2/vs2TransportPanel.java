/*
 * vs2TransportPanel.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2TransportPanel extends vs2ModelOptionsPanel
                     implements vs2Constants {

    public boolean doSpaceCentered;
    public boolean doTimeCentered;
    public int reactionOption;

    protected JRadioButton spaceCentered;
    protected JRadioButton spaceBackward;
    protected JRadioButton timeCentered;
    protected JRadioButton timeBackward;
    protected JRadioButton noAdsorption;
    protected JRadioButton linearAdsorption;
    protected JRadioButton freundlichIsotherm;
    protected JRadioButton langmuirIsotherm;
    protected JRadioButton monoMonovalent;
    protected JRadioButton monoDivalent;
    protected JRadioButton diMonovalent;
    protected JRadioButton diDivalent;

    /**
     * Constructs the panel for transport options
     */
    public vs2TransportPanel(vs2ModelOptionsDialog parentDialog) {

        super(parentDialog);

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        setBorder(new EmptyBorder(10, 10, 10, 10));
        setLayout(gridbag);

        JPanel panel, subPanel;
        ButtonGroup bg;

        // differencing scheme options
        add(panel = new JPanel(new GridLayout(1, 2, 40, 0), false));
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.HORIZONTAL;
        gridbag.setConstraints(panel, c);
        panel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder(
                "Differencing scheme for transport equation"),
                new EmptyBorder(4, 10, 10, 10)));

        panel.add(subPanel = new JPanel(new GridLayout(2, 1)));
        subPanel.add(spaceCentered = new JRadioButton("Centered in space"));
        subPanel.add(spaceBackward = new JRadioButton("Backward in space"));
        bg = new ButtonGroup();
        bg.add(spaceCentered);      
        bg.add(spaceBackward);

        panel.add(subPanel = new JPanel(new GridLayout(2, 1)));
        subPanel.add(timeCentered = new JRadioButton("Centered in time"));
        subPanel.add(timeBackward = new JRadioButton("Backward in time"));
        bg = new ButtonGroup();
        bg.add(timeCentered);
        bg.add(timeBackward);

        if (!vs2App.doHeat()) {
            // sorption options
            add(panel = new JPanel(new GridLayout(1, 2), false));
            c.insets = new Insets(10, 0, 0, 0);
            gridbag.setConstraints(panel, c);
            panel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Adsorption/Ion Exchange"),
                    new EmptyBorder(4, 10, 10, 10)));

            panel.add(subPanel = new JPanel(new GridLayout(4, 1)));
            subPanel.add(noAdsorption = new JRadioButton("None"));
            subPanel.add(linearAdsorption = new JRadioButton("Linear isotherm"));
            subPanel.add(freundlichIsotherm = new JRadioButton("Freundlich isotherm"));
            subPanel.add(langmuirIsotherm = new JRadioButton("Langmuir isotherm"));

            panel.add(subPanel = new JPanel(new GridLayout(4, 1)));
            subPanel.add(monoMonovalent = new JRadioButton(
                                "Monovalent-monovalent ion exchange"));
            subPanel.add(monoDivalent = new JRadioButton(
                                "Monovalent-divalent ion exchange"));
            subPanel.add(diMonovalent = new JRadioButton(
                                "Divalent-monovalent ion exchange"));
            subPanel.add(diDivalent = new JRadioButton(
                                "Divalent-divalent ion exchange"));

            bg = new ButtonGroup();
            bg.add(noAdsorption);
            bg.add(linearAdsorption);
            bg.add(freundlichIsotherm);
            bg.add(langmuirIsotherm);
            bg.add(monoMonovalent);
            bg.add(monoDivalent);
            bg.add(diMonovalent);
            bg.add(diDivalent);
        }
    }

    /**
     * Puts values in the components in the panel
     */
    public void init() {
        spaceCentered.setSelected(doSpaceCentered);
        spaceBackward.setSelected(!doSpaceCentered);
        timeCentered.setSelected(doTimeCentered);
        timeBackward.setSelected(!doTimeCentered);

        if (!vs2App.doHeat()) {
            noAdsorption.setSelected(reactionOption == 
                                    N0_ADSORPTION_NO_ION_EXCHANGE);
            linearAdsorption.setSelected(reactionOption == 
                                                LINEAR_ADSORPTION);
            freundlichIsotherm.setSelected(reactionOption == FREUNDLICH);
            langmuirIsotherm.setSelected(reactionOption == LANGMUIR);
            monoMonovalent.setSelected(reactionOption == 
                                    MONO_MONOVALENT_ION_EXCHANGE);
            monoDivalent.setSelected(reactionOption == 
                                    MONO_DIVALENT_ION_EXCHANGE);
            diMonovalent.setSelected(reactionOption == 
                                    DI_MONOVALENT_ION_EXCHANGE);
            diDivalent.setSelected(reactionOption == 
                                    DI_DIVALENT_ION_EXCHANGE);
        }
    }

    /**
     * Get values from components. This method always returns true because 
     * there is no possibility for input error
     */
    public boolean retrieveData() {
        doSpaceCentered = spaceCentered.isSelected();
        doTimeCentered = timeCentered.isSelected();

        if (!vs2App.doHeat()) {
            if (noAdsorption.isSelected()) {
                reactionOption = N0_ADSORPTION_NO_ION_EXCHANGE;
            }
            else if (linearAdsorption.isSelected()) {
                reactionOption = LINEAR_ADSORPTION;
            }
            else if (freundlichIsotherm.isSelected()) {
                reactionOption = FREUNDLICH;
            }
            else if (langmuirIsotherm.isSelected()) {
                reactionOption = LANGMUIR;
            }
            else if (monoMonovalent.isSelected()) {
                reactionOption = MONO_MONOVALENT_ION_EXCHANGE;
            }
            else if (monoDivalent.isSelected()) {
                reactionOption = MONO_DIVALENT_ION_EXCHANGE;
            }
            else if (diMonovalent.isSelected()) {
                reactionOption = DI_MONOVALENT_ION_EXCHANGE;
            }
            else if (diDivalent.isSelected()) {
                reactionOption = DI_DIVALENT_ION_EXCHANGE;
            }
        }
        return true;
    }
}

