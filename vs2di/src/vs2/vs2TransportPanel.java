/*
 * vs2TransportPanel.java
 */
package vs2;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2TransportPanel extends vs2ModelOptionsPanel
                     implements vs2Constants {

    public boolean doSpaceCentered;
    public boolean doTimeCentered;
    public int reactionOption;
    public String chemFile;          // new in 1.4
    public String databaseFile;      // new in 1.4
    public String prefix;            // new in 1.4
    
    protected JRadioButton spaceCentered;
    protected JRadioButton spaceBackward;
    protected JRadioButton timeCentered;
    protected JRadioButton timeBackward;
    
    protected JTextField chemfileTextField;
    protected JTextField databasefileTextField;
    protected JTextField prefixTextField;
    
    protected JLabel chemfileLabel;
    protected JLabel databasefileLabel;
    protected JLabel prefixLabel;
    
    // IPRNTCHE
    protected JRadioButton noPhreeqcOutputRadioButton;
    protected JRadioButton selectedOutputRadioButton;
    
    // IPOUT
    protected JRadioButton ipoutNoPhreeqcOutputRadioButton;
    protected JRadioButton ipoutExtensivePhreeqcOutputRadioButton;
    

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

        // phreeqc options
        add(panel = new JPanel(false));
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.HORIZONTAL;
        gridbag.setConstraints(panel, c);
        panel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Phreeqc options"),
                new EmptyBorder(4, 10, 10, 10)));
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        JPanel filesPanel = new JPanel(false);
        filesPanel.setLayout(new BoxLayout(filesPanel, BoxLayout.X_AXIS));
        panel.add(filesPanel);

        // Make a left panel to hold the labels
        JPanel transLeftPanel = new JPanel(false);
        transLeftPanel.setLayout(new GridLayout(0, 1, 5, 5));
        filesPanel.add(transLeftPanel);

        // Put space between the left and right panels
        filesPanel.add(Box.createHorizontalStrut(5));

        // Make a right panel to hold the text fields
        JPanel transRightPanel = new JPanel(false);
        transRightPanel.setLayout(new GridLayout(0, 1, 5, 5));
        filesPanel.add(transRightPanel);           

        // add labels and text fields for transport parameters
        transLeftPanel.add(chemfileLabel = new JLabel("Phreeqc input file name", SwingConstants.RIGHT));
        transRightPanel.add(chemfileTextField = new JTextField(20));

        transLeftPanel.add(databasefileLabel = new JLabel("Database file name", SwingConstants.RIGHT));
        transRightPanel.add(databasefileTextField = new JTextField(20));

        transLeftPanel.add(prefixLabel = new JLabel("Prefix name", SwingConstants.RIGHT));
        transRightPanel.add(prefixTextField = new JTextField(20));
    }
    
    public void doSoluteTransport(boolean b) {
        chemfileLabel.setEnabled(b);
        databasefileLabel.setEnabled(b);
        prefixLabel.setEnabled(b);
        
        chemfileTextField.setEnabled(b);
        databasefileTextField.setEnabled(b);
        prefixTextField.setEnabled(b);
        
        revalidate();
    }
    

    /**
     * Puts values in the components in the panel
     */
    public void init() {
        spaceCentered.setSelected(doSpaceCentered);
        spaceBackward.setSelected(!doSpaceCentered);
        timeCentered.setSelected(doTimeCentered);
        timeBackward.setSelected(!doTimeCentered);

        // CHEMFILE
        chemfileTextField.setText(chemFile);                

        // DATABASEFILE
        databasefileTextField.setText(databaseFile);                

        // PREFIX
        prefixTextField.setText(prefix);                
    }

    /**
     * Get values from components. This method always returns true because 
     * there is no possibility for input error
     */
    public boolean retrieveData() {
        doSpaceCentered = spaceCentered.isSelected();
        doTimeCentered = timeCentered.isSelected();

        if (true) {
            // CHEMFILE
            chemFile = chemfileTextField.getText();

            // DATABASEFILE
            databaseFile = databasefileTextField.getText();

            // PREFIX
            prefix = prefixTextField.getText();
        }
        return true;
    }
}

