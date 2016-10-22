/*
 * vs2ChemistryClassDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2ChemistryClassDialog extends vs2Dialog implements vs2Constants,
   vs2VanGenuchtenParameters
{
   //---------------------------------------------------------------------------
   // Fields
   //---------------------------------------------------------------------------
   // A row of recharge period data
   public Object [] aRow;

   // model options
   protected vs2ModelOptions modelOptions;

   // Boolean flag that is true if this dialog box is for editing data, and
   // false if the dialog box is for adding data
   protected boolean doEdit;

   // Components in the dialog box
   protected JButton colorButton;
   protected ColorLabel colorLabel;
   protected JTextField nameTextField;
   
   protected JTextField solutionTextField;   
   protected JTextField equilibriumPhasesTextField;
   protected JTextField exchangeTextField;
   protected JTextField surfaceTextField;
   protected JTextField gasPhaseTextField;
   protected JTextField solidSolutionsTextField;
   protected JTextField kineticsTextField;
   
   protected JCheckBox solutionCheckBox;
   protected JCheckBox equilibriumPhasesCheckBox;
   protected JCheckBox exchangeCheckBox;
   protected JCheckBox surfaceCheckBox;
   protected JCheckBox gasPhaseCheckBox;
   protected JCheckBox solidSolutionsCheckBox;
   protected JCheckBox kineticsCheckBox;
   
   
    // this array is created after super ctor returns
    protected JTextField[] textFields = {
        solutionTextField,
        equilibriumPhasesTextField,
        exchangeTextField,
        surfaceTextField,
        gasPhaseTextField,
        solidSolutionsTextField,
        kineticsTextField
    };
    
    // this array is created after super ctor returns
    protected JCheckBox[] checkBoxes = {
        solutionCheckBox,
        equilibriumPhasesCheckBox,
        exchangeCheckBox,
        surfaceCheckBox,
        gasPhaseCheckBox,
        solidSolutionsCheckBox,
        kineticsCheckBox
    };
   
   //---------------------------------------------------------------------------
   // Constructor
   //---------------------------------------------------------------------------
    public vs2ChemistryClassDialog(String title, vs2ModelOptions modelOptions, boolean doEdit)
    {
        super(title, true, modelOptions);
        this.doEdit = doEdit;
        this.modelOptions = modelOptions;
        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "flowProperties", null);
    }
    
    /**
     * Show the dialog box
     */
    public boolean doModal() {
        // If we are editing data, then put the current data in the text fields
        if (doEdit) {
            colorLabel.setColor((Color) aRow[1]);
            nameTextField.setText((String) aRow[2]);
            SetTextFieldsForTransport();
        }

        // Request focus for the color button when the dialog box opens
        colorButton.requestFocus();

        // Call superclass method to show the dialog box and return result
        // when the dialog box closes
        return super.doModal();
    }    

    //---------------------------------------------------------------------------
    // Make the color button
    //---------------------------------------------------------------------------
    protected void MakeColorButton()
    {
        JPanel topPanel = new JPanel(true);
        topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
        topPanel.setBorder(new EmptyBorder(20, 0, 0, 0));
        getContentPane().add(topPanel, BorderLayout.NORTH);        
        
        JPanel colorPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0), true);        
        colorPanel.add(colorButton = new JButton("Select Color"));
        colorButton.addActionListener(
                new ActionListener() {
                    public void actionPerformed(ActionEvent e){
                        OnSelectColor();
                    }
                }
        );
        colorButton.addKeyListener(
                new KeyAdapter() {
                    public void keyPressed(KeyEvent e){
                        if (e.getKeyCode() == KeyEvent.VK_ENTER) OnSelectColor();
                    }
                }
        );       
        colorPanel.add(colorLabel = new ColorLabel(Color.white));
        
        JPanel namePanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0), true);
        namePanel.add(new JLabel("Name"));
        namePanel.add(nameTextField = new JTextField(10));

        topPanel.add(colorPanel);
        topPanel.add(Box.createVerticalStrut(20));
        topPanel.add(namePanel);
    }

    //---------------------------------------------------------------------------
    // Make components for text fields for transport parameters
    //---------------------------------------------------------------------------
    protected void MakeContentsForTransport(JPanel subPanel, JPanel hydrLeftPanel, JPanel hydrRightPanel, int hydraulicRows) {
        final int MIN_SOLUTION_ROWS = 10;
        if (modelOptions.doEnergyTransport) {
            if (modelOptions.doSoluteTransport) {
                MakeContentsForHeatTransport(subPanel, hydrLeftPanel, hydrRightPanel, Math.max(hydraulicRows, MIN_SOLUTION_ROWS));
            } else {
                MakeContentsForHeatTransport(subPanel, hydrLeftPanel, hydrRightPanel, hydraulicRows);
            }
        }
        if (modelOptions.doSoluteTransport) {
            MakeContentsForSoluteTransport(subPanel, hydrLeftPanel, hydrRightPanel, hydraulicRows);
        }
    }
      
    protected void MakeContentsForHeatTransport(JPanel subPanel, JPanel hydrLeftPanel,
            JPanel hydrRightPanel, int hydraulicRows)
    {
    }
   
    protected void MakeContentsForSoluteTransport(JPanel subPanel, JPanel hydrLeftPanel,
            JPanel hydrRightPanel, int hydraulicRows)
    {
        // Create a penal for transport properties
        JPanel transportPanel = new JPanel(false);
        transportPanel.setBorder(new CompoundBorder(
            BorderFactory.createTitledBorder("Solute transport properties"),
            new EmptyBorder(4, 10, 10, 10)));        
        transportPanel.setLayout(new BoxLayout(transportPanel, BoxLayout.X_AXIS));
        subPanel.add(transportPanel);

        // Within the transport panel, make a left panel to hold the labels
        JPanel transLeftPanel = new JPanel(false);
        transLeftPanel.setLayout(new GridLayout(0, 1, 0, 10));
        transportPanel.add(transLeftPanel);

        // Put space between the left and right panels
        transportPanel.add(Box.createHorizontalStrut(20));

        // Make a right panel to hold the text fields
        JPanel transCenterPanel = new JPanel(false);
        transCenterPanel.setLayout(new GridLayout(0, 1, 0, 10));
        transportPanel.add(transCenterPanel);
        
        // Put space between the center and right panels
        transportPanel.add(Box.createHorizontalStrut(10));
        
        // Make a right panel to hold the text fields
        JPanel transRightPanel = new JPanel(false);
        transRightPanel.setLayout(new GridLayout(0, 1, 0, 7));
        transportPanel.add(transRightPanel);

        // Initialize the transport row count
        int transportRows = 0;

        transLeftPanel.add(new JLabel("Solution", SwingConstants.RIGHT));
        transCenterPanel.add(solutionTextField = new JTextField(5));
        transRightPanel.add(solutionCheckBox = new JCheckBox("As is"));
        solutionCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                solutionTextField.setEnabled(!solutionCheckBox.isSelected());                
            }
        }
        );     
        ++transportRows;

        transLeftPanel.add(new JLabel("Equilibrium phase", SwingConstants.RIGHT));
        transCenterPanel.add(equilibriumPhasesTextField = new JTextField(5));
        transRightPanel.add(equilibriumPhasesCheckBox = new JCheckBox("As is"));
        equilibriumPhasesCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                equilibriumPhasesTextField.setEnabled(!equilibriumPhasesCheckBox.isSelected());                
            }
        }
        );     
        ++transportRows;

        transLeftPanel.add(new JLabel("Exchange", SwingConstants.RIGHT));
        transCenterPanel.add(exchangeTextField = new JTextField(5));
        transRightPanel.add(exchangeCheckBox = new JCheckBox("As is"));
        exchangeCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                exchangeTextField.setEnabled(!exchangeCheckBox.isSelected());                
            }
        }
        );     
        ++transportRows;
        
        transLeftPanel.add(new JLabel("Surface", SwingConstants.RIGHT));
        transCenterPanel.add(surfaceTextField = new JTextField(5));
        transRightPanel.add(surfaceCheckBox = new JCheckBox("As is"));
        surfaceCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                surfaceTextField.setEnabled(!surfaceCheckBox.isSelected());                
            }
        }
        );     
        ++transportRows;

        transLeftPanel.add(new JLabel("Gas phase", SwingConstants.RIGHT));
        transCenterPanel.add(gasPhaseTextField = new JTextField(5));
        transRightPanel.add(gasPhaseCheckBox = new JCheckBox("As is"));
        gasPhaseCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                gasPhaseTextField.setEnabled(!gasPhaseCheckBox.isSelected());                
            }
        }
        );     
        ++transportRows;
        
        transLeftPanel.add(new JLabel("Solid solutions", SwingConstants.RIGHT));
        transCenterPanel.add(solidSolutionsTextField = new JTextField(5));
        transRightPanel.add(solidSolutionsCheckBox = new JCheckBox("As is"));
        solidSolutionsCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                solidSolutionsTextField.setEnabled(!solidSolutionsCheckBox.isSelected());                
            }
        }
        );     
        ++transportRows;
        
        transLeftPanel.add(new JLabel("Kinetics", SwingConstants.RIGHT));
        transCenterPanel.add(kineticsTextField = new JTextField(5));
        transRightPanel.add(kineticsCheckBox = new JCheckBox("As is"));
        kineticsCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                kineticsTextField.setEnabled(!kineticsCheckBox.isSelected());                
            }
        }
        );     
        ++transportRows;
        
        if (hydraulicRows > transportRows) {
            for (int i = 0; i < hydraulicRows - transportRows; i++) {
                transLeftPanel.add(new JLabel(" "));
                transCenterPanel.add(new JLabel(" "));
                transRightPanel.add(new JLabel(" "));
            }
        } else {
            for (int i = 0; i < transportRows - hydraulicRows; i++) {
                hydrLeftPanel.add(new JLabel(" "));
                hydrRightPanel.add(new JLabel(" "));
            }
        }
    }

    //--------------------------------------------------------------------------
    // Put values in text fields for transport parameters
    //--------------------------------------------------------------------------
    protected void SetTextFieldsForTransport()
    {
        for (int i = 0; i < textFields.length; ++i) {
            if (vs2ChemistryClassData.isAsIs(aRow[i+3])) {
                checkBoxes[i].setSelected(true);                
            } else {
                textFields[i].setText(String.valueOf(((Integer)aRow[i+3]).intValue()));
            }
        }
    }
   
    /**
     * Override the onBrowserHelp() method of the super class
     */
    protected void onBrowserHelp()
    {
        mp2HelpWindow.showHelpFile ("flowProperties.html");
    }


    //---------------------------------------------------------------------------
    // Display color chooser for user to select color
    //---------------------------------------------------------------------------
    protected void OnSelectColor()
    {
         Color result = JColorChooser.showDialog(this, "Select Color", colorLabel.getColor());
         if (result == null) return;
         colorLabel.setColor(result);
         getContentPane().repaint();
         if (!doEdit) nameTextField.requestFocus();
    }

    /**
     * Retrieve data from dialog box components
     */
    protected boolean retrieveData()
    {
        // Check to make sure that there are no errors
        // Don't allow white as a color
        if (colorLabel.getColor() == Color.white) {
            mp2MessageBox.showMessageDialog(parent, "Please select a color that is not white.", "Input error");
            return false;
        }
        
        int[] nums = new int[7];
        try {
            for (int i = 0; i < textFields.length; ++i) {
                if (checkBoxes[i].isSelected()) {
                    nums[i] = vs2ChemistryClassData.AS_IS_VALUE;
                } else {
                    nums[i] = Integer.valueOf(textFields[i].getText()).intValue();
                }
            }
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
                    "Input Error");
            return false;
        }

        String[] keys = {
            "\"Solution\"",
            "\"Equilibrium phase\"",
            "\"Exchange\"",
            "\"Surface\"",
            "\"Gas phase\"",
            "\"Solid solutions\"",
            "\"Kinetics\""
        };
        assert(keys.length == textFields.length);      
        for (int i = 0; i < textFields.length; ++i) {
            if (nums[i] != vs2ChemistryClassData.AS_IS_VALUE) {
                if (!dataCheck(nums[i], keys[i], IS_NON_NEGATIVE_OR_NEGATIVE_ONE, textFields[i])) {
                    return false;
                }
            }
        }
        
        // When execution reaches this point, all input data are OK so we put
        // them in aRow and return true
        aRow[1] = colorLabel.getColor();
        aRow[2] = nameTextField.getText();        
        for (int i = 0; i < textFields.length; ++i) {
            aRow[i+3] = new Integer(nums[i]);
        }
        
        return true;
    }
    
    /**
     * Creates the dialog box contents
     */
    protected void makeContents()
    {
        // get the model options back from the custom object
        modelOptions = (vs2ModelOptions) customObject;

        // Call the superclass method to make the color button
        MakeColorButton();
        
        
        // Create a center panel to hold labels, text fields, and table
        JPanel centerPanel = new JPanel(false);
        centerPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        // Create a sub panel to hold hydraulic and transport properties
        JPanel subPanel = new JPanel(false);
        subPanel.setLayout(new GridLayout(1, 3, 20, 0));
        centerPanel.add(subPanel);

        // Create panels for hydraulic properties and for transport properties
        JPanel hydraulicPanel = new JPanel(false);

        // Within the hydraulic panel, make a left panel to hold the labels
        JPanel hydrLeftPanel = new JPanel(false);
        hydrLeftPanel.setLayout(new GridLayout(0, 1, 0, 10));
        hydraulicPanel.add(hydrLeftPanel);

        // Put space between the left and right panels
        hydraulicPanel.add(Box.createHorizontalStrut(20));

        // Make a right panel to hold the text fields
        JPanel hydrRightPanel = new JPanel(false);
        hydrRightPanel.setLayout(new GridLayout(0, 1, 0, 10));
        hydraulicPanel.add(hydrRightPanel);

        // Initialize the hydraulic row count
        int hydraulicRows = 0;
        MakeContentsForSoluteTransport(subPanel, hydrLeftPanel, hydrRightPanel, hydraulicRows);
    }
    
    //---------------------------------------------------------------------------
    // Inner class to define a color label.
    //---------------------------------------------------------------------------
    class ColorLabel extends JLabel
    {
        Color color;

        ColorLabel(Color color)
        {
           super(new mp2ColorRectIcon(color, 100, 20));
           this.color = color;
        }

        public void setColor(Color color)
        {
           this.color = color;
           this.setIcon(new mp2ColorRectIcon(color, 100, 20));
        }

        public Color getColor()
        {
           return color;
        }
    }
}

