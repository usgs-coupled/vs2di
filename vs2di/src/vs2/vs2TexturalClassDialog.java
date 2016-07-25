/*
 * vs2TexturalClassDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public abstract class vs2TexturalClassDialog extends vs2Dialog implements vs2Constants,
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
   protected JTextField anisotropyTextField;
   protected JTextField satHydrCondTextField;
   protected JTextField specificStorageTextField;
   protected JTextField porosityTextField;
   protected JTextField residualMoistureContentTextField;
   protected JTextField flow1TextField;
   protected JTextField flow2TextField;
   protected JTextField flow3TextField;
   protected JTextField flow4TextField;
   protected JTextField energyLongDispTextField;
   protected JTextField energyTransDispTextField;
   protected JTextField soluteLongDispTextField;
   protected JTextField soluteTransDispTextField;
   protected JTextField molDiffusionTextField;
   protected JTextField solutionTextField;
   protected JTextField equilibriumPhasesTextField;
   protected JTextField exchangeTextField;
   protected JTextField surfaceTextField;
   protected JTextField gasPhaseTextField;
   protected JTextField solidSolutionsTextField;
   protected JTextField kineticsTextField;
   protected JTextField decayTextField;
   protected JTextField chem1TextField;
   protected JTextField chem2TextField;
   protected JTextField chem3TextField;
   protected JTextField chem4TextField;
   protected JTextField CsTextField;
   protected JTextField KTrTextField;
   protected JTextField KTsTextField;
   protected JTextField CwTextField;

   //---------------------------------------------------------------------------
   // Constructor
   //---------------------------------------------------------------------------
	public vs2TexturalClassDialog(String title,
      vs2ModelOptions modelOptions, boolean doEdit)
	{
      super(title, true, modelOptions);
      this.doEdit = doEdit;
      this.modelOptions = modelOptions;
	  mp2JavaHelp.hb.enableHelpOnButton(helpButton, "flowProperties", null);
   }

   //---------------------------------------------------------------------------
   // Make the color button
   //---------------------------------------------------------------------------
   protected void MakeColorButton()
   {
      // Create a top panel to hold the color button and color label
      JPanel topPanel = new JPanel(false);
      topPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 20, 0));
      topPanel.setBorder(new EmptyBorder(20, 0, 0, 0));
      getContentPane().add(topPanel, BorderLayout.NORTH);
      topPanel.add(colorButton = new JButton("Select Color"));
      colorButton.addActionListener(new ActionListener()
         {public void actionPerformed(ActionEvent e){OnSelectColor();}});
      colorButton.addKeyListener(new KeyAdapter()
         {public void keyPressed(KeyEvent e){if (e.getKeyCode() ==
         KeyEvent.VK_ENTER) OnSelectColor();}});
      topPanel.add(colorLabel = new ColorLabel(Color.white));
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
            JPanel hydrRightPanel, int hydraulicRows) {
        // Create a penal for transport properties
        JPanel transportPanel = new JPanel(false);
        transportPanel.setBorder(new CompoundBorder(
            BorderFactory.createTitledBorder("Heat transport properties"),
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
        JPanel transRightPanel = new JPanel(false);
        transRightPanel.setLayout(new GridLayout(0, 1, 0, 10));
        transportPanel.add(transRightPanel);

        // Initialize the transport row count
        int transportRows = 0;

        // add labels and text fields for heat transport parameters
        transLeftPanel.add(new JLabel("Long. disp.", SwingConstants.RIGHT));
        transRightPanel.add(energyLongDispTextField = new JTextField(5));

        transLeftPanel.add(new JLabel("Trans. disp.", SwingConstants.RIGHT));
        transRightPanel.add(energyTransDispTextField = new JTextField(5));

        transLeftPanel.add(new JLabel("Cs", SwingConstants.RIGHT));
        transRightPanel.add(CsTextField = new JTextField(5));

        transLeftPanel.add(new JLabel("KTr", SwingConstants.RIGHT));
        transRightPanel.add(KTrTextField = new JTextField(5));

        transLeftPanel.add(new JLabel("KTs", SwingConstants.RIGHT));
        transRightPanel.add(KTsTextField = new JTextField(5));

        transLeftPanel.add(new JLabel("Cw", SwingConstants.RIGHT));
        transRightPanel.add(CwTextField = new JTextField(5));

        transportRows += 6;

        if (hydraulicRows > transportRows) {
            for (int i = 0; i < hydraulicRows - transportRows; i++) {
                transLeftPanel.add(new JLabel(" "));
                transRightPanel.add(new JLabel(" "));
            }
        } else {
            for (int i = 0; i < transportRows - hydraulicRows; i++) {
                hydrLeftPanel.add(new JLabel(" "));
                hydrRightPanel.add(new JLabel(" "));
            }
        }
    }
   
    protected void MakeContentsForSoluteTransport(JPanel subPanel, JPanel hydrLeftPanel,
            JPanel hydrRightPanel, int hydraulicRows) {
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
        JPanel transRightPanel = new JPanel(false);
        transRightPanel.setLayout(new GridLayout(0, 1, 0, 10));
        transportPanel.add(transRightPanel);

        // Initialize the transport row count
        int transportRows = 0;

        // add labels and text fields for transport parameters
        transLeftPanel.add(new JLabel("Long. disp.", SwingConstants.RIGHT));
        transRightPanel.add(soluteLongDispTextField = new JTextField(5));
        ++transportRows;

        transLeftPanel.add(new JLabel("Trans. disp.", SwingConstants.RIGHT));
        transRightPanel.add(soluteTransDispTextField = new JTextField(5));
        ++transportRows;

        transLeftPanel.add(new JLabel("Coef. mol. diff.", SwingConstants.RIGHT));
        transRightPanel.add(molDiffusionTextField = new JTextField(5));
        ++transportRows;
        
        transLeftPanel.add(new JLabel("Solution", SwingConstants.RIGHT));
        transRightPanel.add(solutionTextField = new JTextField(5));
        ++transportRows;

        transLeftPanel.add(new JLabel("Equilibrium phase", SwingConstants.RIGHT));
        transRightPanel.add(equilibriumPhasesTextField = new JTextField(5));
        ++transportRows;

        transLeftPanel.add(new JLabel("Exchange", SwingConstants.RIGHT));
        transRightPanel.add(exchangeTextField = new JTextField(5));
        ++transportRows;
        
        transLeftPanel.add(new JLabel("Surface", SwingConstants.RIGHT));
        transRightPanel.add(surfaceTextField = new JTextField(5));
        ++transportRows;

        transLeftPanel.add(new JLabel("Gas phase", SwingConstants.RIGHT));
        transRightPanel.add(gasPhaseTextField = new JTextField(5));
        ++transportRows;
        
        transLeftPanel.add(new JLabel("Solid solutions", SwingConstants.RIGHT));
        transRightPanel.add(solidSolutionsTextField = new JTextField(5));
        ++transportRows;
        
        transLeftPanel.add(new JLabel("Kinetics", SwingConstants.RIGHT));
        transRightPanel.add(kineticsTextField = new JTextField(5));
        ++transportRows;
        
        if (hydraulicRows > transportRows) {
            for (int i = 0; i < hydraulicRows - transportRows; i++) {
                transLeftPanel.add(new JLabel(" "));
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
    protected void SetTextFieldsForTransport() {
        if (modelOptions.doEnergyTransport) {
            energyLongDispTextField.setText(String.valueOf(((Double) aRow[21]).doubleValue()));
            energyTransDispTextField.setText(String.valueOf(((Double) aRow[22]).doubleValue()));            
            CsTextField.setText(String.valueOf(((Double) aRow[34]).doubleValue()));
            KTrTextField.setText(String.valueOf(((Double) aRow[35]).doubleValue()));
            KTsTextField.setText(String.valueOf(((Double) aRow[36]).doubleValue()));
            CwTextField.setText(String.valueOf(((Double) aRow[37]).doubleValue()));
        }
        if (modelOptions.doSoluteTransport) {
            soluteLongDispTextField.setText(String.valueOf(((Double) aRow[42]).doubleValue()));
            soluteTransDispTextField.setText(String.valueOf(((Double) aRow[43]).doubleValue()));            
            molDiffusionTextField.setText(String.valueOf(((Double) aRow[23]).doubleValue()));
            
            solutionTextField.setText(String.valueOf(((Integer)aRow[44]).intValue()));
            equilibriumPhasesTextField.setText(String.valueOf(((Integer)aRow[45]).intValue()));
            exchangeTextField.setText(String.valueOf(((Integer)aRow[46]).intValue()));
            surfaceTextField.setText(String.valueOf(((Integer)aRow[47]).intValue()));
            gasPhaseTextField.setText(String.valueOf(((Integer)aRow[48]).intValue()));
            solidSolutionsTextField.setText(String.valueOf(((Integer)aRow[49]).intValue()));
            kineticsTextField.setText(String.valueOf(((Integer)aRow[50]).intValue()));
        }
    }
   
    /**
     * Override the onBrowserHelp() method of the super class
     */
    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile ("flowProperties.html");
    }


	//---------------------------------------------------------------------------
   // Display color chooser for user to select color
	//---------------------------------------------------------------------------
   protected void OnSelectColor()
   {
		Color result = JColorChooser.showDialog(this, "Select Color",
         colorLabel.getColor());
      if (result == null) return;
      colorLabel.setColor(result);
      getContentPane().repaint();
      if (!doEdit) nameTextField.requestFocus();
   }

    //---------------------------------------------------------------------------
    // Retrieve Data for transport
    //---------------------------------------------------------------------------
    protected boolean RetrieveDataForTransport() {
        double aL = 0, aT = 0, md = 0;
        double Cs = 0, KTr = 0, KTs = 0, Cw = 0;
        double aLS = 0, aTS = 0;
        int so = -1, ep = -1, ex = -1, su = -1, gp = -1, ss = -1, ki = -1;
        try {
            if (modelOptions.doEnergyTransport) {
                aL  = Double.valueOf(energyLongDispTextField.getText()).doubleValue();
                aT  = Double.valueOf(energyTransDispTextField.getText()).doubleValue();
                Cs  = Double.valueOf(CsTextField.getText()).doubleValue();
                KTr = Double.valueOf(KTrTextField.getText()).doubleValue();
                KTs = Double.valueOf(KTsTextField.getText()).doubleValue();
                Cw  = Double.valueOf(CwTextField.getText()).doubleValue();
            }
            if (modelOptions.doSoluteTransport) {
                aLS = Double.valueOf(soluteLongDispTextField.getText()).doubleValue();
                aTS = Double.valueOf(soluteTransDispTextField.getText()).doubleValue();
                md  = Double.valueOf(molDiffusionTextField.getText()).doubleValue();
                so  = Integer.valueOf(solutionTextField.getText()).intValue();
                ep  = Integer.valueOf(equilibriumPhasesTextField.getText()).intValue();
                ex  = Integer.valueOf(exchangeTextField.getText()).intValue();
                su  = Integer.valueOf(surfaceTextField.getText()).intValue();
                gp  = Integer.valueOf(gasPhaseTextField.getText()).intValue();
                ss  = Integer.valueOf(solidSolutionsTextField.getText()).intValue();
                ki  = Integer.valueOf(kineticsTextField.getText()).intValue();
                
            }
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input.",
                    "Input Error");
            return false;
        }

        if (modelOptions.doEnergyTransport) {
            if (!dataCheck(aL, "\"long. disp.\"", IS_NON_NEGATIVE, energyLongDispTextField)) {
                return false;
            }
            if (!dataCheck(aT, "\"trans. disp.\"", IS_NON_NEGATIVE, energyTransDispTextField)) {
                return false;
            }
            if (!dataCheck(Cs, "\"Cs\"", IS_NON_NEGATIVE, CsTextField)) {
                return false;
            }
            if (!dataCheck(KTr, "\"KTr\"", IS_NON_NEGATIVE, KTrTextField)) {
                return false;
            }
            if (!dataCheck(KTs, "\"KTs\"", IS_NON_NEGATIVE, KTsTextField)) {
                return false;
            }
            if (!dataCheck(Cw, "\"Cw\"", IS_NON_NEGATIVE, CwTextField)) {
                return false;
            }
        }
        if (modelOptions.doSoluteTransport) {
            if (!dataCheck(aLS, "\"long. disp.\"", IS_NON_NEGATIVE, soluteLongDispTextField)) {
                return false;
            }
            if (!dataCheck(aTS, "\"trans. disp.\"", IS_NON_NEGATIVE, soluteTransDispTextField)) {
                return false;
            }
            if (!dataCheck(md, "\"coef. mol. diff.\"", IS_NON_NEGATIVE, molDiffusionTextField)) {
                return false;
            }            
            if (!dataCheck(so, "\"Solution\"", IS_NON_NEGATIVE_OR_NEGATIVE_ONE, solutionTextField)) {
                return false;
            }           
            if (!dataCheck(ep, "\"Equilibrium phase\"", IS_NON_NEGATIVE_OR_NEGATIVE_ONE, equilibriumPhasesTextField)) {
                return false;
            }           
            if (!dataCheck(ex, "\"Exchange\"", IS_NON_NEGATIVE_OR_NEGATIVE_ONE, exchangeTextField)) {
                return false;
            }           
            if (!dataCheck(su, "\"Surface\"", IS_NON_NEGATIVE_OR_NEGATIVE_ONE, surfaceTextField)) {
                return false;
            }           
            if (!dataCheck(gp, "\"Gas phase\"", IS_NON_NEGATIVE_OR_NEGATIVE_ONE, gasPhaseTextField)) {
                return false;
            }           
            if (!dataCheck(ss, "\"Solid solutions\"", IS_NON_NEGATIVE_OR_NEGATIVE_ONE, solidSolutionsTextField)) {
                return false;
            }           
            if (!dataCheck(ki, "\"Kinetics\"", IS_NON_NEGATIVE_OR_NEGATIVE_ONE, kineticsTextField)) {
                return false;
            }           
        }

        // When execution reaches this point, all transport input data are OK so we put
        // them in aRow and return true
        if (modelOptions.doEnergyTransport) {
            aRow[21] = new Double(aL);
            aRow[22] = new Double(aT);            
            aRow[34] = new Double(Cs);
            aRow[35] = new Double(KTr);
            aRow[36] = new Double(KTs);
            aRow[37] = new Double(Cw);
        }
        if (modelOptions.doSoluteTransport) {
            aRow[42] = new Double(aLS);
            aRow[43] = new Double(aTS);
            aRow[23] = new Double(md);
            aRow[44] = new Integer(so);
            aRow[45] = new Integer(ep);
            aRow[46] = new Integer(ex);
            aRow[47] = new Integer(su);
            aRow[48] = new Integer(gp);
            aRow[49] = new Integer(ss);
            aRow[50] = new Integer(ki);
        }
        return true;
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

