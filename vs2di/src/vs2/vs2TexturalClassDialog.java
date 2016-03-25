/*
 * vs2TexturalClassDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public abstract class vs2TexturalClassDialog extends mp2Dialog implements vs2Constants,
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
   protected JTextField longDispTextField;
   protected JTextField transDispTextField;
   protected JTextField molDiffusionTextField;
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
   protected void MakeContentsForTransport(JPanel subPanel, JPanel hydrLeftPanel,
       JPanel hydrRightPanel, int hydraulicRows) { 
       if (vs2App.doHeat()) {
           MakeContentsForHeatTransport(subPanel, hydrLeftPanel, hydrRightPanel, hydraulicRows);
       } else {
           MakeContentsForSoluteTransport(subPanel, hydrLeftPanel, hydrRightPanel, hydraulicRows);
       }
   }
   
      
   protected void MakeContentsForHeatTransport(JPanel subPanel, JPanel hydrLeftPanel,
        JPanel hydrRightPanel, int hydraulicRows) { 
      // Create a penal for transport properties
      JPanel transportPanel = new JPanel(false);
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

      // Use horizontal struct to control  spacing
      transLeftPanel.add(Box.createHorizontalStrut(80));
      transRightPanel.add(Box.createHorizontalStrut(80));
      transportRows++;

      // add labels and text fields for heat transport parameters
      transLeftPanel.add(new JLabel("Long. disp.", SwingConstants.RIGHT));
      transRightPanel.add(longDispTextField = new JTextField(5));

      transLeftPanel.add(new JLabel("Trans. disp.", SwingConstants.RIGHT));
      transRightPanel.add(transDispTextField = new JTextField(5));

      transLeftPanel.add(new JLabel("Cs", SwingConstants.RIGHT));
      transRightPanel.add(CsTextField = new JTextField(5));

      transLeftPanel.add(new JLabel("KTr", SwingConstants.RIGHT));
      transRightPanel.add(KTrTextField = new JTextField(5));

      transLeftPanel.add(new JLabel("KTs", SwingConstants.RIGHT));
      transRightPanel.add(KTsTextField = new JTextField(5));

      transLeftPanel.add(new JLabel("Cw", SwingConstants.RIGHT));
      transRightPanel.add(CwTextField = new JTextField(5));
      
      transportRows += 6;

      if (hydraulicRows > transportRows)
      {
         for (int i=0; i<hydraulicRows-transportRows; i++)
         {
            transLeftPanel.add(new JLabel(" "));
            transRightPanel.add(new JLabel(" "));
         }
      }
      else
      {
         for (int i=0; i<transportRows-hydraulicRows; i++)
         {
            hydrLeftPanel.add(new JLabel(" "));
            hydrRightPanel.add(new JLabel(" "));
         }
      }
   }

   protected void MakeContentsForSoluteTransport(JPanel subPanel, JPanel hydrLeftPanel,
        JPanel hydrRightPanel, int hydraulicRows) { 
      // Create a penal for transport properties
      JPanel transportPanel = new JPanel(false);
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

      // Use horizontal struct to control  spacing
      transLeftPanel.add(Box.createHorizontalStrut(80));
      transRightPanel.add(Box.createHorizontalStrut(80));
      transportRows++;

      // add labels and text fields for transport parameters
      transLeftPanel.add(new JLabel("Long. disp.", SwingConstants.RIGHT));
      transRightPanel.add(longDispTextField = new JTextField(5));

      transLeftPanel.add(new JLabel("Trans. disp.", SwingConstants.RIGHT));
      transRightPanel.add(transDispTextField = new JTextField(5));

      transLeftPanel.add(new JLabel("Coef. mol. diff.", SwingConstants.RIGHT));
      transRightPanel.add(molDiffusionTextField = new JTextField(5));

      transLeftPanel.add(new JLabel("Decay const.", SwingConstants.RIGHT));
      transRightPanel.add(decayTextField = new JTextField(5));

      transportRows += 4;
      switch(modelOptions.reactionOption)
      {
      case LINEAR_ADSORPTION:
         transLeftPanel.add(new JLabel("bulk density", SwingConstants.RIGHT));
         transRightPanel.add(chem1TextField = new JTextField(5));

         transLeftPanel.add(new JLabel("Kd", SwingConstants.RIGHT));
         transRightPanel.add(chem2TextField = new JTextField(5));

         transportRows += 2;
         break;
      case LANGMUIR:
         transLeftPanel.add(new JLabel("bulk density", SwingConstants.RIGHT));
         transRightPanel.add(chem1TextField = new JTextField(5));

         transLeftPanel.add(new JLabel("K1 (Langmuir)", SwingConstants.RIGHT));
         transRightPanel.add(chem2TextField = new JTextField(5));

         transLeftPanel.add(new JLabel("Q (Langmuir)", SwingConstants.RIGHT));
         transRightPanel.add(chem3TextField = new JTextField(5));

         transportRows += 3;
         break;
      case FREUNDLICH:
         transLeftPanel.add(new JLabel("bulk density", SwingConstants.RIGHT));
         transRightPanel.add(chem1TextField = new JTextField(5));

         transLeftPanel.add(new JLabel("Kf (Freundlich)", SwingConstants.RIGHT));
         transRightPanel.add(chem2TextField = new JTextField(5));

         transLeftPanel.add(new JLabel("n (Freundlich)", SwingConstants.RIGHT));
         transRightPanel.add(chem3TextField = new JTextField(5));

         transportRows += 3;
         break;
      case MONO_MONOVALENT_ION_EXCHANGE:
      case MONO_DIVALENT_ION_EXCHANGE:
      case DI_MONOVALENT_ION_EXCHANGE:
      case DI_DIVALENT_ION_EXCHANGE:
         transLeftPanel.add(new JLabel("bulk density", SwingConstants.RIGHT));
         transRightPanel.add(chem1TextField = new JTextField(5));

         transLeftPanel.add(new JLabel("Km (ion exch.)", SwingConstants.RIGHT));
         transRightPanel.add(chem2TextField = new JTextField(5));

         transLeftPanel.add(new JLabel("Q^ (ion exch.)", SwingConstants.RIGHT));
         transRightPanel.add(chem3TextField = new JTextField(5));

         transLeftPanel.add(new JLabel("C0 (ion exch.)", SwingConstants.RIGHT));
         transRightPanel.add(chem4TextField = new JTextField(5));

         transportRows += 4;
         break;
      }

      if (hydraulicRows > transportRows)
      {
         for (int i=0; i<hydraulicRows-transportRows; i++)
         {
            transLeftPanel.add(new JLabel(" "));
            transRightPanel.add(new JLabel(" "));
         }
      }
      else
      {
         for (int i=0; i<transportRows-hydraulicRows; i++)
         {
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
      longDispTextField.setText(String.valueOf(((Double) aRow[21]).doubleValue()));
      transDispTextField.setText(String.valueOf(((Double) aRow[22]).doubleValue()));
      if (vs2App.doHeat()) {
            CsTextField.setText(String.valueOf(((Double) aRow[34]).doubleValue()));
            KTrTextField.setText(String.valueOf(((Double) aRow[35]).doubleValue()));
            KTsTextField.setText(String.valueOf(((Double) aRow[36]).doubleValue()));
            CwTextField.setText(String.valueOf(((Double) aRow[37]).doubleValue()));
      } else {
        molDiffusionTextField.setText(String.valueOf(((Double) aRow[23]).doubleValue()));
        decayTextField.setText(String.valueOf(((Double) aRow[24]).doubleValue()));
        switch(modelOptions.reactionOption)
        {
        case LINEAR_ADSORPTION:
           chem1TextField.setText(String.valueOf(((Double) aRow[25]).doubleValue()));
           chem2TextField.setText(String.valueOf(((Double) aRow[26]).doubleValue()));
           break;
        case LANGMUIR:
           chem1TextField.setText(String.valueOf(((Double) aRow[25]).doubleValue()));
           chem2TextField.setText(String.valueOf(((Double) aRow[27]).doubleValue()));
           chem3TextField.setText(String.valueOf(((Double) aRow[28]).doubleValue()));
           break;
        case FREUNDLICH:
           chem1TextField.setText(String.valueOf(((Double) aRow[25]).doubleValue()));
           chem2TextField.setText(String.valueOf(((Double) aRow[29]).doubleValue()));
           chem3TextField.setText(String.valueOf(((Double) aRow[30]).doubleValue()));
           break;
        case MONO_MONOVALENT_ION_EXCHANGE:
        case MONO_DIVALENT_ION_EXCHANGE:
        case DI_MONOVALENT_ION_EXCHANGE:
        case DI_DIVALENT_ION_EXCHANGE:
           chem1TextField.setText(String.valueOf(((Double) aRow[25]).doubleValue()));
           chem2TextField.setText(String.valueOf(((Double) aRow[31]).doubleValue()));
           chem3TextField.setText(String.valueOf(((Double) aRow[32]).doubleValue()));
           chem4TextField.setText(String.valueOf(((Double) aRow[33]).doubleValue()));
           break;
        }
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
   protected boolean RetrieveDataForTransport()
   {
      double aL=0, aT=0, md=0, decay=0, chem1=0, chem2=0, chem3=0, chem4=0;
      double Cs=0, KTr=0, KTs=0, Cw=0;;
      try
      {
         aL = Double.valueOf(longDispTextField.getText()).doubleValue();
         aT = Double.valueOf(transDispTextField.getText()).doubleValue();
         if (vs2App.doHeat()) {
            Cs = Double.valueOf(CsTextField.getText()).doubleValue();
            KTr = Double.valueOf(KTrTextField.getText()).doubleValue();
            KTs = Double.valueOf(KTsTextField.getText()).doubleValue();
            Cw = Double.valueOf(CwTextField.getText()).doubleValue();
         } else {
            md = Double.valueOf(molDiffusionTextField.getText()).doubleValue();
            decay = Double.valueOf(decayTextField.getText()).doubleValue();
            switch(modelOptions.reactionOption)
            {
            case LINEAR_ADSORPTION:
               chem1 = Double.valueOf(chem1TextField.getText()).doubleValue();
               chem2 = Double.valueOf(chem2TextField.getText()).doubleValue();
               break;
            case LANGMUIR:
               chem1 = Double.valueOf(chem1TextField.getText()).doubleValue();
               chem2 = Double.valueOf(chem2TextField.getText()).doubleValue();
               chem3 = Double.valueOf(chem3TextField.getText()).doubleValue();
               break;
            case FREUNDLICH:
               chem1 = Double.valueOf(chem1TextField.getText()).doubleValue();
               chem2 = Double.valueOf(chem2TextField.getText()).doubleValue();
               chem3 = Double.valueOf(chem3TextField.getText()).doubleValue();
               break;
            case MONO_MONOVALENT_ION_EXCHANGE:
            case MONO_DIVALENT_ION_EXCHANGE:
            case DI_MONOVALENT_ION_EXCHANGE:
            case DI_DIVALENT_ION_EXCHANGE:
               chem1 = Double.valueOf(chem1TextField.getText()).doubleValue();
               chem2 = Double.valueOf(chem2TextField.getText()).doubleValue();
               chem3 = Double.valueOf(chem3TextField.getText()).doubleValue();
               chem4 = Double.valueOf(chem4TextField.getText()).doubleValue();
               break;
            }
         }
      }
      catch (NumberFormatException e)
      {
         mp2MessageBox.showMessageDialog("Please check your input.",
            "Input Error");
         return false;
      }

      if (!dataCheck(aL,    "\"long. disp.\"",    IS_NON_NEGATIVE, longDispTextField))
         return false;
      if (!dataCheck(aT,    "\"trans. disp.\"",   IS_NON_NEGATIVE, transDispTextField))
         return false;
      
      if (vs2App.doHeat()) {
            if (!dataCheck(Cs,  "\"Cs\"", IS_NON_NEGATIVE, CsTextField))
            return false;
            if (!dataCheck(KTr,  "\"KTr\"", IS_NON_NEGATIVE, KTrTextField))
            return false;
            if (!dataCheck(KTs,  "\"KTs\"", IS_NON_NEGATIVE, KTsTextField))
            return false;
            if (!dataCheck(Cw,  "\"Cw\"", IS_NON_NEGATIVE, CwTextField))
            return false;
      } else {
        if (!dataCheck(md,  "\"coef. mol. diff.\"", IS_NON_NEGATIVE, molDiffusionTextField))
           return false;
        if (!dataCheck(decay, "\"decay const.\"",   IS_NON_NEGATIVE, decayTextField))
           return false;
        switch(modelOptions.reactionOption)
        {
        case LINEAR_ADSORPTION:
           if (!dataCheck(chem1, "\"solid density\"", IS_POSITIVE, chem1TextField))
              return false;
           if (!dataCheck(chem2,        "\"Kd\"",     IS_NON_NEGATIVE, chem2TextField))
              return false;
           break;
        case LANGMUIR:
           if (!dataCheck(chem1, "\"solid density\"", IS_POSITIVE, chem1TextField))
              return false;
           if (!dataCheck(chem2, "\"K1 (Langmuir)\"", IS_NON_NEGATIVE, chem2TextField))
              return false;
           if (!dataCheck(chem3, "\"Q (Langmuir)\"",  IS_NON_NEGATIVE, chem3TextField))
              return false;
           break;
        case FREUNDLICH:
           if (!dataCheck(chem1, "\"solid density\"",   IS_POSITIVE, chem1TextField))
              return false;
           if (!dataCheck(chem2, "\"Kf (Freundlich)\"", IS_NON_NEGATIVE, chem2TextField))
              return false;
           if (!dataCheck(chem3, "\"n (Freundlich)\"",  IS_NON_NEGATIVE, chem3TextField))
              return false;
           break;
        case MONO_MONOVALENT_ION_EXCHANGE:
        case MONO_DIVALENT_ION_EXCHANGE:
        case DI_MONOVALENT_ION_EXCHANGE:
        case DI_DIVALENT_ION_EXCHANGE:
           if (!dataCheck(chem1, "\"solid density\"",  IS_POSITIVE, chem1TextField))
              return false;
           if (!dataCheck(chem2, "\"Km (ion exch.)\"", IS_NON_NEGATIVE, chem2TextField))
              return false;
           if (!dataCheck(chem3, "\"Q^ (ion exch.)\"", IS_NON_NEGATIVE, chem3TextField))
              return false;
           if (!dataCheck(chem4, "\"C0 (ion exch.)\"", IS_NON_NEGATIVE, chem4TextField))
              return false;
           break;
        }
      }

      // When execution reaches this point, all transport input data are OK so we put
      // them in aRow and return true
      aRow[21] = new Double(aL);
      aRow[22] = new Double(aT);
      if (vs2App.doHeat()) {
          aRow[34] = new Double(Cs);
          aRow[35] = new Double(KTr);
          aRow[36] = new Double(KTs);
          aRow[37] = new Double(Cw);
      } else {
          aRow[23] = new Double(md);
          aRow[24] = new Double(decay);
          switch(modelOptions.reactionOption)
          {
          case LINEAR_ADSORPTION:
             aRow[25] = new Double(chem1);
             aRow[26] = new Double(chem2);
             break;
          case LANGMUIR:
             aRow[25] = new Double(chem1);
             aRow[27] = new Double(chem2);
             aRow[28] = new Double(chem3);
             break;
          case FREUNDLICH:
             aRow[25] = new Double(chem1);
             aRow[29] = new Double(chem2);
             aRow[30] = new Double(chem3);
             break;
          case MONO_MONOVALENT_ION_EXCHANGE:
          case MONO_DIVALENT_ION_EXCHANGE:
          case DI_MONOVALENT_ION_EXCHANGE:
          case DI_DIVALENT_ION_EXCHANGE:
             aRow[25] = new Double(chem1);
             aRow[31] = new Double(chem2);
             aRow[32] = new Double(chem3);
             aRow[33] = new Double(chem4);
             break;
          }
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

