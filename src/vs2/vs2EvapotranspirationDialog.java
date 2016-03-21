/*
 * vs2EvapotranspirationDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2EvapotranspirationDialog extends mp2Dialog
{
   //---------------------------------------------------------------------------
   // Fields
   //---------------------------------------------------------------------------
   // A row of evapotranspiration period data
   public Object [] aRow;

   // model options
   protected vs2ModelOptions modelOptions;

   // Boolean flag that is true if this dialog box is for editing data, and
   // false if the dialog box is for adding data
   protected boolean doEdit;

   // Components in the dialog box
   protected JTextField potentialEvaporationTextField;
   protected JTextField surfaceResistanceTextField;
   protected JTextField pressurePotentialAtmosphereTextField;
   protected JTextField potentialTranspirationTextField;
   protected JTextField rootDepthTextField;
   protected JTextField rootBaseActivityTextField;
   protected JTextField rootTopActivityTextField;
   protected JTextField rootPressureHeadTextField;

   //---------------------------------------------------------------------------
   // Constructor
   //---------------------------------------------------------------------------
	public vs2EvapotranspirationDialog(String title,
      vs2ModelOptions modelOptions, boolean doEdit)
	{
      super(title, true, modelOptions);
      this.doEdit = doEdit;
      if (modelOptions.doEvaporation) {
	      mp2JavaHelp.hb.enableHelpOnButton(helpButton, "evaporationParameters", null);
      } else if (modelOptions.doTranspiration) {
	      mp2JavaHelp.hb.enableHelpOnButton(helpButton, "transpirationParameters", null);
      }
   }

   //---------------------------------------------------------------------------
   // Implementation of abstract method in superclass.
   // Add components to the center of the dialog box
   //---------------------------------------------------------------------------
   protected void makeContents()
   {
      // get the model options back from the custom object
      modelOptions = (vs2ModelOptions) customObject;

      // Make a center panel to hold all the components.
      JPanel centerPanel = new JPanel(false);
      centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.X_AXIS));
      centerPanel.setBorder(new EmptyBorder(25, 25, 25, 25));
      getContentPane().add(centerPanel, BorderLayout.CENTER);

      // Within the center panel, make a left panel to hold the labels
      JPanel leftPanel = new JPanel(false);
      leftPanel.setLayout(new GridLayout(0, 1, 0, 10));
      centerPanel.add(leftPanel);

      // Put space between the left and right panels
      centerPanel.add(Box.createHorizontalStrut(20));

      // Make a right panel to hold the text fields
      JPanel rightPanel = new JPanel(false);
      rightPanel.setLayout(new GridLayout(0, 1, 0, 10));
      centerPanel.add(rightPanel);

      // Add the labels and text fields to left and right panels respectively
      if (modelOptions.doEvaporation)
      {
         leftPanel.add(new JLabel("Potential evaporation", SwingConstants.RIGHT));
         rightPanel.add(potentialEvaporationTextField = new JTextField(5));

         leftPanel.add(new JLabel("Surface resistance", SwingConstants.RIGHT));
         rightPanel.add(surfaceResistanceTextField = new JTextField(5));

         leftPanel.add(new JLabel("Pressure potential of atmosphere",
            SwingConstants.RIGHT));
         rightPanel.add(pressurePotentialAtmosphereTextField = new JTextField(5));
      }
      if (modelOptions.doTranspiration)
      {
         leftPanel.add(new JLabel("Potential transpiration", SwingConstants.RIGHT));
         rightPanel.add(potentialTranspirationTextField = new JTextField(5));

         leftPanel.add(new JLabel("Rooting depth", SwingConstants.RIGHT));
         rightPanel.add(rootDepthTextField = new JTextField(5));

         leftPanel.add(new JLabel("Activity at root base", SwingConstants.RIGHT));
         rightPanel.add(rootBaseActivityTextField = new JTextField(5));

         leftPanel.add(new JLabel("Activity at root top", SwingConstants.RIGHT));
         rightPanel.add(rootTopActivityTextField = new JTextField(5));

         leftPanel.add(new JLabel("Pressure head in root", SwingConstants.RIGHT));
         rightPanel.add(rootPressureHeadTextField = new JTextField(5));
      }
   }

   //--------------------------------------------------------------------------
   // Override superclass method.
   // If editing evapotranspiration period data, put current values into text
   // field. Then show dialog box.
   //--------------------------------------------------------------------------
   public boolean doModal()
   {
      // If we are editing data, then put the current data in the text fields
      if (doEdit)
      {
         if (modelOptions.doEvaporation)
         {
            potentialEvaporationTextField.setText(String.valueOf(
               ((Double) aRow[1]).doubleValue()));
            surfaceResistanceTextField.setText(String.valueOf(
               ((Double) aRow[2]).doubleValue()));
            pressurePotentialAtmosphereTextField.setText(String.valueOf(
               ((Double) aRow[3]).doubleValue()));
         }
         if (modelOptions.doTranspiration)
         {
            potentialTranspirationTextField.setText(String.valueOf(
               ((Double) aRow[4]).doubleValue()));
            rootDepthTextField.setText(String.valueOf(
               ((Double) aRow[5]).doubleValue()));
            rootBaseActivityTextField.setText(String.valueOf(
               ((Double) aRow[6]).doubleValue()));
            rootTopActivityTextField.setText(String.valueOf(
               ((Double) aRow[7]).doubleValue()));
            rootPressureHeadTextField.setText(String.valueOf(
               ((Double) aRow[8]).doubleValue()));
         }
      }

      // request focus for the appropriate component when the dialog box opens
      if (modelOptions.doTranspiration)
      {
         potentialTranspirationTextField.requestFocus();
      }
      if (modelOptions.doEvaporation)
      {
         potentialEvaporationTextField.requestFocus();
      }

      // Call superclass method to show the dialog box and return result
      // when the dialog box closes
      return super.doModal();
   }

	//---------------------------------------------------------------------------
   // Implementation abstract method in superclass. Retrieve the data
   // from the text fields and check for errors
	//---------------------------------------------------------------------------
   protected boolean retrieveData()
   {
      double pev=0, sres=0, ha=0, pet=0, rdepth=0, rbase=0, rtop=0, hr=0;
      try
      {
         if (modelOptions.doEvaporation)
         {
            pev = Double.valueOf(potentialEvaporationTextField.getText()).doubleValue();
            sres = Double.valueOf(surfaceResistanceTextField.getText()).doubleValue();
            ha = Double.valueOf(pressurePotentialAtmosphereTextField.getText()).doubleValue();
         }
         if (modelOptions.doTranspiration)
         {
            pet = Double.valueOf(potentialTranspirationTextField.getText()).doubleValue();
            rdepth = Double.valueOf(rootDepthTextField.getText()).doubleValue();
            rbase = Double.valueOf(rootBaseActivityTextField.getText()).doubleValue();
            rtop = Double.valueOf(rootTopActivityTextField.getText()).doubleValue();
            hr = Double.valueOf(rootPressureHeadTextField.getText()).doubleValue();
         }
      }
      catch (NumberFormatException e)
      {
         // Show error message
         mp2MessageBox.showMessageDialog("Please check your input",
            "Input Error");
         return false;
      }

      // Check that evaporation data are correct
      if (modelOptions.doEvaporation)
      {
         if (!dataCheck(pev, "\"Potential evaporation\"", IS_NON_NEGATIVE,
            potentialEvaporationTextField)) return false;
         if (!dataCheck(sres, "\"Surface resistance\"", IS_NON_NEGATIVE,
            surfaceResistanceTextField)) return false;
         //if (!dataCheck(ha, "\"Pressure potential of atmosphere\"", IS_NON_NEGATIVE,
         //   pressurePotentialAtmosphereTextField)) return false;
      }

      // Check that transpiration data are correct
      if (modelOptions.doTranspiration)
      {
         if (!dataCheck(pet, "\"Potential transpiration\"", IS_NON_NEGATIVE,
            potentialTranspirationTextField)) return false;
         if (!dataCheck(rdepth, "\"Root depth\"", IS_NON_NEGATIVE,
            rootDepthTextField)) return false;
         if (!dataCheck(rbase, "\"Activity at root base\"", IS_NON_NEGATIVE,
            rootBaseActivityTextField)) return false;
         if (!dataCheck(rtop, "\"Activity at root top\"", IS_NON_NEGATIVE,
            rootTopActivityTextField)) return false;
         if (!dataCheck(hr, "\"Pressure head in root\"", IS_NON_POSITIVE,
            rootPressureHeadTextField)) return false;
      }

      // When execution reaches this point, all input data are OK so we put
      // them in aRow and return true
      if (modelOptions.doEvaporation)
      {
         aRow[1] = new Double(pev);
         aRow[2] = new Double(sres);
         aRow[3] = new Double(ha);
      }
      if (modelOptions.doTranspiration)
      {
         aRow[4] = new Double(pet);
         aRow[5] = new Double(rdepth);
         aRow[6] = new Double(rbase);
         aRow[7] = new Double(rtop);
         aRow[8] = new Double(hr);
      }
      return true;
   }

	//---------------------------------------------------------------------------
	// Overrides the superclass method. Shows help for this dialog box
	//---------------------------------------------------------------------------
   protected void onBrowserHelp()
   {
       if (modelOptions.doEvaporation) {
            mp2HelpWindow.showHelpFile ("evaporationParameters.html");
       } else if (modelOptions.doTranspiration) {
           mp2HelpWindow.showHelpFile("transpirationParameters.html");
       }
   }

}

