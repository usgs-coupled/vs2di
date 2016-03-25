/*
 * mp2DrawingOptionsDialog.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class mp2DrawingOptionsDialog extends mp2Dialog implements mp2Constants {

    public double drawingWidth;
    public double drawingHeight;
    public double distanceXPerInch;
    public double distanceYPerInch;
    public double xOriginInInches;
    public double yOriginInInches;
    public int numDecimal;
    public int rulerUnits;

    protected JTextField drawingWidthTextField;
    protected JTextField drawingHeightTextField;
    protected JTextField distanceXPerInchTextField;
    protected JTextField distanceYPerInchTextField;
    protected JTextField xReferenceTextField;
    protected JTextField yReferenceTextField;
    protected JRadioButton rulerUnitInchesButton;
    protected JRadioButton rulerUnitModelButton;
    protected JComboBox    decimalChooser;
    protected JCheckBox    rulerDimensionCheckBox;
    
    public static final int ORIGIN_FROM_UPPER_LEFT_CORNER = 0;
    public static final int ORIGIN_FROM_LOWER_LEFT_CORNER = 1;
    public static final int UPPER_LEFT_CORNER_COORDINATES = 2;
    public static final int LOWER_LEFT_CORNER_COORDINATES = 3;
    
    protected static int drawingOptionsFlag = ORIGIN_FROM_UPPER_LEFT_CORNER;
    protected static String modelUnitsLower = "model units";
    protected static String modelUnitsUpper = "Model Units";
    
    protected static boolean showHelpButton = true;

    public mp2DrawingOptionsDialog() {
        super("Drawing Options", showHelpButton);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "drawingOptions", null);
    }

    public mp2DrawingOptionsDialog(JFrame parent) {
        super("DrawingOptions", showHelpButton, parent);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "drawingOptions", null);
    }
    
    public static void setDrawingOptionsFlag(int flag) {
        drawingOptionsFlag = flag;
    }

    public static void showHelpButton(boolean b) {
        showHelpButton = b;
    }
    
    public static void setModelUnits(String lower, String upper) {
        modelUnitsLower = lower;
        modelUnitsUpper = upper;
    }
    
    // For backward compatibility. Do not use
    public static void setHorizontalAxisName(String xname) {
        mp2View.setHorizontalAxisName(xname);
    }
    
    // For backward compatibility. Do not use
    public static void setVerticalAxisName(String zname) {
        mp2View.setVerticalAxisName(zname);
    }

    protected void makeContents() {
        
        GridBagLayout      gridbag = new GridBagLayout();
        GridBagConstraints c       = new GridBagConstraints();
        JPanel             subPanel;

        ButtonGroup group;

        JPanel centerPanel = new JPanel(gridbag);
        centerPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        // Drawing Size ---------------------------------------------
        subPanel = new JPanel (new FlowLayout(FlowLayout.CENTER));
        subPanel.setBorder(new CompoundBorder(
            BorderFactory.createTitledBorder ("Drawing Size (in inches)"), 
            new EmptyBorder(5, 5, 5, 5)));

        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        panel.add(new JLabel("Width:"));
        panel.add(drawingWidthTextField = new JTextField(5));
        subPanel.add(panel);

        panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        panel.add(new JLabel("Height:"));
        panel.add(drawingHeightTextField = new JTextField(5));
        subPanel.add(panel);

        c.gridwidth = 1;
        c.gridheight = 1;
        c.insets = new Insets(5, 5, 5, 5);
        c.fill = GridBagConstraints.BOTH;
        gridbag.setConstraints(subPanel, c);
        centerPanel.add(subPanel);

        // Ruler Units --------------------------------------------------
        subPanel = new JPanel (new FlowLayout(FlowLayout.CENTER));
        subPanel.setBorder(new CompoundBorder(
            BorderFactory.createTitledBorder("Ruler Units"),
            new EmptyBorder(5, 5, 5, 5)));
        
        group = new ButtonGroup();
        if (modelUnitsLower.equals("model units")) {
            // original code
            rulerUnitModelButton = new JRadioButton(modelUnitsUpper);
            group.add(rulerUnitModelButton);
            gridbag.setConstraints(rulerUnitModelButton, c);
            subPanel.add(rulerUnitModelButton);

            rulerUnitInchesButton = new JRadioButton("Inches", true);
            group.add(rulerUnitInchesButton);
            subPanel.add(rulerUnitInchesButton);
        } else {
            // Revised on April 7, 2001
            rulerUnitModelButton = new JRadioButton(modelUnitsUpper + " (Model)");
            group.add(rulerUnitModelButton);
            gridbag.setConstraints(rulerUnitModelButton, c);
            subPanel.add(rulerUnitModelButton);

            rulerUnitInchesButton = new JRadioButton("inches (Drawing)", true);
            group.add(rulerUnitInchesButton);
            subPanel.add(rulerUnitInchesButton);
        }

        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(subPanel, c);
        centerPanel.add(subPanel);

        // Drawing Scale ------------------------------------------------
        subPanel = new JPanel(new GridLayout(3, 1));
        subPanel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Scale"),
                new EmptyBorder(5, 5, 5, 5)));
        subPanel.add(new JLabel("1 inch on drawing equals:"));

        panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        panel.add(distanceXPerInchTextField = new JTextField(5));
        panel.add(new JLabel(modelUnitsLower + " in " + mp2View.getHorizontalAxisName() + " direction"));
        subPanel.add(panel);

        panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        panel.add(distanceYPerInchTextField = new JTextField(5));
        panel.add(new JLabel(modelUnitsLower + " in " + mp2View.getVerticalAxisName() + " direction"));
        subPanel.add(panel);

        c.gridwidth = 1;
        c.gridheight = 2;
        gridbag.setConstraints(subPanel, c);
        centerPanel.add(subPanel);

        // Drawing Origin / drawing corner ------------------------------------
        switch (drawingOptionsFlag) {
        case LOWER_LEFT_CORNER_COORDINATES:
            subPanel = new JPanel(new GridLayout(3, 1));
            subPanel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Lower Left Corner"),
                    new EmptyBorder(5, 5, 5, 5)));

            subPanel.add(new JLabel("Set coordinates of lower left corner to:"));

            panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.add(new JLabel(mp2View.getHorizontalAxisName() + " = "));
            panel.add(xReferenceTextField = new JTextField(8));
            subPanel.add(panel);

            panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.add(new JLabel(mp2View.getVerticalAxisName() + " = "));
            panel.add(yReferenceTextField = new JTextField(8));
            subPanel.add(panel);
            break;
        case UPPER_LEFT_CORNER_COORDINATES:
            subPanel = new JPanel(new GridLayout(3, 1));
            subPanel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Upper Left Corner"),
                    new EmptyBorder(5, 5, 5, 5)));

            subPanel.add(new JLabel("Set coordinates of upper left corner to:"));

            panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.add(new JLabel(mp2View.getHorizontalAxisName() + " = "));
            panel.add(xReferenceTextField = new JTextField(8));
            subPanel.add(panel);

            panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.add(new JLabel(mp2View.getVerticalAxisName() + " = "));
            panel.add(yReferenceTextField = new JTextField(8));
            subPanel.add(panel);
            break;
        case ORIGIN_FROM_LOWER_LEFT_CORNER:
            subPanel = new JPanel(new GridLayout(3, 1));
            subPanel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Origin"),
                    new EmptyBorder(5, 5, 5, 5)));

            subPanel.add(new JLabel("Set coordinates origin at:"));

            panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.add(xReferenceTextField = new JTextField(5));
            panel.add(new JLabel("inches from left edge of drawing"));
            subPanel.add(panel);

            panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.add(yReferenceTextField = new JTextField(5));
            panel.add(new JLabel("inches above bottom of drawing"));
            subPanel.add(panel);
            break;
        default:
            //Origin from upper left corner
            subPanel = new JPanel(new GridLayout(3, 1));
            subPanel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Origin"),
                    new EmptyBorder(5, 5, 5, 5)));

            subPanel.add(new JLabel("Set coordinates origin at:"));

            panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.add(xReferenceTextField = new JTextField(5));
            panel.add(new JLabel("inches from left edge of drawing"));
            subPanel.add(panel);

            panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.add(yReferenceTextField = new JTextField(5));
            panel.add(new JLabel("inches below top of drawing"));
            subPanel.add(panel);
            break;
        }

        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(subPanel, c);
        centerPanel.add(subPanel);


        // Coordinate Decimals ------------------------------------------
        panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel.add(new JLabel("Show coordinates values to "));
        panel.add(decimalChooser = new JComboBox());
        panel.add(new JLabel(" decimal places"));
        for (int i=0; i<5; i++) {
            decimalChooser.addItem(String.valueOf(i));
        }
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);
    }

    public boolean doModal() {
        double xReference, yReference;
        switch (drawingOptionsFlag) {
        case LOWER_LEFT_CORNER_COORDINATES:
            // positive y axis is upward
            xReference = (0 - xOriginInInches) * distanceXPerInch;
            yReference = (yOriginInInches - drawingHeight) * distanceYPerInch;
            break;
        case UPPER_LEFT_CORNER_COORDINATES:
            // positive y axis is downward
            xReference = (0 - xOriginInInches) * distanceXPerInch;
            yReference = (0 - yOriginInInches) * distanceYPerInch;
            break;
        case ORIGIN_FROM_LOWER_LEFT_CORNER:
            // positive y axis is upward
            xReference  = xOriginInInches;
            yReference  = drawingHeight - yOriginInInches;
            break;
        default:
            // origin from upper left corner
            // positive y axis is downward
            xReference  = xOriginInInches;
            yReference  = yOriginInInches;
        }
        
        drawingWidthTextField.setText(String.valueOf(drawingWidth));
        drawingHeightTextField.setText(String.valueOf(drawingHeight));
        distanceXPerInchTextField.setText(String.valueOf(distanceXPerInch));
        distanceYPerInchTextField.setText(String.valueOf(distanceYPerInch));
        xReferenceTextField.setText(String.valueOf(xReference));
        yReferenceTextField.setText(String.valueOf(yReference));
        decimalChooser.setSelectedIndex(numDecimal);

        drawingWidthTextField.requestFocus();

        rulerUnitInchesButton.setSelected (rulerUnits == mp2Ruler.INCHES);
        rulerUnitModelButton.setSelected (rulerUnits == mp2Ruler.MODEL_UNITS);

        return super.doModal();
    }

    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile ("drawingOptions.html");
    }

    protected boolean retrieveData() {
        double w=0, h=0, dx=0, dy=0, xo=0, yo=0;
        try {
            w = Double.valueOf(drawingWidthTextField.getText()).doubleValue();
            h = Double.valueOf(drawingHeightTextField.getText()).doubleValue();
            dx = Double.valueOf(distanceXPerInchTextField.getText()).doubleValue();
            dy = Double.valueOf(distanceYPerInchTextField.getText()).doubleValue();
            xo = Double.valueOf(xReferenceTextField.getText()).doubleValue();
            yo = Double.valueOf(yReferenceTextField.getText()).doubleValue();
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog(parent, "Please check your input.",
                    "Input Error");
            return false;
        }

        // Check that data are correct
        if (!dataCheck(w, "drawing width", IS_POSITIVE, drawingWidthTextField)) 
                return false;
        if (!dataCheck(h, "drawing height", IS_POSITIVE, drawingHeightTextField)) 
                return false;
        if (!dataCheck(dx, "drawing inch:model length", IS_POSITIVE,
                distanceXPerInchTextField)) return false;
        if (!dataCheck(dy, "drawing inch:model length", IS_POSITIVE,
                distanceYPerInchTextField)) return false;

        // When execution reaches this point, all input data are OK so we put
        // them in aRow and return true
        drawingWidth = w;
        drawingHeight = h;
        distanceXPerInch = dx;
        distanceYPerInch = dy;
        double xReference = xo;
        double yReference = yo;
        numDecimal = decimalChooser.getSelectedIndex();
        
        switch (drawingOptionsFlag) {
        case LOWER_LEFT_CORNER_COORDINATES:
            // positive y axis is upward
            xOriginInInches = - xReference/distanceXPerInch;
            yOriginInInches = drawingHeight + yReference/distanceYPerInch;
            break;
        case UPPER_LEFT_CORNER_COORDINATES:
            // positive y axis is downward
            xOriginInInches = - xReference/distanceXPerInch;
            yOriginInInches = - yReference/distanceYPerInch;
            break;
        case ORIGIN_FROM_LOWER_LEFT_CORNER:
            // positive y axis is upward
            xOriginInInches = xReference;
            yOriginInInches = drawingHeight - yReference;
            break;
        default:
            // origin from upper left corner
            // positive y axis is downward
            xOriginInInches = xReference;
            yOriginInInches = yReference;
        }
        
        if (rulerUnitInchesButton.isSelected ()) {
            rulerUnits = mp2Ruler.INCHES;
        }
        else {
            rulerUnits = mp2Ruler.MODEL_UNITS;
        }
        return true;
    }
}
