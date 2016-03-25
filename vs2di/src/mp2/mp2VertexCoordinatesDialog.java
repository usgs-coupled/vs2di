/*
 * mp2VertexCoordinatesDialog.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

/**
 * Displays a dialog box for user to specify contour value.
 */
public class mp2VertexCoordinatesDialog extends mp2Dialog {

    public double  xCoord;
    public double  yCoord;
    public boolean useRadialCoordinates = false;

    protected JTextField xCoordTextField;
    protected JTextField yCoordTextField;

    /**
     * Creates a dialog box.
     */
    public mp2VertexCoordinatesDialog (boolean useRadialCoordinates) {
        super("Coordinates", false, new Boolean(useRadialCoordinates));
        this.useRadialCoordinates = useRadialCoordinates;
    }

    /**
     * Makes dialog box contents.
     */
    protected void makeContents() {

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();

        JPanel centerPanel = new JPanel(gridbag);
        centerPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        JPanel panel = new JPanel(new GridLayout(0, 1, 5, 5));
        c.fill = GridBagConstraints.VERTICAL;
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);

        panel.add(new JLabel(mp2View.getHorizontalAxisName() + ":", SwingConstants.RIGHT));
        panel.add(new JLabel(mp2View.getVerticalAxisName() + ":", SwingConstants.RIGHT));

        panel = new JPanel(new GridLayout(0, 1, 5, 5));
        c.insets = new Insets(0, 10, 0, 0);
        gridbag.setConstraints(panel, c);
        centerPanel.add(panel);

        panel.add(xCoordTextField = new JTextField(8));
        panel.add(yCoordTextField = new JTextField(8));
    }

    /**
     * Displays the dialog box with modal behavior.
     */
    public boolean doModal() {

        xCoordTextField.setText(mp2DecimalFormat.format (xCoord));
        yCoordTextField.setText(mp2DecimalFormat.format (yCoord));

        return super.doModal();
    }

    /**
     * Retrives data from dialog box components and checks validity.
     */
    protected boolean retrieveData() {
        try {
            xCoord = Double.valueOf(xCoordTextField.getText()).doubleValue();
            yCoord = Double.valueOf(yCoordTextField.getText()).doubleValue();
        }
        catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please check your input",
                    "Input Error");
            return false;
        }
        if (useRadialCoordinates) {
            if (!dataCheck(xCoord, mp2View.getHorizontalAxisName(), IS_NON_NEGATIVE, xCoordTextField)) {
                return false;
            }
        }
        return true;
    }
}
