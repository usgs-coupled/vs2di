/*
 * mp2ContourMapView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.undo.*;

public class mp2ContourMapView extends mp2BufferedShapesView {

    protected JMenuItem contourValueMenuItem;
    protected JMenuItem interpolationMethodMenuItem;
    protected JMenuItem colorScaleMenuItem;
    protected mp2ToggleButton createPolygonButton;
    protected mp2ToggleButton createPolylineButton;
    protected mp2ToggleButton createPointButton;
    protected mp2ToggleButton contourLabelButton;
    protected mp2ToggleButton discretizeButton;
    protected double minimumContourValue;
    protected double maximumContourValue;
    protected mp2ContourMapData contourMapData;
    protected String contourValueLabel;

    /**
     * Constructor
     */
    public mp2ContourMapView(mp2View view, 
                    mp2ContourMapData shapesData, 
                    mp2AbstractGridData gridData,
                    String homeDirectory) {

        super(view, shapesData, gridData, homeDirectory);
        contourMapData = shapesData;
        shapeIntersectionAllowed = false;
        drawValue = true;
        intersectOtherShapeErrorMessage = "Contours cannot intersect each other";
        intersectSelfErrorMessage = "Contour cannot intersect itself";
        minimumContourValue = Double.NEGATIVE_INFINITY;
        maximumContourValue = Double.POSITIVE_INFINITY;
        
        contourValueLabel = "Contour Value = ";

        contourValueMenuItem = new JMenuItem("Contour Value...");
        contourValueMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onEditContourValue();
            }
        });

        interpolationMethodMenuItem = new JMenuItem("Interpolation Method...");
        interpolationMethodMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onEditInterpolationMethod();
            }
        });

        colorScaleMenuItem = new JMenuItem("Color Scale...");
        colorScaleMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onEditColorScale();
            }
        });

        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        createPolygonButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/polygon.gif")), false);
        createPolylineButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/polyline.gif")), false);
        createPointButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/point.gif")), false);
        discretizeButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretize.gif")), false);
        contourLabelButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/contourlabel.gif")), true);

        createPolygonButton.setToolTipText("Add a closed contour");
        createPolylineButton.setToolTipText("Add an open contour");
        createPointButton.setToolTipText("Add a point value");
        discretizeButton.setToolTipText("Discretize");
        contourLabelButton.setToolTipText("Show or hide contour values");

        ButtonGroup bg = new ButtonGroup();
        bg.add(selectAndEditButton);
        bg.add(createPolygonButton);
        bg.add(createPolylineButton);
        bg.add(createPointButton);
        bg.add(addVertexButton);
        bg.add(discretizeButton);
        bg.add(zoomButton);

        createPolygonButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(CREATE_SHAPE);
                }
            }
        });
        createPolylineButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(CREATE_SHAPE);
                }
            }
        });
        createPointButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(CREATE_SHAPE);
                }
            }
        });
        discretizeButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });
        contourLabelButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onContourLabel(e.getStateChange() == ItemEvent.SELECTED);
            }
        });
    }

    protected void drawDiscretizedShapes(Graphics g) {
        int [] limits = getDrawingLimitsForRectilinearGrid(g);
        double [] xCoord = ((mp2RectilinearGridData) gridData).getXCoords();
        double [] yCoord = ((mp2RectilinearGridData) gridData).getYCoords();
        Point p1, p2;
        double [] valueArray = ((mp2ContourMapData) shapesData).getValueArray();
        int numCol = xCoord.length - 1;
        for (int j=limits[2]; j<limits[3]; j++) {
            for (int i=limits[0]; i<limits[1]; i++) {
                if (view.getModelVerticalAxisOrientation() ==
                        mp2Drawing.MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE) {
                    p1 = view.modelToView(xCoord[i], yCoord[j]);
                    p2 = view.modelToView(xCoord[i+1], yCoord[j+1]); 
                } else {
                    p1 = view.modelToView(xCoord[i], yCoord[j+1]);
                    p2 = view.modelToView(xCoord[i+1], yCoord[j]); 
                }
                double v = valueArray[j*numCol+i];
                if (v != Double.NEGATIVE_INFINITY) {
                    g.setColor(contourMapData.getColorForValue(v));
                    g.fillRect(p1.x, p1.y, p2.x - p1.x, p2.y - p1.y);
                }
            }
        }
    }

    /**
     * Creates a polygon, polyline, or point to represent a contour
     */
    protected mp2Shape createNewShape(Point p) {
        mp2Shape shape;
        if (createPolygonButton.isSelected()) {
           shape = new mp2Polygon();
           shape.setFilled(false);
        } else if (createPolylineButton.isSelected()) {
           shape = new mp2Polyline();
        } else if (createPointButton.isSelected()) {
           shape = new mp2Point();
        } else {
           shape = null;
        }
        return shape;
    }

    /**
     * Enables or disables edit menu item as appropriate
     */
    protected void evaluateEditMenu() {
        super.evaluateEditMenu();
        contourValueMenuItem.setEnabled(
                shapesData.getNumberOfSelectedShapes() > 0);
    }

    /**
     * Handles the "Contour Value" menu item of the "Edit" menu
     */
    protected void onEditContourValue() {
        if (shapesData.getNumberOfSelectedShapes() == 0) {
            mp2MessageBox.showMessageDialog(
                "Please select one or more contours.",
                "Error");
            return;
        }
        int i, j;

        // Save old values for undo
        double [] oldValue = new double [shapesData.getNumberOfShapes()];
        for (i=0; i<oldValue.length; i++) {
            oldValue[i] = shapesData.getShape(i).getValue();
        }

        ContourValueDialog dlg = new ContourValueDialog(contourValueLabel);

        // Find the value of the first selected contour
        for (i=0; i<oldValue.length; i++) {
            if (shapesData.getShape(i).isSelected()) {
                dlg.value = oldValue[i];
                break;
            }
        }

        // If there are more than one selected shape, check if their
        // values are the same as the first selected shape
        for (j=i+1; j<oldValue.length; j++) {
            mp2Shape shape = shapesData.getShape(j);
            if (shape.isSelected() 
                    && shape.getValue() != dlg.value) {
                dlg.value = Double.NEGATIVE_INFINITY;
                break;
            }
        }

        dlg.minimumValue = minimumContourValue;
        dlg.maximumValue = maximumContourValue;

        if (dlg.doModal() == true) {
            double [] newValue = new double [oldValue.length];
            for (i=0; i<newValue.length; i++) {
                mp2Shape shape = shapesData.getShape(i);
                if (shape.isSelected()) {
                    shape.setValue(dlg.value);
                    newValue[i] = dlg.value;
                } else {
                    newValue[i] = oldValue[i];
                }
            }
            undoSupport.postEdit(new ContourValueEdit(oldValue,
                                    newValue, undoCount));
            undoCount++;
            // we have to specifically set data have changed
            // because setting a new value for the shape does not
            // notify the change to shapesData. Should modify this.
            shapesData.setDataHaveChanged();
            view.repaint();
        }
    }

    /**
     * Handles the "Interpolation method" menu item of the "Edit" menu
     */
    protected void onEditInterpolationMethod() {
        InterpolationSchemeDialog dlg = new InterpolationSchemeDialog();
        int oldInterpolationScheme = contourMapData.getInterpolationScheme();
        dlg.interpolationScheme = oldInterpolationScheme;

        if (dlg.doModal() == true && 
                    dlg.interpolationScheme != oldInterpolationScheme) {
            // the set interpolation scheme method will automatically
            // require rediscretization, so the image will be redrawn.
            contourMapData.setInterpolationScheme(dlg.interpolationScheme);
            undoSupport.postEdit(new InterpolationSchemeEdit(oldInterpolationScheme,
                                       dlg.interpolationScheme, undoCount));
            undoCount++;
            if (dlg.interpolationScheme == mp2ContourMapData.TRIANGULATION) {
                createPolylineButton.setEnabled(true);
            } else {
                if (createPolylineButton.isSelected()) {
                    selectAndEditButton.doClick();
                }
                createPolylineButton.setEnabled(false);
            }
            view.repaint();
        }
    }

    /**
     * Handles the "Color Scale" menu item of the "Edit" menu.
     */
    protected void onEditColorScale() {
        ColorScaleDialog dlg = new ColorScaleDialog ();
        int oldPaletteScheme = contourMapData.getPaletteScheme();
        int oldColorPolicy = contourMapData.getColorPolicy();
        double oldValueBlue = contourMapData.getValueBlue();
        double oldValueRed = contourMapData.getValueRed();
        dlg.paletteScheme = oldPaletteScheme;
        dlg.colorPolicy = oldColorPolicy;
        dlg.valueBlue = oldValueBlue;
        dlg.valueRed = oldValueRed;
        if (dlg.doModal() == true &&
                    (dlg.paletteScheme != oldPaletteScheme
                     || dlg.colorPolicy != oldColorPolicy
                     || dlg.valueBlue != oldValueBlue
                     || dlg.valueRed != oldValueRed)) {
            contourMapData.setPaletteScheme(dlg.paletteScheme);
            contourMapData.setColorPolicy(dlg.colorPolicy);
            double newValueBlue = oldValueBlue;
            double newValueRed = oldValueRed;
            if (dlg.colorPolicy == mp2ContourMapData.CUSTOM_COLOR) {
                newValueBlue = dlg.valueBlue;
                newValueRed = dlg.valueRed;
                contourMapData.setValueRange(newValueBlue, newValueRed, true);
            }
            // we don't have to rediscretize, but the image must be redrawn.
            remakeImage = true;
            undoSupport.postEdit(new ColorScaleEdit(
                    oldPaletteScheme, oldColorPolicy, oldValueBlue, oldValueRed, 
                    dlg.paletteScheme, dlg.colorPolicy, newValueBlue, newValueRed,
                    undoCount));
            undoCount++;
            view.repaint();
        }
    }

    /**
     * Activates or deactivates labeling of contours
     */
    protected void onContourLabel(boolean b) {
        drawValue = b;
        view.repaint();
    }

    public void onMouseReleased(MouseEvent e) {
        super.onMouseReleased(e);
        contourValueMenuItem.setEnabled(
                shapesData.getNumberOfSelectedShapes() > 0);
    }

    protected boolean onNewShapeCompletion(mp2Shape shape) {
        ContourValueDialog dlg = new ContourValueDialog(contourValueLabel);
        dlg.value = Double.NEGATIVE_INFINITY;
        dlg.minimumValue = minimumContourValue;
        dlg.maximumValue = maximumContourValue;
        if (dlg.doModal() == true) {
            shape.setValue(dlg.value);
            return true;
        } else {
            return false;
        }
    }

    /**
     * Handles the "Select All" menu item in the "Edit" menu. 
     */
    protected void onSelectAll() {
        super.onSelectAll();
        contourValueMenuItem.setEnabled(
                shapesData.getNumberOfSelectedShapes() > 0);
    }

    /**
     * Paints the contours
     */
    public void paint(Graphics g) {
        if (!isActive) {
            shapesData.deselectAllShapes();
        }
        if (!isVisible || shapesData.getNumberOfShapes() == 0) {
            return;
        }
        Color oldColor = g.getColor();
        if (view.isPaintModeDiscrete()) {
            paintDiscrete(g);
        }
        // The contour lines are drawn for both discrete and
        // continuous modes
        for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
            if (isActive) {
                shapesData.getShape(i).setForeground(Color.black);
            } else {
                shapesData.getShape(i).setForeground(Color.gray);
            }
            shapesData.getShape(i).draw(g);
            if (drawValue) {
                shapesData.getShape(i).drawValue(g);
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    /**
     * Loads the edit menu and tool bar
     */
    public void prepareToActivate() {
        super.prepareToActivate();

        // Set up the "Edit" menu
        JMenu editMenu = view.getFrame().getMenu(EDIT_MENU);
        editMenu.removeAll();
        editMenu.add(undoMenuItem);
        editMenu.add(redoMenuItem);
        editMenu.addSeparator();
        editMenu.add(selectAllMenuItem);
        editMenu.add(deleteMenuItem);
        editMenu.add(contourValueMenuItem);
        editMenu.addSeparator();
        editMenu.add(interpolationMethodMenuItem);
        editMenu.add(colorScaleMenuItem);
        shapesData.deselectAllShapes();
        evaluateEditMenu();

        // Set up buttons on tool bar
        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(selectAndEditButton);
        toolBar.add(createPolygonButton);
        toolBar.add(createPolylineButton);
        toolBar.add(addVertexButton);
        if (gridData.isDefined()) {
            toolBar.add(discretizeButton);
        }
        toolBar.add(zoomButton);
        toolBar.add(Box.createVerticalStrut(10));
        toolBar.add(contourLabelButton);
        if (view.hasSiteMap()) {
            toolBar.add(Box.createVerticalStrut(10));
            toolBar.add(view.getXORButton());
        }

        if (view.isPaintModeDiscrete()) {
            discretizeButton.setSelected(true);
            remakeImage = true;
        } else {
            selectAndEditButton.setSelected(true);
        }
        createPolylineButton.setEnabled(contourMapData.getInterpolationScheme()
                                == mp2ContourMapData.TRIANGULATION && isEditable);

        view.setCursor(Cursor.getDefaultCursor());
    }

    /**
     * Sets the active mode
     */
    protected void setActiveModeTo(int m) {
        super.setActiveModeTo(m);
        contourValueMenuItem.setEnabled(
                shapesData.getNumberOfSelectedShapes() == 1);
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        super.setEditable(b);
        if (!b) {
            if (createPolygonButton.isSelected()
                    || createPolylineButton.isSelected()
                    || addVertexButton.isSelected()) {
                selectAndEditButton.setSelected(true);
            }
        }
        createPolygonButton.setEnabled(b);
        createPolylineButton.setEnabled(b);
        addVertexButton.setEnabled(b);
    }

    public void setContourValueLabel(String label) {
        contourValueLabel = label;
    }

    /**
     * Sets the maximum value allowed for contours. Used to check
     * user input when setting contour value after drawing the contour. 
     * Does not control color scale
     */
    public void setMaximumContourValue(double maxValue) {
        maximumContourValue = maxValue;
    }

    /**
     * Sets the minimum value allowed for contours. Used to check
     * user input when setting contour value after drawing the contour. 
     * Does not control color scale
     */
    public void setMinimumContourValue(double minValue) {
        minimumContourValue = minValue;
    }

    /**
     * Undoable edit to change contour value
     */
    protected class ContourValueEdit extends AbstractDataEdit {

        protected double [] oldValue;
        protected double [] newValue;

        ContourValueEdit(double [] oldValue, double [] newValue, 
                                long undoIndex) {
            super(undoIndex);
            this.oldValue = oldValue;
            this.newValue = newValue;
        }

        public void undo() throws CannotUndoException {
            for (int i=0; i<oldValue.length; i++) {
                shapesData.getShape(i).setValue(oldValue[i]);
            }
            // we have to specifically set data have changed
            // because setting a new value for the shape does not
            // notify the change to shapesData. Should modify this.
            shapesData.setDataHaveChanged();
            super.undo();
        }

        public void redo() throws CannotRedoException {
            for (int i=0; i<oldValue.length; i++) {
                shapesData.getShape(i).setValue(newValue[i]);
            }
            // we have to specifically set data have changed
            // because setting a new value for the shape does not
            // notify the change to shapesData. Should modify this.
            shapesData.setDataHaveChanged();
            super.redo();
        }
    }

    /**
     * Undoable edit to change interpolation scheme
     */
    protected class InterpolationSchemeEdit extends AbstractDataEdit {

        protected int oldInterpolationScheme;
        protected int newInterpolationScheme;

        InterpolationSchemeEdit(int oldInterpolationScheme, int newInterpolationScheme, 
                                long undoIndex) {
            super(undoIndex);
            this.oldInterpolationScheme = oldInterpolationScheme;
            this.newInterpolationScheme = newInterpolationScheme;
        }

        public void undo() throws CannotUndoException {
            contourMapData.setInterpolationScheme(oldInterpolationScheme);
            super.undo();
        }

        public void redo() throws CannotRedoException {
            contourMapData.setInterpolationScheme(newInterpolationScheme);
            super.redo();
        }
    }

    /**
     * Undoable edit to change interpolation scheme
     */
    protected class ColorScaleEdit extends AbstractDataEdit {

        protected int oldPaletteScheme;
        protected int oldColorPolicy;
        protected double oldValueRed;
        protected double oldValueBlue; 
        protected int newPaletteScheme;
        protected int newColorPolicy;
        protected double newValueRed;
        protected double newValueBlue;

        ColorScaleEdit(int oldPaletteScheme, int oldColorPolicy, 
                       double oldValueBlue, double oldValueRed, 
                       int newPaletteScheme, int newColorPolicy, 
                       double newValueBlue, double newValueRed,
                       long undoIndex) {
            super(undoIndex);
            this.oldPaletteScheme = oldPaletteScheme;
            this.oldColorPolicy = oldColorPolicy;
            this.oldValueBlue = oldValueBlue; 
            this.oldValueRed = oldValueRed;
            this.newPaletteScheme = newPaletteScheme;
            this.newColorPolicy = newColorPolicy;
            this.newValueBlue = newValueBlue;
            this.newValueRed = newValueRed;
        }

        public void undo() throws CannotUndoException {
            contourMapData.setPaletteScheme(oldPaletteScheme);
            contourMapData.setColorPolicy(oldColorPolicy);
            contourMapData.setValueRange(oldValueBlue, oldValueRed, true);
            remakeImage = true;
            super.undo();
        }

        public void redo() throws CannotRedoException {
            contourMapData.setPaletteScheme(newPaletteScheme);
            contourMapData.setColorPolicy(newColorPolicy);
            contourMapData.setValueRange(newValueBlue, newValueRed, true);
            remakeImage = true;
            super.redo();
        }
    }

    /**
     * Displays a dialog box for user to specify contour value.
     */
    protected class ContourValueDialog extends mp2Dialog {

        public double value;
        public double minimumValue;
        public double maximumValue;
        protected JTextField valueTextField;

        /**
         * Creates a dialog box.
         */
        public ContourValueDialog(String contourValueLabel) {
            super("Contour Value", false, contourValueLabel);
        }

        /**
         * Makes dialog box contents.
         */
        protected void makeContents() {
            String contourValueLabel = (String) customObject;
            JPanel centerPanel = new JPanel(new GridBagLayout());
            centerPanel.setBorder(new EmptyBorder(15, 15, 15, 15));
            getContentPane().add(centerPanel, BorderLayout.CENTER);
            centerPanel.add(new JLabel(contourValueLabel));
            centerPanel.add(valueTextField = new JTextField(5));
        }

        /**
         * Displays the dialog box with modal behavior.
         */
        public boolean doModal() {
            if (value != Double.NEGATIVE_INFINITY) {
                valueTextField.setText(String.valueOf(value));
            }
            return super.doModal();
        }

        /**
         * Retrives data from dialog box components and checks validity.
         */
        protected boolean retrieveData() {
            try {
                value = Double.valueOf(valueTextField.getText()).doubleValue();
            }
            catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog("Please check your input",
                        "Input Error");
                return false;
            }
            if (minimumValue > Double.NEGATIVE_INFINITY
                        && maximumValue < Double.POSITIVE_INFINITY) {
                if (!dataCheck(value, "the contour value",
                        IS_BETWEEN_INCLUSIVE,
                        minimumValue, String.valueOf(minimumValue),
                        maximumValue, String.valueOf(maximumValue),
                        valueTextField)) {
                    return false;
                }
            }
            else if (minimumValue > Double.NEGATIVE_INFINITY
                        && maximumValue == Double.POSITIVE_INFINITY) {
                if (!dataCheck(value, "the contour value", 
                        IS_GREATER_THAN_OR_EQUAL_TO,
                        minimumValue, String.valueOf(minimumValue),
                        valueTextField)) {
                    return false;
                }
            }
            else if (minimumValue == Double.NEGATIVE_INFINITY
                        && maximumValue < Double.POSITIVE_INFINITY) {
                if (!dataCheck(value, "the contour value", 
                        IS_LESS_THAN_OR_EQUAL_TO,
                        maximumValue, String.valueOf(maximumValue),
                        valueTextField)) {
                    return false;
                }
            }
            return true;
        }
    }

    /**
     * Displays a dialog box for user to specify interpolation scheme
     */
    public class InterpolationSchemeDialog extends mp2Dialog {

        public int interpolationScheme;
        protected JRadioButton closedContourStepRadioButton;
        protected JRadioButton nearestContourRadioButton;
        protected JRadioButton inverseDistanceRadioButton;
        protected JRadioButton inverseQuadrantRadioButton;
        protected JRadioButton inverseDistance2RadioButton;
        protected JRadioButton inverseQuadrant2RadioButton;
        protected JRadioButton triangulationRadioButton;

        /**
         * Creates a dialog box.
         */
        public InterpolationSchemeDialog() {
            super("Interpolation Method", true);
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "interpolationMethods", null);
        }

        /**
         * Makes dialog box contents.
         */
        protected void makeContents() {
            JPanel centerPanel = new JPanel(false);
            centerPanel.setLayout(new GridLayout(0, 1));
            centerPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
            getContentPane().add(centerPanel, BorderLayout.CENTER);
            closedContourStepRadioButton = new JRadioButton("Step (using closed contours only)");
            nearestContourRadioButton    = new JRadioButton("Nearest contour");
            inverseDistanceRadioButton   = new JRadioButton("Inverse-distance");
            inverseQuadrantRadioButton   = new JRadioButton("Inverse-distance with sub-sampled contours");
            inverseDistance2RadioButton  = new JRadioButton("Inverse-distance-squared");
            inverseQuadrant2RadioButton  = new JRadioButton("Inverse-distance-squared with sub-sampled contours");
            triangulationRadioButton = new JRadioButton("Continuous");
            ButtonGroup bg = new ButtonGroup();
            bg.add(closedContourStepRadioButton);
            bg.add(nearestContourRadioButton);
            bg.add(inverseDistanceRadioButton);
            bg.add(inverseQuadrantRadioButton);
            bg.add(inverseDistance2RadioButton);
            bg.add(inverseQuadrant2RadioButton);
            bg.add(triangulationRadioButton);
        
            //centerPanel.add(new JLabel("One of the following 2 methods is recommended:"));
            centerPanel.add(triangulationRadioButton);
            centerPanel.add(closedContourStepRadioButton);
            //centerPanel.add(new JLabel());
            //centerPanel.add(new JLabel("The following methods may be used as alternatives:"));
            //centerPanel.add(inverseDistanceRadioButton);
            //centerPanel.add(inverseDistance2RadioButton);
            //centerPanel.add(new JLabel());
            //centerPanel.add(new JLabel("The following methods are experimental and could require"));
            //centerPanel.add(new JLabel("long computational times:"));
            //centerPanel.add(inverseQuadrantRadioButton);
            //centerPanel.add(inverseQuadrant2RadioButton);
        }

        /**
         * Displays the dialog box with modal behavior.
         */
        public boolean doModal() {
            closedContourStepRadioButton.setSelected(
                    interpolationScheme == mp2ContourMapData.CLOSED_CONTOUR_STEP);
            nearestContourRadioButton.setSelected(
                    interpolationScheme == mp2ContourMapData.NEAREST_CONTOUR);
            inverseDistanceRadioButton.setSelected(
                    interpolationScheme == mp2ContourMapData.INVERSE_DISTANCE);
            inverseQuadrantRadioButton.setSelected(
                    interpolationScheme == mp2ContourMapData.INVERSE_DISTANCE_QUADRANT);
            inverseDistance2RadioButton.setSelected(
                    interpolationScheme == mp2ContourMapData.INVERSE_DISTANCE_2);
            inverseQuadrant2RadioButton.setSelected(
                    interpolationScheme == mp2ContourMapData.INVERSE_DISTANCE_2_QUADRANT);
            triangulationRadioButton.setSelected(
                    interpolationScheme == mp2ContourMapData.TRIANGULATION);
            return super.doModal();
        }

        protected void onBrowserHelp() {
            mp2HelpWindow.showHelpFile("interpolationMethods.html");
        }

        /**
         * Retrives data from dialog box components and checks validity.
         */
        protected boolean retrieveData() {
            if (closedContourStepRadioButton.isSelected()) {
                interpolationScheme = mp2ContourMapData.CLOSED_CONTOUR_STEP;
            }
            else if (nearestContourRadioButton.isSelected()) {
                interpolationScheme = mp2ContourMapData.NEAREST_CONTOUR;
            } 
            else if (inverseDistanceRadioButton.isSelected()) {
                interpolationScheme = mp2ContourMapData.INVERSE_DISTANCE;
            }
            else if (inverseQuadrantRadioButton.isSelected()) {
                interpolationScheme = mp2ContourMapData.INVERSE_DISTANCE_QUADRANT;
            }
            else if (inverseDistance2RadioButton.isSelected()) {
                interpolationScheme = mp2ContourMapData.INVERSE_DISTANCE_2;
            }
            else if (inverseQuadrant2RadioButton.isSelected()) {
                interpolationScheme = mp2ContourMapData.INVERSE_DISTANCE_2_QUADRANT;
            }
            else if (triangulationRadioButton.isSelected()) {
                interpolationScheme = mp2ContourMapData.TRIANGULATION;
            }
            return true;
        }
    }

    /**
     * Displays a dialog box for user to specify limits of color scale
     */
    public class ColorScaleDialog extends mp2Dialog {

        public int paletteScheme;
        public int colorPolicy;
        public double valueRed;
        public double valueBlue;

        protected JRadioButton grayScaleRadioButton;
        protected JRadioButton spectrum2RadioButton;
        protected JRadioButton spectrum1RadioButton;

        protected JRadioButton redHighRadioButton;
        protected JRadioButton blueHighRadioButton;
        protected JRadioButton customColorRadioButton;

        protected JTextField valueRedTextField;
        protected JTextField valueBlueTextField;

        /**
         * Creates a dialog box.
         */
        public ColorScaleDialog () {
            super("Color Scale", true);
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "contourColorScale", null);
        }

        /**
         * Makes dialog box contents.
         */
        protected void makeContents() {
            GridBagLayout gridbag = new GridBagLayout();
            GridBagConstraints c = new GridBagConstraints();
            JPanel centerPanel = new JPanel(gridbag);
            centerPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
            getContentPane().add(centerPanel, BorderLayout.CENTER);

            JPanel palettePanel = new JPanel(new GridLayout(0, 1));
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.fill = GridBagConstraints.HORIZONTAL;
            gridbag.setConstraints(palettePanel, c);
            centerPanel.add(palettePanel);
            palettePanel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder(
                    "Palette"),
                    new EmptyBorder(0, 5, 5, 5)));
            spectrum1RadioButton = new JRadioButton ("Spectrum - 1");
            spectrum2RadioButton = new JRadioButton ("Spectrum - 2");
            grayScaleRadioButton = new JRadioButton ("Gray Scale");
            ButtonGroup bg = new ButtonGroup();
            bg.add(spectrum1RadioButton);
            bg.add(spectrum2RadioButton);
            bg.add(grayScaleRadioButton);
            JPanel panel;
            palettePanel.add(panel = new JPanel(new FlowLayout(FlowLayout.LEFT)));
            panel.add(spectrum1RadioButton);
            palettePanel.add(panel = new JPanel(new FlowLayout(FlowLayout.LEFT)));
            panel.add(spectrum2RadioButton);
            palettePanel.add(panel = new JPanel(new FlowLayout(FlowLayout.LEFT)));
            panel.add(grayScaleRadioButton);

            JPanel scalePanel = new JPanel(gridbag);
            c.insets = new Insets(10, 0, 0, 0);
            gridbag.setConstraints(scalePanel, c);
            centerPanel.add(scalePanel);
            scalePanel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder(
                    "Values at color scale endpoints"),
                    new EmptyBorder(5, 5, 5, 5)));
            redHighRadioButton = new JRadioButton ("Automatic, high value at red/white endpoint");
            blueHighRadioButton= new JRadioButton ("Automatic, high value at blue/black endpoint");
            customColorRadioButton = new JRadioButton("Custom");
            valueRedTextField = new JTextField(5);
            valueBlueTextField = new JTextField(5);
            customColorRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        valueRedTextField.setEnabled(true);
                        valueBlueTextField.setEnabled(true);
                    } else {
                        valueRedTextField.setText("");
                        valueBlueTextField.setText("");
                        valueRedTextField.setEnabled(false);
                        valueBlueTextField.setEnabled(false);                        
                    }
                }
            });
            bg = new ButtonGroup();
            bg.add(redHighRadioButton);
            bg.add(blueHighRadioButton);
            bg.add(customColorRadioButton);

            panel = new JPanel(new GridLayout(3, 1, 10, 10));
            c.insets = new Insets(0, 0, 0, 0);
            gridbag.setConstraints(panel, c);
            scalePanel.add(panel);
            panel.add(redHighRadioButton);
            panel.add(blueHighRadioButton);
            panel.add(customColorRadioButton);

            panel = new JPanel(gridbag);
            gridbag.setConstraints(panel,c);
            scalePanel.add(panel);

            JPanel leftPanel = new JPanel(new GridLayout(2, 1));
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.fill = GridBagConstraints.VERTICAL;
            gridbag.setConstraints(leftPanel, c);
            panel.add(leftPanel);

            JPanel rightPanel = new JPanel(new GridLayout(2, 1, 10, 10));
            c.insets = new Insets(0, 10, 0, 10);
            gridbag.setConstraints(rightPanel, c);
            panel.add(rightPanel);

            leftPanel.add(new JLabel("Value at red/white endpoint:", SwingConstants.RIGHT));
            rightPanel.add(valueRedTextField);

            leftPanel.add(new JLabel("Value at blue/black endpoint:", SwingConstants.RIGHT));
            rightPanel.add(valueBlueTextField);

            
        }

        /**
         * Displays the dialog box with modal behavior.
         */
        public boolean doModal() {
            grayScaleRadioButton.setSelected(
                    paletteScheme == mp2ColorScale.GRAY);
            spectrum2RadioButton.setSelected(
                    paletteScheme == mp2ColorScale.HUE);
            spectrum1RadioButton.setSelected(
                    paletteScheme == mp2ColorScale.SPECTRUM);
            redHighRadioButton.setSelected(
                    colorPolicy == mp2ContourMapData.AUTO_RED_HIGH);
            blueHighRadioButton.setSelected(
                    colorPolicy == mp2ContourMapData.AUTO_BLUE_HIGH);
            customColorRadioButton.setSelected(
                    colorPolicy == mp2ContourMapData.CUSTOM_COLOR);
            if (colorPolicy == mp2ContourMapData.CUSTOM_COLOR) {
                valueRedTextField.setText(String.valueOf(valueRed));
                valueBlueTextField.setText(String.valueOf(valueBlue));
            } else {
                valueRedTextField.setEnabled(false);
                valueBlueTextField.setEnabled(false);
            }
            return super.doModal();
        }

        /**
         * Retrives data from dialog box components and checks validity.
         */
        protected boolean retrieveData() {
            if (grayScaleRadioButton.isSelected()) {
                paletteScheme = mp2ColorScale.GRAY;
            }
            else if (spectrum2RadioButton.isSelected()) {
                paletteScheme = mp2ColorScale.HUE;
            } 
            else if (spectrum1RadioButton.isSelected()) {
                paletteScheme = mp2ColorScale.SPECTRUM;
            }
            if (redHighRadioButton.isSelected()) {
                colorPolicy = mp2ContourMapData.AUTO_RED_HIGH;
            }
            else if (blueHighRadioButton.isSelected()) {
                colorPolicy = mp2ContourMapData.AUTO_BLUE_HIGH;
            }
            else if (customColorRadioButton.isSelected()) {
                colorPolicy = mp2ContourMapData.CUSTOM_COLOR;
                try {
                    valueRed = Double.valueOf(valueRedTextField.getText()).doubleValue();
                    valueBlue = Double.valueOf(valueBlueTextField.getText()).doubleValue();
                }
                catch (NumberFormatException e) {
                    mp2MessageBox.showMessageDialog("Please check your input",
                            "Input Error");
                    return false;
                }
            }
            return true;
        }
    }

}
