/*
* vs2InitialEquilibriumProfileView.java
*/
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.undo.*;

public class vs2InitialEquilibriumProfileView extends mp2GraphicalDataView 
        implements vs2Constants {

    protected vs2InitialEquilibriumProfileData profileData;
    protected mp2RectilinearGridData gridData;
    protected mp2ToggleButton selectAndEditButton;
    protected mp2ToggleButton discretizeButton;
    protected mp2Button equilibriumProfileButton;
    protected JMenuItem equilibriumProfileMenuItem;
    protected int featureUnderMouse;
    protected boolean hasChangedSinceSelected;
    protected int wty = Integer.MIN_VALUE;
    protected int hy = Integer.MIN_VALUE;
    protected Point oldPoint;
    protected Graphics tempG;
    protected int activeMode;

    protected static final int NOTHING = 0;
    protected static final int WATER_TABLE = 1;
    protected static final int MINIMUM_PRESSURE_HEAD = 2;
    protected static final int VIA_DIALOG_BOX = 3;

    protected static final int SELECT_AND_EDIT = 0;
    protected static final int DISCRETIZE = 1;

    public vs2InitialEquilibriumProfileView(mp2View view, 
                vs2InitialEquilibriumProfileData profileData, 
                mp2RectilinearGridData gridData,
                String homeDirectory) {

        super(view, homeDirectory);
        this.profileData = profileData;
        this.gridData = gridData;

        equilibriumProfileMenuItem = 
                new JMenuItem("Equilibrium profile...");
        equilibriumProfileMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onEquilibriumProfile();
            }
        });


        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        selectAndEditButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/arrow.gif")), true);
        selectAndEditButton.setToolTipText("Select or Edit");
        selectAndEditButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(SELECT_AND_EDIT);
                }
            }
        });
        discretizeButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretize.gif")), false);
        discretizeButton.setToolTipText("Discretize");
        discretizeButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });

        ButtonGroup bg = new ButtonGroup();
        bg.add(selectAndEditButton);
        bg.add(discretizeButton);
		bg.add(zoomButton);

        equilibriumProfileButton = new mp2Button(
                new ImageIcon(ClassLoader.getSystemResource("images/watertable.gif")));
        equilibriumProfileButton.setToolTipText("set equilibrium profile");
        equilibriumProfileButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onEquilibriumProfile();
            }
        });
    }

    protected void onEquilibriumProfile() {
        EquilibriumProfileDialog dlg = new EquilibriumProfileDialog();
        double oldWaterTableLocation = profileData.getWaterTableLocation();
        double oldMinimumPressureHead = profileData.getMinimumPressureHead();
        dlg.wt = oldWaterTableLocation;
        dlg.minh = oldMinimumPressureHead;
        if (dlg.doModal() == true) {
            double newWaterTableLocation = dlg.wt;
            double newMinimumPressureHead = dlg.minh;
            undoSupport.postEdit(new DataEdit(oldWaterTableLocation,
                     newWaterTableLocation, oldMinimumPressureHead,
                     newMinimumPressureHead, undoCount));
            undoCount++;
            profileData.setWaterTableLocation(dlg.wt);
            profileData.setMinimumPressureHead(dlg.minh);
            view.repaint();
        }
    }

    public void onMouseDragged(MouseEvent e) {
        if (activeMode == DISCRETIZE) {
            return;
        }

        if (featureUnderMouse == NOTHING) {
            return;
        }
        // Erase the previous line by view over it,
        // except first time around.
        if (hasChangedSinceSelected) {
            tempG.drawLine(0, oldPoint.y, view.getDrawingWidthInPixels(),
                                      oldPoint.y);
        } else {
            hasChangedSinceSelected = true;
        }
        // If the line represents the minimum pressure head, make
        // sure this line does not go below the water table. Otherwise,
        // draw the new line.
        Point p = e.getPoint();
        if (! (featureUnderMouse == MINIMUM_PRESSURE_HEAD && p.y >= wty - 1)) {
            tempG.drawLine(0, p.y, view.getDrawingWidthInPixels(), p.y);
            // Save the curson position
            oldPoint = p;
        }
    }

    public void onMouseMoved(MouseEvent e) {
        if (activeMode == SELECT_AND_EDIT) {
        Point p = e.getPoint();
			if (Math.abs(wty - p.y) <= 1) {
			    view.setCursor(Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR));
			    featureUnderMouse = WATER_TABLE;
			}
			else if (Math.abs(hy - p.y) <= 1) {
			    view.setCursor(Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR));
			    featureUnderMouse = MINIMUM_PRESSURE_HEAD;
			}
			else {
			    view.setCursor(Cursor.getDefaultCursor());
			    featureUnderMouse = NOTHING;
			}
		}
    }

    public void onMousePressed(MouseEvent e) {
        if (activeMode == DISCRETIZE) {
            return;
        }

        if (featureUnderMouse == NOTHING) {
            return;
        }
        // Set up to draw in XOR mode
        Rectangle viewRect = view.getVisibleRect();
        tempG = view.getGraphics();
        tempG.clipRect(viewRect.x, viewRect.y, viewRect.width, 
                                               viewRect.height);
        tempG.setColor(Color.black);
        tempG.setXORMode(Color.white);
        hasChangedSinceSelected = false;
        oldPoint = e.getPoint();
    }

    public void onMouseReleased(MouseEvent e) {
        if (activeMode == DISCRETIZE) {
            return;
        }

        if (featureUnderMouse == NOTHING) {
            return;
        }

        else if (featureUnderMouse == WATER_TABLE) {
            Point p = e.getPoint();
            double [] v = view.viewToModel(p);
            double oldWaterTableLocation = profileData.getWaterTableLocation();
            double newWaterTableLocation = Double.valueOf(
                        mp2DecimalFormat.format(v[1])).doubleValue();
            undoSupport.postEdit(new DataEdit(oldWaterTableLocation,
                                          newWaterTableLocation, undoCount));
            undoCount++;
            profileData.setWaterTableLocation(newWaterTableLocation);
            wty = p.y;
            hy = view.modelToView(0, v[1] + 
                    profileData.getMinimumPressureHead()).y;
        }

        else if (featureUnderMouse == MINIMUM_PRESSURE_HEAD) {
            double [] v = view.viewToModel(oldPoint);
            double oldMinimumPressureHead = profileData.getMinimumPressureHead();
            double newMinimumPressureHead = Double.valueOf(
                    mp2DecimalFormat.format(v[1] - 
                    profileData.getWaterTableLocation())).doubleValue();
            undoSupport.postEdit(new DataEdit(oldMinimumPressureHead, 
                                        newMinimumPressureHead, undoCount));
            undoCount++;
            profileData.setMinimumPressureHead(newMinimumPressureHead);
            hy = oldPoint.y;
        }

        if (tempG != null) {
            tempG.dispose();
        }
        view.repaint();

    }

    public void paint(Graphics g) {
        Color oldColor = g.getColor();
        double waterTableLocation = profileData.getWaterTableLocation();
        double minPressureHead = profileData.getMinimumPressureHead();
        if (!isVisible 
                || waterTableLocation == Double.NEGATIVE_INFINITY
                || minPressureHead == Double.NEGATIVE_INFINITY) {
            return;
        }
        wty = view.modelToView(0, waterTableLocation).y;
        hy = view.modelToView(0, waterTableLocation + minPressureHead).y;

        Rectangle visibleRect = g.getClipBounds();
        if (view.isPaintModeDiscrete()) {
            double x0 = gridData.getXCoordAt(0);
            double y0 = gridData.getYCoordAt(0);
            double x1 = gridData.getXCoordAt(
                            gridData.getXCoordCount()-1);
            double y1 = gridData.getYCoordAt(
                            gridData.getYCoordCount()-1);
            Point p0 = view.modelToView(x0, y0);
            Point p1 = view.modelToView(x1, y1);
            g.setColor(Color.blue);
            g.fillRect(p0.x, wty, p1.x-p0.x, p1.y-wty);
            g.setColor(Color.red);
            g.fillRect(p0.x, hy-1, p1.x-p0.x, 3); 
        }
        else {
            // paint in Continuous mode. If view is active, color
            // the region below the water table blue, If view
            // is not active, just draw a blue line
            if (isActive) {
                g.setColor(Color.blue);
                g.fillRect(visibleRect.x, wty, 
                        Math.min(visibleRect.width, 
                                 view.getDrawingWidthInPixels()),
                        Math.min(visibleRect.height + visibleRect.y - wty,
                                 view.getDrawingHeightInPixels() - wty));
                g.setColor(Color.red);
                g.fillRect(visibleRect.x, hy-1, 
                            Math.min(visibleRect.width, 
                            view.getDrawingWidthInPixels()), 3);
            } else {
                g.setColor(new Color(128, 128, 255));
                g.fillRect(visibleRect.x, wty-1, 
                        Math.min(visibleRect.width, 
                                 view.getDrawingWidthInPixels()), 3);
                g.setColor(new Color(255, 128, 128));
                g.fillRect(visibleRect.x, hy-1, 
                            Math.min(visibleRect.width, 
                            view.getDrawingWidthInPixels()), 3);
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    public void prepareToActivate() {
        super.prepareToActivate();

        // Set up the "Edit" menu
        JMenu editMenu = view.getFrame().getMenu(EDIT_MENU);
        editMenu.removeAll();
        editMenu.add(undoMenuItem);
        editMenu.add(redoMenuItem);
        editMenu.addSeparator();
        editMenu.add(equilibriumProfileMenuItem);

        // Set up buttons on tool bar
        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(selectAndEditButton);
        if (gridData.isDefined()) {
            toolBar.add(discretizeButton);
        }
        toolBar.add(zoomButton);
        toolBar.add(Box.createVerticalStrut(10));
        toolBar.add(equilibriumProfileButton);

        if (view.isPaintModeDiscrete()) {
            discretizeButton.setSelected(true);
        } else {
            selectAndEditButton.setSelected(true);
        }

        view.setCursor(Cursor.getDefaultCursor());
    }

    /**
     * Sets the active mode
     */
    protected void setActiveModeTo(int m) {
        activeMode = m;
        boolean b = (activeMode == DISCRETIZE);
        view.setPaintModeToDiscrete(b);
        equilibriumProfileButton.setEnabled(!b && isEditable);
        equilibriumProfileMenuItem.setEnabled(!b && isEditable);
        view.repaint();
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        super.setEditable(b);
        equilibriumProfileButton.setEnabled(b);
    }

    /**
     * Dialog box to set equilibrium profile
     */
    protected class EquilibriumProfileDialog extends mp2Dialog {

        public double wt;
        public double minh;
        protected JTextField wtTextField;
        protected JTextField minhTextField;

        /**
         * Creates a dialog box.
         */
        public EquilibriumProfileDialog() {
            super("Initial Equilibrium Profile", false, view.getDoc().getData(MODEL_OPTIONS));
        }

        /**
         * Makes dialog box contents.
         */
        protected void makeContents() {

            vs2ModelOptions modelOptions = (vs2ModelOptions)customObject;
            
            GridBagLayout gridbag = new GridBagLayout();
            GridBagConstraints c = new GridBagConstraints();

            JPanel centerPanel = new JPanel(false);
            centerPanel.setLayout(gridbag);
            centerPanel.setBorder(new EmptyBorder(25, 25, 25, 25));
            getContentPane().add(centerPanel, BorderLayout.CENTER);
            
            JPanel leftPanel = new JPanel(new GridLayout(2, 1, 0, 10));
            centerPanel.add(leftPanel);
            c.fill = GridBagConstraints.VERTICAL;
            c.insets = new Insets(0, 0, 0, 10);
            c.gridwidth = GridBagConstraints.RELATIVE;
            gridbag.setConstraints(leftPanel, c);
            
            JPanel rightPanel = new JPanel(new GridLayout(2, 1, 0, 10));
            centerPanel.add(rightPanel);
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.insets = new Insets(0, 0, 0, 0);
            gridbag.setConstraints(rightPanel, c);
            
            JPanel unitsPanel = new JPanel(new GridLayout(2, 1, 0, 10));
            centerPanel.add(unitsPanel);
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(0, 5, 0, 5);
            gridbag.setConstraints(unitsPanel, c);
            
            String L = modelOptions.L();

            leftPanel.add(new JLabel("Z coordinate of water table", 
                                        SwingConstants.RIGHT));
            rightPanel.add(wtTextField = new JTextField(6));
            unitsPanel.add(new JLabel(L, SwingConstants.CENTER));

            leftPanel.add(new JLabel("minimum pressure head",
                                        SwingConstants.RIGHT));
            rightPanel.add(minhTextField = new JTextField(6));
            unitsPanel.add(new JLabel(L, SwingConstants.CENTER));
        }

        /**
         * Displays the dialog box with modal behavior.
         */
        public boolean doModal() {
            if (wt != Double.NEGATIVE_INFINITY) {
                wtTextField.setText(String.valueOf(wt));
            }
            if (minh != Double.NEGATIVE_INFINITY) {
                minhTextField.setText(String.valueOf(minh));
            }
            return super.doModal();
        }

        /**
         * Retrives data from dialog box components and checks validity.
         */
        protected boolean retrieveData() {
            try {
                wt = Double.valueOf(wtTextField.getText()).doubleValue();
                minh = Double.valueOf(minhTextField.getText()).doubleValue();
            }
            catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog("Please check your input",
                        "Input Error");
                return false;
            }
            if (!dataCheck(minh, "minimum pressure head", IS_NON_POSITIVE, 
                        minhTextField)) {
                return false;
            }

            return true;
        }
    }

    /**
     * The undoable edit via dialog box
     */
    protected class DataEdit extends AbstractDataEdit {

        protected double oldWaterTableLocation;
        protected double newWaterTableLocation;
        protected double oldMinimumPressureHead;
        protected double newMinimumPressureHead;
        protected int editType;

        public DataEdit(double oldValue, double newValue, long undoIndex) {
            super(undoIndex);
            if (featureUnderMouse == WATER_TABLE) {
                oldWaterTableLocation = oldValue;
                newWaterTableLocation = newValue;
                editType = WATER_TABLE;
            } else {
                oldMinimumPressureHead = oldValue;
                newMinimumPressureHead = newValue;
                editType = MINIMUM_PRESSURE_HEAD;
            }
        }

        public DataEdit(double oldWaterTableLocation,
                        double newWaterTableLocation, 
                        double oldMinimumPressureHead,
                        double newMinimumPressureHead, long undoIndex) {
            super(undoIndex);
            this.oldWaterTableLocation = oldWaterTableLocation;
            this.newWaterTableLocation = newWaterTableLocation;
            this.oldMinimumPressureHead = oldMinimumPressureHead;
            this.newMinimumPressureHead = newMinimumPressureHead;
            editType = VIA_DIALOG_BOX;
        }

        public void undo() throws CannotUndoException {
            switch (editType) {
            case VIA_DIALOG_BOX:
                profileData.setWaterTableLocation(oldWaterTableLocation);
                profileData.setMinimumPressureHead(oldMinimumPressureHead);
                break;
            case WATER_TABLE:
                profileData.setWaterTableLocation(oldWaterTableLocation);
                break;
            case MINIMUM_PRESSURE_HEAD:
                profileData.setMinimumPressureHead(oldMinimumPressureHead);
                break;
            default:
                return;
            }
            super.undo();
        }

        public void redo() throws CannotRedoException {
            switch (editType) {
            case VIA_DIALOG_BOX:
                profileData.setWaterTableLocation(newWaterTableLocation);
                profileData.setMinimumPressureHead(newMinimumPressureHead);
                break;
            case WATER_TABLE:
                profileData.setWaterTableLocation(newWaterTableLocation);
                break;
            case MINIMUM_PRESSURE_HEAD:
                profileData.setMinimumPressureHead(newMinimumPressureHead);
                break;
            default:
                return;
            }
            super.redo();
        }
    }

}