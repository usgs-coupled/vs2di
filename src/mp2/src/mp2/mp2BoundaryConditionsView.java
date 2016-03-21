/*
* mp2BoundaryConditionsView.java
*/
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.undo.*;

/**
 * Displays and manipulates boundary conditions.
 */
public abstract class mp2BoundaryConditionsView extends mp2GraphicalDataView {

    protected mp2BoundaryConditionsData boundaryConditionsData;
    protected mp2DomainData domainData;
    protected mp2AbstractGridData gridData;

    protected int capturedSegmentIndex;
    protected mp2Polygon capturedBoundary;
    protected int capturedBoundaryIndex;
    protected int startVertex;
    protected boolean [][] selectedSegmentArray;
    protected boolean [][] showValueArray;

    protected JMenuItem copyAllToMenuItem;
    protected JMenuItem cornerPolicyMenuItem;
    protected mp2ToggleButton selectAndEditButton;
    protected mp2ToggleButton discretizeButton;
    protected mp2Button setBCButton;
    protected mp2Button showAllBCValuesButton;
    protected mp2Button hideAllBCValuesButton;

    protected boolean selectingBoundarySegments;
    protected Graphics tempG;

    /**
     * Constructor
     */
    public mp2BoundaryConditionsView(mp2View view, 
                    mp2BoundaryConditionsData boundaryConditionsData, 
                    mp2AbstractGridData gridData,
                    mp2DomainData domainData,
                    String homeDirectory) {

        super(view, homeDirectory);
        this.boundaryConditionsData = boundaryConditionsData;
        this.domainData = domainData;
        this.gridData = gridData;

        // Create the "Copy All to" menu item
        copyAllToMenuItem = new JMenuItem("Copy All To...");
        copyAllToMenuItem.setEnabled(false);
        copyAllToMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCopyAllTo();
            }
        });

        // Create the "Corner Policy" menu item
        cornerPolicyMenuItem = new JMenuItem("Corner Policy...");
        cornerPolicyMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCornerPolicy();
            }
        });

        // Create toolbar buttons
        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        selectAndEditButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "arrow.gif"), true);
        discretizeButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "discretize.gif"), false);
        setBCButton = new mp2Button(new ImageIcon(imageDirectory + "bc.gif"));
        showAllBCValuesButton = 
                new mp2Button(new ImageIcon(imageDirectory + "showbc.gif"));
        hideAllBCValuesButton = 
                new mp2Button(new ImageIcon(imageDirectory + "hidebc.gif"));

        selectAndEditButton.setToolTipText("Select");
        discretizeButton.setToolTipText("Discretize");
        setBCButton.setToolTipText("Set boundary conditions");
        showAllBCValuesButton.setToolTipText("Show all BC values");
        hideAllBCValuesButton.setToolTipText("Hide all BC values");

        ButtonGroup bg = new ButtonGroup();
        bg.add(selectAndEditButton);
        bg.add(discretizeButton);
        bg.add(zoomButton);

        selectAndEditButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onSelectAndEdit();
                }
            }
        });
        discretizeButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onDiscretize();
                }
            }
        });
        setBCButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onSetBC();
            }
        });
        showAllBCValuesButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onShowAllBCValues();
            }
        });
        hideAllBCValuesButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onHideAllBCValues();
            }
        });
    }

    /**
     * Clears selected segments
     */
    public void clearSelectedSegments() {
        // Deselect all segments by setting the selected segment array
        // to false, and disable the Set BC button
        if (selectedSegmentArray != null) {
            for (int i=0; i<selectedSegmentArray.length; i++) {
                for (int j=0; j<selectedSegmentArray[i].length; j++) {
                    selectedSegmentArray[i][j] = false;
                }
            }
        }
        setBCButton.setEnabled(false);
        view.repaint();
    }

    public void deselectAll() {
        clearSelectedSegments();
    }

    /**
     * Evaluates the Edit menu of this data view. Enables or disables 
     * menu items and buttons as appropriate.
     */
    public void evaluateEditMenu() {
        clearSelectedSegments();
        // If there is only one period, disable the copy all to menu item
        if (domainData.getNumberOfShapes() > 0
                && boundaryConditionsData.getNumberOfPeriods() > 1) {
            copyAllToMenuItem.setEnabled(true);
        } else {
            copyAllToMenuItem.setEnabled(false);
        }
        // If there are no periods, disable the show/hide BC values button,
        // and set the show value array to false.
        boolean b = (boundaryConditionsData.getNumberOfPeriods() > 0);
        showAllBCValuesButton.setEnabled(b);
        hideAllBCValuesButton.setEnabled(b);
        if (!b) {
            onHideAllBCValues();
        }
        view.repaint();
    }

    /**
     * Gets the BC color for the specified segment
     */
    protected abstract Color getBCColor(int boundaryIndex, int segmentIndex);

    /**
     * Gets the BC value for the selected period at the specified boundary
     * and specified segment
     */
    public abstract String getLabelForSelectedPeriodBC(int boundaryIndex, 
                                                        int segmentIndex);

    /**
     * Handle the "Copy All To" menu item. Copies the BC for all
     * boundaries of the selected period to user specified periods.
     */
    protected void onCopyAllTo() {

        int i;
        int selectedPeriod = boundaryConditionsData.getSelectedPeriod();

        CopyBCDialog dlg = new CopyBCDialog(selectedPeriod+1);

        for (i=0; i<boundaryConditionsData.getNumberOfPeriods(); i++) {
            if (i != selectedPeriod) {
                dlg.periods.addElement("Period " + String.valueOf(i+1));
            }
        }

        if (dlg.doModal() == true) {
            Vector oldBC = boundaryConditionsData.getClonedBC();
            int [] index = dlg.periodList.getSelectedIndices();
            int p;
            for (i=0; i<index.length; i++) {
                p = index[i];
                if (p >= selectedPeriod) {
                    p++;
                }
                boundaryConditionsData.copySelectedPeriodBCTo(p);
            }
            Vector newBC = boundaryConditionsData.getClonedBC();
            undoSupport.postEdit(new BCEdit(oldBC, newBC, undoCount));
            undoCount++;
        }
    }

    /**
     * Handle the "Corner Policy" menu item
     */
    protected void onCornerPolicy() {
        CornerPolicyDialog dlg = new CornerPolicyDialog();
        dlg.verticalControl = boundaryConditionsData.getVerticalBoundaryControlsCorner();
        if (dlg.doModal() == true) {
            if (dlg.verticalControl != 
                        boundaryConditionsData.getVerticalBoundaryControlsCorner()) {
                boundaryConditionsData.setVerticalBoundaryControlsCorner(dlg.verticalControl);
                undoSupport.postEdit(new CornerPolicyEdit(dlg.verticalControl,
                        undoCount));
                undoCount++;
                view.repaint();
            }
        }
    }

    /**
     * Invoked when the discretize button is pressed
     */
    protected void onDiscretize() {
        for (int i=0; i<selectedSegmentArray.length; i++) {
            for (int j=0; j<selectedSegmentArray[i].length; j++) {
                selectedSegmentArray[i][j] = false;
            }
        }
        setBCButton.setEnabled(false);
        view.setCursor(Cursor.getDefaultCursor());
        view.setPaintModeToDiscrete(true);
        view.repaint();
    }

    /**
     * hides all bc values
     */
    protected void onHideAllBCValues() {
        if (showValueArray != null) {
            for (int i=0; i<showValueArray.length; i++) {
                for (int j=0; j<showValueArray[i].length; j++) {
                    showValueArray[i][j] = false;
                }
            }
            view.repaint();
        }
    }

    /**
     * Invoked when the mouse is moved with the button down
     */
    public void onMouseDragged(MouseEvent e) {
        if (selectingBoundarySegments) {
            capturedSegmentIndex = 
                        capturedBoundary.getSegmentUnderPoint(
                        view.viewToModel(e.getPoint()), 10);
            if (capturedSegmentIndex < 0) {
                return;
            }
            if (selectedSegmentArray[capturedBoundaryIndex][capturedSegmentIndex] 
                    == true) {
                return;
            }

            selectedSegmentArray[capturedBoundaryIndex][capturedSegmentIndex] = true;
            tempG.setColor(Color.lightGray);
            capturedBoundary.drawThickSegment(capturedSegmentIndex, 10, tempG);
            capturedBoundary.draw(tempG);
            capturedBoundary.drawOpenHandles(tempG);
        }
    }

    /** 
     * Invoked when the mouse has moved on the view with no 
     * buttons down.
     */
    public void onMouseMoved(MouseEvent e) {
        capturedSegmentIndex = -1;

        if (boundaryConditionsData.getSelectedPeriod() == -1) {
            return;
        }

        if (discretizeButton.isSelected()) {
            return;
        }

        // "Capture" the edge of a poly-vertex shape. If captured,
        // change cursor to the hand cursor.
        for (int i=0; i<domainData.getNumberOfShapes(); i++) {
            mp2Polygon boundary = domainData.getBoundary(i);
            capturedSegmentIndex = boundary.getSegmentUnderPoint(
                                  view.viewToModel(e.getPoint()), 10);
            // change cursor if an edge is captured
            if (capturedSegmentIndex > -1) {
                view.setCursor(Cursor.getPredefinedCursor(
                                            Cursor.HAND_CURSOR));
                capturedBoundaryIndex = i;
                capturedBoundary = boundary;
                return;
            } else {
                view.setCursor(Cursor.getDefaultCursor());
            }
        }
        return;
    }

    /** 
     * Invoked when the mouse button is pressed
     */
    public void onMousePressed(MouseEvent e) {

        selectingBoundarySegments = false;

        if (boundaryConditionsData.getSelectedPeriod() == -1) {
            return;
        }

        if (discretizeButton.isSelected()) {
            return;
        }

        // First, clear the selected segment array
        clearSelectedSegments();
        view.repaint();

        if (capturedSegmentIndex == -1) {
            setBCButton.setEnabled(false);
            return;
        }

        // If ALT key is hold down (or if clicking right button), 
        //then toggle show/hide bc label
        if (e.isAltDown() || e.getModifiers()==InputEvent.BUTTON3_MASK) {
            showValueArray[capturedBoundaryIndex][capturedSegmentIndex] = 
                !showValueArray[capturedBoundaryIndex][capturedSegmentIndex];
            view.repaint();
            return;
        }

        selectingBoundarySegments = true;
        startVertex = capturedSegmentIndex;
        selectedSegmentArray[capturedBoundaryIndex][capturedSegmentIndex] = true;
        if (tempG != null) {
            tempG.dispose();
        }
        tempG = view.getGraphics();
        tempG.setColor(Color.lightGray);
        Rectangle viewRect = view.getVisibleRect();
        tempG.clipRect(viewRect.x, viewRect.y, viewRect.width, viewRect.height);
        capturedBoundary.drawThickSegment(capturedSegmentIndex, 10, tempG);
        capturedBoundary.draw(tempG);
        capturedBoundary.drawOpenHandles(tempG);
        setBCButton.setEnabled(true);
    }

    /** 
     * Invoked when the mouse button is released
     */
    public void onMouseReleased(MouseEvent e) {
        if (selectingBoundarySegments) {
            if (tempG != null) {
                tempG.dispose();
                tempG = null;
            }
            selectingBoundarySegments = false;
            view.repaint();
            if (e.getClickCount() == 2) {
                onSetBC();
            }
        }
    }

    /**
     * Invoked when the selectAndEditButton is clicked
     */
    protected void onSelectAndEdit() {
        view.setPaintModeToDiscrete(false);
        view.repaint();
    }

    /**
     * Invoked when the setBCButton is clicked
     */
    public abstract void onSetBC();

    /**
     * Shows all bc values
     */
    protected void onShowAllBCValues() {
        if (showValueArray != null) {
            for (int i=0; i<showValueArray.length; i++) {
                mp2Polygon boundary = domainData.getBoundary(i);
                for (int j=0; j<showValueArray[i].length; j++) {
                   showValueArray[i][j] = true;
                }
            }
            view.repaint();
        }
    }

    /**
     * Paints the boundary conditions
     */
    public void paint(Graphics g) {

        Color oldColor = g.getColor();

        if (!isVisible) {
            return;
        }

        if (boundaryConditionsData.getBCForSelectedPeriod() == null) {
            return;
        }

        if (view.isPaintModeDiscrete()) {
            drawDiscretizedShapes(g);
        } else {
            for (int i=0; i<domainData.getNumberOfShapes(); i++) {
                mp2Polygon boundary = domainData.getBoundary(i);
                for (int j=0; j<boundary.getNumberOfVertices(); j++) {
                    Color color = getBCColor(i, j);
                    if (color != null) {
                        g.setColor(color);
                        boundary.drawThickSegment(j, 10, g);
                    }
                }
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    protected void drawDiscretizedShapes(Graphics g) {
        if (!(gridData instanceof mp2RectilinearGridData)) {
            return;
        }
        boundaryConditionsData.discretize();
        double [] xCoord = ((mp2RectilinearGridData) gridData).getXCoords();
        double [] yCoord = ((mp2RectilinearGridData) gridData).getYCoords();
        int numCol = xCoord.length - 1;
        int numRow = yCoord.length - 1;
        int i, j, k ,m, row, col;
        Vector boundaryCellList = 
                boundaryConditionsData.getBoundaryCellListForPeriod(
                boundaryConditionsData.getSelectedPeriod());
        Point p1, p2;
        Color color;
        for (i = 0; i<boundaryCellList.size(); i++) {
            Vector cellsOfSegment = (Vector) boundaryCellList.elementAt(i);
            for (j=0; j<cellsOfSegment.size(); j++) {
                int [] cell = (int []) cellsOfSegment.elementAt(j);
                color = getBCColor(i, j);
                if (color != null) {
                    g.setColor(color);
                    for (m=0; m<cell.length; m++) {
                        row = cell[m]/numCol;
                        col = cell[m] - row*numCol;
                        if (view.getModelVerticalAxisOrientation() == mp2Drawing.MODEL_VERTICAL_AXIS_UPWARD_POSITIVE) {
                            p1 = view.modelToView(xCoord[col], yCoord[row+1]);
                            p2 = view.modelToView(xCoord[col+1], yCoord[row]);
                        } else {
                            p1 = view.modelToView(xCoord[col], yCoord[row]);
                            p2 = view.modelToView(xCoord[col+1], yCoord[row+1]);
                        }
                        g.fillRect(p1.x, p1.y, p2.x - p1.x, p2.y - p1.y);
                    }
                }
            }
        }
    }

    /**
     * Paints on top of everything else. Draws the gray highlight
     * of selected segments.
     */
    public void paintLast(Graphics g) {
        Color oldColor = g.getColor();

        if (boundaryConditionsData.getBCForSelectedPeriod() != null) {
            for (int i=0; i<domainData.getNumberOfShapes(); i++) {
                mp2Polygon boundary = domainData.getBoundary(i);
                for (int j=0; j<boundary.getNumberOfVertices(); j++) {
                    if (selectedSegmentArray[i][j]) {
                        g.setColor(Color.lightGray);
                        boundary.drawThickSegment(j, 10, g);
                    }
                    if (showValueArray[i][j]) {
                        g.setColor(Color.black);
                        Point p = boundary.getSegmentLabelPosition(j, 6);
                        g.drawString(getLabelForSelectedPeriodBC(i, j), 
                                     p.x, p.y);
                    }

                }
                boundary.draw(g);
            }
        }

        for (int i=0; i<domainData.getNumberOfShapes(); i++) {
            mp2Polygon boundary = domainData.getBoundary(i);
            boundary.drawOpenHandles(g);
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    /**
     * Loads the edit menu and toolbar when this data view become active
     */
    public void prepareToActivate() {
        super.prepareToActivate();

        // Create the arrays for selected segment array and value mask
        selectedSegmentArray = new boolean [domainData.getNumberOfShapes()][];
        showValueArray = new boolean [domainData.getNumberOfShapes()][];
        for (int i=0; i<domainData.getNumberOfShapes(); i++) {
            mp2Polygon boundary = domainData.getBoundary(i);
            selectedSegmentArray[i] = new boolean[boundary.getNumberOfVertices()];
            showValueArray[i] = new boolean[boundary.getNumberOfVertices()];
        }

        // Set up the "Edit" menu on the menu bar
        JMenu editMenu = view.getFrame().getMenu(EDIT_MENU);
        editMenu.removeAll();
        editMenu.add(undoMenuItem);
        editMenu.add(redoMenuItem);
        editMenu.addSeparator();
        editMenu.add(copyAllToMenuItem);
        editMenu.add(cornerPolicyMenuItem);

        // Set up buttons on tool bar
        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(selectAndEditButton);
        if (gridData.isDefined()) {
            toolBar.add(discretizeButton);
        }
        toolBar.add(zoomButton);
        toolBar.add(Box.createVerticalStrut(10));
        toolBar.add(setBCButton);
        toolBar.add(Box.createVerticalStrut(10));
        toolBar.add(showAllBCValuesButton);
        toolBar.add(hideAllBCValuesButton);

        if (view.isPaintModeDiscrete()) {
            discretizeButton.setSelected(true);
        } else {
            selectAndEditButton.setSelected(true);
        }
        evaluateEditMenu();
        setBCButton.setEnabled(false);
        view.repaint();
        view.setCursor(Cursor.getDefaultCursor());

    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        super.setEditable(b);
        if (!b) {
            if (selectedSegmentArray != null) {
                for (int i=0; i<selectedSegmentArray.length; i++) {
                    for (int j=0; j<selectedSegmentArray[i].length; j++) {
                        selectedSegmentArray[i][j] = false;
                    }
                }
            }
            setBCButton.setEnabled(false);
        }
    }

    /**
     * Dialog box to copy the BC's from one period to other periods.
     */
    public class CopyBCDialog extends mp2Dialog {

        /**
         * List of possible target periods, which does not
         * include the selected period
         */
        JList periodList;
        Vector periods;

        /**
         * Constructs the dialog box to copy BC'c 
         */
        public CopyBCDialog(int period) {
            super("Copy BC", true, new Integer(period));
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "copyBoundaryConditions", null);
        }

        /**
         * Make contents for this dialog box
         */
        protected void makeContents() {
            int period = ((Integer) customObject).intValue();
            // Make a center panel to hold all the components.
            JPanel centerPanel = new JPanel(false);
            centerPanel.setLayout(new BorderLayout(0, 20));
            centerPanel.setBorder(new EmptyBorder(25, 25, 10, 25));
            getContentPane().add(centerPanel, BorderLayout.CENTER);

            centerPanel.add(new JLabel("Copy all BC's from period " + period + " to:"),
                                            BorderLayout.NORTH);
            periods = new Vector();
            periodList = new JList(periods);
            periodList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
            JScrollPane scrollPane = new JScrollPane(periodList);
            scrollPane.setPreferredSize(new Dimension(100, 200));
            centerPanel.add(scrollPane, BorderLayout.CENTER);

        }

        protected void onBrowserHelp() {
            mp2HelpWindow.showHelpFile("copyBoundaryConditions.html");
        }

        /**
         * Implementation required by super class. Do nothing here
         */
        protected boolean retrieveData() {
            return true;
        }
    }

    /**
     * The undoable edit to edit boundary conditions
     */
    protected class BCEdit extends AbstractDataEdit {

        protected Vector oldBC;
        protected Vector newBC;

        public BCEdit(Vector oldBC, Vector newBC, long undoIndex) {
            super(undoIndex);
            this.oldBC = oldBC;
            this.newBC = newBC;
        }

        public void undo() throws CannotUndoException {
            boundaryConditionsData.setBC(oldBC);
            super.undo();
        }

        public void redo() throws CannotRedoException {
            boundaryConditionsData.setBC(newBC);
            super.redo();
        }
    }

    /**
     * The undoable edit to change corner policy
     */
    protected class CornerPolicyEdit extends AbstractDataEdit {

        protected boolean newCornerPolicy;

        public CornerPolicyEdit(boolean newCornerPolicy, long undoIndex) {
            super(undoIndex);
            this.newCornerPolicy = newCornerPolicy;
        }

        public void undo() throws CannotUndoException {
            boundaryConditionsData.setVerticalBoundaryControlsCorner(!newCornerPolicy);
            super.undo();
        }

        public void redo() throws CannotRedoException {
            boundaryConditionsData.setVerticalBoundaryControlsCorner(newCornerPolicy);
            super.redo();
        }
    }

    /**
     * Dialog box to set corner policy
     */
    protected class CornerPolicyDialog extends mp2Dialog {
        public boolean verticalControl;
        protected JRadioButton verticalControlRadioButton;
        protected JRadioButton horizontalControlRadioButton;

        public CornerPolicyDialog() {
            super("Corner Policy", true);
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "setCornerPolicy", null);
        }

        protected void makeContents() {
            JPanel centerPanel = new JPanel(false);
            GridBagLayout gridbag = new GridBagLayout();
            centerPanel.setLayout(gridbag);
            centerPanel.setBorder(new EmptyBorder(15, 15, 15, 15));
            getContentPane().add(centerPanel, BorderLayout.CENTER);

            JPanel panel = new JPanel(new GridLayout(3, 1));
            centerPanel.add(panel);
            panel.add(new JLabel("During discretization, assign corner cells to"));
            panel.add(verticalControlRadioButton = 
                new JRadioButton("Vertical boundary"));
            panel.add(horizontalControlRadioButton = 
                new JRadioButton("Horizontal boundary"));
            ButtonGroup bg = new ButtonGroup();
            bg.add(verticalControlRadioButton);
            bg.add(horizontalControlRadioButton);
        }

        public boolean doModal() {
            verticalControlRadioButton.setSelected(verticalControl);
            horizontalControlRadioButton.setSelected(!verticalControl);
            return super.doModal();
        }

        protected boolean retrieveData() {
            verticalControl = verticalControlRadioButton.isSelected();
            return true;
        }

        protected void onBrowserHelp() {
            mp2HelpWindow.showHelpFile("setCornerPolicy.html");
        }
    }

}