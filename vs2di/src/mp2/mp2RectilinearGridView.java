/*
 * mp2RectilinearGridView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.undo.*;

/**
 * Manages a rectilinear (finite-difference) grid.
 */
public class mp2RectilinearGridView extends mp2GraphicalDataView {

    protected mp2RectilinearGridData gridData;
    protected mp2DomainData domainData;
    protected mp2RectilinearGridWindowInterface gridWindow;

    protected JMenuItem uniformGridMenuItem;
    protected JMenuItem subdivideMenuItem;
    protected JMenu selectAllMenu;
    protected JMenuItem deleteMenuItem;

    protected mp2ToggleButton editGridButton;
    protected mp2ToggleButton editLineButton;
    protected mp2ToggleButton addHLineButton;
    protected mp2ToggleButton addVLineButton;
    protected mp2ToggleButton gridWindowButton;
    protected mp2ToggleButton discretizeButton;

    protected int activeMode;
    protected Graphics tempG;
    protected boolean hasChanged;

    protected GridRect gridRect;       // represents the grid outline
    protected boolean mouseOverGrid;   // use when moving the grid
    protected int capturedHandle;      // used when resizing the grid
    protected Point oldPoint;

    protected int [] ix;            // x coordinates of vertical grid lines
    protected int [] iy;            // y coordinates of horizontal grid lines
    // index of the captured grid line, counted along screen coordinates
    protected int capturedScreenIndex;
    // index of the captured grid line, counted along coordinate data
    protected int capturedCoordIndex;
    protected int capturedOrientation;    // orientation of the captured grid line

    protected static final int EDIT_GRID = 0;
    protected static final int EDIT_LINE = 1;
    protected static final int ADD_HLINE = 2;
    protected static final int ADD_VLINE = 3;
    protected static final int VERTICAL = 0;
    protected static final int HORIZONTAL = 1;
    protected static final int XCOORD = 0;
    protected static final int YCOORD = 1;

    protected static final int VOID_HANDLE = -1;
    protected static final int NW_HANDLE =  0;
    protected static final int N_HANDLE  =  1;
    protected static final int NE_HANDLE =  2;
    protected static final int E_HANDLE  =  3;
    protected static final int SE_HANDLE =  4;
    protected static final int S_HANDLE  =  5;
    protected static final int SW_HANDLE =  6;
    protected static final int W_HANDLE  =  7;

    /**
     * Constructor
     */
    public mp2RectilinearGridView(mp2View view,
                                  mp2RectilinearGridData gridData,
                                  mp2DomainData domainData,
                                  String homeDirectory)  {
        super(view, homeDirectory);
        this.gridData = gridData;
        this.domainData = domainData;
        if (System.getProperty ("os.name").startsWith ("Windows")) {
            gridWindow = new mp2RectilinearGridWindowWin32(this, gridData,
                view, mp2View.getVerticalAxisName());
        } else {
            gridWindow = new mp2RectilinearGridWindowUnix(this, gridData,
                view, mp2View.getVerticalAxisName());
        }
        gridRect = new GridRect();

        // Create menu items
        uniformGridMenuItem = new JMenuItem("Uniform Grid...");
        uniformGridMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onUniformGrid();
            }
        });
        subdivideMenuItem = new JMenuItem("Subdivide...");
        subdivideMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onSubdivide();
            }
        });

        selectAllMenu = new JMenu("SelectAll");
        JMenuItem item = new JMenuItem("Vertical grid lines");
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                selectAllGridLines(VERTICAL);
            }
        });
        selectAllMenu.add(item);
        item = new JMenuItem("Horizontal grid lines");
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                selectAllGridLines(HORIZONTAL);
            }
        });
        selectAllMenu.add(item);

        deleteMenuItem = new JMenuItem("Delete");
        deleteMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
        deleteMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDelete();
            }
        });

        // Create buttons for tool bar
        String fileSeparator  = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator + "images" + fileSeparator;

        editGridButton = new mp2ToggleButton(
                            new ImageIcon(ClassLoader.getSystemResource("images/movegrid.gif")));
        editLineButton = new mp2ToggleButton(
                            new ImageIcon(ClassLoader.getSystemResource("images/moveline.gif")));
        addVLineButton = new mp2ToggleButton(
                            new ImageIcon(ClassLoader.getSystemResource("images/addvline.gif")));
        addHLineButton = new mp2ToggleButton(
                            new ImageIcon(ClassLoader.getSystemResource("images/addhline.gif")));
        gridWindowButton = new mp2ToggleButton(
                            new ImageIcon(ClassLoader.getSystemResource("images/gridwindow.gif")));
        discretizeButton = new mp2ToggleButton(
                            new ImageIcon(ClassLoader.getSystemResource("images/discretize.gif")));

        ButtonGroup bg = new ButtonGroup();
        bg.add(editGridButton);
        bg.add(editLineButton);
        bg.add(addVLineButton);
        bg.add(addHLineButton);
        bg.add(zoomButton);

        editGridButton.setToolTipText("Move or resize the grid");
        editLineButton.setToolTipText("Move grid lines");
        addVLineButton.setToolTipText("Add vertical grid lines");
        addHLineButton.setToolTipText("Add horizontal grid lines");
        gridWindowButton.setToolTipText("show or hide the Grid Window");
        discretizeButton.setToolTipText("Discretize");

        editGridButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveMode(EDIT_GRID);
                }
            }
        });
        editLineButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveMode(EDIT_LINE);
                }
            }
        });
        addVLineButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveMode(ADD_VLINE);
                }
            }
        });
        addHLineButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveMode(ADD_HLINE);
                }
            }
        });
        gridWindowButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onGridWindow(e);
            }
        });
        discretizeButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onDiscretize(e.getStateChange() == ItemEvent.SELECTED);
            }
        });
    }

    /**
     * Add an x coordinate to the grid data and post undoable edit
     * Returns the index of the added x coordinate
     */
    public int addXCoordWithUndo(double x) {
        int index = gridData.addXCoord(x);
        undoSupport.postEdit(new LineAddEdit(x, index,
                                      VERTICAL, undoCount));
        undoCount++;
        return index;
    }

    /**
     * Add a y coordinate to the grid data and post undoable edit.
     * Returns the index of the added y coordinate
     */
    public int addYCoordWithUndo(double y) {
        int index = gridData.addYCoord(y);
        undoSupport.postEdit(new LineAddEdit(y, index,
                                    HORIZONTAL, undoCount));
        undoCount++;
        return index;
    }

    /**
     * Change a x coordinate and post undoable edit
     * Returns the index of the changed x coordinate
     */
    public int changeXCoordWithUndo(double newX, int oldIndex) {
        // Delete the old coordinate and save it
        double oldX = gridData.deleteXCoordAt(oldIndex);
        // Add the new coordinate and save its index
        int newIndex = gridData.addXCoord(newX);
        // post undo
        undoSupport.postEdit(new LineMoveEdit(
            newX, oldX, newIndex, oldIndex,
            VERTICAL, undoCount));
        undoCount++;
        return newIndex;
    }

    /**
     * Change a y coordinate and post undoable edit.
     * Returns the index of the changed y coordinate
     */
    public int changeYCoordWithUndo(double newY, int oldIndex) {
        // Delete the old coordinate and save it
        double oldY = gridData.deleteYCoordAt(oldIndex);
        // Add the new coordinate and save its index
        int newIndex = gridData.addYCoord(newY);
        // post undo
        undoSupport.postEdit(new LineMoveEdit(
            newY, oldY, newIndex, oldIndex,
            HORIZONTAL, undoCount));
        undoCount++;
        return newIndex;
    }

    /**
     * Make x and y coordinates in screen (pixel) units
     * that corresponds to model coordinates.
     */
    protected void createScreenCoordinates() {
        double [] x = gridData.getXCoords();
        double [] y = gridData.getYCoords();
        double xMin = x[0];
        double yMin = y[0];
        double xMax = x[x.length-1];
        double yMax = y[y.length-1];
        Point p0, p1;
        ix = new int [x.length];
        for (int i=0; i<x.length; i++) {
            ix[i] = view.modelToView(x[i], 0).x;
        }
        iy = new  int [y.length];
        if (modelVerticalAxisUpward()) {
            p0 = view.modelToView(xMin, yMax);
            p1 = view.modelToView(xMax, yMin);
            for (int i=0; i<y.length; i++) {
                iy[i] = view.modelToView(0, y[y.length-i-1]).y;
            }
        } else {
            p0 = view.modelToView(xMin, yMin);
            p1 = view.modelToView(xMax, yMax);
            for (int i=0; i<y.length; i++) {
                iy[i] = view.modelToView(0, y[i]).y;
            }
        }
        gridRect.updateHandles(p0, p1);
    }

    public void deselectAll() {
        gridRect.isSelected = false;
        gridWindow.deselectAllCoords();
    }

    /**
     * Deselects the grid window button
     */
    public void deselectGridWindowButton() {
        gridWindowButton.setSelected(false);
    }

    /**
     * Disposes of the grid window. Called when the program exits
     */
    public void disposeGridWindow() {
        gridWindow.doDispose();
    }

    /**
     * Dispose temporary graphics
     */
    protected void disposeTempG() {
        if (tempG != null) {
            tempG.dispose();
            tempG = null;
        }
    }

    /**
     * Draw the handles of a grid line
     */
    public void drawVerticalGridLineHandles(Graphics g, int coordIndex) {
        int screenIndex = coordIndex;
        g.fillRect(ix[screenIndex] - HALF_HANDLE_SIZE,
                   iy[0] - HALF_HANDLE_SIZE,
                   HANDLE_SIZE, HANDLE_SIZE);
        g.fillRect(ix[screenIndex] - HALF_HANDLE_SIZE,
                   iy[iy.length-1] - HALF_HANDLE_SIZE,
                   HANDLE_SIZE, HANDLE_SIZE);

        g.drawLine(ix[screenIndex], iy[0] + HALF_HANDLE_SIZE + 1,
              ix[screenIndex], iy[iy.length-1] - HALF_HANDLE_SIZE -1);
    }

    /**
     * Draw the handles of a horizontal grid line
     */
    public void drawHorizontalGridLineHandles(Graphics g, int coordIndex) {
        int screenIndex = coordIndex;
        if (modelVerticalAxisUpward()) {
            screenIndex = gridData.getYCoordCount() - screenIndex - 1;
        }
        g.fillRect(ix[0] - HALF_HANDLE_SIZE,
                   iy[screenIndex] - HALF_HANDLE_SIZE,
                   HANDLE_SIZE, HANDLE_SIZE);
        g.fillRect(ix[ix.length-1] - HALF_HANDLE_SIZE,
                   iy[screenIndex] - HALF_HANDLE_SIZE,
                   HANDLE_SIZE, HANDLE_SIZE);
        g.drawLine(ix[0] + HALF_HANDLE_SIZE+1, iy[screenIndex],
             ix[ix.length-1] - HALF_HANDLE_SIZE -1, iy[screenIndex]);
    }

    /**
     * Draw handles of selected grid lines
     */
    protected void drawSelectedGridLinesHandles(Graphics g) {
        int [] coordIndices = gridWindow.getSelectedXCoordIndices();
        for (int i=0; i<coordIndices.length; i++) {
            drawVerticalGridLineHandles(g, coordIndices[i]);
        }
        coordIndices = gridWindow.getSelectedYCoordIndices();
        for (int i=0; i<coordIndices.length; i++) {
            drawHorizontalGridLineHandles(g, coordIndices[i]);
        }
    }

    /**
     * Evaluates the Edit menu of this data view. Enables or disables
     * menu items as appropriate.
     */
    public void evaluateEditMenu() {
        if (activeMode == EDIT_LINE) {
            selectAllMenu.setEnabled(true);
            int numSelected = gridWindow.getSelectedXCoordIndices().length;
            if (numSelected > 0) {
                subdivideMenuItem.setEnabled(numSelected >= 2);
                deleteMenuItem.setEnabled(numSelected <=
                                    gridData.getXCoordCount() - 2);
            } else {
                numSelected = gridWindow.getSelectedYCoordIndices().length;
                if (numSelected > 0) {
                    subdivideMenuItem.setEnabled(numSelected >= 2);
                    deleteMenuItem.setEnabled(numSelected <=
                                    gridData.getYCoordCount() - 2);
                } else {
                    deleteMenuItem.setEnabled(false);
                    subdivideMenuItem.setEnabled(false);
                }
            }
        } else {
            selectAllMenu.setEnabled(false);
            deleteMenuItem.setEnabled(false);
            subdivideMenuItem.setEnabled(false);
        }
        gridWindow.setDeleteButtonEnabled(deleteMenuItem.isEnabled());
        gridWindow.setSubdivideButtonEnabled(subdivideMenuItem.isEnabled());
    }

    /**
     * Hides the grid window
     */
    public void hideGridWindow() {
        gridWindowButton.setSelected(false);
    }

    /**
     * Create temporary graphics (XOR) for drawing
     */
    protected void makeTempG(boolean doXOR) {
        tempG = view.getGraphics();
        if (doXOR) {
            tempG.setColor(Color.black);
            tempG.setXORMode(Color.white);
        }
        Rectangle viewRect = view.getVisibleRect();
        tempG.clipRect(viewRect.x, viewRect.y, viewRect.width, viewRect.height);
    }

    protected void onDelete() {
        int [] coordIndices = gridWindow.getSelectedXCoordIndices();

        if (coordIndices.length > 0) {
            if ((gridData.getXCoordCount() - coordIndices.length) < 2) {
                mp2MessageBox.showMessageDialog("Cannot delete. At least " +
                    "2 vertical grid lines must remain.", "Error");
                return;
            }
            double [] deletedXCoords = gridData.deleteXCoords(coordIndices);
            undoSupport.postEdit(new LineDeleteEdit(
                    deletedXCoords, coordIndices, VERTICAL, undoCount));
            undoCount++;
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllXCoords();
        } else {
            coordIndices = gridWindow.getSelectedYCoordIndices();
            if (coordIndices.length > 0) {
                if ((gridData.getYCoordCount() - coordIndices.length) < 2) {
                    mp2MessageBox.showMessageDialog("Cannot delete. At least " +
                        "2 horizontal grid lines must remain.", "Error");
                    return;
                }
                double [] deletedYCoords =
                        gridData.deleteYCoords(coordIndices);
                undoSupport.postEdit(new LineDeleteEdit(
                        deletedYCoords, coordIndices, HORIZONTAL, undoCount));
                undoCount++;
                gridWindow.fireGridDataChanged();
                gridWindow.deselectAllYCoords();
            }
        }
        createScreenCoordinates();
        evaluateEditMenu();
        view.repaint();
        gridWindow.doRepaint();
    }

    /**
     * Invoked when the discretize button is selected or unselected
     */
    protected void onDiscretize(boolean b) {
        view.setPaintModeToDiscrete(b);
        view.repaint();
    }

    /**
     * Shows or hide the grid window in response to the grid window button
     */
    protected void onGridWindow(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.SELECTED
                && !gridWindow.isWindowVisible()) {
            gridWindow.setWindowVisible(true);
        }
        else if (e.getStateChange() == ItemEvent.DESELECTED
                && gridWindow.isWindowVisible()) {
            gridWindow.setWindowVisible(false);
        }
    }

    /**
     * Invoked when the mouse is dragged
     */
    public void onMouseDragged(MouseEvent e) {

        Point p = e.getPoint();

        if (activeMode == EDIT_GRID) {
            // Resize the grid
            if (capturedHandle != VOID_HANDLE) {
                // Erase the previous shape by drawing over it, except
                // first time around
                if (hasChanged) {
                    gridRect.draw(tempG);
                } else {
                    hasChanged = true;
                }
                // Erase the previous handles by drawing over them
                gridRect.drawHandles(tempG);

                // Resize the grid outline
                gridRect.resize(capturedHandle, p,
                                gridData.getUseRadialCoord());

                // Draw the new grid outline and handles
                gridRect.draw(tempG);
                gridRect.drawHandles(tempG);
                return;
            }
            // Move the grid
            if (mouseOverGrid) {
                // Erase the previous rect by drawing over it. Skip this
                // the first time around.
                if (hasChanged) {
                    gridRect.draw(tempG);
                } else {
                    // Select the grid (and draw handles) if the grid is
                    // not already selected,
                    if (!gridRect.isSelected) {
                        gridRect.drawHandles(tempG);
                        gridRect.isSelected = true;
                    }
                    hasChanged = true;
                }
                // Translate the grid outline. Prevent movement in
                // x direction if using radial coordinates.
                if (!gridData.getUseRadialCoord()) {
                    gridRect.moveX(p.x - oldPoint.x);
                }
                gridRect.moveY(p.y - oldPoint.y);

                // Draw the new grid outline
                gridRect.draw(tempG);
                // Save the cursor position
                oldPoint = p;
                return;
            }
        }

        else if (activeMode == EDIT_LINE) {
            // Move the captured grid line
            if (capturedScreenIndex > -1) {
                // erase the previous line by drawing over it.
                // Skip the first time around
                if (hasChanged) {
                    if (capturedOrientation == VERTICAL) {
                        tempG.drawLine(ix[capturedScreenIndex], iy[0],
                                       ix[capturedScreenIndex], iy[iy.length-1]);
                    } else {
                        tempG.drawLine(ix[0], iy[capturedScreenIndex],
                                       ix[ix.length-1], iy[capturedScreenIndex]);
                    }
                } else {
                    // deselect all grid lines that were previously selected.
                    // Erase their handles by drawing over them.
                    drawSelectedGridLinesHandles(tempG);
                    // Select the captured grid line
                    if (capturedOrientation == VERTICAL) {
                        gridWindow.selectXCoord(capturedCoordIndex);
                    } else {
                        gridWindow.selectYCoord(capturedCoordIndex);
                    }
                    // Draw the captured grid line handle
                    if (capturedOrientation == VERTICAL) {
                        drawVerticalGridLineHandles(tempG, capturedCoordIndex);
                    } else {
                        drawHorizontalGridLineHandles(tempG, capturedCoordIndex);
                    }
                    hasChanged = true;
                }
                // Move the captured grid line
                if (capturedOrientation == VERTICAL) {
                    ix[capturedScreenIndex] = p.x;
                } else {
                    iy[capturedScreenIndex] = p.y;
                }
                // Draw the captured grid line in its new position
                if (capturedOrientation == VERTICAL) {
                    tempG.drawLine(ix[capturedScreenIndex], iy[0],
                                   ix[capturedScreenIndex], iy[iy.length-1]);
                } else {
                    tempG.drawLine(ix[0], iy[capturedScreenIndex],
                                   ix[ix.length-1], iy[capturedScreenIndex]);
                }
                return;
            }
        }
    }

    /**
     * Invoked when the mouse is moved
     */
    public void onMouseMoved(MouseEvent e) {

        Point p = e.getPoint();

        if (activeMode == EDIT_GRID) {
            // First, set default values
            mouseOverGrid = false;
            capturedHandle = VOID_HANDLE;
            // Check for handle capture.
            if (gridRect.isSelected) {
                capturedHandle = gridRect.getHandleUnderMouse(p);
                if (capturedHandle != VOID_HANDLE) {
                    return;
                }
            }
            // Check if mouse is over grid
            if (gridRect.contains(p)) {
                mouseOverGrid = true;
                view.setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
                return;
            }
            // If none of the above, then handle is not captured, and
            // mouse is not over grid.
            else {
                view.setCursor(Cursor.getDefaultCursor());
                return;
            }
        }

        else if (activeMode == EDIT_LINE) {
            // Set default value
            capturedScreenIndex = -1;
            // If mouse is outside the grid, show the default cursor
            if (p.x<ix[0]-1 || p.x>ix[ix.length-1]+1 || p.y<iy[0]-1
                        || p.y>iy[iy.length-1]+1) {
                view.setCursor(Cursor.getDefaultCursor());
                return;
            }
            // Check if mouse is over a vertical grid line
            for (int i=0; i<ix.length; i++) {
                if (Math.abs(p.x - ix[i]) <= 1) {
                    view.setCursor(Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR));
                    capturedScreenIndex = i;
                    capturedCoordIndex = i;
                    capturedOrientation = VERTICAL;
                    return;
                }
            }
            // Check if mouse if over a horizontal grid line
            for (int i=0; i<iy.length; i++) {
                if (Math.abs(p.y - iy[i]) <= 1) {
                    view.setCursor(Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR));
                    capturedScreenIndex = i;
                    capturedCoordIndex = i;
                    if (modelVerticalAxisUpward()) {
                        capturedCoordIndex = gridData.getYCoordCount() - capturedCoordIndex - 1;
                    }
                    capturedOrientation = HORIZONTAL;
                    return;
                }
            }
            // If mouse is neither over a horizontal grid line or a
            // vertical grid line, then show the default cursor.
            view.setCursor(Cursor.getDefaultCursor());
            return;
        }
    }

    /**
     * Invoked when the mouse is pressed
     */
    public void onMousePressed (MouseEvent e) {

        Point p = e.getPoint();

        if (activeMode == EDIT_GRID) {
            // Get ready to resize or move grid
            if (capturedHandle != VOID_HANDLE || mouseOverGrid) {
                makeTempG(true);
                oldPoint = p;
                hasChanged = false;
            }
            return;
        }

        else if (activeMode == EDIT_LINE) {
            // Get ready to move the captured grid line
            if (capturedScreenIndex > -1) {
                makeTempG(true);
                oldPoint = p;
                hasChanged = false;
            }
            return;
        }

        if (activeMode == ADD_VLINE) {
            if (!gridRect.contains(p)) {
                return;
            }
            for (int i=0; i<ix.length; i++) {
                if (p.x == ix[i]) {
                    mp2MessageBox.showMessageDialog(
                        "Cannot add grid line on top of an existing grid line",
                        "Error");
                    return;
                }
            }
            makeTempG(false);
            tempG.setColor(Color.lightGray);
            tempG.drawLine(p.x, iy[0], p.x, iy[iy.length-1]);
            disposeTempG();
            double [] v = view.viewToModel(p);
            addXCoordWithUndo(v[0]);
            createScreenCoordinates();
            gridWindow.fireGridDataChanged();
            if (discretizeButton.isSelected()) {
                view.repaint();
            }
            return;
        }

        if (activeMode == ADD_HLINE) {
            if (!gridRect.contains(p)) {
                return;
            }
            for (int i=0; i<iy.length; i++) {
                if (p.y == iy[i]) {
                    mp2MessageBox.showMessageDialog(
                        "Cannot add grid line on top of an existing grid line",
                        "Error");
                    return;
                }
            }
            makeTempG(false);
            tempG.setColor(Color.lightGray);
            tempG.drawLine(ix[0], p.y, ix[ix.length-1], p.y);
            double [] v = view.viewToModel(p);
            addYCoordWithUndo(v[1]);
            createScreenCoordinates();
            gridWindow.fireGridDataChanged();
            if (discretizeButton.isSelected()) {
                view.repaint();
            }
            return;
        }
    }

    /**
     * Invoked when the mouse is released
     */
    public void onMouseReleased(MouseEvent e) {

        if (activeMode == EDIT_GRID) {
            // Finish resize the grid
            if (capturedHandle != VOID_HANDLE) {
                if (hasChanged) {
                    double [] v;
                    double [] w;
                    if (modelVerticalAxisUpward()) {
                        v = view.viewToModel(gridRect.handle[5]);
                        w = view.viewToModel(gridRect.handle[2]);
                    } else {
                        v = view.viewToModel(gridRect.handle[0]);
                        w = view.viewToModel(gridRect.handle[7]);
                    }
                    undoSupport.postEdit(
                            new GridResizeEdit(v[0], v[1], w[0], w[1],
                            gridData.getXCoordAt(0),
                            gridData.getYCoordAt(0),
                            gridData.getXCoordAt(gridData.getXCoordCount()-1),
                            gridData.getYCoordAt(gridData.getYCoordCount()-1),
                            undoCount));
                    undoCount++;
                    gridData.translateByMovingFirstPointTo(v[0], v[1]);
                    gridData.resize(w[0] - v[0], w[1] - v[1]);
                    updateScreenCoordinates();
                    gridWindow.fireGridDataChanged();
                }
            }
            // Finish moving the grid
            else if (mouseOverGrid) {
                if (hasChanged) {
                    double [] v;
                    if (modelVerticalAxisUpward()) {
                        v = view.viewToModel(gridRect.handle[5]);
                    } else {
                        v = view.viewToModel(gridRect.handle[0]);
                    }
                    undoSupport.postEdit(
                            new GridMoveEdit(v[0], v[1],
                            gridData.getXCoordAt(0),
                            gridData.getYCoordAt(0),
                            undoCount));
                    undoCount++;
                    gridData.translateByMovingFirstPointTo(v[0], v[1]);
                    updateScreenCoordinates();
                    gridWindow.fireGridDataChanged();
                } else {
                    gridRect.isSelected = true;    // selects the grid
                }
            }
            else {
                gridRect.isSelected = false;    // Deselect the grid
            }
            disposeTempG();
            onMouseMoved(e);    // checks for cursor style
            evaluateEditMenu();
            view.repaint();
            return;
        }

        else if (activeMode == EDIT_LINE) {
            // Finish moving the grid line
            if (capturedScreenIndex > -1) {
                if (hasChanged) {
                    double [] v = view.viewToModel(e.getPoint());
                    boolean validMove = true;
                    if (capturedOrientation == VERTICAL) {
                        for (int i=0; i<ix.length && validMove; i++) {
                            if (i!=capturedScreenIndex && ix[i] == ix[capturedScreenIndex]) {
                                mp2MessageBox.showMessageDialog(
                                    "Cannot move a grid line to coincide with another grid line",
                                    "Error");
                                validMove = false;
                            }
                        }
                        if (validMove) {
                            capturedCoordIndex = changeXCoordWithUndo(
                                    v[0], capturedCoordIndex);
                            gridWindow.fireGridDataChanged();
                            gridWindow.selectXCoord(capturedCoordIndex);
                        }
                        updateScreenCoordinates();
                    } else {
                        for (int i=0; i<iy.length && validMove; i++) {
                            if (i!=capturedScreenIndex && iy[i] == iy[capturedScreenIndex]) {
                                mp2MessageBox.showMessageDialog(
                                    "Cannot move a grid line to coincide with another grid line",
                                    "Error");
                                validMove = false;
                            }
                        } if (validMove) {
                            capturedCoordIndex = changeYCoordWithUndo(v[1], capturedCoordIndex);
                            gridWindow.fireGridDataChanged();
                            gridWindow.selectYCoord(capturedCoordIndex);
                        }
                        updateScreenCoordinates();
                    }
                } else {
                    if (capturedOrientation == VERTICAL) {
                        gridWindow.deselectAllYCoords();
                        if (e.isShiftDown()) {
                            int [] coordIndex = gridWindow.getSelectedXCoordIndices();
                            if (coordIndex.length == 0) {
                                gridWindow.selectXCoord(capturedCoordIndex);
                            } else {
                                int a = Math.min(coordIndex[0], capturedCoordIndex);
                                int b = Math.max(coordIndex[coordIndex.length-1], capturedCoordIndex);
                                gridWindow.selectXCoordInterval(a, b);
                            }
                        } else {
                            gridWindow.selectXCoord(capturedCoordIndex);
                        }
                    } else {
                        gridWindow.deselectAllXCoords();
                        if (e.isShiftDown()) {
                            int [] coordIndex = gridWindow.getSelectedYCoordIndices();
                            if (coordIndex.length == 0) {
                                gridWindow.selectYCoord(capturedCoordIndex);
                            } else {
                                int a = Math.min(coordIndex[0], capturedCoordIndex);
                                int b = Math.max(coordIndex[coordIndex.length-1], capturedCoordIndex);
                                gridWindow.selectYCoordInterval(a, b);
                            }
                        } else {
                            gridWindow.selectYCoord(capturedCoordIndex);
                        }
                    }
                }
            }
            // if no grid line was captured, the deselect all grid lines
            else {
                gridWindow.deselectAllCoords();
            }
            disposeTempG();
            evaluateEditMenu();
            view.repaint();
            return;
        }
    }

    /**
     * Subdivide a selected interval into columns or rows
     */
    public void onSubdivide() {
        String intervalType;
        double start, end;
        int [] index = gridWindow.getSelectedXCoordIndices();
        if (index.length > 0) {
            intervalType = "column";
            start = gridData.getXCoordAt(index[0]);
            end = gridData.getXCoordAt(index[index.length-1]);
        } else {
            index = gridWindow.getSelectedYCoordIndices();
            intervalType = "row";
            start = gridData.getYCoordAt(index[0]);
            end = gridData.getYCoordAt(index[index.length-1]);
        }

        SubdivideDialog dlg = new SubdivideDialog(intervalType);
        dlg.start = start;
        dlg.end = end;
        if (dlg.doModal() == true) {
            if (intervalType.equals("column")) {
                undoSupport.postEdit(new SubdivideEdit(
                    index[0], index[index.length-1],
                    dlg.numSubInterval, dlg.multiplier,
                    gridData.getXCoords(), XCOORD, undoCount));
                undoCount++;
                gridData.subdivideXInterval(index[0],
                        index[index.length-1],
                        dlg.numSubInterval, dlg.multiplier);
            } else {
                undoSupport.postEdit(new SubdivideEdit(
                    index[0], index[index.length-1],
                    dlg.numSubInterval, dlg.multiplier,
                    gridData.getYCoords(), YCOORD, undoCount));
                undoCount++;
                gridData.subdivideYInterval(index[0],
                        index[index.length-1],
                        dlg.numSubInterval, dlg.multiplier);
            }
            gridWindow.deselectAllCoords();
            gridWindow.fireGridDataChanged();
            createScreenCoordinates();
            evaluateEditMenu();
            view.repaint();
        }
    }

    /**
     * Invoked when uniform spacing menu item is selected
     */
    protected void onUniformGrid() {
        UniformGridWindow dlg = new UniformGridWindow();
        dlg.numCol = gridData.getXCoordCount() - 1;
        dlg.numRow = gridData.getYCoordCount() - 1;
        dlg.shrinkFit = false;
        mp2RectBounds domainBounds = domainData.getBoundary(0).getBounds();
        dlg.domainWidth = domainBounds.width;
        dlg.domainHeight = domainBounds.height;
        dlg.gridWidth = gridData.getXCoordAt(gridData.getXCoordCount()-1)
                          - gridData.getXCoordAt(0);
        dlg.gridHeight = gridData.getYCoordAt(gridData.getYCoordCount()-1)
                          - gridData.getYCoordAt(0);
        if (dlg.doModal() == true) {
            undoSupport.postEdit(new UniformGridEdit(
                    dlg.numCol, dlg.numRow, dlg.shrinkFit,
                    gridData.getXCoords(), gridData.getYCoords(),
                    undoCount));
            undoCount++;
            gridData.makeUniformGrid(dlg.numCol, dlg.numRow, dlg.shrinkFit);
            gridWindow.deselectAllCoords();
            gridWindow.fireGridDataChanged();
            gridRect.isSelected = false;
            createScreenCoordinates();
            evaluateEditMenu();
            view.repaint();
        }
    }

    /**
     * Draws the grid
     */
    public void paint(Graphics g) {

        if (!isActive) {
            gridWindow.deselectAllCoords();
        }
        if (!isVisible) {
            return;
        }

        Color oldColor = g.getColor();
        double [] xCoord = gridData.getXCoords();
        double [] yCoord = gridData.getYCoords();
        if (xCoord == null || yCoord == null) {
            return;
        }

		Rectangle rect = g.getClipBounds();
		int clipLeft = rect.x;
		int clipTop = rect.y;
		int clipRight = rect.x + rect.width;
		int clipBottom = rect.y + rect.height;
        Point p1, p2;
        if (modelVerticalAxisUpward()) {
            p1 = view.modelToView(xCoord[0], yCoord[yCoord.length-1]);
            p2 = view.modelToView(xCoord[xCoord.length-1], yCoord[0]);
        } else {
            p1 = view.modelToView(xCoord[0], yCoord[0]);
            p2 = view.modelToView(xCoord[xCoord.length-1], yCoord[yCoord.length-1]);
        }
        int left = Math.max(p1.x, clipLeft);
        int right = Math.min(p2.x, clipRight);
        if (left > right ) {
            return;
        }
        int top = Math.max(p1.y, clipTop);
        int bottom = Math.min(p2.y, clipBottom);
        if (top > bottom) {
            return;
        }

        Point p;
        g.setColor(Color.lightGray);
        // draw the vertical grid lines
        for (int i=0; i<xCoord.length; i++) {
            p = view.modelToView(xCoord[i], yCoord[0]);
            if (p.x > clipRight) {
                break;
            }
            if (p.x >= clipLeft) {
                g.drawLine(p.x, top, p.x, bottom);
            }
        }
        // draw the horizontal grid lines
        for (int j=0; j<yCoord.length; j++) {
            if (modelVerticalAxisUpward()) {
                p = view.modelToView(xCoord[0], yCoord[yCoord.length - j - 1]);
            } else {
                p = view.modelToView(xCoord[0], yCoord[j]);
            }
            if (p.y > clipBottom) {
                break;
            }
            if (p.y >= clipTop) {
                g.drawLine(left, p.y, right, p.y);
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    /**
     * draw handles
     */
    public void paintLast (Graphics g) {
        Color oldColor = g.getColor();
        if (!isVisible) {
            return;
        }
        updateScreenCoordinates();   // needed in case zoom happened.
        g.setColor(Color.black);
        g.setXORMode(Color.white);
            if (activeMode == EDIT_GRID && gridRect.isSelected) {
                gridRect.drawHandles(g);
            }
        else if (activeMode == EDIT_LINE) {
            drawSelectedGridLinesHandles(g);
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    /**
     * Loads menu items and tool bar buttons for this view
     * when it becomes active
     */
    public void prepareToActivate() {
        super.prepareToActivate();
        JMenu editMenu = view.getFrame().getMenu(EDIT_MENU);
        editMenu.removeAll();
        editMenu.add(undoMenuItem);
        editMenu.add(redoMenuItem);
        editMenu.addSeparator();
        editMenu.add(uniformGridMenuItem);
        editMenu.add(subdivideMenuItem);
        editMenu.add(selectAllMenu);
        editMenu.add(deleteMenuItem);

        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(editGridButton);
        toolBar.add(editLineButton);
        toolBar.add(addVLineButton);
        toolBar.add(addHLineButton);
        toolBar.add(zoomButton);
        toolBar.add(Box.createVerticalStrut(10));
        toolBar.add(gridWindowButton);
        toolBar.add(Box.createVerticalStrut(10));
        toolBar.add(discretizeButton);
        if (view.hasSiteMap()) {
            toolBar.add(Box.createVerticalStrut(10));
            toolBar.add(view.getXORButton());
        }

        // If the grid is not defined, create an initial
        // cell that contains the domain.
        if (!gridData.isDefined()) {
            mp2RectBounds domainBounds = domainData.getBoundary(0).getBounds();
            // If using radial coordinates, force the left side of grid
            // to coincide with r=0.
            if (gridData.getUseRadialCoord() && domainBounds.x > 0) {
                gridData.addXCoord(0);
            }
            gridData.addXCoord(domainBounds.x);
            gridData.addXCoord(domainBounds.x + domainBounds.width);
            gridData.addYCoord(domainBounds.y);
            gridData.addYCoord(domainBounds.y + domainBounds.height);
            gridWindow.fireGridDataChanged();
        }
        createScreenCoordinates();
        editGridButton.setSelected(true);
        discretizeButton.setSelected(view.isPaintModeDiscrete());
        capturedHandle = VOID_HANDLE;
        capturedScreenIndex = -1;
    }

    /**
     * Select all the horizontal or vertical interior grid lines
     */
    protected void selectAllGridLines(int orientation) {
        if (orientation == VERTICAL) {
            gridWindow.selectXCoordInterval(0, gridData.getXCoordCount()-1);
        } else {
            gridWindow.selectYCoordInterval(0, gridData.getYCoordCount()-1);
        }
        evaluateEditMenu();
        view.repaint();
    }

    /**
     * Sets the active mode
     */
    protected void setActiveMode(int mode) {
        activeMode = mode;
        gridRect.isSelected = false;
        gridWindow.deselectAllCoords();
        if (activeMode == ADD_HLINE || activeMode == ADD_VLINE) {
            view.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
        } else {
            view.setCursor(Cursor.getDefaultCursor());
        }
        evaluateEditMenu();
        view.repaint();
    }

    /**
     * Selects the add hline Button
     */
    public void setAddHLineButtonSelected() {
        if (!addHLineButton.isSelected()) {
            addHLineButton.setSelected(true);
        }
    }

    /**
     * Selects the add vline Button
     */
    public void setAddVLineButtonSelected() {
        if (!addVLineButton.isSelected()) {
            addVLineButton.setSelected(true);
        }
    }

    /**
     * Selects the editLine Button
     */
    public void setEditLineButtonSelected() {
        if (!editLineButton.isSelected()) {
            editLineButton.setSelected(true);
        }
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEnabled(boolean b) {
        gridWindow.setEnabled(b);
    }

    /**
     * Update the screen coordinates
     */
    protected void updateScreenCoordinates() {
        double [] x = gridData.getXCoords();
        double [] y = gridData.getYCoords();
        if (x.length != ix.length || y.length != iy.length) {
            createScreenCoordinates();
            return;
        }
        double xMin = x[0];
        double yMin = y[0];
        double xMax = x[x.length-1];
        double yMax = y[y.length-1];
        Point p0, p1;
        for (int i=0; i<x.length; i++) {
            ix[i] = view.modelToView(x[i], 0).x;
        }

        if (modelVerticalAxisUpward()) {
            p0 = view.modelToView(xMin, yMax);
            p1 = view.modelToView(xMax, yMin);
            for (int i=0; i<y.length; i++) {
                iy[i] = view.modelToView(0, y[y.length - i - 1]).y;
            }
        } else {
            p0 = view.modelToView(xMin, yMin);
            p1 = view.modelToView(xMax, yMax);
            for (int i=0; i<y.length; i++) {
                iy[i] = view.modelToView(0, y[i]).y;
            }
        }
        gridRect.updateHandles(p0, p1);
    }

    /**
     * The undoable edit to move the grid
     */
    protected class GridMoveEdit extends AbstractDataEdit {

        protected double newX, newY, oldX, oldY;

        public GridMoveEdit(double newX, double newY,
                double oldX, double oldY, long undoIndex) {
            super(undoIndex);
            this.newX = newX;
            this.newY = newY;
            this.oldX = oldX;
            this.oldY = oldY;
        }

        public void undo() throws CannotUndoException {
            gridData.translateByMovingFirstPointTo(oldX, oldY);
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.undo();
        }

        public void redo() throws CannotRedoException {
            gridData.translateByMovingFirstPointTo(newX, newY);
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.redo();
        }
    }

    /**
     * The undoable edit to resize the grid
     */
    protected class GridResizeEdit extends AbstractDataEdit {

        protected double newX1, newX2, newY1, newY2,
                         oldX1, oldX2, oldY1, oldY2;

        public GridResizeEdit(double newX1, double newY1,
                        double newX2, double newY2,
                        double oldX1, double oldY1,
                        double oldX2, double oldY2, long undoIndex) {
            super(undoIndex);
            this.newX1 = newX1;
            this.newY1 = newY1;
            this.newX2 = newX2;
            this.newY2 = newY2;
            this.oldX1 = oldX1;
            this.oldY1 = oldY1;
            this.oldX2 = oldX2;
            this.oldY2 = oldY2;
        }

        public void undo() throws CannotUndoException {
            gridData.translateByMovingFirstPointTo(oldX1, oldY1);
            gridData.resize(oldX2 - oldX1, oldY2 - oldY1);
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.undo();
        }

        public void redo() throws CannotRedoException {
            gridData.translateByMovingFirstPointTo(newX1, newY1);
            gridData.resize(newX2 - newX1, newY2 - newY1);
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.redo();
        }
    }

    /**
     * The undoable edit to add a grid line
     */
    protected class LineAddEdit extends AbstractDataEdit {

        protected double coord;
        protected int index;
        protected int orientation;

        public LineAddEdit(double coord, int index,
                        int orientation, long undoIndex) {
            super(undoIndex);
            this.coord = coord;
            this.index = index;
            this.orientation = orientation;
        }

        public void undo() throws CannotUndoException {
            if (orientation == VERTICAL) {
                gridData.deleteXCoordAt(index);
            } else {
                gridData.deleteYCoordAt(index);
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.undo();
        }

        public void redo() throws CannotRedoException {
            if (orientation == VERTICAL) {
                gridData.addXCoord(coord);
            } else {
                gridData.addYCoord(coord);
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.redo();
        }
    }

    /**
     * The undoable edit to move the a vertical grid line
     */
    protected class LineMoveEdit extends AbstractDataEdit {

        protected double newCoord, oldCoord;
        protected int newIndex, oldIndex, orientation;

        public LineMoveEdit(double newCoord, double oldCoord,
                int newIndex, int oldIndex, int orientation,
                long undoIndex) {
            super(undoIndex);
            this.newCoord = newCoord;
            this.oldCoord = oldCoord;
            this.newIndex = newIndex;
            this.oldIndex = oldIndex;
            this.orientation = orientation;
        }

        public void undo() throws CannotUndoException {
            if (orientation == VERTICAL) {
                gridData.deleteXCoordAt(newIndex);
                gridData.addXCoord(oldCoord);
            } else {
                gridData.deleteYCoordAt(newIndex);
                gridData.addYCoord(oldCoord);
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.undo();
        }

        public void redo() throws CannotRedoException {
            if (orientation == VERTICAL) {
                gridData.deleteXCoordAt(oldIndex);
                gridData.addXCoord(newCoord);
            } else {
                gridData.deleteYCoordAt(oldIndex);
                gridData.addYCoord(newCoord);
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.redo();
        }
    }

    /**
     * The undoable edit to delete a grid line
     */
    protected class LineDeleteEdit extends AbstractDataEdit {

        protected double [] coord;
        protected int [] index;
        protected int orientation;

        public LineDeleteEdit(double [] coord, int [] index,
                        int orientation, long undoIndex) {
            super(undoIndex);
            this.coord = coord;
            this.index = index;
            this.orientation = orientation;
        }

        public void undo() throws CannotUndoException {
            if (orientation == VERTICAL) {
                for (int i=0; i<coord.length; i++) {
                    gridData.addXCoord(coord[i]);
                }
            } else {
                for (int i=0; i<coord.length; i++) {
                    gridData.addYCoord(coord[i]);
                }
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.undo();
        }

        public void redo() throws CannotRedoException {
            if (orientation == VERTICAL) {
                gridData.deleteXCoords(index);
            } else {
                gridData.deleteYCoords(index);
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.redo();
        }
    }

    /**
     * The undoable edit to subdivide an interval
     */
    protected class SubdivideEdit extends AbstractDataEdit {

        protected int startIndex;
        protected int endIndex;
        protected int numSubInterval;
        protected double multiplier;
        protected double [] oldCoord;
        protected int coordType;

        public SubdivideEdit(int startIndex, int endIndex,
                int numSubInterval, double multiplier,
                double [] oldCoord, int coordType, long undoIndex) {
            super(undoIndex);
            this.startIndex = startIndex;
            this.endIndex = endIndex;
            this.numSubInterval = numSubInterval;
            this.multiplier = multiplier;
            this.oldCoord = oldCoord;
            this.coordType = coordType;
        }

        public void undo() throws CannotUndoException {
            if (coordType == XCOORD) {
                gridData.deleteAllXCoords();
                for (int i=0; i<oldCoord.length; i++) {
                    gridData.addXCoord(oldCoord[i]);
                }
            } else {
                gridData.deleteAllYCoords();
                for (int i=0; i<oldCoord.length; i++) {
                    gridData.addYCoord(oldCoord[i]);
                }
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.undo();
        }

        public void redo() throws CannotRedoException {
            if (coordType == XCOORD) {
                gridData.subdivideXInterval(startIndex,
                        endIndex, numSubInterval, multiplier);
            } else {
                gridData.subdivideYInterval(startIndex,
                        endIndex, numSubInterval, multiplier);
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.redo();
        }
    }

    /**
     * The undoable edit to create a uniform grid
     */
    protected class UniformGridEdit extends AbstractDataEdit {

        protected int numCol;
        protected int numRow;
        protected boolean shrinkFit;
        protected double [] oldXCoord;
        protected double [] oldYCoord;

        public UniformGridEdit(int numCol, int numRow,
                boolean shrinkFit, double [] oldXCoord,
                double [] oldYCoord, long undoIndex) {
            super(undoIndex);
            this.numCol = numCol;
            this.numRow = numRow;
            this.shrinkFit = shrinkFit;
            this.oldXCoord = oldXCoord;
            this.oldYCoord = oldYCoord;
        }

        public void undo() throws CannotUndoException {
            gridData.deleteAllXCoords();
            for (int i=0; i<oldXCoord.length; i++) {
                gridData.addXCoord(oldXCoord[i]);
            }
            gridData.deleteAllYCoords();
            for (int i=0; i<oldYCoord.length; i++) {
                gridData.addYCoord(oldYCoord[i]);
            }
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.undo();
        }

        public void redo() throws CannotRedoException {
            gridData.makeUniformGrid(numCol, numRow, shrinkFit);
            updateScreenCoordinates();
            gridWindow.fireGridDataChanged();
            gridWindow.deselectAllCoords();
            gridRect.isSelected = false;
            super.redo();
        }
    }

    /**
     * Encapsulates the rectangular outline of the grid on the screen
     */
    class GridRect {
        boolean isSelected;
        Point [] handle;

        GridRect() {
            isSelected = false;
            handle = new Point [8];
            for (int i=0; i<8; i++) {
                handle[i] = new Point(0, 0);
            }
        }

        void updateHandles(Point p1, Point p2) {
            handle[0].x = handle[3].x = handle[5].x = p1.x;
            handle[1].x = handle[6].x = (p1.x + p2.x) / 2;
            handle[2].x = handle[4].x = handle[7].x = p2.x;
            handle[0].y = handle[1].y = handle[2].y = p1.y;
            handle[3].y = handle[4].y = (p1.y + p2.y) / 2;
            handle[5].y = handle[6].y = handle[7].y = p2.y;
        }

        boolean contains(Point p) {
            return (p.x > handle[0].x && p.x < handle[7].x
                 && p.y > handle[0].y && p.y < handle[7].y);
        }

        void draw(Graphics g) {
            g.drawRect(getX(), getY(), getWidth(), getHeight());
        }

        void drawHandles(Graphics g) {
            for (int i=0; i<8; i++) {
                g.fillRect(handle[i].x - HALF_HANDLE_SIZE,
                           handle[i].y - HALF_HANDLE_SIZE,
                           HANDLE_SIZE, HANDLE_SIZE);
            }
        }

        int getHandleUnderMouse(Point p) {

            if (!gridData.getUseRadialCoord()
                    && mp2Math.withinNeighborhood(p, handle[3], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.W_RESIZE_CURSOR));
                return W_HANDLE;
            }
            if (mp2Math.withinNeighborhood(p, handle[0], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.NW_RESIZE_CURSOR));
                return NW_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[5], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.SW_RESIZE_CURSOR));
                return SW_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[1], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.N_RESIZE_CURSOR));
                return N_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[2], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.NE_RESIZE_CURSOR));
                return NE_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[4], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.E_RESIZE_CURSOR));
                return E_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[6], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.S_RESIZE_CURSOR));
                return S_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[7], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.SE_RESIZE_CURSOR));
                return SE_HANDLE;
            }
            else {
                return VOID_HANDLE;
            }
        }

        int getX() {
            return handle[0].x;
        }

        int getY() {
            return handle[0].y;
        }

        int getWidth() {
            return handle[7].x - handle[0].x;
        }

        int getHeight() {
            return handle[7].y - handle[0].y;
        }

        void moveX(int dx) {
            for (int i=0; i<8; i++) {
                handle[i].x += dx;
            }
        }

        void moveY(int dy) {
            for (int i=0; i<8; i++) {
                handle[i].y += dy;
            }
        }

        void resize(int capturedHandle, Point p, boolean useRadialCoord) {
            // resize north
            if (capturedHandle == NW_HANDLE || capturedHandle == N_HANDLE
                    || capturedHandle == NE_HANDLE) {
                handle[0].y = p.y;
                handle[1].y = p.y;
                handle[2].y = p.y;
                handle[3].y = (p.y + handle[6].y)/2;
                handle[4].y = handle[3].y;
            }
            // resize east
            if (capturedHandle == NE_HANDLE || capturedHandle == E_HANDLE
                    || capturedHandle == SE_HANDLE) {
                handle[2].x = p.x;
                handle[4].x = p.x;
                handle[7].x = p.x;
                handle[1].x = (p.x + handle[3].x)/2;
                handle[6].x = handle[1].x;
            }
            // resize south
            if (capturedHandle == SE_HANDLE || capturedHandle == S_HANDLE
                    || capturedHandle == SW_HANDLE) {
                handle[5].y = p.y;
                handle[6].y = p.y;
                handle[7].y = p.y;
                handle[3].y = (p.y + handle[1].y)/2;
                handle[4].y = handle[3].y;
            }
            // resize west
            if (capturedHandle == SW_HANDLE || capturedHandle == W_HANDLE
                    || capturedHandle == NW_HANDLE) {
                if (!useRadialCoord) {
                    handle[0].x = p.x;
                    handle[3].x = p.x;
                    handle[5].x = p.x;
                    handle[1].x = (p.x + handle[4].x)/2;
                    handle[6].x = handle[1].x;
                }
            }
        }
    }

    /**
     * Dialog box to set uniform grid
     */
    class UniformGridWindow extends mp2Dialog {

        int numCol;
        int numRow;
        boolean shrinkFit;

        JTextField numColTextField;
        JTextField numRowTextField;
        JRadioButton yesShrinkFitRadioButton;
        JRadioButton noShrinkFitRadioButton;
        JLabel gridWidthLabel;
        JLabel gridHeightLabel;
        JLabel colWidthLabel;
        JLabel rowHeightLabel;

        double domainWidth;
        double domainHeight;
        double gridWidth;
        double gridHeight;

        /**
         * Creates a dialog box.
         */
        public UniformGridWindow() {
            super("Uniform Grid", true);
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "uniformGrid", null);
        }

        /**
         * Makes dialog box contents.
         */
        protected void makeContents() {

            // Make a center panel to hold all the components.
            GridBagLayout gridbag = new GridBagLayout();
            GridBagConstraints c = new GridBagConstraints();
            JPanel centerPanel = new JPanel(gridbag);
            centerPanel.setBorder(new EmptyBorder(10, 10, 5, 10));
            getContentPane().add(centerPanel, BorderLayout.CENTER);

            JPanel leftPanel = new JPanel(gridbag);
            leftPanel.setBorder(new CompoundBorder(
                    BorderFactory.createEtchedBorder(),
                    new EmptyBorder(10, 10, 10, 10)));
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.fill = GridBagConstraints.VERTICAL;
            gridbag.setConstraints(leftPanel, c);
            centerPanel.add(leftPanel);

            JPanel rightPanel = new JPanel(new GridLayout(4, 2, 0, 10));
            rightPanel.setBorder(new CompoundBorder(
                    BorderFactory.createEtchedBorder(),
                    new EmptyBorder(10, 10, 10, 10)));
            c.insets = new Insets(0, 10, 0, 0);
            c.gridwidth = GridBagConstraints.REMAINDER;
            gridbag.setConstraints(rightPanel, c);
            centerPanel.add(rightPanel);
            rightPanel.add(new JLabel("grid width",
                                        SwingConstants.RIGHT));
            rightPanel.add(gridWidthLabel = new JLabel(" ",
                                        SwingConstants.LEFT));
            rightPanel.add(new JLabel("grid height",
                                        SwingConstants.RIGHT));
            rightPanel.add(gridHeightLabel = new JLabel(" ",
                                        SwingConstants.LEFT));
            rightPanel.add(new JLabel("column width",
                                        SwingConstants.RIGHT));
            rightPanel.add(colWidthLabel = new JLabel(" ",
                                        SwingConstants.LEFT));
            rightPanel.add(new JLabel("row height",
                                        SwingConstants.RIGHT));
            rightPanel.add(rowHeightLabel = new JLabel(" ",
                                        SwingConstants.LEFT));

            // Make upper panel for labels and text fields
            JPanel leftUpperPanel = new JPanel(gridbag);
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(0, 0, 0, 0);
            c.fill = GridBagConstraints.HORIZONTAL;
            gridbag.setConstraints(leftUpperPanel, c);
            leftPanel.add(leftUpperPanel);

            // Make a panel to hold the labels
            JPanel labelPanel = new JPanel(new GridLayout(2, 1, 10, 10));
            c.fill = GridBagConstraints.VERTICAL;
            c.gridwidth = GridBagConstraints.RELATIVE;
            gridbag.setConstraints(labelPanel, c);
            leftUpperPanel.add(labelPanel);

            // Make a panel to hold the text fields
            JPanel fieldPanel = new JPanel(new GridLayout(2, 1, 10, 10));
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(0, 10, 0, 0);
            gridbag.setConstraints(fieldPanel, c);
            leftUpperPanel.add(fieldPanel);

            DocumentListener docListener = new DocumentListener() {
                public void insertUpdate(DocumentEvent e) {update();}
                public void removeUpdate(DocumentEvent e) {update();}
                public void changedUpdate(DocumentEvent e) {}
            };

            // Add the labels and text fields to left and right panels respectively
            labelPanel.add(new JLabel("Number of columns", SwingConstants.RIGHT));
            fieldPanel.add(numColTextField = new JTextField(5));
            numColTextField.getDocument().addDocumentListener(docListener);

            labelPanel.add(new JLabel("Number of rows", SwingConstants.RIGHT));
            fieldPanel.add(numRowTextField = new JTextField(5));
            numRowTextField.getDocument().addDocumentListener(docListener);

            // Make lower panel to hold radio buttons
            JPanel leftLowerPanel = new JPanel(new GridLayout(2, 1));
            c.fill = GridBagConstraints.RELATIVE;
            c.insets = new Insets(10, 0, 0, 0);
            gridbag.setConstraints(leftLowerPanel, c);
            leftPanel.add(leftLowerPanel);
            noShrinkFitRadioButton =
                    new JRadioButton("Use current grid boundaries");
            leftLowerPanel.add(noShrinkFitRadioButton);
            yesShrinkFitRadioButton =
                    new JRadioButton("Shrink fit around domain");
            leftLowerPanel.add(yesShrinkFitRadioButton);
            ButtonGroup bg = new ButtonGroup();
            bg.add(yesShrinkFitRadioButton);
            bg.add(noShrinkFitRadioButton);
            yesShrinkFitRadioButton.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    update();
                }
            });

        }

        /**
         * Displays the dialog box with modal behavior.
         */
        public boolean doModal() {
            numColTextField.setText(String.valueOf(numCol));
            numRowTextField.setText(String.valueOf(numRow));
            noShrinkFitRadioButton.setSelected(!shrinkFit);
            yesShrinkFitRadioButton.setSelected(shrinkFit);
            return super.doModal();
        }

        /**
         * Show help in web browser
         */
        protected void onBrowserHelp() {
            mp2HelpWindow.showHelpFile("uniformGrid.html");
        }

        /**
         * Retrives data from dialog box components and checks validity.
         */
        protected boolean retrieveData() {
            try {
                numCol = Integer.parseInt(numColTextField.getText());
                numRow = Integer.parseInt(numRowTextField.getText());
            }
            catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog("Please check your input",
                        "Input Error");
                return false;
            }
            if (!dataCheck(numCol, "\"Number of columns\"", IS_POSITIVE,
                    numColTextField)) {
                return false;
            }
            if (!dataCheck(numRow, "\"Number of rows\"", IS_POSITIVE,
                    numRowTextField)) {
                return false;
            }
            shrinkFit = yesShrinkFitRadioButton.isSelected();
            return true;
        }

        protected void update() {
            try {
                numCol = Integer.parseInt(numColTextField.getText());
                if (numCol > 0) {
                    if (yesShrinkFitRadioButton.isSelected()) {
                        colWidthLabel.setText("  =  " +
                            mp2DecimalFormat.format(domainWidth/numCol));
                    } else {
                        colWidthLabel.setText("  =  " +
                            mp2DecimalFormat.format(gridWidth/numCol));
                    }
                } else {
                    colWidthLabel.setText("  =  ?");
                }
            } catch (NumberFormatException ex) {
                    colWidthLabel.setText("  =  ?");
            }
            try {
                numRow = Integer.parseInt(numRowTextField.getText());
                if (numRow > 0) {
                    if (yesShrinkFitRadioButton.isSelected()) {
                        rowHeightLabel.setText("  =  " +
                            mp2DecimalFormat.format(domainHeight/numRow));
                    } else {
                        rowHeightLabel.setText("  =  " +
                            mp2DecimalFormat.format(gridHeight/numRow));
                    }
                } else {
                    rowHeightLabel.setText("  =  ?");
                }
            } catch (NumberFormatException ex) {
                rowHeightLabel.setText("  =  ?");
            }
            if (yesShrinkFitRadioButton.isSelected()) {
                gridWidthLabel.setText("  =  " +
                    mp2DecimalFormat.format(domainWidth));
                gridHeightLabel.setText("  =  " +
                    mp2DecimalFormat.format(domainHeight));

            } else {
                gridWidthLabel.setText("  =  " +
                    mp2DecimalFormat.format(gridWidth));
                gridHeightLabel.setText("  =  " +
                    mp2DecimalFormat.format(gridHeight));
            }
        }
    }

    /**
     * Dialog box to subdivide a selected interval into columns or rows.
     */
    class SubdivideDialog extends mp2Dialog {

        int numSubInterval;
        double multiplier;
        JTextField numSubIntervalTextField;
        JTextField multiplierTextField;
        JComboBox factorChooser;
        JLabel lengthLabel;
        JLabel startSpacingLabel;
        JLabel endSpacingLabel;
        double start;
        double end;

        public SubdivideDialog(Object customObject) {
            super("Subdivide", true, customObject);
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "subdivideGridInterval", null);
        }

        protected void makeContents() {
            String intervalType = (String) customObject;

            GridBagLayout gridbag = new GridBagLayout();
            GridBagConstraints c = new GridBagConstraints();

            JPanel centerPanel = new JPanel(gridbag);
            centerPanel.setBorder(new EmptyBorder(10, 0, 10, 0));
            getContentPane().add(centerPanel, BorderLayout.CENTER);

            JPanel upperPanel = new JPanel(new GridLayout(2, 1));
            upperPanel.setBorder(new CompoundBorder(
                    BorderFactory.createEtchedBorder(),
                    new EmptyBorder(10, 10, 10, 10)));
            c.gridwidth = GridBagConstraints.REMAINDER;
            gridbag.setConstraints(upperPanel, c);
            centerPanel.add(upperPanel);

            DocumentListener docListener = new DocumentListener() {
                public void insertUpdate(DocumentEvent e) {update();}
                public void removeUpdate(DocumentEvent e) {update();}
                public void changedUpdate(DocumentEvent e) { }
            };

            JPanel subPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            upperPanel.add(subPanel);
            subPanel.add(new JLabel("Divide the selected interval into "));
            subPanel.add(numSubIntervalTextField = new JTextField(5));
            subPanel.add(new JLabel(" " + intervalType + "s"));
            numSubIntervalTextField.getDocument().addDocumentListener(docListener);

            subPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            subPanel.add(factorChooser = new JComboBox());
            upperPanel.add(subPanel);
            factorChooser.setEditable(false);
            factorChooser.addItem("Multiply");
            factorChooser.addItem("Divide");
            factorChooser.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    update();
                }
            });
            String spacingType;
            if (intervalType.equalsIgnoreCase("column")) {
                spacingType = "widths";
            } else {
                spacingType = "heights";
            }
            subPanel.add(new JLabel("successive " + intervalType + " "
                          + spacingType + " by "));
            subPanel.add(multiplierTextField = new JTextField(4));
            multiplierTextField.getDocument().addDocumentListener(docListener);

            JPanel lowerPanel = new JPanel(new GridLayout(3, 2, 0, 10));
            lowerPanel.setBorder(new CompoundBorder(
                    BorderFactory.createEtchedBorder(),
                    new EmptyBorder(10, 10, 10, 10)));
            c.insets = new Insets(10, 0, 0, 0);
            c.fill = GridBagConstraints.HORIZONTAL;
            gridbag.setConstraints(lowerPanel, c);
            centerPanel.add(lowerPanel);
            lowerPanel.add(new JLabel("interval length",
                                        SwingConstants.RIGHT));
            lowerPanel.add(lengthLabel = new JLabel(" ",
                                        SwingConstants.LEFT));
            lowerPanel.add(new JLabel("starting " + intervalType
                        + " " + spacingType, SwingConstants.RIGHT));
            lowerPanel.add(startSpacingLabel = new JLabel(" ",
                                        SwingConstants.LEFT));
            lowerPanel.add(new JLabel("ending " + intervalType
                        + " " + spacingType, SwingConstants.RIGHT));
            lowerPanel.add(endSpacingLabel = new JLabel(" ",
                                        SwingConstants.LEFT));

        }

        public boolean doModal() {
            multiplierTextField.setText("1");
            lengthLabel.setText("  =  " + mp2DecimalFormat.format(end - start));
            return super.doModal();
        }

        protected void onBrowserHelp() {
            mp2HelpWindow.showHelpFile("subdivideGridInterval.html");
        }

        protected boolean retrieveData() {
            try {
                numSubInterval = Integer.parseInt(numSubIntervalTextField.getText());
                multiplier = Double.valueOf(multiplierTextField.getText()).doubleValue();
            }
            catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog("Please check your input.",
                        "Input Error");
                return false;
            }
            if (!dataCheck(numSubInterval, "number of sub-intervals",
                           IS_POSITIVE, numSubIntervalTextField)) {
                return false;
            }
            if (!dataCheck(multiplier, "multiplication or division factor",
                           IS_POSITIVE, multiplierTextField)) {
                return false;
            }
            if (factorChooser.getSelectedIndex() == 1) {
                multiplier = 1./multiplier;
            }
            return true;
        }

            protected void update() {
            boolean valid = true;
            try {
                numSubInterval = Integer.parseInt(numSubIntervalTextField.getText());
                multiplier = Double.valueOf(multiplierTextField.getText()).doubleValue();
                if (numSubInterval > 0 && multiplier > 0) {
                    double startSpacing;
                    double endSpacing;
                    if (factorChooser.getSelectedIndex() == 1) {
                        multiplier = 1./multiplier;
                    }
                    if (multiplier == 1) {
                        startSpacing = (end - start)/numSubInterval;
                        endSpacing = startSpacing;
                    } else {
                        startSpacing = (end - start)*(multiplier - 1)/
                                (Math.pow(multiplier, numSubInterval) - 1);
                        endSpacing = startSpacing * Math.pow(multiplier,
                                                        numSubInterval-1);
                    }
                    startSpacingLabel.setText("  =  " +
                                mp2DecimalFormat.format(startSpacing));
                    endSpacingLabel.setText("  =  " +
                                mp2DecimalFormat.format(endSpacing));
                } else {
                    startSpacingLabel.setText("  =  ?");
                    endSpacingLabel.setText("  =  ?");
                }
            } catch (NumberFormatException ex) {
                startSpacingLabel.setText("  =  ?");
                endSpacingLabel.setText("  =  ?");
            }
        }

    }
}
