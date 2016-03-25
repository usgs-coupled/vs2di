/*
 * mp2SourceView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.undo.*;

public abstract class mp2SourceView extends mp2ShapesView {

    protected mp2ToggleButton createPointButton;
    protected mp2ToggleButton discretizeButton;
    protected JMenuItem propertiesMenuItem;
    protected JPopupMenu popup;
    boolean pointCreated = false;
    public static final Color SOURCE_POINT_COLOR = new Color(255, 0, 0);

    public mp2SourceView(mp2View view, 
                    mp2SourceData shapesData, 
                    mp2AbstractGridData gridData,
                    String homeDirectory) {

        super(view, shapesData, gridData, homeDirectory);

        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        createPointButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "square.gif"), false);
        createPointButton.setToolTipText("Add a point source");
        createPointButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onCreateShape();
                }
            }
        });
        discretizeButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "discretize.gif"), false);
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
        bg.add(createPointButton);
        bg.add(discretizeButton);
        bg.add(zoomButton);

        propertiesMenuItem = new JMenuItem("Properties...");
        propertiesMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onProperties();
            }
        });

        popup = new JPopupMenu("source");
        view.add(popup);

        JMenuItem menuItem = new JMenuItem("Position...");
        this.popup.add(menuItem);
        menuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                setPointPosition();
            }
        });
        menuItem = new JMenuItem("Properties...");
        this.popup.add(menuItem);
        menuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onProperties();
            }
        });
    }

    protected mp2Shape createNewShape(Point p) {
        mp2Point point = new mp2Point();
        point.setIcon(mp2Point.SQUARE_ICON);
        point.setForeground(SOURCE_POINT_COLOR);
        return point;
    }

    protected void onCreateShape() {
        setActiveModeTo(CREATE_SHAPE);
    }

    protected mp2SourceData getFluidSourceData() {
        return (mp2SourceData) shapesData;
    }

    public void onMouseReleased(MouseEvent e) {
        // popup menu when right click on point
        if (activeMode == SELECT_AND_EDIT 
                && shapeBeingReshaped == null
                && shapeHavingVertexReset == null 
                && shapeUnderMouse != null
                && e.getModifiers()==InputEvent.BUTTON3_MASK) {
            popup.show(e.getComponent(), e.getPoint().x, e.getPoint().y);
            return;
        }
        super.onMouseReleased(e);
        propertiesMenuItem.setEnabled(
                activeMode == SELECT_AND_EDIT
            &&  shapesData.getNumberOfSelectedShapes() == 1);
        if (pointCreated) {
            onPointCreated();
            pointCreated = false;
            return;
        }
        if (activeMode == SELECT_AND_EDIT && e.getClickCount() == 2) {
            onProperties();
        }
    }

    protected void onProperties() {
        int src = -1;
        for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
            if (shapesData.getShape(i).isSelected()) {
                src = i;
                break;
            }
        }
        if (src == -1) {
            return;
        }
        getPropertiesForSource(src);
    }

    protected void onPointCreated() {
        int src = shapesData.getNumberOfShapes() - 1;
        getPropertiesForSource(src);
    }

    protected abstract void getPropertiesForSource(int isrc);

    public void paint(Graphics g) {
        if (!isActive) {
            shapesData.deselectAllShapes();
        }
        if (!isVisible || shapesData.getNumberOfShapes() == 0) {
            return;
        }
        Color oldColor = g.getColor();
        if (view.isPaintModeDiscrete()) {
            shapesData.discretize();
            drawDiscretizedShapes(g);
        } else {
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                shapesData.getShape(i).draw(g);
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    // override this method for finite element data
    protected void drawDiscretizedShapes(Graphics g) {
        if (!(gridData instanceof mp2RectilinearGridData)) {
            return;
        }
        Rectangle rect = g.getClipBounds();
        int clipRight = rect.x + rect.width;
        int clipBottom = rect.y + rect.height;
        Vector sourceCellIndices = 
                ((mp2SourceData) shapesData).getSourceCellIndices();
        double [] xCoord = ((mp2RectilinearGridData) gridData).getXCoords();
        double [] yCoord = ((mp2RectilinearGridData) gridData).getYCoords();
        int numCol = xCoord.length - 1;
        int numRow = yCoord.length - 1;
        Point p1, p2;
        int col, row;
        g.setColor(SOURCE_POINT_COLOR);
        for (int i=0; i<sourceCellIndices.size(); i++) {
            int cellIndex = ((Integer) sourceCellIndices.elementAt(i)).intValue();
            row = cellIndex/numCol;
            col = cellIndex - row*numCol;
            if (view.getModelVerticalAxisOrientation() ==
                    mp2Drawing.MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE) {
                p1 = view.modelToView(xCoord[col], yCoord[row]);
                p2 = view.modelToView(xCoord[col+1], yCoord[row+1]);
            } else {
                p1 = view.modelToView(xCoord[col], yCoord[row+1]);
                p2 = view.modelToView(xCoord[col+1], yCoord[row]);
            }
            if (p1.x <= clipRight && p1.y <= clipBottom 
                        && p2.x >= rect.x && p2.y >= rect.y) {
                g.fillRect(p1.x, p1.y, p2.x - p1.x, p2.y - p1.y);
            }
        }
    }

    protected void onDelete() {
        if (shapesData.getNumberOfSelectedShapes() == 0) {
            return;
        }
        Vector oldShapes = shapesData.getClonedShapes();
        Vector oldStrengths = ((mp2SourceData) shapesData).getClonedStrengths();
        shapesData.deleteSelectedShapes();
        Vector newShapes = shapesData.getClonedShapes();
        Vector newStrengths = ((mp2SourceData) shapesData).getClonedStrengths();
        undoSupport.postEdit(new FluidSourceEdit(oldShapes, 
            oldStrengths, newShapes, newStrengths, undoCount));
        undoCount++;
        evaluateEditMenu();
        view.repaint();
    }

    protected boolean onNewShapeCompletion(mp2Shape shape) {
        if (!super.onNewShapeCompletion(shape)) {
            return false;
        }
        pointCreated = true;
        return true;
    }

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
        editMenu.addSeparator();
        editMenu.add(propertiesMenuItem);
        propertiesMenuItem.setEnabled(false);
        shapesData.deselectAllShapes();
        evaluateEditMenu();

        // Set up buttons on tool bar
        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(selectAndEditButton);
        toolBar.add(createPointButton);
        if (gridData.isDefined()) {
            toolBar.add(discretizeButton);
        }
        toolBar.add(zoomButton);

        if (view.isPaintModeDiscrete()) {
            discretizeButton.setSelected(true);
        } else {
            selectAndEditButton.setSelected(true);
        }

        view.setCursor(Cursor.getDefaultCursor());
    }

    protected void setActiveModeTo(int mode) {
        super.setActiveModeTo(mode);
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        super.setEditable(b);
        if (!b) {
            if (createPointButton.isSelected()) {
                selectAndEditButton.setSelected(true);
            }
        }
        createPointButton.setEnabled(b);
    }

    protected class FluidSourceEdit extends AbstractDataEdit {

        protected Vector oldShapes;
        protected Vector oldStrengths;
        protected Vector newShapes;
        protected Vector newStrengths;

        FluidSourceEdit(Vector oldShapes, Vector oldStrengths,
                Vector newShapes, Vector newStrengths,
                long undoIndex) {
            super(undoIndex);
            this.oldShapes = oldShapes;
            this.oldStrengths = oldStrengths;
            this.newShapes = newShapes;
            this.newStrengths = newStrengths;
        }

        public void undo() throws CannotUndoException {
            getFluidSourceData().setShapes(oldShapes);
            getFluidSourceData().setStrengths(oldStrengths);
            super.undo();
        }

        public void redo() throws CannotRedoException {
            getFluidSourceData().setShapes(newShapes);
            getFluidSourceData().setStrengths(newStrengths);
            super.redo();
        }
    }
}
