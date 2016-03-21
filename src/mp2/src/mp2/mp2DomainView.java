/*
 * mp2DomainView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.undo.*;

/**
 * Manages the model domain
 */ 
public class mp2DomainView extends mp2ShapesView {

    protected mp2BoundaryConditionsData boundaryConditionsData;
    protected mp2ToggleButton createExteriorBoundaryButton;
    protected mp2ToggleButton createInteriorBoundaryButton;
    protected mp2ToggleButton discretizeButton;
    protected Vector oldVertices;

    public mp2DomainView(mp2View view, 
                    mp2DomainData shapesData, 
                    mp2AbstractGridData gridData, 
                    String homeDirectory) {
        this(view, shapesData, gridData, null, homeDirectory);
    }
    
    /**
     * Creates a new mp2DomainView
     */
    public mp2DomainView(mp2View view, 
                    mp2DomainData shapesData, 
                    mp2AbstractGridData gridData, 
                    mp2BoundaryConditionsData boundaryConditionsData,
                    String homeDirectory) {

        super(view, shapesData, gridData, homeDirectory);
        this.boundaryConditionsData = boundaryConditionsData;
        intersectOtherShapeErrorMessage = "Boundaries cannot intersect each other";
        intersectSelfErrorMessage = "Boundary cannot intersect itself";

        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        createExteriorBoundaryButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "polygon.gif"), false);
        createInteriorBoundaryButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "polydotted.gif"), false);
        discretizeButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "discretize.gif"), false);

        createExteriorBoundaryButton.setToolTipText("Create the exterior boundary");
        createInteriorBoundaryButton.setToolTipText("Create an interior boundary");
        discretizeButton.setToolTipText("Discretize");

        ButtonGroup bg = new ButtonGroup();
        bg.add(selectAndEditButton);
        bg.add(createExteriorBoundaryButton);
        bg.add(createInteriorBoundaryButton);
        bg.add(addVertexButton);
        bg.add(discretizeButton);
        bg.add(zoomButton);

        createExteriorBoundaryButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(CREATE_SHAPE);
                }
            }
        });
        createInteriorBoundaryButton.addItemListener(new ItemListener() {
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
    }

    /**
     * creates a new shape
     */
    protected mp2Shape createNewShape(Point p) {
        // If creating interior boundary, make sure the mouse point
        // is inside the exterior boundary.
        if (createInteriorBoundaryButton.isSelected()) {
            mp2Shape exteriorBoundary = shapesData.getShape(0);
            if (!exteriorBoundary.contains(view.viewToModel(p))) {
                return null;
            }
        }       
        return new mp2Polygon();
     }

    /**
     * Invoked when the "Delete" menu item is selected.
     * Deletes the selected shapes and the boundary conditions
     * associated with it.
     */
    protected void onDelete() {
        if (shapesData.getNumberOfSelectedShapes() == 0) {
            return;
        }
        if (shapesData.getShape(0).isSelected()) {
            mp2MessageBox.showMessageDialog(
                    "Cannot delete exterior boundary","Error");
            return;
        }
        // Store the old bcs for undo. Note that we have to
        // clone at two levels
        Vector oldBC = null;
        Vector newBC = null;
        if (boundaryConditionsData != null) {
            oldBC = boundaryConditionsData.getClonedBC();
            // remove the bc corresponding to the boundary to be removed
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                mp2Shape shape = shapesData.getShape(i);
                if (shape.isSelected()) {
                    boundaryConditionsData.deleteBoundary(
                            shapesData.getIndexOfShape(shape));
                }
            }
            // Save the new bcs for redo
            newBC = boundaryConditionsData.getClonedBC();
        }

        // Save the old boundaries for undo
        Vector oldShapes = shapesData.getClonedShapes();
        // Remove the selected boundaries
        shapesData.deleteSelectedShapes();
        // Save the new boundaries for redo.
        Vector newShapes = shapesData.getClonedShapes();
        undoSupport.postEdit(new DeleteEdit(oldShapes, newShapes, 
                                        oldBC, newBC, undoCount));
        undoCount++;
        evaluateEditMenu();
        view.repaint();
    }

    /**
     * Invoked when a mouse button has been pressed on the view.
     */
    public void onMousePressed(MouseEvent e) {
        if (activeMode == ADD_VERTEX && capturedSegmentIndex > -1) {
            mp2PolyVertexShape pvShape = 
                (mp2PolyVertexShape) shapesData.getShape(capturedShapeIndex);
            double [] v = view.viewToModel(e.getPoint());
            pvShape.insertVertexOnSegment(v, capturedSegmentIndex);
            Vector oldBC = null;
            Vector newBC = null;
            if (boundaryConditionsData != null) {
                oldBC = boundaryConditionsData.getClonedBC();
                boundaryConditionsData.splitSegment(capturedShapeIndex,
                    capturedSegmentIndex);
                newBC = boundaryConditionsData.getClonedBC();
            }
            undoSupport.postEdit(new AddVertexToBoundaryEdit(
                        pvShape, v, capturedSegmentIndex, 
                        oldBC, newBC, undoCount));
            undoCount++;
        } else {
            super.onMousePressed(e);
        }
    }

    /**
     * Invoked when the mouse button has been released
     */
    public void onMouseReleased(MouseEvent e) {

        // overrides the super class method here because
        // we want to post a CreateBoundaryEdit here instead
        // of a CreateShapeEdit.
        if (activeMode == CREATE_SHAPE && newShape != null
                            && !newShape.isBeingCreated()) {
            if (onNewShapeCompletion(newShape)) {
                Vector oldBC = null;
                Vector newBC = null;
                if (boundaryConditionsData != null) {
                    oldBC = boundaryConditionsData.getClonedBC();
                    boundaryConditionsData.addBoundary(
                        ((mp2Polygon) newShape).getNumberOfVertices());
                    newBC = boundaryConditionsData.getClonedBC();
                }
                // add the new shape to the data
                shapesData.addShape(newShape);
                undoSupport.postEdit(new CreateBoundaryEdit(
                    newShape, oldBC, newBC, undoCount));
                undoCount++;
                if (newShape.getType() == mp2Shape.POLY_VERTEX_SHAPE) {
                    addVertexButton.setEnabled(true);
                }
                if (!sequentialShapeCreation) {
                    newShape.setSelected(true);
                    selectAndEditButton.setSelected(true);
                }
            }
            newShape = null;
            view.repaint();
            onMouseMoved(e);
        } else {
            super.onMouseReleased(e);
        }

        // If the exterior boundary is selected, disallow delete.
        if (activeMode == SELECT_AND_EDIT || activeMode == ADD_VERTEX) {
            if (shapesData.getShape(0) != null
                    && shapesData.getShape(0).isSelected()) {
                deleteMenuItem.setEnabled(false);
            }
        }
    }

    /**
     * Invoked when a new shape is completed. Note that
     * at this stage, the shape hasn't been added to the
     * shapes data.
     */
    protected boolean onNewShapeCompletion(mp2Shape shape) {
        if (createExteriorBoundaryButton.isSelected()) {
            createExteriorBoundaryButton.setEnabled(false);
            createInteriorBoundaryButton.setEnabled(true);
            selectAndEditButton.setSelected(true);
        } else {
            newShape.setFilled(true);
            newShape.setFillPattern(mp2Shape.FILL_DOTTED);
        }
        return true;
    }

    /**
     * Invoked when the "Select All" menu item is selected.
     * Selects all the shapes in this view.
     */
    protected void onSelectAll() {
        super.onSelectAll();
        // Because the exterior boundary is selected in this
        // operation, the delete menu item is disabled
        deleteMenuItem.setEnabled(false);
    }

    /**
     * Paints this domain. Paints the domain boundaries
     */
    public void paint(Graphics g) {
        Color oldColor = g.getColor();
        if (!isActive) {
            shapesData.deselectAllShapes();
        }
        if (view.isPaintModeDiscrete()) {
            shapesData.discretize();
            Vector d = ((mp2DomainData) shapesData).getDiscretizedDomain();
            g.setColor(Color.black);
            Point p1, p2;
            for (int i=0; i<d.size(); i+=2) {
                p1 = view.modelToView((double []) d.elementAt(i));
                p2 = view.modelToView((double []) d.elementAt(i+1));
                g.drawLine(p1.x, p1.y, p2.x, p2.y);
            }
        }
        else {
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                shapesData.getShape(i).draw(g);
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    /**
     * Loads the buttons of this graphical data view to the toolbar
     * on the main frame window.
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
        shapesData.deselectAllShapes();
        evaluateEditMenu();

        // Set up buttons on tool bar
        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(selectAndEditButton);
        toolBar.add(createExteriorBoundaryButton);
        toolBar.add(createInteriorBoundaryButton);
        toolBar.add(addVertexButton);
        if (gridData.isDefined()) {
            toolBar.add(discretizeButton);
        }
        toolBar.add(zoomButton);
        if (view.hasSiteMap()) {
            toolBar.add(Box.createVerticalStrut(10));
            toolBar.add(view.getXORButton());
        }

        if (shapesData.getShape(0) != null) {
            createExteriorBoundaryButton.setEnabled(false);
            createInteriorBoundaryButton.setEnabled(isEditable);
            addVertexButton.setEnabled(isEditable);
        } else {
            createExteriorBoundaryButton.setEnabled(isEditable);
            createInteriorBoundaryButton.setEnabled(false);
            addVertexButton.setEnabled(false);
        }
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
        super.setActiveModeTo(m);
        if (shapesData.getNumberOfShapes() > 0 
                    && shapesData.getShape(0).isSelected()) {
            deleteMenuItem.setEnabled(false);
        }
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        super.setEditable(b);
        if (!b) {
            if (createInteriorBoundaryButton.isSelected()
                || addVertexButton.isSelected()) {
                selectAndEditButton.setSelected(true);
            }
        }
        createInteriorBoundaryButton.setEnabled(b);
        addVertexButton.setEnabled(b);
    }

    /**
     * The undoable edit to create a boundary. 
     * Enables button to create exterior boundary if
     * exterior boundary is uncreated.
     */
    protected class CreateBoundaryEdit extends AbstractDataEdit {

        protected mp2Shape newShape;
        protected Vector oldBC;
        protected Vector newBC;

        public CreateBoundaryEdit(mp2Shape newShape, Vector oldBC,
                    Vector newBC, long undoIndex) {
            super(undoIndex);
            this.newShape = newShape;
            this.oldBC = oldBC;
            this.newBC = newBC;
        }

        public void undo() throws CannotUndoException {
            shapesData.deleteShapeAt(shapesData.getIndexOfShape(newShape));
            if (boundaryConditionsData != null && oldBC != null) {
                boundaryConditionsData.setBC(oldBC);
            }
            if (shapesData.getNumberOfShapes() == 0) {
                createExteriorBoundaryButton.setEnabled(true);
            }
            super.undo();
        }

        public void redo() throws CannotRedoException {
            shapesData.addShape(newShape);
            if (boundaryConditionsData != null && newBC != null) {
                boundaryConditionsData.setBC(newBC);
            }
            if (shapesData.getNumberOfShapes() > 0) {
                createExteriorBoundaryButton.setEnabled(false);
            }
            super.redo();
        }
    }

    /**
     * The undoable edit to add a vertex to a shape
     */
    protected class AddVertexToBoundaryEdit extends AbstractDataEdit {

        protected mp2PolyVertexShape pvShape;
        protected double [] v;
        protected int capturedSegmentIndex;
        protected Vector oldBC;
        protected Vector newBC;

        public AddVertexToBoundaryEdit(mp2PolyVertexShape pvShape, 
                        double [] v, int capturedSegmentIndex, 
                        Vector oldBC, Vector newBC, long undoIndex) {
            super(undoIndex);
            this.pvShape = pvShape;
            this.v = v;
            this.capturedSegmentIndex = capturedSegmentIndex;
            this.oldBC = oldBC;
            this.newBC = newBC;
        }

        public void undo() throws CannotUndoException {
            pvShape.deleteVertexAt(capturedSegmentIndex + 1);
            if (boundaryConditionsData != null && oldBC != null) {
                boundaryConditionsData.setBC(oldBC);
            }
            super.undo();
        }

        public void redo() throws CannotRedoException {
            pvShape.insertVertexOnSegment(v, capturedSegmentIndex);
            if (boundaryConditionsData != null && newBC != null) {
                boundaryConditionsData.setBC(newBC);
            }
            super.redo();
        }
    }

    /**
     * Undoable edit to delete boundary and associated bcs
     */
    protected class DeleteEdit extends AbstractDataEdit {

        protected Vector oldShapes;
        protected Vector newShapes;
        protected Vector oldBC;
        protected Vector newBC;

        DeleteEdit(Vector oldShapes, Vector newShapes, 
                   Vector oldBC, Vector newBC, long undoIndex) {
            super(undoIndex);
            this.oldShapes = oldShapes;
            this.newShapes = newShapes;
            this.oldBC = oldBC;
            this.newBC = newBC;
        }

        public void undo() throws CannotUndoException {
            shapesData.setShapes(oldShapes);
            if (boundaryConditionsData != null && oldBC != null) {
                boundaryConditionsData.setBC(oldBC);
            }
            super.undo();
        }

        public void redo() throws CannotRedoException {
            shapesData.setShapes(newShapes);
            if (boundaryConditionsData != null && newBC != null) {
                boundaryConditionsData.setBC(newBC);
            }
            super.redo();
        }
    }
}
