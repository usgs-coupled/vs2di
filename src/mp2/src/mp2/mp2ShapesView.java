/*
 * mp2ShapesView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.undo.*;

/**
 * Manages a data view that consists of mp2Shape objects
 * that are to be discretized.
 * Description to be added.
 *
 * @see mp2.mp2ShapesData
 */
public abstract class mp2ShapesView extends mp2GraphicalDataView {

    /**
     * The data for the shapes in this view
     */
    protected mp2ShapesData shapesData;

    /**
     * The rectilinear grid data
     */
    protected mp2AbstractGridData gridData;

    /**
     * Button to select and edit a shape
     */
    protected mp2ToggleButton selectAndEditButton;

    /**
     * Button to add a vertex to a shape
     */
    protected mp2ToggleButton addVertexButton;

    /**
     * Menu item to select all shapes in the view
     */
    protected JMenuItem selectAllMenuItem;

    /**
     * Menu item to delete the selected shape(s)
     */
    protected JMenuItem deleteMenuItem;

    /**
     * Menu item to bring a shape to the front
     */
    protected JMenuItem bringToFrontMenuItem;

    /**
     * Menu item to send a shape to the back
     */
    protected JMenuItem sendToBackMenuItem;

    /**
     * Indicates whether or not shapes in this view are allowed
     * to intersect one anther
     */
    protected boolean shapeIntersectionAllowed;

    /**
     * Error message when shape intersects another
     */
    protected String intersectOtherShapeErrorMessage;

    /**
     * Error message when shape intersects itself
     */
    protected String intersectSelfErrorMessage;

    /**
     * Indicates whether or not to switch to the "Select and edit"
     * mode after shape creation
     */
    protected boolean sequentialShapeCreation;

    /**
     * Indicates whether or not to draw shape values
     */
    protected boolean drawValue;

    protected boolean useRadialCoordinates;
    protected int leftBound;

    protected mp2Shape newShape;
    protected mp2Shape shapeUnderMouse;
    protected mp2PolyVertexShape shapeBeingReshaped;
    protected mp2PolyVertexShape shapeHavingVertexReset;
    protected Point mousePressedPoint;
    protected boolean shapeUnderMousePreviouslySelected;
    protected int capturedSegmentIndex;
    protected int capturedShapeIndex;
    protected int activeMode;
    public static final int CREATE_SHAPE = 0;
    public static final int SELECT_AND_EDIT = 1;
    public static final int ADD_VERTEX = 2;
    public static final int DISCRETIZE = 3;

    /**
     * Creates a new view that consists of shapes
     */
    public mp2ShapesView(mp2View view, 
                mp2ShapesData shapesData,
                mp2AbstractGridData gridData,
                String homeDirectory) {

        super(view, homeDirectory);
        this.shapesData = shapesData;
        this.gridData = gridData;
        shapeIntersectionAllowed = true;
        sequentialShapeCreation = true;
        drawValue = false;
        useRadialCoordinates = false;
        intersectOtherShapeErrorMessage = "Intersection not allowed";
        intersectSelfErrorMessage = "Self intersection not allowed";

        // Create buttons for tool bar
        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        selectAndEditButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "arrow.gif"), true);
        selectAndEditButton.setToolTipText("Select or Edit");
        selectAndEditButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(SELECT_AND_EDIT);
                }
            }
        });
        addVertexButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "addvertex.gif"), false);
        addVertexButton.setToolTipText("Add a vertex");
        addVertexButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(ADD_VERTEX);
                }
            }
        });

        // Create menu items for the "Edit" menu
        selectAllMenuItem = new JMenuItem("Select All");
        selectAllMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_A, Event.CTRL_MASK));
        selectAllMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onSelectAll();
            }
        });
        deleteMenuItem = new JMenuItem("Delete");
        deleteMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
        deleteMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDelete();
            }
        });
        bringToFrontMenuItem = new JMenuItem("Bring to Front");
        bringToFrontMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_F, Event.CTRL_MASK));
        bringToFrontMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onBringToFront();
            }
        });
        sendToBackMenuItem = new JMenuItem("Send to Back");
        sendToBackMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_B, Event.CTRL_MASK));
        sendToBackMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onSendToBack();
            }
        });

        activeMode = SELECT_AND_EDIT;
    }

    /**
     * Creates a new shape. Concrete subclass will decide which 
     * type of shape to create and return it.
     */
    protected abstract mp2Shape createNewShape(Point p);

    public void deselectAll() {
        shapesData.deselectAllShapes();
    }

    /**
     * Enables or disables items in the edit menu depending on the
     * state of this view.
     */
    protected void evaluateEditMenu() {
        if (activeMode == CREATE_SHAPE) {
            selectAllMenuItem.setEnabled(false);
        } else {
            selectAllMenuItem.setEnabled(shapesData.getNumberOfShapes() > 0);
        }
        boolean hasSelectedShapes = (shapesData.getNumberOfSelectedShapes() > 0);
        deleteMenuItem.setEnabled(hasSelectedShapes);
        bringToFrontMenuItem.setEnabled(hasSelectedShapes);
        sendToBackMenuItem.setEnabled(hasSelectedShapes);
    }

    /**
     * Determines whether or not a move is acceptable. If intersection
     * is not allowed, then check to make sure that moved shapes
     * do not intersect unmoved shapes.
     */
    protected boolean evaluateMove() {
        if (newShape instanceof mp2Point) {
            return true;
        }
        if (!shapeIntersectionAllowed) {
            // Test for intersection. Separate moved and unmoved polyvertex
            // shapes (polys) into 2 different vectors. For moved 
            // polys, work with shallow copy and call endMove to
            // update vertex coordinates. Then check that moved polys
            // don't intersect unmoved polys.
            Vector movedPolys = new Vector();
            Vector unmovedPolys = new Vector();
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                mp2Shape shape = shapesData.getShape(i);
                if (shape.getType() == mp2Shape.POLY_VERTEX_SHAPE) {
                    mp2PolyVertexShape poly = (mp2PolyVertexShape) shape;
                    if (poly.isSelected()) {
                        mp2PolyVertexShape polyCopy = poly.makeShallowCopy();
                        movedPolys.addElement(polyCopy);
                        polyCopy.endMove();
                    } else {
                        unmovedPolys.addElement(poly);
                    }
                }
            }
            for (int i=0; i<movedPolys.size(); i++) {
                mp2PolyVertexShape movedPoly = 
                            (mp2PolyVertexShape) movedPolys.elementAt(i);
                for (int j=0; j<unmovedPolys.size(); j++) {
                    mp2PolyVertexShape unmovedPoly = 
                                (mp2PolyVertexShape) unmovedPolys.elementAt(j);
                    if (movedPoly.intersects(unmovedPoly)) {
                        mp2MessageBox.showMessageDialog(
                            intersectOtherShapeErrorMessage, "Error");
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /**
     * Determines whether or not a newly created shape is acceptable
     */
    protected boolean evaluateNewShape() {
        if (!shapeIntersectionAllowed && newShape instanceof mp2Polygon) {
            mp2PolyVertexShape newPoly = 
                ((mp2PolyVertexShape) newShape).makeShallowCopy();
            if (newPoly.intersectsSelf()) {
                return false;
            }
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                if (shapesData.getShape(i).intersects(newPoly)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Determines whether or not a new vertex during shape creation
     * is acceptable. If intersection is not allowed, then check that
     * the new vertex does not cause to shape boundary to intersect
     * itself, and the shape does not intersect other shapes.
     */
    protected boolean evaluateNewVertex(Point p) {
        if (newShape instanceof mp2Point) {
            return true;
        }
        if (!shapeIntersectionAllowed) {
            mp2Polyline tempPoly = new mp2Polyline();
            tempPoly.init(view);
            tempPoly.setVertices((Vector) ((mp2PolyVertexShape) newShape).getVertices().clone());
            tempPoly.addNextVertex(p);
            if (tempPoly.intersectsSelf()) {
                return false;
            }
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                if (shapesData.getShape(i).intersects(tempPoly)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Determines whether or not the reshape is acceptable. If 
     * intersection is not allowed, check that the reshape does
     * not result in either the shape boundary intersects itself,
     * or the shape intersects another shape.
     */
    protected boolean evaluateReshape(Point p) {
        if (useRadialCoordinates) {
            double [] v = view.viewToModel(p);
            if (v[0] < 0) {
                mp2MessageBox.showMessageDialog(
                    "Radial coordinate (r) cannot be less than zero", "Error");
                return false;
            }
        }

        if (!shapeIntersectionAllowed) {
            // Test for intersection.
            mp2PolyVertexShape polyBeingReshaped = 
                ((mp2PolyVertexShape) shapeBeingReshaped).makeShallowCopy();
            polyBeingReshaped.endReshape();
            if (polyBeingReshaped.intersectsSelf()) {
                mp2MessageBox.showMessageDialog(
                    intersectSelfErrorMessage, "Error");
                return false;
            }
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                if (!shapeBeingReshaped.equals(shapesData.getShape(i))) {
                    if (shapesData.getShape(i).intersects(polyBeingReshaped)) {
                        mp2MessageBox.showMessageDialog(
                            intersectOtherShapeErrorMessage, "Error");
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /**
     * Invoked when "Bring to Front" menu item is selected. Bring
     * the selected shape(s) to the front.
     */
    protected void onBringToFront() {
        Vector oldShapes = shapesData.getClonedShapes();
        shapesData.bringSelectedShapesToFront();
        Vector newShapes = shapesData.getClonedShapes();
        undoSupport.postEdit(new ShapesEdit(oldShapes, newShapes, undoCount));
        undoCount++;
        view.repaint();
    }

    /**
     * Invoked when the "Delete" menu item is selected.
     * Deletes the selected shape(s).
     */
    protected void onDelete() {
        if (shapesData.getNumberOfSelectedShapes() == 0) {
            return;
        }
        Vector oldShapes = shapesData.getClonedShapes();
        shapesData.deleteSelectedShapes();
        Vector newShapes = shapesData.getClonedShapes();
        undoSupport.postEdit(new ShapesEdit(oldShapes, newShapes, undoCount));
        undoCount++;
        evaluateEditMenu();
        view.repaint();
    }

    /**
     * Invoked when a mouse button is pressed on the view and
     * and then dragged.
     */
    public void onMouseDragged(MouseEvent e) {
        Point p = e.getPoint();
        if (activeMode == SELECT_AND_EDIT) {
            // Reshape 
            if (shapeBeingReshaped != null) {
                shapeBeingReshaped.reshape(p);
                return;
            }
            if (shapeHavingVertexReset != null) {
                return;
            }
            // Move
            if (shapeUnderMouse != null) {
                if (shapeUnderMouse instanceof mp2Point &&
                        e.getModifiers()==InputEvent.BUTTON3_MASK) {
                    return;
                }
                if (shapeUnderMousePreviouslySelected) {
                    if (useRadialCoordinates) {
                        if (p.x - mousePressedPoint.x + leftBound
                                < view.getXOriginInPixels()) {
                            p.x = view.getXOriginInPixels() + mousePressedPoint.x - leftBound;
                        }
                    }
                    for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                        mp2Shape shape = shapesData.getShape(i);
                        if (shape.isSelected()) {
                            shape.move(p);
                        }
                    }
                } else {
                    Graphics tempG = view.getGraphics();
                    tempG.setColor(Color.black);
                    tempG.setXORMode(Color.white);
                    Rectangle viewRect = view.getVisibleRect();
                    tempG.clipRect(viewRect.x, viewRect.y, viewRect.width, viewRect.height);
                    for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                        mp2Shape shape = shapesData.getShape(i);
                        if (shape.isSelected()) {
                            shape.drawHandles(tempG);
                            if (!shape.equals(shapeUnderMouse)) {
                                shape.setSelected(false);
                            }
                        }
                    }
                    tempG.dispose();
                    if (useRadialCoordinates) {
                        mp2RectBounds bounds = shapeUnderMouse.getBounds();
                        leftBound = view.modelToView(bounds.x, bounds.y).x;
                        if (p.x - mousePressedPoint.x + leftBound
                                < view.getXOriginInPixels()) {
                            p.x = view.getXOriginInPixels() + mousePressedPoint.x - leftBound;
                        }
                    }
                    shapeUnderMouse.move(p);
                    shapeUnderMousePreviouslySelected = true;
                }
                return;
            }
        }
    }

    /**
     * Invoked when the mouse has moved on the view (with no 
     * buttons down).
     */
    public void onMouseMoved(MouseEvent e) {

        if (activeMode == CREATE_SHAPE && newShape != null 
                                 && newShape.isBeingCreated()) {
            newShape.onMouseMovedDuringCreation(e.getPoint());
            return;
        }

        if (activeMode == SELECT_AND_EDIT) {
            // Find the front-most shape that is under the mouse,
            // That is, search from front to back and break when 
            // when encounter first shape that in under the mouse. 
            shapeUnderMouse = null;
            for (int i=shapesData.getNumberOfShapes()-1; i>=0; i--) {
                mp2Shape shape = shapesData.getShape(i);
                if (shape.isUnderMouse(e.getPoint())) {
                    shapeUnderMouse = shape;
                    break;
                }
            }
            // Display cursor depending if a shape is under the mouse
            if (shapeUnderMouse != null) {
                view.setCursor(shapeUnderMouse.getMoveCursor());
            } else {
                view.setCursor(Cursor.getDefaultCursor());
            }
            // Find handle to capture
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                mp2Shape shape = shapesData.getShape(i);
                if (shape.isSelected()) {
                    shape.findHandleToCapture(e.getPoint());
                }
            }
            return;
        }

        if (activeMode == ADD_VERTEX) {
            // "Capture" the edge of a poly-vertex shape. If captured,
            // change cursor to crosshair.
            for (int i=shapesData.getNumberOfShapes()-1; i>=0; i--) {
                mp2Shape shape = shapesData.getShape(i);
                // Consider only shapes of poly-vertex type
                if (shape.getType() == mp2Shape.POLY_VERTEX_SHAPE) {
                    mp2PolyVertexShape pvShape = (mp2PolyVertexShape) shape;
                    double [] v = view.viewToModel(e.getPoint());
                    capturedSegmentIndex = -2;
                    capturedShapeIndex = -2;
                    // Check if point is covered by overlying shapes
                    for (int j=shapesData.getNumberOfShapes()-1; j>i; j--) {
                        mp2Shape overlyingShape = shapesData.getShape(j);
                        if (overlyingShape.isFilled()
                                    && overlyingShape.contains(v)) {
                            capturedSegmentIndex = -1;
                        }
                    }
                    // if the point is not covered by overlying shapes,
                    // check if point is over an edge
                    if (capturedSegmentIndex != -1) {
                        capturedSegmentIndex = pvShape.getSegmentUnderPoint(
                                       view.viewToModel(e.getPoint()), 1);
                    }
                    // change cursor depending of whether an edge is captured
                    if (capturedSegmentIndex > -1) {
                        view.setCursor(Cursor.getPredefinedCursor(
                                    Cursor.CROSSHAIR_CURSOR));
                        capturedShapeIndex = i;
                        return;
                    } else {
                        view.setCursor(Cursor.getDefaultCursor());
                    }
                }
            }
            return;
        }
    }

    /**
     * Invoked when a mouse button has been pressed on the view.
     */
    public void onMousePressed(MouseEvent e) {

        mousePressedPoint = e.getPoint();

        if (activeMode == CREATE_SHAPE) {
            // First, make sure the clicked point is inside the 
            // drawing.
            Point p = e.getPoint();
            if (p.x >= view.getDrawingWidthInPixels() || 
                    p.y >= view.getDrawingHeightInPixels()) {
                return;
            }
            // If using radial coordinates, the point must be
            // be right of the origin.
            if (useRadialCoordinates) {
                double [] v = view.viewToModel(p);
                if (v[0] < 0) {
                    return;
                }
            }

            // If a new shape doesn't exist, instantiate one.
            if (newShape == null) {
                newShape = createNewShape(p);
                if (newShape == null) {
                    return;
                }
                newShape.init(view);
                newShape.prepareToCreate(p);
            }
            if (e.getClickCount() == 1) {
                if (evaluateNewVertex(p)) {
                    newShape.addNextVertex(p);
                }
            } else if (e.getClickCount() == 2
                        && newShape.getType() 
                            == mp2Shape.POLY_VERTEX_SHAPE
                        && evaluateNewShape()
                        && newShape.getNumberOfVertices()
                            >= newShape.getMinimumNumberOfVertices()) {
                newShape.endCreation();
            }
            return;
        }

        if (activeMode == SELECT_AND_EDIT) {
            // If a handle has been captured, the set up to either
            // 1. reshape if left button is not pressed, or
            // 2. reset vertex if right button (BUTTON3) is pressed
            shapeBeingReshaped = null;
            shapeHavingVertexReset = null;
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                mp2Shape shape = shapesData.getShape(i);
                if (shape.hasCapturedHandle()) {
                    if (e.getModifiers()==InputEvent.BUTTON3_MASK) {
                        shapeHavingVertexReset = (mp2PolyVertexShape) shape;
                    } else {
                        shapeBeingReshaped = (mp2PolyVertexShape) shape;
                        shapeBeingReshaped.prepareToReshape();
                    }
                    return;
                }
            }
            // If no handle is captured but a shape is under the
            // mouse, then set up to move.
            if (shapeUnderMouse != null) {
                shapeUnderMousePreviouslySelected = shapeUnderMouse.isSelected();
                shapeUnderMouse.setSelected(true);
                // if the shape is a point and the right button is clicked,
                // we reset the point coordinates via a dialog box when the
                // button is released.
                if (shapeUnderMouse instanceof mp2Point &&
                        e.getModifiers()==InputEvent.BUTTON3_MASK) {
                    return;
                }
                leftBound = Integer.MAX_VALUE;
                for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                    mp2Shape shape = shapesData.getShape(i);
                    if (shape.isSelected()) {
                        shape.prepareToMove(e.getPoint());
                        if (useRadialCoordinates) {
                            mp2RectBounds bounds = shape.getBounds();
                            int x = view.modelToView(bounds.x, bounds.y).x;
                            if (x < leftBound) {
                                leftBound = x;
                            }
                        }
                    }
                }
            }
            // If no handle is captured and no shape in under the mouse,
            // then deselect all shapes.
            else {
                shapesData.deselectAllShapes();
            }
            return;
        }

        if (activeMode == ADD_VERTEX && capturedSegmentIndex > -1) {
            mp2PolyVertexShape pvShape = 
                (mp2PolyVertexShape) shapesData.getShape(capturedShapeIndex);
            double [] v = view.viewToModel(e.getPoint());
            undoSupport.postEdit(new AddVertexEdit(pvShape, v, 
                                capturedSegmentIndex, undoCount));
            undoCount++;
            pvShape.insertVertexOnSegment(v, capturedSegmentIndex);
            return;
        }
    }

    /**
     * Invoked when the mouse button has been released
     */
    public void onMouseReleased(MouseEvent e) {

        if (activeMode == CREATE_SHAPE && newShape != null
                            && !newShape.isBeingCreated()) {
            if (onNewShapeCompletion(newShape)) {
                // create an undoable edit to record the effect of the edit,
                // and place it in the undo manager's undo queue
                // *** we also should note whether or not the data has
                // been previously changed, so undo can properly restore the state.
                // to be implemented.
                undoSupport.postEdit(new CreateShapeEdit(newShape, undoCount));
                undoCount++;
                // add the new shape to the data
                shapesData.addShape(newShape);
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
            return;
        }
        
        if (activeMode == SELECT_AND_EDIT) {
            Point p = e.getPoint();
            if (shapeBeingReshaped != null) {
                if (evaluateReshape(p)) {
                    int i = shapeBeingReshaped.getCapturedIndex();
                    undoSupport.postEdit(new ReshapeEdit(
                            shapeBeingReshaped, i,
                            shapeBeingReshaped.getVertex(i),
                            shapeBeingReshaped.getCapturedVertex(),
                            undoCount));
                    undoCount++;
                    shapeBeingReshaped.endReshape();
                    shapesData.setDataHaveChanged();
                }
            }
            else if (shapeHavingVertexReset != null) {
                int i = shapeHavingVertexReset.getCapturedIndex();
                double [] oldVertex = shapeHavingVertexReset.getCapturedVertex();
                mp2VertexCoordinatesDialog dlg = new mp2VertexCoordinatesDialog(useRadialCoordinates);
                dlg.xCoord = oldVertex[0];
                dlg.yCoord = oldVertex[1];
                if (dlg.doModal() == true) {
                    double [] newVertex = new double[2];
                    newVertex[0] = dlg.xCoord;
                    newVertex[1] = dlg.yCoord;
                    undoSupport.postEdit(new ReshapeEdit(
                            shapeHavingVertexReset, i,
                            oldVertex, newVertex, undoCount));
                    undoCount++;
                    shapeHavingVertexReset.setVertexAt(newVertex, i);
                    shapesData.setDataHaveChanged();
                }
            }
            else if (shapeUnderMouse != null) {
                if (shapeUnderMouse instanceof mp2Point &&
                        e.getModifiers()==InputEvent.BUTTON3_MASK) {
                    setPointPosition();
                }else if (mousePressedPoint.x == p.x 
                            && mousePressedPoint.y == p.y) {
                    if (!e.isShiftDown() && !e.isControlDown()) {
                        shapesData.deselectAllShapes();
                        shapeUnderMouse.setSelected(true);
                    } else {
                        if (shapeUnderMousePreviouslySelected) {
                            shapeUnderMouse.setSelected(false);
                        } else {
                            shapeUnderMouse.setSelected(true);
                        }
                    }
 
                } else {
                    if (evaluateMove()) {
                        int n = shapesData.getNumberOfShapes();
                        boolean [] shapeIsSelected = new boolean[n];
                        double [] start = view.viewToModel(mousePressedPoint);
                        double [] end = view.viewToModel(e.getPoint());
                        double [] displacement = new double[2];
                        displacement[0] = end[0] - start[0];
                        displacement[1] = end[1] - start[1];

                        for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                            mp2Shape shape = shapesData.getShape(i);
                            if (shape.isSelected()) {
                                shapeIsSelected[i] = true;
                                shape.endMove();
                            }
                        }
                        undoSupport.postEdit(new MoveEdit(shapeIsSelected,
                                                   displacement, undoCount));
                        undoCount++;
                        shapesData.setDataHaveChanged();
                    }
                }
            }
            evaluateEditMenu();
            view.repaint();
            return;
        }


        if (activeMode == ADD_VERTEX && capturedSegmentIndex > -1) {
            mp2PolyVertexShape capturedShape = 
                    (mp2PolyVertexShape) shapesData.getShape(
                    capturedShapeIndex);
            capturedShape.setSelected(true);
            evaluateEditMenu();
            shapesData.setDataHaveChanged();
            view.repaint();
            return;
        }
    }

    /**
     * Invoked when a new shape is completed.
     */
     protected boolean onNewShapeCompletion(mp2Shape shape) {
        return true;
     }

    /**
     * Invoked when select and edit ends (mouse released)
     */
    protected void onSelectAndEditCompletion() {}

    /**
     * Invoked when the "Select All" menu item is selected.
     * Selects all the shapes in this view.
     */
    protected void onSelectAll() {
        shapesData.selectAllShapes();
        deleteMenuItem.setEnabled(true);
        view.repaint();
    }

    protected void setPointPosition() {
        mp2VertexCoordinatesDialog dlg = new mp2VertexCoordinatesDialog(useRadialCoordinates);
        mp2Point pt = (mp2Point) shapeUnderMouse;
        double [] oldVertex = pt.getCoord();
        dlg.xCoord = oldVertex[0];
        dlg.yCoord = oldVertex[1];
        if (dlg.doModal() == true) {
            double [] displacement = new double [2];
            displacement[0] = dlg.xCoord - oldVertex[0];
            displacement[1] = dlg.yCoord - oldVertex[1];
            int n = shapesData.getNumberOfShapes();
            boolean [] shapeIsSelected = new boolean[n];
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                shapeIsSelected[i] = shapesData.getShape(i).isSelected();
            }
            undoSupport.postEdit(new MoveEdit(shapeIsSelected,
                                       displacement, undoCount));
            undoCount++;
            pt.translate(displacement[0], displacement[1]);
            shapesData.setDataHaveChanged();
        }
        shapesData.deselectAllShapes();
        pt.setSelected(true);
    }

    /**
     * Invoked when "Send to Back" menu item is selected. 
     * Sends the selected shape(s) to the back.
     */
    protected void onSendToBack() {
        Vector oldShapes = shapesData.getClonedShapes();
        shapesData.sendSelectedShapesToBack();
        Vector newShapes = shapesData.getClonedShapes();
        undoSupport.postEdit(new ShapesEdit(oldShapes, newShapes, undoCount));
        undoCount++;
        view.repaint();
    }

    /**
     * Paints the shapes
     */
    public void paint(Graphics g) {
        Color oldColor = g.getColor();
        if (!isActive) {
            shapesData.deselectAllShapes();
        }
        if (!isVisible) {
            return;
        }
        if (view.isPaintModeDiscrete()) {
            return;
        } else {
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                shapesData.getShape(i).draw(g);
                if (drawValue) {
                    shapesData.getShape(i).drawValue(g);
                }
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    /**
     * Paints on top of everything else on the view. Draws handles
     * of selected shape.
     */
    public void paintLast(Graphics g) {
        Color oldColor = g.getColor();
        g.setColor(Color.black);
        g.setXORMode(Color.white);
        for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
            mp2Shape shape = shapesData.getShape(i);
            if (shape.isSelected()) {
                shape.drawHandles(g);
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    /**
     * Sets the active mode
     */
    protected void setActiveModeTo(int m) {
        activeMode = m;

        if (activeMode == CREATE_SHAPE) {
            view.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
            shapesData.deselectAllShapes();
        } else {
            view.setCursor(Cursor.getDefaultCursor());
        }

        if (activeMode == DISCRETIZE) {
            view.setPaintModeToDiscrete(true);
            shapesData.deselectAllShapes();
        } else {
            view.setPaintModeToDiscrete(false);
        }
        evaluateEditMenu();
        view.repaint();
    }

    /**
     * Sets whether or not shape values are drawn
     */
    public void setDrawValue(boolean b) {
        drawValue = b;
    }

    /**
     * Sets whether or not shapes can be created sequentially
     */
    public void setSequentialShapeCreation(boolean b) {
        sequentialShapeCreation = b;
    }

    /**
     * Sets whether or not shapes allowed to intersect itself or
     * one another
     */
    public void setShapeIntersectionAllowed(boolean b) {
        shapeIntersectionAllowed = b;
    }

    /**
     * Sets whether or not radial coordinates are used.
     */
    public void setUseRadialCoordinates(boolean b) {
        useRadialCoordinates = b;
    }

    /**
     * The undoable edit to create a shape
     */
    protected class CreateShapeEdit extends AbstractDataEdit {

        protected mp2Shape newShape;

        public CreateShapeEdit(mp2Shape newShape, long undoIndex) {
            super(undoIndex);
            this.newShape = newShape;
        }

        public void undo() throws CannotUndoException {
            shapesData.deleteShapeAt(shapesData.getIndexOfShape(newShape));
            super.undo();
        }

        public void redo() throws CannotRedoException {
            shapesData.addShape(newShape);
            super.redo();
        }
    }

    /**
     * The undoable edit to move selected shapes
     */
    protected class MoveEdit extends AbstractDataEdit {

        protected boolean [] shapeIsSelected;
        protected double [] displacement;

        public MoveEdit(boolean [] shapeIsSelected, 
                        double [] displacement, long undoIndex) {
            super(undoIndex);
            this.shapeIsSelected = shapeIsSelected;
            this.displacement = displacement;
        }

        public void undo() throws CannotUndoException {
            for (int i=0; i<shapeIsSelected.length; i++) {
                if (shapeIsSelected[i]) {
                    shapesData.getShape(i).translate(-displacement[0],
                                                     -displacement[1]);
                }
            }
            shapesData.setDataHaveChanged();
            super.undo();
        }

        public void redo() throws CannotRedoException {
            for (int i=0; i<shapeIsSelected.length; i++) {
                if (shapeIsSelected[i]) {
                    shapesData.getShape(i).translate(displacement[0],
                                                     displacement[1]);
                }
            }
            shapesData.setDataHaveChanged();
            super.redo();
        }
    }

    /**
     * The undoable edit to reshape a shape (move a vertex)
     */
    protected class ReshapeEdit extends AbstractDataEdit {

        protected mp2PolyVertexShape pvShape;
        protected int vertexIndex;
        protected double [] oldVertexCoord;
        protected double [] newVertexCoord;

        public ReshapeEdit(mp2PolyVertexShape pvShape, 
                             int vertexIndex,
                             double [] oldVertexCoord, 
                             double [] newVertexCoord,
                             long undoIndex) {
            super(undoIndex);
            this.pvShape = pvShape;
            this.vertexIndex = vertexIndex;
            this.oldVertexCoord = oldVertexCoord;
            this.newVertexCoord = newVertexCoord;
        }

        public void undo() throws CannotUndoException {
            pvShape.setVertexAt(oldVertexCoord, vertexIndex);
            shapesData.setDataHaveChanged();
            super.undo();
        }

        public void redo() throws CannotRedoException {
            pvShape.setVertexAt(newVertexCoord, vertexIndex);
            shapesData.setDataHaveChanged();
            super.redo();
        }
    }

    /**
     * The undoable edit to add a vertex to a shape
     */
    protected class AddVertexEdit extends AbstractDataEdit {

        protected mp2PolyVertexShape pvShape;
        protected double [] v;
        protected int capturedSegmentIndex;

        public AddVertexEdit(mp2PolyVertexShape pvShape, 
                        double [] v, int capturedSegmentIndex, long undoIndex) {
            super(undoIndex);
            this.pvShape = pvShape;
            this.v = v;
            this.capturedSegmentIndex = capturedSegmentIndex;
        }

        public void undo() throws CannotUndoException {
            pvShape.deleteVertexAt(capturedSegmentIndex + 1);
            shapesData.setDataHaveChanged();
            super.undo();
        }

        public void redo() throws CannotRedoException {
            pvShape.insertVertexOnSegment(v, capturedSegmentIndex);
            shapesData.setDataHaveChanged();
            super.redo();
        }
    }

    /**
     * Undoable edit to delete selected shapes, bring to front,
     * or send to rear
     */
    protected class ShapesEdit extends AbstractDataEdit {

        protected Vector oldShapes;
        protected Vector newShapes;

        ShapesEdit(Vector oldShapes, Vector newShapes, long undoIndex) {
            super(undoIndex);
            this.oldShapes = oldShapes;
            this.newShapes = newShapes;
        }

        public void undo() throws CannotUndoException {
            shapesData.setShapes(oldShapes);
            super.undo();
        }

        public void redo() throws CannotRedoException {
            shapesData.setShapes(newShapes);
            super.redo();
        }
    }
}
