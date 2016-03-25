/*
 * mp2Shape.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

/**
 * Defines the behavior of a gemoetric shape. A shape has
 * a foreground color and a background color. All coordinate
 * points defining the shape are expressed in model-specified 
 * units, for example, meters. 
 *
 * <P>For a shape to interact with the screen/mouse,
 * it is necessary to convert betwen model-specified coordinates
 * and screen coordinates (pixels). To do this, a shape holds
 * a reference to the view in order to access the 
 * <code>modelToView</code> and <code>viewToModel</code>
 * methods.</P>
 *
 * <P>A shape can also be "selected" on the screen.
 * When the shape is selected, the user can edit it by
 * mouse clicks and mouse movements. This class implements
 * default (do-nothing) methods to handle mouse events.
 * These methods can be overridden as needed by a concrete
 * subclass.</P>
 *
 * <P>This class implements the Serializable interface so that
 * objects can be serialized and deserialized. Therefore,
 * object creation is a two-step procedure in which
 * the <code>init</code> method is called after construction
 * or deserialization. This is similar to how document objects
 * are created.
 *
 * @see mp2.mp2Doc
 * @see mp2.mp2Drawing
 */
public abstract class mp2Shape implements Serializable, mp2Constants {

    static final long serialVersionUID = -5624814269217511201L;

    public static final int POLY_VERTEX_SHAPE = 1;

    public static final int POINT = 2;

    public static final int FILL_SOLID = 0;

    public static final int FILL_DOTTED = 1;

    /**
     * The foreground color of this shape.
     */
    protected Color foreground;

    /**
     * The background color of this shape
     */
    protected Color background;

    /**
     * The rectangular boundary that contains the shape
     */
    protected mp2RectBounds bounds;

    /**
     * The id of this shape
     */
    protected int id;
    
    /**
     * Indicates whether or not there is a value associated with this shape
     */
    protected boolean hasValue;

    /**
     * The scalar value associated with this shape
     */
    protected double value;

    /**
     * Indicates whether the shape is filled or not. 
     */
    protected boolean isFilled;

    /**
     * The fill pattern
     */
    protected int fillPattern;

    /**
     * Indicates whether or not this shape is being created.
     */
    protected boolean isBeingCreated;
    
    /**
     * The name of this shape (any character string)
     */
    protected String name;
    
    protected Object customObject;

    /**
     * The view in which this shape is displayed.
     */
    protected transient mp2Drawing view;

    /**
     * The graphics object for view
     */
    protected transient Graphics g;

    /**
     * Indicates whether or not this shape is selected.
     */
    protected transient boolean isSelected;

    /**
     * Indicates whether or not this shaped has changed since
     * it was selected.
     */
    protected transient boolean hasChangedSinceSelected;

    /**
     * Called by concrete subclass during construction
     */
    public mp2Shape() {
        foreground = Color.black;
        background = Color.white;
        id = -1;
        hasValue = false;
        value = 0;
        bounds = new mp2RectBounds();
        isFilled = false;
        isBeingCreated = true;
        fillPattern = FILL_SOLID;
        name = new String();
    }
    
    public void setCustomObject(Object obj) {
        customObject = obj;
    }
    
    public Object getCustomObject() {
        return customObject;
    }

    /**
     * Determines whether or not the specified point is contained by 
     * this shape
     *
     * @param  x  the <i>x</i> coordinate of the point to be tested
     * @parem  y  the <i>y</i> coordinate of the point to be tested
     *
     * @return <code>true</code> if the point (<i>x</i>, <i>y</i>) is 
     *         contained by this shape; <code>false</code> otherwise.
     */
    public abstract boolean contains(double x, double y);

    /**
     * Determines whether or not the specified point is contained by 
     * this shape. 
     *
     * @param  v  the 2-element double array whose first element is the
     *            <i>x</i> coordinate of the point to be tested, and
     *            the second element is the <i>y</i> coordinate of the 
     *            point to be tested
     *
     * @return <code>true</code> if the point is 
     *         contained by this shape; <code>false</code> otherwise.
     */
    public boolean contains(double [] v) {
        if (v == null || v.length <2) {
            return false;
        }
        return contains(v[0], v[1]);
    }

    /**
     * Disposes of the Graphics object in this shape
     */
    public void disposeGraphics() {
        if (g != null) {
            g.dispose();
            g = null;
        }
    }

    /**
     * Draws this shape using the specified Graphics object.
     */
    public abstract void draw(Graphics g);

    /**
     * Draws the handles of this shape, using the specified
     * Graphics object.
     */
    public abstract void drawHandles(Graphics g);

    /**
     * Draws the handles of this shape as open rectangles, using 
     * the specified Graphics object.
     */
    public abstract void drawOpenHandles(Graphics g);

    public abstract void drawValue(Graphics g);

    /**
     * Gets the id of this shape
     */
    public int getId() {
        return id;
    }

    /**
     * Gets the rectangular boundary that contains the shape
     */
    public mp2RectBounds getBounds() {
        return bounds;
    }

    public Color getBackground() {
        return background;
    }
    
    public Color getForeground() {
        return foreground;
    }
    
    public double getLeftBound() {
        return bounds.x;
    }

    public Cursor getMoveCursor() {
        return Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR);
    }
    
    public String getName() {
        return name;
    }

    public abstract int getNumberOfVertices();

    public abstract int getMinimumNumberOfVertices();

    /**
     * Gets type of this shape.
     *
     * @return POLY_VERTEX_SHAPE if the shape is polyline or 
     *         polygon, POINT if the shape is a point.
     */
    public abstract int getType();

    /**
     * Gets the value of this shape
     */
    public double getValue() {
        return value;
    }

    /**
     * Indicates whether or not this shape has changed since it
     * was last selected.
     */
    public boolean hasChangedSinceSelected() {
        return hasChangedSinceSelected;
    }
    
    public boolean hasValue() {
        return hasValue;
    }

    /**
     * Indicates whether or not a handle of this shape has been
     * captured. A handle is captured if the mouse is sufficiently
     * close to the handle so that the handle is "detected".
     */
    public abstract boolean hasCapturedHandle();

    /**
     * Initializes this shape. Initializes the transient members
     * of this class.
     *
     * @param  view  The view in which this shape is displayed
     */
    public void init(mp2Drawing view) {
        this.view = view;
        g = null;
        isSelected = false;
        hasChangedSinceSelected = false;
        //bounds.init(view);
        if (name == null) {
            name = new String();
        }
    }

    /**
     * Indicates whether or not this shape intersects the
     * specified poly vertex shape
     */
    public abstract boolean intersects(mp2PolyVertexShape otherPoly);

    public boolean isBeingCreated() {
        return isBeingCreated;
    }

    /**
     * Indicates whether or not this figure is filled
     */
    public boolean isFilled() {
        return isFilled;
    }

    /**
     * Indicates whether or not this shape is selected.
     *
     * @return <code>true</code> if this shape is selected.
     *         <code>false</code> otherwise.
     */
    public boolean isSelected() {
        return isSelected;
    }

    /**
     * Indicates whether or not this shape is "under" the 
     * mouse at the specified point. The meaning of "under"
     * depends on the subclass
     */
    public abstract boolean isUnderMouse(Point p);

    /**
     * Sets the background color.
     *
     * @param  color  the background color for this shape
     */
    public void setBackground(Color color) {
        background = color;
    }

    /**
     * Specifies whether or not the polygon should be filled
     */
    public void setFilled(boolean b) {
        isFilled = b;
    }

    /**
     * Sets the fill pattern
     */
    public void setFillPattern(int p) {
        fillPattern = p;
    }

    /**
     * Sets the foreground color.
     *
     * @param  color  the foreground color for this shape
     */
    public void setForeground(Color color) {
        foreground = color;
    }

    /**
     * Sets the id of this shape
     */
    public void setId(int id) {
        this.id = id;
    }
    
    public void setName(String n) {
        name = n;
    }

    /**
     * Sets this shape as selected or not selected, depending
     * on the argument.
     *
     * @param  b  is <code>true</code> if the shape is to
     *            be set as selected, <code>false</code> otherwise.
     */
    public void setSelected(boolean b) {
        isSelected = b;
    }

    /**
     * Sets the value of this shape
     */
    public void setValue(double value) {
        this.value = value;
        hasValue = true;
    }

    public void prepareToCreate(Point p) {}

    public void addNextVertex(Point p) {}

    public void onMouseMovedDuringCreation(Point p) {}

    public void endCreation() {}

    public void findHandleToCapture(Point p) {}

    public void prepareToReshape() {}

    public void reshape(Point p) {}

    public void endReshape() {}

    public void prepareToMove(Point p) {}

    public void move(Point p) {}

    public void endMove() {}

    public abstract void translate(double dx, double dy);

}
