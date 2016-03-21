/*
 * mp2Point.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Vector;

/**
 * TO BE REVISED. THIS VERSION NEEDS BETTER ORGANIZATION.
 * Encapsulates a point
 *
 * <P>This class implements the Serializable interface so that
 * objects can be serialized and deserialized. Therefore,
 * object creation is a two-step procedure in which
 * the <code>init</code> method is called after construction
 * or deserialization. This is similar to how document objects
 * are created.
 *
 * @see mp2.mp2Doc
 * @see mp2.mp2View
 */
public class mp2Point extends mp2Shape implements 
        mp2Constants, Serializable {

    static final long serialVersionUID = -6706742515636653681L;

    protected double x;
    protected double y;
    protected int pointIcon;
    protected transient Point oldPoint;
    protected transient Point tempPoint;
    public static final int SQUARE_ICON = 0;
    public static final int DIAMOND_ICON = 1;
    public static final int [] DIAMOND_X = 
        {0, 1, 2, 3, 4, 3, 2, 1, 0, -1, -2, -3, -4, -3, -2, -1,
         0, 1, 2, 3, 2, 1, 0, -1, -2, -3, -2, -1};
    public static final int [] DIAMOND_Y =
        {-4, -3, -2, -1, 0, 1, 2, 3, 4, 3, 2, 1, 0, -1, -2, -3,
         -3, -2, -1, 0, 1, 2, 3, 2, 1, 0, -1, -2}; 
    public static final int [] DIAMOND_FILL_X = 
        {0, -1, 0, 1, -2, -1, 0, 1, 2, -1, 0, 1, 0};
    public static final int [] DIAMOND_FILL_Y = 
        {-2, -1, -1, -1, 0, 0, 0, 0, 0, 1, 1, 1, 2};

    public mp2Point() {
        super();
        pointIcon = SQUARE_ICON;
    }

    /**
     * Determines whether or not the specified point is contained
     * by this shape. Always returns false.
     */
    public boolean contains(double xx, double yy) {
        return false;
    }

    /**
     * Draws the point
     *
     * @param  g  The Graphics object for view.
     */
    public void draw(Graphics g) {
        g.setColor(foreground);
        if (isSelected) {
            g.setXORMode(background);
        }
        drawPointIcon(g, view.modelToView(x, y));
    }

    public void draw(Graphics g, Color color) {
        g.setColor(color);
        if (isSelected) {
            g.setXORMode(background);
        }
        drawPointIcon(g, view.modelToView(x, y));
    }

    public void drawHandles(Graphics g) {
        drawHandleIcon(g, view.modelToView(x, y));
    }

    public void drawOpenHandles(Graphics g) {
        // doesn't apply 
    }

    public void drawValue(Graphics g) {
        // to be implemented
    }

    public double [] getCoord() {
        double [] v = new double[2];
        v[0] = x;
        v[1] = y;
        return v;
    }

    public int getMinimumNumberOfVertices() {
        return 1;
    }

    public Cursor getMoveCursor() {
        return Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR);
    }

    public int getNumberOfVertices() {
        return 1;
    }

    public int getType() {
        return POINT;
    }

    public boolean isFilled() {
        return false;
    }

    public void addNextVertex(Point p) {
        isBeingCreated = false;
    }

    public void findHandleToCapture(Point p) {
        // Does nothing in this class
    }

    public boolean hasCapturedHandle() {
        return false;
    }

    public boolean intersects(mp2PolyVertexShape otherPoly) {
        return false;
    }

    /**
     * Indicates whether or not this shape is "under" the 
     * mouse at the specified point
     */
    public boolean isUnderMouse(Point p) {
        Point q = view.modelToView(x, y);
        return mp2Math.withinNeighborhood(p, q, HANDLE_SIZE);
    }

    public void prepareToCreate(Point p) {
        // Draw the clicked point.
        Graphics g = view.getGraphics();
        g.setColor(Color.black);
        Rectangle viewRect = view.getVisibleRect();
        g.clipRect(viewRect.x, viewRect.y, viewRect.width, 
                                           viewRect.height);
        double [] v = view.viewToModel(p);
        x = v[0];
        y = v[1];
        draw(g);
        g.dispose();
    }

    public void prepareToMove(Point p) {
        // Set up to draw in XOR mode
        g = view.getGraphics();
        g.setColor(Color.black);
        g.setXORMode(Color.white);
        Rectangle viewRect = view.getVisibleRect();
        g.clipRect(viewRect.x, viewRect.y, viewRect.width, 
                                           viewRect.height);
        tempPoint = view.modelToView(x, y);
        oldPoint = new Point(p.x, p.y);
        hasChangedSinceSelected = false;
    }

    public void move(Point p) {
        // Erase the previous point by view over it,
        // except first time around.
        if (hasChangedSinceSelected) {
            drawPointIcon(g, tempPoint);
        } else {
            hasChangedSinceSelected = true;
        }
        // Draw the new point
        // Translate the outline
        int dx = p.x - oldPoint.x;
        int dy = p.y - oldPoint.y;
        tempPoint.x += dx;
        tempPoint.y += dy;
        drawPointIcon(g, tempPoint);
        // Save the curson position
        oldPoint = p;
        return;
    }

    public void endMove() {
        if (hasChangedSinceSelected) {
            double [] v = view.viewToModel(tempPoint);
            x = v[0];
            y = v[1];
        }
        disposeGraphics();
    }

    public void setIcon(int icon) {
        pointIcon = icon;
    }

    protected void drawPointIcon(Graphics g, Point p) {
        if (pointIcon == DIAMOND_ICON) {
            for (int i=0; i<DIAMOND_X.length; i++) {
                g.fillRect(p.x + DIAMOND_X[i], p.y + DIAMOND_Y[i], 1, 1);
            }
        } else {
            g.drawRect(p.x - HALF_HANDLE_SIZE, p.y - HALF_HANDLE_SIZE,
                       HANDLE_SIZE - 1, HANDLE_SIZE - 1);
            g.drawRect(p.x - HALF_HANDLE_SIZE + 1, p.y - HALF_HANDLE_SIZE + 1,
                       HANDLE_SIZE - 3, HANDLE_SIZE - 3);
        }
    }

    protected void drawHandleIcon(Graphics g, Point p) {
        if (pointIcon == DIAMOND_ICON) {
            for (int i=0; i<DIAMOND_FILL_X.length; i++) {
                g.fillRect(p.x + DIAMOND_FILL_X[i], 
                           p.y + DIAMOND_FILL_Y[i], 1, 1);
            }
        } else {
            g.fillRect(p.x - HALF_HANDLE_SIZE + 2, p.y - HALF_HANDLE_SIZE + 2,
                HANDLE_SIZE - 4, HANDLE_SIZE - 4);
        }
    }

    public void translate(double dx, double dy) {
        x += dx;
        y += dy;
    }
}
