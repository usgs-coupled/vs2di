/*
 * mp2PolyVertexShape.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

/**
 * Encapsulates a shape that is defined by vertices, such as
 * a polygon or a polyline
 */
public abstract class mp2PolyVertexShape extends mp2Shape 
        implements mp2Constants, Serializable {

    static final long serialVersionUID = -7134751044012565150L;

    protected Vector vertices;

    protected transient int capturedIndex;
    protected transient Point capturedVertex;
    protected transient Point priorVertex;
    protected transient Point nextVertex;
    protected transient Point firstVertex;
    protected transient Point lastVertex;
    protected transient Point oldPoint;
    protected transient Point [] tempVertex;
    protected transient boolean v2Flag;
    protected transient int labelx;
    protected transient int labely;
    protected transient int labelWidth;
    protected transient int labelHeight;
    protected transient String labelText;

    /**
     * Called by concrete subclass during construction
     */
    public mp2PolyVertexShape() {
        super();
        vertices = new Vector();
    }

    /**
     * Add the next vertex during creation
     */
    public void addNextVertex(Point p) {
        vertices.addElement(view.viewToModel(p));
        if (vertices.size() == 1) {
            firstVertex = new Point(p.x, p.y);
        }
        lastVertex = new Point(p.x, p.y);
        oldPoint = p;
        // If the vertex count is 2, set the v2Flag to true to
        // handle special case.
        if (vertices.size() == 2) {
            v2Flag = true;
        }
    }

    /**
     * Computes the rectangle that bounds this polygon
     */
    protected void computeBounds() {
        double [] v = (double []) vertices.elementAt(0);
        bounds.x = v[0];
        bounds.y = v[1];
        bounds.width = 0;
        bounds.height = 0;

        for (int i=1; i<vertices.size(); i++) {
            v = (double []) vertices.elementAt(i);
            if (v[0] < bounds.x) {
                bounds.width += bounds.x - v[0];
                bounds.x = v[0];
            }
            else if (v[0] > bounds.x + bounds.width) {
                bounds.width = v[0] - bounds.x;
            }
            if (v[1] < bounds.y) {
                bounds.height += bounds.y - v[1];
                bounds.y = v[1];
            }
            else if (v[1] > bounds.y + bounds.height) {
                bounds.height = v[1] - bounds.y;
            }
        }
    }

    public void deleteVertexAt(int i) {
        if (i>=0 && i<vertices.size() && vertices.size() > 2) {
            vertices.removeElementAt(i);
        }
    }


    /**
     * Draw the handles of this shape.
     */
    public void drawHandles(Graphics g) {
        for (int i=0; i<vertices.size(); i++) {
            Point p = view.modelToView((double []) vertices.elementAt(i));
            g.fillRect(p.x - HALF_HANDLE_SIZE, p.y - HALF_HANDLE_SIZE,
                HANDLE_SIZE, HANDLE_SIZE);
        }
    }

    /**
     * Draw open handles of this shape.
     */
    public void drawOpenHandles(Graphics g) {
        for (int i=0; i<vertices.size(); i++) {
            Point p = view.modelToView((double []) vertices.elementAt(i));
            g.setColor(Color.black);
            g.drawRect(p.x - HALF_HANDLE_SIZE, p.y - HALF_HANDLE_SIZE,
                HANDLE_SIZE-1, HANDLE_SIZE-1);
            g.setColor(Color.white);
            g.fillRect(p.x + 1 - HALF_HANDLE_SIZE, p.y + 1 - HALF_HANDLE_SIZE,
                HANDLE_SIZE-2, HANDLE_SIZE-2);
        }
    }

    /**
     * Draws the outline of this shape
     */
    protected abstract void drawOutline(Graphics g, Point [] p);

    /**
     * Draws the specified segment to the specified thickness.
     *
     * @param  i  Index of the specified segment. Segment i is
     *            the line between vertex i and vertex i+1.
     * @param  thickness  Thickness of the segment, in pixels.
     * @param  g  Graphics object for drawing
     *
     */
    public void drawThickSegment(int i, int thickness, Graphics g) {

        double [] u = (double []) vertices.elementAt(i);
        double [] v = (double []) vertices.elementAt((i+1)%vertices.size());
        Point pu = view.modelToView(u);
        Point pv = view.modelToView(v);

        double dist = mp2Math.distanceBetweenTwoPoints(pu, pv);
        int nx = (int) Math.round((thickness/2.0)*(pu.y - pv.y)/dist);
        int ny = (int) Math.round((thickness/2.0)*(pv.x - pu.x)/dist);

        int [] xPoints = new int [4];
        int [] yPoints = new int [4];
        xPoints[0] = pu.x + nx;
        xPoints[1] = pv.x + nx;
        xPoints[2] = pv.x - nx;
        xPoints[3] = pu.x - nx;
        yPoints[0] = pu.y + ny;
        yPoints[1] = pv.y + ny;
        yPoints[2] = pv.y - ny;
        yPoints[3] = pu.y - ny;

        g.fillPolygon(xPoints, yPoints, 4);
    }

    public void drawValue(Graphics g) {
        if (vertices.size() < 2) {
            return;
        }
        labelText = String.valueOf(value);
        FontMetrics fontMetrics = view.getFontMetrics(view.getFont());
        labelWidth = fontMetrics.stringWidth(labelText) + 3;
        labelHeight = fontMetrics.getAscent() + 3;
        Point a = view.modelToView((double []) vertices.elementAt(0));
        Point b = view.modelToView((double []) vertices.elementAt(1));
        labelx = (a.x + b.x)/2 - labelWidth/2;
        labely = (a.y + b.y)/2 - labelHeight/2;
        g.setPaintMode();
        g.setColor(Color.white);
        g.fillRect(labelx, labely, labelWidth, labelHeight);
        g.setColor(foreground);
        g.drawRect(labelx, labely, labelWidth-1, labelHeight-1);
        g.drawString(labelText, labelx + 1, labely + labelHeight-2);
    }

    /**
     * Ends the creation of this shape
     */
    public void endCreation() {
        g.dispose();
        g = null;
        computeBounds();
        // shape has now been created.
        isBeingCreated = false;
    }

    /**
     * Ends the move process for this shape
     */
    public void endMove() {
        if (hasChangedSinceSelected) {
            for (int i=0; i<tempVertex.length; i++) {
                vertices.setElementAt(
                    view.viewToModel(tempVertex[i]), i);
            }
            computeBounds();
        }
        disposeGraphics();
    }

    /**
     * Ends the reshape process for this shape
     */
    public void endReshape() {
        if (capturedVertex == null) {
            return;
        }
        vertices.setElementAt(view.viewToModel(capturedVertex), 
                          capturedIndex);
        computeBounds();
        disposeGraphics();
    }
    
    /**
     * Ends the reshape process by setting the captured vertex to the
     * specified coordinates
     */
    public void endReshape(double [] v) {
        if (v != null) {
            vertices.setElementAt(v, capturedIndex);
            computeBounds();
        }
        disposeGraphics();
    }

    /**
     * Find a handle to capture in the vicinity of the specified point
     */
    public void findHandleToCapture(Point p) {
        capturedIndex = -1;
        capturedVertex = null;
        for (int i=0; i<vertices.size(); i++) {
            Point q = view.modelToView((double []) vertices.elementAt(i));
            if (mp2Math.withinNeighborhood(p, q, HANDLE_SIZE)) {
                capturedIndex = i;
                capturedVertex = q;
                view.setCursor(Cursor.getPredefinedCursor(
                                        Cursor.CROSSHAIR_CURSOR));
                break;
            }
        }
    }

    public int getCapturedIndex() {
        return capturedIndex;
    }

    public double [] getCapturedVertex() {
        return getVertex(capturedIndex);
    }

    public abstract Object []  getNearestSegment(double x, double y);

    /**
     * Gets the vertex after the specified vertex
     */
    protected abstract int getNextVertexIndex(int vertexIndex);

    /**
     * Gets the number of vertices in this polygon.
     */
    public int getNumberOfVertices() {
        return vertices.size();
    }

    /**
     * Gets the vertex before the sprecified vertex
     */
    protected abstract int getPriorVertexIndex(int vertexIndex);

    /**
     * Gets the line segment (line joining two adjacent vertices) that
     * lies under the specified point to within the specified tolerance.
     */
    public abstract int getSegmentUnderPoint(double [] v, int tolerance);

    /**
     * Gets the type of this figure.
     */
    public int getType() {
        return POLY_VERTEX_SHAPE;
    }

    /**
     * Gets the vertice at the specified index
     */
    public double [] getVertex(int i) {
        if (i>=0 && i<vertices.size()) {
            return (double []) vertices.elementAt(i);
        } else {
            return null;
        }
    }

    public Vector getVertices() {
        return vertices;
    }

    public void setVertices(Vector vertices) {
        this.vertices = vertices;
    }

    /**
     * Indicates whether or not a handle of this shape has been
     * captured.
     */
    public boolean hasCapturedHandle() {
        return (capturedVertex != null);
    }

    /**
     * Initializes the polyline. This method initializes the
     * transient variables of this object.
     *
     * @param  view  The view in which this figure is displayed
     */
    public void init(mp2View view) {
        super.init(view);
        oldPoint = null;
        capturedVertex = null;
        priorVertex = null;
        nextVertex = null;
        capturedIndex = -1;
        v2Flag = false;
    }

    /**
     * Insert a vertex on the specified segment. This splits the
     * segment into two pieces.
     */
    public void insertVertexOnSegment(double [] v, int segment) {
        // Make sure the added vertex is exactly in line
        double [] a = (double []) vertices.elementAt(segment);
        double [] b = (double []) vertices.elementAt((segment + 1)%vertices.size());
        double [] test = mp2Math.projectPointToLine(v, a, b);
        double [] w = new double[2];
        w[0] = a[0] + test[1] * (b[0] - a[0]);
        w[1] = a[1] + test[1] * (b[1] - a[1]);
        // Insert the vertex
        if (segment+1 < vertices.size()) {
            vertices.insertElementAt(w, segment+1);
        } else {
            vertices.addElement(w);
        }
    }

    /** 
     * Indicates whether or not the boundary of this shape intersects itself
     */
    public abstract boolean intersectsSelf();

    /**
     * Move the shape
     */
    public void move(Point p) {
        // Erase the previous shape by view over it,
        // except first time around.
        if (hasChangedSinceSelected) {
            drawOutline(g, tempVertex);
        } else {
            hasChangedSinceSelected = true;
        }
        // Translate the outline
        int dx = p.x - oldPoint.x;
        int dy = p.y - oldPoint.y;
        for (int i=0; i<tempVertex.length; i++) {
            tempVertex[i].x += dx;
            tempVertex[i].y += dy;
        }
        // Draw the new outline
        drawOutline(g, tempVertex);
        // Save the curson position
        oldPoint = p;
        return;
    }

    /**
     * Prepares to create by setting up Graphics
     */
    public void prepareToCreate(Point p) {
        g = view.getGraphics();
        g.setColor(foreground);
        g.setXORMode(background);
        Rectangle viewRect = view.getVisibleRect();
        g.clipRect(viewRect.x, viewRect.y, 
                   viewRect.width, viewRect.height);
    }

    /**
     * Prepares to move by setting up Graphics and creating
     * the array of temporary vertices
     */
    public void prepareToMove(Point p) {
        // Create temporary vertices
        tempVertex = new Point[vertices.size()];
        for (int i=0; i<vertices.size(); i++) {
            tempVertex[i] = view.modelToView(
                        (double []) vertices.elementAt(i));
        }
        // Set up to draw in XOR mode
        g = view.getGraphics();
        g.setColor(Color.black);
        g.setXORMode(Color.white);
        Rectangle viewRect = view.getVisibleRect();
        g.clipRect(viewRect.x, viewRect.y, viewRect.width, 
                                           viewRect.height);
        oldPoint = p;
        hasChangedSinceSelected = false;
    }

    /**
     * Prepares to reshape by determining the prior and next
     * vertices, and setting up Graphics
     */
    public void prepareToReshape() {
        // Determine the prior vertex
        int priorIndex = getPriorVertexIndex(capturedIndex);
        if (priorIndex >= 0) {
        priorVertex = view.modelToView(
            (double []) vertices.elementAt(priorIndex));
        } else {
            priorVertex = null;
        }
        // Determine the next vertex
        int nextIndex = getNextVertexIndex(capturedIndex);
        if (nextIndex >= 0) {
        nextVertex = view.modelToView(
            (double []) vertices.elementAt(nextIndex));
        } else {
            nextVertex = null;
        }
        // Set up to draw in XOR mode
        Rectangle viewRect = view.getVisibleRect();
        g = view.getGraphics();
        g.clipRect(viewRect.x, viewRect.y, viewRect.width, viewRect.height);
        g.setColor(Color.black);
        g.setXORMode(Color.white);
        hasChangedSinceSelected = false;
    }

    /** 
     * Reshape by moving the captured vertex to the specified point
     */
    public void reshape(Point p) {
        // Erase old vertex handle and connecting line(s) by drawing
        // over them. Skip this first time around.
        if (hasChangedSinceSelected || !isFilled) {
            g.fillRect(capturedVertex.x - HALF_HANDLE_SIZE, 
                       capturedVertex.y - HALF_HANDLE_SIZE, 
                       HANDLE_SIZE, HANDLE_SIZE);
            if (priorVertex != null) {
                g.drawLine(capturedVertex.x, capturedVertex.y, 
                           priorVertex.x, priorVertex.y);
            }
            if (nextVertex != null) {
                g.drawLine(capturedVertex.x, capturedVertex.y, 
                           nextVertex.x, nextVertex.y);
            }
        }
        // Move the captured vertex to the mouse position
        capturedVertex = p;
        // Draw the new vertex and connecting lines
        g.fillRect(capturedVertex.x - HALF_HANDLE_SIZE, 
                   capturedVertex.y - HALF_HANDLE_SIZE, 
                   HANDLE_SIZE, HANDLE_SIZE);
        if (priorVertex != null) {
            g.drawLine(capturedVertex.x, capturedVertex.y, 
                       priorVertex.x, priorVertex.y);
        }
        if (nextVertex != null) {
            g.drawLine(capturedVertex.x, capturedVertex.y, 
                       nextVertex.x, nextVertex.y);
        }
        if (labelText != null) {
            g.setPaintMode();
            g.setColor(Color.white);
            g.fillRect(labelx, labely, labelWidth, labelHeight);
            g.setColor(foreground);
            g.drawRect(labelx, labely, labelWidth-1, labelHeight-1);
            g.drawString(labelText, labelx + 1, labely + labelHeight-2);
            g.setXORMode(Color.white);
        }
        hasChangedSinceSelected = true;
    }

    public abstract mp2PolyVertexShape makeShallowCopy();

    public void setVertexAt(double [] v, int i) {
        vertices.setElementAt(v, i);
        computeBounds();
    }

    public void translate(double dx, double dy) {
        for (int i=0; i<vertices.size(); i++) {
            double [] v = (double []) vertices.elementAt(i);
            v[0] += dx;
            v[1] += dy;
        }
    }
        
}