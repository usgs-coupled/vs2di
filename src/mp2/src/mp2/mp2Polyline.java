/*
 * mp2Polyline.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Vector;

/**
 * TO BE REVISED. THIS VERSION NEEDS BETTER ORGANIZATION.
 * Encapsulates a polyline. A polyline is described by the 
 * coordinates of its vertices. A polyline is cannot be
 * drawn in the "fill" mode.
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
public class mp2Polyline extends mp2PolyVertexShape implements 
        mp2Constants, Serializable {

    static final long serialVersionUID = 7259756225067858452L;

    /**
     * Determines whether or not the specified point is contained
     * by this polyline. At present, this method returns false.
     * To be modified.
     */
    public boolean contains(double x, double y) {
        return false;
    }

    /**
     * Draws the polyline.
     *
     * @param  g  The Graphics object for view.
     */
    public void draw(Graphics g) {
        g.setColor(foreground);
        if (isSelected) {
            g.setXORMode(background);
        }
        Point p1;
        Point p2;
        p1 = view.modelToView((double []) vertices.elementAt(0));
        for (int i=1; i<vertices.size(); i++) {
            p2 = view.modelToView((double[]) vertices.elementAt(i));
            g.drawLine(p1.x, p1.y, p2.x, p2.y);
            p1 = p2;
        }
        labelText = null;
    }

    /**
     * Draws an outline as specified by the point array in the argument
     */
    protected void drawOutline(Graphics g, Point [] p) {
        Point p1, p2;
        for (int i=0; i<p.length-1; i++) {
            p1 = tempVertex[i];
            p2 = tempVertex[(i+1)];
            g.drawLine(p1.x, p1.y, p2.x, p2.y);
        }
    }
    public int getMinimumNumberOfVertices() {
        return 2;
    }

    /**
     * Gets the segment nearest the specified point
     */
    public Object [] getNearestSegment(double x, double y) {
        double [] v = new double [2];
        v[0] = x;
        v[1] = y;
        double [] a = (double []) vertices.elementAt(0);
        double [] b = (double []) vertices.elementAt(1);
        double [] test = mp2Math.projectPointToLine(v, a, b);
        int nearestSegment = 0;
        double distance, nearestDistance;
        if (test[1] < 0) {
            nearestDistance = mp2Math.distanceBetweenTwoPoints(v, a);
        } else if (test[1] > 1) {
            nearestDistance = mp2Math.distanceBetweenTwoPoints(v, b);
        } else {
            nearestDistance = test[0];
        }
        a = b;
        for (int i=1; i<vertices.size()-1; i++) {
            b = (double []) vertices.elementAt(i+1);
            test = mp2Math.projectPointToLine(v, a, b);
            if (test[1] < 0) {
                distance = mp2Math.distanceBetweenTwoPoints(v, a);
            } else if (test[1] > 1) {
                distance = mp2Math.distanceBetweenTwoPoints(v, b);
            } else {
                distance = test[0];
            }
            if (distance < nearestDistance) {
                nearestDistance = distance;
                nearestSegment = i;
            }
            a = b;
        }
        Object [] result = new Object [2];
        result[0] = new Integer(nearestSegment);
        result[1] = new Double(nearestDistance);
        return result;       
    }

    /**
     * Gets the index of the vertex after the specified vertex
     */
    protected int getNextVertexIndex(int vertexIndex) {
        int nextVertexIndex = vertexIndex + 1;
        if (nextVertexIndex >= vertices.size()) {
            nextVertexIndex = -1;
        }
        return nextVertexIndex;
    }

    /**
     * Gets the index of the vertex prior to specified vertex
     */
    protected int getPriorVertexIndex(int vertexIndex) {
        return (vertexIndex - 1);
    }

    /**
     * Gets the segment under the specified point, with the
     * specified tolerance
     */
    public int getSegmentUnderPoint(double [] v, int tolerance) {
        double [] test;
        Point a = view.modelToView((double []) vertices.elementAt(0));
        Point b;
        Point c = new Point();
        Point pv = view.modelToView(v);
        for (int i=0; i<vertices.size()-1; i++) {
            // a and b are the vertices (converted to pixel coordinates) 
            // of the line segment to be tested
            b = view.modelToView((double []) vertices.elementAt((i+1)%vertices.size()));
            // project the point to line. test[1] contains the parametric value
            // that indicates the location of the projected point on the line.
            test = mp2Math.projectPointToLine(pv.x, pv.y, a.x, a.y, b.x, b.y);
            // Consider only when test[1] is between 0 and 1, which means
            // the intersection point is within the line
            if (test[1] >= 0 && test[1] <= 1) {
                // Find c, the pointof intersection on the line
                c.x = a.x + (int) Math.round(test[1] * (b.x - a.x));
                c.y = a.y + (int) Math.round(test[1] * (b.y - a.y));
                if (mp2Math.distanceBetweenTwoPoints(pv, c) <= tolerance) {
                    return i;
                }
            }
            a = b;
        }
        return -1;
    }

    /**
     * Indicates whether or not this polyline intersects another 
     * polygon or polyline.
     * Uses the algorithm for finding intersection between two lines
     * as given in comp.graphics.algorithms FAQ 1.03
     */
    public boolean intersects(mp2PolyVertexShape otherPoly) {
        double [] a, b, c, d;
        double rn, sn, den, r, s;
        a = (double []) vertices.elementAt(0);
        for (int i=0; i<vertices.size()-1; i++) {
            b = (double []) vertices.elementAt(i+1);
            c = otherPoly.getVertex(0);
            int numSegment = otherPoly.getNumberOfVertices();
            if (otherPoly instanceof mp2Polyline) {
                numSegment--;
            }
            for (int j=0; j<numSegment; j++) {
                d = otherPoly.getVertex((j+1) % otherPoly.getNumberOfVertices());
                rn = (a[1]-c[1])*(d[0]-c[0])-(a[0]-c[0])*(d[1]-c[1]);
                sn = (a[1]-c[1])*(b[0]-a[0])-(a[0]-c[0])*(b[1]-a[1]);
                den = (b[0]-a[0])*(d[1]-c[1])-(b[1]-a[1])*(d[0]-c[0]);
                if (den != 0) {
                    r = rn/den;
                    s = sn/den;
                    if (r >= 0 && r <= 1 && s >= 0 && s <= 1) {
                        return true;
                    }
                }
                c = d;
            }
            a = b;
        }
        return false;
    }

    /**
     * Indicates whether or not the boundary of this polyline intersects
     * itself. Uses the algorithm for finding intersection between two lines
     * as given in comp.graphics.algorithms FAQ 1.03
     */
    public boolean intersectsSelf() {
        double [] a, b, c, d;
        double rn, sn, den, r, s;
        a = (double []) vertices.elementAt(0);
        for (int i=0; i<vertices.size()-3; i++) {
            b = (double []) vertices.elementAt(i+1);
            c = (double []) vertices.elementAt(i+2);
            for (int j=i+3; j<vertices.size(); j++) {
                d = (double []) vertices.elementAt(j);
                rn = (a[1]-c[1])*(d[0]-c[0])-(a[0]-c[0])*(d[1]-c[1]);
                sn = (a[1]-c[1])*(b[0]-a[0])-(a[0]-c[0])*(b[1]-a[1]);
                den = (b[0]-a[0])*(d[1]-c[1])-(b[1]-a[1])*(d[0]-c[0]);
                if (den != 0) {
                    r = rn/den;
                    s = sn/den;
                    if (r >= 0 && r <= 1 && s >= 0 && s <= 1) {
                        return true;
                    }
                }
                c = d;
            }
            a = b;
        }
        return false;
    }

    /**
     * Indicates whether or not this figure is filled. Always 
     * returns false because is polyline is never filled.
     */
    public boolean isFilled() {
        return false;
    }

    /**
     * Indicates whether or not this shape is "under" the 
     * mouse at the specified point
     */
    public boolean isUnderMouse(Point p) {
        // Return true if there is a segment within tolerance
        // distance (5 pixels) of the specified point.
        return (getSegmentUnderPoint(view.viewToModel(p), 5) > -1);
    }

    /**
     * Invoked during mouse moved when this polyline is being created
     */
    public void onMouseMovedDuringCreation(Point p) {

        // Erase the line from old point to last vertex by 
        // drawing over it.
        g.drawLine(lastVertex.x, lastVertex.y, oldPoint.x, 
                   oldPoint.y);
        
        // Draw lines from current point to the last vertex
        g.drawLine(p.x, p.y, lastVertex.x, lastVertex.y);
        oldPoint = p;
    }

    public mp2PolyVertexShape makeShallowCopy() {
        mp2PolyVertexShape copy = new mp2Polyline();
        copy.vertices = (Vector) this.vertices.clone();
        copy.view = this.view;
        copy.capturedIndex = this.capturedIndex;
        copy.capturedVertex = this.capturedVertex;
        copy.tempVertex = this.tempVertex;
        copy.hasChangedSinceSelected = this.hasChangedSinceSelected;
        copy.isSelected = this.isSelected;
        return copy;
    }

}
