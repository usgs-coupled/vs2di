/*
 * mp2Polygon.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Vector;

/**
 * Encapsulates a polygon. A polygon is described by the
 * coordinates of its vertices.
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
public class mp2Polygon extends mp2PolyVertexShape implements
        mp2Constants, Serializable {

    static final long serialVersionUID = -6888067773181878006L;

    /**
     * Determines whether or not the specified point is contained
     * by this polygon
     *
     * @param  x  the <i>x</i> coordinate of the point to be tested
     * @parem  y  the <i>y</i> coordinate of the point to be tested
     *
     * @return <code>true</code> if the point (<i>x</i>, <i>y</i>) is
     *         contained by this polygon; <code>false</code> otherwise.
     */
    public boolean contains(double x, double y) {
        // This algorithm comes from comp.graphics.algorithms FAQ 2.03
        int i, j;
        int npol = vertices.size();
        boolean c = false;
        for (i = 0, j = npol-1; i < npol; j = i++) {
            double [] pi = (double []) vertices.elementAt(i);
            double [] pj = (double []) vertices.elementAt(j);
            if (    (((pi[1]<=y) && (y<pj[1])) ||
                    ((pj[1]<=y) && (y<pi[1]))) &&
                    (x < (pj[0] - pi[0]) * (y - pi[1]) /
                             (pj[1] - pi[1]) + pi[0])) {
                c = !c;
            }
        }
        return c;
    }

    /**
     * Draws the polygon.
     *
     * @param  g  The Graphics object for view.
     */
    public void draw(Graphics g) {
        if (isFilled) {
            int nPoints = vertices.size();
            int [] x = new int[nPoints];
            int [] y = new int[nPoints];
            Point p;

            for (int i=0; i<nPoints; i++) {
                p = view.modelToView((double []) vertices.elementAt(i));
                x[i] = p.x;
                y[i] = p.y;
            }
            g.setColor(foreground);
            if (fillPattern == FILL_SOLID) {
                g.fillPolygon(x, y, nPoints);
                if (view.isPrinting()) {
                    g.drawPolygon(x, y, nPoints);
                }
            } else if (fillPattern == FILL_DOTTED) {
                g.drawPolygon(x, y, nPoints);
                Polygon poly = new Polygon(x, y, nPoints);
                Rectangle polyBounds = poly.getBounds();
                int offset = 0;
                for (int j=polyBounds.y; j<polyBounds.y+polyBounds.height; j+=4) {
                    for (int i=polyBounds.x+offset; i<polyBounds.x+polyBounds.width; i+=4) {
                        if (poly.contains(i, j)) {
                            g.drawRect(i, j, 0, 0);
                        }
                    }
                    offset = (offset+2)%4;
                }
            }
        }
        else {
            g.setColor(foreground);
            if (isSelected) {
                g.setXORMode(background);
            }
            Point p1;
            Point p2;
            p1 = view.modelToView((double []) vertices.elementAt(0));
            for (int i=1; i<=vertices.size(); i++) {
                p2 = view.modelToView(
                    (double[]) vertices.elementAt(i%vertices.size()));
                g.drawLine(p1.x, p1.y, p2.x, p2.y);
                p1 = p2;
            }
        }
        labelText = null;
    }

    /**
     * Draws the outline of a polygon as specified by the point array p
     */
    protected void drawOutline(Graphics g, Point [] p) {
        Point p1, p2;
        for (int i=0; i<p.length; i++) {
            p1 = p[i];
            p2 = p[(i+1)%p.length];
            g.drawLine(p1.x, p1.y, p2.x, p2.y);
        }
    }

    public int getMinimumNumberOfVertices() {
        return 3;
    }

    /**
     * Gets the labeling position for the specified segment with the
     * specified offset.
     */
    public Point getSegmentLabelPosition(int i, int offset) {
        if (i>=0 && i<vertices.size()) {
            double [] midpoint = new double[2];
            double [] a = (double []) vertices.elementAt(i);
            double [] b = (double []) vertices.elementAt((i+1) % vertices.size());
            double d = mp2Math.distanceBetweenTwoPoints(a, b);
            int offsetX = (int) Math.round(offset * (b[1] - a[1]) / d);
            int offsetY = (int) Math.round(offset * (a[0] - b[0]) / d);
            if (offsetX < 0) {
                offsetX *= -1;
                offsetY *= -1;
            }
            if (offsetY > 0) {
                offsetY *= 2;
            }
            midpoint[0] = (a[0] + b[0])/2;
            midpoint[1] = (a[1] + b[1])/2;
            Point mp = view.modelToView(midpoint);
            mp.x += offsetX;
            mp.y += offsetY;
            return mp;
        } else {
            return null;
        }
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
        for (int i=1; i<vertices.size(); i++) {
            b = (double []) vertices.elementAt((i+1)%vertices.size());
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

    public Object [] getNearestSegmentInXDirection(double x, double y) {
        int nearestSegment = -1;
        double distance = -1;
        double nearestDistance = -1;
        double [] a = (double []) vertices.elementAt(0);
        double [] b;
        double c;
        for (int i=0; i<vertices.size(); i++) {
            b = (double []) vertices.elementAt((i+1)%vertices.size());
            if (b[1] != a[1]) {
                c = (y - a[1]) / (b[1] - a[1]);
                if (c >= 0 && c <= 1) {
                    distance = Math.abs(x - (a[0] + c * (b[0] - a[0])));
                    if (nearestSegment == -1 || distance < nearestDistance) {
                        nearestSegment = i;
                        nearestDistance = distance;
                    }
                }
            }
            a = b;
        }
        if (nearestSegment == -1) {
            return null;
        } else {
            Object [] result = new Object [2];
            result[0] = new Integer(nearestSegment);
            result[1] = new Double(nearestDistance);
            return result;
        }
    }

    public Object [] getNearestSegmentInYDirection(double x, double y) {
        int nearestSegment = -1;
        double distance = -1;
        double nearestDistance = -1;
        double [] a = (double []) vertices.elementAt(0);
        double [] b;
        double c;
        for (int i=0; i<vertices.size(); i++) {
            b = (double []) vertices.elementAt((i+1)%vertices.size());
            if (b[0] != a[0]) {
                c = (x - a[0]) / (b[0] - a[0]);
                if (c >= 0 && c <= 1) {
                    distance = Math.abs(y - (a[1] + c * (b[1] - a[1])));
                    if (nearestSegment == -1 || distance < nearestDistance) {
                        nearestSegment = i;
                        nearestDistance = distance;
                    }
                }
            }
            a = b;
        }
        if (nearestSegment == -1) {
            return null;
        } else {
            Object [] result = new Object [2];
            result[0] = new Integer(nearestSegment);
            result[1] = new Double(nearestDistance);
            return result;
        }
    }

    /**
     * Gets the index of the vertex after the specified index.
     */
    protected int getNextVertexIndex(int vertexIndex) {
        return ((vertexIndex + 1) % vertices.size());
    }

    /**
     * Gets the index of the vertex prior to the specified index.
     */
    protected int getPriorVertexIndex(int vertexIndex) {
        return ((vertexIndex + vertices.size() - 1) % vertices.size());
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
        for (int i=0; i<vertices.size(); i++) {
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
     * Indicates whether or not this polygon intersects another
     * polygon or polyline.
     * Uses the algorithm for finding intersection between two lines
     * as given in comp.graphics.algorithms FAQ 1.03
     */
    public boolean intersects(mp2PolyVertexShape otherPoly) {
        double [] a, b, c, d;
        double rn, sn, den, r, s;
        a = (double []) vertices.elementAt(0);
        for (int i=0; i<vertices.size(); i++) {
            b = (double []) vertices.elementAt((i+1) % vertices.size());
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
     * Indicates whether or not the boundary of this polygon intersects
     * itself. Uses the algorithm for finding intersection between two lines
     * as given in comp.graphics.algorithms FAQ 1.03
     */
    public boolean intersectsSelf() {
        double [] a, b, c, d;
        double rn, sn, den, r, s;
        a = (double []) vertices.elementAt(0);
        for (int i=0; i<vertices.size(); i++) {
            b = (double []) vertices.elementAt((i+1) % vertices.size());
            c = (double []) vertices.elementAt((i+2) % vertices.size());
            for (int j=i+2; j<=i+vertices.size()-2; j++) {
                d = (double []) vertices.elementAt((j+1) % vertices.size());
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
     * Indicates whether or not this shape is "under" the
     * mouse at the specified point
     */
    public boolean isUnderMouse(Point p) {
        if (isFilled) {
            // If filled, return true if the point is contained by
            // the polygon
            return this.contains(view.viewToModel(p));
        } else {
            // If not filled, return true if there is a segment
            // within tolerance distance (5 pixels) of the specified
            // point.
            return (getSegmentUnderPoint(view.viewToModel(p), 5) > -1);
        }
    }

    /**
     * Invoked during mouse moved when this polygon is being created
     */
    public void onMouseMovedDuringCreation(Point p) {

        if (vertices.size() == 0) {
            return;
        }

/* temporary fix to get around problem of lines not appearing the first time a polygon is drawn */
		Graphics g = view.getGraphics();
		g.setColor(foreground);
		g.setXORMode(background);
		Rectangle viewRect = view.getVisibleRect();
		g.clipRect(viewRect.x, viewRect.y,viewRect.width, viewRect.height);
/* end of temporary fix */

        if (vertices.size() == 1) {
            // Erase the line from old point to first vertex by
            // drawing over it.
            g.drawLine(oldPoint.x, oldPoint.y, firstVertex.x,
                       firstVertex.y);
            // Draw line from current point to first vertex
            g.drawLine(p.x, p.y, firstVertex.x, firstVertex.y);
        }
        else {
            // Erase the line from the old point to the first vertex
            // by drawing over it. However, we want to skip this step
            // if the 2nd vertex has just been drawn (v2Flag is true).
            if (v2Flag == true) {
                v2Flag = false;
            }
            else {
                g.drawLine(oldPoint.x, oldPoint.y, firstVertex.x,
                           firstVertex.y);
            }
            // Erase the line from old point to last vertex by
            // drawing over it.
            g.drawLine(lastVertex.x, lastVertex.y, oldPoint.x,
                       oldPoint.y);
            // Draw lines from current point to first and last vertex
            g.drawLine(p.x, p.y, firstVertex.x, firstVertex.y);
            g.drawLine(p.x, p.y, lastVertex.x, lastVertex.y);
        }
        oldPoint = p;
    }

    public mp2PolyVertexShape makeShallowCopy() {
        mp2PolyVertexShape copy = new mp2Polygon();
        copy.vertices = (Vector) this.vertices.clone();
        copy.view = this.view;
        copy.capturedIndex = this.capturedIndex;
        copy.capturedVertex = this.capturedVertex;
        copy.tempVertex = this.tempVertex;
        copy.hasChangedSinceSelected = this.hasChangedSinceSelected;
        copy.isSelected = this.isSelected;
        return copy;
    }

    public mp2Polygon makeDeepCopy() {
        mp2Polygon copy = new mp2Polygon();
        copy.init(this.view);
        copy.foreground = this.foreground;
        copy.background = this.background;
        copy.id = this.id;
        copy.hasValue = this.hasValue;
        copy.value = this.value;
        copy.isFilled = this.isFilled;
        copy.fillPattern = this.fillPattern;
        copy.isBeingCreated = this.isBeingCreated;
        copy.capturedIndex = this.capturedIndex;
        copy.capturedVertex = this.capturedVertex;
        copy.tempVertex = this.tempVertex;
        copy.hasChangedSinceSelected = this.hasChangedSinceSelected;
        copy.isSelected = this.isSelected;
        for (int j=0; j<this.vertices.size(); j++) {
            double [] v = (double []) this.vertices.elementAt(j);
            double [] vc = new double [2];
            vc[0] = v[0];
            vc[1] = v[1];
            copy.vertices.addElement(vc);
        }
        copy.computeBounds();
        return copy;
    }
}
