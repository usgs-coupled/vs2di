/*
 * mp2Math.java
 */
package mp2;

import java.awt.*;

/**
 * Provides static functions for mathematical calculations
 */
public class mp2Math {

    // Load shared library containing triangulation function
    static {
        try {
            System.loadLibrary("trimesh"); 
        } catch (UnsatisfiedLinkError e) {
            // Should probably post a dialog box here
        }
    }

    public static native void changeDirectory(String directory);

    /**
     * Performs triangulation
     */
    public static native void trimesh(int quality,
            double [] inputPointList, double [] inputPointAttributeList,
            int [] segmentList, Object [] returnObjectList);

    /**
     * Compute distance (in pixels) between 2 points on the screen
     *
     * @return the distance (in pixels) between the two points
     *
     * @param   a   Location of the first point.
     * @param   b   Location of the second point.
     */
    public static int distanceBetweenTwoPoints(Point a, Point b) {
        return (int) Math.round(Math.sqrt((b.x - a.x)*(b.x - a.x) + 
            (b.y - a.y)*(b.y - a.y)));
    }

    /**
     * Compute distance (in pixels) between 2 points in the model domain
     */
    public static double distanceBetweenTwoPoints(double [] a, double [] b) {
        return Math.sqrt((b[0] - a[0]) * (b[0] - a[0]) +
                         (b[1] - a[1]) * (b[1] - a[1])); 
    }

    /**
     * Determines whether one point is in a neighborhood of the other.
     * "Neighborhood" is defind by a square of a specified size centered
     * about a point.
     *
     * @return <code>true</code> if one point is in neighborhool bo the
     *         the other, <code>false</code> otherwise.
     *
     * @param   a   Location of the first point.
     * @param   b   Location of the second point.
     * @param   size  Size of the neighborhood.
     */
    public static boolean withinNeighborhood(Point a, Point b,
            int size) {
        return ((Math.abs(a.x - b.x) <= size/2) && 
                (Math.abs(a.y - b.y) <= size/2));
    }

    /**
     * Projects a point to a line.
     * 
     * @return  a 2-element double-typed array. The first element
     *          contains the distance from the point to the line.
     *          The second element contains the parametric value
     *          that indicates the location of the projected point
     *          on the line. Given the line AB, if the parametric
     *          value is zero, the projected point is at A.
     *          If the parametric value is 1, the projected point is
     *          at B. If the parametric value is between 0 and 1,
     *          the projected point is between A and B. If the
     *          parametric value is negative, the projected point is
     *          on the backward extension of AB. If the parametric
     *          value is greater than 1, the projected point in on the
     *          forward extension of AB.
     *
     * @param   p   a two-element double-typed array giving the
     *              coordinates of the point to be projected.
     * @param   a   a two-element double-typed array giving the
     *              coordinates of one endpoint of the line.
     * @param   b   a two-element double-typed array giving the
     *              coordinates of the other endpoint of the line
     */
    public static double [] projectPointToLine(double [] p,
                                      double [] a, double [] b) {
        double L = Math.sqrt((b[0] - a[0]) * (b[0] - a[0]) +
                             (b[1] - a[1]) * (b[1] - a[1]));
        double [] ret = new double[2];
        ret[0] = Math.abs(((a[1] - p[1]) * (b[0] - a[0]) - 
                  (a[0] - p[0]) * (b[1] - a[1])) / L);

        ret[1] = ((a[1] - p[1]) * (a[1] - b[1]) - 
                  (a[0] - p[0]) * (b[0] - a[0]))/L/L;
        return ret;
    }

    /**
     * Projects a point to a line. Overloaded version
     */
    public static double [] projectPointToLine(double px, double py,
                 double ax, double ay, double bx, double by) {
        double L = Math.sqrt((bx - ax) * (bx - ax) +
                             (by - ay) * (by - ay));
        double [] ret = new double[2];
        ret[0] = Math.abs(((ay - py) * (bx - ax) - 
                           (ax - px) * (by - ay)) / L);

        ret[1] = ((ay - py) * (ay - by) - 
                  (ax - px) * (bx - ax))/L/L;
        return ret;
    }


} 