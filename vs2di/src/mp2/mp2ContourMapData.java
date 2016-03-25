/*
 * mp2ContourMapData.java
 */
package mp2;

import java.awt.*;
import java.io.*;
import java.util.Vector;

public class mp2ContourMapData extends mp2ShapesData 
        implements Serializable {

    static final long serialVersionUID = -3551557500600757832L;

    protected int interpolationScheme;
    protected mp2ColorScale colorScale;
    protected int colorPolicy;


    protected transient double [] valueArray;

    public final static int CUSTOM_COLOR = 0;
    public final static int AUTO_RED_HIGH = 1;
    public final static int AUTO_BLUE_HIGH = 2;
  
    public final static int NEAREST_CONTOUR             = 0;
    public final static int CLOSED_CONTOUR_STEP         = 1;
    public final static int INVERSE_DISTANCE            = 2;
    public final static int INVERSE_DISTANCE_QUADRANT   = 3;
    public final static int INVERSE_DISTANCE_2          = 4;
    public final static int INVERSE_DISTANCE_2_QUADRANT = 5;
    public final static int NUM_QUADRANT_SEARCH = 8;
    public final static int TRIANGULATION  = 9;

    public mp2ContourMapData() {
        super();
        interpolationScheme = TRIANGULATION;
        colorScale = new mp2ColorScale();
        colorPolicy = AUTO_RED_HIGH;
    }
    
    /**
    * Initializes this object.
    */
    public void init(mp2Doc doc) {
        super.init(doc);
        colorScale.init();
        // Nearest contour and inverse distance methods are no longer used.
        // If these methods are used, we switch to triangulation (continuous)
        // instead.
        if (interpolationScheme != CLOSED_CONTOUR_STEP &&
                interpolationScheme != TRIANGULATION) {
            interpolationScheme = TRIANGULATION;
        }
        // For backward compatibility--set all shapes to have value
        for (int i=0; i<shapes.size(); i++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(i);
            shape.setValue(shape.getValue());
        }
        dataHaveChanged = true;
    }

    public boolean discretize() {

        if (!(gridData instanceof mp2RectilinearGridData)) {
            return false;
        }

        // Don't do this if neither the grid nor the data
        // have changed since the last discretization.
        if ((!activeCellsHaveChanged) && (!dataHaveChanged)) {
            return false;
        }

        // Get the active cell array and the grid line coordinates 
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int [] active = rectGridData.getActiveCellArray();
        double [] xCoord = rectGridData.getXCoords();
        double [] yCoord = rectGridData.getYCoords();
        int numCol = xCoord.length - 1;
        int numRow = yCoord.length - 1;
        int i, j, k, index;

        // Compute the x and y coordinates of the cell centers.
        double [] xCenter = new double[numCol];
        double [] yCenter = new double[numRow];
        for (i=0; i<numCol; i++) {
            xCenter[i] = (xCoord[i] + xCoord[i+1])/2;
        }
        for (i=0; i<numRow; i++) {
            yCenter[i] = (yCoord[i] + yCoord[i+1])/2;
        }

        // Create the value array and initially set all elements 
        // to negative infinity
        valueArray = new double[numCol*numRow];
        for (i=0; i<numCol*numRow; i++) {
            valueArray[i] = Double.NEGATIVE_INFINITY;
        }

        switch (interpolationScheme) {
        case NEAREST_CONTOUR:
            double [] u = new double[2];
            for (j=0; j<numRow; j++) {
                u[1] = yCenter[j];
                for (i=0; i<numCol; i++) {
                    u[0] = xCenter[i];
                    index = j*numCol+i;
                    if (active[index] > -2) {
                        mp2PolyVertexShape shape = (mp2PolyVertexShape) shapes.elementAt(0);
                        Object [] seginfo = shape.getNearestSegment(u[0], u[1]);
                        double nearestDistance = ((Double) seginfo[1]).doubleValue();
                        mp2PolyVertexShape nearestShape = shape;

                        for (int n=1; n<shapes.size(); n++) {
                            shape = (mp2PolyVertexShape) shapes.elementAt(n);
                            seginfo = shape.getNearestSegment(u[0], u[1]);
                            double distance = ((Double) seginfo[1]).doubleValue();
                            if (distance < nearestDistance) {
                                nearestDistance = distance;
                                nearestShape = shape;
                            }
                        }
                        valueArray[index] = nearestShape.getValue();
                    }
                }
            }
            break;
        case INVERSE_DISTANCE:
        case INVERSE_DISTANCE_QUADRANT:
            for (j=0; j<numRow; j++) {
                for (i=0; i<numCol; i++) {
                    index = j*numCol+i;
                    if (active[index] > -2) {
                        valueArray[index] = interpolate(xCenter[i], yCenter[j], interpolationScheme,
                            xCoord, yCoord);
                    }
                }
            }
            break;
        case INVERSE_DISTANCE_2:
        case INVERSE_DISTANCE_2_QUADRANT:
            for (j=0; j<numRow; j++) {
                for (i=0; i<numCol; i++) {
                    index = j*numCol+i;
                    if (active[index] > -2) {
                        valueArray[index] = interpolate_2(xCenter[i], yCenter[j], interpolationScheme,
                                                          xCoord, yCoord);
                    }
                }
            }
            break;
        case CLOSED_CONTOUR_STEP:
            // first pick out only closed contours
            Vector closedContours = new Vector();
            for (i=0; i<shapes.size(); i++) {
                if (shapes.elementAt(i) instanceof mp2Polygon) {
                    closedContours.addElement(shapes.elementAt(i));
                }
            }
            if (closedContours.size() == 0) {
                break;
            }

            // order the closed contours by nesting--outer most contour first
            Vector nestedContours = new Vector();
            while (closedContours.size() > 1) {
                boolean found = false;
                for (i=0; i<closedContours.size() && !found; i++) {
                    mp2Polygon poly1 = (mp2Polygon) closedContours.elementAt(i);
                    double [] v = poly1.getVertex(0);
                    boolean isOuter = true;
                    for (j=0; j<closedContours.size() && isOuter; j++) {
                        mp2Polygon poly2 = (mp2Polygon) closedContours.elementAt(j);
                        if (i!=j && poly2.contains(v)) {
                            isOuter = false;
                        }
                    }
                    if (isOuter) {
                        nestedContours.addElement(poly1);
                        closedContours.removeElement(poly1);
                        found = true;
                    }
                }
            }
            nestedContours.addElement(closedContours.firstElement());

            // put default values in active cells
            double defaultValue = ((mp2Polygon) nestedContours.firstElement()).getValue();
            for (i=0; i<numRow*numCol; i++) {
                if (active[i] > -2) {
                    valueArray[i] = defaultValue;
                }
            }

            for (k=0; k<nestedContours.size(); k++) {
                mp2Polygon poly = (mp2Polygon) nestedContours.elementAt(k);
                for (j=0; j<numRow; j++) {
                    for (i=0; i<numCol; i++) {
                        index = j*numCol+i;
                        if (poly.contains(xCenter[i], yCenter[j]) &&
                                active[index] > -2) {
                            valueArray[index] = poly.getValue();
                        }
                    }
                }
            }
            break;
        case TRIANGULATION:
            triangulate(xCenter, yCenter, active);
            break;
        }
        if (colorPolicy == AUTO_BLUE_HIGH || colorPolicy == AUTO_RED_HIGH) {
            double minValue = Double.POSITIVE_INFINITY;
            double maxValue = Double.NEGATIVE_INFINITY;
            for (i=0; i<valueArray.length; i++) {
                if (valueArray[i] != Double.NEGATIVE_INFINITY) {
                    if (valueArray[i] > maxValue)  {
                        maxValue = valueArray[i];
                    }
                    if (valueArray[i] < minValue) {
                        minValue = valueArray[i];
                    }
                }
            }
            // need to round off the high and low values.
            if (colorPolicy == AUTO_BLUE_HIGH) {
                setValueRange(maxValue, minValue, false);
            } 
            else if (colorPolicy == AUTO_RED_HIGH) {
                setValueRange(minValue, maxValue, false);
            }
        }

        activeCellsHaveChanged = false;
        dataHaveChanged = false;
        return true;
    }

    public Vector getNearestNeighbors (Vector data, double x, double y, int num) {

        boolean          found;
        int              i, j;
        double           dx, dy;
        mp2SpatialSample sample, temp;
        Vector           matches = new Vector (num);

        num = Math.min (num, data.size ());
        if (num == 0) return (null);


        for (i=0; i<data.size (); i++) {
            sample          = (mp2SpatialSample) data.elementAt (i);
            dx              = sample.x - x;
            dy              = sample.y - y;
            sample.distance = (dx * dx) + (dy * dy);

            j     = 0;
            found = false;
            while (!found && (j < matches.size ())) {
                temp = (mp2SpatialSample) matches.elementAt (j);
                if (sample.distance < temp.distance) {
                    matches.insertElementAt (sample, j);
                    found = true;
                }

                j++;
            }
            if (!found && (j < num))
                matches.addElement (sample);
            if (matches.size () > num)
                matches.removeElementAt (num);
        }

        return (matches);
    }

    public Vector getNearestNeighborData () {

        int              i, j;
        mp2SpatialSample sample;
        Vector           data = new Vector();

        for (i=0; i<shapes.size(); i++) {
            mp2PolyVertexShape pvShape = (mp2PolyVertexShape) shapes.elementAt(i);
            double value = pvShape.getValue();
            for (j=0; j<pvShape.getNumberOfVertices(); j++) {
                double [] v = pvShape.getVertex(j);
                sample = new mp2SpatialSample ();
                sample.init(v[0], v[1], value);
                data.addElement(sample);
            }
        }

    return data;
    }

    public Vector getQuadrantNeighborData (double[] xCoord, double[] yCoord) {

    int                i, j, k;
        double             slope, yIntercept, coord;
    mp2SpatialSample[] vertex;
    mp2SpatialSample   sample;
        Vector             data = new Vector();

        for (i=0; i<shapes.size(); i++) {
            mp2PolyVertexShape pvShape = (mp2PolyVertexShape) shapes.elementAt(i);
            double             value   = pvShape.getValue();

        vertex = new mp2SpatialSample[pvShape.getNumberOfVertices ()];
            for (j=0; j<pvShape.getNumberOfVertices(); j++) {
            vertex[j]  = new mp2SpatialSample ();
                double[] v = pvShape.getVertex(j);
        vertex[j].init (v[0], v[1], value);

                sample = new mp2SpatialSample ();
                sample.init(v[0], v[1], value);
                data.addElement(sample);
            }
        for (j=1; j<vertex.length; j++) {
        if ((vertex[j].x - vertex[j-1].x) != 0.0) {
            slope      = (vertex[j].y - vertex[j-1].y) / (vertex[j].x - vertex[j-1].x);
            yIntercept = vertex[j].y - (slope * vertex[j].x);
            for (k=0; k<yCoord.length; k++) {
            if (((yCoord[k] > vertex[j].y) && (yCoord[k] < vertex[j-1].y)) ||
                ((yCoord[k] < vertex[j].y) && (yCoord[k] > vertex[j-1].y))) {

                coord = (yCoord[k] - yIntercept) / slope;
                            sample = new mp2SpatialSample ();
                            sample.init(coord, yCoord[k], value);
                            data.addElement(sample);
            }
            }

            if (slope != 0.0) {
                        for (k=0; k<xCoord.length; k++) {
                            if (((xCoord[k] > vertex[j].x) && (xCoord[k] < vertex[j-1].x)) ||
                                ((xCoord[k] < vertex[j].x) && (xCoord[k] > vertex[j-1].x))) {

                                coord = (slope * xCoord[k]) + yIntercept;
                                sample = new mp2SpatialSample ();
                                sample.init(xCoord[k], coord, value);
                                data.addElement(sample);
                }
                        }
                    }
        }
        }

// UNALLOCATED vertex

        }

    return data;
    }

    public Vector getQuadrantNeighbors (Vector data, double x, double y, int num) {
    boolean          found;
    int              i, j, k;
        int              quadSize = num / 4;
    double           dx, dy;
        double           slope, yIntercept, coord;
    mp2SpatialSample[] vertex;
    mp2SpatialSample sample, temp;
    Vector           matches = new Vector (num);
    Vector           q1      = new Vector (quadSize);
    Vector           q2      = new Vector (quadSize);
    Vector           q3      = new Vector (quadSize);
    Vector           q4      = new Vector (quadSize);
    Vector           q;
    num = Math.min (num, data.size ());
    if (num == 0) return (null);


        for (i=0; i<data.size (); i++) {
        sample          = (mp2SpatialSample) data.elementAt (i);
            dx              = sample.x - x;
            dy              = sample.y - y;
        sample.distance = (dx * dx) + (dy * dy);

            j     = 0;
            found = false;
        if      ((dx >= 0.0) && (dy >= 0.0)) q = q1;
        else if ((dx <  0.0) && (dy >= 0.0)) q = q2;
        else if ((dx <  0.0) && (dy <  0.0)) q = q3;
        else                                 q = q4;
        while (!found && (j < q.size ())) {
                temp = (mp2SpatialSample) q.elementAt (j);
        if (sample.distance < temp.distance) {
            q.insertElementAt (sample, j);
            found = true;
        }
        j++;
        }
        if (!found && (j < quadSize))
        q.addElement (sample);
        if (q.size () > quadSize)
        q.removeElementAt (quadSize);
    }
    for (i=0; i<q1.size(); i++) matches.addElement ((mp2SpatialSample) q1.elementAt (i));
    for (i=0; i<q2.size(); i++) matches.addElement ((mp2SpatialSample) q2.elementAt (i));
    for (i=0; i<q3.size(); i++) matches.addElement ((mp2SpatialSample) q3.elementAt (i));
    for (i=0; i<q4.size(); i++) matches.addElement ((mp2SpatialSample) q4.elementAt (i));

    return (matches);
    }

    public double interpolate (double x, double y, int scheme, double[] xCoord, double[] yCoord) {

    boolean          gradeSet = false;
        double           estimate;
        double           invDistance, sumValue, sumDistance;
        int              i;
        int              numNearest = NUM_QUADRANT_SEARCH;
        double           tolerence  = 0;
        double           distance;
    mp2SpatialSample sample;
    Vector           matches;
    Vector           data;

    if (scheme == INVERSE_DISTANCE) {
        data    = getNearestNeighborData ();
        matches = getNearestNeighbors (data, x, y, numNearest);
    }
    else {
        data    = getQuadrantNeighborData (xCoord, yCoord);
        matches = getQuadrantNeighbors (data, x, y, numNearest);
    }

    estimate    = 0.0;
    sumDistance = 0.0;
    sumValue    = 0.0;
    for (i=0; i<matches.size(); i++) {
        sample   = (mp2SpatialSample) matches.elementAt (i);
        distance = Math.sqrt (sample.distance);
        if (distance <= tolerence) {
        gradeSet = true;
        estimate = sample.value;
        }
        else {
        invDistance  = 1.0 / distance;
        sumDistance += invDistance;
        sumValue    += sample.value * invDistance;
        }
    }
    if (!gradeSet && (sumDistance > 0.0)) {
        estimate = sumValue / sumDistance;
    }

    return estimate;
    }

    public double interpolate_2 (double x, double y, int scheme, double[] xCoord, double[] yCoord) {

    boolean          gradeSet = false;
        double           estimate;
        double           invDistance, sumValue, sumDistance;
        int              i;
        int              numNearest = NUM_QUADRANT_SEARCH;
        double           tolerence2 = 0;
    mp2SpatialSample sample;
    Vector           matches;
    Vector           data;

    if (scheme == INVERSE_DISTANCE_2) {
        data    = getNearestNeighborData ();
        matches = getNearestNeighbors (data, x, y, numNearest);
    }
    else {
        data    = getQuadrantNeighborData (xCoord, yCoord);
        matches = getQuadrantNeighbors (data, x, y, numNearest);
    }

    estimate    = 0.0;
    sumDistance = 0.0;
    sumValue    = 0.0;
    for (i=0; i<matches.size(); i++) {
        sample = (mp2SpatialSample) matches.elementAt (i);
        if (sample.distance <= tolerence2) {
        gradeSet = true;
        estimate = sample.value;
        }
        else {
        invDistance  = 1.0 / sample.distance;
        sumDistance += invDistance;
        sumValue    += sample.value * invDistance;
        }
    }
    if (!gradeSet && (sumDistance > 0.0)) {
        estimate = sumValue / sumDistance;
    }

    return estimate;
    }

    public int getColorPolicy() {
        return colorPolicy;
    }

    public Color getColorForValue(double v) {
        return colorScale.GetColor((float) v);
    }

    public int getInterpolationScheme() {
        return interpolationScheme;
    }

    public int getPaletteScheme() {
        return colorScale.GetPaletteScheme();
    }

    public double [] getValueArray() {
        return valueArray;
    }

    public double getValueBlue() {
        return colorScale.GetValueBlue();
    }

    public double getValueRed() {
        return colorScale.GetValueRed();
    }

    public void setInterpolationScheme(int scheme) {
        interpolationScheme = scheme;
        setDataHaveChanged();
    }

    public void setPaletteScheme(int scheme) {
        colorScale.SetPaletteScheme(scheme);
        // we don't call setDataHaveChanged here, because
        // we don't need to rediscretize. Just mark the document
        // as changed. The view will recreate the image
        // using the new palette scheme.
        doc.setChanged(true);
    }

    public void setColorPolicy(int p) {
        colorPolicy = p;
        doc.setChanged(true);
    }

    public void setValueRange(double valueBlue, double valueRed,
                                boolean setChanged) {
        colorScale.SetLimits(valueBlue, valueRed);
        // we don't call setDataHaveChanged here, because
        // we don't need to rediscretize. Just mark the document
        // as changed. The view will recreate the image using
        // the new value range.
        if (setChanged) {
            doc.setChanged(true);
        }
    }

    protected void triangulate(double [] xCenter, 
                    double [] yCenter, int [] active) {

        // Prepare data for trimesh
        int numberOfPoints = 0;
        int numberOfSegments = 0;
        for (int i=0; i<shapes.size(); i++) {
            mp2PolyVertexShape shape = (mp2PolyVertexShape) shapes.elementAt(i);
            numberOfPoints += shape.getNumberOfVertices();
            numberOfSegments += shape.getNumberOfVertices();
            if (shape instanceof mp2Polyline) {
                numberOfSegments--;
            }
        }
        double [] pointList = new double [numberOfPoints*2];
        double [] valueList = new double [numberOfPoints];
        int [] segmentList = new int[numberOfSegments*2];
        int ipt = 0;
        int iatt = 0;
        int iseg = 0;
        double value;
        for (int i=0; i<shapes.size(); i++) {
            mp2PolyVertexShape shape = (mp2PolyVertexShape) shapes.elementAt(i);
            value = shape.getValue();
            int numberOfVertices = shape.getNumberOfVertices();
            for (int j=0; j<numberOfVertices; j++) {
                double [] v = shape.getVertex(j);
                pointList[ipt++] = v[0];
                pointList[ipt++] = v[1];
                valueList[iatt++] = value;
                if (j < numberOfVertices - 1) {
                    segmentList[iseg++] = iatt-1;
                    segmentList[iseg++] = iatt;
                } else if (shape instanceof mp2Polygon) {
                    segmentList[iseg++] = iatt-1;
                    segmentList[iseg++] = iatt-numberOfVertices;
                }
            }
        }

        Object [] triOutput = new Object [4];
        int quality = 0; // do constrained triangulation only
        mp2Math.trimesh(quality, pointList, valueList, 
                    segmentList, triOutput);

        // Get the results from triOutput
        int [] triangleList = (int []) triOutput[0];
        pointList = (double []) triOutput[1];
        valueList = (double []) triOutput[2];
        segmentList = (int []) triOutput[3];

        // Interpolate over the triangles
        double [] shapeFunction = new double [3];
        double x, y, x0, y0, x1, y1, x2, y2, x02, x12, xx2, y02, y12, yy2, det;
        int i, j, k, kk, n, index, start;
        start = 0;
        for (j=0; j<yCenter.length; j++) {
            y = yCenter[j];
            for (i=0; i<xCenter.length; i++) {
                x = xCenter[i];
                index = j * xCenter.length + i;
                if (active[index] > -2) {
                    // Search over all triangles to find the one
                    // that contains the point (x, y). This is a brute
                    // force approach that might take too much time
                    // if there are many triangles, in which case
                    // might be better to divide the triangles
                    // into bins according to location.
                    // However, to speed thing up, we start the search
                    // from the previous found triangle, hoping that
                    // the next point is sufficiently close to the previous
                    // point that it lies in the same triangle.
                    boolean found = false;
                    for (kk=start; kk<start+triangleList.length && !found; kk+=3) {
                        k = kk % triangleList.length;

                        // Get the points of the triangle vertices
                        x0 = pointList[2*triangleList[k]];
                        y0 = pointList[2*triangleList[k]+1];
                        x1 = pointList[2*triangleList[k+1]];
                        y1 = pointList[2*triangleList[k+1]+1];
                        x2 = pointList[2*triangleList[k+2]];
                        y2 = pointList[2*triangleList[k+2]+1];

                        // Solve for values of shape functions N0, N1, N2 such that
                        //     x = N0*x0 + N1*x1 + N2*x2
                        //     y = N0*y0 + N1*y1 + N3*y2
                        //     1 = N0 + N1 + N2
                        // where x0 = x0[0], x1 = x1[0], y0 = x0[1], y1 = x1[1], etc
                        // and N0 = shapeFunction[0], N1 = shapeFunction[1], etc
                        x02 = x0 - x2;
                        x12 = x1 - x2;
                        xx2 = x - x2;
                        y02 = y0 - y2;
                        y12 = y1 - y2;
                        yy2 = y - y2;
                        det = x02*y12 - x12*y02;
                        shapeFunction[0] = (xx2*y12 - x12*yy2)/det;
                        shapeFunction[1] = (x02*yy2 - xx2*y02)/det;
                        shapeFunction[2] = 1 - shapeFunction[0] - shapeFunction[1];
                        // if values of all three shape functions are between 0 and 1,
                        // then the point x is inside the triangle, and we do a linear
                        // interpolation using shape functions.
                        if (shapeFunction[0] >= 0 && shapeFunction[0] <= 1 &&
                            shapeFunction[1] >= 0 && shapeFunction[1] <= 1 &&
                            shapeFunction[2] >= 0 && shapeFunction[2] <= 1) {
                            valueArray[index] = shapeFunction[0] * valueList[triangleList[k]]
                                              + shapeFunction[1] * valueList[triangleList[k+1]]
                                              + shapeFunction[2] * valueList[triangleList[k+2]];
                            found = true;
                            start = k;
                        }
                    }
                    if (!found) {
                        // if the point is not in any triangle, then
                        // assign the value of the closest contour
                        mp2PolyVertexShape shape = (mp2PolyVertexShape) shapes.elementAt(0);
                        Object [] seginfo = shape.getNearestSegment(x, y);
                        double nearestDistance = ((Double) seginfo[1]).doubleValue();
                        mp2PolyVertexShape nearestShape = shape;

                        for (n=1; n<shapes.size(); n++) {
                            shape = (mp2PolyVertexShape) shapes.elementAt(n);
                            seginfo = shape.getNearestSegment(x, y);
                            double distance = ((Double) seginfo[1]).doubleValue();
                            if (distance < nearestDistance) {
                                nearestDistance = distance;
                                nearestShape = shape;
                            }
                        }
                        valueArray[index] = nearestShape.getValue();
                    }
                }
            }
        }
    }

    class mp2SpatialSample {

        protected double x, y, value, distance;

        public mp2SpatialSample () {}

        public void init (double x, double y, double value) {
        
        this.x     = x;
        this.y     = y;
        this.value = value;
        }
    }

}
