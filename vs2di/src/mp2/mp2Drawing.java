/*
 * mp2Drawing.java
 */
package mp2;

import java.awt.*;
import javax.swing.*;

/**
 * Represents a drawing that is displayed on the screen. The size of
 * the drawing as displayed on the screen is defined by the drawing
 * width and height in pixels.
 * 
 * <p>A point on the drawing can be represented with reference to two
 * coordinate system--screen coordinates and model coordinates.
 * 
 * <p>The screen coordinate system is measured in pixels on the screen.
 * The origin of screen coordinates system is always at the upper
 * left corner of the drawing. A point on the drawing is specified by
 * the horizontal distance (in number of pixels) from the left edge of
 * the drawing to the point, and by the vertical distance (in number of
 * pixels) from the top of the drawing to the point.
 * 
 * <p>The model coordinate system is measured in "model unit" which is
 * application specific. For example, the model unit might be meters.
 * The x axis of the model coordinate system always points to the right.
 * The y axis may point upward or downward. The origin of the model
 * coordinate system is defined by the application or the user and is
 * specified by screen coordinates. In other words, the origin of the
 * model coordinate system is specified by the horizontal distance (in
 * number of pixels) from the left edge of the drawing and the vertical
 * distance (in number of pixels) from the top of the drawing.
 * 
 * <p>The transformation between model coordinate system and screen
 * coordinate system is performed by the methods ModelToView and 
 * ViewToModel. 
 */
public abstract class mp2Drawing extends JComponent implements mp2Constants {

    /**
     * The width of the drawing in pixels
     */
    protected int drawingWidthInPixels;

    /**
     * The height of the drawing in pixels
     */
    protected int drawingHeightInPixels;

    /**
     * Model distance (in model units, such as meters) in the
     * x direction represented by each pixel
     */
    protected double modelDistancePerPixelX;

    /**
     * Model distance (in model units, such as meters) in the
     * y direction represented by each pixel
     */
    protected double modelDistancePerPixelY;

    /**
     * Orientation of the vertical axis of the model coordinate system
     */
    protected int modelVerticalAxisOrientation;

    /**
     * Horizontal distance (in numbero of pixels) from the left edge of the 
     * drawing to the origin of the model coordinate system.
     */
    protected int xOriginInPixels;

    /**
     * Vertical distance (in number of pixels) from the top edge of the
     * drawing to the origin of the model coordinate system. 
     * Note that this definition is the independent of the orientation 
     * of the model's vertical axis.
     */
    protected int yOriginInPixels;
    
    protected boolean isPrinting;
    
    protected double screenDistortionX;
    
    protected double screenDistortionY;

    /**
     * Constant to specify a downward-positive vertical axis for the model.
     */
    public static final int MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE = 1;

    /**
     * Constant to specify an upward positive vertical axis for the model.
     */
    public static final int MODEL_VERTICAL_AXIS_UPWARD_POSITIVE = 2;

    /**
     * Creates a new drawing with the specified orientation of the vertical
     * axis for the model coordinate system.
     * 
     * @param modelVerticalAxisOrientation If this parameter is set to
     *              MODEL_VERTICAL_AXIS_UPWARD_POSITIVE, then the positive
     *              vertical axis direction points upward. If set to 
     *              MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE, then the positive
     *              vertical axis direction points downward.
     */
    public mp2Drawing(int modelVerticalAxisOrientation) {
        this.modelVerticalAxisOrientation  = modelVerticalAxisOrientation;
        drawingWidthInPixels = 
                (int) Math.round(DEFAULT_DRAWING_WIDTH_IN_INCHES * 
                PIXELS_PER_INCH);
        drawingHeightInPixels = 
                (int) Math.round(DEFAULT_DRAWING_HEIGHT_IN_INCHES *
                PIXELS_PER_INCH);
        modelDistancePerPixelX = 1;
        modelDistancePerPixelY = 1;
        xOriginInPixels = 0;
        yOriginInPixels = 0;
        isPrinting = false;
        screenDistortionX = 1;
        screenDistortionY = 1;
    }

    /**
     * Creates a new drawing with a downward positive vertical axis
     * for the model coordinate system.
     */
    public mp2Drawing() {
        this(MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE);
    }
    
    public double getScreenDistortionX() {
        return screenDistortionX;
    }
    
    
    public double getScreenDistortionY() {
        return screenDistortionY;
    }
    
    public boolean setScreenDistortionX(double newScreenDistortionX) {
        if (newScreenDistortionX <= 0) {
            return false;
        }
        double scale = newScreenDistortionX/screenDistortionX;
        int newDrawingWidthInPixels = (int) Math.round(drawingWidthInPixels * scale);
        if (newDrawingWidthInPixels > MAXIMUM_DRAWING_DIMENSION_IN_PIXELS) {
            return false;
        }
        drawingWidthInPixels = newDrawingWidthInPixels;
        modelDistancePerPixelX = modelDistancePerPixelX / scale;
        xOriginInPixels = (int) Math.round(xOriginInPixels * scale);
        screenDistortionX = newScreenDistortionX;
        return true;
    }
    
    public boolean setScreenDistortionY(double newScreenDistortionY) {
        if (newScreenDistortionY <= 0) {
            return false;
        }
        double scale = newScreenDistortionY/screenDistortionY;
        int newDrawingHeightInPixels = (int) Math.round(drawingHeightInPixels * scale);
        if (newDrawingHeightInPixels > MAXIMUM_DRAWING_DIMENSION_IN_PIXELS) {
            return false;
        }
        drawingHeightInPixels = newDrawingHeightInPixels;
        modelDistancePerPixelY = modelDistancePerPixelY / scale;
        yOriginInPixels = (int) Math.round(yOriginInPixels * scale);
        screenDistortionY = newScreenDistortionY;
        return true;
    }

    /**
     * Gets the the width of the drawing in pixels
     *
     * @return the width of the drawing in pixels.
     */
    public int getDrawingWidthInPixels() {
        return drawingWidthInPixels;
    }

    /**
     * Gets the the height of the drawing in pixels
     *
     * @return the height of the drawing in pixels
     */
    public int getDrawingHeightInPixels() {
        return drawingHeightInPixels;
    }

    /**
     * Gets the model distance (e.g. in meters) per pixel in the 
     * x direction
     * 
     * @return the model distance per pixel in the x direction
     */
    public double getModelDistancePerPixelX() {
        return modelDistancePerPixelX;
    }

    /**
     * Gets the model distance (e.g. in meters) per pixel in the 
     * y direction
     * 
     * @return the model distance per pixel in the y direction
     */
    public double getModelDistancePerPixelY() {
        return modelDistancePerPixelY;
    }

    /**
     * Gets the orientation of the vertical axis in the model coordinate system.
     * 
     * @return the constant MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE if the
     *         positive vertical axis points downward, or the constant
     *         MODEL_VERTICAL_AXIS_UPWARD_POSITIVE if the positive vertical
     *         axis points upward.
     */
    public int getModelVerticalAxisOrientation() {
        return modelVerticalAxisOrientation;
    }

    /**
     * Returns the minimum size of this drawing (in pixels).
     *
     * @return a java.awt.Dimension object containing the minimum width and
     *         height (in pixels) of this drawing
     */
    public Dimension getMinimumSize() {
        return new Dimension(drawingWidthInPixels, drawingHeightInPixels);
    }

    /**
     * Returns the preferred size of this drawing (in pixels).
     *
     * @return a java.awt.Dimension object containing the preferred width and
     *         height (in pixels) of this drawing
     */
    public Dimension getPreferredSize() {
        return new Dimension(drawingWidthInPixels, drawingHeightInPixels);
    }

    /**
     * Gets the horizontal distance (in number of pixels) from the left edge 
     * of the drawing to the origin of the model coordinate system.
     * 
     * @return the horizontal distance (in number of pixels) from the left edge 
     *         of the drawing to origin of the model coordinate system.
     */
    public int getXOriginInPixels() {
        return xOriginInPixels;
    }

    /**
     * Gets the vertical distance (in number of pixels) from the top of the
     * drawing to origin of the model coordinate system. Note that
     * this definition is independent of vertical axis orientation.
     * 
     * @return the vertical distance (in number of pixels) from the top of the
     *         drawing to origin of the model coordinate system.
     */
    public int getYOriginInPixels() {
        return yOriginInPixels;
    }

    public boolean isPrinting() {
        return isPrinting;
    }

    /**
     * Sets the drawing size (width and height) in pixels. This is the
     * size of the drawing as shown on the screen.
     *
     * @parem  w  the width of the drawing in pixels
     * @param  h  the height of the drawing in pixels
     */
    public void setDrawingSizeInPixels(int w, int h) {
        drawingWidthInPixels = w; 
        drawingHeightInPixels = h;
    }

    /**
     * Sets the location of the origin of the coordinates axis system.
     * This location is defined by screen coordinates. 
     *
     * @param  xo  horizontal distance (in number of pixels) from the 
     *             left edge of the drawing to the origin of the 
     *             to the origin of the model coordinate system.
     * @param  yo  vertical distance (in number of pixels) from the 
     *             top edge of the drawing to the origin of the model
     *             coordinate system.
     */
    public void setOriginInPixels(int xo, int yo) {
        xOriginInPixels = xo;
        yOriginInPixels = yo;
    }

    /**
     * Sets the model distance (in model units such as meters) per
     * pixel in the x and y directions.
     *
     * @param px the model distance in the <i>x</i> direction equivalent
     *           to one pixel on the screen.
     * @param py the model distance in the <i>y</i> direction equivalent
     *           to one pixel on the screen.
     */
    public void setModelDistancePerPixel(double px, double py) {
        modelDistancePerPixelX = px;
        modelDistancePerPixelY = py;
    }

    /**
     * Sets the orientation of the model's vertical axis
     */
    public void setModelVerticalAxisOrientation(int v) {
        modelVerticalAxisOrientation = v;
    }

    /**
     * Transforms model coordinates (for example, meters) to screen
     * coordinates (in pixels).
     * 
     * @param  x  the <i>x</i> value in the model coordinate system.
     * @param  y  the <i>y</i> value in the model coordinate system.
     *
     * @return  a <code>java.awt.Point</code> object containing the
     *           location of the point in screen coordinates (pixels).
     */
    public Point modelToView(double x, double y) {
        int ix, iy;
        ix = (int) Math.round(x/modelDistancePerPixelX) + xOriginInPixels;
        if (modelVerticalAxisOrientation == MODEL_VERTICAL_AXIS_UPWARD_POSITIVE) {
            iy = yOriginInPixels - (int) Math.round(y/modelDistancePerPixelY);
        } else {
            iy = (int) Math.round(y/modelDistancePerPixelY) + yOriginInPixels;
        }
        return new Point(ix, iy);
    }

    /**
     * Transforms model coordinates (for example, meters) to screen
     * coordinates (in pixels).
     * 
     * @param  x  A 2-element <code>double</code> array of which the 
     *            first element is the <i>x</i> value, and the 
     *            second element is the <i>y</i> value in model
     *            coordinate system.
     *
     * @return  a <code>java.awt.Point</code> object containing the
     *           location of the point in screen coordinates (pixels).
     */
    public Point modelToView(double [] x) {
        return modelToView(x[0], x[1]);
    }

    /**
     * Transforms screen coordinates (in pixels) to model coordinates
     * (for example, meters).
     *
     * @param  p  A <code>java.awt.Point</code> object specifying the
     *            point in screen coordinates.
     *
     * @return  a 2-element double array of which the first element
     *          is the x value and the second element is the y value
     *          in model coordinates.
     */
    public double [] viewToModel(Point p) {
        double [] w = new double[2];
        w[0] = (p.x - xOriginInPixels) * modelDistancePerPixelX;
        if (modelVerticalAxisOrientation == MODEL_VERTICAL_AXIS_UPWARD_POSITIVE) {
            w[1] = (yOriginInPixels - p.y) * modelDistancePerPixelY;
        } else {
            w[1] = (p.y - yOriginInPixels) * modelDistancePerPixelY;
        }
        return w;
    }

    /**
     * Transforms screen coordinates (in pixels) to model coordinates
     * (for example, meters).
     *
     * @param  i  the horizontal distance (in pixels) from the left edge of
     *            the drawing to the specified point.
     * @param  j  the vertical distance (in pixels) from the top of the 
     *            drawing to the specified point.
     *
     * @return  a 2-element double array of which the first element
     *          is the x value and the second element is the y value
     *          in model coordinates.
     */
    public double [] viewToModel(int i, int j) {
        return viewToModel(new Point(i, j));
    }
}
