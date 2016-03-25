/*
 * mp2Ruler.java
 */
package mp2;

import java.awt.*;
//import java.awt.geom.*;
import java.awt.image.*;
import java.text.*;
import java.util.*;
import javax.swing.*;

/**
 * A ruler to show the scale for a drawing.
 * 
 * <p>The ruler may be horizontal or vertical. The ruler may display inches
 * on the drawing or model units. The size of a major tick interval 
 * is automatically determined to be close to a target size. 
 * The number of sub tick intervals is either 4 or 5 and is also 
 * automatically determined.
 * 
 * @see mp2.mp2Drawing
 */
public class mp2Ruler extends JComponent implements mp2Constants {
    
    /**
     * The "ascent" (or height) of characters used in this ruler.
     */
    protected int charAscent;

    /**
     * The drawing for which this ruler is provided.
     */
    protected mp2Drawing drawing;
    
    /**
     * The font used by this ruler
     */
    protected Font font = new Font ("Dialog", Font.PLAIN, 12);
    
    /**
     * The font matrics used in this ruler.
     */
    protected FontMetrics fontMetrics;
    
    /**
     * The level of magnification
     */
    protected double magnification;

    /**
     * A NumberFormat object used to format ruler labels.
     */
    protected NumberFormat nf = NumberFormat.getInstance();

    /**
     * The number of sub-intervals in a major interval. Defines the
     * number of minor ticks per majar interval.
     */
    protected int numSubInterval;
    
    /**
     * The orientation (horizontal or vertical) if this ruler
     */
    protected int orientation;
    
    /**
     * Size of a major interval in pixels.
     */
    protected double pixelsPerMajorInterval;
    
    /**
     * Size of a sub interval in pixels.
     */
    protected double pixelsPerSubInterval;
    
    /**
     * The number of pixels per ruler unit.
     */
    protected double pixelsPerRulerUnit;
    
    /**
     * The units per major interval.
     */
    protected double rulerUnitsPerMajorInterval;

    /**
     * The starting point for drawing the ruler during paint.
     */
    protected double rulerStart;
    
    /**
     * The starting value for labeling the ruler during paint.
     */
    protected double rulerStartValue;
    
    /**
     * The target size (in pixels) of a major interval. 
     */
    protected final double targetPixelsPerMajorInterval = 120;
    
    /**
     * The units displayed by this ruler. This can either be inches
     * on the drawing or model units.
     */
    protected int units;
    
    protected static final String VERTICAL_CHAR[] = {"0", "1", "2", "3",
                            "4", "5", "6", "7", "8", "9", ".", "-", ",", "?"};
    
    protected Hashtable verticalLabels;

    /**
     * Constant to specify that this ruler displays inches
     */
    public static final int INCHES = 0;
    
    /**
     * Constant to specify that this ruler displays model units.
     */
    public static final int MODEL_UNITS = 1;
    
    /**
     * Constant to specify that this ruler is horizontal.
     */
    public static final int HORIZONTAL = 0;
    
    /**
     * Constant to specify that this ruler is vertical.
     */
    public static final int VERTICAL = 1;
    
    /**
     * Constant to specify the size of this ruler. The size is
     * width perpendicular to the longitudinal direction of the ruler.
     */
    protected static final int RULER_SIZE = 20;
    
    /**
     * Constant that holds the value of the natural logarithm of 10.
     */
    protected static final double LN10 = Math.log(10.0);

    /**
     * Creates a ruler of the specified orientation for the specified drawing.
     * 
     * @param orientation if HORIZONTAL, the ruler is horizontal. If VERTICAL,
     *                    the ruler is vertical.
     * @param drawing the drawing for which this ruler is provided.
     */
    public mp2Ruler(int orientation, mp2Drawing drawing) {
        this.drawing = drawing;
        this.orientation = orientation;
        this.units = INCHES;
        magnify(1);  //This also updates the scales and labels
        setFont(font);
        fontMetrics = getFontMetrics(getFont());
        charAscent = fontMetrics.getAscent();
        verticalLabels = null;
    }
    
    /**
     * Computes the starting point and starting value for painting
     * a hozitonal ruler.
     */
    protected void computeHorizontalStart() {
        double xOrigin, offset;
        xOrigin = drawing.getXOriginInPixels();
        if (xOrigin > 0) {
            offset = Math.ceil(xOrigin / pixelsPerMajorInterval)
                           * pixelsPerMajorInterval;
            rulerStart = xOrigin - offset;
            rulerStartValue = - offset / pixelsPerRulerUnit;
        } else if (xOrigin < 0) {
            offset = Math.floor(-xOrigin / pixelsPerMajorInterval)
                           * pixelsPerMajorInterval;
            rulerStart = xOrigin + offset;
            rulerStartValue = offset / pixelsPerRulerUnit;
        } else {
            rulerStart = 0;
            rulerStartValue = 0;
        }
    }
    
    /**
     * Computes the starting point and starting value (at the bottom
     * of the drawing) for painting a vertical upward ruler.
     */
    protected void computeVerticalUpwardStart() {
        double yOrigin, offset;
        yOrigin = drawing.getYOriginInPixels();
        int drawingHeight = drawing.getDrawingHeightInPixels();
        double y0 = drawingHeight - yOrigin;
        if (y0 > 0) {
            offset = Math.ceil(y0 / pixelsPerMajorInterval)
                           * pixelsPerMajorInterval;
            rulerStart = yOrigin + offset;
            rulerStartValue = - offset / pixelsPerRulerUnit;
        } else if (y0 < 0) {
            offset = Math.floor(-y0 / pixelsPerMajorInterval)
                           * pixelsPerMajorInterval;
            rulerStart = yOrigin - offset;
            rulerStartValue = offset / pixelsPerRulerUnit;
        } else {
            rulerStart = drawingHeight;
            rulerStartValue = 0;
        }
    }
    
    /**
     * Computes the starting point and starting value (at the top
     * of the drawing) for painting a vertical downward ruler.
     */
    protected void computeVerticalDownwardStart() {
        double yOrigin, offset;
        yOrigin = drawing.getYOriginInPixels();
        if (yOrigin > 0) {
            offset = Math.ceil(yOrigin / pixelsPerMajorInterval)
                           * pixelsPerMajorInterval;
            rulerStart = yOrigin - offset;
            rulerStartValue = - offset / pixelsPerRulerUnit;
        } else if (yOrigin < 0) {
            offset = Math.floor(-yOrigin / pixelsPerMajorInterval)
                           * pixelsPerMajorInterval;
            rulerStart = yOrigin + offset;
            rulerStartValue = offset / pixelsPerRulerUnit;
        } else {
            rulerStart = 0;
            rulerStartValue = 0;
        }
    }

    protected void createVerticalLabels() {
        verticalLabels = new Hashtable();
        int bufferHeight = charAscent + 2;
        Image image;
        for (int i=0; i<VERTICAL_CHAR.length; i++) {
            int bufferWidth = fontMetrics.stringWidth(VERTICAL_CHAR[i]);
            image = createImage(bufferWidth, bufferHeight);
            Graphics g = image.getGraphics();
            g.setFont(font);
            g.setColor(Color.white);
            g.fillRect(0, 0, bufferWidth, bufferHeight);
            g.setColor(Color.black);
            g.drawString(VERTICAL_CHAR[i], 1, charAscent + 1);
            g.dispose();
        
            int [] buffer = new int[bufferWidth*bufferHeight];
            int [] rotate = new int[bufferWidth*bufferHeight];
        
            PixelGrabber grabber = new PixelGrabber(image, 
                    0, 0, bufferWidth, bufferHeight, buffer, 0, bufferWidth);
            try {
                grabber.grabPixels();
            } catch(InterruptedException e) {
                e.printStackTrace();
            }
            for (int y = 0; y < bufferHeight; y++) {
                for (int x = 0; x < bufferWidth; x++) {
                    rotate[((bufferWidth-x-1)*bufferHeight)+y] = buffer[(y*bufferWidth)+x];
                }
            }
            image = createImage(new MemoryImageSource(bufferHeight, bufferWidth, 
                                    rotate, 0, bufferHeight)); 
            verticalLabels.put(VERTICAL_CHAR[i], image);
        }
    }

    /**
     * Determine the optimal label policy in terms of:  
     * a. ruler units per major interval, and
     * b. pixels per major interval
     */
    protected void doOptimalLabelPolicy() {
        double a, b, c, p;
        a = targetPixelsPerMajorInterval / pixelsPerRulerUnit;
        p = Math.floor(Math.log(a)/LN10);
        b = a/Math.pow(10, p);
        if (b > 3.5) {
            c = 5;
            numSubInterval = 5;
        } else if (b >1.5) {
            c = 2;
            numSubInterval = 4;
        } else {
            c = 1;
            numSubInterval = 5;
        }
        rulerUnitsPerMajorInterval = c * Math.pow(10, p);
        pixelsPerMajorInterval = 
                rulerUnitsPerMajorInterval * pixelsPerRulerUnit;
        pixelsPerSubInterval = 
                pixelsPerMajorInterval / numSubInterval;
        if (p < 0) {
            int digits = - (int) Math.round(p);
            nf.setMaximumFractionDigits(digits);
            nf.setMinimumFractionDigits(digits);
        } else {
            nf.setMaximumFractionDigits(0);
            nf.setMinimumFractionDigits(0);
        }        
    }
    
    /**
     * Gets the preferred size of this Component.
     * 
     * @return A java.awt.Dimension object containing the preferred size.
     */
    public Dimension getPreferredSize() {
        if (orientation == HORIZONTAL) {
            return new Dimension(drawing.getDrawingWidthInPixels(), 
                                 RULER_SIZE);
        } else {
            return new Dimension(RULER_SIZE, 
                                 drawing.getDrawingHeightInPixels());
        }
    }

    /**
     * Magnifies the ruler by changing the number of pixels per inch
     */
    public void magnify(double magnification) {
        this.magnification = magnification;
        updateScalesAndLabels();
    }

    /**
     * Paints this Component.
     * 
     * @param g the java.awt.Graphics object for drawing
     */
    public void paintComponent(Graphics g) {
        if (orientation == HORIZONTAL) {
            paintHorizontalRuler(g);
        }
        else if (orientation == VERTICAL) {
            if (drawing.getModelVerticalAxisOrientation() ==
                    mp2Drawing.MODEL_VERTICAL_AXIS_UPWARD_POSITIVE) {
                paintVerticalUpwardRuler(g);
            } else {
                paintVerticalDownwardRuler(g);
            }
        }
    }
    
    /**
     * Paints a horizontal ruler
     * 
     * @param g the java.awt.Graphics object for drawing
     */
    protected void paintHorizontalRuler(Graphics g) {
        double d, start, end, value;
        int j, p, tickSize, n;
        String text;

        // Draw the background. Fill only the visible area
        Rectangle visibleRect = g.getClipBounds();
        g.setColor(Color.white);
        g.fillRect(visibleRect.x, visibleRect.y, 
                   visibleRect.width, visibleRect.height);
        
        // compute start and end positions for labels and tick marks
        n = (int) (visibleRect.x/pixelsPerMajorInterval) - 1;
        start = rulerStart + n*pixelsPerMajorInterval;
        value = rulerStartValue + n*rulerUnitsPerMajorInterval;
        end = visibleRect.x + visibleRect.width + pixelsPerMajorInterval;
        
        // Draw text and ticks
        g.setColor(Color.black);
        for (d=start; d<end; d += pixelsPerMajorInterval) {
            for (j=0; j<numSubInterval; j++) {
                p = (int) Math.round(d + j*pixelsPerSubInterval);
                if (j==0) {
                    tickSize = RULER_SIZE;
                    if (d == 0 && units == INCHES) {
                        text = "0 in";
                    } else {
                        text = nf.format(value);
                    }
                    g.drawString(text, p+3, RULER_SIZE-5);
                } else {
                    tickSize = RULER_SIZE/3;
                }
                g.drawLine(p, RULER_SIZE-1, p, RULER_SIZE-tickSize-1);
            }
            value += rulerUnitsPerMajorInterval;
        }
    }
                                        
    /**
     * Paints a vertical ruler pointing downward
     * 
     * @param g the java.awt.Graphics object for drawing
     */
    protected void paintVerticalDownwardRuler(Graphics g) {
        double d, start, end, value;
        int j, p, tickSize, n;
        String text;

        if (verticalLabels == null) {
            createVerticalLabels();
        }
        
        // Draw the background. Fill only the visible area
        Rectangle visibleRect = g.getClipBounds();
        g.setColor(Color.white);
        g.fillRect(visibleRect.x, visibleRect.y, 
                   visibleRect.width, visibleRect.height);
        
        // compute start and end positions for labels and tick marks
        n = (int) (visibleRect.y/pixelsPerMajorInterval) - 1;
        start = rulerStart + n*pixelsPerMajorInterval;
        value = rulerStartValue + n*rulerUnitsPerMajorInterval;
        end = visibleRect.y + visibleRect.height + pixelsPerMajorInterval;
        
        // Draw text and ticks
        g.setColor(Color.black);
        //Graphics2D g2 = (Graphics2D) g;
        //g2.rotate(Math.toRadians(-90));
        for (d=start; d<end; d += pixelsPerMajorInterval) {
            for (j=0; j<numSubInterval; j++) {
                p = (int) Math.round(d + j*pixelsPerSubInterval);
                if (j==0) {
                    tickSize = RULER_SIZE;
                    text = nf.format(value);
                    //g2.drawString(text, -p+2, charAscent);
                    Image image;
                    int q = p+4;
                    for (n=text.length()-1; n>=0; n--) {
                        String key = String.valueOf(text.charAt(n));
                        image = (Image) verticalLabels.get(key);
                        if (image == null) {
                            // just in case we get junk
                            image = (Image) verticalLabels.get("?");
                        }
                        g.drawImage(image, 2, q, this);  
                        q += image.getHeight(this);
                    }
                } else {
                    tickSize = RULER_SIZE/3;
                }
                //g2.drawLine(-p, RULER_SIZE-1, -p, RULER_SIZE-tickSize-1);
                g.drawLine(RULER_SIZE-1, p, RULER_SIZE-tickSize-1, p);
            }
            value += rulerUnitsPerMajorInterval;
        }
    }
    
    /**
     * Paints a vertical ruler pointing upward
     * 
     * @param g the java.awt.Graphics object for drawing
     */
    protected void paintVerticalUpwardRuler(Graphics g) {
        double d, start, end, value;
        int j, p, tickSize, n;
        String text;
        
        if (verticalLabels == null) {
            createVerticalLabels();
        }

        // Draw the background. Fill only the visible area
        Rectangle visibleRect = g.getClipBounds();
        g.setColor(Color.white);
        g.fillRect(visibleRect.x, visibleRect.y, 
                   visibleRect.width, visibleRect.height);
        
        // compute start and end positions for labels and tick marks
        // ***** check this before using vertical upward ruler *****
        n = (int) ((rulerStart - visibleRect.y - visibleRect.height)/pixelsPerMajorInterval) - 1;
        start = rulerStart - n*pixelsPerMajorInterval;
        value = rulerStartValue + n*rulerUnitsPerMajorInterval;
        end = visibleRect.y - pixelsPerMajorInterval;
        
        // Draw text and ticks
        g.setColor(Color.black);
        //Graphics2D g2 = (Graphics2D) g;
        //g2.rotate(Math.toRadians(-90));
        for (d=start; d>end; d -= pixelsPerMajorInterval) {
            for (j=0; j<numSubInterval; j++) {
                p = (int) Math.round(d - j*pixelsPerSubInterval);
                if (j==0) {
                    tickSize = RULER_SIZE;
                    text = nf.format(value);
                    //g2.drawString(text, -p+2, charAscent);
                    Image image;
                    int q = p+4;
                    for (n=text.length()-1; n>=0; n--) {
                        String key = String.valueOf(text.charAt(n));
                        image = (Image) verticalLabels.get(key);
                        if (image == null) {
                            // just in case we get junk
                            image = (Image) verticalLabels.get("?");
                        }
                        g.drawImage(image, 2, q, this);  
                        q += image.getHeight(this);
                    }
                } else {
                    tickSize = RULER_SIZE/3;
                }
                //g2.drawLine(-p, RULER_SIZE-1, -p, RULER_SIZE-tickSize-1);
                g.drawLine(RULER_SIZE-1, p, RULER_SIZE-tickSize-1, p);
            }
            value += rulerUnitsPerMajorInterval;
        }
    }

    /**
     * Sets the units on the ruler (in inches or model units)
     * 
     * @param units if this parameter is INCHES, the ruler displays inches
     *              on the drawing. If this parameter is MODEL_UNITS, the
     *              ruler displays model units.
     */
    public void setUnits (int units) {
        this.units = units;
        updateScalesAndLabels();
    }

    /**
     * Updates the scales and labels. This is needed if there is
     * a change in drawing scales or coordinate axis origin
     */
    public void updateScalesAndLabels() {

        // Determine the pixels per ruler unit
        if (units == INCHES) {
            pixelsPerRulerUnit = PIXELS_PER_INCH * magnification;
        } else {
            double [] v1 = drawing.viewToModel(new Point(0, 0));
            double [] v2 = drawing.viewToModel(
                        new Point(drawing.getDrawingWidthInPixels(),
                                  drawing.getDrawingHeightInPixels()));
            if (orientation == HORIZONTAL) {
                pixelsPerRulerUnit = drawing.getDrawingWidthInPixels()
                            / Math.abs(v2[0] - v1[0]);
            } else {
                pixelsPerRulerUnit = drawing.getDrawingHeightInPixels()
                            / Math.abs(v2[1] - v1[1]);
            }
        }

        // Determine the optimal labeling policy
        doOptimalLabelPolicy();

        // Determine the starting point and starting value for 
        // drawing labels
        if (units == INCHES) {
            rulerStartValue = 0;
            if (orientation == VERTICAL && drawing.getModelVerticalAxisOrientation() ==
                    mp2Drawing.MODEL_VERTICAL_AXIS_UPWARD_POSITIVE) {
                rulerStart = drawing.getDrawingHeightInPixels();
            } else {
                rulerStart = 0;
            }
        } else {
            if (orientation == HORIZONTAL) {
                computeHorizontalStart();
            } else {
                if (drawing.getModelVerticalAxisOrientation() ==
                            mp2Drawing.MODEL_VERTICAL_AXIS_UPWARD_POSITIVE) {
                    computeVerticalUpwardStart();

                } else {
                    computeVerticalDownwardStart();
                }
            }
        }
    }
}
