/*
 * mp2ColorScale.java
 */
package mp2;

import java.awt.*;
import java.io.*;

public class mp2ColorScale implements Serializable {

    static final long serialVersionUID = -278880825436585620L;

    protected double valueBlue;
    protected double valueRed;
    protected double colorInterval;
    protected double labelInterval;
    protected int    paletteScheme;
    protected int [] customCode;
    protected Color [] customColor;
    protected String [] customLabel;
    protected boolean doCustom;
    protected boolean doDraw;

    protected transient double valueRange;
    protected transient double oneThirdOfValueRange;
    protected transient double twoThirdsOfValueRange;
    protected transient int height;
    protected transient int width;

    public final static int GRAY     = 0;
    public final static int HUE      = 1;
    public final static int SPECTRUM = 2;


    /**
     * Constructor
     */
    public mp2ColorScale() {
        valueBlue = 0;
        valueRed = 1;
        colorInterval = 0.1;
        labelInterval = 0.1;
        paletteScheme = SPECTRUM;
        doCustom = false;
        doDraw = true;
    }

    /**
     * Initializes transient data
     */
    public void init() {
        valueRange = Math.abs(valueRed - valueBlue);
        oneThirdOfValueRange = valueRange/3;
        twoThirdsOfValueRange = 2*valueRange/3;
        height = 200;
        width = 20;
    }


    /**
     * Gets the color for a given value
     */
    public Color GetColor(float value) {

        double min     = Math.min (valueRed, valueBlue);
        double max     = Math.max (valueRed, valueBlue);
        double range   = max - min;
        double percent = (value - min) / range;
        Color  color   = new Color (0, 0, 0);
        int    r, g, b;

        percent = Math.min (1.0, Math.max (0.0, percent));
        if (valueBlue > valueRed) {
            percent = 1.0 - percent;
        }

        r = g = b = 0;
        switch (paletteScheme) {
        case GRAY:
            r = g = b = (int) (percent * 255);
            break;
        case HUE:
            if (percent < 0.5) {
                b = (int) ((1.0 - (2.0 * percent)) * 255);
                g = 255 - b;
            } else {
                r = (int) ((2.0 * (percent - 0.5)) * 255);
                g = 255 - r;
            }
            break;
        default:        // fall through
        case SPECTRUM:
            // Handle the case when valueRed is greater than valueBlue
            double v;
            if (valueRed > valueBlue) {
                v = value - valueBlue;
                if (v < oneThirdOfValueRange) {
                    r = 0;
                    g = (int) (255*v/oneThirdOfValueRange);
                    b = 255;
                } else if (v > twoThirdsOfValueRange) {
                    r = 255;
                    g = (int) (255*(valueRange-v)/oneThirdOfValueRange);
                    b = 0;
                } else {
                    r = (int) (255*(v-oneThirdOfValueRange)/oneThirdOfValueRange);
                    g = 255;
                    b = (int) (255*(twoThirdsOfValueRange-v)/oneThirdOfValueRange);
                }
            }

            // Handle the case when valueBlue is greater than valueRed
            else {
                v = value - valueRed;
                if (v < oneThirdOfValueRange) {
                    r = 255;
                    g = (int) (255*v/oneThirdOfValueRange);
                    b = 0;
                } else if (v > twoThirdsOfValueRange) {
                    r = 0;
                    g = (int) (255*(valueRange-v)/oneThirdOfValueRange);
                    b = 255;
                } else {
                    b = (int) (255*(v-oneThirdOfValueRange)/oneThirdOfValueRange);
                    g = 255;
                    r = (int) (255*(twoThirdsOfValueRange-v)/oneThirdOfValueRange);
                }
            }

            // Clamp the RGB components to the end if they exceed the limit
            if (r<0) {
                r = 0;
            }
            if (r>255) {
                r= 255;
            }
            if (g<0) {
                g = 0;
            }
            if (g>255) {
                g = 255;
            }
            if (b<0) {
                b = 0;
            }
            if (b>255) {
                b = 255;
            }
            break;
        }
        return new Color(r, g, b);
    }
    
    public Color GetColorForCustomCode(int code) {
        if (customCode == null) {
            return Color.black;
        }
        for (int i=0; i<customCode.length; i++) {
            if (code == customCode[i]) {
                return customColor[i];
            }
        }
        return Color.black;
    }

    /**
     * Gets the color interval. This is the contour interval.
     */
    public double GetColorInterval() {
        return colorInterval;
    }

    /**
     * Gets the label increment. This is the interval 
     * for labeling values on the color scale
     */
    public double GetLabelInterval() {
        return labelInterval;
    }

    /**
     * Gets the palette scheme
     */
    public int GetPaletteScheme() {
        return paletteScheme;
    }

    /**
     * Gets the value at the blue limit
     */
    public double GetValueBlue() {
        return valueBlue;
    }

    /**
     * Gets the value at the red limit
     */
    public double GetValueRed() {
        return valueRed;
    }
    
    /**
     * Sets the custom codes and colors
     */
    public void SetCustom(int [] code, Color [] color, String [] label) {
        customCode = code;
        customColor = color;
        customLabel = label;
    }

    /**
     * Sets the color interval
     */
    public void SetColorInterval(double colorInterval) {
        this.colorInterval = colorInterval;
    }
    
    public void SetDoCustom(boolean b) {
        this.doCustom = b;
    }
    
    public void SetDoDraw(boolean b) {
        this.doDraw = b;
    }

    /**
     * Sets the palette scheme
     */
    public void SetPaletteScheme(int paletteScheme) {
        this.paletteScheme = paletteScheme;
    }

    /**
     * Sets the label interval
     */
    public void SetLabelInterval(double labelInterval) {
        this.labelInterval = labelInterval;
    }

    /**
     * Sets the values at the blue and red limits
     */
    public void SetLimits(double valueBlue, double valueRed) {
        this.valueBlue = valueBlue;
        this.valueRed = valueRed;
        valueRange = Math.abs(valueRed - valueBlue);
        oneThirdOfValueRange = valueRange/3;
        twoThirdsOfValueRange = 2*valueRange/3;
    }

    /**
     * Sets the width and height of the color scale for drawing on screen
     */
    public void SetScaleDimensions(int width, int height) {
        this.width = width;
        this.height = height;
    }

    /**
     * draw the color scale
     */
    public void DrawScale(Graphics g, int drawingMode) {
        int i;
        if (!doDraw) {
            return;
        }
        
        if (doCustom) {
            drawCustomScale(g);
            return;   
        }
        
        double value, valueMin, valueMax, threshold, midValue;

        // Draw the scale
        valueMin = valueBlue;
        valueMax = valueRed;
        if (valueBlue > valueRed) {
            valueMin = valueRed;
            valueMax = valueBlue;
        }
        threshold = valueMin + colorInterval;
        midValue = valueMin + colorInterval/2;
        for (i=0; i<height; i++) {
            value = i*Math.abs(valueRange)/(height-1) + valueMin;
            if (drawingMode == 
                    mp2PostProcessorView.CONTOUR_DRAWING_MODE) {
                if (value < threshold) {
                    value = midValue;
                }
                else {
                    threshold += colorInterval;
                    midValue += colorInterval;
                    value = midValue;
                }
            }
            g.setColor(GetColor((float) value));
            g.fillRect(0, height-i, width, 1);
        }

        // Draw the color scale labels
        int left = width+10;
        g.setColor(Color.black);
        g.drawString(String.valueOf(valueMax), left, 4);
        g.drawString(String.valueOf(valueMin), left, height+4);
        i = 1;
        double labelValue = valueMin + labelInterval;
        while (labelValue < valueMax-0.05*valueRange) {
            g.drawString(String.valueOf((float) labelValue), left, 
                height+4- (int) ((labelValue - valueMin)/Math.abs(valueRange)*height));
            i++;
            labelValue = valueMin + i*labelInterval;
        }
    }
    
    protected void drawCustomScale(Graphics g) {
        int i;
        int L = height/customColor.length;
        for (i=0; i<customColor.length; i++) {
            g.setColor(customColor[i]);
            g.fillRect(0, height-i*L, width, L);
        }
        int left = width+10;
        g.setColor(Color.black);
        for (i=0; i<customLabel.length; i++) {
            g.drawString(customLabel[i], left, height-i*L + (L/2) + 4);
        }
    }

}
