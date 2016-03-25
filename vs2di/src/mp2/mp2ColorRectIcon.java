/*
 * mp2ColorRectIcon.java
 */
package mp2;

import java.awt.*;
import javax.swing.*;

/**
 * Encapsulates a icon that is rectangular in shape and filled
 * with a uniform color. This class implements the <code>Icon
 * </code> interface. An object of this class can be placed
 * in a JTable cell.
 */
public class mp2ColorRectIcon implements Icon {

    /**
     * The color of the rectangular icon
     */
    Color color;

    /**
     * The width of the rectangular icon in pixels
     */
    int width;

    /**
     * The width of the rectangular icon in pixels
     */
    int height;

    /**
     * Creates a new mp2ColorRectIcon of the specified color, 
     * width and height.
     *
     * @param  color  Color of the rectangular icon.
     * @param  width  width of the rectangular icon.
     * @param  height height of the rectangular icon.
     */
    public mp2ColorRectIcon(Color color, int width, int height) {
        this.color = color;
        this.width = width;
        this.height = height;
    }

    /**
     * Gets the width of the icon
     *
     * @return  the width of the icon;
     */
    public int getIconWidth() {
        return width;
    }

    /**
     * Gets the height of the icon
     *
     * @return  the height of the icon;
     */
    public int getIconHeight() { 
        return height;
    }

    /**
     * Draws the icon at the specified location
     *
     * @param  component  Not used. Can be null.
     * @param  g          The Graphics object for drawing
     * @param  x          The <i>x</i> coordinate of the location
     *                    to draw the icon
     * @param  y          The <i>y</i> coordinate of the location
     *                    to draw the icon
     */
    public void paintIcon(Component component, Graphics g, 
            int x, int y) {
        g.setColor(color);
        g.fillRect(x, y, width, height);
        g.setColor(Color.black);
        g.drawRect(x, y, width, height);
    }
}
