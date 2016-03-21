/*
 * mp2RectBounds.java
 */
package mp2;

import java.awt.*;
import java.io.*;

/**
 * Encapsulates a rectangle boundary of a shape, in model coordinates.
 */
public class mp2RectBounds implements Serializable, mp2Constants {

    static final long serialVersionUID = -6260181145611379564L;

    public double x;
    public double y;
    public double width;
    public double height;

    public mp2RectBounds() {
        x = 0;
        y = 0;
        width = 0;
        height = 0;
    }

}
