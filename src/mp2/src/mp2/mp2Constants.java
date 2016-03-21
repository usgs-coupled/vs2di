/*
 * mp2Constants.java
 */
package mp2;

import java.awt.*;

/**
 * Defines constants common to classes in the mp2 package
 */
public interface mp2Constants {

    /**
     * The default file name of a new document.
     */
    public static final String DEFAULT_FILE_NAME = "Untitled";

    /**
     * The screen resolution in dots or pixels per inch 
     */
    public static final int PIXELS_PER_INCH = 
                Toolkit.getDefaultToolkit().getScreenResolution();

    /**
     * The default width of the drawing
     */
    public static final double DEFAULT_DRAWING_WIDTH_IN_INCHES = 10;

    /**
     * The default height of the drawing
     */
    public static final double DEFAULT_DRAWING_HEIGHT_IN_INCHES = 8;

    public static final int MAXIMUM_DRAWING_DIMENSION_IN_PIXELS = 10000000;
    /**
     * The default foreground color of a data view.
     */
    public static final Color DEFAULT_FOREGROUND_COLOR = Color.black;

    /**
     * The default background color of a data view.
     */
    public static final Color DEFAULT_BACKGROUND_COLOR = Color.white;

    /**
     * The distance tolerance (in pixels) within which two points
     * on the screen are considered coincident.
     */
    public static final int DISTANCE_TOLERANCE = 4;

    /**
     * The size of a figure handle, in pixels..
     */
    public static final int HANDLE_SIZE = 7;

    /**
     * The size of a figure half-handle, in pixels. .
     */
    public static final int HALF_HANDLE_SIZE = HANDLE_SIZE/2;

    /**
     * The amount of magnification (or shrinkage) when zooming in
     * (or zooming out)
     */
    public static final double ZOOM_FACTOR = 1.5;
    
    public static final int MAXIMUM_IMAGE_WIDTH = 2000;
    public static final int MAXIMUM_IMAGE_HEIGHT = 1600;

    public final static int GRID_FILE_TOOL  = 0;
    public final static int LINE_TOOL       = 1;
    public final static int POINT_TOOL      = 2;
    public final static int POINT_FILE_TOOL = 3;
    public final static int POLYGON_TOOL    = 4;
    public final static int RECTANGLE_TOOL  = 5;
    
    public static final int VECTOR_AS_VELOCITY = 1;
    public static final int VECTOR_AS_FLUX = 2;

    
    public static final int FILE_MENU = 1000;
    public static final int EDIT_MENU = 2000;
    public static final int OPTIONS_MENU = 3000;
    public static final int SHOW_MENU = 4000;
    public static final int MODEL_MENU = 5000;
    public static final int HELP_MENU = 9000;

    public static final int DRAWING_OPTIONS = 3001;
    public static final int MODEL_OPTIONS = 3002;
    public static final int POST_PROCESSOR_OPTIONS = 3003;
    public static final int BUFFERED_GRAPHICS = 3004;
    
    public static final int DOMAIN = 4001;
    public static final int BOUNDARY_CONDITIONS = 4002;
    public static final int OBSERVATION_POINTS = 4003;
    public static final int MODEL_GRID = 4004;
    public static final int SITE_MAP = 4005;
    public static final int POST_PROCESSOR = 4006;

}
