/*
 * mp2Doc.java
 */
package mp2;

import java.io.*;

/**
 * Specifies the behavior of a document, which holds the
 * application data. The document also
 * keeps track of whether or not its data have been changed
 * since the last time the document was saved.
 *
 * <P> The document implements the Serializable interface
 * so it can be stored on file. To properly serialize and
 * deserialize the document, all instance variables of a document
 * must either implement the Serializable interface, or
 * must be declared <code>transient</code>. This requirement
 * extends all the way up the inheritance structure of each
 * instance variable to the topmost superclass.</P>
 *
 * <P>When a document is serialized, the values of all serializable
 * instance variables are stored on file. The values of all
 * transient instance variables are not stored on file.</P>
 *
 * <P>When a document is deserialized, the values of all
 * serializable instance variables are read back from file.
 * All transient variables are left uninitialized. To properly
 * construct a document via deserialization, the <code>init</code>
 * method is called to initialize the transient variables.
 *
 * For the above reason, a document is created by a two-step
 * process. To create a new document, first step is to invoke
 * the constructor, which creates the serializable variables. The
 * second step is to call the <code>init</code> method, which
 * initializes the transient instance variables. To create
 * a document via deserialization, the first step is to read
 * the values of the serializable variables from file. The second
 * step is to call the <code>init</code> method.
 *
 * Because the document interacts closely with the frame and the
 * view, both the frame and the view are also created by
 * a two-step process (first invoking the constructor and then
 * calling the <code>init</code> method. This is necessary even
 * though the view and the frame are themselves not serialized.
 *
 * @see mp2.mp2App
 * @see mp2.mp2View
 */
public abstract class mp2Doc implements mp2Constants,
        Serializable {

    static final long serialVersionUID = 4188303841376280165L;

    /**
     * The current version of this document
     */
    protected static String currentVersion;

    /**
     * The drawing width in inches
     */
    protected double drawingWidthInInches;

    /**
     * The drawing height in inches
     */
    protected double drawingHeightInInches;

    /**
     * The magnification level. A value of 1 means no magnification
     */
    protected double magnification;

    /**
     * The model distance per drawing inch in the x direction
     */
    protected double modelDistancePerInchX;

    /**
     * The model distance per drawing inch in the y direction
     */
    protected double modelDistancePerInchY;

    /**
     * The number of decimal places to show
     */
    protected int numDecimal;

    /**
     * The units (Inches or map units) display on rulers
     */
    protected int rulerUnits;

    /**
     * The version that is serialized with this document.
     */
    protected String serializedVersion;

    /**
     * Distance in inches from left boundary of drawing to
     * coordinate axis origin
     */
    protected double xOriginInInches;

    /**
     * Distance in inches from top edge of drawing to
     * coordinate axis origin
     */
    protected double yOriginInInches;

    /**
     * The document's file name.
     */
    protected transient String fileName;

    /**
     * The document's directory
     */
    protected transient String directory;

    /**
     * The application to which this document belongs.
     */
    protected transient mp2App theApp;

    /**
     * The view that shows the data in this document.
     */
    protected transient mp2View view;

    protected transient double maximumMagnification;

    protected static transient double startupMagnification = 1;

    /**
     * Indicates if the document has been changed since it was
     * last saved. If <code>true</code> then the document has
     * been changed. Otherwise, the document has not been changed.
     * This is a transient variable that is not serialized.
     */
    protected transient boolean isChanged;

    public static final int UNABLE_TO_EXPORT_ERROR = 1;
    public static final int ILLEGAL_FILE_NAME_ERROR = 2;

    /**
     * Constructs a document.
     */
    public mp2Doc() {
        fileName = DEFAULT_FILE_NAME;
        directory = new String();
        drawingWidthInInches = DEFAULT_DRAWING_WIDTH_IN_INCHES;
        drawingHeightInInches = DEFAULT_DRAWING_HEIGHT_IN_INCHES;
        magnification = startupMagnification;
        modelDistancePerInchX = 1;
        modelDistancePerInchY = 1;
        xOriginInInches = 1;
        yOriginInInches = 1;
        numDecimal = 2;
        rulerUnits = mp2Ruler.MODEL_UNITS;
        serializedVersion = "";
        currentVersion = "";
        isChanged = false;
    }

    /**
     * Converts the serialized version to current version
     */
    public void convertToCurrentVersion() {
        serializedVersion = new String(currentVersion);
        isChanged = true;
    }

    /**
     * Export the data in this document.
     *
     */
    public abstract int exportData(String directory, String file);

    public mp2App getApp() {return theApp;}

    /**
     * Gets the current version of this document
     */
    public static String getCurrentVersion() {
        return currentVersion;
    }

    /**
     * Gets the specified data.
     *
     * @param  dataType  the specified data identified by
     *                   an <code>int</code> constant that is
     *                   application specific.
     *
     * @return  the requested data cast to the type
     *          <code>java.lang.Object</code>.
     */
    public abstract Object getData(int dataType);

    /**
     * Gets the document's directory.
     *
     * @return  The document's directory.
     */
    public String getDirectory() {
        return directory;
    }

    public double getDrawingWidthInInches() {
        return drawingWidthInInches;
    }

    public double getDrawingHeightInInches() {
        return drawingHeightInInches;
    }

    /**
     * Gets the document's file name.
     *
     * @return  The document's file name.
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * Gets the magnification.
     *
     * @return  the amount of magnification
     */
    public double getMagnification() {
        return magnification;
    }

    public double getModelDistancePerInchX() {
        return modelDistancePerInchX;
    }

    public double getModelDistancePerInchY() {
        return modelDistancePerInchY;
    }

    public void setModelDistancePerInchX(double x) {
        modelDistancePerInchX = x;
        double scaleX =  PIXELS_PER_INCH * magnification * view.getScreenDistortionX();
        double scaleY =  PIXELS_PER_INCH * magnification * view.getScreenDistortionY();
        view.setModelDistancePerPixel(
                modelDistancePerInchX / scaleX,
                modelDistancePerInchY / scaleY);
    }

    public void setModelDistancePerInchY(double y) {
        modelDistancePerInchY = y;
        double scaleX =  PIXELS_PER_INCH * magnification * view.getScreenDistortionX();
        double scaleY =  PIXELS_PER_INCH * magnification * view.getScreenDistortionY();
        view.setModelDistancePerPixel(
                modelDistancePerInchX / scaleX,
                modelDistancePerInchY / scaleY);
    }

    public int getNumberOfDecimalPlacesToShow() {
        return numDecimal;
    }

    /**
     * Gets the serialized version of this document. If this
     * document is created from deserialization, this contains
     * the version of the document as saved on file.
     */
    public String getSerializedVersion() {
        return serializedVersion;
    }


    /**
     * Gets the units on the ruler
     *
     * @return  an <code>int</code> value representing
     *          the units on the ruler
     */
    public int getRulerUnits() {
        return rulerUnits;
    }

    /**
     * Gets the document's view
     *
     * @return  the view.
     */
    public mp2View getView() {
        return view;
    }

    public double getXOriginInInches() {
        return xOriginInInches;
    }

    public double getYOriginInInches() {
        return yOriginInInches;
    }

    /**
     * Indicates if the document has been changed since it was
     * last saved.
     *
     * @return  <code>true</code> is the document has been changed
     *         since it was last saved. <code>false</code> otherwise.
     */
    public boolean isChanged() {
        return isChanged;
    }

    /**
     * Initializes the document
     *
     * @param  theApp  the application object to which this
     *                 document belongs.
     */
    public void init(mp2App theApp) {
        this.theApp = theApp;
        view = theApp.getView();

        setViewSizeAndScale();
        mp2DecimalFormat.setFractionDigits(numDecimal);
        mp2DecimalFormat.setGroupingUsed(false);
        maximumMagnification = MAXIMUM_DRAWING_DIMENSION_IN_PIXELS/
                (Math.max(drawingWidthInInches, drawingHeightInInches)
                    * PIXELS_PER_INCH);
    }

    /**
     * Edits drawing size and scale by displaying dialog box.
     * If user clicked ok, then update the view size and scales and
     * return true;
     */
    public boolean editDrawingOptions() {

        mp2DrawingOptionsDialog dlg = new mp2DrawingOptionsDialog();
        dlg.drawingWidth      = drawingWidthInInches;
        dlg.drawingHeight     = drawingHeightInInches;
        dlg.distanceXPerInch  = modelDistancePerInchX;
        dlg.distanceYPerInch  = modelDistancePerInchY;
        dlg.xOriginInInches   = xOriginInInches;
        dlg.yOriginInInches   = yOriginInInches;
        dlg.numDecimal        = numDecimal;
        dlg.rulerUnits = rulerUnits;

        if (dlg.doModal() == true) {
            drawingWidthInInches  = dlg.drawingWidth;
            drawingHeightInInches = dlg.drawingHeight;
            modelDistancePerInchX = dlg.distanceXPerInch;
            modelDistancePerInchY = dlg.distanceYPerInch;
            xOriginInInches = dlg.xOriginInInches;
            yOriginInInches = dlg.yOriginInInches;
            numDecimal = dlg.numDecimal;
            rulerUnits = dlg.rulerUnits;
            mp2DecimalFormat.setFractionDigits(numDecimal);
            maximumMagnification = MAXIMUM_DRAWING_DIMENSION_IN_PIXELS/
                    (Math.max(drawingWidthInInches, drawingHeightInInches)
                        * PIXELS_PER_INCH);
            if (magnification > maximumMagnification) {
                magnification = maximumMagnification;
            }
            view.setScreenDistortionX(1.0);
            view.setScreenDistortionY(1.0);
            setViewSizeAndScale();
            setChanged(true);
            view.noUndoableDataChanged();
            // Rulers will be reset by frame when this method returns
            return true;
        } else {
            return false;
        }
    }

    /**
     * Indicates if the data in this document is sufficient for
     * exporting.
     *
     * @return  <code>true(/code> if there are sufficient data for
     *          exporting. <code>false</code> otherwise.
     */
    public abstract boolean readyToExport();

    /**
     * Marks this document as changed or not changed according to
     * the argument b.
     *
     * @param  b  If <code>true</code>, mark this document as changed
     *            since it was last saved. If <code>false</code>,
     *            mark this document as unchanged since it was last
     *            saved.
     */
    public void setChanged(boolean b) {
        // Set the document change flag;
        isChanged = b;

        // If the document is changed, set the frame title
        // with "*" appended to the end, and disallow exporting
        if (isChanged) {
            theApp.getFrame().setTitle(theApp.getFrameTitle() +
                                       ": " + fileName+ " *");
        }

        // If the document is unchanged, set the frame title
        // without the "*"appended to the end, and allow exporting.
        else {
            theApp.getFrame().setTitle(theApp.getFrameTitle() + ": "
                                       + fileName);
        }
    }

    public void setChangedWithNoUndo() {
        setChanged(true);
        view.noUndoableDataChanged();
    }

    public void setChangedAndClearUndo() {
        setChanged(true);
        view.clearUndoQueue();
    }

    /**
     * Set the document's directory
     *
     * @param  directory  the <code>String</code> containing the
     *                    directory.
     */
    public void setDirectory(String directory) {
        this.directory = directory;
    }

    /**
     * Set the document's file name
     *
     * @param  fileName  the <code>String</code> containing the
     *                   file name.
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    /**
     * Sets the size and scale of the view.
     * The interface with the view needs to be cleaned up due to the
     * introduction of screen distortion in the x and y direction.
     * The revised interface should have the view figure out its
     * own pixel dimensions based on the screen distortion. The
     * doc will pass the measurements in inches and the view will
     * do the rest.
     */
    protected void setViewSizeAndScale() {
        double scaleX =  PIXELS_PER_INCH * magnification * view.getScreenDistortionX();
        double scaleY =  PIXELS_PER_INCH * magnification * view.getScreenDistortionY();
        view.setDrawingSizeInPixels(
                (int) Math.round(drawingWidthInInches  * scaleX),
                (int) Math.round(drawingHeightInInches * scaleY));
        view.setModelDistancePerPixel(
                modelDistancePerInchX / scaleX,
                modelDistancePerInchY / scaleY);
        view.setOriginInPixels(
                (int) Math.round(xOriginInInches * scaleX) ,
                (int) Math.round(yOriginInInches * scaleY));
    }

    /**
     * Magnifies the view by a preset amount.
     */
    public boolean zoomIn() {
        if (magnification*ZOOM_FACTOR > maximumMagnification) {
            return false;
        }
        magnification *= ZOOM_FACTOR;
        setViewSizeAndScale();
        return true;
    }

    /**
     * Shrinks the view by a preset amount.
     */
    public void zoomOut() {
        magnification /= ZOOM_FACTOR;
        setViewSizeAndScale();
    }

    public boolean setMagnification(double magnification) {
        if (magnification > maximumMagnification) {
            return false;
        }
        this.magnification = magnification;
        setViewSizeAndScale();
        return true;
    }

    public static void setStartupMagnification(double v) {
        startupMagnification = v;
    }

    public static double getStartupMagnification() {
        return startupMagnification;
    }
}
