/*
 * mp2View.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;

/**
 * Defines the behavior of the application's view, which
 * manages the application's graphical data and table data.
 * (Here, "Managing the data" means editing the data and
 * displaying the data on screen). The major responsibilities 
 * of the application's view are listed as follows.
 *
 * <P> 1. In general, graphical data and table data are divided 
 * into different types. Examples of graphical data are 
 * domain and initial conditions. Examples of table
 * data are textural classes and recharge periods. Each
 * type of graphical data is managed by a corresponding 
 * "data view" object. Similarily, each type of table data is 
 * managed by a corresponding "table window" object. 
 * The view class holds all the data views objects and 
 * table window objects. </P>
 *
 * <P> 2. At any time when the application is running, one
 * data view can be designated as "active", while all
 * other data views are "Inactive". The active data view is 
 * selected from the dataChooser (drop-list box) in the frame. 
 * The view keeps track of the current active data view using the
 * variable <code>activeDataView</code>. </P>
 *
 * <P> 3. Graphical data managed by the active data view 
 * can be edited by mouse clicks and and movements. The view 
 * routes all mouse events to the active view. It is up to 
 * the active data view to respond to the mouse events.</P>
 *
 * <P> 4. The view paints the data views by calling the data
 * view's <code>paint</code> method. If a data view manages
 * a discretizable data object, then it can be painted in two 
 * possible modes--"continuous" and "discrete".
 * The view keeps track of the current painting mode by using
 * the boolean variable <code>paintDiscrete</code>.</P>
 *
 * <P>Note that creating an application view is a two-step process. 
 * The first step is to invoke the constructor.
 * The second step is to call the <code>init</code> method.</P>
 *
 * @see mp2.mp2App
 * @see mp2.mp2Frame
 * @see mp2.mp2Doc
 */
public abstract class mp2View extends mp2Drawing implements mp2Constants,
        MouseListener, MouseMotionListener {

    /**
     * Indicates the active data view.
     */
    protected mp2GraphicalDataView activeDataView;

    /**
     * The document whose data are displayed in this view
     */
    protected mp2Doc doc;

    /**
     * Indicates whether or not to use XOR mode when drawing.
     * If <code>true</code>, drawing is in XOR mode.
     * Otherwise, drawing is not in XOR mode.
     */
    protected boolean drawInXORMode = false;

    /**
     * The the main frame window in which this view is displayed
     */
    protected mp2Frame frame;
    
    protected Graphics graphics;
    protected int px0;
    protected int py0;

    /**
     * The label of the horizontal axis, generally either "x" or "r".
     */
    protected static String horizontalAxisName;

    /**
     * Indicates whether or not the editing capabilities of this
     * view are locked.
     */
    protected boolean isEditable;

    /**
     * Indicates whether or not the left mouse button is down.
     * This instance variable is need to overcome an awt bug
     * that occasionally sends a mouse move event instead
     * of a mouse dragged event when dragging begins.
     */
    private boolean mouseDown;
    
    protected Point mouseDownPoint;
    
    protected boolean shiftDown;

    /**
     * Indicates whether or not the current paint mode is "discrete".
     * If <code>true</code> current paint mode is "discrete".
     * Otherwise, current point mode is "continuous".
     */
    protected boolean paintDiscrete;

    /**
     * The application to which this view belongs
     */
    protected mp2App theApp;

    /**
     * The label of the vertical axis. generally either "y" or "z"
     */
    protected static String verticalAxisName;

    /**
     * The "XOR" button
     */
    protected mp2ToggleButton XORButton;
    
    protected boolean zoomActive;
    
    protected int bitmapResolution = 0;

    /**
     * Create a view
     */
    public mp2View() {
        setFont(new Font ("Dialog", Font.PLAIN, 12));
        horizontalAxisName = "x";
        verticalAxisName = "z";
        paintDiscrete = false;
        addMouseListener(this);
        addMouseMotionListener(this);
        mouseDown = false;
        shiftDown = false;
        zoomActive = false;
    }

    /**
     * Indicates whether or not the drawing contains the specified 
     * point on the screen.
     *
     * @param  p  the screen coordinates of the point.
     */
    public boolean contains(Point p) {
        return (p.x < drawingWidthInPixels && p.y < drawingHeightInPixels);
    }

    /**
     * Disposes of the resources used by this class
     */
    public abstract void dispose();

    /**
     * Invoked when the document is saved. The active data view
     * is notified so that it could reset the redo queue.
     */
    public void docSaved() {
        activeDataView.docSaved();
    }

    /**
     * Gets the doc displayed by this view
     */
    public mp2Doc getDoc() {
        return doc;
    }

    /**
     * Gets the frame that contains this view
     */
    public mp2Frame getFrame() {
        return frame;
    }

    public static String getHorizontalAxisName() {
        return horizontalAxisName;
    }
    
    public static String getVerticalAxisName() {
        return verticalAxisName;
    }
    
    /**
     * Get the visible portion of this view
     */
    public Rectangle getVisibleRect() {
        return frame.getScrollPane().getViewport().getViewRect();
    }

    /**
     * Gets the "XOR" Button
     */
    public mp2ToggleButton getXORButton() {
        return XORButton;
    }

    /**
     * Indicates whether or not to use XOR mode for drawing.
     *
     * @return <code>true</code> if XOR mode should be used for
     *         drawing. <code>false</code> otherwise.
     */
    public boolean getXORMode() {
        return drawInXORMode;
    }

    /**
     * Indicates whether or not a site map has been defined.
     */
    public boolean hasSiteMap() {
        mp2SiteMapData siteMapData = (mp2SiteMapData) doc.getData(SITE_MAP);
        if (siteMapData != null && siteMapData.isDefined()) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Initializes this view
     *
     * @param  theApp  the application object to which this
     *                 view belongs.
     */
    public void init(mp2App theApp) {
        this.theApp = theApp;
        frame = theApp.getFrame();
        doc = theApp.getDoc();
        isEditable = true;

        // Create the zoom in and zoom out buttons
        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = theApp.getHomeDirectory() + fileSeparator
                                + fileSeparator + "images" + fileSeparator;
        XORButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "xor.gif"));

        XORButton.setToolTipText("XOR mode");
        XORButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onXOR();
            }
        });
    }

    /**
     * Indicates whether or not the paint mode is discrete
     *
     * @return  <code>true</code> if the paint mode is currently
     *          set to discrete, <code>false</code> otherwise.
     */
    public boolean isPaintModeDiscrete() {
        return paintDiscrete;
    }

    /**
     * Invoked when a mouse button is clicked on the view. The
     * MouseEvent is passed to the active view, if it is defined.
     */
    public void mouseClicked(MouseEvent e) {
        if (activeDataView != null && isEditable && !zoomActive) {
            activeDataView.onMouseClicked(e);
        }
    }

    /**
     * Invoked when when a mouse button is pressed on the view 
     * and then moved. The cursor coordinates are updated, and
     * the MouseEvent is passed to the active view, if it is 
     * defined.
     */
    public void mouseDragged(MouseEvent e) {
        Point p = e.getPoint();
        double [] x = viewToModel(p);
        frame.setStatusLabelText(horizontalAxisName
                 + " = " +  mp2DecimalFormat.format(x[0])
                 + ", "+ verticalAxisName + " = " +  
                mp2DecimalFormat.format(x[1]));
        if (activeDataView != null) {
            if (zoomActive) {
                if (shiftDown) {
                    graphics.drawLine(mouseDownPoint.x, mouseDownPoint.y,
                                        px0, py0);
                    if (Math.abs(mouseDownPoint.x - p.x) >
                            Math.abs(mouseDownPoint.y - p.y)) {
                        px0 = p.x;
                        py0 = mouseDownPoint.y;
                    } else {
                        px0 = mouseDownPoint.x;
                        py0 = p.y;
                    }
                    graphics.drawLine(mouseDownPoint.x, mouseDownPoint.y,
                                        px0, py0);
                } else {
                    graphics.drawRect(
                        Math.min(mouseDownPoint.x, px0),
                        Math.min(mouseDownPoint.y, py0),
                        Math.abs(px0-mouseDownPoint.x), 
                        Math.abs(py0-mouseDownPoint.y));
                    px0 = p.x;
                    py0 = p.y;
                    graphics.drawRect(
                        Math.min(mouseDownPoint.x, px0),
                        Math.min(mouseDownPoint.y, py0),
                        Math.abs(px0-mouseDownPoint.x), 
                        Math.abs(py0-mouseDownPoint.y));
                }
            } else if (isEditable) {
                activeDataView.onMouseDragged(e);
            }
        }
    }

    /**
     * Invoked when the mouse enters the view. The MouseEvent
     * is passed to the active view, if it is defined.
     */
    public void mouseEntered(MouseEvent e) {
        if (activeDataView != null && isEditable && !zoomActive) {
            activeDataView.onMouseEntered(e);
        }
    }

    /**
     * Invoked when the mouse exits the view. The MouseEvent
     * is passed to the active view, if it is defined.
     */
    public void mouseExited(MouseEvent e) {
        frame.setStatusLabelText("Ready");
        if (activeDataView != null && isEditable && !zoomActive) {
            activeDataView.onMouseExited(e);
        }
    }

    /**
     * Invoked when when the mouse has moved on the view (with 
     * no buttons down). The cursor coordinates are updated, and
     * the MouseEvent is passed to the active view, if it is 
     * defined.
     */
    public void mouseMoved(MouseEvent e) {
        // The following check is needed due to bug in awt
        if (mouseDown) {
            return;
        }
        Point p = e.getPoint();
        double [] x = viewToModel(p);
        frame.setStatusLabelText(horizontalAxisName
                 + " = " +  mp2DecimalFormat.format(x[0])
                 + ", "+ verticalAxisName + " = " +  
                mp2DecimalFormat.format(x[1]));
        if (activeDataView != null && isEditable && !zoomActive) {
            activeDataView.onMouseMoved(e);
        }
    }

    /**
     * Invoked when when a mouse button is pressed. 
     * If the zoom button is not clicked, the MouseEvent
     * is passed to the active view, if it is defined.
     */
    public void mousePressed(MouseEvent e) {
        // The next line is needed due to bug in awt
        mouseDown = true;
        if (activeDataView != null) {
            if (zoomActive) {
                mouseDownPoint = e.getPoint();
                shiftDown = e.isShiftDown();
                px0 = mouseDownPoint.x;
                py0 = mouseDownPoint.y;
                graphics = getGraphics();
                graphics.setXORMode(Color.white);
                graphics.drawRect(px0, py0, 0, 0);
            } else if (isEditable) {
                activeDataView.onMousePressed(e);
            }
        }
    }

    /**
     * Invoked when when a mouse button is released. The MouseEvent
     * is passed to the active view, if it is defined.
     */
    public void mouseReleased(MouseEvent e) {
        // The next line is needed due to bug in awt
        mouseDown = false;
        if (activeDataView != null) {
            if (zoomActive) {
                Point mouseUpPoint = e.getPoint();
                if (Math.abs(mouseUpPoint.x - mouseDownPoint.x) <= 2 &&
                    Math.abs(mouseUpPoint.y - mouseDownPoint.y) <= 2) {
                    graphics.drawRect(
                        Math.min(mouseDownPoint.x, px0),
                        Math.min(mouseDownPoint.y, py0),
                        Math.abs(px0-mouseDownPoint.x), 
                        Math.abs(py0-mouseDownPoint.y));
                    if (e.isShiftDown()) {
                        // restore to no magnification and no distortion
                        frame.zoom(mouseUpPoint, 0);
                    } else if ((e.getModifiers() & InputEvent.BUTTON3_MASK)
                                                == InputEvent.BUTTON3_MASK) {
                        // Zoom out
                        frame.zoom(mouseUpPoint, -1);
                    } else {
                        // Zoom in
                        frame.zoom(mouseUpPoint, 1);
                    }
                } else if (shiftDown) {
                    graphics.drawLine(mouseDownPoint.x, mouseDownPoint.y,
                                        px0, py0);
                    if (Math.abs(mouseUpPoint.x - mouseDownPoint.x) >
                            Math.abs(mouseUpPoint.y - mouseDownPoint.y)) {
                        // distort in the x direction
                        frame.distortX(mouseUpPoint.x, mouseDownPoint.x, mouseUpPoint);
                    } else {
                        // distort in the y direction
                        frame.distortY(mouseUpPoint.y, mouseDownPoint.y, mouseUpPoint);
                    }
                } else {
                    graphics.drawRect(
                        Math.min(mouseDownPoint.x, px0),
                        Math.min(mouseDownPoint.y, py0),
                        Math.abs(px0-mouseDownPoint.x), 
                        Math.abs(py0-mouseDownPoint.y));
                    // can only drag zoom if drag rectangle is not a line
                    if ((mouseUpPoint.x != mouseDownPoint.x &&
                            mouseUpPoint.y != mouseDownPoint.y)) {
                        frame.dragZoom(mouseDownPoint, mouseUpPoint);
                    }
                }
                graphics.dispose();
                graphics = null;
            } else if (isEditable) {
                activeDataView.onMouseReleased(e);
            }
        }
    }

    /**
     * Invoked when non graphical data (table data or model options)
     * are changed.
     */
    public void noUndoableDataChanged() {
        activeDataView.noUndoableDataChanged();
    }

    public void clearUndoQueue() {
        activeDataView.clearUndoQueue();
    }

    /**
     * Invoked when the "XOR" button is clicked
     */
    public void onXOR() {
        drawInXORMode = XORButton.isSelected();
        repaint ();
    }

    /**
     * Prints the view
     */
    public void print() {
        // Get a print job from the toolkit
        Toolkit toolkit = getToolkit();
        PrintJob printJob;
        try {
            printJob = toolkit.getPrintJob(frame, "Printing", null);
        } catch (Exception e) {
            return;
        }

        // If user cancels, printjob returns null.
        if (printJob == null) {
            return;
        }

        Graphics printG = printJob.getGraphics();
        if (printG == null) {
            printJob.end();
            return;
        }
        isPrinting = true;
        // rescale everything so the page resolution is equal to screen resolution
        // in terms of pixels per inch
        double currentMagnification = doc.getMagnification();
        double pageResolution = (double) printJob.getPageResolution();
        double currentScreenDistortionX = getScreenDistortionX();
        double currentScreenDistortionY = getScreenDistortionY();
        // should revise this to avoid calling doc
        doc.setMagnification(pageResolution/PIXELS_PER_INCH);
        setScreenDistortionX(1);
        setScreenDistortionY(1);
        printG.setClip(0, 0, drawingWidthInPixels, drawingHeightInPixels);  
        paintComponent(printG);
        printG.dispose();
        printJob.end();
        isPrinting = false;
        // should revise this to avoid calling doc
        doc.setMagnification(currentMagnification);
        setScreenDistortionX(currentScreenDistortionX);
        setScreenDistortionY(currentScreenDistortionY);
        repaint();
    }

    /**
     * Save the window display as a bitmap
     */
    public void exportBitmap() {
        mp2BitmapResolutionDialog bmpdlg = new mp2BitmapResolutionDialog();
        bmpdlg.resolution = bitmapResolution;
        if (bmpdlg.doModal() != true) {
            return;
        }
        bitmapResolution = bmpdlg.resolution;
        double height = bmpdlg.height;
        double width = bmpdlg.width;
        bmpdlg = null;

        mp2FileChooser fc = new mp2FileChooser();
        mp2FileFilter filter = new mp2FileFilter();
        filter.addExtension("bmp");
        filter.setDescription("Bitmap File (*.bmp)");
        fc.addChoosableFileFilter(filter);
        fc.setDialogTitle("Export Bitmap");
        fc.setCurrentDirectory(new File(frame.getApp().getCurrentDirectory()));
        if (fc.showSaveDialog(frame) != mp2FileChooser.APPROVE_OPTION) {            return;        }
        String dir = fc.getCurrentDirectory().getPath();
        String bmpFile = fc.getSelectedFile().getName();
        if (!bmpFile.endsWith(".bmp")) {
            bmpFile += ".bmp";
        }
        String file = dir + System.getProperty("file.separator") + bmpFile;
        
        double currentMagnification = doc.getMagnification();
		Rectangle rect = frame.getScrollPane().getViewport().getViewRect();
        int imageX = rect.x;
        int imageY = rect.y;
        int imageWidth = rect.width;
        int imageHeight = rect.height;

        if (bitmapResolution != 0) {
            double mag;
            int res = (bitmapResolution == 1) ? 150 : 300;
            if (width > 0) {
                imageWidth =  (int) (width*res);
                imageHeight = (imageWidth * rect.height)/rect.width;
                mag = imageWidth /(double) rect.width;
            } else {
                imageHeight = (int) (height*res);
                imageWidth = (imageHeight * rect.width) / rect.height;
                mag = imageHeight/(double) rect.height;
            }
            imageX = (int) (rect.x * mag);
            imageY = (int) (rect.y * mag);
            // should revise this to avoid calling doc
            doc.setMagnification(currentMagnification * mag);
            int fontSize = (int) Math.max(12, 12*mag);
            setFont(new Font ("Dialog", Font.PLAIN, fontSize));
        }
        Image image;
        try {
            image = createImage(imageWidth, imageHeight);
        } catch (OutOfMemoryError e) {
            mp2MessageBox.showMessageDialog(frame, 
                        "Not enough memory to create bitmap file.", "Error");
            return;
        }
        Graphics g = image.getGraphics();
        g.setFont(getFont());
        g.setColor(Color.white);
        g.fillRect(0, 0, imageWidth, imageHeight);
        g.translate(-imageX, -imageY);
        g.setClip(imageX, imageY, imageWidth, imageHeight);
        paintComponent(g);
        {
            mp2BMPFile bmp = new mp2BMPFile();
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            bmp.saveBitmap (file, image, imageWidth, imageHeight, bitmapResolution);
            setCursor(Cursor.getDefaultCursor());
        }
        g.dispose();
        g = null;
        image.flush();
        image = null;
        if (bitmapResolution != 0) {
            // should revise this to avoid calling doc
            doc.setMagnification(currentMagnification);
            setFont(new Font ("Dialog", Font.PLAIN, 12));
        }
        frame.getApp().setCurrentDirectory(fc.getCurrentDirectory().getPath());
        Runtime.getRuntime().gc();
    }

    /**
     * Designates the specified data view as active
     *
     * @param  dataViewId  The data view identified by an
     *                     (code>int</code> value that is 
     *                     application specific.
     */
    public abstract void setActiveDataView(int dataViewId);

    /**
     * Sets the name of the horizontal axis
     */
    public static void setHorizontalAxisName(String hName) {
        horizontalAxisName = hName;
    }

    /**
     * Sets the name of the vertical axis
     */
    public static void setVerticalAxisName(String vName) {
        verticalAxisName = vName;
    }

    /**
     * Locks or unlocks the view for editing.
     */
    public void setEditable(boolean b) {
        this.isEditable = b;
    }

    /**
     * Sets the model distance (in model units such as meters) per
     * inch in the x and y directions.
     */
    public void setModelDistancePerPixel(double px, double py) {
        super.setModelDistancePerPixel(px, py);
        mp2BufferedShapesView.remakeImage();
    }

    /**
     * Sets the paint mode to discrete or continuous.
     *
     * @param  b  If <code>true</code> set the paint mode to discrete.
     *            Otherwise set the paint mode to continuous.
     */
    public void setPaintModeToDiscrete(boolean b) {
        paintDiscrete = b;
    }
    
    public boolean setScreenDistortionX(double newScreenDistortionX) {
        if (!super.setScreenDistortionX(newScreenDistortionX)) {
            return false;
        }
        mp2BufferedShapesView.remakeImage();
        return true;
    }
    
    public boolean setScreenDistortionY(double newScreenDistortionY) {
        if (!super.setScreenDistortionY(newScreenDistortionY)) {
            return false;
        }
        mp2BufferedShapesView.remakeImage();
        return true;
    }

    /**
     * Sets whether or not radial coordinates are used.
     *
     * @param  b  if <code>true</code> horizontal axis is labelled
     *            as "r", otherwise, horizontal axis is labelled as "x"
     */
    public void setUseRadialCoordinates(boolean b) {
        if (b) {
            horizontalAxisName = "r";
        } else {
            horizontalAxisName = "x";
        }
    }

    /**
     * Shows or hides the specified data view or table window.
     *
     * @param  item   the item to be shown or hidden, identified
     *                by an <code>int</code> constant that is
     *                application specific.
     *
     * @param  b  If <code>true</code> show the specified item.
     *            Otherwise, hide the specified item.
     */
    public abstract void setVisible(int item, boolean b);
    
    public void setZoomActive(boolean b) {
        zoomActive = b;
        if (b) {
            setCursor(Cursor.getDefaultCursor());
            if (activeDataView != null) {
                activeDataView.deselectAll();
                repaint();
            }
        }
    }
}
