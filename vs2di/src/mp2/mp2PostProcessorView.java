/*
 * mp2PostProcessorView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.*;
import javax.swing.*;
import javax.swing.border.*;

public abstract class mp2PostProcessorView extends mp2Drawing 
    implements mp2ColorScaleDialogCaller, MouseListener, MouseMotionListener, mp2Constants {
    
    protected mp2PostProcessorFrame frame;
    protected mp2Model model;
    
    protected static NumberFormat nf = NumberFormat.getInstance();
    protected FontMetrics fontMetrics;
    protected int charHeight;
    protected int display;
    protected int drawingMode;
    protected int bitmapResolution = 0;
    protected boolean showVectors;
    protected boolean zoomActive;
    protected double magnification;
    protected float vectorMagnitudePerPixel;
    protected Point mouseDownPoint;
    protected Graphics graphics;
    protected int px0;
    protected int py0;

    protected mp2ColorScale colorScale;
    protected int colorScaleLeft;
    protected int colorScaleTop;
    protected double colorScaleWidth;   // Use double to maintain precision with zooming
    protected int colorScaleHeight;

    protected int extendedGridWidth;
    protected int extendedGridHeight;
    protected int extendedGridX;
    protected int extendedGridY;
    protected int gridBorder;
    
    protected Image image;
	protected int imageX;
	protected int imageY;
	protected int imageWidth;
	protected int imageHeight;
	protected int maxImageWidth = MAXIMUM_IMAGE_WIDTH;
	protected int maxImageHeight = MAXIMUM_IMAGE_HEIGHT;
    protected boolean imageBufferingEnabled;
    
    public static final int CELL_DRAWING_MODE = 1;
    public static final int CONTOUR_DRAWING_MODE = 2;

    /**
     * Creates a view for the postprocessor
     */
    public mp2PostProcessorView(mp2PostProcessorFrame frame) {

        this.frame = frame;
        setFont(new Font("System", Font.PLAIN, 12));
        fontMetrics = getFontMetrics(getFont());
        charHeight = fontMetrics.getHeight();
        nf.setMaximumFractionDigits(2);
        nf.setMinimumFractionDigits(2);

        addMouseListener(this);
        addMouseMotionListener(this);

        // default settings
        drawingMode = CELL_DRAWING_MODE;
        showVectors = false;
        colorScale = null;
        drawingWidthInPixels = 
                (int) Math.round(DEFAULT_DRAWING_WIDTH_IN_INCHES * 
                PIXELS_PER_INCH);
        drawingHeightInPixels = 
                (int) Math.round(DEFAULT_DRAWING_HEIGHT_IN_INCHES *
                PIXELS_PER_INCH);
        modelDistancePerPixelX = 1.0/PIXELS_PER_INCH;
        modelDistancePerPixelY = 1.0/PIXELS_PER_INCH;
        xOriginInPixels = 0;
        yOriginInPixels = 0;
        colorScaleWidth = 20;
        colorScaleHeight = 200;
        gridBorder = (int) PIXELS_PER_INCH/2;   // half-inch border
        vectorMagnitudePerPixel = (float) (1.0/PIXELS_PER_INCH);
        zoomActive = false;
        imageBufferingEnabled = false;
        model = null;
    }
    
    /**
     * Callback from the color scale dialog
     */
    public void applyColorScale(double valueBlue, double valueRed, 
                                    double colorInterval, double labelInterval) {
        if (colorScale == null) {
            return;
        }
        colorScale.SetLimits(valueBlue, valueRed);
        colorScale.SetColorInterval(colorInterval);
        colorScale.SetLabelInterval(labelInterval);
        if (model.getType() == mp2Model.COMPUTATIONAL) {
            frame.setChanged();
        }
        draw();
    }

    protected void computePixelComponentsForVector(double vx, double vy, int [] pc) {
        if (vx == 0 && vy == 0) {
            pc[0] = 0;
            pc[1] = 0;
            return;
        }
        
        // compute the direction of the vector on the screen
        // Note that the direction is affected by the scale
        // in x and y directions.
        double ux = vx/modelDistancePerPixelX;
        double uy = vy/modelDistancePerPixelY;
        double ulen = Math.sqrt(ux*ux + uy*uy);
        double ex = ux/ulen;
        double ey = uy/ulen;
        
        // compute the pixel components 
        double vlength = Math.sqrt(vx*vx + vy*vy)/vectorMagnitudePerPixel;
        pc[0] = (int) Math.round(vlength * ex);
        pc[1] = (int) Math.round(vlength * ey);
    }

    protected void computePixelComponentsForVector(float vx, float vy, int [] pc) {
        computePixelComponentsForVector((double) vx, (double) vy, pc);
    }
    
    /**
     * Draws the view. 
     */
    public void draw() {
        getDataFromModel();
        if (imageBufferingEnabled) {
            makeImage();
        }
        repaint();
    }

    /**
     * Draws the text
     */
    protected abstract void drawText(Graphics g);
    
    /**
     * Read get data from model here
     */
    protected void getDataFromModel() {}

    /**
     * Draws the model
     */
    protected abstract void drawModel(Graphics g);

    /**
     * Exports this view as a bitmap
     */
    public void exportBitmap() {
        mp2BitmapResolutionDialog bmpdlg = new mp2BitmapResolutionDialog(frame);
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
        if (frame.getWorkingDirectory() != null) {
            fc.setCurrentDirectory(new File(frame.getWorkingDirectory()));
        } else if (frame.getProperties() != null) {
            String directory = frame.getProperties().getProperty("directory");
            if (directory != null && directory.length() > 0) {
                fc.setCurrentDirectory(new File(directory));
            }
        }

        if (fc.showSaveDialog(frame) != mp2FileChooser.APPROVE_OPTION) {            return;        }
        String dir = fc.getCurrentDirectory().getPath();
        String bmpFile = fc.getSelectedFile().getName();
        if (!bmpFile.endsWith(".bmp")) {
            bmpFile += ".bmp";
        }
        String file = dir + System.getProperty("file.separator") + bmpFile;
        
        double currentMagnification = magnification;
        boolean imageBufferingFlag = imageBufferingEnabled;
        setImageBufferingEnabled(false);
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
            int fontSize = (int) Math.max(12, 12*mag);
            setFont(new Font ("Dialog", Font.PLAIN, fontSize));
            fontMetrics = getFontMetrics(getFont());
            resizeContents(currentMagnification * mag);
            // The following is needed because resizeContents creates new buffers
            getDataFromModel();
        }
        
        if (image != null) {
            image.flush();
            image = null;
            Runtime.getRuntime().gc();
        }
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
            setFont(new Font ("Dialog", Font.PLAIN, 12));
            fontMetrics = getFontMetrics(getFont());
            resizeContents(currentMagnification);
            getDataFromModel();
        }
        setImageBufferingEnabled(imageBufferingFlag);
        Runtime.getRuntime().gc();
    }

    /**
     * Makes an in-memory image of the display
     */
    protected void makeImage() {
        Rectangle rect = frame.getScrollPane().getViewport().getViewRect();
		int viewportX = rect.x;
		int viewportY = rect.y;
		int viewportWidth = rect.width;
		int viewportHeight = rect.height;

		if (extendedGridWidth > maxImageWidth) {
			imageX = Math.max(extendedGridX, viewportX - (maxImageWidth-viewportWidth)/2);
			imageWidth = maxImageWidth;
		} else {
            imageX = extendedGridX;
			imageWidth = extendedGridWidth;
		}
		if (extendedGridHeight > maxImageHeight) {
			imageY = Math.max(extendedGridY, viewportY - (maxImageHeight-viewportHeight)/2);
			imageHeight = maxImageHeight;
		} else {
            imageY = extendedGridY;
			imageHeight = extendedGridHeight;
		}
        
        boolean makeNewImage = false;
        if (image != null) {
            if ((image.getWidth(this) != imageWidth) ||
                (image.getHeight(this) != imageHeight)) {
                makeNewImage = true;
                image.flush();
                image = null;
                Runtime.getRuntime().gc();
            }
        } else {
            makeNewImage = true;
        }
        if (makeNewImage) {
            try {
                image = createImage(imageWidth, imageHeight);
            } catch (OutOfMemoryError e) {
                System.out.println("Out of memory in post processor");
            }
        }
        
        // At startup, the image will not be created so we end here.
        if (image == null) {
            return;
        }
        Graphics g = image.getGraphics();
        g.setColor(Color.white);
        g.fillRect(0, 0, imageWidth, imageHeight);
        g.setClip(0, 0, imageWidth, imageHeight);
        g.translate(-imageX, -imageY);
        drawModel(g);
        g.dispose();
    }
    
    /**
     * Invoked when the mouse is clicked
     */
    public void mouseClicked(MouseEvent e) {}

    /**
     * Invoked when the mouse is dragged
     */
    public void mouseDragged(MouseEvent e) {
        Point p = e.getPoint();
        double [] x = viewToModel(p);
        frame.setCoordsLabelText("x = " +  frame.getNumberFormat().format(x[0])
                 + ", z = " + frame.getNumberFormat().format(x[1]));
        if (zoomActive) {
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
    }

    /**
     * Invoked when the mouse enters the view 
     */
    public void mouseEntered(MouseEvent e) {}

    /**
     * Invoked when the mouse exits the view 
     */
    public void mouseExited(MouseEvent e) {
        frame.setCoordsLabelText("");
    }

    /**
     * Invoked when the mouse is moved
     */
    public void mouseMoved(MouseEvent e) {
        Point p = e.getPoint();
        double [] x = viewToModel(p);
        frame.setCoordsLabelText("x = " +  frame.getNumberFormat().format(x[0])
                 + ", z = " + frame.getNumberFormat().format(x[1]));
    }

    /**
     * Invoked when the mouse button is pressed
     */
    public void mousePressed(MouseEvent e) {
        if (zoomActive) {
            mouseDownPoint = e.getPoint();
            px0 = mouseDownPoint.x;
            py0 = mouseDownPoint.y;
            graphics = getGraphics();
            graphics.setXORMode(Color.white);
            graphics.drawRect(px0, py0, 0, 0);
        }    
    }

    /**
     * Invoked when the mouse button is released 
     */
    public void mouseReleased(MouseEvent e) {
        if (zoomActive) {
            Point mouseUpPoint = e.getPoint();
            if (Math.abs(mouseUpPoint.x - mouseDownPoint.x) <= 2 &&
                Math.abs(mouseUpPoint.y - mouseDownPoint.y) <= 2) {
                if (e.isShiftDown()) {
                    frame.zoom(mouseUpPoint, 0);
                } else if ((e.getModifiers() & InputEvent.BUTTON3_MASK)
                                            == InputEvent.BUTTON3_MASK) {
                    frame.zoom(mouseUpPoint, -1);
                } else {
                    frame.zoom(mouseUpPoint, 1);
                }
            } else {
                graphics.drawRect(
                    Math.min(mouseDownPoint.x, px0),
                    Math.min(mouseDownPoint.y, py0),
                    Math.abs(px0-mouseDownPoint.x), 
                    Math.abs(py0-mouseDownPoint.y));
                graphics.dispose();
                graphics = null;
                if (mouseUpPoint.x == mouseDownPoint.x ||
                        mouseUpPoint.y == mouseDownPoint.y) {
                    return;
                }
                frame.zoom(mouseDownPoint, mouseUpPoint);
            }
        }
    }

    /**
     * paints this view
     */
    public void paintComponent(Graphics g) {
        g.setColor(Color.white);
        g.fillRect(0, 0, drawingWidthInPixels, drawingHeightInPixels);
        if (model == null) {
            return;
        }
        if (imageBufferingEnabled && !isPrinting) {
		    Rectangle rect = frame.getScrollPane().getViewport().getViewRect();
		    int viewportX = rect.x;
		    int viewportY = rect.y;
		    int viewportWidth = rect.width;
		    int viewportHeight = rect.height;
            int viewportRight = viewportX + viewportWidth;
            int viewportBottom = viewportY + viewportHeight;
            if (image == null
                || ((viewportX > extendedGridX) && (viewportX < imageX))
                || ((viewportY > extendedGridY) && (viewportY < imageY))
				|| ((viewportRight < extendedGridX + extendedGridWidth) && (viewportRight > imageX + imageWidth))
				|| ((viewportBottom < extendedGridY + extendedGridHeight) && (viewportBottom > imageY + imageHeight))) {
                makeImage();
            }
            if (image != null) {
                g.clipRect(0, 0, drawingWidthInPixels, drawingHeightInPixels);
                g.drawImage(image, imageX, imageY, this);
                g.setClip(rect.x, rect.y, rect.width, rect.height);
            }
        } else {
            Rectangle rect = g.getClipBounds();
            g.clipRect(extendedGridX, extendedGridY, extendedGridWidth, extendedGridHeight);
            drawModel(g);
            g.setClip(rect.x, rect.y, rect.width, rect.height);
        }
        if (colorScale != null) {
            g.translate(colorScaleLeft, colorScaleTop);
            colorScale.DrawScale(g, drawingMode);
            g.translate(-colorScaleLeft, -colorScaleTop);
        }
        drawText(g);
    }

    /**
     * Prints this view on the printer
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

        if (model == null) {
            // prints a blank page
            printJob.end();
            return;
        }

        Graphics printG = printJob.getGraphics();
        if (printG == null) {
            printJob.end();
            return;
        }
        isPrinting = true;
        double currentMagnification = magnification;
        // rescale everything so the page resolution is equal to screen resolution
        // in terms of pixels per inch
        double pageResolution = (double) printJob.getPageResolution();
        resizeContents(pageResolution/PIXELS_PER_INCH);
        getDataFromModel();
        printG.setClip(0, 0, drawingWidthInPixels, drawingHeightInPixels);
        paintComponent(printG);
        printG.dispose();
        printJob.end();
        isPrinting = false;
        resizeContents(currentMagnification);
        getDataFromModel();
        repaint();
    }

    /**
     * Resizes the items to be drawn
     */
    public void resizeContents(double magnification) {
        this.magnification = magnification;
        double scale =  PIXELS_PER_INCH * magnification;
        setDrawingSizeInPixels(
                (int) Math.round(model.getDrawingWidthInInches()  * scale),
                (int) Math.round(model.getDrawingHeightInInches() * scale));
        setModelDistancePerPixel(
                model.getModelDistancePerInchX() / scale,
                model.getModelDistancePerInchY() / scale);
        setOriginInPixels(
                (int) Math.round(model.getXOriginInInches() * scale) ,
                (int) Math.round(model.getYOriginInInches() * scale));
        gridBorder = (int) scale/2;
        vectorMagnitudePerPixel = (float) (model.getVectorMagnitudePerInch() / scale);
    }
    
    /**
     * Sets the color scales
     */
    public abstract void setColorScales(mp2ColorScale [] colorScale);

    /**
     * Sets the type of model output to display
     */
    public void setDisplay(int d) {
        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        display = d;
        draw();
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

    /**
     * Sets the drawing model to cells
     */
    public void setDrawingModeToCells() {
        drawingMode = CELL_DRAWING_MODE;
        draw();
    }

    /**
     * Sets the drawing mode to contours
     */
    public void setDrawingModeToContours() {
        drawingMode = CONTOUR_DRAWING_MODE;
        draw();
    }

    /**
     * Tunrs image buffering on or off
     */
    public void setImageBufferingEnabled(boolean b) {
        imageBufferingEnabled = b;
        if (!b && image != null) {
            image.flush();
            image = null;
            Runtime.getRuntime().gc();
        }
    }

    /**
     * Sets the model
     */
    public void setModel(mp2Model model) {
        this.model = model;
    }

    /**
     * Sets the scale for the vectors
     */
    public void setVectorMagnitudePerPixel(float vp) {
        vectorMagnitudePerPixel = vp;
    }
    
    /**
     * Activates of inactivates the zoom capability
     */
    public void setZoomActive(boolean b) {
        zoomActive = b;
    }
    
    /**
     * Shows the color scale dialog for user to set color limits.
     */
    public void showColorScaleDialog() {
        mp2ColorScaleDialog dlg = new mp2ColorScaleDialog(frame, this);
        dlg.valueRed = colorScale.GetValueRed();
        dlg.valueBlue = colorScale.GetValueBlue();
        dlg.colorInterval = colorScale.GetColorInterval();
        dlg.labelInterval = colorScale.GetLabelInterval();
        if (dlg.doModal() == true) {
            applyColorScale(dlg.valueBlue, dlg.valueRed, 
                        dlg.colorInterval, dlg.labelInterval);
        }
    }

    /**
     * Turns vector display on or off.
     */
    public void showVectors(boolean b) {
        showVectors = b;
        draw();
    }
}