/*
 * mp2BufferedShapesView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public abstract class mp2BufferedShapesView extends mp2ShapesView {
    
    protected static boolean imageBufferingEnabled;
    protected static Image image;  //Memory image buffer shared by all subclasses
    protected static boolean remakeImage;
	protected int imageX;
	protected int imageY;
	protected int imageWidth;
	protected int imageHeight;
	protected int maxImageWidth = MAXIMUM_IMAGE_WIDTH;
	protected int maxImageHeight = MAXIMUM_IMAGE_HEIGHT;
    protected int gridX;
    protected int gridY;
    protected int gridWidth;
    protected int gridHeight;
    
    public mp2BufferedShapesView(mp2View view, 
                mp2ShapesData shapesData,
                mp2AbstractGridData gridData,
                String homeDirectory) {
        super(view, shapesData, gridData, homeDirectory);
        remakeImage = true;
        imageBufferingEnabled = false;
    }

    public boolean isImageBufferingEnabled() {
        return imageBufferingEnabled;
    }
    
    /**
     * Creates an Image object for drawing the discretized view
     */
    protected void makeImage() throws OutOfMemoryError {
        Point p1, p2;
        double [] bounds = gridData.getBounds();

        if (view.getModelVerticalAxisOrientation() ==
                mp2Drawing.MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE) {
            p1 = view.modelToView(bounds[0], bounds[1]);
            p2 = view.modelToView(bounds[2], bounds[3]);
        } else {
            p1 = view.modelToView(bounds[0], bounds[3]);
            p2 = view.modelToView(bounds[2], bounds[1]);
        }
        
        gridX = p1.x;
        gridY = p1.y;
        gridWidth = p2.x - gridX + 1;
        gridHeight = p2.y - gridY + 1;

		Rectangle rect = view.getFrame().getScrollPane().getViewport().getViewRect();
		int viewportX = rect.x;
		int viewportY = rect.y;
		int viewportWidth = rect.width;
		int viewportHeight = rect.height;

        int i, j;
		if (gridWidth > maxImageWidth) {
			imageX = Math.max(gridX, viewportX - (maxImageWidth-viewportWidth)/2);
			imageWidth = maxImageWidth;
		} else {
            imageX = gridX;
			imageWidth = gridWidth;
		}
		if (gridHeight > maxImageHeight) {
			imageY = Math.max(gridY, viewportY - (maxImageHeight-viewportHeight)/2);
			imageHeight = maxImageHeight;
		} else {
            imageY = gridY;
			imageHeight = gridHeight;
		}

        boolean makeNewImage = false;
        if (image != null) {
            if ((image.getWidth(view) != imageWidth) ||
                (image.getHeight(view) != imageHeight)) {
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
                image = view.createImage(imageWidth, imageHeight);
            } catch (OutOfMemoryError e) {
                remakeImage = true;
                throw e;
            }
        }

        Graphics g = image.getGraphics();
        g.setColor(Color.white);
        g.fillRect(0, 0, imageWidth, imageHeight);
        g.translate(-imageX, -imageY);
        drawDiscretizedShapes(g);
        g.dispose();
        remakeImage = false;
    }

    protected abstract void drawDiscretizedShapes(Graphics g);
    
    protected int [] getDrawingLimitsForRectilinearGrid(Graphics g) {
        double [] xCoord = ((mp2RectilinearGridData) gridData).getXCoords();
        double [] yCoord = ((mp2RectilinearGridData) gridData).getYCoords();
        int numCol = xCoord.length - 1;
        int numRow = yCoord.length - 1;
        int istart = 0;
        int iend = numCol;
        int jstart = 0;
        int jend = numRow;
        int i, j;
        if (imageBufferingEnabled && !view.isPrinting()) {
            Point p1, p2;
            int imageRight = imageX + imageWidth;
            for (i=1; i<=numCol; i++) {
                p1 = view.modelToView(xCoord[i], yCoord[0]);
                if (p1.x > imageX) {
                    istart = i-1;
                    break;
                }
                istart = numCol;
            }
            for (i=numCol-1; i>=0; i--) {
                p1 = view.modelToView(xCoord[i], yCoord[0]);
                if (p1.x < imageRight) {
                    iend = i+1;
                    break;
                }
                iend = istart;
            }
            int imageBottom = imageY + imageHeight;
            if (view.getModelVerticalAxisOrientation() ==
                    mp2Drawing.MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE) {
                for (j=1; j<=numRow; j++) {
                    p1 = view.modelToView(xCoord[0], yCoord[j]);
                    if (p1.y > imageY) {
                        jstart = j-1;
                        break;
                    }
                    jstart = numRow;
                }
                for (j=numRow-1; j>=0; j--) {
                    p1 = view.modelToView(xCoord[0], yCoord[j]);
                    if (p1.y < imageBottom) {
                        jend = j+1;
                        break;
                    }
                    jend = jstart;
                }
            } else {
                for (j=1; j<=numRow; j++) {
                    p1 = view.modelToView(xCoord[0], yCoord[j]);
                    if (p1.y < imageBottom) {
                        jstart = j-1;
                        break;
                    }
                    jstart = numRow;
                }
                for (j=numRow-1; j>=0; j--) {
                    p1 = view.modelToView(xCoord[0], yCoord[j]);
                    if (p1.y > imageY) {
                        jend = j+1;
                        break;
                    }
                    jend = jstart;
                }
            }
        } else {
            Point p1, p2;
		    Rectangle rect = g.getClipBounds();
		    int clipLeft = rect.x;
		    int clipTop = rect.y;
		    int clipRight = rect.x + rect.width;
		    int clipBottom = rect.y + rect.height;
            for (i=1; i<=numCol; i++) {
                p1 = view.modelToView(xCoord[i], yCoord[0]);
                if (p1.x > clipLeft) {
                    istart = i-1;
                    break;
                }
                istart = numCol;
            }
            for (i=numCol-1; i>=0; i--) {
                p1 = view.modelToView(xCoord[i], yCoord[0]);
                if (p1.x < clipRight) {
                    iend = i+1;
                    break;
                }
                iend = istart;
            }
            if (view.getModelVerticalAxisOrientation() ==
                    mp2Drawing.MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE) {
                for (j=1; j<=numRow; j++) {
                    p1 = view.modelToView(xCoord[0], yCoord[j]);
                    if (p1.y > clipTop) {
                        jstart = j-1;
                        break;
                    }
                    jstart = numRow;
                }
                for (j=numRow-1; j>=0; j--) {
                    p1 = view.modelToView(xCoord[0], yCoord[j]);
                    if (p1.y < clipBottom) {
                        jend = j+1;
                        break;
                    }
                    jend = jstart;
                }
            } else {
                for (j=1; j<=numRow; j++) {
                    p1 = view.modelToView(xCoord[0], yCoord[j]);
                    if (p1.y < clipBottom) {
                        jstart = j-1;
                        break;
                    }
                    jstart = numRow;
                }
                for (j=numRow-1; j>=0; j--) {
                    p1 = view.modelToView(xCoord[0], yCoord[j]);
                    if (p1.y > clipTop) {
                        jend = j+1;
                        break;
                    }
                    jend = jstart;
                }
            }
        }
        int [] limits = new int[4];
        limits[0] = istart;
        limits[1] = iend;
        limits[2] = jstart;
        limits[3] = jend;
        return limits;
    }
    
    /**
     * Indicates that the image needs to be recreated
     */
    public static void remakeImage() {
        remakeImage = true;
    }

    protected void paintDiscrete(Graphics g) {
        if (imageBufferingEnabled && !view.isPrinting()) {
		    Rectangle rect = g.getClipBounds();
		    int clipX = rect.x;
		    int clipY = rect.y;
            int clipRight = clipX + rect.width;
            int clipBottom = clipY + rect.height;
            int imageRight = imageX + imageWidth;
            int imageBottom = imageY + imageHeight;
            if (shapesData.discretize() || remakeImage
                || ((imageX > gridX) && (clipX < imageX))
                || ((imageY > gridY) && (clipY < imageY))
				|| ((imageRight < gridX + gridWidth) && (clipRight > imageRight))
				|| ((imageBottom < gridY + gridHeight) && (clipBottom > imageBottom))) {
                try {
                    makeImage();
                } catch (OutOfMemoryError e) {
                    // Hopefully this won't happen!
                    System.out.println("Out of memory");
                    return;
                }
            }
            g.drawImage(image, imageX, imageY, view);
        } else {
            shapesData.discretize();
            drawDiscretizedShapes(g);
        }
    }
    
    public static void setImageBufferingEnabled(boolean b) {
        imageBufferingEnabled = b;
        if (b) {
            remakeImage = true;
        }
        else if (image != null) {
            image.flush();
            image = null;
            Runtime.getRuntime().gc();
        }
    }
}
