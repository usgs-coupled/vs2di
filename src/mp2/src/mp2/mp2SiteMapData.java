/*
 * mp2SiteMapData.java
 */
package mp2;

import java.io.*;
import java.awt.*;
import javax.swing.*;

/**
 * Encapsulates site map grid data. This class holds data 
 * that defines a graphic site map.
 *
 * <P>This class implements the Serializable interface so
 * objects can be serialized and deserialized. Therefore,
 * object creation is a two-step procedure in which
 * the <code>init</code> method is called after construction
 * or deserialization. This is similar to how document objects
 * are created.
 *
 * @see mp2.mp2Doc
 */
public class mp2SiteMapData extends mp2GraphicalData
                    implements Serializable, mp2Constants {
 
    static final long serialVersionUID = 7291890122208766666L;

    protected String imageFileName;
    protected mp2RectBounds bounds;
    protected double [] fixedPoint;
    protected transient Image image;

    /**
     * Constructor
     */
    public mp2SiteMapData() {
        imageFileName = new String();
        bounds = new mp2RectBounds();
        fixedPoint = new double[2];
    }

    /**
     * Delete image
     */
    public void deleteImage() {
        image = null;
        imageFileName = new String();
        doc.setChanged(true);
        bounds.x = 0;
        bounds.y = 0;
        bounds.width = 0;
        bounds.height = 0;
    }

    /**
     * Gets the outer bounds (in model coordinates)
     */
    public mp2RectBounds getBounds() {
        return bounds;
    }

    /**
     * Gets the outer bounds (in model coordinates)
     */
    public double [] getFixedPoint() {
        return fixedPoint;
    }

    /**
     * Gets the site map image
     */
    public Image getImage() {
        return image;
    }

    /**
     * Gets the image file name
     */
    public String getImageFileName() {
        return imageFileName;
    }

    /**
     * initializes the image
     */
    public void init(mp2Doc doc) {
        super.init(doc);
        if (imageFileName.length() == 0) {
            return;
        }
        // If the image file name is not empty, then
        // this object was created via deserialization.
        // So load the image file.

        // First check if the file exists
        File file = new File(imageFileName);
        if (!file.exists()) {
            String [] message = new String[2];
            message[0] = "Unable to open file " + imageFileName +".";
            message[1] = "Do you want to locate this file yourself?";
            int result = mp2MessageBox.showYesNoDialog(message, "Error");
            if (result == mp2MessageBox.NO_OPTION) {
                imageFileName = new String();
                image = null;
                bounds.x = 0;
                bounds.y = 0;
                bounds.width = 0;
                bounds.height = 0;
                mp2MessageBox.showMessageDialog("Site map will not be loaded.", "Warning");
                doc.setChanged(true);
                return;
            } else {
                mp2FileChooser fc = new mp2FileChooser();
                mp2FileFilter filter = new mp2FileFilter();
                filter.addExtension("gif");
                filter.addExtension("jpg");
                filter.addExtension("png");
                filter.setDescription("Image File (*.gif, *.jpg, *png)");
                fc.addChoosableFileFilter(filter);
                fc.setDialogTitle("Open Site Map File");
                if (doc.getFileName().equals(DEFAULT_FILE_NAME)) {
                    fc.setCurrentDirectory(new File(doc.getApp().getCurrentDirectory()));
                } else {
                    fc.setCurrentDirectory(new File(doc.getDirectory()));
                }
                if (fc.showOpenDialog(doc.getApp().getFrame()) == mp2FileChooser.APPROVE_OPTION) {
                    imageFileName = fc.getSelectedFile().getPath();
                    doc.getApp().setCurrentDirectory(fc.getCurrentDirectory().getPath());
                } else {                    imageFileName = new String();
                    image = null;
                    bounds.x = 0;
                    bounds.y = 0;
                    bounds.width = 0;
                    bounds.height = 0;
                    mp2MessageBox.showMessageDialog("Site map will not be loaded.", "Warning");
                    doc.setChanged(true);
                    return;
                }
                doc.setChanged(true);
            }
        }

        // Read the image from the file
        Image img;
        try { 
            img = Toolkit.getDefaultToolkit().getImage(imageFileName);
            // use a media tracker to make sure the image is fully loaded
            MediaTracker tracker = new MediaTracker(doc.getView());
            tracker.addImage(img, 0);
            tracker.waitForAll();
        } catch (Exception e) {
            imageFileName = new String();
            image = null;
            bounds.x = 0;
            bounds.y = 0;
            bounds.width = 0;
            bounds.height = 0;
            mp2MessageBox.showMessageDialog("Unable to load site map.", 
                                            "Error");
            return;
        }

        // Make sure the image exists
        if (img == null) {
            imageFileName = new String();
            image = null;
            bounds.x = 0;
            bounds.y = 0;
            bounds.width = 0;
            bounds.height = 0;
            mp2MessageBox.showMessageDialog("Unable to load site map.", 
                                            "Error");
            return;
        }
        image = img;

    }

    /**
     * Indicates whether or not the site map has been defined
     */
    public boolean isDefined() {
        return (image != null);
    }

    /**
     * Sets the bounds of the map in model coordinates
     */
    public void setFixedPoint(double x, double y) {
        fixedPoint[0] = x;
        fixedPoint[1] = y;
        doc.setChanged(true);
    }

    /**
     * Sets the bounds of the map in model coordinates
     */
    public void setBounds(double x, double y, double width,
                                              double height) {
        bounds.x = x;
        bounds.y = y;
        bounds.width = width;
        bounds.height = height;
        doc.setChanged(true);
    }

    /**
     * Sets the image
     */
    public void setImage(Image image) {
        this.image = image;
        doc.setChanged(true);
    }

    /**
     * Sets the file name that contains the image
     */
    public void setImageFileName(String fileName) {
        imageFileName = fileName;
        doc.setChanged(true);
    }
}
