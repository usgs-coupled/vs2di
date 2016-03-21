/*
 * mp2SiteMapView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.undo.*;

/**
 * Manages a site map
 */
public class mp2SiteMapView extends mp2GraphicalDataView 
                                            implements mp2Constants {
    protected mp2SiteMapData siteMapData;
    protected JMenuItem deleteMenuItem;
    protected mp2ToggleButton selectAndEditButton;
    protected mp2ToggleButton fixedPointResizeButton;
    protected mp2Button loadSiteMapButton;
    protected int activeMode;
    protected boolean hasChanged;
    protected boolean mouseOverMap;
    protected boolean mouseOverFixedPoint;
    protected int capturedHandle;
    protected Point oldPoint;
    protected MapRect mapRect;
    protected Graphics tempG;
    protected String siteMapName = "Site Map";

    protected static final int VOID_HANDLE = -1;
    protected static final int NW_HANDLE =  0;
    protected static final int N_HANDLE  =  1;
    protected static final int NE_HANDLE =  2;
    protected static final int E_HANDLE  =  3;
    protected static final int SE_HANDLE =  4;
    protected static final int S_HANDLE  =  5;
    protected static final int SW_HANDLE =  6;
    protected static final int W_HANDLE  =  7;

    protected static final int SELECT_AND_EDIT = 0;
    protected static final int FIXED_POINT_RESIZE = 1;

    /**
     * Creates a new mp2SiteMapView
     */
    public mp2SiteMapView (mp2View view, mp2SiteMapData data, 
                                                String homeDirectory)  {
        super(view, homeDirectory);
        this.siteMapData = data;
        mapRect = new MapRect();

        deleteMenuItem = new JMenuItem("Delete");
        deleteMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
        deleteMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDelete();
            }
        });

        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator + 
                                            "images" + fileSeparator;

        selectAndEditButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "arrow.gif"), true);
        selectAndEditButton.setToolTipText("Move or resize the map");
        selectAndEditButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(SELECT_AND_EDIT);
                }
            }
        });
        fixedPointResizeButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "fixedpoint.gif"), true);
        fixedPointResizeButton.setToolTipText("Resize with fixed point");
        fixedPointResizeButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(FIXED_POINT_RESIZE);
                }
            }
        });
        ButtonGroup bg = new ButtonGroup();
        bg.add(selectAndEditButton);
        bg.add(fixedPointResizeButton);
        bg.add(zoomButton);

        loadSiteMapButton = new mp2Button(new ImageIcon(
                                    imageDirectory + "sitemap.gif"));
        loadSiteMapButton.setToolTipText("Load site map");
        loadSiteMapButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onLoadSiteMap();
            }
        });
    }

    public void deselectAll() {
        mapRect.isSelected = false;
    }

    /**
     * Deletes the site map
     */
    protected void onDelete() {
        if (!mapRect.isSelected || !siteMapData.isDefined()) {
            return;
        }
        mp2RectBounds bounds = siteMapData.getBounds();
        undoSupport.postEdit(new DeleteEdit(siteMapData.getImageFileName(),
                siteMapData.getImage(), bounds.x, bounds.y, bounds.width,
                bounds.height, undoCount));
        undoCount++;
        siteMapData.deleteImage();
        deleteMenuItem.setEnabled(false);
        mapRect.isSelected = false;
        selectAndEditButton.setSelected(true);
        fixedPointResizeButton.setEnabled(false);
        showXORButton(false);
        view.setCursor(Cursor.getDefaultCursor());
        view.repaint();
    }

    /**
     * Loads the site map
     */
    protected void onLoadSiteMap() {

        mp2FileChooser fc = new mp2FileChooser();
        mp2FileFilter filter = new mp2FileFilter();
        filter.addExtension("bmp");
        filter.addExtension("gif");
        filter.setDescription("Image File (*.bmp, *.gif)");
        fc.addChoosableFileFilter(filter);
        fc.setDialogTitle("Load " + siteMapName);
        fc.setCurrentDirectory(new File(view.getFrame().getApp().getCurrentDirectory()));
        if (fc.showOpenDialog(view.getFrame()) != mp2FileChooser.APPROVE_OPTION) {
            return;
        }
        
        // Check that the file exists
        String fileName = fc.getSelectedFile().getPath();
        File file = new File(fileName);
        if (!file.exists()) {
            String [] message = new String[2];
            message[0] = "Unable to open file " + fileName +".";
            message[1] = "Site map will not be loaded.";
            mp2MessageBox.showMessageDialog(message, "Error");
            return;
        }

        // Read the image
        Image image = null;
        try {
            image = Toolkit.getDefaultToolkit().getImage(fileName);
            // use a media tracker to make sure the image is fully loaded
            MediaTracker tracker = new MediaTracker(view);
            tracker.addImage(image, 0);
            tracker.waitForAll();
        } catch (Exception e) {
            mp2MessageBox.showMessageDialog("Unable to load site map.", 
                                            "Error");
            return;
        }

        // Make sure the image exists
        if (image == null) {
            mp2MessageBox.showMessageDialog("Unable to load site map.", 
                                            "Error");
            return;
        }

        // Compute bounds and initial fixed point
        double [] v = view.viewToModel(new Point(0, 0));
        double [] w = view.viewToModel(new Point(image.getWidth(view), 
                                                 image.getHeight(view)));
        double width = w[0] - v[0];
        double height = w[1] - v[1];
        double fixedPointX = v[0] + (w[0] - v[0])/2;
        double fixedPointY = v[1] + (w[1] - v[1])/2;

        // save the old data for undo
        Image oldImage = siteMapData.getImage();
        String oldFileName = siteMapData.getImageFileName();
        mp2RectBounds oldBounds = siteMapData.getBounds();
        double [] oldFixedPoint = siteMapData.getFixedPoint();
        undoSupport.postEdit(new LoadSiteMapEdit(image, fileName,
                v[0], v[1], width, height, fixedPointX, fixedPointY,
                oldImage, oldFileName, oldBounds.x, oldBounds.y, 
                oldBounds.width, oldBounds.height,
                oldFixedPoint[0], oldFixedPoint[1], undoCount));
        undoCount++;

        // Send data to siteMapData
        siteMapData.setImageFileName(fileName);
        siteMapData.setImage(image);
        siteMapData.setBounds(v[0], v[1], width, height);
        siteMapData.setFixedPoint(fixedPointX, fixedPointY);                                   
        mapRect.updateHandles();
        selectAndEditButton.setSelected(true);
        fixedPointResizeButton.setEnabled(true);
        showXORButton(true);
        view.repaint();
        view.getFrame().getApp().setCurrentDirectory(fc.getCurrentDirectory().getPath());
    }

    protected void showXORButton(boolean b) {
        JToolBar toolBar = view.getFrame().getToolBar();
        if (b) {
            if (toolBar.getComponentCount() == 7) {
                return;
            }
            toolBar.add(view.getXORButton());
        } else {
            if (toolBar.getComponentCount() == 6) {
                return;
            }
            toolBar.remove(view.getXORButton());
        }
        view.getXORButton().setSelected(false);
        toolBar.revalidate();
        toolBar.repaint();
    }

    /**
     * Handle mouse dragged
     */
    public void onMouseDragged(MouseEvent e) {

        if (!siteMapData.isDefined()) {
            return;
        }

        Point p = e.getPoint();
        // Resize (normal or with fixed point)
        if (capturedHandle != VOID_HANDLE) {
            // Erase the previous shape by drawing over it, except
            // first time around
            if (hasChanged) {
                mapRect.draw(tempG);
            } else {
                hasChanged = true;
            }
            // Erase the previous handles by drawing over them
            mapRect.drawHandles(tempG);

            // Resize the map outline (normal or with fixed point)
            mapRect.resize(capturedHandle, p);

            // Draw the new map outline and handles
            mapRect.draw(tempG);
            mapRect.drawHandles(tempG);
            return;
        }
        // Move the map
        if (mouseOverMap) {
            // Erase the previous rect by drawing over it. Skip this
            // the first time around.
            if (hasChanged) {
                mapRect.draw(tempG);
            } else {
                // Select the map (and draw handles) if the map is
                // not already selected,
                if (!mapRect.isSelected) {
                    mapRect.drawHandles(tempG);
                    mapRect.isSelected = true;
                }
                hasChanged = true;
            }
            // Translate the map outline.
            mapRect.moveX(p.x - oldPoint.x);
            mapRect.moveY(p.y - oldPoint.y);

            // Draw the new map outline
            mapRect.draw(tempG);
            // Save the cursor position
            oldPoint = p;
            return;
        }
        // Move the fixed point
        if (mouseOverFixedPoint) {
            if (hasChanged) {
                tempG.drawOval(oldPoint.x - HALF_HANDLE_SIZE - 2,
                       oldPoint.y - HALF_HANDLE_SIZE - 2,
                       HANDLE_SIZE+3, HANDLE_SIZE+3);
            } else {
                hasChanged = true;
            }
            // keep the fixed point inside the map
            int limit = mapRect.handle[0].x + HANDLE_SIZE;
            if (p.x < limit) {
                p.x = limit;
            }
            limit = mapRect.handle[2].x - HANDLE_SIZE;
            if (p.x > limit) {
                p.x = limit;
            }
            limit = mapRect.handle[0].y + HANDLE_SIZE;
            if (p.y < limit) {
                p.y = limit;
            }
            limit = mapRect.handle[5].y - HANDLE_SIZE;
            if (p.y > limit) {
                p.y = limit;
            }
            // draw the new position
            tempG.drawOval(p.x - HALF_HANDLE_SIZE - 2,
                           p.y - HALF_HANDLE_SIZE - 2,
                           HANDLE_SIZE+3, HANDLE_SIZE+3);
            oldPoint = p;
        }
    }

    /**
     * Invoked when the mouse has moved on the view (with no
     * buttons down). Overrides the default method in the superclass.
     */
    public void onMouseMoved(MouseEvent e) {

        if (!siteMapData.isDefined()) {
            return;
        }

        Point p = e.getPoint();

        // First, set default values
        mouseOverMap = false;
        capturedHandle = VOID_HANDLE;
        // Check for handle capture.
        if (mapRect.isSelected) {
            capturedHandle = mapRect.getHandleUnderMouse(p);
            if (capturedHandle != VOID_HANDLE) {
                return;
            }
        }
        if (activeMode == FIXED_POINT_RESIZE) {
            mouseOverFixedPoint = mp2Math.withinNeighborhood(p, 
                    view.modelToView(siteMapData.getFixedPoint()),
                    HANDLE_SIZE);
            if (mouseOverFixedPoint) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.CROSSHAIR_CURSOR));
            } else {
                view.setCursor(Cursor.getDefaultCursor());
            }
            return;
        }

        // Check if mouse is over map
        if (activeMode == SELECT_AND_EDIT && mapRect.contains(p)) {
            mouseOverMap = true;
            view.setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
            return;
        }
        // If none of the above, then handle is not captured, and
        // mouse is not over map.
        else {
            view.setCursor(Cursor.getDefaultCursor());
            return;
        }
    }

    /**
     * Handle mouse pressed
     */
    public void onMousePressed (MouseEvent e) {
        if (!siteMapData.isDefined()) {
            return;
        }
        Point p = e.getPoint();
        if (capturedHandle != VOID_HANDLE || mouseOverMap
                            || mouseOverFixedPoint) {
            tempG = view.getGraphics();
            tempG.setColor(Color.black);
            tempG.setXORMode(Color.white);
            Rectangle viewRect = view.getVisibleRect();
            tempG.clipRect(viewRect.x, viewRect.y, viewRect.width, 
                                                   viewRect.height);
            oldPoint = p;
            hasChanged = false;
        }
    }

    /**
     * Handle mouse released
     */
    public void onMouseReleased(MouseEvent e) {
        if (!siteMapData.isDefined()) {
            return;
        }
        if (activeMode == SELECT_AND_EDIT) {
            // Finish resize or moving the map
            if (capturedHandle != VOID_HANDLE || mouseOverMap) {
                if (hasChanged) {
                    double [] v = view.viewToModel(mapRect.handle[0]);
                    double [] w = view.viewToModel(mapRect.handle[7]);
                    double newWidth = w[0] - v[0];
                    double newHeight = w[1] - v[1];
                    mp2RectBounds oldBounds = siteMapData.getBounds();
                    double [] oldFixedPoint = siteMapData.getFixedPoint();
                    double newFixedPointX = v[0] + 
                            (oldFixedPoint[0] - oldBounds.x) * newWidth /
                            oldBounds.width;
                    double newFixedPointY = v[1] +
                            (oldFixedPoint[1] - oldBounds.y) * newHeight /
                            oldBounds.height;
                    undoSupport.postEdit(
                            new ResizeOrMoveEdit(v[0], v[1], newWidth, newHeight,
                            newFixedPointX, newFixedPointY,
                            oldBounds.x, oldBounds.y, oldBounds.width, oldBounds.height,
                            oldFixedPoint[0], oldFixedPoint[1], undoCount));
                    undoCount++;
                    siteMapData.setBounds(v[0], v[1], newWidth, newHeight);
                    siteMapData.setFixedPoint(newFixedPointX, newFixedPointY);
                } else {
                    mapRect.isSelected = true;    // selects the map
                }
                deleteMenuItem.setEnabled(true);
            }
            // if no captured handle and mouse not over map, then deselect the map.
            else {
                mapRect.isSelected = false;
                deleteMenuItem.setEnabled(false);
            }
        }
        if (activeMode == FIXED_POINT_RESIZE) {

            // Finish moving the fixed point
            if (mouseOverFixedPoint) {
                if (hasChanged) {
                    double [] v = view.viewToModel(oldPoint);
                    double [] oldFixedPoint = siteMapData.getFixedPoint();
                    undoSupport.postEdit(new MoveFixedPointEdit(v[0], v[1], 
                            oldFixedPoint[0], oldFixedPoint[1], undoCount));
                    undoCount++;
                    siteMapData.setFixedPoint(v[0], v[1]);
                }
            }
            // Finish resize with fixed point
            if (capturedHandle != VOID_HANDLE) {
                if (hasChanged) {
                    double [] v = view.viewToModel(mapRect.handle[0]);
                    double [] w = view.viewToModel(mapRect.handle[7]);
                    double newWidth = w[0] - v[0];
                    double newHeight = w[1] - v[1];
                    mp2RectBounds oldBounds = siteMapData.getBounds();
                    undoSupport.postEdit(
                            new FixedPointResizeEdit(v[0], v[1], newWidth, newHeight,
                            oldBounds.x, oldBounds.y, oldBounds.width, oldBounds.height,
                            undoCount));
                    undoCount++;
                    siteMapData.setBounds(v[0], v[1], newWidth, newHeight);
                }
            }
        }
        if (tempG != null) {
            tempG.dispose();
            tempG = null;
        }
        onMouseMoved(e);    // checks for cursor style
        view.repaint();
   }

    /**
     * Paints the site map
     */
    public void paint(Graphics g) {
        if (!isActive) {
            mapRect.isSelected = false;
        }

        if (!isVisible || !siteMapData.isDefined()) {
            return;
        }
        Image image = siteMapData.getImage();
        if (image == null) {
            return;
        }
        mp2RectBounds bounds = siteMapData.getBounds();
        Point p1 = view.modelToView(bounds.x, bounds.y);
        Point p2 = view.modelToView(bounds.x + bounds.width,
                                    bounds.y + bounds.height);
        int imageWidth = image.getWidth(view);
        int imageHeight = image.getHeight(view);
        double fx = imageWidth/(double) (p2.x - p1.x);
        double fy = imageHeight/(double) (p2.y - p1.y);

        Rectangle rect = view.getFrame().getScrollPane().getViewport().getViewRect();
        int rectRight = rect.x + rect.width;
        int rectBottom = rect.y + rect.height;
        int dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2;
        if (p1.x > rect.x) {
            dx1 = p1.x;
            sx1 = 0;
        } else {
            dx1 = rect.x;
            sx1 = (int) Math.round((rect.x - p1.x)*fx);
        }
        if (p1.y > rect.y) {
            dy1 = p1.y;
            sy1 = 0;
        } else {
            dy1 = rect.y;
            sy1 = (int) Math.round((rect.y - p1.y)*fy);
        }
        if (p2.x < rectRight) {
            dx2 = p2.x;
            sx2 = imageWidth;
        } else {
            dx2 = rectRight;
            sx2 = (int) Math.round((rectRight - p1.x)*fx);
        }
        if (p2.y < rectBottom) {
            dy2 = p2.y;
            sy2 = imageHeight;
        } else {
            dy2 = rectBottom;
            sy2 = (int) Math.round((rectBottom - p1.y)*fy);
        }
        g.drawImage(image, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, view);
    }

    /**
     * Paints the handles
     */
    public void paintLast(Graphics g) {
        Color oldColor = g.getColor();
        mapRect.updateHandles();   // in case of zoom
        g.setColor(Color.black);
        if (mapRect.isSelected) {
            mapRect.drawHandles(g);
        }
        if (activeMode == FIXED_POINT_RESIZE) {
            Point fp = view.modelToView(
                        siteMapData.getFixedPoint());
            g.setColor(Color.red);
            g.drawOval(fp.x - HALF_HANDLE_SIZE-2,
                       fp.y - HALF_HANDLE_SIZE-2,
                       HANDLE_SIZE+3, HANDLE_SIZE+3);
            g.fillArc(fp.x - HALF_HANDLE_SIZE-2,
                       fp.y - HALF_HANDLE_SIZE-2,
                       HANDLE_SIZE+4, HANDLE_SIZE+4, 0, 90);
            g.fillArc(fp.x - HALF_HANDLE_SIZE-2,
                       fp.y - HALF_HANDLE_SIZE-2,
                       HANDLE_SIZE+4, HANDLE_SIZE+4, 180, 90);

        }
        g.setColor(oldColor);
    }

    /**
     * Loads the toolbar buttons and edit menu
     */
    public void prepareToActivate() {
        super.prepareToActivate();

        JMenu editMenu = view.getFrame().getMenu(EDIT_MENU);
        editMenu.removeAll();
        editMenu.add(undoMenuItem);
        editMenu.add(redoMenuItem);
        editMenu.addSeparator();
        editMenu.add(deleteMenuItem);
        deleteMenuItem.setEnabled(false);
        
        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(selectAndEditButton);
        toolBar.add(fixedPointResizeButton);
        toolBar.add(zoomButton);
        toolBar.add(Box.createVerticalStrut(10));
        toolBar.add(loadSiteMapButton);
        toolBar.add(Box.createVerticalStrut(10));
        if (siteMapData.isDefined()) {
            showXORButton(true);
        }

        selectAndEditButton.setSelected(true);
        fixedPointResizeButton.setEnabled(siteMapData.isDefined());
        // The paint mode for the site map is always continuous.
        view.setPaintModeToDiscrete(false);
    }

    /**
     * Sets the active model
     */
    protected void setActiveModeTo(int mode) {
        activeMode = mode;
        if  (mode == FIXED_POINT_RESIZE) {
            mapRect.isSelected = true;
            deleteMenuItem.setEnabled(true);
        } else {
            mapRect.isSelected = false;
            deleteMenuItem.setEnabled(false);
        }
        view.repaint();
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        super.setEditable(b);
        loadSiteMapButton.setEnabled(b);
    }

    public void setSiteMapName(String name) {
        siteMapName = name;
    }

    /**
     * Encapsulates the rectangular outline of the map 
     * using screen coordinates.
     */
    class MapRect {
        boolean isSelected;
        Point [] handle;
        Point fp;

        MapRect() {
            isSelected = false;
            handle = new Point [8];
            for (int i=0; i<8; i++) {
                handle[i] = new Point(-1, -1);
            }
            fp = new Point();
        }

        boolean contains(Point p) {
            return (p.x > handle[0].x && p.x < handle[7].x
                 && p.y > handle[0].y && p.y < handle[7].y);
        }

        void draw(Graphics g) {
            g.drawRect(getX(), getY(), getWidth(), getHeight());
        }

        void drawHandles(Graphics g) {
            for (int i=0; i<8; i++) {
                g.fillRect(handle[i].x - HALF_HANDLE_SIZE,
                           handle[i].y - HALF_HANDLE_SIZE,
                           HANDLE_SIZE, HANDLE_SIZE);
            }
        }

        int getHandleUnderMouse(Point p) {

            if (mp2Math.withinNeighborhood(p, handle[3], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.W_RESIZE_CURSOR));
                return W_HANDLE;
            }
            if (mp2Math.withinNeighborhood(p, handle[0], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.NW_RESIZE_CURSOR));
                return NW_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[5], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.SW_RESIZE_CURSOR));
                return SW_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[1], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.N_RESIZE_CURSOR));
                return N_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[2], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.NE_RESIZE_CURSOR));
                return NE_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[4], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.E_RESIZE_CURSOR));
                return E_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[6], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.S_RESIZE_CURSOR));
                return S_HANDLE;
            }
            else if (mp2Math.withinNeighborhood(p, handle[7], HANDLE_SIZE)) {
                view.setCursor(Cursor.getPredefinedCursor(
                        Cursor.SE_RESIZE_CURSOR));
                return SE_HANDLE;
            }
            else {
                return VOID_HANDLE;
            }
        }

        int getX() {
            return handle[0].x;
        }

        int getY() {
            return handle[0].y;
        }

        int getWidth() {
            return handle[7].x - handle[0].x;
        }

        int getHeight() {
            return handle[7].y - handle[0].y;
        }

        void moveX(int dx) {
            for (int i=0; i<8; i++) {
                handle[i].x += dx;
            }
        }

        void moveY(int dy) {
            for (int i=0; i<8; i++) {
                handle[i].y += dy;
            }
        }

        void resize(int capturedHandle, Point p) {

            double [] fp = siteMapData.getFixedPoint();
            mp2RectBounds bounds = siteMapData.getBounds();

            // resize north
            if (capturedHandle == NW_HANDLE || capturedHandle == N_HANDLE
                    || capturedHandle == NE_HANDLE) {
                handle[0].y = p.y;
                handle[1].y = p.y;
                handle[2].y = p.y;
                if (activeMode == FIXED_POINT_RESIZE) {
                    double [] h0 = view.viewToModel(handle[0]);
                    double h5y = fp[1] + (bounds.y + bounds.height - fp[1]) *
                            (fp[1] - h0[1])  / (fp[1] - bounds.y);
                    handle[5].y = view.modelToView(bounds.x, h5y).y;
                    handle[6].y = handle[5].y;
                    handle[7].y = handle[5].y;
                }
                handle[3].y = (handle[0].y + handle[5].y)/2;
                handle[4].y = handle[3].y;
            }
            // resize east
            if (capturedHandle == NE_HANDLE || capturedHandle == E_HANDLE
                    || capturedHandle == SE_HANDLE) {
                handle[2].x = p.x;
                handle[4].x = p.x;
                handle[7].x = p.x;
                if (activeMode == FIXED_POINT_RESIZE) {
                    double [] h2 = view.viewToModel(handle[2]);
                    double h0x = fp[0] - (fp[0] - bounds.x) *
                            (h2[0] - fp[0])/(bounds.x + bounds.width - fp[0]);
                    handle[0].x = view.modelToView(h0x, bounds.y).x;
                    handle[3].x = handle[0].x;
                    handle[5].x = handle[0].x;
                }
                handle[1].x = (handle[0].x + handle[2].x)/2;
                handle[6].x = handle[1].x;
            }
            // resize south
            if (capturedHandle == SE_HANDLE || capturedHandle == S_HANDLE
                    || capturedHandle == SW_HANDLE) {
                handle[5].y = p.y;
                handle[6].y = p.y;
                handle[7].y = p.y;
                if (activeMode == FIXED_POINT_RESIZE) {
                    double [] h5 = view.viewToModel(handle[5]);
                    double h0y = fp[1] - (fp[1] - bounds.y) *
                            (h5[1] - fp[1])/(bounds.y + bounds.height - fp[1]);
                    handle[0].y = view.modelToView(bounds.x, h0y).y;
                    handle[1].y = handle[0].y;
                    handle[2].y = handle[0].y;
                }
                handle[3].y = (handle[0].y + handle[5].y)/2;
                handle[4].y = handle[3].y;
            }
            // resize west
            if (capturedHandle == SW_HANDLE || capturedHandle == W_HANDLE
                    || capturedHandle == NW_HANDLE) {
                handle[0].x = p.x;
                handle[3].x = p.x;
                handle[5].x = p.x;
                if (activeMode == FIXED_POINT_RESIZE) {
                    double [] h0 = view.viewToModel(handle[0]);
                    double h2x = fp[0] + (bounds.x + bounds.width - fp[0]) *
                            (fp[0] - h0[0])/(fp[0] - bounds.x);
                    handle[2].x = view.modelToView(h2x, bounds.y).x;
                    handle[4].x = handle[2].x;
                    handle[7].x = handle[2].x;
                }
                handle[1].x = (handle[0].x + handle[2].x)/2;
                handle[6].x = handle[1].x;
            }
        }

        void updateHandles() {
            mp2RectBounds bounds = siteMapData.getBounds();

            Point p1 = view.modelToView(bounds.x, bounds.y);
            Point p2 = view.modelToView(bounds.x + bounds.width,
                                        bounds.y + bounds.height);
            handle[0].x = handle[3].x = handle[5].x = p1.x;
            handle[1].x = handle[6].x = (p1.x + p2.x) / 2;
            handle[2].x = handle[4].x = handle[7].x = p2.x;
            handle[0].y = handle[1].y = handle[2].y = p1.y;
            handle[3].y = handle[4].y = (p1.y + p2.y) / 2;
            handle[5].y = handle[6].y = handle[7].y = p2.y;
        }
    }

    /**
     * The undoable edit to load the site map
     */
    protected class LoadSiteMapEdit extends AbstractDataEdit {

        protected Image oldImage, newImage;
        protected String oldImageFileName, newImageFileName;
        protected double newX, newY, newWidth, newHeight,
                         newFixedPointX, newFixedPointY,
                         oldX, oldY, oldWidth, oldHeight,
                         oldFixedPointX, oldFixedPointY;

        public LoadSiteMapEdit(Image newImage, String newImageFileName,
                double newX, double newY, double newWidth, double newHeight,
                double newFixedPointX, double newFixedPointY,
                Image oldImage, String oldImageFileName,
                double oldX, double oldY, double oldWidth, double oldHeight, 
                double oldFixedPointX, double oldFixedPointY, long undoIndex) {
            super(undoIndex);
            this.newImage = newImage;
            this.newImageFileName = newImageFileName;
            this.newX = newX;
            this.newY = newY;
            this.newWidth = newWidth;
            this.newHeight = newHeight;
            this.newFixedPointX = newFixedPointX;
            this.newFixedPointY = newFixedPointY;
            this.oldImage = oldImage;
            this.oldImageFileName = oldImageFileName;
            this.oldX = oldX;
            this.oldY = oldY;
            this.oldWidth = oldWidth;
            this.oldHeight = oldHeight;
            this.oldFixedPointX = oldFixedPointX;
            this.oldFixedPointY = oldFixedPointY;
        }

        public void undo() throws CannotUndoException {
            siteMapData.setImage(oldImage);
            siteMapData.setImageFileName(oldImageFileName);
            siteMapData.setBounds(oldX, oldY, oldWidth, oldHeight);
            siteMapData.setFixedPoint(oldFixedPointX, oldFixedPointY);
            selectAndEditButton.setSelected(true);
            fixedPointResizeButton.setEnabled(oldImage != null);
            showXORButton(oldImage != null);
            view.setCursor(Cursor.getDefaultCursor());
            mapRect.updateHandles();
            super.undo();
        }

        public void redo() throws CannotRedoException {
            siteMapData.setImage(newImage);
            siteMapData.setImageFileName(newImageFileName);
            siteMapData.setBounds(newX, newY, newWidth, newHeight);
            siteMapData.setFixedPoint(newFixedPointX, newFixedPointY);
            selectAndEditButton.setSelected(true);
            fixedPointResizeButton.setEnabled(true);
            showXORButton(true);
            mapRect.updateHandles();
            super.redo();
        }
    }


    /**
     * The undoable edit to resize the site map
     */
    protected class ResizeOrMoveEdit extends AbstractDataEdit {

        protected double newX, newY, newWidth, newHeight,
                         newFixedPointX, newFixedPointY,
                         oldX, oldY, oldWidth, oldHeight,
                         oldFixedPointX, oldFixedPointY;

        public ResizeOrMoveEdit(double newX, double newY,
                        double newWidth, double newHeight,
                        double newFixedPointX, double newFixedPointY,
                        double oldX, double oldY,
                        double oldWidth, double oldHeight, 
                        double oldFixedPointX, double oldFixedPointY,
                        long undoIndex) {
            super(undoIndex);
            this.newX = newX;
            this.newY = newY;
            this.newWidth = newWidth;
            this.newHeight = newHeight;
            this.newFixedPointX = newFixedPointX;
            this.newFixedPointY = newFixedPointY;
            this.oldX = oldX;
            this.oldY = oldY;
            this.oldWidth = oldWidth;
            this.oldHeight = oldHeight;
            this.oldFixedPointX = oldFixedPointX;
            this.oldFixedPointY = oldFixedPointY;
        }

        public void undo() throws CannotUndoException {
            siteMapData.setBounds(oldX, oldY, oldWidth, oldHeight);
            siteMapData.setFixedPoint(oldFixedPointX, oldFixedPointY);
            selectAndEditButton.setSelected(true);
            mapRect.updateHandles();
            super.undo();
        }

        public void redo() throws CannotRedoException {
            siteMapData.setBounds(newX, newY, newWidth, newHeight);
            siteMapData.setFixedPoint(newFixedPointX, newFixedPointY);
            selectAndEditButton.setSelected(true);
            mapRect.updateHandles();
            super.redo();
        }
    }

    /**
     * The undoable edit to delete the site map
     */
    protected class DeleteEdit extends AbstractDataEdit {

        protected String imageFileName ;
        protected Image image;
        protected double x;
        protected double y;
        protected double width;
        protected double height;

        public DeleteEdit(String imageFileName, Image image, double x,
                double y, double width, double height, long undoIndex) {
            super(undoIndex);
            this.imageFileName = imageFileName;
            this.image = image;
            this.x = x;
            this.y = y;
            this.height = height;
            this.width = width;
        }

        public void undo() throws CannotUndoException {
            siteMapData.setImageFileName(imageFileName);
            siteMapData.setImage(image);
            Point p1 = new Point(0, 0);
            Point p2 = new Point(image.getWidth(view), 
                                    image.getHeight(view));
            double [] v = view.viewToModel(p2);
            siteMapData.setBounds(x, y, width, height);
            mapRect.updateHandles();
            selectAndEditButton.setSelected(true);
            fixedPointResizeButton.setEnabled(true);
            showXORButton(true);
            super.undo();
        }

        public void redo() throws CannotRedoException {
            siteMapData.deleteImage();
            deleteMenuItem.setEnabled(false);
            mapRect.isSelected = false;
            selectAndEditButton.setSelected(true);
            fixedPointResizeButton.setEnabled(false);
            view.setCursor(Cursor.getDefaultCursor());
            showXORButton(false);
            super.redo();
        }
    }

    /**
     * The undoable edit to set the fixed point
     */
    protected class MoveFixedPointEdit extends AbstractDataEdit {

        protected double newFixedPointX, newFixedPointY,
                         oldFixedPointX, oldFixedPointY;

        public MoveFixedPointEdit(double newFixedPointX, double newFixedPointY,
                        double oldFixedPointX, double oldFixedPointY,
                        long undoIndex) {
            super(undoIndex);
            this.newFixedPointX = newFixedPointX;
            this.newFixedPointY = newFixedPointY;
            this.oldFixedPointX = oldFixedPointX;
            this.oldFixedPointY = oldFixedPointY;
        }

        public void undo() throws CannotUndoException {
            fixedPointResizeButton.setSelected(true);
            siteMapData.setFixedPoint(oldFixedPointX, oldFixedPointY);
            super.undo();
        }

        public void redo() throws CannotRedoException {
            fixedPointResizeButton.setSelected(true);
            siteMapData.setFixedPoint(newFixedPointX, newFixedPointY);
            super.redo();
        }
    }

    /**
     * The undoable edit to resize the site map with fixed point
     */
    protected class FixedPointResizeEdit extends AbstractDataEdit {

        protected double newX, newY, newWidth, newHeight,
                         oldX, oldY, oldWidth, oldHeight;

        public FixedPointResizeEdit(double newX, double newY,
                        double newWidth, double newHeight,
                        double oldX, double oldY,
                        double oldWidth, double oldHeight, 
                        long undoIndex) {
            super(undoIndex);
            this.newX = newX;
            this.newY = newY;
            this.newWidth = newWidth;
            this.newHeight = newHeight;
            this.oldX = oldX;
            this.oldY = oldY;
            this.oldWidth = oldWidth;
            this.oldHeight = oldHeight;
        }

        public void undo() throws CannotUndoException {
            siteMapData.setBounds(oldX, oldY, oldWidth, oldHeight);
            fixedPointResizeButton.setSelected(true);
            mapRect.updateHandles();
            super.undo();
        }

        public void redo() throws CannotRedoException {
            siteMapData.setBounds(newX, newY, newWidth, newHeight);
            fixedPointResizeButton.setSelected(true);
            mapRect.updateHandles();
            super.redo();
        }
    }
}
