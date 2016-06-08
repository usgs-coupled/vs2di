/*
 * mp2Frame.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * Defines the behavior of the application's main frame window.
 *
 * <P>Note that creating an application frame is a two-step process. 
 * The first step is to invoke the constructor.
 * The second step is to call the <code>init</code> method.</P>
 *
 * @see mp2.mp2App
 * @see mp2.mp2View
 */
public class mp2Frame extends JFrame implements 
        mp2Constants { 

    /**
     * The panel that holds the data chooser (drop-list box).
     */
    protected JPanel chooserPanel;

    /**
     * The drop-list box that allows user to select the
     * type of graphical data to edit.
     */
    protected JComboBox dataChooser;

    /**
     * The "Drawing Options" menu item
     */
    protected JMenuItem drawingOptionsMenuItem;

    /**
     * The "Edit" menu
     */
    protected JMenu editMenu;

    /**
     * The "File" menu
     */
    protected JMenu fileMenu;

    /**
     *  The "Help" menu
     */
    protected JMenu helpMenu;

    /**
     * The manager for this frame
     */
    protected mp2FrameManager manager;

    /**
     * The menu bar at the top of this frame.
     */
    protected JMenuBar menuBar;

    /**
     * The "Model" menu
     */
    protected JMenu modelMenu;

    /**
     * The "Drawing Options" menu item
     */
    protected JMenuItem modelOptionsMenuItem;

    /**
     * The "Option" menu
     */
    protected JMenu optionsMenu;

    /**
     * The "Show" menu
     */ 
    protected JMenu showMenu;

    /**
     * The scroll pane that holds the view.
     */
    protected JScrollPane scrollPane;

    /**
     * The status bar that holds program status information.
     */
    protected JPanel statusBar;

    /**
     * The label that shows the program status.
     */
    protected JLabel statusLabel;

    /**
     * The panel that holds application information
     */
    protected JPanel statusPanel;

    /**
     * The application object to which this frame belongs.
     */
    protected mp2App theApp;

    /**
     * The tool bar that holds buttons for editing.
     */
    protected JToolBar toolBar;

    /**
     * Indicates whether or not the menu bar has a Model menu
     * for selecting among multiple models
     */
    protected boolean hasMultipleModels;

    /**
     * The view that is shown in the central part of this frame.
     */
    protected mp2View view;

    /**
     * The horizontal ruler
     */
    protected mp2Ruler xRuler;

    /**
     * The vertical ruler
     */
    protected mp2Ruler yRuler;

    /**
     * Creates the manu bar and the "File" menu, the data chooser,
     * the tool bar, and the status bar.
     */
    public mp2Frame(boolean hasMultipleModels) {
        this.hasMultipleModels = hasMultipleModels;
        getContentPane().setLayout(new BorderLayout());
    }

    /**
     * Initializes this frame
     *
     * @param  theApp  the application object to which this 
     *                 frame belongs.
     */
    public void init(mp2App theApp) {
        this.theApp = theApp;

        // Set this frame as the parent of mp2Dialog and 
        // mp2Message classes.
        mp2Dialog.setMainFrame(this);
        mp2MessageBox.setMainFrame(this);

        // Create a menu bar 
        menuBar = new JMenuBar();

        // Construct the "File" menu 
        JMenuItem item;
        menuBar.add(fileMenu = new JMenu("File"));
        fileMenu.setMnemonic(KeyEvent.VK_F);
        fileMenu.add(item = new JMenuItem("New"));
        item.setMnemonic(KeyEvent.VK_N);
        item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, Event.CTRL_MASK));
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onFileNew();
            }
        });
        fileMenu.add(item = new JMenuItem("Open..."));
        item.setMnemonic(KeyEvent.VK_O);
        item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, Event.CTRL_MASK));
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onFileOpen();
            }
        });
        fileMenu.add(item = new JMenuItem("Save"));
        item.setMnemonic(KeyEvent.VK_S);
        item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, Event.CTRL_MASK));
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onFileSave();
            }
        });
        fileMenu.add(item = new JMenuItem("Save As..."));
        item.setMnemonic(KeyEvent.VK_V);
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onFileSaveAs();
            }
        });
        fileMenu.addSeparator();
        fileMenu.add(item = new JMenuItem("Print..."));
        item.setMnemonic(KeyEvent.VK_P);
        item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, Event.CTRL_MASK));
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onFilePrint();
            }
        });
        fileMenu.add(item = new JMenuItem("Export bitmap..."));
        item.setMnemonic(KeyEvent.VK_B);
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onFileExportBitmap();
            }
        });
        fileMenu.add(item = new JMenuItem("Export data..."));
        item.setMnemonic(KeyEvent.VK_D);
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onFileExportData();
            }
        });
        fileMenu.addSeparator();
        fileMenu.add(item = new JMenuItem("Exit"));
        item.setMnemonic(KeyEvent.VK_X);
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onFileExit();
            }
        });

        // Add the "Edit" menu. Menu item will be filled
        // according to application
        menuBar.add(editMenu = new JMenu("Edit"));
        editMenu.setMnemonic(KeyEvent.VK_E);

        // Add the "Option" menu
        menuBar.add(optionsMenu = new JMenu("Options"));
        optionsMenu.setMnemonic(KeyEvent.VK_O);
        drawingOptionsMenuItem = new JMenuItem("Drawing...");
        drawingOptionsMenuItem.setMnemonic(KeyEvent.VK_D);
        drawingOptionsMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDrawingOptions();
            }
        });
        modelOptionsMenuItem = new JMenuItem("Model...");
        modelOptionsMenuItem.setMnemonic(KeyEvent.VK_M);
        modelOptionsMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onModelOptions();
            }
        });
        optionsMenu.add(modelOptionsMenuItem);
        optionsMenu.add(drawingOptionsMenuItem);

        // Add the "Show" menu
        menuBar.add(showMenu = new JMenu("Show"));
        showMenu.setMnemonic(KeyEvent.VK_S);

        // Add the "Model" menu
        if (hasMultipleModels) {
            menuBar.add(modelMenu = new JMenu("Model"));
            modelMenu.setMnemonic(KeyEvent.VK_M);
        }

        // Add the "Help" menu
        menuBar.add(helpMenu = new JMenu("Help"));
        helpMenu.setMnemonic(KeyEvent.VK_H);

        // Create the data chooser
        chooserPanel = new JPanel(new GridLayout(1,0));
        JPanel subPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        chooserPanel.add(subPanel);
        subPanel.add(new JLabel("Active Data: "));
        subPanel.add(dataChooser = new JComboBox());
        dataChooser.setEditable(false);
        dataChooser.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onSelectedData();
                }
            }
        });

        // Create a tool bar
        toolBar = new JToolBar();
        toolBar.setBorder(new EmptyBorder(0, 10, 10, 10));
        toolBar.setLayout(new BoxLayout(toolBar, BoxLayout.Y_AXIS));
        toolBar.setFloatable(false);

        // Create a status bar
        statusPanel = new JPanel(new GridLayout(1, 0));
        statusPanel.setBorder(new EmptyBorder(2, 2, 2, 2));
        statusLabel = new JLabel("  Ready", JLabel.LEFT);
        statusPanel.add(statusLabel);
        statusBar = new JPanel(new GridLayout(1,0));
        statusPanel.add(statusBar);
    }

    /**
     */
    public void addOptionsMenu (JMenuItem item) { optionsMenu.add (item); }
    public void addOptionsMenuSeparator () { optionsMenu.addSeparator (); }

    /**
     * Gets the parent App
     */
    public mp2App getApp() {
        return theApp;
    }

    /**
     * Gets the panel containing the data chooser
     */
    public JPanel getChooserPanel() {
        return chooserPanel;
    }

    /**
     * Gets the data chooser (drop-list box).
     *
     * @return  the data chooser in this frame.
     */
    public JComboBox getDataChooser() {
        return dataChooser;
    }

    /**
     * Gets the menu bar of this frame
     */
    public JMenuBar getJMenuBar() {
        return menuBar;
    }

    /** 
     * Gets the manager of this frame
     */
    public mp2FrameManager getManager() {
        return manager;
    }

    /**
     * Gets the specified menu
     */
    public JMenu getMenu(int index) {
        switch (index) {
        case FILE_MENU:
            return fileMenu;
        case EDIT_MENU:
            return editMenu;
        case OPTIONS_MENU:
            return optionsMenu;
        case SHOW_MENU:
            return showMenu;
        case MODEL_MENU:
            return modelMenu;
        case HELP_MENU:
            return helpMenu;
        default:
            return null;
        }
    }

    /**
     * Gets the specified menu item
     */
    public JMenuItem getMenuItem(int index) {
        switch (index) {
        case DRAWING_OPTIONS:
            return drawingOptionsMenuItem;
        case MODEL_OPTIONS:
            return modelOptionsMenuItem;
        default:
            return manager.getMenuItem(index);
        }
    }

    /**
     * Gets the scroll pane
     */
    public JScrollPane getScrollPane() {
        return scrollPane;
    }

    /**
     * Gets the status bar
     */
    public JPanel getStatusBar() {
        return statusBar;
    }

    /**
     * Gets the tool bar.
     *
     * @return  the tool bar in this frame.
     */
    public JToolBar getToolBar() {
        return toolBar;
    }

    /**
     * Loads the menu bar, tool bar, chooser panel, status panel,
     * and menus
     */
    public void loadComponents() {
        setJMenuBar(menuBar);
        manager.setMenuItemsStartingStates();
        getContentPane().add(toolBar, BorderLayout.WEST);
        getContentPane().add(chooserPanel, BorderLayout.NORTH);
        getContentPane().add(statusPanel, BorderLayout.SOUTH);
    }

    /**
     * Invoked when the "Drawing Options" menu item is selected
     */
    protected void onDrawingOptions() {
        if (theApp.getDoc().editDrawingOptions()) {
            xRuler.setUnits(theApp.getDoc().getRulerUnits());
            yRuler.setUnits(theApp.getDoc().getRulerUnits());
            view.revalidate();
            xRuler.revalidate();
            yRuler.revalidate();
            scrollPane.repaint();
        }
    }

    /**
     * Invoked when the "File|Exit" menu item is selected.
     */
    protected void onFileExit() {
        theApp.appExit();
    }

    protected void onFileExportBitmap() {
        view.exportBitmap();
    }
    
    /**
     * Invoked when the "File|Export data" menu item is selected.
     */
    protected void onFileExportData() {
        theApp.exportData();
    }

    /**
     * Invoked when the "File|New" menu item is selected.
     */
    protected void onFileNew() {
        theApp.newDocument();
    }

    /**
     * Invoked when the "File|Open" menu item is selected.
     */
    protected void onFileOpen() {
        theApp.openDocument();
    }

    /**
     * Invoked when the "File|Print" menu item is selected.
     */
    protected void onFilePrint() {
        view.print();
    }

    /**
     * Invoked when the "File|Save" menu item is selected.
     */
    protected void onFileSave() {
        theApp.saveDocument();
    }

    /**
     * Invoked when the "File|Save As" menu item is selected.
     */
    protected void onFileSaveAs() {
        theApp.saveDocumentAs();
    }

    /**
     * Invoked when the "Model Options" menu item is selected.
     */
    protected void onModelOptions() {
        manager.onModelOptions();
    }

    /**
     * Invoked when a data item is selected in the data chooser.
     */
    protected void onSelectedData() {
        manager.onSelectedData();
    }

    /**
     * Zooms the view, with no change in distortion
     */
    protected void zoom(Point p, int flag) {
        view.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        double [] v = view.viewToModel(p);
        Point vp = scrollPane.getViewport().getViewPosition();
        int dx = p.x - vp.x;
        int dy = p.y - vp.y;
        switch (flag) {
        case -1:
            theApp.getDoc().zoomOut();
            break;
        case 0:
            theApp.getDoc().setMagnification(theApp.getDoc().getStartupMagnification());
            theApp.getView().setScreenDistortionX(1.0);
            theApp.getView().setScreenDistortionY(1.0);
            break;
        case 1:
            if (!theApp.getDoc().zoomIn()) {
                mp2MessageBox.showMessageDialog("Zoom limit reached", "Error");
                view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                return;
            }
            break;
        }
        xRuler.magnify(theApp.getDoc().getMagnification());
        yRuler.magnify(theApp.getDoc().getMagnification());
        view.revalidate();
        scrollPane.getHorizontalScrollBar().revalidate();
        scrollPane.getVerticalScrollBar().revalidate();
        xRuler.revalidate();
        yRuler.revalidate();
        scrollPane.validate();
        Point p1 = view.modelToView(v);
        Dimension extentSize = scrollPane.getViewport().getExtentSize();
        p1.x = Math.max(0, p1.x - extentSize.width/2);
        p1.y = Math.max(0, p1.y - extentSize.height/2);
        scrollPane.getViewport().setViewPosition(p1);
        scrollPane.repaint();
        p1.x += dx;
        p1.y += dy;
        double [] x = view.viewToModel(p1);
        statusLabel.setText(mp2View.getHorizontalAxisName()
                 + " = " +  mp2DecimalFormat.format(x[0])
                 + ", "+ mp2View.getVerticalAxisName() + " = " +  
                mp2DecimalFormat.format(x[1]));
        view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }

    /**
     * Zooms the view, with option for distortion
     */
    protected void dragZoom(Point mouseDownPoint, Point mouseUpPoint) {
        view.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Point vp = scrollPane.getViewport().getViewPosition();
        int dx = mouseUpPoint.x - vp.x;
        int dy = mouseUpPoint.y - vp.y;
        int left = Math.min(mouseUpPoint.x, mouseDownPoint.x);
        int top = Math.min(mouseUpPoint.y, mouseDownPoint.y);
        int width = Math.abs(mouseUpPoint.x - mouseDownPoint.x);
        int height = Math.abs(mouseUpPoint.y - mouseDownPoint.y);
        double [] v = view.viewToModel(new Point(left+width/2, top+height/2));
        Dimension extentSize = scrollPane.getViewport().getExtentSize();
        double xfactor = ((double) extentSize.width)/width;
        double yfactor = ((double) extentSize.height)/height;
        mp2Doc doc = theApp.getDoc();
        double oldMagnification = doc.getMagnification();
        double magnification = oldMagnification* Math.min(xfactor, yfactor);
        if (!doc.setMagnification(magnification)) {
            mp2MessageBox.showMessageDialog("Zoom limit reached.", "Error");
            view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            return;
        }
        xRuler.magnify(magnification);
        yRuler.magnify(magnification);
        view.revalidate();
        scrollPane.getHorizontalScrollBar().revalidate();
        scrollPane.getVerticalScrollBar().revalidate();
        xRuler.revalidate();
        yRuler.revalidate();
        scrollPane.validate();
        Point p1 = view.modelToView(v);
        p1.x = Math.max(0, p1.x - extentSize.width/2);
        p1.y = Math.max(0, p1.y - extentSize.height/2);
        scrollPane.getViewport().setViewPosition(p1);
        scrollPane.repaint();
        p1.x += dx;
        p1.y += dy;
        double [] x = view.viewToModel(p1);
        statusLabel.setText(mp2View.getHorizontalAxisName()
                 + " = " +  mp2DecimalFormat.format(x[0])
                 + ", "+ mp2View.getVerticalAxisName() + " = " +  
                mp2DecimalFormat.format(x[1]));
        view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
    
    protected void distortX(int left, int bottom, Point mousePoint) {
        if (left == bottom) {
            return;
        }
        if (left > bottom) {
            int temp = left;
            left = bottom;
            bottom = temp;
        }
        view.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Point pos = scrollPane.getViewport().getViewPosition();
        int dx = mousePoint.x - pos.x;
        int dy = mousePoint.y - pos.y;
        double [] vPos = view.viewToModel(pos);
        double [] vLeft = view.viewToModel(left, 0);
        Dimension extentSize = scrollPane.getViewport().getExtentSize();
        double distort = ((double) extentSize.width)/(bottom - left + 1);
        mp2View view = theApp.getView();
        double newScreenDistortionX = view.getScreenDistortionX() * distort;
        if (!view.setScreenDistortionX(newScreenDistortionX)) {
            mp2MessageBox.showMessageDialog("Zoom limit reached.", "Error");
            view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            view.repaint();
            return;
        }
        // The next line is a hack so that the ruler will stretch itself
        // correctly. Need to modify the ruler class to do this in a cleaner way.
        xRuler.magnify(theApp.getDoc().getMagnification());
        view.revalidate();
        scrollPane.getHorizontalScrollBar().revalidate();
        xRuler.revalidate();
        scrollPane.validate();
        Point p1 = view.modelToView(vLeft[0], vPos[1]);
        scrollPane.getViewport().setViewPosition(p1);
        scrollPane.repaint();
        p1.x += dx;
        p1.y += dy;
        double [] x = view.viewToModel(p1);
        statusLabel.setText(mp2View.getHorizontalAxisName()
                 + " = " +  mp2DecimalFormat.format(x[0])
                 + ", "+ mp2View.getVerticalAxisName() + " = " +  
                mp2DecimalFormat.format(x[1]));
        view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
    
    protected void distortY(int top, int bottom, Point mousePoint) {
        if (top == bottom) {
            return;
        }
        if (top > bottom) {
            int temp = top;
            top = bottom;
            bottom = temp;
        }
        view.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Point pos = scrollPane.getViewport().getViewPosition();
        int dx = mousePoint.x - pos.x;
        int dy = mousePoint.y - pos.y;
        double [] vPos = view.viewToModel(pos);
        double [] vTop = view.viewToModel(0, top);
        Dimension extentSize = scrollPane.getViewport().getExtentSize();
        double distort = ((double) extentSize.height)/(bottom - top + 1);
        mp2View view = theApp.getView();
        double newScreenDistortionY = view.getScreenDistortionY() * distort;
        if (!view.setScreenDistortionY(newScreenDistortionY)) {
            mp2MessageBox.showMessageDialog("Zoom limit reached.", "Error");
            view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            view.repaint();
            return;
        }
        // The next line is a hack so that the ruler will stretch itself
        // correctly. Need to modify the ruler class to do this in a cleaner way.
        yRuler.magnify(theApp.getDoc().getMagnification());
        view.revalidate();
        scrollPane.getVerticalScrollBar().revalidate();
        yRuler.revalidate();
        scrollPane.validate();
        Point p1 = view.modelToView(vPos[0], vTop[1]);
        scrollPane.getViewport().setViewPosition(p1);
        scrollPane.repaint();
        p1.x += dx;
        p1.y += dy;
        double [] x = view.viewToModel(p1);
        statusLabel.setText(mp2View.getHorizontalAxisName()
                 + " = " +  mp2DecimalFormat.format(x[0])
                 + ", "+ mp2View.getVerticalAxisName() + " = " +  
                mp2DecimalFormat.format(x[1]));
        view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }

    public void setInformationText(int information, String text) {
        manager.setInformationText(information, text);
    }
    
    public void initializeInformationText() {
        manager.initializeInformationText();
    }

    public void setEditable(boolean b) {
        fileMenu.setEnabled(b);
        editMenu.setEnabled(b);
        optionsMenu.setEnabled(b);
        view.setEditable(b);
    }

    /**
     * Sets the frame manager. Always set view to null in the app before
     * setting the manager
     */
    public void setManager(mp2FrameManager manager) {
        this.manager = manager;
        manager.prepareFrame(this);
    }

    /**
     * Sets the text in the status label.
     *
     * @param  text  the text to be show in the status label on
     *               the status bar.
     */
    public void setStatusLabelText(String text) {
        statusLabel.setText(text);
    }

    /**
     * Sets the view to be shown in the central part of this frame.
     * Also sets up horizontal and vertical rulers for the view
     *
     * @param  view  the view to be shown in the central part 
     *               of this frame
     */
    public void setView(mp2View view) {
        this.view = view;

        if (dataChooser.getItemCount() > 0) {
            dataChooser.setSelectedIndex(0);
        }

        // Discard the old scroll pane, if it exists
        if(scrollPane!=null) {
              scrollPane.getViewport().removeAll();
              getContentPane().remove(scrollPane);
        }

        // Create a new scroll pane and put the view in
        // the viewport
        scrollPane = new JScrollPane();
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        scrollPane.setPreferredSize(new Dimension(3*screenSize.width/4, 3*screenSize.height/4));
        scrollPane.setViewportBorder(
                BorderFactory.createLineBorder(Color.black));
        scrollPane.getViewport().add(view);

        // Create rulers and put in column and row headers
        xRuler = new mp2Ruler(mp2Ruler.HORIZONTAL, view);
        yRuler = new mp2Ruler(mp2Ruler.VERTICAL, view);
        xRuler.setUnits(theApp.getDoc().getRulerUnits());
        yRuler.setUnits(theApp.getDoc().getRulerUnits());
        xRuler.magnify(theApp.getDoc().getMagnification());
        yRuler.magnify(theApp.getDoc().getMagnification());
        scrollPane.setColumnHeaderView(xRuler);
        scrollPane.setRowHeaderView(yRuler);

        // Fill corners with white color
        scrollPane.setCorner(JScrollPane.UPPER_LEFT_CORNER, 
                             new Corner());
        scrollPane.setCorner(JScrollPane.LOWER_LEFT_CORNER,
                             new Corner());
        scrollPane.setCorner(JScrollPane.UPPER_RIGHT_CORNER,
                             new Corner());

        // Add the scroll pane to the frame and validate
        getContentPane().add(scrollPane, BorderLayout.CENTER);
        scrollPane.getHorizontalScrollBar().setUnitIncrement(10);
        scrollPane.getVerticalScrollBar().setUnitIncrement(10);
        scrollPane.getHorizontalScrollBar().validate();
        scrollPane.getVerticalScrollBar().validate();
        getContentPane().validate();

        manager.resetMenuItems();
    }

    /**
     * Updates the scales and labels of the x and y rulers,
     * and repaint them.
     */
    public void updateRulerScalesAndLabels() {
        xRuler.updateScalesAndLabels();
        yRuler.updateScalesAndLabels();
        xRuler.repaint();
        yRuler.repaint();
    }

    /**
     * A white rectangle to fill space at corners
     */
    private class Corner extends JComponent {
        public void paintComponent(Graphics g) {
            g.setColor(Color.white);
            g.fillRect(0, 0, getWidth(), getHeight());
        }
    }
}
