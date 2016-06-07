/*
 * mp2PostProcessorFrame.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;

public abstract class mp2PostProcessorFrame extends JFrame 
                            implements mp2VectorDialogCaller, mp2Constants {

    protected mp2App theApp;
    protected String homeDirectory;
    protected String workingDirectory;
    protected String settingsDirectory;
    protected String settingsFileName;
    protected Properties properties;
    protected mp2Model model;
    protected mp2ComputationalModel computationalModel;
    protected mp2PlaybackBinary playbackBinaryModel;
    protected String computationalInputFile;
    protected String playbackInputFile;
    protected boolean doViewer = false;

    protected JMenuBar menuBar;
    protected JMenu fileMenu;
    protected JMenu optionMenu;
    protected JMenuItem loadMenuItem;
    protected JMenuItem loadSettingsMenuItem;
    protected JMenuItem saveSettingsMenuItem;
    protected JMenuItem printMenuItem;
    protected JMenuItem exportBitmapMenuItem;
    protected JMenuItem exitMenuItem;
    protected JMenuItem terminateComputationMenuItem;
    protected JMenuItem restartComputationMenuItem;
    protected JMenuItem vectorMenuItem;
    protected JMenuItem drawingMenuItem;
    protected JMenuItem simulationMenuItem;
    protected JCheckBoxMenuItem bufferedGraphicsMenuItem;

    protected JToolBar toolBar;
    protected mp2Button runButton;
    protected mp2Button stopButton;
    protected mp2Button stepButton;
    protected mp2Button resetButton;
    protected mp2ToggleButton zoomButton;
    protected mp2ToggleButton vectorButton;
    protected ImageIcon velocityImageIcon;
    protected ImageIcon fluxImageIcon;
    
    protected JScrollPane scrollPane;
    protected mp2PostProcessorView view;
    protected mp2Ruler xRuler;
    protected mp2Ruler yRuler;
    protected JMenuItem colorMenuItem;
    protected JPanel chooserPanel;
    protected JComboBox displayChooser;
    protected JComboBox drawingModeChooser;
    protected double magnification;
    protected static double startupMagnification = 1;
    protected NumberFormat nf = NumberFormat.getInstance();

    protected JPanel statusBar;
    protected JLabel coordsLabel;
    protected JLabel modelTimeLabel;
    protected JLabel statusLabel;
    protected int vectorOption;

    protected mp2SwingWorker worker;
    protected boolean isSimulating;
    protected int runStatus;    // 0 = active, 1 = finished, >1 = error
    protected int timeStep;
    protected float modelTime;
    protected int simulationEndStatus;
    
    protected static final int SIMULATION_COMPLETED = 0;
    protected static final int SIMULATION_STOPPED = 1;
    protected static final int SIMULATION_ABORTED = 2;
    
    public static final int DISPLAY_VELOCITY_AND_FLUX = 0;
    public static final int DISPLAY_FLUX_ONLY = 1;
    public static final int DISPLAY_VELOCITY_ONLY = 2;
    
    /**
     * Creates a post processor
     */
    public mp2PostProcessorFrame(mp2App app, String home) {
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onExit();
            }
        });
        
        theApp = app;  // null when running in standalone mode
        workingDirectory = null;
        doViewer = false;
        magnification = startupMagnification;
        vectorOption = DISPLAY_VELOCITY_AND_FLUX;
        setTitle(getPostprocessorTitle());
        
        // Models
        if (theApp != null) {
            computationalModel = createComputationalModel();
            homeDirectory = theApp.getHomeDirectory();
            properties = null;
        } else {
            computationalModel = null;
            homeDirectory = home;
            properties = new Properties();
            readProperties();
        }
        playbackBinaryModel = createPlaybackModel();

        // Menus
        menuBar = new JMenuBar();
        setJMenuBar(menuBar);
        String fileLabel;
        String exitLabel;
        if (theApp != null) {
            fileLabel = "Action";
            exitLabel = "Done";
        } else {
            fileLabel = "File";
            exitLabel = "Exit";
        }
        menuBar.add(fileMenu = new JMenu(fileLabel));
        loadMenuItem = new JMenuItem("Load...");
        loadSettingsMenuItem = new JMenuItem("Load Settings...");
        saveSettingsMenuItem = new JMenuItem("Save Settings...");
        terminateComputationMenuItem = new JMenuItem("Terminate computation");
        restartComputationMenuItem = new JMenuItem("Restart computation");
        printMenuItem = new JMenuItem("Print");
        exportBitmapMenuItem = new JMenuItem("Export bitmap...");
        exitMenuItem = new JMenuItem(exitLabel);
        if (theApp != null) {
            fileMenu.add(terminateComputationMenuItem);
            fileMenu.add(restartComputationMenuItem);
        } else {
            fileMenu.add(loadMenuItem);
        }
        fileMenu.addSeparator();
        fileMenu.add(printMenuItem);
        fileMenu.add(exportBitmapMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(exitMenuItem);

        menuBar.add(optionMenu = new JMenu("Options"));
        optionMenu.add(drawingMenuItem = new JMenuItem("Drawing..."));
        optionMenu.add(colorMenuItem = new JMenuItem("Color Scale..."));
        optionMenu.add(vectorMenuItem = new JMenuItem("Vector..."));
        optionMenu.addSeparator();
        optionMenu.add(simulationMenuItem = new JMenuItem("Simulation..."));
        optionMenu.addSeparator();
        optionMenu.add(bufferedGraphicsMenuItem = new JCheckBoxMenuItem("Buffered Graphics", false));

        // Tool bar
        toolBar = new JToolBar();
        toolBar.setBorder(new EmptyBorder(0, 10, 10, 10));
        toolBar.setLayout(new BoxLayout(toolBar, BoxLayout.Y_AXIS));
        toolBar.setFloatable(false);
        getContentPane().add(toolBar, BorderLayout.WEST);

        toolBar.add(runButton = new mp2Button(new ImageIcon(ClassLoader.getSystemResource("images/run.gif"))));
        toolBar.add(stopButton = new mp2Button(new ImageIcon(ClassLoader.getSystemResource("images/stop.gif"))));
        toolBar.add(Box.createVerticalStrut(5));
        toolBar.add(stepButton = new mp2Button(new ImageIcon(ClassLoader.getSystemResource("images/step.gif"))));
        toolBar.add(Box.createVerticalStrut(5));
        toolBar.add(resetButton = new mp2Button(new ImageIcon(ClassLoader.getSystemResource("images/reset.gif"))));
        toolBar.add(Box.createVerticalStrut(10));
        toolBar.add(zoomButton = new mp2ToggleButton(new ImageIcon(ClassLoader.getSystemResource("images/zoom.gif"))));
        toolBar.add(Box.createVerticalStrut(10));
        velocityImageIcon = new ImageIcon(ClassLoader.getSystemResource("images/velvector.gif"));
        fluxImageIcon = new ImageIcon(ClassLoader.getSystemResource("images/fluxvector.gif"));
        toolBar.add(vectorButton = new mp2ToggleButton(velocityImageIcon));
        runButton.setToolTipText("Run");
        stopButton.setToolTipText("Stop");
        stepButton.setToolTipText("Advance to next time step");
        resetButton.setToolTipText("Reset playback to beginning");
        zoomButton.setToolTipText("Zoom");
        vectorButton.setToolTipText("Show or hide vectors");
        zoomButton.setEnabled(false);
        vectorButton.setEnabled(false);

        // Status bar
        statusBar = new JPanel(new GridLayout(1, 3, 10, 0));
        getContentPane().add(statusBar, BorderLayout.SOUTH);
        statusBar.add(coordsLabel = new JLabel(" ", SwingConstants.LEFT));
        statusBar.add(modelTimeLabel = new JLabel(" ", SwingConstants.CENTER));
        statusBar.add(statusLabel = new JLabel(" ", SwingConstants.RIGHT));
        
        // Scroll pane, rulers, and view
        view = createPostProcessorView();
        xRuler = new mp2Ruler(mp2Ruler.HORIZONTAL, view);
        yRuler = new mp2Ruler(mp2Ruler.VERTICAL, view);
        xRuler.setUnits(mp2Ruler.MODEL_UNITS);
        yRuler.setUnits(mp2Ruler.MODEL_UNITS);
        xRuler.magnify(1);
        yRuler.magnify(1);
        scrollPane = new JScrollPane();
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        scrollPane.setPreferredSize(new Dimension(3*screenSize.width/4, 3*screenSize.height/4));
        scrollPane.setViewportBorder(
                BorderFactory.createLineBorder(Color.black));
        scrollPane.getViewport().add(view);
        scrollPane.setColumnHeaderView(xRuler);
        scrollPane.setRowHeaderView(yRuler);
        scrollPane.setCorner(JScrollPane.UPPER_LEFT_CORNER, 
                             new Corner());
        scrollPane.setCorner(JScrollPane.LOWER_LEFT_CORNER,
                             new Corner());
        scrollPane.setCorner(JScrollPane.UPPER_RIGHT_CORNER,
                             new Corner());
        getContentPane().add(scrollPane, BorderLayout.CENTER);

        // display chooser and drawing mode chooser
        // Items in display chooser will be loaded by subclass
        chooserPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        chooserPanel.add(new JLabel("Display:"));
        chooserPanel.add(displayChooser = new JComboBox());
        chooserPanel.add(drawingModeChooser = new JComboBox());
        getContentPane().add(chooserPanel, BorderLayout.NORTH);
        drawingModeChooser.addItem("Cells");
        drawingModeChooser.addItem("Contours");
        drawingModeChooser.setEnabled(false);
        displayChooser.setEnabled(false);

        // Listeners
        loadMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onLoad();
            }
        });
        loadSettingsMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onLoadSettings();
            }
        });
        saveSettingsMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onSaveSettings();
            }
        });
        terminateComputationMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onTerminateComputation();
            }
        });
        restartComputationMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onRestartComputation();
            }
        });
        printMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onPrint();
            }
        });
        exportBitmapMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onExportBitmap();
            }
        });
        exitMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onExit();
            }
        });
        drawingMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDrawingOptions();
            }
        });
        vectorMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onVectorProperties();
            }
        });
        simulationMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onSimulationOptions();
            }
        });
        runButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onRun();
            }
        });
        stopButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onStop();
            }
        });
        stepButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onStep();
            }
        });
        resetButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onResetPlaybackToBeginning();
            }
        });
        zoomButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                setZoomActive(e.getStateChange() == ItemEvent.SELECTED);
            }
        });
        bufferedGraphicsMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onBufferedGraphics(e.getStateChange()==ItemEvent.SELECTED);
            }
        });           
        colorMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                view.showColorScaleDialog();
            }
        });
        displayChooser.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onDisplayChooser();
                }
            }
        });
        drawingModeChooser.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    if (drawingModeChooser.getSelectedIndex() == 1) {
                        view.setDrawingModeToContours();
                    } else {
                        view.setDrawingModeToCells();
                    }
                }
            }
        });
        vectorButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                view.showVectors(e.getStateChange() == ItemEvent.SELECTED);
            }
        });

        printMenuItem.setEnabled(false);
        exportBitmapMenuItem.setEnabled(false);

        optionMenu.setEnabled(false);
        runButton.setEnabled(false);
        stopButton.setEnabled(false);
        stepButton.setEnabled(false);
        resetButton.setEnabled(false);
    }

    /**
     * Callback function for the vector dialog
     */
    public void applyVectorProperties(float vectorMagnitudePerInch, 
                int ci, int ri, int mode, boolean showStems) {
        model.setVectorMagnitudePerInch(vectorMagnitudePerInch);
        model.setVectorColInterval(ci);
        model.setVectorRowInterval(ri);
        model.setVectorMode(mode);
        model.setShowStems(showStems);
        if (mode == VECTOR_AS_VELOCITY) {
            vectorButton.setIcon(velocityImageIcon);
            vectorButton.setDisabledIcon(velocityImageIcon);
            vectorButton.setDisabledSelectedIcon(velocityImageIcon);
        } else {
            vectorButton.setIcon(fluxImageIcon);
            vectorButton.setDisabledIcon(fluxImageIcon);
            vectorButton.setDisabledSelectedIcon(fluxImageIcon);
        }
        view.setVectorMagnitudePerPixel(
            (float) (vectorMagnitudePerInch / PIXELS_PER_INCH / magnification));
        view.draw();
        if (theApp != null) {
            theApp.getDoc().setChanged(true);
        }
    }

    /**
     * Creates a computational model
     */
    protected abstract mp2ComputationalModel createComputationalModel();
    
    /**
     * Creates a playback model
     */
    protected abstract mp2PlaybackBinary createPlaybackModel();
    
    /**
     * Creates a view for the post processor
     */
    protected abstract mp2PostProcessorView createPostProcessorView();
    
    /**
     * Performs model start up calculations.
     */
    protected int doModelStartUpCalculations() {
        return 0;
    }
    
    /**
     * Performs inner loops of a time step, such as moving particles.
     * The default implementation does nothing
     */
    protected int doInnerSteps(boolean updateLabel) {
        return 0;
    }
    
    protected void drawViewDuringSimulation() {
        view.draw();
    }
    
    protected void drawViewOnResetPlaybackToBeginning() {
        view.draw();
    }
    
    protected void drawViewOnRestartComputation() {
        view.draw();
    }
    
    /**
     * Gets the filename for writing binary output from the computational model
     */
    protected abstract String getComputationalOutputFile();

    /**
     * Gets the number format
     */
    public NumberFormat getNumberFormat() {
        return nf;
    }

    protected abstract String getPostprocessorTitle();
    
    public Properties getProperties() {
        return properties;
    }
    
    /**
     * Gets the directory containing the properties for this
     * post processor
     */
    protected String getPropertiesDirectory() {
        return homeDirectory;
    }
    
    /**
     * Gets the file name containing the properties for this
     * post processor
     */
    protected String getPropertiesFileName() {
        return null;
    }
    
    /**
     * Gets the scroll pane
     */
    public JScrollPane getScrollPane() {
        return scrollPane;
    }
    
    public String getWorkingDirectory() {
        return workingDirectory;
    }

    /**
     * loads the specified data file from the specified directory
     */
    public boolean loadData(String directory, String file) {
        mp2Math.changeDirectory(directory + ".");
        String inputFile;
        if (theApp != null) {
            model = computationalModel;
            computationalInputFile = file;
            playbackInputFile = getComputationalOutputFile();
            inputFile = computationalInputFile;
            computationalModel.setDoc(theApp.getDoc());
            computationalModel.setPostProcessorOptions(
                (mp2PostProcessorOptions) theApp.getDoc().getData(POST_PROCESSOR_OPTIONS));
            computationalModel.setOutputDirectory(directory);
            computationalModel.setOutputFileName(getComputationalOutputFile());
            mp2PostProcessorOptions postOptions = 
                (mp2PostProcessorOptions) theApp.getDoc().getData(POST_PROCESSOR_OPTIONS);
            if (postOptions != null) {
                bufferedGraphicsMenuItem.setSelected(postOptions.getImageBufferingEnabled());
            }
            setTitle(getPostprocessorTitle() + ": " + theApp.getDoc().getFileName());
        } else if (doViewer) {
            if (model != null) {
                model.closeIO();
            }
            model = computationalModel;
            computationalInputFile = file;
            playbackInputFile = getComputationalOutputFile();
            inputFile = computationalInputFile;
            computationalModel.setOutputDirectory(directory);
            computationalModel.setOutputFileName(getComputationalOutputFile());
            setTitle(getPostprocessorTitle() + ": " + file);
        } else {
            playbackInputFile = file;
            inputFile = playbackInputFile;
            model = playbackBinaryModel;
            setTitle(getPostprocessorTitle() + ": " + file);
        }

        runStatus = 0;
        // load the file to the model
        if (!model.setup(inputFile, false)) {
            onModelStartupFailure(inputFile);
            return false;
        }
        // perform any inner steps necessary for the model startup
        runStatus = doModelStartUpCalculations();
        if (runStatus > 1) {
            onRunAborted();
            return false;
        }
        
        view.setModel(model);
        
        // update the view and rulers
        view.setColorScales(model.getColorScales());
        view.resizeContents(magnification);
        xRuler.setUnits(model.getRulerUnits());
        yRuler.setUnits(model.getRulerUnits());
        xRuler.magnify(magnification);
        yRuler.magnify(magnification);
        nf.setMaximumFractionDigits(model.getNumberOfDecimalPlacesToShow());
        nf.setMinimumFractionDigits(model.getNumberOfDecimalPlacesToShow());
        view.revalidate();
        xRuler.revalidate();
        yRuler.revalidate();

        // show the initial conditions 
        view.draw();
        scrollPane.repaint();

        timeStep = model.getTimeStep();
        modelTime = (float) model.getModelTime();
        updateModelTimeText();
        if (model.getType() == mp2Model.COMPUTATIONAL) {
            statusLabel.setText("Ready to compute");
        } else {
            statusLabel.setText("Ready to playback");
        }

        if (model.getVectorMode() == VECTOR_AS_VELOCITY) {
            vectorButton.setIcon(velocityImageIcon);
            vectorButton.setDisabledIcon(velocityImageIcon);
            vectorButton.setDisabledSelectedIcon(velocityImageIcon);
        } else {
            vectorButton.setIcon(fluxImageIcon);
            vectorButton.setDisabledIcon(fluxImageIcon);
            vectorButton.setDisabledSelectedIcon(fluxImageIcon);
        }

        // Enable/disable menu items and buttons.
        terminateComputationMenuItem.setEnabled(false);
        restartComputationMenuItem.setEnabled(false);
        printMenuItem.setEnabled(true);
        exportBitmapMenuItem.setEnabled(true);
        optionMenu.setEnabled(true);
        runButton.setEnabled(true);
        stopButton.setEnabled(false);
        stepButton.setEnabled(true);
        resetButton.setEnabled(false);
        zoomButton.setEnabled(true);
        vectorButton.setEnabled(true);
        
        displayChooser.setEnabled(true);
        drawingModeChooser.setEnabled(true);
        displayChooser.setSelectedIndex(0);
        drawingModeChooser.setSelectedIndex(0);
        colorMenuItem.setEnabled(true);
        
        if (model.getType() == mp2Model.PLAYBACK_BINARY) {
            if (!((mp2PlaybackBinary) model).dataAvailable()) {
                statusLabel.setText("Playback completed");
                onRunCompleted();
                resetButton.setEnabled(false);
            }
        } else if (runStatus == 1) {
            statusLabel.setText("Run completed");
            onRunCompleted();
            resetButton.setEnabled(false);
        }            
        return true;
    }
    
    protected void loadFileMenuForPlayback() {
        fileMenu.removeAll();
        fileMenu.add(loadMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(printMenuItem);
        fileMenu.add(exportBitmapMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(exitMenuItem);
    }
    
    protected void loadFileMenuForViewer() {
        fileMenu.removeAll();
        fileMenu.add(loadMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(loadSettingsMenuItem);
        fileMenu.add(saveSettingsMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(terminateComputationMenuItem);
        fileMenu.add(restartComputationMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(printMenuItem);
        fileMenu.add(exportBitmapMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(exitMenuItem);
    }
    
    /**
     * Invokde when the buffered graphics menu item is selected
     */
    protected void onBufferedGraphics(boolean b) {
        view.setImageBufferingEnabled(b);
        if (theApp != null) {
            mp2PostProcessorOptions postOptions = 
                (mp2PostProcessorOptions) theApp.getDoc().getData(POST_PROCESSOR_OPTIONS);
            if (postOptions != null && postOptions.getImageBufferingEnabled() != b) {
                postOptions.setImageBufferingEnabled(b);
                theApp.getDoc().setChanged(true);
            }
        }
    }
    
    /**
     * Invoked when the selection on the display chooser is changed
     */
    protected abstract void onDisplayChooser();

    /**
     * Invoked when the drawing options menu item is selected
     */
    public void onDrawingOptions() {

        mp2DrawingOptionsDialog dlg = new mp2DrawingOptionsDialog(this);
        dlg.drawingWidth      = model.getDrawingWidthInInches();
        dlg.drawingHeight     = model.getDrawingHeightInInches();
        dlg.distanceXPerInch  = model.getModelDistancePerInchX();
        dlg.distanceYPerInch  = model.getModelDistancePerInchY();
        dlg.xOriginInInches   = model.getXOriginInInches();
        dlg.yOriginInInches   = model.getYOriginInInches();
        dlg.numDecimal        = model.getNumberOfDecimalPlacesToShow();
        dlg.rulerUnits = model.getRulerUnits();

        if (dlg.doModal() == true) {
            model.setDrawingWidthInInches(dlg.drawingWidth);
            model.setDrawingHeightInInches(dlg.drawingHeight);
            model.setModelDistancePerInchX(dlg.distanceXPerInch);
            model.setModelDistancePerInchY(dlg.distanceYPerInch);
            model.setXOriginInInches(dlg.xOriginInInches);
            model.setYOriginInInches(dlg.yOriginInInches);
            model.setNumberOfDecimalPlacesToShow(dlg.numDecimal);
            model.setRulerUnits(dlg.rulerUnits);

            nf.setMaximumFractionDigits(dlg.numDecimal);
            nf.setMinimumFractionDigits(dlg.numDecimal);
            view.resizeContents(magnification);
            xRuler.setUnits(dlg.rulerUnits);
            yRuler.setUnits(dlg.rulerUnits);
            view.revalidate();
            xRuler.revalidate();
            yRuler.revalidate();
            view.draw();
            scrollPane.repaint();
        }
    }

    /**
     * Invoked when user selects Done, Exit, or closes the window
     */
    protected void onExit() {
        if (theApp == null) {
            if (doViewer && !quitOK()) {
                return;
            }
            writeProperties();
            dispose();
            System.exit(0);
        } else {
            if (quitOK()) {
                // The preprocessor will set this frame invisible.
                theApp.getFrame().getMenuItem(POST_PROCESSOR).setSelected(false);
            }
        }
    }

    /**
     * Invoked when the export bitmap menu item is selected.
     */
    protected void onExportBitmap() {
        view.exportBitmap();
    }

    /**
     * Invoked when the load menu item is selected
     */
    protected boolean onLoad() {
        if (doViewer && model != null && model.getType() == mp2Model.COMPUTATIONAL
                     && runStatus == 0) {
            int result = mp2MessageBox.showYesNoDialog(this, "The computation is not finished. " + 
                "Do you want to load another file?", "Warning");
            if (result == mp2MessageBox.NO_OPTION) {
                return false;
            }
        }
        if (model != null) {
            model.closeIO();
        }
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Load");
        if (workingDirectory != null) {
            fc.setCurrentDirectory(new File(workingDirectory));
        } else if (properties != null) {
            String directory = properties.getProperty("directory");
            if (directory != null && directory.length() > 0) {
                fc.setCurrentDirectory(new File(directory));
            }
        }
        if (fc.showOpenDialog(this) != mp2FileChooser.APPROVE_OPTION) {
            return false;
        }
                
        workingDirectory = new String(fc.getCurrentDirectory().getPath());
        String file = new String(fc.getSelectedFile().getName());
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        if (!loadData(workingDirectory, file)) {
            setCursor(Cursor.getDefaultCursor());
            return false;
        }
        setCursor(Cursor.getDefaultCursor());
        scrollPane.getHorizontalScrollBar().setValue(0);
        scrollPane.getVerticalScrollBar().setValue(0);
        magnification = startupMagnification;
        view.setVectorMagnitudePerPixel(
            (float) model.getVectorMagnitudePerInch() / PIXELS_PER_INCH);
        view.resizeContents(magnification);
        view.revalidate();
        xRuler.updateScalesAndLabels();
        yRuler.updateScalesAndLabels();
        xRuler.revalidate();
        yRuler.revalidate();
        view.draw();
        scrollPane.repaint();
        loadSettingsMenuItem.setEnabled(true);
        saveSettingsMenuItem.setEnabled(true);
        return true;
    }

    
    protected boolean onLoadSettings() {
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Load Settings");
        if (workingDirectory != null) {
            fc.setCurrentDirectory(new File(workingDirectory));
        } else if (properties != null) {
            String directory = properties.getProperty("directory");
            if (directory != null && directory.length() > 0) {
                fc.setCurrentDirectory(new File(directory));
            }
        }
        if (fc.showOpenDialog(this) != mp2FileChooser.APPROVE_OPTION) {
            return false;
        }
	    settingsDirectory = fc.getCurrentDirectory().getPath();
	    settingsFileName = fc.getSelectedFile().getName();
        try {
            File file = new File(settingsDirectory, settingsFileName);
            BufferedReader in = new BufferedReader(new FileReader(file));
            readSettingsFile(in);	    
            in.close ();
            magnification = startupMagnification;
            view.resizeContents(magnification);
            view.setVectorMagnitudePerPixel(
                (float) computationalModel.getVectorMagnitudePerInch() / PIXELS_PER_INCH);
            view.revalidate();
            xRuler.updateScalesAndLabels();
            yRuler.updateScalesAndLabels();
            xRuler.revalidate();
            yRuler.revalidate();
            view.draw();
            scrollPane.repaint();
        } catch (Exception e) {
            mp2MessageBox.showMessageDialog(this, "Read error encountered.", "Error");
            return false;
        }
        return true;
    }
    
    protected void onModelStartupFailure(String inputFile) {
        if (model.getType() == mp2Model.COMPUTATIONAL) {
            if (isVisible()) {
                mp2MessageBox.showMessageDialog(this, "Unable to start model.", "Error");
            } else {
                mp2MessageBox.showMessageDialog("Unable to start model.", "Error");
            }
        } else {
            if (isVisible()) {
                mp2MessageBox.showMessageDialog(this, "Unable to load file \""
                        +inputFile + "\"", "Error");
            } else {
                mp2MessageBox.showMessageDialog("Unable to load file \""
                        +inputFile + "\"", "Error");
            }
        }
    }
    
    protected void readSettingsFile(BufferedReader in) throws Exception {
        try {
            String line;
            line = in.readLine();
            double drawingWidth = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
            computationalModel.setDrawingWidthInInches(drawingWidth);
	    
            line = in.readLine();
            double drawingHeight = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
            computationalModel.setDrawingHeightInInches(drawingHeight);
	    
            line = in.readLine();
            double modelDistancePerInchX = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
            computationalModel.setModelDistancePerInchX(modelDistancePerInchX);
	    
            line = in.readLine();
            double modelDistancePerInchY = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
            computationalModel.setModelDistancePerInchY(modelDistancePerInchY);
	    
            line = in.readLine();
            double xOriginInInches = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
            computationalModel.setXOriginInInches(xOriginInInches);
	    
            line = in.readLine();
            double yOriginInInches = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
            computationalModel.setYOriginInInches(yOriginInInches);
	    
            line = in.readLine();
            int numDecimal = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
            computationalModel.setNumberOfDecimalPlacesToShow(numDecimal);
	    
            line = in.readLine();
            float secPerStep = Float.valueOf(line.substring(line.indexOf(':')+1).trim()).floatValue();
            computationalModel.setSecPerStep(secPerStep);
	    
            line = in.readLine();
            float saveInterval = Float.valueOf(line.substring(line.indexOf(':')+1).trim()).floatValue();
            computationalModel.setSaveInterval(saveInterval);
	    
            line = in.readLine();
            int saveBinaryFlag = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
	        computationalModel.setSaveOutputAsBinary(saveBinaryFlag == 1 ? true : false);
	    
            line = in.readLine();
            float vectorMagnitudePerInch = Float.valueOf(line.substring(line.indexOf(':')+1).trim()).floatValue();
            computationalModel.setVectorMagnitudePerInch(vectorMagnitudePerInch);
	    
            line = in.readLine();
            int vectorColInterval = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
	        computationalModel.setVectorColInterval(vectorColInterval);
	    
            line = in.readLine();
            int vectorRowInterval = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
	        computationalModel.setVectorRowInterval(vectorRowInterval);
	    
            line = in.readLine();
            int vectorMode = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
	        computationalModel.setVectorMode(vectorMode);
            if (vectorMode == VECTOR_AS_VELOCITY) {
                vectorButton.setIcon(velocityImageIcon);
                vectorButton.setDisabledIcon(velocityImageIcon);
                vectorButton.setDisabledSelectedIcon(velocityImageIcon);
            } else {
                vectorButton.setIcon(fluxImageIcon);
                vectorButton.setDisabledIcon(fluxImageIcon);
                vectorButton.setDisabledSelectedIcon(fluxImageIcon);
            }
	    
            line = in.readLine();
            int showStemsFlag = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
	        computationalModel.setShowStems(showStemsFlag == 1 ? true : false);
	    
	        mp2ColorScale [] colorScales = computationalModel.getColorScales();
		
		    double valueRed, valueBlue, colorInterval, labelInterval;
		    for (int i=0; i<colorScales.length; i++) {
                line = in.readLine();
                valueRed = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
                line = in.readLine();
                valueBlue = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
                line = in.readLine();
                colorInterval = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
                line = in.readLine();
                labelInterval = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
                colorScales[i].SetLimits(valueBlue, valueRed);
		        colorScales[i].SetColorInterval(colorInterval);
			    colorScales[i].SetLabelInterval(labelInterval);
		    }
        } catch (Exception e) {
            throw e;
        }
    }
    
    protected void onSaveSettings() {
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Save Settings");
	    if (settingsDirectory != null) {
            fc.setCurrentDirectory(new File(settingsDirectory));
	    } else if (workingDirectory != null) {
            fc.setCurrentDirectory(new File(workingDirectory));
	    } else if (properties != null) {
            String directory = properties.getProperty("directory");
            if (directory != null && directory.length() > 0) {
                fc.setCurrentDirectory(new File(directory));
            }
        }
	    if (settingsFileName != null) {
	        fc.setSelectedFile(new java.io.File(settingsFileName));
	    }
        if (fc.showSaveDialog(this) != mp2FileChooser.APPROVE_OPTION) {
            return;
        }
	    settingsDirectory = fc.getCurrentDirectory().getPath();
	    settingsFileName = fc.getSelectedFile().getName();
        try {
            File file = new File(settingsDirectory, settingsFileName);
            FileOutputStream fos = new FileOutputStream(file);
            PrintWriter pw = new PrintWriter(fos, true);
	        writeSettingsFile(pw);
        } catch (Exception e) {
        	mp2MessageBox.showMessageDialog("Unable to write file.", "Error");  
        }
    }
    
    protected void writeSettingsFile(PrintWriter pw) throws Exception {
        try {
            pw.println("drawing width in inches: " + computationalModel.getDrawingWidthInInches());
            pw.println("drawing height in inches: " + computationalModel.getDrawingHeightInInches());
            pw.println("model distance per inch x: " + computationalModel.getModelDistancePerInchX());
            pw.println("model distance per inch y: " + computationalModel.getModelDistancePerInchY());
            pw.println("x origin in inches: " + computationalModel.getXOriginInInches());
            pw.println("y origin in inches: " + computationalModel.getYOriginInInches());
            pw.println("number of decimal places: " + computationalModel.getNumberOfDecimalPlacesToShow());
            pw.println("seconds per time step: " + (computationalModel.getSecPerStep()));
            pw.println("save interval: " + computationalModel.getSaveInterval());
	        pw.println("save output: " + (computationalModel.getSaveOutputAsBinary() ? 1 : 0));
            pw.println("vector magnitude per inch: " + computationalModel.getVectorMagnitudePerInch());
            pw.println("vector vector column interval: " + computationalModel.getVectorColInterval());
            pw.println("vector row interval: " + computationalModel.getVectorRowInterval());
            pw.println("vector mode:" + computationalModel.getVectorMode());
	        pw.println("show vector base: " + (computationalModel.getShowStems() ? 1 : 0));
	        mp2ColorScale [] colorScales = computationalModel.getColorScales();
            for (int i=0; i<colorScales.length; i++) {
                pw.println("color scale " + i + " red limit: " + colorScales[i].GetValueRed());
                pw.println("color scale " + i + " blue limit: " + colorScales[i].GetValueBlue());
                pw.println("color scale " + i + " color interval: " + colorScales[i].GetColorInterval());
                pw.println("color scale " + i + " label interval: " + colorScales[i].GetLabelInterval());
            }
        } catch (Exception e) {
            throw e;
        }
    }
    /**
     * Invoked when the print menu item is selected
     */
    protected void onPrint() {
        view.print();
    }

    /**
     * Invoked when the reset button is clicked
     */
    protected void onResetPlaybackToBeginning() {
        // If the model has saved binary output, then switch to playback mode.
        // and copy parameters from computational model to playback model.
        model.closeIO();
        if ((model.getType() == mp2Model.COMPUTATIONAL) && computationalModel.getSaveOutputAsBinary()) {
            model = playbackBinaryModel;
            model.copySimulationInfoFrom(computationalModel);
            view.setModel(model);
        }
        if (!model.setup(playbackInputFile, true)) {
            mp2MessageBox.showMessageDialog(this, "IO Error encountered. Cannot play back.",
                                            "Error");
            return;
        }
        runStatus = 0;
        timeStep = model.getTimeStep();
        modelTime = (float) model.getModelTime();
        drawViewOnResetPlaybackToBeginning();
        updateModelTimeText();
        
        if (!((mp2PlaybackBinary) model).dataAvailable()) {
            statusLabel.setText("Playback completed");
            onRunCompleted();
            resetButton.setEnabled(false);
        } else {
            statusLabel.setText("Ready to playback");
            runButton.setEnabled(true);
            stopButton.setEnabled(false);
            stepButton.setEnabled(true);
            resetButton.setEnabled(false);
        }
    }

    /**
     * Invoked when the restart computation menu item is selected
     */
    protected void onRestartComputation() {
        int result = mp2MessageBox.showYesNoDialog(this, 
                "Do you want to restart the computation?", "Warning");
        if (result == mp2MessageBox.NO_OPTION) {
            return;
        }
        model.closeIO();
        if (model.getType() == mp2Model.PLAYBACK_BINARY) {
            model = computationalModel;
            model.copySimulationInfoFrom(playbackBinaryModel);
            view.setModel(model);
        }
        if (!model.setup(computationalInputFile, true)) {
            mp2MessageBox.showMessageDialog(this, "Error encountered. Cannot restart computation.",
                                            "Error");
            return;
        }
        // perform any inner steps necessary for the model startup
        runStatus = doInnerSteps(false);
        if (runStatus > 1) {
            mp2MessageBox.showMessageDialog(this, "Error encountered. Cannot restart computation.",
                                            "Error");
            return;
        }
        runStatus = 0;
        timeStep = model.getTimeStep();
        modelTime = (float) model.getModelTime();
        drawViewOnRestartComputation();
        updateModelTimeText();
        statusLabel.setText("Ready to compute");
        terminateComputationMenuItem.setEnabled(false);
        restartComputationMenuItem.setEnabled(false);
        runButton.setEnabled(true);
        stopButton.setEnabled(false);
        stepButton.setEnabled(true);
        resetButton.setEnabled(false);
    }

    /**
     * Invoked when the run button is clicked
     */
    protected void onRun() {
        if (model.getType() == mp2Model.COMPUTATIONAL) {
            statusLabel.setText("Computing");
        } else {
            statusLabel.setText("Playing");
        }
        fileMenu.setEnabled(false);
        optionMenu.setEnabled(false);
        runButton.setEnabled(false);
        stopButton.setEnabled(true);
        stepButton.setEnabled(false);
        resetButton.setEnabled(false);
        zoomButton.setSelected(false);
        zoomButton.setEnabled(false);
        vectorButton.setEnabled(false);
        displayChooser.setEnabled(false);
        drawingModeChooser.setEnabled(false);
        isSimulating = true;
        worker = new mp2SwingWorker() {
            public Object construct() {
                return simulate();
            }
            public void finished() {
                if (simulationEndStatus == SIMULATION_ABORTED) {
                    onRunAborted();
                }
                else if (simulationEndStatus == SIMULATION_STOPPED) {
                    onRunStopped();
                } else {
                    if (model.getType() == mp2Model.PLAYBACK_BINARY) {
                        statusLabel.setText("Playback completed");
                    } else {
                        statusLabel.setText("Completed at time step " 
                                                    + timeStep);
                    }
                    onRunCompleted();
                }
            }
        };
    }

    /**
     * Performs cleanup after a run is aborted (cannot be continued).
     */
    protected void onRunAborted() {
        if (model.getType() == mp2Model.PLAYBACK_BINARY) {
            statusLabel.setText("Playback aborted");
        } else {
            statusLabel.setText("Aborted at time step " 
                                        + timeStep);
        }
        onRunCompleted();
    }

    /**
     * Performs cleanup after a run is completed
     */
    protected void onRunCompleted() {
        model.closeIO();
        runStatus = 1;
        fileMenu.setEnabled(true);
        optionMenu.setEnabled(true);
        terminateComputationMenuItem.setEnabled(false);
        restartComputationMenuItem.setEnabled(true);
        runButton.setEnabled(false);
        stopButton.setEnabled(false);
        stepButton.setEnabled(false);
        resetButton.setEnabled((model.getType() == mp2Model.PLAYBACK_BINARY) ||
            ((model.getType() == mp2Model.COMPUTATIONAL) && computationalModel.getSaveOutputAsBinary()));
        zoomButton.setEnabled(true);
        vectorButton.setEnabled(true);
        displayChooser.setEnabled(true);
        drawingModeChooser.setEnabled(true);
    }

    /**
     * Performs cleanup after a run is stopped (but not aborted, which means
     * the the run can continue).
     */
    protected void onRunStopped() {
        if (model.getType() == mp2Model.PLAYBACK_BINARY) {
            statusLabel.setText("Ready to playback");
        } else {
            statusLabel.setText("Stopped at time step " 
                                        + timeStep);
        }
        fileMenu.setEnabled(true);
        optionMenu.setEnabled(true);
        terminateComputationMenuItem.setEnabled(model.getType() == mp2Model.COMPUTATIONAL);
        restartComputationMenuItem.setEnabled(true);
        runButton.setEnabled(true);
        stopButton.setEnabled(false);
        stepButton.setEnabled(true);
        resetButton.setEnabled(model != computationalModel);
        zoomButton.setEnabled(true);
        vectorButton.setEnabled(true);
        displayChooser.setEnabled(true);
        drawingModeChooser.setEnabled(true);
    }

    /**
     * Invoked when the simulation menu item is selected. Displays
     * the simulation options dialog for users to set simulation parameters.
     */
    protected void onSimulationOptions() {
        boolean computing = (model.getType() == mp2Model.COMPUTATIONAL);
        mp2SimulationDialog dlg = new mp2SimulationDialog(this, computing);
        dlg.secPerStep = model.getSecPerStep();
        if (computing) {
            dlg.saveInterval = computationalModel.getSaveInterval();
            dlg.saveBinary = computationalModel.getSaveOutputAsBinary();
        }

        if (dlg.doModal() == true) {
            model.setSecPerStep(dlg.secPerStep);
            if (computing) {
                computationalModel.setSaveInterval(dlg.saveInterval);
                computationalModel.setSaveOutputAsBinary(dlg.saveBinary);
            }
            if (theApp != null) {
                theApp.getDoc().setChanged(true);
            }
        }
    }

    /**
     * Invoked when the step button is clicked. Advances one time step
     */
    protected void onStep() {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        runStatus = model.step();
        if (runStatus > 1) {
            onRunAborted();
        } else {
            timeStep = model.getTimeStep();
            doInnerSteps(false);
            modelTime = (float) model.getModelTime();
            drawViewDuringSimulation();
            updateModelTimeText();
            if (runStatus == 1) {
                if (model.getType() == mp2Model.PLAYBACK_BINARY) {
                    statusLabel.setText("Playback completed");
                } else {
                    statusLabel.setText("Completed at time step " 
                                                + timeStep);
                }
                onRunCompleted();
            } else {
                onRunStopped();
            }
        }
        setCursor(Cursor.getDefaultCursor());
    }

    /**
     * Invoked when the stop button is clicked. Stops the run.
     */
    protected void onStop() {
        isSimulating = false;
        Runnable disableStopButtonRunnable = new Runnable() {
            public void run () {
                stopButton.setEnabled(false);
            }
        };
        try {
            SwingUtilities.invokeLater(disableStopButtonRunnable);
        } catch (Exception e) {}
    }

    /**
     * Invoked when the terminate computation menu item is selected.
     * Terminates the run.
     */
    protected void onTerminateComputation() {
        int result = mp2MessageBox.showYesNoDialog(this, "Do you want to terminate the computation?", 
                "Warning");
        if (result == mp2MessageBox.NO_OPTION) {
            return;
        }
        statusLabel.setText("Terminated at time step " 
                                    + timeStep);
        onRunCompleted();
    }

    /**
     * Invoked when the vector menu item is selected. Displays the
     * vector dialog box for users to set vector properties.
     */
    protected void onVectorProperties() {
        boolean doVectorOption = (vectorOption == DISPLAY_VELOCITY_AND_FLUX);
        mp2VectorDialog dlg = new mp2VectorDialog(this, this, "Vector", doVectorOption);
        dlg.vectorMagnitudePerInch = model.getVectorMagnitudePerInch();
        dlg.vectorColInterval = model.getVectorColInterval();
        dlg.vectorRowInterval = model.getVectorRowInterval();
        dlg.vectorMode = model.getVectorMode();
        dlg.showStems = model.getShowStems();
        if (dlg.doModal() == true) {
            applyVectorProperties(dlg.vectorMagnitudePerInch, dlg.vectorColInterval,
                                   dlg.vectorRowInterval, dlg.vectorMode, dlg.showStems);
        }
    }

    /**
     * Prepares to execute a time step
     */
    protected void prepareToStep() {
        Runnable updateLabelRunnable = new Runnable() {
            public void run () {
                statusLabel.setText("Computing time step " + (timeStep+1));
            }
        };
        try {
            SwingUtilities.invokeAndWait(updateLabelRunnable);
        } catch (Exception e) {}  
    }
    
    /**
     * Indicates whether or not it is ok to quit.
     */
    public boolean quitOK() {
        if (model == null) {
            return true;
        }
        if (model != computationalModel || runStatus == 1) {
            model.closeIO();
            return true;
        }
        if (isSimulating) {
            isSimulating = false;
        }

        int result = mp2MessageBox.showYesNoDialog(this, "The computation is not finished. " + 
            "Do you want to quit anyway?", "Warning");
        if (result == mp2MessageBox.NO_OPTION) {
            return false;
        }
        model.closeIO();
        runStatus = 1;
        return true;
    }

    /**
     * Read properties.
     */
    protected void readProperties() {
        if (theApp != null) {
            return;
        }
        String fileName = getPropertiesFileName();
        if (fileName == null) {
            return;
        }
        FileInputStream in;
        try {
            in = new FileInputStream(new File(
                        getPropertiesDirectory(), fileName));
        } catch (FileNotFoundException e) {
            return;
        }
        try {
            properties.load(in);
            in.close();
        } catch(IOException e) {
            return;
        }
    }
    
    /**
     * Resets the postprocessor for a new simulation
     */
    public void reset() {
        magnification = startupMagnification;
        scrollPane.getHorizontalScrollBar().setValue(0);
        scrollPane.getVerticalScrollBar().setValue(0);
    }

    /**
     * If the post postprocessor not in standalone mode, mark
     * the document as changed.
     */
    public void setChanged() {
        if (theApp != null) {
            theApp.getDoc().setChanged(true);
        }
    }

    /**
     * Sets the coordinates label
     */
    public void setCoordsLabelText(String text) {
        coordsLabel.setText(text);
    }
    
    public static void setStartupMagnification(double v) {
        startupMagnification = v;
    }
    
    /**
     * Sets the quantities represented by the vector
     */
    public void setVectorOption(int option) {
        vectorOption = option;
    }

    /**
     * Invoked when the zoom button is selected or unselected
     */
    protected void setZoomActive(boolean b) {
        view.setZoomActive(b);
    }

    /**
     * Simulation thread
     */
    protected Object simulate() {
        while (isSimulating) {
            // Remember the system time at the start of the step
            long startTime = System.currentTimeMillis();
            
            // advance one time step. The try block catches any interruptions
            try {
            
                // prepare to execute a time step. The default implementation
                // displays the label "Computing time step s", where s is the
                // time step number.
                if (model.getType() == mp2Model.COMPUTATIONAL) {
                    prepareToStep();
                }
                
                // executes a time step
                runStatus = model.step();
                
                // Checks for error
                if (runStatus > 1) {
                    // encountered error
                    isSimulating = false;
                    simulationEndStatus = SIMULATION_ABORTED;
                    return "Simulation Aborted";
                }
                
                // updates the time step
                timeStep = model.getTimeStep();
                
                // executes inner time steps, such as moving particles.
                // The default implementation does nothing
                doInnerSteps(true);
                
                // updates the view and model time label
                modelTime = (float) model.getModelTime();
                Runnable updateViewRunnable = new Runnable() {
                    public void run () {
                        drawViewDuringSimulation();
                        updateModelTimeText();
                    }
                };
                try {
                    SwingUtilities.invokeAndWait(updateViewRunnable);
                } catch (Exception e) {}   
                
                // Checks for end of simulation
                if (runStatus == 1) {
                    isSimulating = false;
                    simulationEndStatus = SIMULATION_COMPLETED;
                    return "Simulation Completed";
                }
                
                // Sleep between time steps, if required.
                long sleepTime = startTime + (int) (model.getSecPerStep()*1000) - 
                                                System.currentTimeMillis();
		        if (Thread.interrupted()) {
		            throw new InterruptedException();
		        }
                if (sleepTime > 0) {
                    Thread.sleep(sleepTime);
                } else {
                    Thread.sleep(100);
                }
            } catch (InterruptedException ex) {
                isSimulating = false;
            }
        }
        simulationEndStatus = SIMULATION_STOPPED;
        return "Simulation Stopped";
    }
    
    /**
     * Updates the model time text
     */
    protected void updateModelTimeText() {
        modelTimeLabel.setText("Time = " + modelTime);        
    }
    
    /**
     * Writes properties to disk
     */
    protected void writeProperties() {
        if (theApp != null) {
            return;
        }
        String fileName = getPropertiesFileName();
        if (fileName == null) {
            return;
        }
        if (workingDirectory == null || workingDirectory.length() == 0) {
            return;
        }
        FileOutputStream out;
        try {
            out = new FileOutputStream(new File(
                        getPropertiesDirectory(), fileName));
            properties.put("directory", workingDirectory);
            properties.store(out, null);
            out.close();
        } catch (IOException e) {
            return;
        }
    }

    /**
     * Click zoom
     */
    public void zoom(Point p, int flag) {
        view.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        double [] v = view.viewToModel(p);
        Point vp = scrollPane.getViewport().getViewPosition();
        int dx = p.x - vp.x;
        int dy = p.y - vp.y;
        switch (flag) {
        case -1:
            magnification /= ZOOM_FACTOR;
            break;
        case 0:
            magnification = startupMagnification;
            break;
        case 1:
            magnification *= ZOOM_FACTOR;
            break;
        }
        view.resizeContents(magnification);
        xRuler.magnify(magnification);
        yRuler.magnify(magnification);
        view.draw();
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
        setCoordsLabelText("x = " + nf.format(x[0])
                       + ", z = " + nf.format(x[1]));
        view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }

    /**
     * Drag zoom.
     */
    public void zoom(Point mouseDownPoint, Point mouseUpPoint) {
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
        magnification *= Math.min(((double) extentSize.width)/width,
                         ((double) extentSize.height)/height);
        view.resizeContents(magnification);
        xRuler.magnify(magnification);
        yRuler.magnify(magnification);
        view.draw();
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
        setCoordsLabelText("x = " + nf.format(x[0])
                       + ", z = " + nf.format(x[1]));
        view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }

    private class Corner extends JComponent {
        public void paintComponent(Graphics g) {
            g.setColor(Color.white);
            g.fillRect(0, 0, getWidth(), getHeight());
        }
    }
}
