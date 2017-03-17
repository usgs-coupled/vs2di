/*
 * vs2FrameManager.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.help.*;

/**
 * Manages the menus and status bar of the main frame window
 */
public class vs2FrameManager extends mp2FrameManager
            implements vs2Constants {

    /**
     * Menu item to show or hide the textural class window
     */
    protected JCheckBoxMenuItem texturalClassMenuItem;

    /**
     * Menu item to show or hide the chemistry class window
     */
    protected JCheckBoxMenuItem chemistryClassMenuItem;

    /**
     * Menu item to show or hide the evapotranspiration window
     */
    protected JCheckBoxMenuItem evapotranspirationMenuItem;

    /**
     * Menu item to show or hide the recharge period window
     */
    protected JCheckBoxMenuItem rechargePeriodMenuItem;

    /**
     * Menu item to display or hide the textural map on the view
     */
    protected JCheckBoxMenuItem texturalMapMenuItem;
    
    /**
     * Menu item to display or hide the chemistry map on the view
     */
    protected JCheckBoxMenuItem chemistryMapMenuItem;    

    /**
     * Menu item to display or hide the initial conditions for flow on the view
     */
    protected JCheckBoxMenuItem initialFlowMenuItem;

    /**
     * Menu item to display or hide the initial temperature on the view
     */
    protected JCheckBoxMenuItem initialTemperatureMenuItem;

    /**
     * Menu item to display or hide the boundary conditions on the view
     */
    protected JCheckBoxMenuItem boundaryConditionsMenuItem;

    protected JCheckBoxMenuItem fluidSourceMenuItem;

    /**
     * Menu item to display or hide the observation points on the view
     */
    protected JMenuItem observationPointsMenuItem;

    /**
     * Menu item to display or hide the grid on the view
     */
    protected JCheckBoxMenuItem gridMenuItem;

    /**
     * Menu item to display or hide the site map on the view
     */
    protected JCheckBoxMenuItem siteMapMenuItem;

    /**
     * Menu item to active the post processor
     */
    protected JCheckBoxMenuItem postProcessorMenuItem;

    /**
     * The Help menu item
     */
    protected JMenuItem helpMenuItem;

    /**
     * The About menu item
     */
    protected JMenuItem aboutMenuItem;

    /**
     * Label on status bar to provide boundary condition information
     */
    protected JLabel boundaryConditionsLabel;

    /**
     * Label on status bar to provide recharge period information
     */
    protected JLabel rechargePeriodLabel;

    protected JCheckBoxMenuItem bufferedGraphicsMenuItem;

    protected mp2Frame frame;

    /**
     * Items in the data chooser.
     */
    protected static String DATA_CHOOSER_ITEM[] =
            {"Domain",                          // 0
             "Textural Map",                    // 1
             "Chemistry Map",                   // 2  *new
             "Initial Equilibrium Profile",     // 3
             "Initial Temperature",             // 4
             "Boundary Conditions",             // 5
             "Source/Sink Points",              // 6
             "Observation Points",              // 7
             "Grid",                            // 8
             "Site Map"};                       // 9

    /**
     * Constructor
     */
    public vs2FrameManager(mp2App theApp) {
        super(theApp);

        texturalClassMenuItem =
                new JCheckBoxMenuItem("Textural Class Window", false);
        texturalClassMenuItem.setMnemonic(KeyEvent.VK_T);
        chemistryClassMenuItem =
                new JCheckBoxMenuItem("Chemistry Class Window", false);
        chemistryClassMenuItem.setMnemonic(KeyEvent.VK_H);
        evapotranspirationMenuItem =
                new JCheckBoxMenuItem("Evapotranspiration Window", false);
        evapotranspirationMenuItem.setMnemonic(KeyEvent.VK_E);
        rechargePeriodMenuItem =
                new JCheckBoxMenuItem("Recharge Period Window", false);
        rechargePeriodMenuItem.setMnemonic(KeyEvent.VK_R);
        texturalMapMenuItem =
                new JCheckBoxMenuItem("Textural Map", false);
        texturalMapMenuItem.setMnemonic(KeyEvent.VK_M);
        chemistryMapMenuItem =
                new JCheckBoxMenuItem("Chemistry Map", false);
        chemistryMapMenuItem.setMnemonic(KeyEvent.VK_E);
        initialFlowMenuItem =
                new JCheckBoxMenuItem("Initial Pressure Head", false);
        initialFlowMenuItem.setMnemonic(KeyEvent.VK_I);
        initialTemperatureMenuItem
                = new JCheckBoxMenuItem("Initial Temperature", false);
        initialTemperatureMenuItem.setMnemonic(KeyEvent.VK_N);
        boundaryConditionsMenuItem =
                new JCheckBoxMenuItem("Boundary Conditions", false);
        boundaryConditionsMenuItem.setMnemonic(KeyEvent.VK_B);
        fluidSourceMenuItem =
                new JCheckBoxMenuItem("Source/Sink Points", false);
        fluidSourceMenuItem.setMnemonic(KeyEvent.VK_U);
        observationPointsMenuItem =
                new JCheckBoxMenuItem("Observation Points", false);
        observationPointsMenuItem.setMnemonic(KeyEvent.VK_O);
        gridMenuItem =
                new JCheckBoxMenuItem("Grid", false);
        gridMenuItem.setMnemonic(KeyEvent.VK_G);
        siteMapMenuItem =
                new JCheckBoxMenuItem("Site Map", false);
        siteMapMenuItem.setMnemonic(KeyEvent.VK_S);
        postProcessorMenuItem =
                new JCheckBoxMenuItem("Postprocessor", false);
        postProcessorMenuItem.setMnemonic(KeyEvent.VK_P);
        postProcessorMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_F6, 0));
        helpMenuItem = new JMenuItem("Contents...");
        helpMenuItem.setMnemonic(KeyEvent.VK_C);
        aboutMenuItem = new JMenuItem("About...");
        aboutMenuItem.setMnemonic(KeyEvent.VK_A);

        boundaryConditionsLabel = new JLabel("", JLabel.RIGHT);
        rechargePeriodLabel = new JLabel("", JLabel.RIGHT);

        texturalClassMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(TEXTURAL_CLASS, e);
            }
        });
        chemistryClassMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(CHEMISTRY_CLASS, e);
            }
        });
        evapotranspirationMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(EVAPOTRANSPIRATION, e);
            }
        });
        rechargePeriodMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(RECHARGE_PERIOD, e);
            }
        });
        texturalMapMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(TEXTURAL_MAP, e);
            }
        });
        chemistryMapMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(CHEMISTRY_MAP, e);
            }
        });
        initialFlowMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(INITIAL_FLOW, e);
            }
        });
        initialTemperatureMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(INITIAL_TEMPERATURE, e);
            }
        });
        boundaryConditionsMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(BOUNDARY_CONDITIONS, e);
            }
        });
        fluidSourceMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(FLUID_SOURCE, e);
            }
        });
        observationPointsMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(OBSERVATION_POINTS, e);
            }
        });
        gridMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(MODEL_GRID, e);
            }
        });
        siteMapMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(SITE_MAP, e);
            }
        });
        postProcessorMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onShowMenuItem(POST_PROCESSOR, e);
            }
        });
        aboutMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                showAbout();
            }
        });
        bufferedGraphicsMenuItem = new JCheckBoxMenuItem("Buffered Graphics", false);
        bufferedGraphicsMenuItem.setMnemonic(KeyEvent.VK_B);
        bufferedGraphicsMenuItem.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onBufferedGraphics(e.getStateChange()==ItemEvent.SELECTED);
            }
        });
        if (mp2App.useJavaHelp()) {
            mp2JavaHelp.initialize("vs2drtiHelp");
            helpMenuItem.addActionListener(new CSH.DisplayHelpFromSource(mp2JavaHelp.hb));
        } else {
            helpMenuItem.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    mp2HelpWindow.showHelpFile ("contents.html");
                }
            });
        }
    }

    public JMenuItem getMenuItem(int index) {
        switch (index) {
        case TEXTURAL_CLASS:
            return texturalClassMenuItem;
        case CHEMISTRY_CLASS:
            return chemistryClassMenuItem;
        case EVAPOTRANSPIRATION:
            return evapotranspirationMenuItem;
        case RECHARGE_PERIOD:
            return rechargePeriodMenuItem;
        case POST_PROCESSOR:
            return postProcessorMenuItem;
        case BUFFERED_GRAPHICS:
            return bufferedGraphicsMenuItem;
        default:
            return null;
        }
    }

    /**
     * Clears out the infomation text at File New
     */
    public void initializeInformationText() {
        boundaryConditionsLabel.setText("");
        rechargePeriodLabel.setText("");
    }

    /**
     * Prepares the frame for the selected model
     */
    public void prepareFrame(mp2Frame frame) {

//        String helpFolder = vs2App.doHeat() ? "energy" : "solute";
        String helpFolder = "solute";
        if (!mp2App.useJavaHelp()) {
            mp2HelpWindow.setup(theApp.getHomeDirectory(), helpFolder);
        }

        JMenu showMenu = frame.getMenu(SHOW_MENU);
        showMenu.removeAll();

        showMenu.add(texturalClassMenuItem);
        showMenu.add(chemistryClassMenuItem);
        showMenu.add(evapotranspirationMenuItem);
        showMenu.add(rechargePeriodMenuItem);
        showMenu.addSeparator();
        showMenu.add(texturalMapMenuItem);
        showMenu.add(chemistryMapMenuItem);
        showMenu.add(initialFlowMenuItem);
        showMenu.add(initialTemperatureMenuItem);
        showMenu.add(boundaryConditionsMenuItem);
        showMenu.add(fluidSourceMenuItem);
        showMenu.add(observationPointsMenuItem);
        showMenu.add(gridMenuItem);
        showMenu.add(siteMapMenuItem);
        showMenu.addSeparator();
        showMenu.add(postProcessorMenuItem);

        JMenu helpMenu = frame.getMenu(HELP_MENU);
        helpMenu.removeAll();
        helpMenu.add(helpMenuItem);
        helpMenu.add(aboutMenuItem);

        frame.getMenu(OPTIONS_MENU).addSeparator();
        frame.getMenu(OPTIONS_MENU).add(bufferedGraphicsMenuItem);

        JComboBox dataChooser = frame.getDataChooser();
        dataChooser.removeAllItems();
        for (int i=0; i<DATA_CHOOSER_ITEM.length; i++) {
            dataChooser.addItem(DATA_CHOOSER_ITEM[i]);
        }
        dataChooser.setMaximumRowCount(DATA_CHOOSER_ITEM.length);

        JPanel statusBar = frame.getStatusBar();
        statusBar.removeAll();
        //statusBar.add(new JLabel());
        statusBar.add(boundaryConditionsLabel);
        statusBar.add(rechargePeriodLabel);

        this.frame = frame;
    }

    protected void onBufferedGraphics(boolean b) {
        mp2BufferedShapesView.setImageBufferingEnabled(b);
        vs2ModelOptions modelOptions = (vs2ModelOptions) theApp.getDoc().getData(MODEL_OPTIONS);
        if (modelOptions.imageBufferingEnabled != b) {
            modelOptions.imageBufferingEnabled = b;
            theApp.getDoc().setChanged(true);
        }
    }

    /**
     * Invoked when the "Edit|Model Options" menu item is selected.
     */
    protected void onModelOptions() {
        ((vs2Doc) theApp.getDoc()).editModelOptions();
    }

    /**
     * Invoked when an item in the data chooser list is selected.
     *
     * @param  dataName  the name of the data type that was
     *                   selected.
     */
    public void onSelectedData() {
        mp2View view = theApp.getView();
        if (view == null) {
            return;
        }
        mp2DomainData domainData =
            (mp2DomainData) theApp.getDoc().getData(DOMAIN);

        JComboBox dataChooser = theApp.getFrame().getDataChooser();

        String dataName = (String) dataChooser.getSelectedItem();

        if (dataName.equals(DATA_CHOOSER_ITEM[0])) {
            view.setActiveDataView(DOMAIN);
            // Note that the domain is always visible
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[1])) {
            view.setActiveDataView(TEXTURAL_MAP);
            texturalMapMenuItem.setSelected(true);
            // Show textural class window also
            texturalClassMenuItem.setSelected(true);
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[2])) {
            view.setActiveDataView(CHEMISTRY_MAP);
            chemistryMapMenuItem.setSelected(true);
            // Show chemistry class window also
            chemistryClassMenuItem.setSelected(true);
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[3])) {
            view.setActiveDataView(INITIAL_FLOW);
            initialFlowMenuItem.setSelected(true);
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[4])) {
            view.setActiveDataView(INITIAL_TEMPERATURE);
            initialTemperatureMenuItem.setSelected(true);
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[5])) {
            if (domainData.getBoundary(0) == null) {
                mp2MessageBox.showMessageDialog(
                    "Boundary conditions cannot be defined "
                    + "until after the domain is defined.",
                    "Warning");
                dataChooser.setSelectedIndex(0);
            } else {
                view.setActiveDataView(BOUNDARY_CONDITIONS);
                boundaryConditionsMenuItem.setSelected(true);
                // Show the recharge period window also
                rechargePeriodMenuItem.setSelected(true);
            }
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[6])) {
            vs2RechargePeriodData rechargePeriodData =
                (vs2RechargePeriodData) theApp.getDoc().getData(RECHARGE_PERIOD);
            if (rechargePeriodData.getNumberOfRows() == 0) {
                mp2MessageBox.showMessageDialog(
                    "Source/sink points cannot be defined "
                    + "until after recharge periods are defined.",
                    "Warning");
                dataChooser.setSelectedIndex(0);
            } else {
                view.setActiveDataView(FLUID_SOURCE);
                fluidSourceMenuItem.setSelected(true);
            }
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[7])) {
            view.setActiveDataView(OBSERVATION_POINTS);
            observationPointsMenuItem.setSelected(true);
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[8])) {
            if (domainData.getBoundary(0) == null) {
                mp2MessageBox.showMessageDialog(
                    "Grid cannot be defined "
                    + "until after the domain is defined.",
                    "Warning");
                dataChooser.setSelectedIndex(0);
            } else {
                view.setActiveDataView(MODEL_GRID);
                gridMenuItem.setSelected(true);
            }
        }
        else if (dataName.equals(DATA_CHOOSER_ITEM[9])) {
            view.setActiveDataView(SITE_MAP);
            siteMapMenuItem.setSelected(true);
        }
    }

    /**
     * Invoked when a menu item in the "Show" menu is selected.
     */
    protected void onShowMenuItem(int menuItemId, ItemEvent e) {
        mp2View view = theApp.getView();
        mp2Doc doc = theApp.getDoc();
        if (e.getStateChange() == ItemEvent.SELECTED) {
            if (menuItemId == POST_PROCESSOR) {
                if (!doc.readyToExport()) {
                    postProcessorMenuItem.setSelected(false);
                    return;
                }
                if (doc.getFileName().equals(DEFAULT_FILE_NAME)) {
                    mp2MessageBox.showMessageDialog("This is a new document. "
                        + "Please save it before invoking the postprocessor.", "Warning");
                    if (!theApp.saveDocumentAs()) {
                        postProcessorMenuItem.setSelected(false);
                        return;
                    }
                }
                String masterFile = vs2App.getFilePrefix() + ".fil";
                frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                int result = doc.exportData(doc.getDirectory(), masterFile);
                if (result == mp2Doc.UNABLE_TO_EXPORT_ERROR) {
                    mp2MessageBox.showMessageDialog("Unable to export data",
                            "Error");
                    frame.setCursor(Cursor.getDefaultCursor());
                    postProcessorMenuItem.setSelected(false);
                    return;
                }

                vs2PostProcessorFrame postFrame =
                    (vs2PostProcessorFrame) theApp.getPostProcessorFrame();
                if (!postFrame.loadData(doc.getDirectory(), masterFile)) {
                    frame.setCursor(Cursor.getDefaultCursor());
                    postProcessorMenuItem.setSelected(false);
                    return;
                }
                frame.setCursor(Cursor.getDefaultCursor());
                postFrame.setVisible(true);
                frame.setEditable(false);
                return;
            }
            if (view != null) {
                view.setVisible(menuItemId, true);
            }
            if (menuItemId == BOUNDARY_CONDITIONS) {
                boundaryConditionsLabel.setVisible(true);
                rechargePeriodLabel.setVisible(true);
            }
        }
        else {
            if (menuItemId == POST_PROCESSOR) {
                mp2PostProcessorFrame postFrame =
                                    theApp.getPostProcessorFrame();
                if (postFrame.isVisible() && postFrame.quitOK()) {
                    postFrame.setVisible(false);
                    frame.setEditable(true);
                    frame.requestFocus();
                }
                return;
            }
            if (view != null) {
                view.setVisible(menuItemId, false);
            }
            if (menuItemId == BOUNDARY_CONDITIONS) {
                boundaryConditionsLabel.setVisible(false);
                rechargePeriodLabel.setVisible(false);
            }
        }
    }

    public void resetMenuItems() {
        texturalClassMenuItem.setSelected(false);
        chemistryClassMenuItem.setSelected(false);
        evapotranspirationMenuItem.setSelected(false);
        rechargePeriodMenuItem.setSelected(false);
        texturalMapMenuItem.setSelected(false);
        chemistryMapMenuItem.setSelected(false);
        initialFlowMenuItem.setSelected(false);
        initialTemperatureMenuItem.setSelected(false);
        boundaryConditionsMenuItem.setSelected(false);
        fluidSourceMenuItem.setSelected(false);
        observationPointsMenuItem.setSelected(false);
        gridMenuItem.setSelected(false);
        siteMapMenuItem.setSelected(false);
    }

    public void setInformationText(int information, String text) {
        switch (information) {
        case RECHARGE_PERIOD:
            rechargePeriodLabel.setText(text);
            break;
        case BOUNDARY_CONDITIONS:
            boundaryConditionsLabel.setText(text);
            break;
        default:
            super.setInformationText(information, text);
            break;
        }
    }

    protected void showAbout() {
        AboutDialog dlg = new AboutDialog(frame);
        dlg.setVisible(true);
    }

    public void updateDataChooserAndShowMenu(vs2ModelOptions modelOptions) {
        // Update the data chooser
        JComboBox dataChooser = frame.getDataChooser();
        Object selectedItem = dataChooser.getSelectedItem();
        int oldSelectedIndex = dataChooser.getSelectedIndex();
        switch (modelOptions.initialFlowType) {
        case INITIAL_PRESSURE_HEAD:
            DATA_CHOOSER_ITEM[3] = "Initial Pressure Head";
            break;
        case INITIAL_MOISTURE_CONTENT:
            DATA_CHOOSER_ITEM[3] = "Initial Moisture Content";
            break;
        case INITIAL_EQUILIBRIUM_PROFILE:  // fall through
        default:
            DATA_CHOOSER_ITEM[3] = "Initial Equilibrium Profile";
            break;
        }
        dataChooser.removeAllItems();
        for (int i=0; i<DATA_CHOOSER_ITEM.length; i++) {
            if (!((i==4 && !modelOptions.doEnergyTransport) || (i==6 && modelOptions.useRadialCoord) || (i==2 && !modelOptions.doSoluteTransport))) {
                dataChooser.addItem(DATA_CHOOSER_ITEM[i]);
            }
        }
        dataChooser.revalidate();
        frame.getChooserPanel().revalidate();
        if (oldSelectedIndex == 2) {
            dataChooser.setSelectedIndex(2);
        } else {
            dataChooser.setSelectedIndex(0);   // default selection, if selectedItem is no longer in the data chooser
            dataChooser.setSelectedItem(selectedItem);
        }

        // Update the show menu
        initialFlowMenuItem.setText(DATA_CHOOSER_ITEM[3]);
        initialFlowMenuItem.setMnemonic(KeyEvent.VK_I);
        JMenu showMenu = frame.getMenu(SHOW_MENU);
        showMenu.removeAll();
        showMenu.add(texturalClassMenuItem);
        if (modelOptions.doSoluteTransport) {
            showMenu.add(chemistryClassMenuItem);
        }
        if (modelOptions.doEvaporation || modelOptions.doTranspiration) {
            showMenu.add(evapotranspirationMenuItem);
        }
        showMenu.add(rechargePeriodMenuItem);
        showMenu.addSeparator();
        showMenu.add(texturalMapMenuItem);
        if (modelOptions.doSoluteTransport) {
            showMenu.add(chemistryMapMenuItem);
        }
        showMenu.add(initialFlowMenuItem);
        if (modelOptions.doEnergyTransport) {
            showMenu.add(initialTemperatureMenuItem);
        } else {
            initialTemperatureMenuItem.setSelected(false);
        }
        showMenu.add(boundaryConditionsMenuItem);
        if (modelOptions.useRadialCoord) {
            fluidSourceMenuItem.setSelected(false);
        } else {
            showMenu.add(fluidSourceMenuItem);
        }
        showMenu.add(observationPointsMenuItem);
        showMenu.add(gridMenuItem);
        showMenu.add(siteMapMenuItem);
        showMenu.addSeparator();
        showMenu.add(postProcessorMenuItem);
    }

    class AboutDialog extends JDialog {

        public AboutDialog(JFrame frame) {
            super(frame, "About VS2DRTI", true);
            setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            getContentPane().setLayout(new BorderLayout());

            GridBagLayout gridbag = new GridBagLayout();
            GridBagConstraints c = new GridBagConstraints();
            c.insets = new Insets(5, 10, 5, 10);
            JPanel centerPanel = new JPanel(gridbag);
            getContentPane().add(centerPanel, BorderLayout.CENTER);

            JPanel panel = new JPanel(new GridLayout(0, 1));
            c.gridwidth = GridBagConstraints.RELATIVE;
            c.weightx = 2;
            gridbag.setConstraints(panel, c);
            centerPanel.add(panel);
            String program = "VS2DRTI";
            panel.add(new JLabel(program + " - Version " + VS2_VERSION, SwingConstants.CENTER));
            panel.add(new JLabel("U.S. Geological Survey", SwingConstants.CENTER));

            c.gridwidth = GridBagConstraints.REMAINDER;
            c.insets = new Insets(5, 0, 5, 10);
            c.weightx = 1;
            JButton okButton = new JButton("OK");
            gridbag.setConstraints(okButton, c);
            centerPanel.add(okButton);
            okButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    dispose();
                }
            });

            c.insets = new Insets(5, 10, 5, 10);
            c.fill = GridBagConstraints.HORIZONTAL;

            panel = new JPanel(new GridLayout(0, 1));
            gridbag.setConstraints(panel, c);
            centerPanel.add(panel);
            panel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Disclaimer"),
                    new EmptyBorder(2, 5, 5, 5)));
            panel.add(new JLabel("This software is provided on a \"as is\" basis. The user assumes all risk for"));
            panel.add(new JLabel("any damages whatsoever resulting from loss of use, data, or profits arising"));
            panel.add(new JLabel("in connection with the access, use, quality, or performance of this software."));

            panel = new JPanel(new GridLayout(0, 1));
            gridbag.setConstraints(panel, c);
            centerPanel.add(panel);
            panel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Notice"),
                    new EmptyBorder(2, 5, 5, 5)));
            panel.add(new JLabel("A component of this software uses the program triangle.c version 1.3, written"));
            panel.add(new JLabel("and copyrighted by Jonathan Richard Shewchuk (jrs@cs.berkeley.edu). The author"));
            panel.add(new JLabel("of triangle.c has granted free distribution of that program for noncommercial"));
            panel.add(new JLabel("use. Distribution of triangle.c as part of a commerical system is permissible"));
            panel.add(new JLabel("only by direct arrangement with the author of that program."));

            panel = new JPanel(new GridLayout(0, 1));
            gridbag.setConstraints(panel, c);
            centerPanel.add(panel);
            panel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Credits"),
                    new EmptyBorder(2, 5, 5, 5)));
            panel.add(new JLabel(program + " was written by:"));
            panel.add(new JLabel("Paul Hsieh (pahsieh@usgs.gov), U.S. Geological Survey, Menlo Park, CA"));
            panel.add(new JLabel("William Wingle (wwingle@mines.edu), Colorado School of Mines, Golden, CO"));
            panel.add(new JLabel("Richard Healy (rwhealy@usgs.gov), U.S. Geological Survey, Denver, CO"));

            panel = new JPanel(new GridLayout(0, 1));
            gridbag.setConstraints(panel, c);
            centerPanel.add(panel);
            panel.setBorder(new CompoundBorder(
                    BorderFactory.createTitledBorder("Contact"),
                    new EmptyBorder(2, 5, 5, 5)));
            panel.add(new JLabel("For additional information, please contact:"));
            panel.add(new JLabel("Richard Healy (rwhealy@usgs.gov)"));

            pack();

            Dimension screenSize = getToolkit().getScreenSize();
            Dimension dlgSize = getSize();
            int left = (screenSize.width - dlgSize.width)/2;
            int top = (screenSize.height - dlgSize.height)/2;
            if (left < 0) {
                left = 0;
            }
            if (top < 0) {
                top = 0;
            }
            setLocation(left, top);
        }
    }
}
