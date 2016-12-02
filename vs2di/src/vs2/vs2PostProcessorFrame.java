/*
 * vs2PostProcessorFrame.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.help.*;


public class vs2PostProcessorFrame extends mp2PostProcessorFrame implements vs2Constants {

    protected int usage;
    protected mp2ToggleButton zonationButton;
    protected int firstComponentIndex;

    /**
     * Creates a post processor in interactive mode
     */
    public vs2PostProcessorFrame(mp2App app) {
        this(app, null);
    }

    /**
     * Creates a post processor
     */
    public vs2PostProcessorFrame(mp2App app, String home) {
        super(app, home);
        if (theApp != null) {
            usage = SOLUTE_AND_ENERGY_TRANSPORT;
        } else {
            usage = USAGE_UNDEFINED;    
        }
        displayChooser.addItem("Total Head");
        displayChooser.addItem("Pressure Head");
        displayChooser.addItem("Moisture Content");
        displayChooser.addItem("Saturation");
        displayChooser.addItem("Temperature");
        displayChooser.addItem("Concentration");
        displayChooser.addItem("Vector");
        displayChooser.addItem("None");
        displayChooser.setMaximumRowCount(10);
        
        JMenu helpMenu = new JMenu("Help");
        helpMenu.setMnemonic(KeyEvent.VK_H);
        JMenuItem helpMenuItem;
        helpMenuItem = new JMenuItem("Postprocessor Help...");
        helpMenuItem.setMnemonic(KeyEvent.VK_P);
        menuBar.add(helpMenu);
        helpMenu.add(helpMenuItem);
        
        if (theApp == null) {
            // Running in standalone mode.
            if (mp2App.useJavaHelp()) {
                mp2JavaHelp.initialize("vs2postHelp");
                helpMenuItem.addActionListener(new CSH.DisplayHelpFromSource(mp2JavaHelp.hb));
            } else {
                mp2HelpWindow.setup(home, "post");
                helpMenuItem.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        onBrowserHelp();
                    }
                });
            }
        } else {
            if (mp2App.useJavaHelp()) {
	            mp2JavaHelp.hb.enableHelpOnButton(helpMenuItem, "postprocessor", null);
            } else {
                helpMenuItem.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        onBrowserHelp();
                    }
                });
            }
        }
        
        toolBar.add(Box.createVerticalStrut(5));
        toolBar.add(zonationButton = new mp2ToggleButton(new ImageIcon(ClassLoader.getSystemResource("images/zonation.gif"))));
        zonationButton.setToolTipText("Show or hide boundaries between textural classes");
        zonationButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                onZonationButton(e.getStateChange() == ItemEvent.SELECTED);
            }
        });

        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        setLocation(screenSize.width/10, screenSize.height/10);
        setSize(3*screenSize.width/4, 3*screenSize.height/4);

        scrollPane.getHorizontalScrollBar().setUnitIncrement(10);
        scrollPane.getHorizontalScrollBar().validate();
        scrollPane.getVerticalScrollBar().setUnitIncrement(10);
        scrollPane.getVerticalScrollBar().validate();
        getContentPane().validate();
    }
    
    protected void onExit() {
        if (theApp != null && model.getType() == mp2Model.COMPUTATIONAL) {
            vs2PostProcessorOptions po = 
                (vs2PostProcessorOptions) theApp.getDoc().getData(POST_PROCESSOR_OPTIONS);
            int display = displayChooser.getSelectedIndex();
            int mode = drawingModeChooser.getSelectedIndex();
            boolean zone = zonationButton.isSelected();
            boolean vector = vectorButton.isSelected();
            if (po.startupDisplay != display || po.startupDrawingMode != mode
                    || po.showZonationAtStartup != zone || po.showVectorAtStartup != vector) {
                theApp.getDoc().setChanged(true);
            }
            po.startupDisplay = display;
            po.startupDrawingMode = mode;
            po.showZonationAtStartup = zone;
            po.showVectorAtStartup = vector;
        }
        super.onExit();
        if (model.getType() == mp2Model.COMPUTATIONAL) {
            // release memory if not cancelled in super.onExit() / quitOK
            if (theApp != null && !theApp.getFrame().getMenuItem(POST_PROCESSOR).isSelected()) {
                ((vs2ComputationalModel) model).releaseMemory();
            }
        }
    }

    /**
     * Creates a computational model
     */
    protected mp2ComputationalModel createComputationalModel() {
        if (theApp != null) {
            return new vs2drt();
        } else {
            return null;
        }
    }
    
    /**
     * Creates a playback model
     */
    protected mp2PlaybackBinary createPlaybackModel() {
        return new vs2PlaybackBinary();
    }
    
    /**
     * Creates a postprocessor view
     */
    protected mp2PostProcessorView createPostProcessorView() {
        return new vs2PostProcessorView(this);
    }
    
    /**
     * Gets the name of the output file for the computational model to
     * write binary data
     */
    protected String getComputationalOutputFile() {
        return "vs2drt.sim";
    }
    
    protected String getPostprocessorTitle() {
        if (theApp != null) {
            return "VS2DRTI Postprocessor";
        } else {
            return "VS2DRTI Postprocessor--Standalone Mode";
        }
    }

    /**
     * Gets the directory containing the properties files
     */
    protected String getPropertiesDirectory() {
        if (System.getProperty("os.name").startsWith("Windows")) {
            String drive = System.getenv("HOMEDRIVE");
            String path = System.getenv("HOMEPATH");
            return drive + path;
        }
        return homeDirectory + System.getProperty("file.separator") + "bin";
    }
    
    /**
     * Gets the file name for holding properties
     */
    protected String getPropertiesFileName() {
        return "vs2post.properties";
    }
    
    /**
     * Gets the usage (solute vs energy transport)
     */
    public int getUsage() {
        return usage;
    }

    /**
     * Loads data
     */
    public boolean loadData(String directory, String file) {
        System.out.println("loadData in");
        
        if (theApp == null && !findModelTypeAndUsage(directory, file)) {
            System.out.println("loadData out 1");
            return false;
        }

        if (!super.loadData(directory,file)) {
            System.out.println("loadData out 2");
            return false;
        }
        if (theApp != null) {
            displayChooser.removeAllItems();
            displayChooser.addItem("Total Head");
            displayChooser.addItem("Pressure Head");
            displayChooser.addItem("Moisture Content");
            displayChooser.addItem("Saturation");
            vs2ModelOptions modelOptions = (vs2ModelOptions) theApp.getDoc().getData(MODEL_OPTIONS);
            if (modelOptions.doEnergyTransport) {
                displayChooser.addItem("Temperature");
            }
            if (modelOptions.doSoluteTransport) {
                assert(computationalModel != null);
                firstComponentIndex = displayChooser.getItemCount();
                String [] comps = ((vs2ComputationalModel) computationalModel).getComponents();
                if (comps.length > 1) {
                    for (int i=0; i<comps.length; ++i) {
                        displayChooser.addItem("Concentration (" + comps[i] + ")");
                    }
                } else {
                    displayChooser.addItem("Concentration");
                }
            }
            displayChooser.addItem("Vector");
            displayChooser.addItem("None");
            displayChooser.revalidate();
            vectorButton.setVisible(true);
            vectorMenuItem.setEnabled(true);
            vs2PostProcessorOptions po = 
                (vs2PostProcessorOptions) theApp.getDoc().getData(POST_PROCESSOR_OPTIONS);
            if (po.startupDisplay < displayChooser.getItemCount()) {
                displayChooser.setSelectedIndex(po.startupDisplay);
            }
            drawingModeChooser.setSelectedIndex(po.startupDrawingMode);
            zonationButton.setSelected(po.showZonationAtStartup);
            vectorButton.setSelected(po.showVectorAtStartup);
            toolBar.revalidate();
        } else if (doViewer) {
            loadFileMenuForViewer();
            displayChooser.removeAllItems();
            displayChooser.addItem("Total Head");
            displayChooser.addItem("Pressure Head");
            displayChooser.addItem("Moisture Content");
            displayChooser.addItem("Saturation");
            if (((vs2ComputationalModel) computationalModel).getDoEnergyTransport()) {
                displayChooser.addItem("Temperature");
            }
            if (((vs2ComputationalModel) computationalModel).getDoSoluteTransport()) {
                firstComponentIndex = displayChooser.getItemCount();
                String [] comps = ((vs2ComputationalModel) computationalModel).getComponents();
                if (comps.length > 1) {
                    for (int i=0; i<comps.length; ++i) {
                        displayChooser.addItem("Concentration (" + comps[i] + ")");
                    }
                } else {
                    displayChooser.addItem("Concentration");
                }
            }
            displayChooser.addItem("Vector");
            displayChooser.addItem("None");
            displayChooser.revalidate();
            vectorButton.setVisible(true);
            vectorMenuItem.setEnabled(true);
            toolBar.revalidate();
        } else {
            loadFileMenuForPlayback();
            displayChooser.removeAllItems();
            displayChooser.addItem("Total Head");
            displayChooser.addItem("Pressure Head");
            vs2PlaybackBinary pb = (vs2PlaybackBinary) playbackBinaryModel;
            if (pb.getDoMoistureContent()) {
                displayChooser.addItem("Moisture Content");
            }
            if (pb.getDoSaturation()) {
                displayChooser.addItem("Saturation");
            }
            if (pb.getDoEnergyTransport()) {
                displayChooser.addItem("Temperature");
            }
            if (pb.getDoSoluteTransport()) {
                firstComponentIndex = displayChooser.getItemCount();
                String [] comps = pb.getComponents();
                if (comps != null) {
                    for (int i=0; i<comps.length; ++i) {
                        displayChooser.addItem("Concentration (" + comps[i] + ")");
                    }
                } else {
                    displayChooser.addItem("Concentration");
                }
            }
            if (pb.getDoVector()) {
                displayChooser.addItem("Vector");
                vectorButton.setVisible(true);
                vectorMenuItem.setEnabled(true);
                toolBar.revalidate();
            } else {
                vectorButton.setSelected(false);
                vectorButton.setVisible(false);
                vectorMenuItem.setEnabled(false);
                toolBar.revalidate();
            }
            displayChooser.addItem("None");
            displayChooser.revalidate();
        }
        System.out.println("loadData out 3");
        return true;
    }

    protected boolean findModelTypeAndUsage(String directory, String filename) {
        if (filename.endsWith(".sim")) {
            doViewer = false;
            return true;
        } else if (filename.endsWith(".fil")) {
            doViewer = true;
        } else {
            vs2FileTypeDialog dlg = new vs2FileTypeDialog(this, filename);
            dlg.doViewer = doViewer;
            if (dlg.doModal() == false) {
                return false;
            }
            doViewer = dlg.doViewer;
            if (!doViewer) {
                return true;
            }
        }
        try {
            File file = new File(directory, filename);
            BufferedReader in = new BufferedReader(new FileReader(file));
            String line;
            line = in.readLine();
            line = in.readLine();
            line = in.readLine();
            line = in.readLine();
            line = in.readLine();
            line = in.readLine();
            line = in.readLine();
            if (!line.startsWith("#")) {
                line = in.readLine();
            }
            if (!line.startsWith("#")) {
                line = in.readLine();
            }
            in.close ();
            if (line != null && line.length() > 3 && line.startsWith("#")) {
                String code = line.substring(1).trim().toLowerCase();
                if (code.startsWith("vs2drt1.")) {
                    usage = SOLUTE_AND_ENERGY_TRANSPORT;
                    computationalModel = new vs2drt();
                    return true;
                } else if (code.startsWith("vs2dt3.")) {
                    usage = SOLUTE_TRANSPORT;
                    computationalModel = new vs2dt();
                    return true;
                } else if (code.startsWith("vs2dh3.")) {
                    usage = ENERGY_TRANSPORT;
                    computationalModel = new vs2dh();
                    return true;
                } else if (code.startsWith("vs2dt2.5") && code.length() == 10) {
                    int hydr = -1;
                    int adsorp = - 1;
                    char func = code.charAt(8);
                    if (func == 'b') {
                        hydr = 0;
                    } else if (func == 'v') {
                        hydr = 1;
                    } else if (func == 'h') {
                        hydr = 2;
                    } else if (func == 't') {
                        hydr = 3;
                    }
                    char sorp = code.charAt(9);
                    if (sorp == '1') {
                        adsorp = 1;
                    } else if (sorp == '2') {
                        adsorp = 2;
                    } else if (sorp == '3') {
                        adsorp = 3;
                    } else if (sorp == '4') {
                        adsorp = 4;
                    } else if (sorp == '5') {
                        adsorp = 5;
                    } else if (sorp == '6') {
                        adsorp = 6;
                    } else if (sorp == '7') {
                        adsorp = 7;
                    }
                    if (hydr != -1 && adsorp != -1) {
                        usage = SOLUTE_TRANSPORT;
                        computationalModel = new vs2dt(hydr, adsorp);
                        return true;
                    }
                } else if (code.startsWith("vs2dh1.0") && code.length() == 9) {
                    int hydr = -1;
                    char func = code.charAt(8);
                    if (func == 'b') {
                        hydr = 0;
                    } else if (func == 'v') {
                        hydr = 1;
                    } else if (func == 'h') {
                        hydr = 2;
                    } else if (func == 't') {
                        hydr = 3;
                    }
                    if (hydr != -1) {
                        usage = ENERGY_TRANSPORT;
                        computationalModel = new vs2dh(hydr);
                        return true;
                    }
                }
            }
            vs2ModelTypeDialog dlg = new vs2ModelTypeDialog(this, filename);
            if (computationalModel != null) {
                vs2ComputationalModel cm = (vs2ComputationalModel) computationalModel;
                dlg.usage = usage;
                dlg.useOldVersion = cm.getUseOldVersion();
                dlg.hydraulicFunctionType = cm.getHydraulicFunctionType();
                dlg.adsorptionType = cm.getAdsorptionType();
            } else {
                dlg.usage = SOLUTE_TRANSPORT;
                dlg.useOldVersion = false;
                dlg.hydraulicFunctionType = 1;
                dlg.adsorptionType = 1;
            }
            dlg.usage = usage;
            if (dlg.doModal() == false) {
                return false;
            }
            usage = dlg.usage;
            if (usage == ENERGY_TRANSPORT) {
                if (dlg.useOldVersion) {
                    computationalModel = new vs2dh(dlg.hydraulicFunctionType);
                } else {
                    computationalModel = new vs2dh();
                }
            } else {
                if (dlg.useOldVersion) {
                    computationalModel = new vs2dt(dlg.hydraulicFunctionType, 
                                                   dlg.adsorptionType);
                } else {
                    computationalModel = new vs2dt();
                }
            }
        } catch (IOException e) {
            return false;
        }
        return true;
    }
    
    /**
     * Entry point when running post processor as standalone app
     */
    public static void main(String [] args) {
        mp2FileChooser.useJFileChooser(true);
        try {
            // Use windows look and feel when running under windows
            if (System.getProperty("os.name").startsWith("Windows")) {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                // When launched by a windows exe program to run on jre, the
                // javax.swing.JFileChooser doesn't work so we use java.awt.FileDialog
                // instead
                //
                // SRC 08/26/2016 seems to be working on 1.8 
                // mp2FileChooser.useJFileChooser(false);
            }
        } catch (Exception e) {
            System.out.println("Error loading L&F: " + e);
        }
        vs2PostProcessorFrame frame = new vs2PostProcessorFrame(null);
        Image appIcon = Toolkit.getDefaultToolkit().getImage(
                                    ClassLoader.getSystemResource("images/posticon.gif"));
        frame.setIconImage(appIcon);
        frame.setVisible(true);
    }

    /**
     * Selects the type of display
     */
    protected void onDisplayChooser() {
        String item = (String) displayChooser.getSelectedItem();
        if (item.equals("Pressure Head")) {
            view.setDisplay(vs2PostProcessorView.PRESSURE_HEAD);
        }
        else if (item.equals("Moisture Content")) {
            view.setDisplay(vs2PostProcessorView.MOISTURE_CONTENT);
        }
        else if (item.equals("Saturation")) {
            view.setDisplay(vs2PostProcessorView.SATURATION);
        }
        else if (item.equals("Temperature")) {
            view.setDisplay(vs2PostProcessorView.ENERGY_TRANSPORT);
        }
        else if (item.startsWith("Concentration")) {
            ((vs2PostProcessorView)view).setDisplayString(item);
            int n = displayChooser.getSelectedIndex() - firstComponentIndex;
            ((vs2PostProcessorView)view).setComponentIndex(n);
            // must set index before setting display
            view.setDisplay(vs2PostProcessorView.SOLUTE_TRANSPORT);
        }
        else if (item.equals("Vector")) {
            view.setDisplay(vs2PostProcessorView.VECTOR);
        }
        else if (item.equals("None")) {
            view.setDisplay(vs2PostProcessorView.NO_DISPLAY);
        }
        if (item.equals("Total Head")) {
            view.setDisplay(vs2PostProcessorView.TOTAL_HEAD);
        }
    }
    
    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile ("postprocessor.html");
    }
    
    protected void onResetPlaybackToBeginning() {
        // If we have just finished computation, we need to release memory.
        if ((model.getType() == mp2Model.COMPUTATIONAL) && computationalModel.getSaveOutputAsBinary()) {
            ((vs2ComputationalModel) computationalModel).releaseMemory();
        }
        boolean displayChooserRequiresUpdate = (model.getType() == mp2Model.COMPUTATIONAL);
        String selectedDisplay = (String) displayChooser.getSelectedItem();
        super.onResetPlaybackToBeginning();
        if (displayChooserRequiresUpdate) {
            displayChooser.removeAllItems();
            displayChooser.addItem("Total Head");
            displayChooser.addItem("Pressure Head");
            vs2PlaybackBinary pb = (vs2PlaybackBinary) playbackBinaryModel;
            if (pb.getDoMoistureContent()) {
                displayChooser.addItem("Moisture Content");
            }
            if (pb.getDoSaturation()) {
                displayChooser.addItem("Saturation");
            }
            if (pb.getDoEnergyTransport()) {
                displayChooser.addItem("Temperature");
            }
            if (pb.getDoSoluteTransport()) {
                firstComponentIndex = displayChooser.getItemCount();
                String [] comps = pb.getComponents();
                if (comps.length > 1) {
                    for (int i=0; i<comps.length; ++i) {
                        displayChooser.addItem("Concentration (" + comps[i] + ")");
                    }
                } else {
                    displayChooser.addItem("Concentration");
                }
            }
            if (pb.getDoVector()) {
                displayChooser.addItem("Vector");
            } else {
                vectorMenuItem.setEnabled(false);
                vectorButton.setSelected(false);
                vectorButton.setVisible(false);
                toolBar.revalidate();
            }
            displayChooser.addItem("None");
            displayChooser.revalidate();
            displayChooser.setSelectedItem(selectedDisplay);
        }
    }  
    
    protected void onRestartComputation() {
        boolean displayChooserRequiresUpdate = (model.getType() == mp2Model.PLAYBACK_BINARY);
        String selectedDisplay = (String) displayChooser.getSelectedItem();
        super.onRestartComputation();
        if (restartComputationMenuItem.isEnabled()) {
            // user pressed No to "Do you want to restart the computation?"
            return;
        }
        if (displayChooserRequiresUpdate) {
            displayChooser.removeAllItems();
            displayChooser.addItem("Total Head");
            displayChooser.addItem("Pressure Head");
            displayChooser.addItem("Moisture Content");
            displayChooser.addItem("Saturation");
            if (((vs2ComputationalModel) computationalModel).getDoEnergyTransport()) {
                displayChooser.addItem("Temperature");
            }
            if (((vs2ComputationalModel) computationalModel).getDoSoluteTransport()) {
                firstComponentIndex = displayChooser.getItemCount();
                String [] comps = ((vs2ComputationalModel) computationalModel).getComponents();
                if (comps.length > 1) {
                    for (int i=0; i<comps.length; ++i) {
                        displayChooser.addItem("Concentration (" + comps[i] + ")");
                    }
                } else {
                    displayChooser.addItem("Concentration");
                }
            }
            displayChooser.addItem("Vector");
            displayChooser.addItem("None");
            displayChooser.revalidate();
            displayChooser.setSelectedItem(selectedDisplay);
            vectorButton.setVisible(true);
            vectorMenuItem.setEnabled(true);
            toolBar.validate();
        }
    }
    
    /**
     * Performs cleanup when run is aborted.
     */
    protected void onRunAborted() {
        switch (runStatus){
        case 2:
            mp2MessageBox.showMessageDialog(this,
                    "Rotation angle must be between"
                    + " -90 and +90 degrees.  Simulation halted.",
                    "Warning");
            break;
        case 3:
            mp2MessageBox.showMessageDialog(this,
                    "Number of nodes exceeds array sizes.  Simulation halted."
                    + " Reduce grid size or redimension arrays in vs2dt.f",
                    "Warning");
            break;
        case 4:
            mp2MessageBox.showMessageDialog(this,
                    "Initial moisture content at a node is less than 0."
                    + " Simulation halted",
                    "Warning");
            break;
        case 5:
            mp2MessageBox.showMessageDialog(this,
                    "Simulation terminated.  Length of all recharge "
                    + "periods is less than TMAX.  Simulation halted.",
                    "Warning");
            break;
        case 6:
            mp2MessageBox.showMessageDialog(this,
                    "Initial moisture content at a node is less than residual"
                    + " Simulation halted.",
                    "Warning");
            break;
        case 7:
            mp2MessageBox.showMessageDialog(this,
                    "Initial conditions in terms of moisture content is not"
                    + " allowed with tabular data.  Simulation halted.",
                    "Warning");
            break;
        case 8:
            mp2MessageBox.showMessageDialog(this,
                    "Maximum number of time steps exceeded"
                    + " Simulation halted.",
                    "Warning");
            break;
        case 9:
            mp2MessageBox.showMessageDialog(this,
                    "Maximum number of iterations exceeded for "
                    + " flow equation.  Simulation halted.",
                    "Warning");
            break;
        case 10:
            mp2MessageBox.showMessageDialog(this,
                    "Maximum number of iterations exceeded for"
                    + " transport equation.  Simulation halted.",
                    "Warning");
            break;
        case 11:
            mp2MessageBox.showMessageDialog(this,
                    "Convergence of flow and transport equation not attained. "
                    + "Simulation halted.",
                    "Warning");
            break;
        case 12:
            mp2MessageBox.showMessageDialog(this,
                    "Error in PhreeqcRM. See *.log.txt file. "
                    + "Simulation halted.",
                    "Warning");
            break;
        case 99:
            mp2MessageBox.showMessageDialog(this,
                    "Read error encountered.  Playback halted.",
                    "Warning");
            break;
        case 201:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-1:  TITL.  Simulation halted.",
                    "Warning");
            break;
        case 202:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-2:  TMAX,STIM,ANG.  Simulation halted.",
                    "Warning");
            break;
        case 203:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-3:  ZUNIT,TUNIT,CUNX,HUNX.  Simulation halted.",
                    "Warning");
            break;
        case 204:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-4:  NXR,NLY.  Simulation halted.",
                    "Warning");
            break;
        case 205:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-5:  NRECH,NUMT.  Simulation halted.",
                    "Warning");
            break;
        case 206:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-6:  RAD,ITSTOP,HEAT,SOLUTE.  Simulation halted.",
                    "Warning");
            break;
        case 207:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-7:  CHEMFILE.  Simulation halted.",
                    "Warning");
            break;
        case 208:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-8:  DATABASEFILE.  Simulation halted.",
                    "Warning");
            break;
        case 209:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-9:  PREFIX.  Simulation halted.",
                    "Warning");
            break;
        case 210:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-10: CIS,CIT.  Simulation halted.",
                    "Warning");
            break;
        case 211:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-11: INPRXZ.  Simulation halted.",
                    "Warning");
            break;
        case 212:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-12:  F11P,F7P,F8P,F9P,F6P.  Simulation halted.",
                    "Warning");
            break;
        case 213:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-13:  THPT,SPNT,PPNT,HPNT,VPNT.  Simulation halted.",
                    "Warning");
            break;
        case 214:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-14:  IFAC,FACX.  Simulation halted.",
                    "Warning");
            break;
        case 215:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-15: (DXR(K),K=1,NXR).  Simulation halted.",
                    "Warning");
            break;
        case 216:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-16:  XMULT,XMAX.  Simulation halted.",
                    "Warning");
            break;
        case 217:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-17:  JFAC,FACZ.  Simulation halted.",
                    "Warning");
            break;
        case 218:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-18:  (DELZ(K),K=1,NLY).  Simulation halted.",
                    "Warning");
            break;
        case 219:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-19:  ZMULT,ZMAX.  Simulation halted.",
                    "Warning");
            break;
        case 220:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-20:  NPLT.  Simulation halted.",
                    "Warning");
            break;
        case 221:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-21:  (PLTIM(K),K=1,NPLT).  Simulation halted.",
                    "Warning");
            break;
        case 222:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-22:  NOBS.  Simulation halted.",
                    "Warning");
            break;
        case 223:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-23:  ((KDUM(K,J),J=1,2),K=1,NOBS).  Simulation halted.",
                    "Warning");
            break;
        case 224:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-24: NMB9.  Simulation halted.",
                    "Warning");
            break;
        case 225:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading A-25:  (MB9(K),K=1,NMB9).  Simulation halted.",
                    "Warning");
            break;
        case 226:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-1:  EPS,HMAX,WUS.  Simulation halted.",
                    "Warning");
            break;
        case 227:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-2: EPS1,EPS2.  Simulation halted.",
                    "Warning");
            break;
        case 228:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-3: EPS3.  Simulation halted.",
                    "Warning");
            break;
        case 229:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-4:  MINIT,ITMAX.  Simulation halted.",
                    "Warning");
            break;
        case 230:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-5:  PHRD.  Simulation halted.",
                    "Warning");
            break;
        case 231:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-6:  NTEX,NPROP.  Simulation halted.",
                    "Warning");
            break;
        case 232:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-7:  hydraulicFunctionType.  Simulation halted.",
                    "Warning");
            break;
        case 233:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-8:  ITEX.  Simulation halted.",
                    "Warning");
            break;
        case 234:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-9:  ANIZ(J),(HK(J,I),I=1,NPROP).  Simulation halted.",
                    "Warning");
            break;
        case 235:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-10:  (HT(j,I),I=1,6).  Simulation halted.",
                    "Warning");
            break;
        case 236:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-11:  (HS(j,I),I=1,3),(ITEXSOL(J,I),I=1,7).  Simulation halted.",
                    "Warning");
            break;
        case 237:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-12:  IROW.  Simulation halted.",
                    "Warning");
            break;
        case 238:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-13:  (JTEX(N),N=1,NXR).  Simulation halted.",
                    "Warning");
            break;
        case 239:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-14:  IL,IR,JBT,JRD.  Simulation halted.",
                    "Warning");
            break;
        case 240:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-15:  IREAD,FACTOR.  Simulation halted.",
                    "Warning");
            break;
        case 241:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-16:  DWTX,HMIN.  Simulation halted.",
                    "Warning");
            break;
        case 242:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-17: IU,IFMT.  Simulation halted.",
                    "Warning");
            break;
        case 243:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-18:  BCIT,ETSIM.  Simulation halted.",
                    "Warning");
            break;
        case 244:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-19: NPV,ETCYC.  Simulation halted.",
                    "Warning");
            break;
        case 245:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-20: (PEVAL(I),I=1,NPV).  Simulation halted.",
                    "Warning");
            break;
        case 246:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-21: (RDC(1,I),I=1,NPV).  Simulation halted.",
                    "Warning");
            break;
        case 247:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-22: (RDC(2,I),I=1,NPV).  Simulation halted.",
                    "Warning");
            break;
        case 248:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-23: (PTVAL(I),I=1,NPV).  Simulation halted.",
                    "Warning");
            break;
        case 249:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-24:  (RDC(3,I),I=1,NPV).  Simulation halted.",
                    "Warning");
            break;
        case 250:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-25:  (RDC(4,I),I=1,NPV).  Simulation halted.",
                    "Warning");
            break;
        case 251:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-26:  (RDC(5,I),I=1,NPV).  Simulation halted.",
                    "Warning");
            break;
        case 252:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-27:  (RDC(6,I),I=1,NPV).  Simulation halted.",
                    "Warning");
            break;
        case 253:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-28:  IREAD,FACTOR.  Simulation halted.",
                    "Warning");
            break;
        case 254:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-29: IU,IFMT.  Simulation halted.",
                    "Warning");
            break;
        case 255:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-30:  IREAD.  Simulation halted.",
                    "Warning");
            break;
        case 256:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-31:  INSOL1.  Simulation halted.",
                    "Warning");
            break;
        case 257:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-32:  INDSOL.  Simulation halted.",
                    "Warning");
            break;
        case 258:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-33:  numBF, maxnumcells.  Simulation halted.",
                    "Warning");
            break;
        case 259:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-34:  idBF(i), numcellsBF(i).  Simulation halted.",
                    "Warning");
            break;
        case 260:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading B-35:  jj,nn.  Simulation halted.",
                    "Warning");
            break;
        case 261:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-1:  TPER,DELT.  Simulation halted.",
                    "Warning");
            break;
        case 262:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-2:  TMLT,DLTMX,DLTMIN,TRED.  Simulation halted.",
                    "Warning");
            break;
        case 263:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-3:  DSMAX,STERR.  Simulation halted.",
                    "Warning");
            break;
        case 264:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-4:  POND.  Simulation halted.",
                    "Warning");
            break;
        case 265:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-5:  PRNT.  Simulation halted.",
                    "Warning");
            break;
        case 266:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-6:  BCIT,ETSIM,SEEP.  Simulation halted.",
                    "Warning");
            break;
        case 267:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-7:  NFCS.  Simulation halted.",
                    "Warning");
            break;
        case 268:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-8:  JJ,JLAST(K) .  Simulation halted.",
                    "Warning");
            break;
        case 269:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-9:  ((JSPX(L,J,K),L=2,3),J=1,JJ).  Simulation halted.",
                    "Warning");
            break;
        case 270:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-10:  IBC .  Simulation halted.",
                    "Warning");
            break;
        case 271:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-11:  JJ,NN,NTX,PFDUM.  Simulation halted.",
                    "Warning");
            break;
        case 272:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-12: NTT,TF.  Simulation halted.",
                    "Warning");
            break;
        case 273:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-13: NTC,INSBC1.  Simulation halted.",
                    "Warning");
            break;
        case 274:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-14:  JJ,NN,NTX,PFDUM.  Simulation halted.",
                    "Warning");
            break;
        case 275:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-15:  JJT,JJB,NNL,NNR,NTX,PFDUM.  Simulation halted.",
                    "Warning");
            break;
        case 276:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-16: NTT,TF.  Simulation halted.",
                    "Warning");
            break;
        case 277:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-17: NTC,INSBC1 .  Simulation halted.",
                    "Warning");
            break;
        case 278:
            mp2MessageBox.showMessageDialog(this,
                    "Error reading C-18:  JJT,JJB,NNL,NNR,NTX,PFDUM.  Simulation halted.",
                    "Warning");
            break;
        default:
            break;
        }
        super.onRunAborted();
    }
    
    protected void onSimulationOptions() {
        boolean computing = (model.getType() == mp2Model.COMPUTATIONAL);
        vs2SimulationDialog dlg = new vs2SimulationDialog(this, computing);
        dlg.showDataList = computing && (!computationalModel.doesOutputStreamExist());
        dlg.secPerStep = model.getSecPerStep();
        vs2ComputationalModel cm = (vs2ComputationalModel) computationalModel;
        if (computing) {
            dlg.saveInterval = cm.getSaveInterval();
            dlg.saveBinary = cm.getSaveOutputAsBinary();
            if (dlg.showDataList) {
                dlg.saveMoistureContent = cm.getSaveMoistureContent();
                dlg.saveSaturation = cm.getSaveSaturation();
                dlg.saveVectors = cm.getSaveVectors();
            }
        }

        if (dlg.doModal() == true) {
            model.setSecPerStep(dlg.secPerStep);
            if (computing) {
                cm.setSaveInterval(dlg.saveInterval);
                cm.setSaveOutputAsBinary(dlg.saveBinary);
                if (dlg.showDataList) {
                    cm.setSaveMoistureContent(dlg.saveMoistureContent);
                    cm.setSaveSaturation(dlg.saveSaturation);
                    cm.setSaveVectors(dlg.saveVectors);
                }
            }
            if (theApp != null) {
                theApp.getDoc().setChanged(true);
            }
        }
    }
    
    
    protected void onZonationButton(boolean b) {
        ((vs2PostProcessorView) view).showZonation(b);
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
            
            line = in.readLine();
            int colorScaleCount = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());

            java.util.Map<String, mp2ColorScale> map = ((vs2ComputationalModel)computationalModel).getColorScaleMap();
            double valueRed, valueBlue, colorInterval, labelInterval;
            for (int i = 0; i < colorScaleCount; ++i) {
                line = in.readLine();
                String k = line.substring(line.indexOf(':')+1).trim();

                mp2ColorScale v = new mp2ColorScale();
                v.init();
                
                line = in.readLine();
                valueRed = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
                line = in.readLine();
                valueBlue = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
                line = in.readLine();
                colorInterval = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
                line = in.readLine();
                labelInterval = Double.valueOf(line.substring(line.indexOf(':')+1).trim()).doubleValue();
                
                v.SetLimits(valueBlue, valueRed);
                v.SetColorInterval(colorInterval);
                v.SetLabelInterval(labelInterval);
                
                map.put(k, v);
            }
            
            vs2ComputationalModel cm = (vs2ComputationalModel) computationalModel;
            line = in.readLine();
            int saveMoistureContentFlag = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
            cm.setSaveMoistureContent((saveMoistureContentFlag != 0) ? true : false);
	    
            line = in.readLine();
            int saveSaturationFlag = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
            cm.setSaveSaturation((saveSaturationFlag != 0) ? true : false);
            
            line = in.readLine();
            int saveVectorsFlag = Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
            cm.setSaveVectors((saveVectorsFlag != 0) ? true : false);
        } catch (Exception e) {
            throw e;
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
            
            vs2ComputationalModel cm = (vs2ComputationalModel) computationalModel;
            java.util.Map<String, mp2ColorScale> map = cm.getColorScaleMap();
            assert(map != null && map.size() > 5);
            pw.println("color scale count: " + map.size());
            
            int i = 0;
            for (java.util.Map.Entry<String, mp2ColorScale> entry : map.entrySet()) {
                String key = entry.getKey();
                pw.println("color scale " + i + " name: " + key);
                
                mp2ColorScale cs = entry.getValue();
                pw.println("color scale " + i + " red limit: " + cs.GetValueRed());
                pw.println("color scale " + i + " blue limit: " + cs.GetValueBlue());
                pw.println("color scale " + i + " color interval: " + cs.GetColorInterval());
                pw.println("color scale " + i + " label interval: " + cs.GetLabelInterval());
                ++i;
            }
            
            pw.println("save moisture content: " + (cm.getSaveMoistureContent() ? "1" : "0"));
            pw.println("save saturation: " + (cm.getSaveSaturation() ? "1" : "0"));
            pw.println("save vectors: " + (cm.getSaveVectors() ? "1" : "0"));
        } catch (Exception e) {
            throw e;
        }
    }
    
    /**
     * Invoked when the load menu item is selected
     * 
     * 2016-08-26 SRC -- overloaded in order to call releaseMemory
     */
    protected boolean onLoad() {
        if (doViewer && model != null && model.getType() == mp2Model.COMPUTATIONAL
                     && runStatus == 0) {
            int result = mp2MessageBox.showYesNoDialog(this, "The computation is not finished. " + 
                "Do you want to load another file?", "Warning");
            if (result == mp2MessageBox.NO_OPTION) {
                return false;
            }
            ((vs2ComputationalModel) model).releaseMemory();
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
    
    /**
     * Performs model start up calculations.
     */
    protected int doModelStartUpCalculations() {
        if (model.getType() == mp2Model.COMPUTATIONAL) {
            return ((vs2ComputationalModel)model).getJStop();
        }
        return 0;
    }

    /**
     * Only used for unit testing
     * @return run JButton
     */
    public JButton getRunButton() {
        return this.runButton;        
    }

    /**
     * Only used for unit testing
     * @return reset JButton
     */
    public JButton getResetButton() {
        return this.resetButton;        
    }

    /**
     * Only used for unit testing
     * @return step JButton
     */
    public JButton getStepButton() {
        return this.stepButton;        
    }
    
    /**
     * Only used for unit testing
     * @return stop JButton
     */
    public JButton getStopButton() {
        return this.stopButton;        
    }
    
    /**
     * Only used for unit testing
     * @return displayChooser JComboBox
     */
    public JComboBox getDisplayChooser() {
        return this.displayChooser;        
    }
}
