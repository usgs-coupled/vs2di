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
        
        if (theApp == null && !findModelTypeAndUsage(directory, file)) {
            return false;
        }

        if (!super.loadData(directory,file)) {
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
                ////mp2FileChooser.useJFileChooser(false);
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
        ((vs2ComputationalModel) computationalModel).releaseMemory();
        super.onRestartComputation();
        if (displayChooserRequiresUpdate) {
            displayChooser.removeAllItems();
            displayChooser.addItem("Total Head");
            displayChooser.addItem("Pressure Head");
            displayChooser.addItem("Moisture Content");
            displayChooser.addItem("Saturation");
            displayChooser.addItem("Temperature");
            displayChooser.addItem("Concentration");
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
            super.readSettingsFile(in);
            vs2ComputationalModel cm = (vs2ComputationalModel) computationalModel;
            String line;
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
            super.writeSettingsFile(pw);
            vs2ComputationalModel cm = (vs2ComputationalModel) computationalModel;
            pw.println("save moisture content: " + (cm.getSaveMoistureContent() ? "1" : "0"));
            pw.println("save saturation: " + (cm.getSaveSaturation() ? "1" : "0"));
            pw.println("save vectors: " + (cm.getSaveVectors() ? "1" : "0"));
        } catch (Exception e) {
            throw e;
        }
    }
    
    /**
     * Only used for unit testing
     * @return step JButton
     */
    public JButton getStepButton() {
        return this.stepButton;        
    }
}
