/*
 * vs2App.java
 */
package vs2;

import mp2.*;
import javax.swing.*;

/**
 * Creates the vs2dti application.
 */
public class vs2App extends mp2App implements vs2Constants {

    /**
     * The one and only static instance of this application.
     */
    protected static vs2App theApp;
    
    private static boolean doHeat = false;

    /**
     * Creates the vs2dti application
     */
    public vs2App(String splashScreenFile, String appIconFile, String homeDir) {
        super(splashScreenFile, appIconFile, homeDir);
    }

    /**
     * Creates a document
     */
    protected mp2Doc createDoc() {
        return new vs2Doc();
    }

    /**
     * Creates a view
     */
    protected mp2View createView() {
        return new vs2View();
    }

    /**
     * Creates a frame manager
     */
    protected mp2FrameManager createFrameManager() {
        return new vs2FrameManager(this);
    }

    /**
     * Creates a post processor
     */
    protected mp2PostProcessorFrame createPostProcessorFrame() {
        return new vs2PostProcessorFrame(this);
    }

    /**
     * Gets whether or not the simulation is heat flow.
     */
    public static boolean doHeat() {
        return doHeat;
    }
    

    /**
     * Get the file filter for the document
     */
    public javax.swing.filechooser.FileFilter getDocFileFilter() {
        mp2FileFilter filter = new mp2FileFilter();
        filter.addExtension("vs2");
        filter.setDescription("VS2 File (*.vs2)");
        return filter;
    }

    /**
     * Gets the file prefix, either "vs2dh" or "vs2dt" depending on usage
     */
    public static String getFilePrefix() {
        if (doHeat) {
            return "vs2dh";
        } else {
            return "vs2dt";
        }
    }
    
    /**
     * Gets the title for the application frame window
     */
    public String getFrameTitle() {
        if (doHeat) {
            return "VS2DHI Preprocessor";
        } else {
            return "VS2DTI Preprocessor";
        }
    }

    /**
     * Gets the directory containing the properties files
     */
    protected String getPropertiesDirectory() {
        return homeDirectory + System.getProperty("file.separator") + "bin";
    }
    
    /**
     * Gets the application properties file name
     */
    protected String getPropertiesFileName() {
        if (doHeat) {
            return "vs2dhi.properties";
        } else {
            return "vs2dti.properties";
        }
    }

    /**
     * The main method. This is the entry point of the application.
     */
    public static void main(String[] args) {
        String homeDir = null;
        String splashScreenFile = "splash.gif";
        String appIconFile = "appicon.gif";
        mp2FileChooser.useJFileChooser(true);
        for (int i=0; i<Math.min(args.length, 3); i++) {
            if (args[i].equalsIgnoreCase("-heat")) {
                doHeat = true;
                splashScreenFile = "heatsplash.gif";
                appIconFile = "heaticon.gif";
            } else if (args[i].equalsIgnoreCase("-w")) {
                try {
                    // Use windows look and feel
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                    // When launched by a windows exe program to run on jre, the
                    // javax.swing.JFileChooser doesn't work so we use java.awt.FileDialog
                    // instead
                    mp2FileChooser.useJFileChooser(false);
                } catch (Exception e) {
                    System.out.println("Error loading L&F: " + e);
                }
                
            } else {
                homeDir = args[i];
            }
        }
        if (homeDir == null)  {
            homeDir = System.getProperty("user.dir");
        }
        theApp = new vs2App(splashScreenFile, appIconFile, homeDir);
    }
    
    /**
     * Checks that the specified doc has the correct type
     */
    protected boolean typeCheck(mp2Doc aDoc) {
        if (aDoc instanceof vs2Doc) {
            return true;
        } else {
            if (doHeat) {
                mp2MessageBox.showMessageDialog(
                    "This file does not contain VS2DHI data",
                    "IO Error");
            } else {
                mp2MessageBox.showMessageDialog(
                    "This file does not contain VS2DTI data",
                    "IO Error");
            }
            return false;
        }
    }

    /**
     * Checks that the version is correct
     */
    protected boolean versionCheck(mp2Doc d) {
        String serializedVersion = d.getSerializedVersion();
        String currentVersion = d.getCurrentVersion();
        if (!serializedVersion.equals(currentVersion)) {
            // extract the numerical version number.
            double serializedVersionNumber, currentVersionNumber;
            int indexOfSpace = serializedVersion.indexOf(' ');
            if (indexOfSpace == -1) {
                serializedVersionNumber = Double.valueOf(serializedVersion).doubleValue();
            } else {
                serializedVersionNumber = Double.valueOf(serializedVersion.substring(0, indexOfSpace)).doubleValue();
            }
            indexOfSpace = currentVersion.indexOf(' ');
            if (indexOfSpace == -1) {
                currentVersionNumber = Double.valueOf(currentVersion).doubleValue();
            } else {
                currentVersionNumber = Double.valueOf(currentVersion.substring(0, indexOfSpace)).doubleValue();
            }
            // Only allow docs that have a version number smaller then or equal
            // to the current version number
            if (serializedVersionNumber < currentVersionNumber) {
                String [] messageLine= new String[2];
                messageLine[0] = "Opening a Version " + serializedVersion + " document.";
                messageLine[1] = "Do you want to convert this to a Version " + currentVersion
                                  + " document ?";
                int result = mp2MessageBox.showYesNoDialog(messageLine, "Warning");
                if (result == mp2MessageBox.NO_OPTION) {
                    return false;
                }
            } else if (serializedVersionNumber > currentVersionNumber){
                mp2MessageBox.showMessageDialog("Cannot open a document created by a later version of this program.",
                        "Error");
                return false;
            }
        }
        // Check for change in data usage
        if (vs2App.doHeat() && 
                ((vs2Doc) d).getUsage() == SOLUTE_TRANSPORT) {
            String [] messageLine= new String[2];
            messageLine[0] = "Opening a file previously used for SOLUTE transport simulation.";
            messageLine[1] = "Do you want to use this file for ENERGY transport simulation?";
            int result = mp2MessageBox.showYesNoDialog(messageLine, "Warning");
            return (result == mp2MessageBox.YES_OPTION);
        } 
        if (!vs2App.doHeat() && 
                ((vs2Doc) d).getUsage() == ENERGY_TRANSPORT) {
            String [] messageLine= new String[2];
            messageLine[0] = "Opening a file previously used for ENERGY transport simulation.";
            messageLine[1] = "Do you want to use this for SOLUTE transport simulation?";
            int result = mp2MessageBox.showYesNoDialog(messageLine, "Warning");
            return (result == mp2MessageBox.YES_OPTION);
        }
        return true;
    }
}
