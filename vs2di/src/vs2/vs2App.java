/*
 * vs2App.java
 */
package vs2;

import java.awt.Cursor;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;
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
    
    
    /**
     * Creates the vs2dti application
     */
    public vs2App() {
        super(ClassLoader.getSystemResource("images/splash.gif"),
                ClassLoader.getSystemResource("images/appicon.gif"),
                false,
                System.getProperty("user.dir"));
    }
    

    /**
     * Creates the vs2dti application
     */
    public vs2App(String splashScreenFile, String appIconFile, String homeDir) {
        super(splashScreenFile, appIconFile, homeDir);
    }

    /**
     * Creates the vs2dti application
     */
    public vs2App(java.net.URL splashScreenURL, java.net.URL appIconURL, String homeDir) {
        super(splashScreenURL, appIconURL, false, homeDir);
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
        return "vs2drt";
    }
    
    /**
     * Gets the title for the application frame window
     */
    public String getFrameTitle() {
        return "VS2DRTI Preprocessor";
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
     * Gets the application properties file name
     */
    protected String getPropertiesFileName() {
        return "vs2drti.properties";
    }

    /**
     * The main method. This is the entry point of the application.
     */
    public static void main(String[] args) {
        mp2FileChooser.useJFileChooser(true);
        try {
            // Use windows look and feel when running under windows
            if (System.getProperty("os.name").startsWith("Windows")) {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                // When launched by a windows exe program to run on jre, the
                // javax.swing.JFileChooser doesn't work so we use java.awt.FileDialog
                // instead
                mp2FileChooser.useJFileChooser(false);
            }
        } catch (Exception e) {
            System.out.println("Error loading L&F: " + e);
        }
        String homeDir = System.getProperty("user.dir");;
        theApp = new vs2App(
                ClassLoader.getSystemResource("images/splash.gif"),
                ClassLoader.getSystemResource("images/appicon.gif"),
                homeDir);
    }
    
    /**
     * Checks that the specified doc has the correct type
     */
    protected boolean typeCheck(mp2Doc aDoc) {
        if (aDoc instanceof vs2Doc) {
            return true;
        } else {
            mp2MessageBox.showMessageDialog(
                "This file does not contain VS2DRTI data",
                "IO Error");
            return false;
        }
    }
    
    /**
     * Checks that the version is correct
     */
    protected boolean versionCheck(mp2Doc d) {
        return versionCheck(d, false);
    }
    
    /**
     * Checks that the version is correct
     */
    protected boolean versionCheck(mp2Doc d, boolean quiet) {
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
                if (!quiet) {
                    String [] messageLine= new String[2];
                    messageLine[0] = "Opening a Version " + serializedVersion + " document.";
                    messageLine[1] = "Do you want to convert this to a Version " + currentVersion
                                      + " document ?";
                    int result = mp2MessageBox.showYesNoDialog(messageLine, "Warning");
                    if (result == mp2MessageBox.NO_OPTION) {
                        return false;
                    }
                }
            } else if (serializedVersionNumber > currentVersionNumber){
                mp2MessageBox.showMessageDialog("Cannot open a document created by a later version of this program.",
                        "Error");
                return false;
            }
        }
        // Check for change in data usage
        return true;
    }
    
    /**
     * Used for testing purposes. (Modified from mp2App.openDocument()
     */
    public void openFile(File inFile) {
        try {
            // Create an input stream for object deserialization.
            FileInputStream input = new FileInputStream(inFile);
            ObjectInputStream objectIn = new ObjectInputStream(input);

            // Create the document by deserialization from file
            mp2Doc d = (mp2Doc) objectIn.readObject();

            if (!typeCheck(d) || !versionCheck(d)) {
                frame.setCursor(Cursor.getDefaultCursor());
                return;
            }

            frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            currentDirectory = new String(inFile.getParent());
            doc = d;
            doc.setDirectory(inFile.getParent());
            doc.setFileName(inFile.getName());
            if (!doc.getSerializedVersion().equals(doc.getCurrentVersion())) {
                doc.convertToCurrentVersion();
            }

            // Set the data chooser to the first item. Usually, this is the domain.
            JComboBox dataChooser = frame.getDataChooser();
            if (dataChooser!= null && dataChooser.getItemCount() > 0) {
                dataChooser.setSelectedIndex(0);
            }

            // Dispose of the resources used by the previous view
            view.dispose();

            // Create a new view
            view = createView();

            // Initialize both the document and the view.
            doc.init(this);
            view.init(this);
            postProcessorFrame.reset();

            // Put the view in the frame, and update the frame title.
            frame.setView(view);
            String title = frameTitle + ": " + doc.getFileName();
            if (doc.isChanged()) {
                title += " *";
            }
            frame.setTitle(title);
        }
        catch(FileNotFoundException e1) {
            mp2MessageBox.showMessageDialog(
                "Unable to open file " + inFile.getName(),
                "IO Error");
        }
        catch(IOException e2) {
            mp2MessageBox.showMessageDialog(
                "Unable to read from file " + inFile.getName(),
                "IO Error");
        }
        catch(ClassNotFoundException  e3) {
            mp2MessageBox.showMessageDialog(
                "Unable to read from file " + inFile.getName(),
                "IO Error");
        }
        mp2Math.changeDirectory(doc.getDirectory() + ".");        
    }
}
