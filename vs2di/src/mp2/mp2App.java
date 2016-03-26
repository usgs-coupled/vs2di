/*
 * mp2App.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

/**
 * Defines the application framework. The major components of the
 * application are:
 * <UL>
 * <LI>document--holds the application data
 * <LI>view--displays the application data on screen
 * <LI>main frame window (preprocessor)
 * <LI>post processor frame
 * </UL>
 */
public abstract class mp2App implements mp2Constants {

    /**
     * The application's document.
     */
    protected mp2Doc doc;

    /**
     * The application's main frame window (preprocessor).
     */
    protected mp2Frame frame;

    /**
     * The frame window title.
     */
    protected String frameTitle;

    /**
     * The application's home directory.
     */
    protected String homeDirectory;

    protected String currentDirectory;

    /**
     * The frame window for post processing.
     */
    protected mp2PostProcessorFrame postProcessorFrame;

    /**
     * Application properties saved on disk.
     */
    protected Properties properties;

    /**
     * The application's view.
     */
    protected mp2View view;

    /**
     * Defines the type of help window.
     * If true, display help pages in JavaHelp window.
     * If false, display help pages in web browser
     */
    protected static boolean useJavaHelp = true;

    /**
     * Creates the application framework with no splash screen,
     * default icon, and for a single model.
     *
     * @param  homeDir  The pathname to the application's home
     *                  directory.
     */
    public mp2App(String homeDir) {
        this((String)null, (String)null, false, homeDir);
    }

    /**
     * Creates the application framework with specified splash screen,
     * default icon, and for a single model.
     *
     * @param  splashScreenFile  name of the gif file containing
     *                 the splash screen. If null, then no
     *                 splash screen is display.
     *
     * @param  homeDir  The pathname to the application's home
     *                  directory.
     */
    public mp2App(String splashScreenFile, String homeDir) {
        this(splashScreenFile, null, false, homeDir);
    }


    /**
     * Creates the application framework with specified splash screen,
     * specified icon, and for a single model.
     *
     * @param  splashScreenFile  name of the gif file containing
     *                 the splash screen. If null, then no
     *                 splash screen is display.
     *
     * @param  appIconFile  name of the gif file containing the
     *                 application icon. If null, then the default
     *                 icon is used.
     *
     * @param  homeDir  The pathname to the application's home
     *                  directory.
     */
    public mp2App(String splashScreenFile, String appIconFile,
            String homeDir) {
        this(splashScreenFile, appIconFile, false, homeDir);
    }

    /**
     * Creates the application framework with specified splash screen,
     * specified icon, and specified multiple model.
     *
     * @param  splashScreenFile  name of the gif file containing
     *                 the splash screen. If null, then no
     *                 splash screen is display.
     *
     * @param  appIconFile  name of the gif file containing the
     *                 application icon. If null, then the default
     *                 icon is used.
     *
     * @param  hasMultipleModel  if <code>true</code> menu bar contains
     *                 a "Model" menu. Otherwise, menu bar does not
     *                 contain a "Model" menu.
     *
     * @param  homeDir  The pathname to the application's home
     *                  directory.
     */
    public mp2App(String splashScreenFile, String appIconFile,
                    boolean multipleModels, String homeDir) {
        homeDirectory = homeDir;
        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        // create the frame
        frame = new mp2Frame(multipleModels);

        // display splash screen while program loads
        if (splashScreenFile != null) {
            new mp2SplashScreen(frame, imageDirectory + splashScreenFile);
        }

        // Continue with application startup
        properties = new Properties();
        readAppProperties();
        currentDirectory = properties.getProperty("directory");
        frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                appExit();
            }
        });
        frame.init(this);
        frame.setManager(createFrameManager());
        postProcessorFrame = createPostProcessorFrame();
        if (appIconFile != null) {
            Image appIcon = Toolkit.getDefaultToolkit().getImage(
                                            imageDirectory + appIconFile);
            frame.setIconImage(appIcon);
            postProcessorFrame.setIconImage(appIcon);
        }
        doc = createDoc();
        view = createView();
        doc.init(this);
        view.init(this);
        postProcessorFrame.reset();
        frame.loadComponents();
        frame.setView(view);
        frameTitle = getFrameTitle();
        frame.setTitle(frameTitle + ": " + doc.getFileName());
        frame.validate();

        // Show the frame in the middle of the screen
        frame.pack();
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension frameSize = frame.getSize();
        frame.setLocation((screenSize.width-frameSize.width)/2,
                          (screenSize.height-frameSize.height)/2);
        frame.pack();
        frame.setVisible(true);
        frame.requestFocus();
    }

    /**
     * Creates the application framework with specified splash screen,
     * specified icon, and specified multiple model.
     *
     * @param  splashScreenURL  URL of the gif file containing
     *                 the splash screen. If null, then no
     *                 splash screen is display.
     *
     * @param  appIconURL  URL of the gif file containing the
     *                 application icon. If null, then the default
     *                 icon is used.
     *
     * @param  hasMultipleModel  if <code>true</code> menu bar contains
     *                 a "Model" menu. Otherwise, menu bar does not
     *                 contain a "Model" menu.
     *
     * @param  homeDir  The pathname to the application's home
     *                  directory.
     */
    public mp2App(java.net.URL splashScreenURL, java.net.URL appIconURL,
                    boolean multipleModels, String homeDir) {
        homeDirectory = homeDir;

        // create the frame
        frame = new mp2Frame(multipleModels);

        // display splash screen while program loads
        if (splashScreenURL != null) {
            new mp2SplashScreen(frame, splashScreenURL);
        }

        // Continue with application startup
        properties = new Properties();
        readAppProperties();
        currentDirectory = properties.getProperty("directory");
        frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                appExit();
            }
        });
        frame.init(this);
        frame.setManager(createFrameManager());
        postProcessorFrame = createPostProcessorFrame();
        if (appIconURL != null) {
            Image appIcon = Toolkit.getDefaultToolkit().getImage(appIconURL);
            frame.setIconImage(appIcon);
            postProcessorFrame.setIconImage(appIcon);
        }
        doc = createDoc();
        view = createView();
        doc.init(this);
        view.init(this);
        postProcessorFrame.reset();
        frame.loadComponents();
        frame.setView(view);
        frameTitle = getFrameTitle();
        frame.setTitle(frameTitle + ": " + doc.getFileName());
        frame.validate();

        // Show the frame in the middle of the screen
        frame.pack();
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension frameSize = frame.getSize();
        frame.setLocation((screenSize.width-frameSize.width)/2,
                          (screenSize.height-frameSize.height)/2);
        frame.pack();
        frame.setVisible(true);
        frame.requestFocus();
    }
    
    /**
     * Handles application shutdown. If the document has been
     * changed since it was last saved, query the user on
     * whether the document should be saved before shutdown.
     */
    public void appExit() {
        if (postProcessorFrame.isVisible() && !postProcessorFrame.quitOK()) {
            return;
        }
        if (doc != null && doc.isChanged()) {
            if (querySaveDocument() == false) {
                return;
            }
        }
        writeAppProperties();
        frame.dispose();
        System.exit(0);
    }

    /**
     * Creates and returns a document for the active model.
     */
    protected abstract mp2Doc createDoc();

    /**
     * Creates and returns a frame manager for the active model.
     */
    protected abstract mp2FrameManager createFrameManager();

    /**
     * Creates and returns a frame window for post processing.
     */
    protected abstract mp2PostProcessorFrame createPostProcessorFrame();

    /**
     * Creates and returns a view for the active model.
     */
    protected abstract mp2View createView();

    /**
     * Exports the data in the document for external use.
     */
    public void exportData() {
        if (!doc.readyToExport()) {
            return;
        }

        // Create and show a file dialog box for user to select
        // the file to contain the exported data
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Export Data");
        if (currentDirectory != null) {
            fc.setCurrentDirectory(new File(currentDirectory));
        }
        if (fc.showSaveDialog(frame) == mp2FileChooser.APPROVE_OPTION) {
            frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            File f = fc.getSelectedFile();
            int result = doc.exportData(fc.getCurrentDirectory().getPath(), f.getName());
            if (result == mp2Doc.UNABLE_TO_EXPORT_ERROR) {
                mp2MessageBox.showMessageDialog("Unable to export data",
                        "Error");
            } else if (result == mp2Doc.ILLEGAL_FILE_NAME_ERROR) {
                mp2MessageBox.showMessageDialog("The file name \"" +
                        f.getName() + "\" is not allowed. " +
                        "Please specify a different file name.",
                        "Error");
            } else {
                currentDirectory = new String(fc.getCurrentDirectory().getPath());
            }
            frame.setCursor(Cursor.getDefaultCursor());
        }
    }

    public String getCurrentDirectory() {
        return currentDirectory;
    }

    /**
     * Gets this application's document.
     */
    public mp2Doc getDoc() {
        return doc;
    }

    /**
     * Get the file filter for the document. Over-ride this in subclass
     * to implement a file filter
     */
    public javax.swing.filechooser.FileFilter getDocFileFilter() {
        return null;
    }

    /**
     * Gets this application's main frame window.
     */
    public mp2Frame getFrame() {
        return frame;
    }

    /**
     * Creates the title for the frame. This title varies
     * according to the active model.
     */
    public abstract String getFrameTitle();

    /**
     * Gets this application's home directory.
     */
    public String getHomeDirectory() {
        return homeDirectory;
    }

    /**
     * Gets the frame window for post processing
     */
    public mp2PostProcessorFrame getPostProcessorFrame() {
        return postProcessorFrame;
    }

    /**
     * Gets the directory containing the properties file
     */
    protected String getPropertiesDirectory() {
        return homeDirectory;
    }

    /**
     * Gets the file name containing the properties for
     * this application
     */
    protected String getPropertiesFileName() {
        return null;
    }

    /**
     * Gets this application's view.
     */
    public mp2View getView() {
        return view;
    }

    /**
     * Creates a new document.
     */
    public void newDocument() {
        // If the current document has been changed since it was
        // save, query the user if the document should be saved.
        if (doc != null && doc.isChanged()) {
            if (querySaveDocument() == false) {
                return;
            }
        }

        // Set the data chooser to the first item. Usually, this is the domain.
        JComboBox dataChooser = frame.getDataChooser();
        if (dataChooser!= null && dataChooser.getItemCount() > 0) {
            dataChooser.setSelectedIndex(0);
        }

        frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        // dispose of the resources used by the current view
        view.dispose();
        view = null;

        // If we changed model, then we need to set up a new frame
        // manager and post processor frame
        if (!frameTitle.equalsIgnoreCase(getFrameTitle())) {
            frame.setManager(createFrameManager());
            frameTitle = getFrameTitle();
            postProcessorFrame = createPostProcessorFrame();
        } else {
            postProcessorFrame.reset();
        }

        // Create new document and view, and initialize them.
        doc = createDoc();
        view = createView();
        doc.init(this);
        view.init(this);

        // Put the new view in the frame, update the title,
        // and disallow exporting.
        frame.setView(view);
        frame.setTitle(frameTitle + ": " + doc.getFileName());
        frame.initializeInformationText();
        frame.setCursor(Cursor.getDefaultCursor());
    }

    /**
     * Opens a document.
     */
    public void openDocument() {
        // If the current document has been changed since it was
        // save, query the user if the document should be saved.
        if (doc.isChanged()) {
            if (querySaveDocument() == false) {
                return;
            }
        }

        // Create and show a file open dialog for user to select
        // the document to open
        mp2FileChooser fc = new mp2FileChooser();
        javax.swing.filechooser.FileFilter fil = getDocFileFilter();
        if (fil != null) {
            fc.addChoosableFileFilter(fil);
        }
        if (doc.getFileName().equals(DEFAULT_FILE_NAME)) {
            if (currentDirectory != null) {
                fc.setCurrentDirectory(new File(currentDirectory));
            }
        } else {
            if (doc.getDirectory() != null) {
                fc.setCurrentDirectory(new File(doc.getDirectory()));
            }
        }

        int returnVal = fc.showOpenDialog(frame);

        if (returnVal == mp2FileChooser.APPROVE_OPTION) {
            frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            File inFile = fc.getSelectedFile();
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
                currentDirectory = new String(fc.getCurrentDirectory().getPath());
                doc = d;
                doc.setDirectory(fc.getCurrentDirectory().getPath());
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
            frame.setCursor(Cursor.getDefaultCursor());
            mp2Math.changeDirectory(doc.getDirectory() + ".");
        }
    }

    /**
     * Querys the user if the current document should be saved.
     *
     * @return  <code>true</code> if the user answered yes and
     *          the document was saved.
     *          <code>false</code> otherwise.
     */
    protected boolean querySaveDocument() {

        String query = "The data in " + doc.getFileName() +
        " have changed. Do you want to save the changes?";

        int result = mp2MessageBox.showConfirmDialog(query, frameTitle);

        // If user answers "Yes" to query, then save the document
        if (result == mp2MessageBox.YES_OPTION) {
            return saveDocument();
        }

        // If user answers "No" to query, then we return true
        // without saving the document.
        else if (result == mp2MessageBox.NO_OPTION) {
            return true;
        }

        // The remaining cases are (1) user answers "Cancel" or
        // (2) user closes the dialog box. For either case, return false
        else {
            return false;
        }
    }

    /**
     * Reads the application's properties.
     */
    protected void readAppProperties() {
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
     * Saves the current document.
     *
     * @return  <code>true</code> if the document is saved.
     *          <code>false</code> otherwise.
     */
    public boolean saveDocument() {
        // If the document is new (that is, it has a default file name),
        // then call the saveDocumentAs method so user can specify
        // a file name to save the document
        if (doc.getFileName().equals(DEFAULT_FILE_NAME)) {
            return saveDocumentAs();
        }

        // If the document is not new, save data to document's
        // file name
        else {
            frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            try {
                File outFile = new File(doc.getDirectory(),
                                        doc.getFileName());
                FileOutputStream out = new FileOutputStream(outFile);
                ObjectOutputStream objectOut =
                        new ObjectOutputStream(out);
                objectOut.writeObject(doc);
                objectOut.flush();
                doc.setChanged(false);
                view.docSaved();
                frame.setCursor(Cursor.getDefaultCursor());
                return true;
            } catch(IOException e) {
                mp2MessageBox.showMessageDialog(
                        "Unable to write to file", "IO Error");
                frame.setCursor(Cursor.getDefaultCursor());
                return false;
            }
        }
    }

    /**
     * Saves the document to a specified file name.
     *
     * @return  <code>true</code> if the document is saved.
     *          <code>false</code> otherwise.
     */
    public boolean saveDocumentAs() {

        // Create and show a "Save As" dialog box.
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Save As");
        javax.swing.filechooser.FileFilter fil = getDocFileFilter();
        if (fil != null) {
            fc.addChoosableFileFilter(fil);
        }
        if (doc.getFileName().equals(DEFAULT_FILE_NAME)) {
            if (currentDirectory != null) {
                fc.setCurrentDirectory(new File(currentDirectory));
            }
        } else {
            if (doc.getDirectory() != null) {
                fc.setCurrentDirectory(new File(doc.getDirectory()));
            }
        }
        fc.setSelectedFile(new File(doc.getFileName()));

        // If the user specified file name is not null, then
        // save the document
        if (fc.showSaveDialog(frame) == mp2FileChooser.APPROVE_OPTION) {
            frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

            File f = fc.getSelectedFile();
            currentDirectory = new String(fc.getCurrentDirectory().getPath());
            doc.setDirectory(fc.getCurrentDirectory().getPath());
            doc.setFileName(f.getName());

            try {
                FileOutputStream out = new FileOutputStream(f);
                ObjectOutputStream objectOut =
                        new ObjectOutputStream(out);
                objectOut.writeObject(doc);
                objectOut.flush();
                doc.setChanged(false);
                view.docSaved();
                frame.setCursor(Cursor.getDefaultCursor());
                return true;
            }
            catch(IOException e) {
                mp2MessageBox.showMessageDialog(
                        "Unable to write to file", "IO Error");
                frame.setCursor(Cursor.getDefaultCursor());
                return false;
            }
        }

        // If the file name is null, that means user has either
        // clicked the "Cancel" button in the dialog box or
        // closed the dialog box. In either case, return false.
        else {
            return false;
        }
    }

    public void setCurrentDirectory(String d) {
        currentDirectory = d;
    }

    /**
     * Checks that the specified doc is of the correct type
     * for the active model.
     *
     * @return  <code>true</code> if the document is of the
     *          correct type for the current active model,
     *          <code>false</code> otherwise.
     */
    protected abstract boolean typeCheck(mp2Doc d);

    public static boolean useJavaHelp() {
        return useJavaHelp;
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
        return true;
    }

    /**
     * Writes properties to disk
     */
    protected void writeAppProperties() {
        String fileName = getPropertiesFileName();
        if (fileName == null) {
            return;
        }
        String directory = doc.getDirectory();
        if (directory == null || directory.length() == 0) {
            return;
        }
        FileOutputStream out;
        try {
            out = new FileOutputStream(new File(
                        getPropertiesDirectory(), fileName));
            properties.put("directory", directory);
            properties.store(out, null);
            out.close();
        } catch (IOException e) {
            return;
        }
    }
}
