/*
 * mp2FileChooser.java
 */
package mp2;
import java.awt.*;
import java.io.*;
import javax.swing.*;

public class mp2FileChooser {
    
    public static final int APPROVE_OPTION = JFileChooser.APPROVE_OPTION;    public static final int CANCEL_OPTION = JFileChooser.CANCEL_OPTION;        protected static boolean useJFileChooser = false;    protected JFileChooser jfc;
    protected FileDialog fd;
    protected String dialogTitle;
    protected File currentDirectory;    protected File selectedFile;
    protected javax.swing.filechooser.FileFilter fileFilter;        public mp2FileChooser() {        dialogTitle = null;        currentDirectory = null;
        selectedFile = null;        fileFilter = null;
    }
    
    public static void useJFileChooser(boolean b) {
        useJFileChooser = b;
    }        public void setDialogTitle(String t) {        dialogTitle = t;    }        public void setCurrentDirectory(File d) {        currentDirectory = d;    }
    
    public void addChoosableFileFilter(javax.swing.filechooser.FileFilter f) {
        fileFilter = f;    }
    
    public void setSelectedFile(File f) {
        selectedFile = f;
    }        public int showOpenDialog(Frame frame) {        if (useJFileChooser) {
            jfc = new JFileChooser();            if (dialogTitle==null) {                jfc.setDialogTitle(dialogTitle);
            }
            if (currentDirectory != null) {
                jfc.setCurrentDirectory(currentDirectory);            }
            if (fileFilter != null) {                jfc.addChoosableFileFilter(fileFilter);            }
            return jfc.showOpenDialog(frame);        } else {            if (dialogTitle==null) {                dialogTitle = "Open";            }
            fd = new FileDialog(frame, dialogTitle);
            if (currentDirectory != null) {
                fd.setDirectory(currentDirectory.getPath());
            }
            fd.setVisible(true);
            if (fd.getFile() == null) {
                return mp2FileChooser.CANCEL_OPTION;
            } else {
                return mp2FileChooser.APPROVE_OPTION;
            }        }    }        public int showSaveDialog(Frame frame) {        if (useJFileChooser) {
            jfc = new JFileChooser();
            if (dialogTitle==null) {                jfc.setDialogTitle(dialogTitle);
            }
            if (currentDirectory != null) {
                jfc.setCurrentDirectory(currentDirectory);
            }            if (fileFilter != null) {                jfc.addChoosableFileFilter(fileFilter);            }            if (selectedFile != null) {                jfc.setSelectedFile(selectedFile);            }
            return jfc.showSaveDialog(frame);        } else {
            fd = new FileDialog(frame, dialogTitle, FileDialog.SAVE);            if (currentDirectory != null) {
                fd.setDirectory(currentDirectory.getPath());
            }
            if (selectedFile != null) {                fd.setFile(selectedFile.getName());            }
            fd.setVisible(true);
            if (fd.getFile() == null) {
                return mp2FileChooser.CANCEL_OPTION;
            } else {
                return mp2FileChooser.APPROVE_OPTION;
            }        }    }        public File getSelectedFile() {
        if (useJFileChooser) {            return jfc.getSelectedFile();        } else {
            if (fd.getFile() == null) {                return null;
            } else {
                return new File(fd.getDirectory(), fd.getFile());
            }
        }    }
    
    public File getCurrentDirectory() {
        if (useJFileChooser) {
            if (jfc == null) {
                return null;
            } else {                return jfc.getCurrentDirectory();
            }        } else {
            if (fd == null) {
                return null;
            } else {                return new File(fd.getDirectory());
            }        }    }
}
