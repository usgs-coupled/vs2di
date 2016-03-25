/*
 * vs2TexturalClassCustomDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2TexturalClassCustomDialog extends JDialog implements mp2Constants {

    private   mp2App                 theApp;
    private   vs2TexturalClassData   texturalClassData;
    private   int insertPosition;
    private   int soilModel;

    protected JButton openFileButton;
    protected JButton saveFileButton;
    protected JButton doneButton;
    protected JButton helpButton;

    public vs2TexturalClassCustomDialog (mp2App app, vs2TexturalClassData texturalClassData,
            int soilModel, int insertPosition) {
        super(app.getFrame(), "Custom Textural Classes", true);

        this.theApp = app;
        this.texturalClassData = texturalClassData;
        this.soilModel = soilModel;
        this.insertPosition = insertPosition;

        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onDone();
            }
        });

        JPanel panel = new JPanel(new GridLayout(1, 0, 20, 0));
        panel.setBorder(new EmptyBorder(20, 20, 20, 0));
        getContentPane().add(panel, BorderLayout.CENTER);

        panel.add(openFileButton = new JButton("Load Data From File"));
        openFileButton.addActionListener(new ActionListener() {
	    public void actionPerformed (ActionEvent e) {
                getMaterialLibraryFile();
            }
        });

        panel.add(saveFileButton = new JButton("Save Data To File"));
        saveFileButton.addActionListener(new ActionListener() {
            public void actionPerformed (ActionEvent e) {
		        saveMaterialLibraryFile();
            }
        });

        panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0));
        panel.setBorder(new EmptyBorder(20, 0, 20, 0));
        getContentPane().add(panel, BorderLayout.SOUTH);

        panel.add(doneButton = new JButton("Done"));
        doneButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDone();
            }
        });

        panel.add(helpButton = new JButton("Help"));
        if (mp2App.useJavaHelp()) {
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "importTexturalClass", null);        } else {
            helpButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    mp2HelpWindow.showHelpFile ("importTexturalClass.html");
                }
            });
        }
        pack();

        Dimension screenSize = getToolkit().getScreenSize();
        Dimension dlgSize = getSize();
        int left = Math.max((screenSize.width - dlgSize.width)/2, 0);
        int top = Math.max((screenSize.height - dlgSize.height)/2, 0);
        setLocation(left, top);
        pack(); // needed to workaround Java bug
    }

    /**
     * Read textural class data from file
     */
    protected void getMaterialLibraryFile() {
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Load Soil Textural Data File");
        fc.setCurrentDirectory(new File(theApp.getCurrentDirectory()));
        if (fc.showOpenDialog(theApp.getFrame()) == mp2FileChooser.APPROVE_OPTION) {
            texturalClassData.openAndAddFileData(fc.getSelectedFile().getPath(),
                                     soilModel, insertPosition);
            theApp.setCurrentDirectory(fc.getCurrentDirectory().getPath());
        }
    }

    /**
     * Write textural class data to file
     */
    public void saveMaterialLibraryFile() {
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Save Soil Textural Data File");
        fc.setCurrentDirectory(new File(theApp.getCurrentDirectory()));
        if (fc.showSaveDialog(theApp.getFrame()) == mp2FileChooser.APPROVE_OPTION) {
            texturalClassData.saveFileData (fc.getSelectedFile().getPath(), soilModel);
            theApp.setCurrentDirectory(fc.getCurrentDirectory().getPath());
        }
    }

    protected void onDone() {
        setVisible(false);
        dispose();
    }
}
