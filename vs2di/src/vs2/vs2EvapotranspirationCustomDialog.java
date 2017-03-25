/*
 * vs2EvapotranspirationCustomDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2EvapotranspirationCustomDialog extends JDialog implements mp2Constants {

    private   mp2App                 theApp;
    private   vs2EvapotranspirationData   evapotranspirationData;
    private   int soilModel;

    protected JButton openFileButton;
    protected JButton saveFileButton;
    protected JButton doneButton;
    protected JButton helpButton;

    public vs2EvapotranspirationCustomDialog (mp2App app, vs2EvapotranspirationData evapotranspirationData) {
        super(app.getFrame(), "Custom Evapotranspiration Data", true);

        this.theApp = app;
        this.evapotranspirationData = evapotranspirationData;

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
                loadDataFromFile();
            }
        });

        panel.add(saveFileButton = new JButton("Save Data To File"));
        saveFileButton.addActionListener(new ActionListener() {
            public void actionPerformed (ActionEvent e) {
		        saveDataToFile();
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
	        mp2JavaHelp.hb.enableHelpOnButton(helpButton, "importETData", null);
        } else {
            helpButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    mp2HelpWindow.showHelpFile ("importETData.html");
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
     * Read evapotranspiration data from file
     */
    protected void loadDataFromFile() {
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Load Evapotranspiration Data File");
        fc.setCurrentDirectory(new File(theApp.getCurrentDirectory()));
        if (fc.showOpenDialog(theApp.getFrame()) == mp2FileChooser.APPROVE_OPTION) {
            evapotranspirationData.loadDataFromFile(fc.getSelectedFile().getPath());
            theApp.setCurrentDirectory(fc.getCurrentDirectory().getPath());
        }
    }

    /**
     * Write evapotranspiration data to file
     */
    public void saveDataToFile() {
        mp2FileChooser fc = new mp2FileChooser();
        fc.setDialogTitle("Save Evapotranspiration Data File");
        fc.setCurrentDirectory(new File(theApp.getCurrentDirectory()));
        if (fc.showSaveDialog(theApp.getFrame()) == mp2FileChooser.APPROVE_OPTION) {
            evapotranspirationData.saveDataToFile (fc.getSelectedFile().getPath());
            theApp.setCurrentDirectory(fc.getCurrentDirectory().getPath());
        }
    }

    protected void onDone() {
        setVisible(false);
        dispose();
    }
}
