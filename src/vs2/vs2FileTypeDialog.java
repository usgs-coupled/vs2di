/*
 * vs2FileTypeDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2FileTypeDialog extends mp2Dialog implements vs2Constants {

    public boolean doViewer;
    
    protected JRadioButton playbackRadioButton;
    protected JRadioButton viewerRadioButton;
    
    public vs2FileTypeDialog(JFrame frame, String filename) {
        super("File Type", true, (Object) filename, frame);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "fileType", null);
    }

    protected void makeContents() {
        String filename = (String) customObject;
        JPanel centerPanel = new JPanel(new GridLayout(0, 1, 5, 5));
        centerPanel.setBorder(new EmptyBorder(25, 25, 25, 25));
        getContentPane().add(centerPanel, BorderLayout.CENTER);
        centerPanel.add(new JLabel("You have selected to load the file \"" + filename + "\""));
        centerPanel.add(new JLabel("Please specify the purpose of this file:"));
        centerPanel.add(playbackRadioButton = new JRadioButton("For playing back a saved simulation"));
        centerPanel.add(viewerRadioButton = new JRadioButton("For running VS2DT/H"));
        ButtonGroup bg = new ButtonGroup();
        bg.add(playbackRadioButton);
        bg.add(viewerRadioButton);
    }

    public boolean doModal() {
        playbackRadioButton.setSelected(!doViewer);
        viewerRadioButton.setSelected(doViewer);
        return super.doModal();
    }

    protected boolean retrieveData() {
        doViewer = viewerRadioButton.isSelected();
        return true;
    }

    protected void onBrowserHelp() {
        mp2HelpWindow.showHelpFile("fileType.html");
    }

}

