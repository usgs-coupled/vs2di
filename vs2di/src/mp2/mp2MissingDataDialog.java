/*
 * mp2MissingDataDialog.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;

public class mp2MissingDataDialog extends JDialog {

    public mp2MissingDataDialog(JFrame frame, Vector missingData) {
        super(frame, "Error", true);
        getContentPane().setLayout(new BorderLayout());

        JPanel centerPanel = new JPanel();
        getContentPane().add(centerPanel, BorderLayout.CENTER);
        centerPanel.setLayout(new GridLayout(0, 1));
        centerPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
        centerPanel.add(new JLabel("Insufficient data to export."));
        centerPanel.add(new JLabel(" "));
        centerPanel.add(new JLabel("The following data are missing:"));
        for (int i=0; i<missingData.size(); i++) {
            centerPanel.add(new JLabel(" - " + (String) missingData.elementAt(i)));
        }

        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        JButton okButton = new JButton("OK");
        buttonPanel.add(okButton);
        okButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                dispose();
            }
        });
        okButton.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    dispose();
                }
            }
        });

        enableEvents(AWTEvent.WINDOW_EVENT_MASK);
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
        pack();
    }

    protected void processWindowEvent(WindowEvent e) {
        if (e.getID() == WindowEvent.WINDOW_CLOSING) {
            dispose();
        }
        super.processWindowEvent(e);
    }
}
