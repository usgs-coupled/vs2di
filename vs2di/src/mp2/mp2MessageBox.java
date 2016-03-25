/*
 * mp2MessageBox.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * A dialog box that displays a message.
 */
public class mp2MessageBox extends JDialog {

    public static final int CANCEL_OPTION = 0;
    public static final int NO_OPTION = -1;
    public static final int YES_OPTION = 1;
    protected static mp2Frame theMainFrame;
    protected int result;
    protected static final int CONFIRM_DIALOG = 1;
    protected static final int YES_NO_DIALOG = 2;
    protected static final int MESSAGE_DIALOG = 3;

    /**
     * Create a the message box to display the specified title
     * and message. Use static methods to instantiate this class.
     */
    protected mp2MessageBox(Frame frame, JPanel messagePanel,
            String title, int dialogType) {

        super(frame, title, true);

        // Put message in center of message box
        getContentPane().add(messagePanel, BorderLayout.CENTER);
        messagePanel.setBorder(new EmptyBorder(20, 20, 20, 20));

        // Create buttons at the bottom of message box
        JPanel buttonPanel = new JPanel();
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        buttonPanel.setLayout(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setBorder(new EmptyBorder(0, 20, 20, 20));

        if (dialogType == CONFIRM_DIALOG || dialogType == YES_NO_DIALOG) {
            // Create Yes, No, and Cancel buttons 
            JButton yesButton = new JButton("Yes");
            JButton noButton = new JButton("No");
            JButton cancelButton = new JButton("Cancel");
            buttonPanel.add(yesButton);
            buttonPanel.add(noButton);
            if (dialogType == CONFIRM_DIALOG) {
                buttonPanel.add(cancelButton);
            }
            yesButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    result = YES_OPTION;
                    dispose();
                }
            });
            yesButton.addKeyListener(new KeyAdapter() {
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                        result = YES_OPTION;
                        dispose();
                    }
                }
            });
            noButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    result = NO_OPTION;
                    dispose();
                }
            });
            noButton.addKeyListener(new KeyAdapter() {
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                        result = NO_OPTION;
                        dispose();
                    }
                }
            });
            cancelButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    result = CANCEL_OPTION;
                    dispose();
                }
            });
            cancelButton.addKeyListener(new KeyAdapter() {
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                        result = CANCEL_OPTION;
                        dispose();
                    }
                }
            });
            yesButton.requestFocus();
        }
        else {
            // Create OK button
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
            okButton.requestFocus();
        }

        enableEvents(AWTEvent.WINDOW_EVENT_MASK);
        pack();
    }

    /**
     * Show the message box with modal behavior. This method returns
     * when the message box is closed.
     *
     * @return  an <code>int</code> value representing how the
     *          dialog box was closed. 
     */
    protected int doModal() {
        validate();
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
        setVisible(true);
        return result;
    }

    /**
     * Invoked when the window is closed. The result is
     * the same as if the user clicked the "Cancel" button.
     */
    protected void processWindowEvent(WindowEvent e) {
        if (e.getID() == WindowEvent.WINDOW_CLOSING) {
            result = CANCEL_OPTION;
            dispose();
        }
        super.processWindowEvent(e);
    }

    /**
     * Set the static reference to the main frame
     */
    public static void setMainFrame(mp2Frame frame) {
        theMainFrame = frame;
    }

    public static int showConfirmDialog(String message, String title) {
        return showConfirmDialog(theMainFrame, message, title);
    }

    /**
     * Static method to create and show a message box with Yes,
     * No, and Cancel buttons.
     *
     * @param  message  The message to be placed in the message box.
     *
     * @param  title    The title of the message box
     */
    public static int showConfirmDialog(Frame frame, String message, String title)
    {
        if (frame == null) {
            return CANCEL_OPTION;
        }

        JPanel messagePanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        messagePanel.add(new JLabel(message));
        mp2MessageBox dlg = new mp2MessageBox(frame, messagePanel, title, CONFIRM_DIALOG);
        return dlg.doModal();
    }

    public static int showConfirmDialog(String [] messageLines, String title) {
        return showConfirmDialog(theMainFrame, messageLines, title);
    }

    public static int showConfirmDialog(Frame frame, String [] messageLines, String title)
    {
        if (frame == null) {
            return CANCEL_OPTION;
        }

        JPanel messagePanel = new JPanel(new GridLayout(0, 1));
        for (int i=0; i<messageLines.length; i++) {
            messagePanel.add(new JLabel(messageLines[i]));
        }
        mp2MessageBox dlg = new mp2MessageBox(frame, messagePanel, title, CONFIRM_DIALOG);
        return dlg.doModal();
    }
    /**
     * Static method to create and show a message box with Yes and no buttons
     *
     * @param  message  The message to be placed in the message box.
     *
     * @param  title    The title of the message box
     */
    public static int showYesNoDialog(Frame frame, String message, String title)
    {
        if (frame == null) {
            return NO_OPTION;
        }

        JPanel messagePanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        messagePanel.add(new JLabel(message));
        mp2MessageBox dlg = new mp2MessageBox(frame, messagePanel, title, YES_NO_DIALOG);
        return dlg.doModal();
    }

    public static int showYesNoDialog(String message, String title) {
        return showYesNoDialog(theMainFrame, message, title);
    }

    public static int showYesNoDialog(Frame frame, String [] messageLines, String title)
    {
        if (frame == null) {
            return NO_OPTION;
        }

        JPanel messagePanel = new JPanel(new GridLayout(0, 1));
        for (int i=0; i<messageLines.length; i++) {
            messagePanel.add(new JLabel(messageLines[i]));
        }
        mp2MessageBox dlg = new mp2MessageBox(frame, messagePanel, title, YES_NO_DIALOG);
        return dlg.doModal();
    }

    public static int showYesNoDialog(String [] messageLines, String title) {
        return showYesNoDialog(theMainFrame, messageLines, title);
    }

    public static void showMessageDialog(String message, String title) {
        showMessageDialog(theMainFrame, message, title);
    }

    /**
     * Static method to create and show a message box with OK button
     *
     * @param  message  The message to be placed in the message box.
     *
     * @param  title    The title of the message box
     */
    public static void showMessageDialog(Frame frame, String message, String title)
    {
        if (frame == null) {
            return;
        }

        JPanel messagePanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        messagePanel.add(new JLabel(message));
        mp2MessageBox dlg = new mp2MessageBox(frame, messagePanel, title, MESSAGE_DIALOG);
        dlg.doModal();
    }

    public static void showMessageDialog(String [] messageLines, String title) {
        showMessageDialog(theMainFrame, messageLines, title);
    }

    public static void showMessageDialog(Frame frame, String [] messageLines, String title) {
        if (frame == null) {
            return;
        }

        JPanel messagePanel = new JPanel(new GridLayout(0, 1));
        for (int i=0; i<messageLines.length; i++) {
            messagePanel.add(new JLabel(messageLines[i]));
        }
        mp2MessageBox dlg = new mp2MessageBox(frame, messagePanel, title, MESSAGE_DIALOG);
        dlg.doModal();
    }


}
