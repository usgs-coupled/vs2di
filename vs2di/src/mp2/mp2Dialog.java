/*
 * mp2Dialog.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * Defines the behavior of a dialog box for user to input data.
 */
public abstract class mp2Dialog extends JDialog {

    /**
     * The main frame window of the application. This is taken to be
     * the dialog's parent. This static (class) variable can be
     * defined by the static method </code>setMainFrame<code>.
     * Once this variable is defined (for example, at program 
     * startup), all future instantiations will automatically use
     * the main frame as the parent.
     */
    protected static mp2Frame theMainFrame;

    /**
     * Indicates how this dialog box was closed. <code>true</code> if 
     * the user clicked the "OK" button to close the dialog box.
     * <code>false</code> otherwise.
     */
    protected boolean clickedOK;

    /**
     * Object passed to the constructor for creation of dialog box.
     */
    protected Object customObject;

    /**
     * Array of Objects passed to the constructor for creation 
     * of dialog box.
     */
    protected Object [] customArray;

    /**
     * The panel that holds the "OK", "Cancel" and the optional
     * "Apply" and "Help" buttons.
     */
    protected JPanel buttonPanel;

    /**
     * The "OK" button
     */
    protected JButton okButton;

    /**
     * The "Apply" button
     */
    protected JButton applyButton;

    /**
     * The "Cancel" button
     */
    protected JButton cancelButton;

    /**
     * The "Help" button
     */
    protected JButton helpButton;

    // Added to allow post processor to use this class
    protected static JFrame parent;

    // Constants used for data checking
    public static final int IS_POSITIVE = 101;
    public static final int IS_NON_POSITIVE = 102;
    public static final int IS_NEGATIVE = 103;
    public static final int IS_NON_NEGATIVE = 104;
    public static final int IS_GREATER_THAN = 105;
    public static final int IS_GREATER_THAN_OR_EQUAL_TO = 106;
    public static final int IS_LESS_THAN = 107;
    public static final int IS_LESS_THAN_OR_EQUAL_TO = 108;
    public static final int IS_BETWEEN_INCLUSIVE = 109;
    public static final int IS_BETWEEN_EXCLUSIVE = 110;

    private boolean modal = true;

    /**
     * Creates a dialog box with the specified title and 
     * help button option.
     *
     * @param  title  The title of the dialog box
     *
     * @param  doHelp  If <code>true</code> include a "Help" 
     *                 in the dialog box. Otherwise,
     *                 do not include a "Help" button.
     */
    public mp2Dialog(String title, boolean doHelp) {
        super(theMainFrame, title, true);
        construct(doHelp);
        parent = theMainFrame;
    }

    public mp2Dialog(String title, boolean doHelp, JFrame parent) {
        super(parent, title, true);
        construct(doHelp);
        this.parent = parent;
    }

    /**
     * Creates a dialog box with the specified title and 
     * help button option, using parameters in customObject
     *
     * @param  title  The title of the dialog box
     *
     * @param  doHelp  If <code>true</code> include a "Help" 
     *                 in the dialog box. Otherwise,
     *                 do not include a "Help" button.
     *
     * @param  customObject  an java.lang.Object used for 
     *                       construction.
     */
    public mp2Dialog(String title, boolean doHelp, 
                     Object customObject) {
        super(theMainFrame, title, true);
        this.customObject = customObject;
        construct(doHelp);
        parent = theMainFrame;
    }

    public mp2Dialog(String title, boolean doHelp, 
                     Object customObject, JFrame parent) {
        super(parent, title, true);
        this.customObject = customObject;
        construct(doHelp);
        this.parent = parent;
    }

    /**
     * Creates a dialog box with the specified title and
     * help button option, using parameters in customObject
     *
     * @param  title  The title of the dialog box
     *
     * @param  doHelp  If <code>true</code> include a "Help"
     *                 in the dialog box. Otherwise,
     *                 do not include a "Help" button.
     *
     * @param  modal   Set if dialog modal (true) or
     *                 non-modal (false);
     */
    public mp2Dialog(String title, boolean doHelp, boolean modal) {
        super(theMainFrame, title, modal);

    this.modal = modal;

        construct(doHelp);
        parent = theMainFrame;
    }

    /**
     * Creates a dialog box with the specified title and 
     * help button option, using parameters in customArray
     *
     * @param  title  The title of the dialog box
     *
     * @param  doHelp  If <code>true</code> include a "Help" 
     *                 in the dialog box. Otherwise,
     *                 do not include a "Help" button.
     *
     * @param  customArray  an java.lang.Object array used for 
     *                       construction.
     */
    public mp2Dialog(String title, boolean doHelp, 
                     Object [] customArray) {
        super(theMainFrame, title, true);
        this.customArray = customArray;
        construct(doHelp);
        parent = theMainFrame;
    }
    
    public mp2Dialog(String title, boolean doHelp, 
                     Object [] customArray, JFrame parent) {
        super(parent, title, true);
        this.customArray = customArray;
        construct(doHelp);
        this.parent = parent;
    }

    /**
     * Construct the dialog box
     */
    protected void construct(boolean doHelp) {
        // Add application specific components to the dialog box.
        makeContents();

        // Create a panel to hold the OK, Cancel, and Help buttons
        buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 
                40,10));
        buttonPanel.setBorder(new EmptyBorder(0, 0, 20, 0));
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        // Create the OK button
        buttonPanel.add(okButton = new JButton("  OK  "));
        okButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });
        okButton.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e){
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    onOK();
                }
            }
        });

        // Create the Apply button
        if (!modal) {
            buttonPanel.add(applyButton = new JButton("Apply "));
            applyButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    onApply();
                }
            });
            applyButton.addKeyListener(new KeyAdapter() {
                public void keyPressed(KeyEvent e){
                    if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                        onApply();
                    }
                }
            });
    }

        // Create the Cancel button
        buttonPanel.add(cancelButton = new JButton("Cancel"));
        cancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        });
        cancelButton.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    onCancel();
                }
            }
        });

        // Create the Help button if needed
        if (doHelp) {
            buttonPanel.add(helpButton = new JButton(" Help "));
            if (!mp2App.useJavaHelp()) {
                helpButton.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        onBrowserHelp();
                    }
                });
                helpButton.addKeyListener(new KeyAdapter() {
                    public void keyPressed(KeyEvent e) {
                        if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                            onBrowserHelp();
                        }
                    }
                });
            }
        }

        // Enable window event to handle dialog closing
        enableEvents(AWTEvent.WINDOW_EVENT_MASK);

        // Set clickedOK to false
        clickedOK = false;
    }

    /**
     * Add application specific components to the dialog box
     */
    protected abstract void makeContents();

    /**
     * Retrieve data from dialog box components
     */
    protected abstract boolean retrieveData();

    /**
     * Invoked when the Apply button is clicked.
     */
    protected void onApply() {
        if (retrieveData() == true) {
            clickedOK = false;
        }
    }

    /**
     * Invoked with the "Cancel" button is clicked
     */
    protected void onCancel() {
        clickedOK = false;
        setVisible(false);
        dispose();
    Notify();
    }

    /**
     * Show help.
     */
    protected void onBrowserHelp() {
        mp2MessageBox.showMessageDialog(parent,
                "Sorry, there is no help for this dialog box", 
                "Help");
    }

    /**
     * Invoked when the OK button is clicked.
     */
    protected void onOK() {
        if (retrieveData() == true) {
            clickedOK = true;
            setVisible(false);
            dispose();
            Notify();
        }
    }

    /** *************************************************************************
     * Handle events that need to know when OK and Cancel pressed
     */ /***********************************************************************/
    protected void Notify () {
    }

    /**
     * Display the dialog box with modal behavior. This method
     * returns when the dialog box is closed.
     *
     * @return  <code>true</code> if the user closed the dialog
     *          box by clicking the OK button. <code>false</code>
     *          otherwise.
     */
    public boolean doModal() {
        getContentPane().validate();
        pack();

        // show the dialog box in the middle of the screen
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
        return clickedOK;
    }

    /**
     */
    public void doNonModal() {
        getContentPane().validate();
        pack();

        // show the dialog box in the middle of the screen
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
    }

    /**
     * Invoded when the dialog box is closed. The result
     * is the same as if the "Cancel" button is closed.
     */
    protected void processWindowEvent(WindowEvent e) {
        if (e.getID()==WindowEvent.WINDOW_CLOSING) {
            clickedOK = false;
            setVisible(false);
            dispose();
        }
        super.processWindowEvent(e);
    }

    /**
     * Check the input data according to the specified comparison
     * criterion.
     *
     * @param  value  the variable to be checked
     *
     * @param  name   the String that contains the variable's name
     *
     * @param  comparison  comparison criterion, identified by
     *                     an <code>int</code> constant. 
     *
     * @param  c   the text field to receive focus if the 
     *             comparison criterion is not satisfied.
     *
     * @return  <code>true</code> if the comparison criterion is 
     *          satisfied, <code>false</code> otherwise.
     */
    public static boolean dataCheck(double value, String name, 
                                int comparison, JTextField c) {
        switch (comparison) {
        case IS_POSITIVE:
            if (value > 0) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name + " is positive", "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        case IS_NON_POSITIVE:
            if (value <= 0) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name + " is less than or equal to 0", 
                    "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        case IS_NEGATIVE:
            if (value < 0) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name + " is negative", "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        case IS_NON_NEGATIVE:
            if (value >= 0) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name + " is greater than or equal to 0", 
                    "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        default:
            return false;
        }
    }

    /**
     * Compares two values according to the specified comparison
     * criterion.
     *
     * @param  value1  the first variable to be compared
     *
     * @param  name1   the String that contains the first 
     *                 variable's name
     *
     * @param  value2  the second variable to be compared
     *
     * @param  name2   the String that contains the second 
     *                 variable's name
     *
     * @param  comparison  comparison criterion, identified by
     *                     an <code>int</code> constant. 
     *
     * @param  c   the text field to receive focus if the
     *             comparison criterion is not met.
     *
     * @return  <code>true</code> if the check criterion is satisfied,
     *          <code>false</code> otherwise.
     */
    public static boolean dataCheck(double value1, String name1, int comparison,
                                double value2, String name2, JTextField c) {
        switch(comparison) {
        case IS_GREATER_THAN:
            if (value1 > value2) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name1 + " is greater than " + name2, "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        case IS_GREATER_THAN_OR_EQUAL_TO:
            if (value1 >= value2) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name1 + " is greater than or equal to " + name2, 
                    "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        case IS_LESS_THAN:
            if (value1 < value2) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name1 + " is less than " + name2, "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        case IS_LESS_THAN_OR_EQUAL_TO:
            if (value1 <= value2) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name1 + " is less than or equal to " + name2, 
                    "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        default:
            return false;
        }
    }

    /**
     * Check that a value lies within a given range. 
     *
     * @param  value  the variable to be checked
     *
     * @param  name   the String that contains the variable's name
     *
     * @param  value1  the value of the range at one limit
     *
     * @param  name1   the String that contains the value at one
     *                 limit
     *
     * @param  value2  the value of the range at the other limit
     *
     * @param  name2   the String that contains the value at the
     *                 other limit
     *
     * @param  comparison  comparison criterion, identified by
     *                     an <code>int</code> constant. 
     *
     * @param  c   the text field to receive focus if the
     *             comparison criterion is not met.
     *
     * @return  <code>true</code> if the check criterion is satisfied,
     *          <code>false</code> otherwise.
     */
    public static boolean dataCheck(double value, String name, int comparison,
                                double value1, String name1, double value2, 
                                String name2, JTextField c) {
        double max = value1;
        double min = value2;
        if (value2 > value1) {
            max = value2;
            min = value1;
        }

        switch(comparison) {
        case IS_BETWEEN_INCLUSIVE:
            if (value <= max && value >= min) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name + " is between than " + name1 + " and " 
                    + name2, "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        case IS_BETWEEN_EXCLUSIVE:
            if (value < max && value > min) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name + " is between than " + name1 + " and " 
                    + name2, "Input error");
            if (c != null) {
                c.requestFocus();
                c.selectAll();
            }
            return false;
        default:
            return false;
        }
    }

    /**
     * Set the static reference to the main frame
     *
     * @param   frame   the application main frame window.
     */
    public static void setMainFrame(mp2Frame frame) {
        theMainFrame = frame;
    }
}
