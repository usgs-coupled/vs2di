/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vs2;

import mp2.*;
import javax.swing.JFrame;
import javax.swing.JTextField;

/**
 *
 * @author charlton
 */
public abstract class vs2Dialog extends mp2Dialog {

    // Constants used for data checking
    public static final int IS_NON_NEGATIVE_OR_NEGATIVE_ONE = 200;   
    
    
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
    public vs2Dialog(String title, boolean doHelp) {
        super(title, doHelp);
    }

    public vs2Dialog(String title, boolean doHelp, JFrame parent) {
        super(title, doHelp, parent);
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
    public vs2Dialog(String title, boolean doHelp, Object customObject) {
        super(title, doHelp, customObject);
    }

    public vs2Dialog(String title, boolean doHelp, Object customObject, JFrame parent) {
        super(title, doHelp, customObject, parent);
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
    public vs2Dialog(String title, boolean doHelp, boolean modal) {
        super(title, doHelp, modal);
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
    public vs2Dialog(String title, boolean doHelp, Object [] customArray) {
        super(title, doHelp, customArray);
    }
    
    public vs2Dialog(String title, boolean doHelp, Object [] customArray, JFrame parent) {
        super(title, doHelp, customArray, parent);
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
    public static boolean dataCheck(int value, String name, 
                                int comparison, JTextField c) {
        switch (comparison) {
        case IS_NON_NEGATIVE_OR_NEGATIVE_ONE:
            if (value >= 0 || value == -1) {
                return true;
            }
            mp2MessageBox.showMessageDialog(parent, "Please ensure that " 
                    + name + " is greater than or equal to zero, or -1", 
                    "Input error");
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
            assert(false);
            return false;
        }
    }
}
