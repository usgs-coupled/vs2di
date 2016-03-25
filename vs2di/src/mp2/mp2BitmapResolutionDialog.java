/*
 * mp2BitmapResolutionDialog.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class mp2BitmapResolutionDialog extends mp2Dialog implements mp2Constants {

    public int resolution;
    public double width;
    public double height;

    protected JRadioButton screenRadioButton;
    protected JRadioButton ppi150RadioButton;
    protected JRadioButton ppi300RadioButton;
    protected JRadioButton widthRadioButton;
    protected JRadioButton heightRadioButton;
    protected JTextField widthTextField;
    protected JTextField heightTextField;

    public mp2BitmapResolutionDialog(JFrame parent) {
        super("Bitmap Resolution & Size", false, parent);
    }

    public mp2BitmapResolutionDialog() {
        super("Bitmap Resolution & Size", false);
    }

    protected void makeContents() {

        JPanel centerPanel = new JPanel(new BorderLayout(10, 10));
        centerPanel.setBorder(new EmptyBorder(20, 20, 5, 20));
        getContentPane().add(centerPanel, BorderLayout.CENTER);

        JPanel leftPanel = new JPanel(new GridLayout(0, 1));
        leftPanel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Resolution"),
                new EmptyBorder(0, 10, 10, 10)));
        leftPanel.add(screenRadioButton = new JRadioButton("Screen"));
        leftPanel.add(ppi150RadioButton = new JRadioButton("150 ppi"));
        leftPanel.add(ppi300RadioButton = new JRadioButton("300 ppi"));
        centerPanel.add(leftPanel, BorderLayout.WEST);
        
        ButtonGroup bg = new ButtonGroup();
        bg.add(screenRadioButton);
        bg.add(ppi150RadioButton);
        bg.add(ppi300RadioButton);
        
        JPanel rightPanel = new JPanel(new BorderLayout());
        JPanel panel1 = new JPanel(new GridLayout(0, 1, 10, 10));
        panel1.add(widthRadioButton = new JRadioButton("Width", true));
        panel1.add(heightRadioButton = new JRadioButton("Height", false));
        rightPanel.add(panel1, BorderLayout.WEST);
        JPanel panel2 = new JPanel(new GridLayout(0, 1, 10, 10));
        panel2.add(widthTextField = new JTextField(5));
        panel2.add(heightTextField = new JTextField(5));
        rightPanel.add(panel2, BorderLayout.CENTER);
        
        JPanel bufferPanel = new JPanel(new GridBagLayout());
        bufferPanel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Size (inches)"),
                new EmptyBorder(0, 10, 10, 10)));
        bufferPanel.add(rightPanel);
        
        centerPanel.add(bufferPanel, BorderLayout.CENTER);
        
        bg = new ButtonGroup();
        bg.add(widthRadioButton);
        bg.add(heightRadioButton);
        
        screenRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    heightRadioButton.setEnabled(false);
                    widthRadioButton.setEnabled(false);
                    heightTextField.setEnabled(false);
                    widthTextField.setEnabled(false);
                } else {
                    heightRadioButton.setEnabled(true);
                    widthRadioButton.setEnabled(true);
                    if (heightRadioButton.isSelected()) {
                        heightTextField.setEnabled(true);
                    } else {
                        widthTextField.setEnabled(true);
                    }
                }
            }
        });
        widthRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    widthTextField.setEnabled(true);
                    heightTextField.setText("");
                    heightTextField.setEnabled(false);
                }
            }
        });
        heightRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    heightTextField.setEnabled(true);
                    widthTextField.setText("");
                    widthTextField.setEnabled(false);
                }
            }
        });
    }

    public boolean doModal() {
        if (resolution == 2) {
            ppi300RadioButton.setSelected(true);
        } else if (resolution == 1) {
            ppi150RadioButton.setSelected(true);
        } else {
            screenRadioButton.setSelected(true);
        }
        if (resolution == 0) {
            heightRadioButton.setEnabled(false);
            widthRadioButton.setEnabled(false);
            heightTextField.setEnabled(false);
            widthTextField.setEnabled(false);
        } else {
            widthRadioButton.setSelected(true);
        }
        return super.doModal();
    }

    protected boolean retrieveData() {
        if (ppi300RadioButton.isSelected()) {
            resolution = 2;
        } else if (ppi150RadioButton.isSelected()) {
            resolution = 1;
        } else {
            resolution = 0;
            return true;
        }
        if (widthRadioButton.isSelected()) {
            try {
                width = Double.valueOf(widthTextField.getText()).doubleValue();
            }catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog(parent, "Please check your input.",
                        "Input Error");
                return false;
            }
            if (!dataCheck(width, "width", IS_POSITIVE, widthTextField)) {
                    return false;
            }
            height = -1;
        } else {
            try {
                height = Double.valueOf(heightTextField.getText()).doubleValue();
            }catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog(parent, "Please check your input.",
                        "Input Error");
                return false;
            }
            if (!dataCheck(height, "height", IS_POSITIVE, heightTextField)) {
                    return false;
            }
            width = -1;
        }
        return true;
    }
}
