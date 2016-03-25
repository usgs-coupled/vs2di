/*
 * vs2OutputPanel.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.border.*;
import javax.swing.*;
import javax.swing.event.*;

public class vs2OutputPanel extends vs2ModelOptionsPanel 
                        implements ListSelectionListener, vs2Constants {

    public boolean moistureContentOut;
    public boolean saturationOut;
    public boolean pressureHeadOut;
    public boolean totalHeadOut;
    public boolean velocityOut;
    public boolean outputMassBalanceEveryTimeStep;
    public boolean outputToAuxFilesEveryTimeStep;
    public boolean highPrecisionAuxiliaryOutput;
    public int outputTimeOption;
    public double outputTimeInterval;

    public DefaultListModel listModel;

    protected JCheckBox moistureContentOutCheckBox;
    protected JCheckBox saturationOutCheckBox;
    protected JCheckBox pressureHeadOutCheckBox;
    protected JCheckBox totalHeadOutCheckBox;
    protected JCheckBox velocityOutCheckBox;

    protected JRadioButton massBalanceEveryTimeStepRadioButton;
    protected JRadioButton massBalanceOutputTimesRadioButton;
    protected JRadioButton auxFilesEveryTimeStepRadioButton;
    protected JRadioButton auxFilesOutputTimesRadioButton;
    protected JRadioButton noOutputTimeRadioButton;
    protected JRadioButton timeIntervalRadioButton;
    protected JRadioButton specifiedTimesRadioButton;
    protected JTextField timeIntervalTextField;
    protected JRadioButton normalPrecisionRadioButton;
    protected JRadioButton highPrecisionRadioButton;

    protected JList list;
    protected JTextField outputTimeTextField;
    protected JButton addButton;
    protected JButton modifyButton;
    protected JButton deleteButton;


    /**
     * Creates the panel for output options
     */
    public vs2OutputPanel(vs2ModelOptionsDialog parentDialog) {

        super(parentDialog);

        listModel = new DefaultListModel();

        JPanel panel, subpanel, p1, p2, leftPanel, rightPanel;
        JLabel label;
        ButtonGroup bg;

        // Put an empty border around the panel
        setBorder(new EmptyBorder(10, 10, 10, 10));

        // Use the gridbag layout
        GridBagConstraints c = new GridBagConstraints();
        GridBagLayout gridbag = new GridBagLayout();
        setLayout(gridbag);

        // Left side
        add(leftPanel = new JPanel(gridbag, false));
        c.gridwidth = GridBagConstraints.RELATIVE;
        c.fill = GridBagConstraints.VERTICAL;
        gridbag.setConstraints(leftPanel, c);

        // Create a panel to hold output variables
        leftPanel.add(panel = new JPanel(gridbag, false));
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.HORIZONTAL;
        gridbag.setConstraints(panel, c);
        panel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Main Output File"),
                new EmptyBorder(5, 5, 5, 5)));
        panel.add(label = new JLabel("Output variables:", SwingConstants.LEFT));
        c.insets = new Insets(0, 5, 5, 0);
        gridbag.setConstraints(label, c);
        panel.add(subpanel = new JPanel(gridbag, false));
        c.insets = new Insets(0, 0, 0, 0);
        gridbag.setConstraints(subpanel, c);
        subpanel.add(p1 = new JPanel(new GridLayout(3, 1, 0, 0), false));
        c.gridwidth = GridBagConstraints.RELATIVE;
        c.insets = new Insets(0, 0, 0, 5);
        gridbag.setConstraints(p1, c);
        p1.add(pressureHeadOutCheckBox = new JCheckBox("Pressure Head"));
        p1.add(moistureContentOutCheckBox = new JCheckBox("Moisture Content"));
        p1.add(velocityOutCheckBox = new JCheckBox("Velocity"));
        subpanel.add(p1 = new JPanel(new GridLayout(3, 1, 0, 0), false));
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.insets = new Insets(0, 0, 0, 0);
        gridbag.setConstraints(p1, c);
        p1.add(totalHeadOutCheckBox = new JCheckBox("Total Head"));
        p1.add(saturationOutCheckBox = new JCheckBox("Saturation"));
        
        Component strut = Box.createVerticalStrut(10);
        panel.add(strut);
        gridbag.setConstraints(strut, c);
        
        panel.add(label = new JLabel("Mass Balance:", SwingConstants.LEFT));
        c.insets = new Insets(4, 5, 0, 5);
        c.anchor = GridBagConstraints.NORTHWEST;
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(label, c);
        panel.add(subpanel = new JPanel(new GridLayout(2, 1, 0, 0), false));
        c.insets = new Insets(0, 0, 0, 0);
        c.anchor = GridBagConstraints.CENTER;
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(subpanel, c);
        subpanel.add(massBalanceOutputTimesRadioButton =
                new JRadioButton("at output times"));
        subpanel.add(massBalanceEveryTimeStepRadioButton =
                new JRadioButton("at every time step"));
        bg = new ButtonGroup();
        bg.add(massBalanceEveryTimeStepRadioButton);
        bg.add(massBalanceOutputTimesRadioButton);
        
        leftPanel.add(panel = new JPanel(gridbag, false));
        c.insets = new Insets(20, 0, 0, 0);
        c.anchor = GridBagConstraints.WEST;
        gridbag.setConstraints(panel, c);
        panel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Auxiliary Output Files"),
                new EmptyBorder(5, 5, 5, 5)));
        panel.add(label = new JLabel("Observ. Points & Mass Balance:", SwingConstants.LEFT));
        c.insets = new Insets(4, 0, 5, 0);
        c.anchor = GridBagConstraints.CENTER;
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(label, c);
        panel.add(subpanel = new JPanel(new GridLayout(2, 1, 0, 0), false));
        c.insets = new Insets(0, 0, 0, 0);
        gridbag.setConstraints(subpanel, c);
        subpanel.add(auxFilesOutputTimesRadioButton =
                new JRadioButton("at output times"));
        subpanel.add(auxFilesEveryTimeStepRadioButton =
                new JRadioButton("at every time step"));
        bg = new ButtonGroup();
        bg.add(auxFilesEveryTimeStepRadioButton);
        bg.add(auxFilesOutputTimesRadioButton);
        panel.add(label = new JLabel("Output Precision:", SwingConstants.LEFT));
        c.insets = new Insets(4, 0, 5, 0);
        c.anchor = GridBagConstraints.CENTER;
        c.gridwidth = GridBagConstraints.REMAINDER;
        gridbag.setConstraints(label, c);
        panel.add(subpanel = new JPanel(new GridLayout(1, 2, 0, 0), false));
        c.insets = new Insets(0, 0, 0, 0);
        gridbag.setConstraints(subpanel, c);
        subpanel.add(normalPrecisionRadioButton =
                new JRadioButton("Normal"));
        subpanel.add(highPrecisionRadioButton =
                new JRadioButton("High"));
        bg = new ButtonGroup();
        bg.add(normalPrecisionRadioButton);
        bg.add(highPrecisionRadioButton);

        // Right side
        add(rightPanel = new JPanel(gridbag, false));
        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.VERTICAL;
        c.anchor = GridBagConstraints.WEST;
        gridbag.setConstraints(rightPanel, c);
        rightPanel.setBorder(new CompoundBorder(
                BorderFactory.createTitledBorder("Output Times"),
                new EmptyBorder(5, 10, 5, 10)));

        rightPanel.add(panel = new JPanel(new GridLayout(4, 1), false));
        c.insets = new Insets(0, 0, 0, 0);
        c.fill = GridBagConstraints.NONE;
        gridbag.setConstraints(panel, c);
        panel.add(noOutputTimeRadioButton =
                new JRadioButton("default"));
        panel.add(timeIntervalRadioButton =
                new JRadioButton("at regular time interval"));
        JPanel aLine = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel.add(aLine);
        aLine.add(new JLabel("Time interval = "));
        aLine.add(timeIntervalTextField = new JTextField(5));
        panel.add(specifiedTimesRadioButton =
                new JRadioButton("at specified times"));

        bg = new ButtonGroup();
        bg.add(noOutputTimeRadioButton);
        bg.add(timeIntervalRadioButton);
        bg.add(specifiedTimesRadioButton);

        rightPanel.add(panel = new JPanel(gridbag, false));
        c.insets = new Insets(5, 0, 0, 0);
        c.fill = GridBagConstraints.VERTICAL;
        gridbag.setConstraints(panel, c);

        JPanel editPanel = new JPanel(gridbag, false);
        c.insets = new Insets(0, 0, 0, 0);
        c.gridwidth = GridBagConstraints.RELATIVE;
        gridbag.setConstraints(editPanel, c);
        panel.add(editPanel);

        label = new JLabel("Time:", SwingConstants.LEFT);
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.anchor = GridBagConstraints.NORTHWEST;
        editPanel.add(label);
        gridbag.setConstraints(label, c);
        editPanel.add(outputTimeTextField = new JTextField(5));
        c.insets = new Insets(5, 0, 0, 0);
        gridbag.setConstraints(outputTimeTextField, c);
        c.insets = new Insets(10, 0, 0, 0);
        editPanel.add(addButton = new JButton("Add"));
        gridbag.setConstraints(addButton, c);
        c.insets = new Insets(5, 0, 0, 0);
        editPanel.add(modifyButton = new JButton("Modify"));
        gridbag.setConstraints(modifyButton, c);
        editPanel.add(deleteButton = new JButton("Delete"));
        gridbag.setConstraints(deleteButton, c);
        
        list = new JList(listModel);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        list.addListSelectionListener(this);
        JScrollPane scrollPane = new JScrollPane(list);
        scrollPane.setPreferredSize(new Dimension(100, 100));
        c.insets = new Insets(0, 10, 0, 0);
        c.fill = GridBagConstraints.VERTICAL;
        gridbag.setConstraints(scrollPane, c);
        panel.add(scrollPane);

        noOutputTimeRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onNoOutputTime();
                }
            }
        });
        timeIntervalRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onTimeInterval();
                }
            }
        });
        specifiedTimesRadioButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onSpecifiedTimes();
                }
            }
        });
        outputTimeTextField.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e){
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    onAdd();
                    outputTimeTextField.selectAll();
                }
            }
        });

        addButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onAdd();
            }
        });
        modifyButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onModify();
            }
        });
        deleteButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDelete();
            }
        });
    }

    protected void onNoOutputTime() {
        timeIntervalTextField.setEnabled(false);
        list.clearSelection();
        list.setEnabled(false);
        outputTimeTextField.setEnabled(false);
        addButton.setEnabled(false);
        modifyButton.setEnabled(false);
        deleteButton.setEnabled(false);
    }

    protected void onTimeInterval() {
        timeIntervalTextField.setEnabled(true);
        timeIntervalTextField.requestFocus();
        list.clearSelection();
        list.setEnabled(false);
        outputTimeTextField.setEnabled(false);
        addButton.setEnabled(false);
        modifyButton.setEnabled(false);
        deleteButton.setEnabled(false);
    }

    protected void onSpecifiedTimes() {
        timeIntervalTextField.setEnabled(false);
        list.setEnabled(true);
        addButton.setEnabled(true);
        list.clearSelection();
        if (listModel.getSize() > 0) {
            list.setSelectedIndex(0);
        }
        outputTimeTextField.setEnabled(true);
        outputTimeTextField.requestFocus();
    }

    protected void onAdd() {
        String time = outputTimeTextField.getText();
        double t;
        try {
            t = Double.valueOf(time).doubleValue();
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please enter a number",
                "Error");
            return;
        }
        if (t < 0) {
            mp2MessageBox.showMessageDialog("Please ensure that output time" 
                + " is greater than zero", "Error");
            return;
        }
        for (int i=0; i<listModel.getSize(); i++) {
            double t1 = Double.valueOf((String) listModel.get(i)).doubleValue();
            if (t == t1) {
                mp2MessageBox.showMessageDialog("Output time already exists",
                    "Error");
                int index = list.getSelectedIndex();
                outputTimeTextField.setText((String) listModel.get(index));
                list.ensureIndexIsVisible(index);
                return;
            }
            else if (t < t1) {
                listModel.add(i, time);
                list.setSelectedIndex(i);
                list.ensureIndexIsVisible(i);
                return;
            }
        }
        listModel.addElement(time);
        list.setSelectedIndex(listModel.getSize() - 1);
        list.ensureIndexIsVisible(listModel.getSize() - 1);
        //As of version 1.2, there is no limit on the number of output times
    }

    protected void onModify() {
        String time = outputTimeTextField.getText();
        double t;
        try {
            t = Double.valueOf(time).doubleValue();
        } catch (NumberFormatException e) {
            mp2MessageBox.showMessageDialog("Please enter a number",
                "Error");
            return;
        }
        if (t < 0) {
            mp2MessageBox.showMessageDialog("Please ensure that output time" 
                + " is greater than zero", "Error");
            return;
        }
        int index = list.getSelectedIndex();
        String removedTime = (String) listModel.get(index);
        listModel.remove(index);
        for (int i=0; i<listModel.getSize(); i++) {
            double t1 = Double.valueOf((String) listModel.get(i)).doubleValue();
            if (t == t1) {
                mp2MessageBox.showMessageDialog("Output time already exists",
                    "Error");
                listModel.add(index, removedTime);
                list.setSelectedIndex(index);
                return;
            }
            else if (t < t1) {
                listModel.add(i, time);
                list.setSelectedIndex(i);
                list.ensureIndexIsVisible(i);
                return;
            }
        }
        listModel.addElement(time);
        list.setSelectedIndex(listModel.getSize() - 1);
        list.ensureIndexIsVisible(listModel.getSize() - 1);
    }

    protected void onDelete() {
        int index = list.getSelectedIndex();
        listModel.remove(index);
        if (listModel.getSize() == index) {
            index--;
        }
        list.setSelectedIndex(index);
        list.ensureIndexIsVisible(index );
    }

    /**
     * Put data in components
     */
    public void init() {
        moistureContentOutCheckBox.setSelected(moistureContentOut);
        saturationOutCheckBox.setSelected(saturationOut);
        pressureHeadOutCheckBox.setSelected(pressureHeadOut);
        totalHeadOutCheckBox.setSelected(totalHeadOut);
        velocityOutCheckBox.setSelected(velocityOut);

        massBalanceEveryTimeStepRadioButton.setSelected(
                            outputMassBalanceEveryTimeStep);
        massBalanceOutputTimesRadioButton.setSelected(
                            !outputMassBalanceEveryTimeStep);
        auxFilesEveryTimeStepRadioButton.setSelected(
                            outputToAuxFilesEveryTimeStep);
        auxFilesOutputTimesRadioButton.setSelected(
                            !outputToAuxFilesEveryTimeStep);
        switch (outputTimeOption) {
        case vs2ModelOptions.INTERVAL_OUTPUT_TIME:
            timeIntervalRadioButton.setSelected(true);
            timeIntervalTextField.setText(String.valueOf(outputTimeInterval));
            break;
        case vs2ModelOptions.SPECIFIED_OUTPUT_TIMES:
            specifiedTimesRadioButton.setSelected(true);
            break;
        default:
            noOutputTimeRadioButton.setSelected(true);
            break;
        }
        
        normalPrecisionRadioButton.setSelected(!highPrecisionAuxiliaryOutput);
        highPrecisionRadioButton.setSelected(highPrecisionAuxiliaryOutput);

        if (listModel.getSize() > 0 && list.isEnabled()) {
            list.setSelectedIndex(0);
        } else {
            modifyButton.setEnabled(false);
            deleteButton.setEnabled(false);
        }
    }

    /**
     * Get data from components
     */
    public boolean retrieveData() {

        if (noOutputTimeRadioButton.isSelected()) {
            outputTimeOption = vs2ModelOptions.NO_OUTPUT_TIME;
        } else if (timeIntervalRadioButton.isSelected()) {
            try {
                outputTimeInterval = Double.valueOf(
                        timeIntervalTextField.getText()).doubleValue();
            } catch (NumberFormatException e) {
                mp2MessageBox.showMessageDialog("Please enter a number",
                    "Input Error");
                timeIntervalTextField.requestFocus();
                return false;
            }
            if (outputTimeInterval <= 0) {
                mp2MessageBox.showMessageDialog("Please ensure that" +
                    "time interval is a positive number",
                    "Input Error");
                timeIntervalTextField.requestFocus();
                return false;
            }
            outputTimeOption = vs2ModelOptions.INTERVAL_OUTPUT_TIME;
        } else if (specifiedTimesRadioButton.isSelected()) {
            if (listModel.getSize() == 0) {
                mp2MessageBox.showMessageDialog("Please specify output times.",
                    "Error");
                outputTimeTextField.requestFocus();
                return false;
            }
            outputTimeOption = vs2ModelOptions.SPECIFIED_OUTPUT_TIMES;
        }

        moistureContentOut = moistureContentOutCheckBox.isSelected();
        saturationOut = saturationOutCheckBox.isSelected();
        pressureHeadOut = pressureHeadOutCheckBox.isSelected();
        totalHeadOut = totalHeadOutCheckBox.isSelected();
        velocityOut = velocityOutCheckBox.isSelected();
        outputMassBalanceEveryTimeStep = 
                massBalanceEveryTimeStepRadioButton.isSelected();
        outputToAuxFilesEveryTimeStep = 
                auxFilesEveryTimeStepRadioButton.isSelected();
        highPrecisionAuxiliaryOutput = highPrecisionRadioButton.isSelected();

        return true;
    }

    public void valueChanged(ListSelectionEvent e) {
        if (e.getValueIsAdjusting()) {
            return;
        }
        if (!list.isEnabled()) {
            return;
        }
        if (list.isSelectionEmpty()) {
            modifyButton.setEnabled(false);
            deleteButton.setEnabled(false);
            outputTimeTextField.setText("");
        } else{
            int index = list.getSelectedIndex();
            outputTimeTextField.setText((String) listModel.get(index));
            modifyButton.setEnabled(true);
            deleteButton.setEnabled(true);
        }
    }
}
