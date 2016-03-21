/*
 * vs2SimulationDialog
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class vs2SimulationDialog extends mp2SimulationDialog {

    public boolean showDataList;
    public boolean saveMoistureContent;
    public boolean saveSaturation;
    public boolean saveVectors;
    protected JCheckBox saveMoistureContentCheckBox;
    protected JCheckBox saveSaturationCheckBox;
    protected JCheckBox saveVectorsCheckBox;

    public vs2SimulationDialog(JFrame parent, boolean computing) {
        super(parent, computing);
    }
    
    protected void makeContents() {
        super.makeContents();
        saveMoistureContentCheckBox = new JCheckBox("include moisture content");
        saveSaturationCheckBox = new JCheckBox("include saturation");
        saveVectorsCheckBox = new JCheckBox("include vectors");
    }

    public boolean doModal() {
        if (showDataList) {
            JPanel panel = new JPanel(new GridLayout(0, 1, 5, 5));
            dataListPanel.add(panel, BorderLayout.CENTER);
            panel.add(saveMoistureContentCheckBox);
            panel.add(saveSaturationCheckBox);
            panel.add(saveVectorsCheckBox);
            saveBinaryCheckBox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    onSaveBinary(e.getStateChange() == ItemEvent.SELECTED);
                }
            });
            saveVectorsCheckBox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    onSaveVectors(e.getStateChange() == ItemEvent.SELECTED);
                }
            });
            saveMoistureContentCheckBox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    onSaveMoistureContent(e.getStateChange() == ItemEvent.SELECTED);
                }
            });
        }
        saveMoistureContentCheckBox.setSelected(saveMoistureContent);
        saveSaturationCheckBox.setSelected(saveSaturation);
        saveVectorsCheckBox.setSelected(saveVectors);
        return super.doModal();
    }
    
    protected void onSaveBinary(boolean b) {
        saveMoistureContentCheckBox.setEnabled(b);
        saveSaturationCheckBox.setEnabled(b);
        saveVectorsCheckBox.setEnabled(b);
    }
    
    protected void onSaveVectors(boolean b) {
        if (b) {
            saveMoistureContentCheckBox.setSelected(true);
        }
    }
    
    protected void onSaveMoistureContent(boolean b) {
        if (!b) {
            saveVectorsCheckBox.setSelected(false);
        }
    }

    protected boolean retrieveData() {
        if (!super.retrieveData()) {
            return false;
        }
        saveMoistureContent = saveMoistureContentCheckBox.isSelected();
        saveSaturation = saveSaturationCheckBox.isSelected();
        saveVectors = saveVectorsCheckBox.isSelected();
        return true;
    }
}
