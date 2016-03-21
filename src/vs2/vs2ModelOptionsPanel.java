/*
 * vs2ModelOptionsPanel.java
 */
package vs2;

import mp2.*;
import javax.swing.*;

/**
 * Abstract super class for tab panels in the model options dialog box
 */
public abstract class vs2ModelOptionsPanel extends JPanel {
    
    protected vs2ModelOptionsDialog parentDialog;

    public vs2ModelOptionsPanel(vs2ModelOptionsDialog parentDialog) {
        this.parentDialog = parentDialog;
    }

    public abstract void init();

    public abstract boolean retrieveData();
}
