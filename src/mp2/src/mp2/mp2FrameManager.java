/*
 * mp2FrameManager.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;

public abstract class mp2FrameManager implements mp2Constants {

    protected mp2App theApp;

    public mp2FrameManager(mp2App theApp) {
        this.theApp = theApp;
    }

    public abstract void prepareFrame(mp2Frame frame);

    public void onSelectedData() {}

    public void setMenuItemsStartingStates() {}

    protected void onModelOptions() {}

    public void setInformationText(int information, String text) {}
    
    public void initializeInformationText() {}

    public JMenuItem getMenuItem(int index) {
        return null;
    }

    public abstract void resetMenuItems();


}
