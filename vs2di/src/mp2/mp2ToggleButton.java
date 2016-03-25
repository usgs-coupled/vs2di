/*
 * mp2ToggleButton.java
 */
package mp2;

import java.awt.*;
import javax.swing.*;

/**
 * Extends a JToggleButton by customizing its size and behavior
 * for placement on a tool bar.
 */
public class mp2ToggleButton extends JToggleButton {

    public mp2ToggleButton(Icon icon) {
        super(icon);
        customize();
    }

    public mp2ToggleButton(Icon icon, boolean selected) {
        super(icon, selected);
        customize();
    }

    protected void customize() {
        setPreferredSize(new Dimension(36, 36));
        setFocusPainted(false);
    }
}

