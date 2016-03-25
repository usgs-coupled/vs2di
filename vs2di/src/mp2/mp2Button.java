/*
 * mp2Button.java
 */
package mp2;

import java.awt.*;
import javax.swing.*;

/**
 * Extends a JButton by customizing its size and behavior
 * for placement on a tool bar.
 */
public class mp2Button extends JButton {

    public mp2Button(Icon icon) {
        super(icon);
        setPreferredSize(new Dimension(36, 36));
        setFocusPainted(false);
    }
}

