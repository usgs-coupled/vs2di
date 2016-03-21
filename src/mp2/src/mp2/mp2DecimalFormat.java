/*
 * mp2DecimalFormat.java
 */
package mp2;

import java.text.*;

/**
 * A helper class to provide a static method so all other classes
 * can use a common way to show numbers to a maximum
 * number of digits.
 */
public class mp2DecimalFormat {

    /**
     * Static instance of a NumberFormat class to be shared
     */
    protected static NumberFormat nf = NumberFormat.getInstance();

    /**
     * Sets a fixed number of decimal places
     */
    public static void setFractionDigits(int numDecimal) {
        nf.setMaximumFractionDigits(numDecimal);
        nf.setMinimumFractionDigits(numDecimal);
    }

    public static void setGroupingUsed(boolean b) {
        nf.setGroupingUsed(b);
    }

    /**
     * Formats the specified value to show a maximum number of
     * decimal places.
     */
    public static String format(double value) {
        return nf.format(value);
    }
}