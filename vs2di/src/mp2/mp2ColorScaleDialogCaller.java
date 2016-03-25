/* 
 * mp2ColorScaleDialogCaller
 */
package mp2;

/**
 * A class that calls the color dialog should implement this interface
 * to enable callback from the dialog when the "Applied" button is
 * clicked.
 */
public interface mp2ColorScaleDialogCaller {

    public abstract void applyColorScale(double colorBlue, double colorRed, 
                                         double colorInterval, double labelInterval);

}