/*
 * mp2RectilinearGridWindowInterface.java
 */
package mp2;

/**
 * Defines methods to be implemented by the win32 or unix version
 * of grid window. The win32 version subclasses JDialog, whereas
 * the unix version subclass JFrame. This is necessary do to
 * inconsistent bug in unix implementation of JDialog.
 */
public interface mp2RectilinearGridWindowInterface {

    abstract public void deselectAllCoords();

    abstract public void deselectAllXCoords();

    abstract public void deselectAllYCoords();
    
    abstract public void doDispose();
    
    abstract public void doRepaint();

    abstract public void fireGridDataChanged();

    abstract public int getSelectedXCoordCount();

    abstract public int [] getSelectedXCoordIndices();

    abstract public int getSelectedYCoordCount();

    abstract public int [] getSelectedYCoordIndices();
    
    abstract public boolean isWindowVisible();

    abstract public void selectXCoord(int i);

    abstract public void selectXCoordInterval(int a, int b);

    abstract public void selectYCoord(int i);

    abstract public void selectYCoordInterval(int a, int b);

    abstract public void setDeleteButtonEnabled(boolean b);

    abstract public void setEnabled(boolean b);

    abstract public void setSubdivideButtonEnabled(boolean b);
    
    abstract public void setWindowVisible(boolean b);
}
