/*
 * mp2AbstractGridData.java
 */
package mp2;

import java.io.*;
import java.awt.*;
import java.util.Vector;

public abstract class mp2AbstractGridData extends mp2GraphicalData
        implements Serializable, mp2Constants {
    
    static final long serialVersionUID = -3527539557492316249L;
    
    protected transient Vector register;

    public abstract boolean isDefined();
    
    public void init(mp2Doc doc) {
        super.init(doc);
        register = new Vector();
    }
    
    public abstract double [] getBounds();

    /**
     * Registers those objects that need to be notified if the grid changes
     */
    public void register(mp2Discretizable d) {
        register.addElement(d);
    }

    /**
     * Notify registered data that grid has changed
     */
    public void notifyRegisteredData() {
        for (int i=0; i<register.size(); i++) {
            ((mp2Discretizable) register.elementAt(i)).notifyActiveCellsChanged();
        }
    }

    /**
     * Invoked when the domain is changed
     */
    public void onDomainChanged() {
        notifyRegisteredData();
    }

}