/*
 * vs2BoundaryCondition.java
 */
package vs2;

import mp2.*;
import java.io.*;
/**
 * An empty interface used to designated a class as
 * the boundary conditions for a specific model
 */
public class vs2BoundaryCondition extends mp2BoundaryCondition
                                  implements Serializable {

    static final long serialVersionUID = -1093336568431090111L;

    public int flowType;
    public double flowValue;
    // The following two fields should have been called
    // soluteType and soluteValue, but we will keep these
    // names for backward compatibility. However, we will
    // access the four fields below by using get and set
    // methods, which get and set the correct data depending
    // on whether we are simulating solute or energy transport.
    private int transportType;
    private double transportValue;
    private int energyType;      // new in version 1.1
    private double energyValue;  // new in version 1.1

    public void copy(mp2BoundaryCondition bc) {
        if (bc instanceof vs2BoundaryCondition) {
            vs2BoundaryCondition vs2bc = (vs2BoundaryCondition) bc;
            flowType = vs2bc.flowType;
            flowValue = vs2bc.flowValue;
            transportType = vs2bc.transportType;
            transportValue = vs2bc.transportValue;
            energyType = vs2bc.energyType;
            energyValue = vs2bc.energyValue;
        } else {
            // should throw an exception for trying to
            // copy the wrong type of bc
        }
    }
    
    // In the four methods below, "transport" means solute or 
    // energy transport. These methods set and get the proper
    // data depending on whether we are simulating energy 
    // transport or solute transport.
    
    public void setTransportType(int t) {
        if (vs2App.doHeat()) {
            energyType = t;
        } else {
            transportType = t;
        }
    }
    
    public void setTransportValue(double v) {
        if (vs2App.doHeat()) {
            energyValue = v;
        } else {
            transportValue = v;
        }
    }
        
    public int getTransportType() {
        if (vs2App.doHeat()) {
            return energyType;
        } else {
            return transportType;
        }
    }
    
    public double getTransportValue() {
        if (vs2App.doHeat()) {
            return energyValue;
        } else {
            return transportValue;
        }
    }
}