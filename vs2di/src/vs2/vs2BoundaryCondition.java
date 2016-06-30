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
    private int soluteType;      // new in version 1.4
    private int soluteValue;     // new in version 1.4
    
    public void copy(mp2BoundaryCondition bc) {
        if (bc instanceof vs2BoundaryCondition) {
            vs2BoundaryCondition vs2bc = (vs2BoundaryCondition) bc;
            flowType = vs2bc.flowType;
            flowValue = vs2bc.flowValue;
            transportType = vs2bc.transportType;
            transportValue = vs2bc.transportValue;
            energyType = vs2bc.energyType;
            energyValue = vs2bc.energyValue;
            soluteType = vs2bc.soluteType;    // new in version 1.4
            soluteValue = vs2bc.soluteValue;  // new in version 1.4
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
        assert(false);
    }
    
    public void setEnergyTransportType(int t) {
        energyType = t;
    }
    public void setSoluteTransportType(int t) {
        soluteType = t;
    }
    
    public void setTransportValue(double v) {
        assert(false);
    }
    public void setEnergyTransportValue(double v) {
        energyValue = v;
    }
    public void setSoluteTransportValue(int v) {
        soluteValue = v;
    }
        
    public int getEnergyTransportType() {
        return energyType;
    }    
    public int getSoluteTransportType() {
        return soluteType;
    }
    
    public double getEnergyTransportValue() {
        return energyValue;
    }    
    public int getSoluteTransportValue() {
        return soluteValue;
    }
}