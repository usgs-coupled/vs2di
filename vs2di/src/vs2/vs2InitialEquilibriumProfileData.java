/*
 * vs2InitialEquilibriumProfileData.java
 */
package vs2;

import mp2.*;
import java.io.*;

public class vs2InitialEquilibriumProfileData extends mp2GraphicalData 
        implements vs2Constants, Serializable {

    static final long serialVersionUID = 1701276681305605888L;

    protected double waterTableLocation = Double.NEGATIVE_INFINITY;
    protected double minimumPressureHead = Double.NEGATIVE_INFINITY;

    public void exportData(PrintWriter pw, double [] yCoord) {
        String s;
        final int commentOffset = 23;        
        s = String.valueOf("2 1.0"); 
        pw.println(s + vs2App.tab(s, commentOffset)
                + "/B-15 -- IREAD, FACTOR");
        float wt = (float) (waterTableLocation - yCoord[0]);
        float minP = (float) minimumPressureHead;
        s = String.valueOf(wt + " " + minP);
        pw.println(s + vs2App.tab(s, commentOffset)
                + "/B-16 -- DWTX, HMIN");
    }
    
    public void setWaterTableLocation(double z) {
        waterTableLocation = z;
        doc.setChanged(true);
    }

    public double getWaterTableLocation() {
        return waterTableLocation;
    }

    public void setMinimumPressureHead(double h) {
        minimumPressureHead = h;
        doc.setChanged(true);
    }

    public double getMinimumPressureHead() {
        return minimumPressureHead;
    }
}
