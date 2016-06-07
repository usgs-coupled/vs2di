/*
 * vs2dh.java
 */
package vs2;

public class vs2dh extends vs2ComputationalModel {

    static  {
        try {
            System.loadLibrary("vs2dhJni");
        } catch (UnsatisfiedLinkError e) {
            System.out.println(e.toString());
        }
    }
    
    public vs2dh() {
        super();
        useOldVersion = false;
        hydraulicFunctionType = 1;
        adsorptionType = 1;
    }
    
    public vs2dh(int hydr) {
        super();
        useOldVersion = true;
        hydraulicFunctionType = hydr;
        adsorptionType = 1;
    }

    public native void start(int jold, int jhydr, int jsorp, String dataFile);
    
    public native boolean getDoTransport();
    
    public boolean getDoEnergyTransport() {
        return getDoTransport();
    }    
    
    public boolean getDoSoluteTransport() {
        return false;
    }    

    public native int advanceOneStep();

    public native int getNumCellAlongX();

    public native int getNumCellAlongZ();

    public native void getCellSizesAlongX(float [] value);

    public native void getCellSizesAlongZ(float [] value);

    public native void getTransport(float [] value);

    public native void getMoistureContent(float [] value);

    public native void getSaturation(float [] value);

    public native void getPressureHead(float [] value);

    public native void getKSat(float [] value);
    
    public native void getTexturalClassArray(int [] value);

    public native void getVx(float [] value);

    public native void getVz(float [] value);

    public native int getTimeStep();

    public native float getModelTime();

    public native void getFlowMassBalanceErrors(double [] err);

    public native void getTransportMassBalanceErrors(double [] err);

    public native void cleanup();

    public native void releaseMemory();

}
