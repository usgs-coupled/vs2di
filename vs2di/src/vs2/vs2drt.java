/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vs2;

/**
 *
 * @author charlton
 */
public class vs2drt extends vs2ComputationalModel {
    static  {
        try {
            System.loadLibrary("vs2drtJni");
        } catch (UnsatisfiedLinkError e) {
            System.out.println(e.toString());
            /*
            java.util.Properties p = System.getProperties();
            java.util.Enumeration keys = p.keys();
            while (keys.hasMoreElements()) {
                String key = (String)keys.nextElement();
                String value = (String)p.get(key);
                System.out.println(key + ": " + value);
            }
            */
        }
    }
    
    public vs2drt() {
        super();
        useOldVersion = false;
        hydraulicFunctionType = 1;
        adsorptionType = 1;
    }
    
    public vs2drt(int hydr) {
        super();
        useOldVersion = true;
        hydraulicFunctionType = hydr;
        adsorptionType = 1;
    }

    @Override
    public native void start(int jold, int jhydr, int jsorp, String dataFile);
    
    @Override
    public native boolean getDoTransport();
    
    @Override
    public native boolean getDoEnergyTransport();

    @Override
    public native boolean getDoSoluteTransport();    

    @Override
    public native int advanceOneStep();
    
    @Override
    public native int getNumCellAlongX();

    @Override
    public native int getNumCellAlongZ();

    @Override
    public native void getCellSizesAlongX(float [] value);

    @Override
    public native void getCellSizesAlongZ(float [] value);

    @Override
    public native void getTransport(float [] value);

    @Override
    public native void getTemperature(float [] value);
    
    @Override
    public native void getConcentration(int index, float [] value);
    
    @Override
    public native int getComponentCount();
    
    @Override
    public native String[] getComponents();    

    /**
     *
     * @param value
     */
    @Override
    public native void getMoistureContent(float [] value);

    @Override
    public native void getSaturation(float [] value);

    @Override
    public native void getPressureHead(float [] value);

    @Override
    public native void getKSat(float [] value);
    
    @Override
    public native void getTexturalClassArray(int [] value);

    @Override
    public native void getVx(float [] value);

    @Override
    public native void getVz(float [] value);

    @Override
    public native int getTimeStep();

    @Override
    public native float getModelTime();

    @Override
    public native void getFlowMassBalanceErrors(double [] err);

    @Override
    public native void getTransportMassBalanceErrors(double [] err);
    
    @Override
    public native void getHeatTransportMassBalanceErrors(double [] err);

    @Override
    public native void getSoluteTransportMassBalanceErrors(int index, double [] err);
    
    @Override
    public native void cleanup();

    @Override
    public native void releaseMemory();
    
}
