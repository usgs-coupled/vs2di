/*
 * vs2Model.java
 */
package vs2;

import java.util.Map;
import mp2.mp2ColorScale;

public interface vs2Model {

    public static final int VERSION_ID = 13; 

    public abstract int getNumCellAlongX();

    public abstract int getNumCellAlongZ();

    public abstract void getCellSizesAlongX(float [] value);

    public abstract void getCellSizesAlongZ(float [] value);

    public abstract void getTransport(float [] value); // get concentration or temperature (replaced by getTemperature and getConcentration)

    public abstract void getTemperature(float [] value);
    
    public abstract void getConcentration(int index, float [] value);

    public abstract void getMoistureContent(float [] value);

    public abstract void getSaturation(float [] value);

    public abstract void getPressureHead(float [] value);

    public abstract void getKSat(float [] value);
        
    public abstract void getTexturalClassArray(int [] value);

    public abstract void getVx(float [] value);

    public abstract void getVz(float [] value);

    public abstract void getFlowMassBalanceErrors(double [] err);

    public abstract void getTransportMassBalanceErrors(double [] err);     // replaced by getHeatTransportMassBalanceErrors and getSoluteTransportMassBalanceErrors

    public abstract void getHeatTransportMassBalanceErrors(double [] err);
    
    public abstract void getSoluteTransportMassBalanceErrors(int index, double [] err);
    
    public abstract Map<String, mp2ColorScale> getColorScaleMap();

}