/*
 * vs2Model.java
 */
package vs2;

public interface vs2Model {

    public static final int VERSION_ID = 12; 

    public abstract int getNumCellAlongX();

    public abstract int getNumCellAlongZ();

    public abstract void getCellSizesAlongX(float [] value);

    public abstract void getCellSizesAlongZ(float [] value);

    public abstract void getTransport(float [] value); // get concentration or temperature

    public abstract void getMoistureContent(float [] value);

    public abstract void getSaturation(float [] value);

    public abstract void getPressureHead(float [] value);

    public abstract void getKSat(float [] value);
        
    public abstract void getTexturalClassArray(int [] value);

    public abstract void getVx(float [] value);

    public abstract void getVz(float [] value);

    public abstract void getFlowMassBalanceErrors(double [] err);

    public abstract void getTransportMassBalanceErrors(double [] err);
}