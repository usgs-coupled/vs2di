/* 
 * mp2PostProcessorOptions.java
 */
package mp2;

public interface mp2PostProcessorOptions {

    public abstract mp2ColorScale [] getColorScales();
    
    public abstract float getMilliSecPerStep();
    public abstract float getSaveInterval();
    public abstract boolean getSaveBinary();
    public abstract float getVectorMagnitudePerInch();
    public abstract int getVectorColInterval();
    public abstract int getVectorRowInterval();
    public abstract int getVectorMode();
    public abstract boolean getShowStems();
    public abstract boolean getImageBufferingEnabled();
    
    public abstract void setMilliSecPerStep(float s);
    public abstract void setSaveInterval(float s);
    public abstract void setSaveBinary(boolean b);
    public abstract void setVectorMagnitudePerInch(float s);
    public abstract void setVectorColInterval(int c);
    public abstract void setVectorRowInterval(int r);
    public abstract void setVectorMode(int m);
    public abstract void setShowStems(boolean b);
    public abstract void setImageBufferingEnabled(boolean b);
}

