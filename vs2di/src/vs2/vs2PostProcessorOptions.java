/* 
 * vs2PostProcessorOptions.java
 */
package vs2;

import mp2.*;
import java.io.*;
import java.util.Map;
import java.util.HashMap;

public class vs2PostProcessorOptions implements mp2PostProcessorOptions, mp2Constants, Serializable {

    static final long serialVersionUID = 3055414245232367302L;

    private mp2ColorScale pressureHeadColor;
    private mp2ColorScale moistureColor;
    private mp2ColorScale saturationColor;
    private mp2ColorScale concentrationColor;
    private mp2ColorScale temperatureColor;   // new in version 1.1
    private mp2ColorScale velocityColor;
    private mp2ColorScale totalHeadColor;
    protected Map<String, mp2ColorScale> colorScaleMap;
    public float milliSecPerStep = 0;
    public float saveInterval;
    public boolean saveBinary;
    public float vectorMagnitudePerInch;
    public int vectorColInterval;
    public int vectorRowInterval;
    public int vectorMode;
    public boolean imageBufferingEnabled;
    public boolean showStems;
    public boolean saveMoistureContent;
    public boolean saveSaturation;
    public boolean saveVectors;
    public int startupDisplay;
    public int startupDrawingMode;
    public boolean showZonationAtStartup;
    public boolean showVectorAtStartup;

    public vs2PostProcessorOptions() {
        pressureHeadColor = new mp2ColorScale();
        pressureHeadColor.SetLimits(0, -5);
        pressureHeadColor.SetColorInterval(0.5);
        pressureHeadColor.SetLabelInterval(0.5);
        moistureColor = new mp2ColorScale();
        moistureColor.SetLimits(0.5, 0);
        moistureColor.SetColorInterval(0.05);
        moistureColor.SetLabelInterval(0.05);
        saturationColor = new mp2ColorScale();
        saturationColor.SetLimits(1, 0);
        saturationColor.SetColorInterval(0.1);
        saturationColor.SetLabelInterval(0.1);
        concentrationColor = new mp2ColorScale();
        concentrationColor.SetLimits(0, 1);
        concentrationColor.SetColorInterval(0.1);
        concentrationColor.SetLabelInterval(0.1);
        temperatureColor = new mp2ColorScale();
        temperatureColor.SetLimits(4, 24);
        temperatureColor.SetColorInterval(2);
        temperatureColor.SetLabelInterval(2);
        velocityColor = new mp2ColorScale();
        velocityColor.SetLimits(0, 1);
        velocityColor.SetColorInterval(0.1);
        velocityColor.SetLabelInterval(0.1);
        totalHeadColor = new mp2ColorScale();
        milliSecPerStep = 0;
        saveInterval = 0;
        saveBinary = false;
        vectorMagnitudePerInch = 1;
        vectorColInterval = 1;
        vectorRowInterval = 1;
        vectorMode = VECTOR_AS_VELOCITY;
        saveMoistureContent = true;
        saveSaturation = true;
        saveVectors = true;
        startupDisplay = 1;
        startupDrawingMode = 0;
        showZonationAtStartup = false;
        showVectorAtStartup = false;
    }
    
    private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException {
        // unserialize default
        in.defaultReadObject();
        assert(true);
    }
    

    public void init() {
        // assign default values if this object is deserialized
        // from an earlier version. This can be identified by
        // absence of a temperature scale.
        if (temperatureColor == null) {
            vectorMagnitudePerInch = 1;
            vectorColInterval = 1;
            vectorRowInterval = 1;
            // Note that in both versions 1.0 beta and 1.0 final,
            // velocityColor was mistakenly used for concentration,
            // and concentrationColor was not used. Here we have
            // to reverse this.
            mp2ColorScale temp = concentrationColor;
            concentrationColor = velocityColor;
            velocityColor = temp;
            // Now we set the velocity color scale
            velocityColor.SetLimits(0, 1);
            velocityColor.SetColorInterval(0.1);
            velocityColor.SetLabelInterval(0.1);
            vectorMode = VECTOR_AS_VELOCITY;
            temperatureColor = new mp2ColorScale();
            temperatureColor.SetLimits(4, 24);
            temperatureColor.SetColorInterval(2);
            temperatureColor.SetLabelInterval(2);
            saveMoistureContent = true;
            saveSaturation = true;
            saveVectors = true;
        }
        if (totalHeadColor == null) {
            totalHeadColor = new mp2ColorScale();
        }
        if (colorScaleMap == null) {
            colorScaleMap = new HashMap<>();
            colorScaleMap.put("Pressure Head",    pressureHeadColor);
            colorScaleMap.put("Moisture Content", moistureColor);
            colorScaleMap.put("Saturation",       saturationColor);
            colorScaleMap.put("Temperature",      temperatureColor);
            colorScaleMap.put("Concentration",    concentrationColor);
            colorScaleMap.put("Vector",           velocityColor);
            colorScaleMap.put("Total Head",       totalHeadColor);
        }
        for (Map.Entry<String, mp2ColorScale> entry : colorScaleMap.entrySet()) {
            mp2ColorScale cs = entry.getValue();
            // In versions 1.0 and 1.1, the color scale objects do not have
            // the member variable doDraw. This variable will be initialized as
            // false, so it is necessary to set it to true.
            cs.SetDoDraw(true);
            // Now initialize the color scales
            cs.init();
        }
    }

    public mp2ColorScale [] getColorScales() {
        mp2ColorScale [] colorScale = new mp2ColorScale[7];
        colorScale[0] = pressureHeadColor;
        colorScale[1] = moistureColor;
        colorScale[2] = saturationColor;
        colorScale[3] = temperatureColor;
        colorScale[4] = concentrationColor;
        colorScale[5] = velocityColor;
        colorScale[6] = totalHeadColor;
        return colorScale;
    }

    public Map<String, mp2ColorScale> getColorScaleMap() {
        assert(colorScaleMap != null);
        return colorScaleMap;
    }
    
    public float getMilliSecPerStep() {return milliSecPerStep;}
    public float getSaveInterval() {return saveInterval;}
    public boolean getSaveBinary() {return saveBinary;}
    public float getVectorMagnitudePerInch() {return vectorMagnitudePerInch;}
    public int getVectorColInterval() {return vectorColInterval;}
    public int getVectorRowInterval() {return vectorRowInterval;}
    public int getVectorMode() {return vectorMode;}
    public boolean getShowStems() {return showStems;}
    public boolean getImageBufferingEnabled() {return imageBufferingEnabled;}
    
    public void setMilliSecPerStep(float s) {milliSecPerStep = s;}
    public void setSaveInterval(float s) {saveInterval = s;}
    public void setSaveBinary(boolean b) {saveBinary = b;}
    public void setVectorMagnitudePerInch(float s) {vectorMagnitudePerInch = s;}
    public void setVectorColInterval(int c) {vectorColInterval = c;}
    public void setVectorRowInterval(int r) {vectorRowInterval = r;}
    public void setVectorMode(int m) {vectorMode = m;}
    public void setShowStems(boolean b) {showStems = b;}
    public void setImageBufferingEnabled(boolean b) {imageBufferingEnabled = b;}
}

