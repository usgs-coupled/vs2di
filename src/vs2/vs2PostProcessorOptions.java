/* 
 * vs2PostProcessorOptions.java
 */
package vs2;

import mp2.*;
import java.io.*;

public class vs2PostProcessorOptions implements mp2PostProcessorOptions, mp2Constants, Serializable {

    static final long serialVersionUID = 3055414245232367302L;

    public mp2ColorScale pressureHeadColor;
    public mp2ColorScale moistureColor;
    public mp2ColorScale saturationColor;
    public mp2ColorScale concentrationColor;
    public mp2ColorScale temperatureColor;   // new in version 1.1
    public mp2ColorScale velocityColor;
    public mp2ColorScale totalHeadColor;
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
        // In versions 1.0 and 1.1, the color scale objects do not have
        // the member variable doDraw. This variable will be initialized as
        // false, so it is necessary to set it to true.
        pressureHeadColor.SetDoDraw(true);
        moistureColor.SetDoDraw(true);
        saturationColor.SetDoDraw(true);
        concentrationColor.SetDoDraw(true);
        temperatureColor.SetDoDraw(true);
        velocityColor.SetDoDraw(true);
        totalHeadColor.SetDoDraw(true);
        // Now initialize the color scales
        pressureHeadColor.init();
        moistureColor.init();
        saturationColor.init();
        concentrationColor.init();
        temperatureColor.init();
        velocityColor.init();
        totalHeadColor.init();
    }

    public mp2ColorScale [] getColorScales() {
        mp2ColorScale [] colorScale = new mp2ColorScale [6];
        colorScale[0] = pressureHeadColor;
        colorScale[1] = moistureColor;
        colorScale[2] = saturationColor;
        if (vs2App.doHeat()) {
            colorScale[3] = temperatureColor;
        } else {
            colorScale[3] = concentrationColor;
        }
        colorScale[4] = velocityColor;
        colorScale[5] = totalHeadColor;
        return colorScale;
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

