/*
 * mp2Model.java
 */
package mp2;

import java.io.*;

public abstract class mp2Model  implements mp2Constants {

    protected double drawingWidthInInches;
    protected double drawingHeightInInches;
    protected double modelDistancePerInchX;
    protected double modelDistancePerInchY;
    protected double xOriginInInches;
    protected double yOriginInInches;
    protected int numDecimal;
    protected int rulerUnits;

    protected float gridReferenceX;
    protected float gridReferenceY;
    protected float secPerStep;
    protected float vectorMagnitudePerInch;
    protected int vectorColInterval;
    protected int vectorRowInterval;
    protected int vectorMode;
    protected boolean showStems;
    
    protected mp2Doc doc;
    
    public static final int COMPUTATIONAL = 1;
    public static final int PLAYBACK_BINARY = 2;

    public mp2Model() {
        drawingWidthInInches = 11;
        drawingHeightInInches = 8.5;
        modelDistancePerInchX = 1;
        modelDistancePerInchY = 1;
        xOriginInInches = 0;
        yOriginInInches = 0;
        numDecimal = 3;
        rulerUnits = mp2Ruler.MODEL_UNITS;
        gridReferenceX = 0;
        gridReferenceY = 0;
        secPerStep = 0;
        vectorMagnitudePerInch = 0;
        vectorColInterval = 1;
        vectorRowInterval = 1;
        vectorMode = VECTOR_AS_FLUX;
        showStems = false;
    }
    
    public abstract boolean setup(String datafile, boolean restart);

    public abstract int step();
    
    public abstract void closeIO();

    public abstract float getModelTime();
    
    public abstract int getTimeStep();
    
    public abstract int getType();
    
    public abstract mp2ColorScale [] getColorScales();
    
    public void copySimulationInfoFrom(mp2Model source) {
        drawingWidthInInches = source.drawingWidthInInches;
        drawingHeightInInches = source.drawingHeightInInches;
        modelDistancePerInchX = source.modelDistancePerInchX;
        modelDistancePerInchY = source.modelDistancePerInchY;
        xOriginInInches = source.xOriginInInches;
        yOriginInInches = source.yOriginInInches;
        numDecimal = source.numDecimal;
        rulerUnits = source.rulerUnits;

        gridReferenceX = source.gridReferenceX;
        gridReferenceY = source.gridReferenceY;
        secPerStep = source.secPerStep;
        vectorMagnitudePerInch = source.vectorMagnitudePerInch;
        vectorColInterval = source.vectorColInterval;
        vectorRowInterval = source.vectorRowInterval;
        vectorMode = source.vectorMode;
        showStems = source.showStems;
    }
    
    public double getDrawingHeightInInches() {
        return drawingHeightInInches;
    }

    public double getDrawingWidthInInches() {
        return drawingWidthInInches;
    }

    public float getGridReferenceX() {
        return gridReferenceX;
    }

    public float getGridReferenceY() {
        return gridReferenceY;
    }

    public double getModelDistancePerInchX() {
        return modelDistancePerInchX;
    }

    public double getModelDistancePerInchY() {
        return modelDistancePerInchY;
    }

    public int getNumberOfDecimalPlacesToShow() {
        return numDecimal;
    }

    public int getRulerUnits() {
        return rulerUnits;
    }

    public float getSecPerStep() {
        return secPerStep;
    }
    
    public boolean getShowStems() {
        return showStems;
    }

    public int getVectorColInterval() {
        return vectorColInterval;
    }

    public float getVectorMagnitudePerInch() {
        return vectorMagnitudePerInch;
    }

    public int getVectorMode() {
        return vectorMode;
    }

    public int getVectorRowInterval() {
        return vectorRowInterval;
    }

    public double getXOriginInInches() {
        return xOriginInInches;
    }

    public double getYOriginInInches() {
        return yOriginInInches;
    }

    // Set methods

    public void setDoc(mp2Doc doc) {
        this.doc = doc;
    }

    public void setDrawingHeightInInches(double h) {
        drawingHeightInInches = h;
    }

    public void setDrawingWidthInInches(double w) {
        drawingWidthInInches = w;
    }

    public void setGridReferenceX(float x) {
        gridReferenceX = x;
    }

    public void setGridReferenceY(float y) {
        gridReferenceY = y;
    }

    public void setModelDistancePerInchX(double d) {
        modelDistancePerInchX = d;
    }

    public void setModelDistancePerInchY(double d) {
        modelDistancePerInchY = d;
    }

    public void setNumberOfDecimalPlacesToShow(int n) {
        numDecimal = n;
    }

    public void setRulerUnits(int u) {
        rulerUnits = u;
    }

    public void setSecPerStep(float s) {
        secPerStep = s;
    }
    
    public void setShowStems(boolean b) {
        showStems = b;
    }

    public void setVectorColInterval(int c) {
        vectorColInterval = c;
    }

    public void setVectorMagnitudePerInch(float v) {
        vectorMagnitudePerInch = v;
    }

    public void setVectorMode(int m) {
        vectorMode = m;
    }

    public void setVectorRowInterval(int r) {
        vectorRowInterval = r;
    }

    public void setXOriginInInches(double xo) {
        xOriginInInches = xo;
    }

    public void setYOriginInInches(double yo) {
        yOriginInInches = yo;
    }
}
