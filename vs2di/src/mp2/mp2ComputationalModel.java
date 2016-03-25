/*
 * mp2ComputationalModel.java
 */
package mp2;

import java.io.*;

public abstract class mp2ComputationalModel extends mp2Model implements mp2Constants {
    
    // ivars of this class are used for saving simulation results to file
    protected String outputDirectory;
    protected String outputFileName;
    protected DataOutputStream bout;
    protected boolean saveOutputAsBinary;
    protected float saveInterval;
    protected float nextSaveTime;
    protected mp2PostProcessorOptions postOptions;
    private mp2ColorScale [] colorScales = null;

    // implementations of abstract methods in superclass
    
    public int getType() {
        return COMPUTATIONAL;
    }

    public boolean setup(String datafile, boolean restart) {
        if (!restart && doc != null) {
            drawingWidthInInches  = doc.getDrawingWidthInInches();
            drawingHeightInInches = doc.getDrawingHeightInInches();
            modelDistancePerInchX = doc.getModelDistancePerInchX();
            modelDistancePerInchY = doc.getModelDistancePerInchY();
            xOriginInInches = doc.getXOriginInInches();
            yOriginInInches = doc.getYOriginInInches();
            numDecimal = doc.getNumberOfDecimalPlacesToShow();
            rulerUnits = doc.getRulerUnits();
            float [] c = getGridBounds();
            gridReferenceX = c[0];
            gridReferenceY = c[1];
            secPerStep = postOptions.getMilliSecPerStep()/1000;
            saveInterval = postOptions.getSaveInterval();
            saveOutputAsBinary = postOptions.getSaveBinary();
            vectorMagnitudePerInch = postOptions.getVectorMagnitudePerInch();
            vectorColInterval = postOptions.getVectorColInterval();
            vectorRowInterval = postOptions.getVectorRowInterval();
            vectorMode = postOptions.getVectorMode();
            showStems = postOptions.getShowStems();
        }
        if (bout != null) {
            try {
                bout.flush();
                bout.close();
            } catch (IOException e) {}
            bout = null;
        }
        return startModel(datafile);
    }

    public int step() {
        if (saveOutputAsBinary && bout == null) {
            try {
                bout = new DataOutputStream(new BufferedOutputStream(
                       new FileOutputStream(new File(outputDirectory, outputFileName))));
                writeSimulationInfo();
                writeData();
                nextSaveTime = ((int) (getModelTime()/saveInterval) + 1) * saveInterval;
            } catch (Exception e) {
                System.out.println("io error in mp2ComputationalModel.step");
            }            
        }
        int runStatus = advanceOneStep();
        if (runStatus < 2 && saveOutputAsBinary && bout != null &&
            (saveInterval == 0 || getModelTime() >= nextSaveTime)) {
            try {
                writeData();
                nextSaveTime = ((int) (getModelTime()/saveInterval) + 1) * saveInterval;
            } catch (Exception e) {}
        }
        return runStatus;
    }

    public void closeIO() {
        cleanup();
        if (saveOutputAsBinary && bout != null) {
            try {
                bout.flush();
                bout.close();
                bout = null;
            } catch (Exception e) {}
        }
    }

    // overrides of methods in superclass
    
    public void setSecPerStep(float s) {
        super.setSecPerStep(s);
        if (postOptions != null) {
            postOptions.setMilliSecPerStep(s*1000);
        }
    }
    
    public void setShowStems(boolean b) {
        super.setShowStems(b);
        if (postOptions != null) {
            postOptions.setShowStems(b);
        }
    }

    public void setVectorColInterval(int c) {
        super.setVectorColInterval(c);
        if (postOptions != null) {
            postOptions.setVectorColInterval(c);
        }
    }

    public void setVectorMagnitudePerInch(float v) {
        super.setVectorMagnitudePerInch(v);
        if (postOptions != null) {
            postOptions.setVectorMagnitudePerInch(v);
        }
    }

    public void setVectorMode(int m) {
        super.setVectorMode(m);
        if (postOptions != null) {
            postOptions.setVectorMode(m);
        }
    }

    public void setVectorRowInterval(int r) {
        super.setVectorRowInterval(r);
        if (postOptions != null) {
            postOptions.setVectorRowInterval(r);
        }
    }

    // Get methods for ivars of this class
    
    public String getOutputDirectory() {
        return outputDirectory;
    }

    public String getOutputFileName() {
        return outputFileName;
    }

    public float getSaveInterval() {
        return saveInterval;
    }
    
    public boolean getSaveOutputAsBinary() {
        return saveOutputAsBinary;
    }

    // Set methods for ivars of this class
    
    public void setOutputDirectory(String outputDirectory) {
        this.outputDirectory = outputDirectory;
    }

    public void setOutputFileName(String outputFileName) {
        this.outputFileName = outputFileName;
    }
    
    public void setPostProcessorOptions(mp2PostProcessorOptions po) {
        postOptions = po;
    }
    
    public void setSaveInterval(float s) {
        saveInterval = s;
        if (postOptions != null) {
            postOptions.setSaveInterval(s);
        }
    }

    public boolean setSaveOutputAsBinary(boolean b) {
        if (saveOutputAsBinary != b) {
            if (bout != null) {
                try {
                    bout.flush();
                    bout.close();
                } catch (IOException e) {}
                bout = null;
            }
            saveOutputAsBinary = b;
            if (postOptions != null) {
                postOptions.setSaveBinary(b);
            }
        }
        if (saveOutputAsBinary && saveInterval > 0) {
            nextSaveTime = ((int) (getModelTime()/saveInterval) + 1) * saveInterval;
        }
        return true;
    }
    
    // Abstract Methods of this class
    
    protected abstract int getNumberOfColorScales();

    protected abstract boolean startModel(String datafile);
        
    public abstract int advanceOneStep();
    
    protected abstract void writeSimulationInfo() throws IOException;
    
    protected abstract void writeData() throws IOException;

    public abstract void cleanup();
    
    // Methods of this class
    
    public boolean doesOutputStreamExist() {
        return bout != null;
    }
    
    public mp2ColorScale [] getColorScales() {
        if (postOptions != null) {
            return postOptions.getColorScales();
        } else {
	        if (colorScales == null) {
                colorScales = new mp2ColorScale[getNumberOfColorScales()];
                for (int i=0; i<getNumberOfColorScales(); i++) {
                    colorScales[i] = new mp2ColorScale();
                    colorScales[i].init();
                }
		    }
            return colorScales;
        }
    }
    
    protected float [] getGridBounds() {
        if (doc != null) {
            mp2AbstractGridData gridData = 
                        (mp2AbstractGridData) doc.getData(MODEL_GRID);
            double [] b = gridData.getBounds();
            float [] bounds = new float[4];
            bounds[0] = (float) b[0];
            bounds[1] = (float) b[1];
            bounds[2] = (float) b[2];
            bounds[3] = (float) b[3];
            return bounds;
        } else {
            float [] bounds = new float[4];
            bounds[0] = 0;
            bounds[1] = 0;
            bounds[2] = 1;
            bounds[3] = 1;
            return bounds;
        }
    }
}
