/*
 * vs2ComputationalModel.java
 */
package vs2;

import mp2.*;
import java.io.*;

public abstract class vs2ComputationalModel extends mp2ComputationalModel 
                                        implements vs2Model, vs2Constants {

    protected int numCell;
    protected boolean doTransport;
    protected boolean saveMoistureContent;
    protected boolean saveSaturation;
    protected boolean saveVectors;
    protected boolean useOldVersion;
    protected int hydraulicFunctionType;
    protected int adsorptionType;
    protected double [] err = new double[2];

    // implementation of abstract methods in superclass
    
    protected int getNumberOfColorScales() {
        return 6;
    }
    
    public boolean startModel(String datafile) {
        int iold = useOldVersion ? 1 : 0;
        start(iold, hydraulicFunctionType, adsorptionType, datafile);
        return true;
    }
    
    public abstract void releaseMemory();
    
    protected void writeData() throws IOException {
        int i;
        if (bout == null) {
            return;
        }
        try {
            bout.writeInt(getTimeStep());
            bout.writeFloat(getModelTime());
            float [] buffer = new float[numCell];
            getPressureHead(buffer);
            for (i=0; i<numCell; i++) {
                bout.writeFloat(buffer[i]);
            }
            if (saveMoistureContent) {
                getMoistureContent(buffer);
                for (i=0; i<numCell; i++) {
                    bout.writeFloat(buffer[i]);
                }
            }
            if (saveSaturation) {
                getSaturation(buffer);
                for (i=0; i<numCell; i++) {
                    bout.writeFloat(buffer[i]);
                }
            }
            if (doTransport) {
                getTransport(buffer);
                for (i=0; i<numCell; i++) {
                    bout.writeFloat(buffer[i]);
                }
            }
            if (saveVectors) {
                getVx(buffer);
                for (i=0; i<numCell; i++) {
                    bout.writeFloat(buffer[i]);
                }
                getVz(buffer);
                for (i=0; i<numCell; i++) {
                    bout.writeFloat(buffer[i]);
                }
            }
            getFlowMassBalanceErrors(err);
            bout.writeDouble(err[0]);
            bout.writeDouble(err[1]);
            if (doTransport) {
                getTransportMassBalanceErrors(err);
                bout.writeDouble(err[0]);
                bout.writeDouble(err[1]);
            }
        } catch (IOException e) {
            System.out.println("Error in write data");
            throw e;
        }
    }

    protected void writeSimulationInfo() throws IOException {
        int i;
        int nx = getNumCellAlongX();
        int nz = getNumCellAlongZ();
        numCell = nx*nz;
        doTransport = getDoTransport();
        int usage;
        if (this instanceof vs2dh) {
            usage = ENERGY_TRANSPORT;
        } else {
            usage = SOLUTE_TRANSPORT;
        }
        float [] buffer = new float[numCell];
        int [] ibuffer = new int[numCell];
        try {
            bout.writeInt(VERSION_ID);
            bout.writeInt(usage);
            bout.writeDouble(drawingWidthInInches);
            bout.writeDouble(drawingHeightInInches);
            bout.writeDouble(modelDistancePerInchX);
            bout.writeDouble(modelDistancePerInchY);
            bout.writeDouble(xOriginInInches);
            bout.writeDouble(yOriginInInches);
            bout.writeInt(numDecimal);
            bout.writeInt(rulerUnits);
            bout.writeFloat(gridReferenceX);
            bout.writeFloat(gridReferenceY);
            bout.writeFloat(vectorMagnitudePerInch);
            bout.writeInt(vectorColInterval);
            bout.writeInt(vectorRowInterval);
            bout.writeInt(vectorMode);
            bout.writeBoolean(showStems);
            bout.writeFloat(secPerStep);
            mp2ColorScale [] cs = getColorScales();
            bout.writeInt(cs.length * 4);
            for (i=0; i<cs.length; i++) {
                bout.writeDouble(cs[i].GetValueBlue());
                bout.writeDouble(cs[i].GetValueRed());
                bout.writeDouble(cs[i].GetColorInterval());
                bout.writeDouble(cs[i].GetLabelInterval());
            }
            bout.writeBoolean(doTransport);
            bout.writeBoolean(saveMoistureContent);
            bout.writeBoolean(saveSaturation);
            bout.writeBoolean(saveVectors);
            bout.writeInt(nx);
            bout.writeInt(nz);
            getCellSizesAlongX(buffer);
            for (i=0; i<nx; i++) {
                bout.writeFloat(buffer[i]);
            }
            getCellSizesAlongZ(buffer);
            for (i=0; i<nz; i++) {
                bout.writeFloat(buffer[i]);
            }
            getKSat(buffer);
            for (i=0; i<numCell; i++) {
                bout.writeFloat(buffer[i]);
            }
            getTexturalClassArray(ibuffer);
            for (i=0; i<numCell; i++) {
                bout.writeFloat(ibuffer[i]);
            }
        } catch (IOException e) {
            throw e;
        }
    }
    
    // Override method of superclass
        
    public boolean setup(String datafile, boolean restart) {
        if (!restart && postOptions != null) {
            vs2PostProcessorOptions po = (vs2PostProcessorOptions) postOptions;
            saveMoistureContent = po.saveMoistureContent;
            saveSaturation = po.saveSaturation;
            saveVectors = po.saveVectors;
        }
        return super.setup(datafile, restart);
    }

    // Abstract method of this class
    protected abstract void start(int jold, int jhydr, int jsorp, String datafile);
    
    protected abstract boolean getDoTransport();
    
    // get methods for ivars of this class
    
    public boolean getUseOldVersion() {
        return useOldVersion;
    }
    
    public int getHydraulicFunctionType() {
        return hydraulicFunctionType;
    }
    
    public int getAdsorptionType() {
        return adsorptionType;
    }
    
    public boolean getSaveMoistureContent() {
        return saveMoistureContent;
    }
    
    public boolean getSaveSaturation() {
        return saveSaturation;
    }
    
    public boolean getSaveVectors() {
        return saveVectors;
    }
    
    // Set methods for ivars of this class
    
    public void setSaveMoistureContent(boolean b) {
        saveMoistureContent = b;
        if (postOptions != null) {
            ((vs2PostProcessorOptions) postOptions).saveMoistureContent = b;
        }
    }
    
    public void setSaveSaturation(boolean b) {
        saveSaturation = b;
        if (postOptions != null) {
            ((vs2PostProcessorOptions) postOptions).saveSaturation = b;
        }
    }
    
    public void setSaveVectors(boolean b) {
        saveVectors = b;
        if (postOptions != null) {
            ((vs2PostProcessorOptions) postOptions).saveVectors = b;
        }
    }
}
