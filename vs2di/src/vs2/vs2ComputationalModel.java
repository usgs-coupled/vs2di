/*
 * vs2ComputationalModel.java
 */
package vs2;

import mp2.*;
import java.io.*;
import java.util.HashMap;
import java.util.Map;

public abstract class vs2ComputationalModel extends mp2ComputationalModel 
                                        implements vs2Model, vs2Constants {

    protected int numCell;
//    protected boolean doTransport;
    protected boolean doEnergyTransport;    // new version 1.4
    protected boolean doSoluteTransport;    // new version 1.4
    protected boolean saveMoistureContent;
    protected boolean saveSaturation;
    protected boolean saveVectors;
    protected boolean useOldVersion;
    protected int hydraulicFunctionType;
    protected int adsorptionType;
    protected double [] err = new double[2];
    protected int componentCount;
    protected String[] components;
    private Map<String, mp2ColorScale> colorScaleMap = null;

    // implementation of abstract methods in superclass
    
    protected int getNumberOfColorScales() {
//        return 7;
        return 6 + componentCount;
    }
    
    public boolean startModel(String datafile) {
        int iold = useOldVersion ? 1 : 0;
        start(iold, hydraulicFunctionType, adsorptionType, datafile);
        componentCount = getComponentCount();
        if (componentCount > 0) {
            components = getComponents();
            /*
            int i = 0;
            for (String item : components) {
                System.out.println("Item " + i++ + " " + item);
            }
            */
        }
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
            if (doEnergyTransport) {
                getTemperature(buffer);
                for (i=0; i<numCell; i++) {
                    bout.writeFloat(buffer[i]);
                }
            }
            if (doSoluteTransport) {
                for (int j=0; j<componentCount; j++) {
                    getConcentration(j, buffer);
                    for (i=0; i<numCell; i++) {
                        bout.writeFloat(buffer[i]);
                    }
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
            bout.writeUTF("<Error>");
            getFlowMassBalanceErrors(err);
            bout.writeDouble(err[0]);
            bout.writeDouble(err[1]);
            if (doEnergyTransport) {
                getHeatTransportMassBalanceErrors(err);
                bout.writeDouble(err[0]);
                bout.writeDouble(err[1]);
            }
            if (doSoluteTransport) {
                for (int j=0; j<componentCount; j++) {
                    getSoluteTransportMassBalanceErrors(j, err);
                    bout.writeDouble(err[0]);
                    bout.writeDouble(err[1]);
                }
            } else {
                assert(componentCount == 0);
            }
            bout.writeUTF("</Error>");
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
        doEnergyTransport = getDoEnergyTransport();
        doSoluteTransport = getDoSoluteTransport();
        int usage = SOLUTE_AND_ENERGY_TRANSPORT;
        assert(this instanceof vs2drt);
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
            assert(VERSION_ID >= 13);
            Map<String, mp2ColorScale> map = getColorScaleMap();
            assert(map != null && map.size() > 5);
            bout.writeInt(map.size());
            for (Map.Entry<String, mp2ColorScale> entry : map.entrySet()) {
                String key = entry.getKey();
                bout.writeUTF(key);
                mp2ColorScale cs = entry.getValue();
                bout.writeDouble(cs.GetValueBlue());
                bout.writeDouble(cs.GetValueRed());
                bout.writeDouble(cs.GetColorInterval());
                bout.writeDouble(cs.GetLabelInterval());
            }
            assert(usage == SOLUTE_AND_ENERGY_TRANSPORT);
            bout.writeBoolean(doEnergyTransport);
            bout.writeBoolean(doSoluteTransport);
            if (doSoluteTransport) {
                bout.writeInt(componentCount);
                assert(componentCount == components.length);
                for (i=0; i<componentCount; i++) {
                    bout.writeUTF(components[i]);
                }
            } else {
                assert(componentCount == 0);
            }
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
    
    protected abstract boolean getDoEnergyTransport();
    
    protected abstract boolean getDoSoluteTransport();
    
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
    
    public void getTemperature(float [] value) {
        getTransport(value);
    }
    
    public void getConcentration(int index, float [] value) {
        getTransport(value);
    }
    
    public void getHeatTransportMassBalanceErrors(double [] err) {
        getTransportMassBalanceErrors(err);
    }
    
    public void getSoluteTransportMassBalanceErrors(int index, double [] err) {
        getTransportMassBalanceErrors(err);        
    }

    public int getComponentCount() {
        return 0;
    }
    
    public int getJStop() {
        return 0;
    }

    public String[] getComponents() {
        return new String[0];
    }

    public mp2ColorScale [] getColorScales() {
        if (postOptions != null) {
            return postOptions.getColorScales();
        } else {
            if (colorScales == null) {
                super.getColorScales();
                if (colorScaleMap == null) {
                    colorScaleMap = new HashMap<>();
                }
                colorScaleMap.put("Pressure Head",    colorScales[0]);
                colorScaleMap.put("Moisture Content", colorScales[1]);
                colorScaleMap.put("Saturation",       colorScales[2]);
                colorScaleMap.put("Temperature",      colorScales[3]);
                colorScaleMap.put("Concentration",    colorScales[3]);
                colorScaleMap.put("Vector",           colorScales[4]);
                colorScaleMap.put("Total Head",       colorScales[5]);        
            }
            return colorScales;
        }
    }

    public Map<String, mp2ColorScale> getColorScaleMap() {
        if (postOptions != null) {
            return ((vs2PostProcessorOptions)postOptions).getColorScaleMap();
        }
        assert(colorScaleMap != null);
        return colorScaleMap;
    }
    
}
