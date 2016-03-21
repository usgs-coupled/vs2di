/*
 * vs2PlaybackBinary.java
 */
package vs2;

import mp2.*;
import java.io.*;

public class vs2PlaybackBinary extends mp2PlaybackBinary implements vs2Model, vs2Constants {

    protected int usage;
    protected int nx;
    protected int nz;
    protected int numCell;
    protected int versionID;
    protected boolean doTransport;
    protected boolean doMoistureContent;
    protected boolean doSaturation;
    protected boolean doVector;
    protected float [] dx;
    protected float [] dz;
    protected float [] kSat;
    protected float [] transport;
    protected float [] moisture;
    protected float [] saturation;
    protected float [] phead;
    protected float [] vx;
    protected float [] vz;
    protected int [] tex;
    protected double [] mbFlow = new double [6];    // For backward compatibility
    protected double [] mbTransport = new double [6];    // For backward compatibility
    protected double [] mbFlowError = new double [2];
    protected double [] mbTransportError = new double [2];
    protected double [] colorParam;

    
    public vs2PlaybackBinary() {
        super();
        usage = vs2App.doHeat() ? ENERGY_TRANSPORT : SOLUTE_TRANSPORT;
    }
    
    // Implementation of abstract methods in superclass
    
    public mp2ColorScale [] getColorScales() {
        mp2ColorScale [] colorScales;
        if (versionID > 11) {
            colorScales = new mp2ColorScale[colorParam.length/4];
        } else {
            // For backward compatibility. In version 1.1, there are only
            // five color scales. In version 1.2 there are 6.
            colorScales = new mp2ColorScale[colorParam.length/4 + 1];
            // This is for the total head color scale
            colorScales[colorParam.length/4] = new mp2ColorScale();
        }
        for (int i=0; i<colorParam.length/4; i++) {
            colorScales[i] = new mp2ColorScale();
            colorScales[i].init();
            colorScales[i].SetLimits(colorParam[4*i], colorParam[4*i+1]);
            colorScales[i].SetColorInterval(colorParam[4*i+2]);
            colorScales[i].SetLabelInterval(colorParam[4*i+3]);
        }
        return colorScales;
    }

    protected void readData() throws IOException {
        int i;
        try {
            timeStep = bin.readInt();
            modelTime = bin.readFloat();
            for (i=0; i<numCell; i++) {
                phead[i] = bin.readFloat();
            }
            if (doMoistureContent) {
                for (i=0; i<numCell; i++) {
                    moisture[i] = bin.readFloat();
                }
            }
            if (doSaturation) {
                for (i=0; i<numCell; i++) {
                    saturation[i] = bin.readFloat();
                }
            }
            if (doTransport) {
                for (i=0; i<numCell; i++) {
                    transport[i] = bin.readFloat();
                }
            }
            if (doVector) {
                for (i=0; i<numCell; i++){
                    vx[i] = bin.readFloat();
                }
                for (i=0; i<numCell; i++) {
                    vz[i] = bin.readFloat();
                }
            }
            if (versionID > 11) {
                mbFlowError[0] = bin.readDouble();
                mbFlowError[1] = bin.readDouble();
                if (doTransport) {
                    mbTransportError[0] = bin.readDouble();
                    mbTransportError[1] = bin.readDouble();
                } else {
                    mbTransportError[0] = 0;
                    mbTransportError[1] = 0;
                }
            } else {
                // For backward compatibility
                for (i=0; i<6; i++) {
                    mbFlow[i] = bin.readDouble();
                }
                if (doTransport) {
                    for (i=0; i<6; i++) {
                        mbTransport[i] = bin.readDouble();
                    }
                } else {
                    for (i=0; i<6; i++) {
                        mbTransport[i] = 0;
                    }
                }
            }
        } catch (IOException e) {
            throw e;
        } catch (Exception e) {}
              
    }

    public boolean readSimulationInfo(boolean restart) throws IOException {
        int i;
        try {
            versionID = bin.readInt();
            if (versionID > VERSION_ID) {
                closeIO();
                return false;
            }
            usage = bin.readInt();
            if (!restart) {
                drawingWidthInInches  = bin.readDouble();
                drawingHeightInInches  = bin.readDouble();
                modelDistancePerInchX  = bin.readDouble();
                modelDistancePerInchY  = bin.readDouble();
                xOriginInInches = bin.readDouble();
                yOriginInInches = bin.readDouble();
                numDecimal  = bin.readInt();
                rulerUnits  = bin.readInt();
                gridReferenceX = bin.readFloat();
                gridReferenceY = bin.readFloat();
                vectorMagnitudePerInch = bin.readFloat();
                vectorColInterval = bin.readInt();
                vectorRowInterval = bin.readInt();
                vectorMode = bin.readInt();
                showStems = bin.readBoolean();
                secPerStep = bin.readFloat();
                int numColorParam = bin.readInt();
                colorParam = new double [numColorParam];
                for (i=0; i<numColorParam; i++) {
                    colorParam[i] = bin.readDouble();
                }
            } else {
                bin.readDouble();
                bin.readDouble();
                bin.readDouble();
                bin.readDouble();
                bin.readDouble();
                bin.readDouble();
                bin.readInt();
                bin.readInt();
                bin.readFloat();
                bin.readFloat();
                bin.readFloat();
                bin.readInt();
                bin.readInt();
                bin.readInt();
                bin.readBoolean();
                bin.readFloat();
                int numColorParam = bin.readInt();
                for (i=0; i<numColorParam; i++) {
                    bin.readDouble();
                }
            }
            doTransport = bin.readBoolean();
            doMoistureContent = bin.readBoolean();
            doSaturation = bin.readBoolean();
            doVector = bin.readBoolean();
            nx = bin.readInt();
            nz = bin.readInt();
            dx = new float[nx];
            dz = new float[nz];
            numCell = nx*nz;
            kSat = new float[numCell];
            phead = new float[numCell];
            tex = new int[numCell];
            if (doMoistureContent) {
                moisture = new float[numCell];
            }
            if (doSaturation) {
                saturation = new float[numCell];
            }
            if (doTransport) {
                transport = new float[numCell];
            }
            if (doVector) {
                vx = new float[numCell];
                vz = new float[numCell];
            }
            for (i=0; i<nx; i++) {
                dx[i] = bin.readFloat();
            }
            for (i=0; i<nz; i++) {
                dz[i] = bin.readFloat();
            }
            for (i=0; i<numCell; i++) {
                kSat[i] = bin.readFloat();
            }
            if (versionID > 11) {
                for (i=0; i<numCell; i++) {
                    tex[i] = bin.readInt();
                }
            } else {
                for (i=0; i<numCell; i++) {
                    tex[i] = 0;
                }
            }
        }
        catch(IOException e) {
            throw e;
        }
        catch(OutOfMemoryError e) {
            return false;
        }
        catch(Exception e) {
            return false;
        }
        return true;
    }

    // Implementation of abstract methods declared by interface vs2Model

    public int getNumCellAlongX() {
        return nx;
    }

    public int getNumCellAlongZ() {
        return nz;
    }
    
    public void getCellSizesAlongX(float [] buffer) {
        for (int i=0; i<nx; i++) {
            buffer[i] = dx[i];
        }
    }

    public void getCellSizesAlongZ(float [] buffer) {
        for (int i=0; i<nz; i++) {
            buffer[i] = dz[i];
        }
    }

    public void getMoistureContent(float [] buffer) {
        for (int i=0; i<numCell; i++) {
            buffer[i] = doMoistureContent ? moisture[i] : 0;
        }
    }

    public void getPressureHead(float [] buffer) {
        for (int i=0; i<numCell; i++) {
            buffer[i] = phead[i];
        }
    }

    public void getSaturation(float [] buffer) {
        for (int i=0; i<numCell; i++) {
            buffer[i] = doSaturation ? saturation[i] : 0;
        }
    }

    public void getTransport(float [] buffer) {
        for (int i=0; i<numCell; i++) {
            buffer[i] = doTransport ? transport[i] : 0;
        }
    }

    public void getKSat(float [] buffer) {
        for (int i=0; i<numCell; i++) {
            buffer[i] = kSat[i];
        }
    }
        
    public void getTexturalClassArray(int [] buffer) {
        for (int i=0; i<numCell; i++) {
            buffer[i] = tex[i];
        }
    }

    public void getVx(float [] buffer) {
        for (int i=0; i<numCell; i++) {
            buffer[i] = doVector ? vx[i] : 0;
        }
    }

    public void getVz(float [] buffer) {
        for (int i=0; i<numCell; i++) {
            buffer[i] = doVector ? vz[i] : 0;
        }
    }

    public void getFlowMassBalanceErrors(double [] err) {
        if (versionID > 11) {
            err[0] = mbFlowError[0];
            err[1] = mbFlowError[1];
        } else {
            // For backward compatibility
            err[0]=0;
            err[1]=0;
            double d = Math.max(Math.abs(mbFlow[0]), Math.max(Math.abs(mbFlow[1]), Math.abs(mbFlow[2])));
            if (d > 0) {
                err[0] = 100 * (mbFlow[0] + mbFlow[1] - mbFlow[2])/d;
            }
            d = Math.max(Math.abs(mbFlow[3]), Math.max(Math.abs(mbFlow[4]), Math.abs(mbFlow[5])));
            if (d > 0) {
                err[1] = 100 * (mbFlow[3] + mbFlow[4] - mbFlow[5])/d;
            }
        }
    }

    public void getTransportMassBalanceErrors(double [] err) {
        if (versionID > 11) {
            err[0] = mbTransportError[0];
            err[1] = mbTransportError[1];
        } else {
            // For backward compatibility
            err[0]=0;
            err[1]=0;
            double d = Math.max(Math.abs(mbTransport[0]), Math.max(Math.abs(mbTransport[1]), Math.abs(mbTransport[2])));
            if (d > 0) {
                err[0] = 100 * (mbTransport[0] + mbTransport[1] - mbTransport[2])/d;
            }
            d = Math.max(Math.abs(mbTransport[3]), Math.max(Math.abs(mbTransport[4]), Math.abs(mbTransport[5])));
            if (d > 0) {
                err[1] = 100 * (mbTransport[3] + mbTransport[4] - mbTransport[5])/d;
            }
        }
    }

    // Get methods for ivars of this class
    
    public boolean getDoTransport() {
        return doTransport;
    }
    
    public boolean getDoMoistureContent() {
        return doMoistureContent;
    }
    
    public boolean getDoSaturation() {
        return doSaturation;
    }
    
    public boolean getDoVector() {
        return doVector;
    }
    
    public int getUsage() {
        return usage;
    }
}
