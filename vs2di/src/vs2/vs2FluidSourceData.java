/*
 * htFluidSourceData
 */

package vs2;

import mp2.*;
import java.awt.*;
import java.io.*;
import java.util.Vector;

public class vs2FluidSourceData extends mp2SourceData 
        implements vs2Constants, Serializable {
    
    static final long serialVersionUID = 6947483098856679836L;
    
    protected mp2TableData createStrengthData() {
        return new vs2SourceStrengthData();
    }
    
    public void exportPeriod(PrintWriter pw, int period, vs2ModelOptions modelOptions) {
        discretize();
        if (sourceCellIndices == null || sourceCellIndices.size() == 0) {
            return;
        }
        int cellIndex, numFluidSource, row, col, numCol, j, flowType, transportType;
        double flowValue, transportValue;
        Object [] aRow;
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        double [] xCoord = rectGridData.getXCoords();
        double [] yCoord = rectGridData.getYCoords();
        numCol = rectGridData.getXCoordCount() - 1;
        numFluidSource = shapes.size();
        for (j=0; j<numFluidSource; j++) {
            cellIndex = ((Integer) sourceCellIndices.elementAt(j)).intValue();
            row = cellIndex/numCol;
            col = cellIndex - row*numCol;
            aRow = ((mp2TableData) sourceStrengthAllData.elementAt(j)).getRow(period);
            flowType = ((Integer) aRow[1]).intValue();
            flowValue = ((Double) aRow[2]).doubleValue();
            if ( flowType == NORMAL_FLUID_FLUX_BC) {
                if (flowValue == 0) {
                    flowType = NO_FLOW_BC;
                } else {
                    flowValue /= (xCoord[col+1] - xCoord[col]);
                }
            }
            else if (flowType == TOTAL_HEAD_BC) {
                // The total head in VS2DT is measured from the
                // top of the grid (the top of the second row if
                // boundary cells are counted).
                flowValue += yCoord[0];
            }
            if (modelOptions.doEnergyTransport || modelOptions.doSoluteTransport) {
                pw.println((row+2) + " " + (col+2) + " " + flowType + " " + (float) flowValue + "    /C11 -- JJ, NN, NTX, PFDUM");
                if (modelOptions.doEnergyTransport) {
                    transportType = ((Integer) aRow[5]).intValue();
                    transportValue = ((Double) aRow[6]).doubleValue();
                    if (transportType == DIFFUSIVE_FLUX_BC) {
                        transportValue /= (xCoord[col+1] - xCoord[col]);
                    }
                    pw.println(transportType + " " + (float) transportValue + "    /C12 -- NTT, TF");
                }
                if (modelOptions.doSoluteTransport) {
                    transportType = ((Integer) aRow[3]).intValue();
                    int solnum = ((Integer) aRow[4]).intValue();
                    pw.println(transportType + " " + solnum + " -1 1.    /C13 -- NTC, INSBC1, INSBC2, SBFRAC");
                }
            } else {
                pw.println((row+2) + " " + (col+2) + " " + flowType + " " + (float) flowValue + "    /C14 -- JJ, NN, NTX, PFDUM");
            }

        }
        
    }
}
