/*
 * vs2FluidSourceView.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.undo.*;

public class vs2FluidSourceView extends mp2SourceView
                                implements mp2Constants, vs2Constants {

    protected vs2ModelOptions modelOptions;

    public vs2FluidSourceView(mp2View view,
                    vs2FluidSourceData shapesData,
                    mp2AbstractGridData gridData,
                    vs2ModelOptions modelOptions,
                    String homeDirectory) {

        super(view, shapesData, gridData, homeDirectory);

        this.modelOptions = modelOptions;
    }
    
    protected void getPropertiesForSource(int isrc) {
        vs2SourceStrengthData tableData = new vs2SourceStrengthData();
        Object [] customArray = new Object[4];
        customArray[0] = tableData;
//        if (modelOptions.doTransport) {
        if (modelOptions.doEnergyTransport || modelOptions.doSoluteTransport) {
            if (modelOptions.doEnergyTransport && modelOptions.doSoluteTransport) {
                // energy and solute transport
                int [] columnMask = new int [7];
                columnMask[0] = 0;
                columnMask[1] = 1;
                columnMask[2] = 2;
                columnMask[3] = 5;
                columnMask[4] = 6;
                columnMask[5] = 3;
                columnMask[6] = 4;
                customArray[1] = "Both";
                customArray[2] = columnMask;
            } else {
                // energy or solute transport
                assert(modelOptions.doEnergyTransport ^ modelOptions.doSoluteTransport);
                int [] columnMask = new int [5];
                columnMask[0] = 0;
                columnMask[1] = 1;
                columnMask[2] = 2;
                if (modelOptions.doEnergyTransport) {
                    columnMask[3] = 5;
                    columnMask[4] = 6;
                    customArray[1] = "Temp.";
                } else {
                    columnMask[3] = 3;
                    columnMask[4] = 4;
                    customArray[1] = "Conc.";
                }
                customArray[2] = columnMask;
            }
        } else {
            // no transport
            int [] columnMask = new int [3];
            columnMask[0] = 0;
            columnMask[1] = 1;
            columnMask[2] = 2;
            customArray[1] = null;
            customArray[2] = columnMask;
        }
        //customArray[3] = new Boolean(true);   //single interval selection

        vs2SourcePropertiesDialog dlg = new vs2SourcePropertiesDialog(customArray);
        mp2TableData strengths =
                ((mp2SourceData) shapesData).getSourceStrengthData(isrc);
        Object [] aRow, dataRow;
        for (int r=0; r<strengths.getNumberOfRows(); r++) {
            aRow = dlg.tableData.createDefaultRow();
            dataRow = strengths.getRow(r);
            for (int i=1; i<aRow.length; i++) {
                if (dataRow[i] instanceof Double) {
                    aRow[i] = new Double(((Double) dataRow[i]).doubleValue());
                } else if (dataRow[i] instanceof Integer) {
                    aRow[i] = new Integer(((Integer) dataRow[i]).intValue());
                }
            }
            dlg.tableData.addRow(Math.max(r-1, 0), aRow);
        }
        if (dlg.doModal()==true) {
            strengths.setData(dlg.tableData.getData());
        }
    }

}
