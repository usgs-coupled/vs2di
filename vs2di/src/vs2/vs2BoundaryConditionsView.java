/*
 * vs2BoundaryConditionsView.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.undo.*;

public class vs2BoundaryConditionsView extends mp2BoundaryConditionsView implements
            vs2Constants {

    protected vs2ModelOptions modelOptions;

    protected JLabel flowBCColorsLabel;
    protected JLabel radialFlowBCColorsLabel;
    protected JToolBar toolBar;
    protected vs2BoundaryConditionsData bcData; // needed to provide access to undo

    protected static final Color [] FLOW_BC_COLOR =
            {null,
             new Color(189, 222, 255),   // pressure head
             new Color(123, 255,  90),   // normal flux
             new Color(255, 189,  90),   // seepage face
             new Color( 90, 123, 255),   // total head
             new Color(255, 255,   0),   // evaporation
             new Color(255,  90,  90),   // volumetric flow (radial coord only)
             new Color(123, 255,  90),   // vertical flux
             new Color(255, 128, 255)};  // gravity drain

    protected static final String [] FLOW_BC_LABEL =
            {new String("q = 0"),
             new String("p = "),
             new String("qn = "),
             new String("seep"),
             new String("h = "),
             new String("ET"),
             new String("Q = "),
             new String("qv = "),
             new String("grv_drn")};


    /**
     * Constructor
     */
    public vs2BoundaryConditionsView(mp2View view,
                    vs2BoundaryConditionsData boundaryConditionsData,
                    mp2RectilinearGridData gridData,
                    vs2ModelOptions modelOptions,
                    mp2DomainData domainData,
                    String homeDirectory) {

        super(view, boundaryConditionsData, gridData, domainData, homeDirectory);
        this.modelOptions = modelOptions;
        this.bcData = boundaryConditionsData;

        flowBCColorsLabel = new JLabel(new ImageIcon(ClassLoader.getSystemResource("images/flowbcx.gif")),
                SwingConstants.CENTER);
        radialFlowBCColorsLabel = new JLabel(new ImageIcon(ClassLoader.getSystemResource("images/flowbcr.gif")),
                SwingConstants.CENTER);
    }

    /**
     * Invoked when the setBCButton is clicked
     */
    public void onSetBC() {
        int i, j, k;
        int selectedPeriod = boundaryConditionsData.getSelectedPeriod();
        vs2BoundaryConditionsDialog dlg =  new vs2BoundaryConditionsDialog(
                            selectedPeriod + 1, modelOptions);

        vs2BoundaryCondition bc0 = new vs2BoundaryCondition();
        boolean firstPass = true;
        Vector boundaries = boundaryConditionsData.getBCForSelectedPeriod();
        for (i=0; i<boundaries.size(); i++) {
            Vector segments = (Vector) boundaries.elementAt(i);
            for (j=0; j<segments.size(); j++) {
                if (selectedSegmentArray[i][j]) {
                    vs2BoundaryCondition bc =
                            (vs2BoundaryCondition) segments.elementAt(j);
                    if (firstPass) {
                        bc0.copy(bc);
                        firstPass = false;
                    } else {
                        if (bc0.flowType != bc.flowType) {
                            bc0.flowType = 0;
                        }
                        if (bc0.flowValue != bc.flowValue) {
                            bc0.flowValue = Double.MIN_VALUE;
                        }
                        if (bc0.getEnergyTransportType() != bc.getEnergyTransportType()) {
                            bc0.setEnergyTransportType(0);
                        }
                        if (bc0.getEnergyTransportValue() != bc.getEnergyTransportValue()) {
                            bc0.setEnergyTransportValue(Double.MIN_VALUE);
                        }
                        if (bc0.getSoluteTransportType() != bc.getSoluteTransportType()) {
                            bc0.setSoluteTransportType(0);
                        }
                        if (bc0.getSoluteTransportValue() != bc.getSoluteTransportValue()) {
                            bc0.setSoluteTransportValue(Integer.MIN_VALUE);
                        }
                    }
                }
            }
        }

        dlg.flowBCType = bc0.flowType;
        dlg.flowBCValue = bc0.flowValue;
        if (modelOptions.doEnergyTransport) {
            dlg.energyTransportBCType = bc0.getEnergyTransportType();
            dlg.energyTransportBCValue = bc0.getEnergyTransportValue();
        }
        if (modelOptions.doSoluteTransport) {
            dlg.soluteTransportBCType = bc0.getSoluteTransportType();
            dlg.soluteTransportBCValue = bc0.getSoluteTransportValue();
            assert(dlg.soluteTransportBCType != DIFFUSIVE_FLUX_BC);
        }

        if (dlg.doModal() == true) {
            assert(dlg.soluteTransportBCType != DIFFUSIVE_FLUX_BC);
            if (dlg.getFromFile) {

                mp2FileChooser fc = new mp2FileChooser();
                fc.setDialogTitle("Load boundary conditions from file");
                fc.setCurrentDirectory(new File(view.getFrame().getApp().getCurrentDirectory()));
                if (fc.showOpenDialog(view.getFrame()) != mp2FileChooser.APPROVE_OPTION) {
                    return;
                }

                String fileName = fc.getSelectedFile().getPath();

                BufferedReader in;
                try {
                    in  = new BufferedReader (new FileReader(fileName));
                } catch (java.io.FileNotFoundException e1) {
                    mp2MessageBox.showMessageDialog("Unable to open file: " + fileName, "Error");
                    return;
                }
               	double [] bcValue = new double[3];
                String line;
                Vector oldBC = boundaryConditionsData.getClonedBC();
                Vector newBC = boundaryConditionsData.getClonedBC();
                try {
                    for (k=selectedPeriod; k<newBC.size(); k++) {
                        Vector bd = (Vector) newBC.elementAt(k);
                        line = in.readLine();
                        if (line==null) {
                            break;
                        }
                        parseLine(line, bcValue);
                        for (i=0; i<bd.size(); i++) {
                            Vector segments = (Vector) bd.elementAt(i);
                            for (j=0; j<segments.size(); j++) {
                                if (selectedSegmentArray[i][j]) {
                                    vs2BoundaryCondition bc =
                                            (vs2BoundaryCondition) segments.elementAt(j);
                                    bc.flowType = dlg.flowBCType;

                                    int nextIndex = 0;
                                    bc.flowValue = 0;
                                    if (requiresFlowData(dlg.flowBCType)) {
                                        bc.flowValue = bcValue[nextIndex];
                                        ++nextIndex;
                                    }
                                    if (dlg.flowBCValue == -1) bc.flowValue = -bc.flowValue;
                                    if (modelOptions.doEnergyTransport) {
                                        bc.setEnergyTransportType(dlg.energyTransportBCType);
                                        if (requiresEnergyData(dlg.flowBCType, dlg.flowBCValue, dlg.energyTransportBCType)) {
                                            bc.setEnergyTransportValue(bcValue[nextIndex]);                                            
                                            ++nextIndex;
                                        }
                                    }
                                    if (modelOptions.doSoluteTransport) {
                                        bc.setSoluteTransportType(dlg.soluteTransportBCType);
                                        if (requiresSoluteData(dlg.flowBCType, dlg.flowBCValue, dlg.soluteTransportBCType)) {
                                            bc.setSoluteTransportValue((int)Math.round(bcValue[nextIndex]));                                            
                                            ++nextIndex;
                                        }
                                    }
                                }
                            }
                        }
                    }
                } catch (java.io.IOException e) {
                    mp2MessageBox.showMessageDialog("Read error.", "Error");
                    return;
                }
                boundaryConditionsData.setBC(newBC);
                boundaryConditionsData.setDataHaveChanged();
                newBC = boundaryConditionsData.getClonedBC();
                undoSupport.postEdit(new vs2BCEdit(oldBC, newBC, undoCount));
                undoCount++;
                view.getFrame().getApp().setCurrentDirectory(fc.getCurrentDirectory().getPath());
            } else {

                Vector oldBC = boundaryConditionsData.getClonedBC();
                for (i=0; i<boundaries.size(); i++) {
                    Vector segments = (Vector) boundaries.elementAt(i);
                    for (j=0; j<segments.size(); j++) {
                        if (selectedSegmentArray[i][j]) {
                            vs2BoundaryCondition bc =
                                    (vs2BoundaryCondition) segments.elementAt(j);
                            bc.flowType = dlg.flowBCType;
                            bc.flowValue = dlg.flowBCValue;
                            if (modelOptions.doEnergyTransport) {
                                bc.setEnergyTransportType(dlg.energyTransportBCType);
                                bc.setEnergyTransportValue(dlg.energyTransportBCValue);
                            }
                            if (modelOptions.doSoluteTransport) {
                                bc.setSoluteTransportType(dlg.soluteTransportBCType);
                                bc.setSoluteTransportValue(dlg.soluteTransportBCValue);
                            }
                        }
                    }
                }

                if (dlg.applyToThisPeriodOnly == false) {
                    boundaryConditionsData.extendSelectedPeriodBCToFuture(
                                                                selectedSegmentArray);
                }
                boundaryConditionsData.setDataHaveChanged();
                Vector newBC = boundaryConditionsData.getClonedBC();
                undoSupport.postEdit(new vs2BCEdit(oldBC, newBC, undoCount));
                undoCount++;
            }

            for (i=0; i<selectedSegmentArray.length; i++) {
                for (j=0; j<selectedSegmentArray[i].length; j++) {
                    selectedSegmentArray[i][j] = false;
                }
            }
            setBCButton.setEnabled(false);
        }
        view.repaint();
    }

    private boolean parseLine(String line, double [] bcValue) {
        for (int i = 0; i < bcValue.length; ++i) {
            bcValue[i] = 0;
        }
        line = line.replaceAll("\\s\\s+", " ");
        String[] items = line.split(" ");
        for (int i = 0; i < items.length; ++i) {
            try {
                bcValue[i] = Double.valueOf(items[i]).doubleValue();
            } catch (NumberFormatException e) {
                return false;
            }
        }
        return true;
    }

    /**
     * Gets the BC color for the specified segment
     */
    protected Color getBCColor(int boundaryIndex, int segmentIndex) {
        Vector boundaries = boundaryConditionsData.getBCForSelectedPeriod();
        Vector segments = (Vector) boundaries.elementAt(boundaryIndex);
        vs2BoundaryCondition bc = (vs2BoundaryCondition) segments.elementAt(segmentIndex);
        return FLOW_BC_COLOR[bc.flowType];
    }

    /**
     * Gets the BC value for the selected period at the specified boundary
     * and specified segment
     */
    public String getLabelForSelectedPeriodBC(int boundaryIndex, int segmentIndex) {
        Vector boundaries = boundaryConditionsData.getBCForSelectedPeriod();
        Vector segments = (Vector) boundaries.elementAt(boundaryIndex);
        vs2BoundaryCondition bc = (vs2BoundaryCondition) segments.elementAt(segmentIndex);

        String label = FLOW_BC_LABEL[bc.flowType];

        if (bc.flowType == PRESSURE_HEAD_BC
                    || bc.flowType == NORMAL_FLUID_FLUX_BC
                    || bc.flowType == TOTAL_HEAD_BC
                    || bc.flowType == VOLUMETRIC_FLOW_BC
                    || bc.flowType == VERTICAL_FLUID_FLUX_BC) {
            String v = String.valueOf(bc.flowValue);
            if (v.endsWith(".0")) {
                v = v.substring(0, v.length()-2);
            }
            label += v;

            if ((modelOptions.doEnergyTransport || modelOptions.doSoluteTransport)
                        && !(bc.flowType == NORMAL_FLUID_FLUX_BC && bc.flowValue < 0)
                        && !(bc.flowType == VERTICAL_FLUID_FLUX_BC && bc.flowValue < 0)
                        && !(bc.flowType == VOLUMETRIC_FLOW_BC && bc.flowValue < 0)) {
                v = String.valueOf(bc.getEnergyTransportValue());
                if (v.endsWith(".0")) {
                    v = v.substring(0, v.length()-2);
                }
                if (modelOptions.doEnergyTransport) {
                    if (bc.getEnergyTransportType() == DEFAULT_CONC_BC) {
                        label += ", Ti = " + v;
                    }
                    else if (bc.getEnergyTransportType() == SPECIFIED_CONC_BC) {
                        label += ", Tb = " + v;
                    }
                }
                v = String.valueOf(bc.getSoluteTransportValue());
                if (modelOptions.doSoluteTransport) {
                    if (bc.getSoluteTransportType() == DEFAULT_CONC_BC) {
                        label += ", Si = " + v;
                    }
                    else if (bc.getSoluteTransportType() == SPECIFIED_CONC_BC) {
                        label += ", Sb = " + v;
                    }
                }
            }
        } else if (modelOptions.doEnergyTransport
                   && (bc.flowType == SEEPAGE_FACE_BC || bc.flowType == GRAVITY_DRAIN_BC || bc.flowType == EVAPORATION_BC)
                   && bc.getEnergyTransportType() == SPECIFIED_CONC_BC) {
            String v = String.valueOf(bc.getEnergyTransportValue());
            if (v.endsWith(".0")) {
                v = v.substring(0, v.length()-2);
            }
            label += ", T = " +v;

        } else if (bc.flowType == NO_FLOW_BC) {
            if (modelOptions.doEnergyTransport) {
                String v = String.valueOf(bc.getEnergyTransportValue());
                if (v.endsWith(".0")) {
                    v = v.substring(0, v.length()-2);
                }
                if (bc.getEnergyTransportType() == DEFAULT_CONC_BC) {
                    label += ", J = 0";
                }
                if (bc.getEnergyTransportType() == SPECIFIED_CONC_BC) {
                    label += ", Tb = " + v;
                }
                else if (bc.getEnergyTransportType() == DIFFUSIVE_FLUX_BC) {
                    label += ", Jc = " + v;
                }
            }
            if (modelOptions.doSoluteTransport) {
                String v = String.valueOf(bc.getSoluteTransportValue());
                if (bc.getSoluteTransportType() == DEFAULT_CONC_BC) {
                    label += ", J = 0";
                }
                if (bc.getSoluteTransportType() == SPECIFIED_CONC_BC) {
                    label += ", Sb = " + v;
                }
                assert(bc.getSoluteTransportType() != DIFFUSIVE_FLUX_BC);  // invalid for 1.4
            }
        }

        return label;
    }

    /**
     * Loads the buttons to the tool bar in the frame window.
     */
    public void prepareToActivate() {
        super.prepareToActivate();
        toolBar = view.getFrame().getToolBar();
        toolBar.add(Box.createVerticalStrut(10));
        if (modelOptions.useRadialCoord) {
            toolBar.add(radialFlowBCColorsLabel);
        } else {
            toolBar.add(flowBCColorsLabel);
        }
        view.setCursor(Cursor.getDefaultCursor());
    }
    
    static public boolean requiresFlowData(int flowBCType) {
        // test flowtype
        if (flowBCType == NO_FLOW_BC || flowBCType == SEEPAGE_FACE_BC
                || flowBCType == GRAVITY_DRAIN_BC || flowBCType == EVAPORATION_BC) {
            return false;
        }
        return true;
    }
    
    static public boolean requiresEnergyData(int flowBCType, double flowBCValue, int energyTransportBCType) {
        if (flowBCType == NO_FLOW_BC) {
            return (energyTransportBCType == SPECIFIED_CONC_BC || energyTransportBCType == DIFFUSIVE_FLUX_BC);
        }
        if (flowBCType == PRESSURE_HEAD_BC || flowBCType == TOTAL_HEAD_BC) {
            return true;
        }
        if (flowBCType == NORMAL_FLUID_FLUX_BC || flowBCType == VOLUMETRIC_FLOW_BC) {
            return (flowBCValue > 0);
        }
        if (flowBCType == EVAPORATION_BC || flowBCType == SEEPAGE_FACE_BC || flowBCType == GRAVITY_DRAIN_BC) {
            return (energyTransportBCType == SPECIFIED_CONC_BC);
        }
        return false;
    }
    
    
    static public boolean requiresSoluteData(int flowBCType, double flowBCValue, int soluteTransportBCType) {
        if (flowBCType == NO_FLOW_BC) {
            // DIFFUSIVE_FLUX_BC for soluteTransport not supported
            assert(soluteTransportBCType != DIFFUSIVE_FLUX_BC);
            return (soluteTransportBCType == SPECIFIED_CONC_BC);
        }
        if (flowBCType == PRESSURE_HEAD_BC || flowBCType == TOTAL_HEAD_BC || flowBCType == VERTICAL_FLUID_FLUX_BC) {
            return true;
        }
        if (flowBCType == NORMAL_FLUID_FLUX_BC || flowBCType == VOLUMETRIC_FLOW_BC) {
            return (flowBCValue > 0);
        }
        if (flowBCType == EVAPORATION_BC || flowBCType == SEEPAGE_FACE_BC || flowBCType == GRAVITY_DRAIN_BC) {
            return false;
        }
        return false;
    }

    /**
     * The undoable edit to edit boundary conditions
     */
    public class vs2BCEdit extends AbstractDataEdit {

        protected Vector oldBC;
        protected Vector newBC;

        public vs2BCEdit(Vector oldBC, Vector newBC,
                                            long undoIndex) {
            super(undoIndex);
            this.oldBC = oldBC;
            this.newBC = newBC;
        }

        public void undo() throws CannotUndoException {
            bcData.setBC(oldBC);
            super.undo();
        }

        public void redo() throws CannotRedoException {
            bcData.setBC(newBC);
            super.redo();
        }
    }

}