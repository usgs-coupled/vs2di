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

    protected static String Ci = "Ci";
    protected static String Cb = "Cb";

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

        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        flowBCColorsLabel = new JLabel(new ImageIcon(imageDirectory + "flowbcx.gif"),
                SwingConstants.CENTER);
        radialFlowBCColorsLabel = new JLabel(new ImageIcon(imageDirectory + "flowbcr.gif"),
                SwingConstants.CENTER);

        if (vs2App.doHeat()) {
            Ci = "Ti";
            Cb = "Tb";
        }

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
                        if (bc0.getTransportType() != bc.getTransportType()) {
                            bc0.setTransportType(0);
                        }
                        if (bc0.getTransportValue() != bc.getTransportValue()) {
                            bc0.setTransportValue(Double.MIN_VALUE);
                        }
                    }
                }
            }
        }

        dlg.flowBCType = bc0.flowType;
        dlg.flowBCValue = bc0.flowValue;
        if (modelOptions.doTransport) {
            dlg.transportBCType = bc0.getTransportType();
            dlg.transportBCValue = bc0.getTransportValue();
        }

        if (dlg.doModal() == true) {

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
               	double [] bcValue = new double[2];
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
                                    if (dlg.flowBCType == NO_FLOW_BC || dlg.flowBCType == SEEPAGE_FACE_BC
                                            || dlg.flowBCType == GRAVITY_DRAIN_BC || dlg.flowBCType == EVAPORATION_BC) {
                                        bc.flowValue = 0;
                                    } else {
                                        bc.flowValue = bcValue[0];
                                    }
                                    if (dlg.flowBCValue == -1) bc.flowValue = -bc.flowValue;
                                    if (modelOptions.doTransport) {
                                        bc.setTransportType(dlg.transportBCType);
                                        if (dlg.flowBCType == NO_FLOW_BC || dlg.flowBCType == SEEPAGE_FACE_BC
                                            || dlg.flowBCType == GRAVITY_DRAIN_BC || dlg.flowBCType == EVAPORATION_BC) {
                                            bc.setTransportValue(bcValue[0]);
                                        } else {
                                            bc.setTransportValue(bcValue[1]);
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
                            if (modelOptions.doTransport) {
                                bc.setTransportType(dlg.transportBCType);
                                bc.setTransportValue(dlg.transportBCValue);
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
        bcValue[0] = 0;
        bcValue[1] = 0;
        String a = line.trim();
        if (a.length() == 0) return true;
        int p = a.indexOf(' ');
        try {
            if (p == -1) {
                bcValue[0] = Double.valueOf(a).doubleValue();
                return true;
            } else {
                bcValue[0] = Double.valueOf(a.substring(0, p)).doubleValue();
            }
        } catch (NumberFormatException e) {
            return false;
        }
        String b = a.substring(p).trim();
        if (b.length() == 0) return true;
        try {
            bcValue[1] = Double.valueOf(b).doubleValue();
        } catch (NumberFormatException e) {
            return false;
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

            if (modelOptions.doTransport
                        && !(bc.flowType == NORMAL_FLUID_FLUX_BC && bc.flowValue < 0)
                        && !(bc.flowType == VERTICAL_FLUID_FLUX_BC && bc.flowValue < 0)
                        && !(bc.flowType == VOLUMETRIC_FLOW_BC && bc.flowValue < 0)) {
                v = String.valueOf(bc.getTransportValue());
                if (v.endsWith(".0")) {
                    v = v.substring(0, v.length()-2);
                }
                if (bc.getTransportType() == DEFAULT_CONC_BC) {
                    label += ", " + Ci + " = " + v;
                }
                else if (bc.getTransportType() == SPECIFIED_CONC_BC) {
                    label += ", " + Cb + " = " + v;
                }
            }
        } else if (modelOptions.doTransport && vs2App.doHeat()
                   && (bc.flowType == SEEPAGE_FACE_BC || bc.flowType == GRAVITY_DRAIN_BC || bc.flowType == EVAPORATION_BC)
                   && bc.getTransportType() == SPECIFIED_CONC_BC) {
            String v = String.valueOf(bc.getTransportValue());
            if (v.endsWith(".0")) {
                v = v.substring(0, v.length()-2);
            }
            label += ", T = " +v;

        } else if (bc.flowType == NO_FLOW_BC && modelOptions.doTransport) {
            String v = String.valueOf(bc.getTransportValue());
            if (v.endsWith(".0")) {
                v = v.substring(0, v.length()-2);
            }
            if (bc.getTransportType() == DEFAULT_CONC_BC) {
                label += ", J = 0";
            }
            if (bc.getTransportType() == SPECIFIED_CONC_BC) {
                label += ", " + Cb + " = " + v;
            }
            else if (bc.getTransportType() == DIFFUSIVE_FLUX_BC) {
                if (vs2App.doHeat()) {
                    label += ", Jc = " + v;
                } else {
                    label += ", Jd = " + v;
                }
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