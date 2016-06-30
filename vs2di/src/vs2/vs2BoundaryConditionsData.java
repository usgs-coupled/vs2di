/*
 * vs2BoundaryConditionsData.java
 */
package vs2;

import mp2.*;
import java.io.*;
import java.util.*;

public class vs2BoundaryConditionsData extends mp2BoundaryConditionsData
            implements Serializable, vs2Constants {

    static final long serialVersionUID = 6076112268053583282L;

    protected transient Vector boundaryCellLists;


    /**
     * Creates a boundary condition
     */
    public mp2BoundaryCondition createBoundaryCondition() {
        return new vs2BoundaryCondition();
    }

    /**
     * Discretize the boundary conditions
     */
    public boolean discretize() {
        // Don't do this if neither the grid nor the data
        // have changed since the last discretization.
        if ((!activeCellsHaveChanged) && (!dataHaveChanged)) {
            return false;
        }
        super.discretize();
        // Create the boundary cell lists.
        boundaryCellLists = new Vector();
        for (int p=0; p<periods.size(); p++) {
            // Find the number of seepage face boundary segments in the
            // period
            int numberOfSeepageFaces = 0;
            Vector boundaries = (Vector) periods.elementAt(p);
            for (int j=0; j<boundaries.size(); j++) {
                Vector segments = (Vector) boundaries.elementAt(j);
                for (int k=0; k<segments.size(); k++) {
                    vs2BoundaryCondition bc =
                            (vs2BoundaryCondition) segments.elementAt(k);
                    if (bc.flowType == SEEPAGE_FACE_BC) {
                        numberOfSeepageFaces++;
                    }
                }
            }
            if (numberOfSeepageFaces == 0) {
                // If no seepage face, then use the boundary cell master list
                boundaryCellLists.addElement(boundaryCellMasterList);
            } else {
                // If there are seepage faces, find the number of "unwanted" cells
                int [] unwantedCells = getUnwantedCellsForPeriod(p);
                if (unwantedCells.length == 0) {
                    // If no unwanted cells, use boundary cell master list
                    boundaryCellLists.addElement(boundaryCellMasterList);
                } else {
                    // If there are excluded cells, make a new list without
                    // unwanted seepage face cells.
                    boundaryCellLists.addElement(
                        makeListLessUnwantedCells(p, unwantedCells));
                }
            }
        }
        activeCellsHaveChanged = false;
        dataHaveChanged = false;
        return true;
    }

    /**
     * Gets the boundary list for the specified period
     */
    public Vector getBoundaryCellListForPeriod(int p) {
        discretize();
        if (boundaryCellLists == null || p >= boundaryCellLists.size()) {
            return null;
        }
        return (Vector) boundaryCellLists.elementAt(p);
    }

    /**
     * Get the excluded cells for the specified period
     */
    private int [] getUnwantedCellsForPeriod(int period) {
        // Make a list of all the seepage face cells in the period.
        Vector sfcells = new Vector();
        Vector boundaries = (Vector) periods.elementAt(period);
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        int numCol = rectGridData.getXCoordCount() - 1;
        int numRow = rectGridData.getYCoordCount() - 1;
        for (int j=0; j<boundaries.size(); j++) {
            Vector segments = (Vector) boundaries.elementAt(j);
            Vector cellsOfSegment = (Vector) boundaryCellMasterList.elementAt(j);
            for (int k=0; k<segments.size(); k++) {
                vs2BoundaryCondition bc =
                        (vs2BoundaryCondition) segments.elementAt(k);
                int [] cell = (int []) cellsOfSegment.elementAt(k);
                if (bc.flowType == SEEPAGE_FACE_BC) {
                    for (int m=cell.length-1; m>=0; m--) {   // list from lowest to highest
                        int [] p = new int[3];
                        p[0] = cell[m];                // index
                        p[2] = cell[m]/numCol;         // row
                        p[1] = cell[m] - p[2]*numCol;   // col
                        sfcells.addElement(p);
                    }
                }
            }
        }

        // Set up the neighbor relations
        int [] neighbor = new int [4];
        neighbor[0] = - rectGridData.getXCoords().length + 1;
        neighbor[1] = -1;
        neighbor[2] = 1;
        neighbor[3] = - neighbor[0];

        // Search for "unwanted" cells. There occur where two seepage face
        // boundary segments meet, forming a locally convex region.
        Vector unwantedCells = new Vector();
        for (int i=0; i<sfcells.size(); i++) {
            int caseNumber = 0;
            int [] ip = (int []) sfcells.elementAt(i);
            for (int j=0; j<sfcells.size(); j++) {
                int [] jp = (int []) sfcells.elementAt(j);
                if (i!=j) {
                    if (ip[1]==jp[1] && ip[2]-1==jp[2]) {
                        caseNumber |= 1;
                    }
                    else if (ip[1]-1==jp[1] && ip[2]==jp[2]) {
                        caseNumber |= 2;
                    }
                    else if (ip[1]+1==jp[1] && ip[2]==jp[2]) {
                        caseNumber |= 4;
                    }
                    else if (ip[1]==jp[1] && ip[2]+1==jp[2]) {
                        caseNumber |= 8;
                    }
                }
            }
            if (caseNumber == 3 || caseNumber == 5 ||
                caseNumber == 7 || caseNumber > 9) {
                unwantedCells.addElement(new Integer(ip[0]));
                ip[1] = -100; // set sufficiently large neg value
                ip[2] = -100; // to remove cell from further consideration
            }
        }

        int [] result = new int[unwantedCells.size()];
        for (int i=0; i<result.length; i++) {
            result[i] = ((Integer) unwantedCells.elementAt(i)).intValue();
        }
        return result;
    }

    /**
     * Make list of boundary cells with "unwanted" cells removed for the
     * specified period
     */
    private Vector makeListLessUnwantedCells(int p, int [] unwantedCells) {
        Vector boundaries = (Vector) periods.elementAt(p);
        Vector newList = new Vector();
        for (int j=0; j<boundaries.size(); j++) {
            Vector segments = (Vector) boundaries.elementAt(j);
            Vector cellsOfSegment = (Vector) boundaryCellMasterList.elementAt(j);
            Vector newSegments = new Vector();
            for (int k=0; k<segments.size(); k++) {
                vs2BoundaryCondition bc =
                        (vs2BoundaryCondition) segments.elementAt(k);
                int [] cell = (int []) cellsOfSegment.elementAt(k);
                if (bc.flowType != SEEPAGE_FACE_BC) {
                    newSegments.addElement(cell);
                } else {
                    Vector newCells = new Vector();
                    for (int i=0; i<cell.length; i++) {
                        boolean exclude = false;
                        for (int m=0; m<unwantedCells.length && !exclude; m++) {
                            if (cell[i] == unwantedCells[m]) {
                                exclude = true;
                            }
                        }
                        if (!exclude) {
                            newCells.addElement(new Integer(cell[i]));
                        }
                    }
                    int [] newCell = new int[newCells.size()];
                    for (int i=0; i<newCell.length; i++) {
                        newCell[i] = ((Integer) newCells.elementAt(i)).intValue();
                    }
                    newSegments.addElement(newCell);
                }
            }
            newList.addElement(newSegments);
        }
        return newList;
    }

    /**
     * Initializes this object.
     */
    public void init(mp2Doc doc) {
        super.init(doc);

        // If there are bc data, then this object was
        // created via serialization, in which case
        // we need to discretize.
        if (periods.size() > 0) {
            dataHaveChanged = true;
        }
    }

    /**
     * Exports boundary data for the specified period
     */
    public void exportPeriod(PrintWriter pw, int p, vs2ModelOptions modelOptions) {
        if (p >= periods.size()) {
            return;
        }
        discretize();
        int j, k ,m, row, col;
        mp2RectilinearGridData rectGridData = (mp2RectilinearGridData) gridData;
        double [] xCoord = rectGridData.getXCoords();
        double [] yCoord = rectGridData.getYCoords();

        int numCol = xCoord.length - 1;
        int numRow = yCoord.length - 1;

        double [] xSpacing = new double[numCol];
        double [] ySpacing = new double[numRow];
        for (j=0; j<numCol; j++) {
            xSpacing[j] = xCoord[j+1] - xCoord[j];
        }
        for (j=0; j<numRow; j++) {
            ySpacing[j] = yCoord[j+1] - yCoord[j];
        }

        Vector boundaryCellList = (Vector) boundaryCellLists.elementAt(p);
        Vector boundaries = (Vector) periods.elementAt(p);

        // Find the number of seepage face segments for the period,
        // using list with unwanted cells eliminated
        int numberOfSeepageFaces = 0;
        for (j=0; j<boundaries.size(); j++) {
            Vector segments = (Vector) boundaries.elementAt(j);
            Vector cellsOfSegment = (Vector) boundaryCellList.elementAt(j);
            for (k=0; k<segments.size(); k++) {
                vs2BoundaryCondition bc =
                        (vs2BoundaryCondition) segments.elementAt(k);
                int [] cell = (int []) cellsOfSegment.elementAt(k);
                if (bc.flowType == SEEPAGE_FACE_BC && cell.length > 0) {
                    numberOfSeepageFaces++;
                }
            }
        }

        // rest of card C-6 (BCIT and ETSIM printed in rechargePeriodData.exportPeriod)
        pw.println((numberOfSeepageFaces > 0 ? "T" : "F")
                + "     /C6 -- BCIT, ETSIM, SEEP");
        if (numberOfSeepageFaces > 0) {
            // Card C-7
            pw.println(numberOfSeepageFaces + "     /C7 -- NFCS");
            for (j=0; j<boundaries.size(); j++) {
                Vector segments = (Vector) boundaries.elementAt(j);
                Vector cellsOfSegment = (Vector) boundaryCellList.elementAt(j);
                for (k=0; k<segments.size(); k++) {
                    vs2BoundaryCondition bc =
                                (vs2BoundaryCondition) segments.elementAt(k);
                    int [] cell = (int []) cellsOfSegment.elementAt(k);
                    if (bc.flowType == SEEPAGE_FACE_BC && cell.length > 0) {
                        // Card C-8  JLAST is hard wired to 0
                        pw.println(cell.length + " 0" +
                            "     /C8 -- JJ, JLAST. C-9 begins next line: J, N");
                        for (m=cell.length-1; m>=0; m--) {   // list from lowest to highest
                            row = cell[m]/numCol;
                            col = cell[m] - row*numCol;
                            // Card C-9
                            pw.println((row+2) + " " + (col+2));
                        }
                    }
                }
            }
        }

        // Card C-10 (BC always read by individual node)
        pw.println("0" + "     /C10 -- IBC");

        // Card C-11
        boolean doExport;
        Vector lastPeriod = null;
        if (p > 0) {
            lastPeriod = (Vector) periods.elementAt(p-1);
        }
        for (j = 0; j<boundaries.size(); j++) {
            Vector segments = (Vector) boundaries.elementAt(j);
            Vector cellsOfSegment = (Vector) boundaryCellList.elementAt(j);

            for (k=0; k<segments.size(); k++) {
                vs2BoundaryCondition bc =
                        (vs2BoundaryCondition) segments.elementAt(k);
                int [] cell = (int []) cellsOfSegment.elementAt(k);

//                if (bc.flowType != SEEPAGE_FACE_BC) { // skip seepage faces
//                Starting in version 1.2, the seepage face cells are written out. This
//                allows for specifying temperature for seepage face.
                    if (p == 0) {
                        // if first period, print non default bc
                        doExport = !(
                                (bc.flowType == NO_FLOW_BC) &&
                                (modelOptions.doEnergyTransport ? (bc.getEnergyTransportType() == DEFAULT_CONC_BC) : true ) &&
                                (modelOptions.doSoluteTransport ? (bc.getSoluteTransportType() == DEFAULT_CONC_BC) : true )
                                );
                        
                    } else {
                        // if not first period, print bcs that have changed since last period
                        Vector lastPeriodSegments =
                            (Vector) lastPeriod.elementAt(j);
                        vs2BoundaryCondition lastPeriodBc =
                            (vs2BoundaryCondition) lastPeriodSegments.elementAt(k);
                        doExport = !(
                                (bc.flowType  == lastPeriodBc.flowType)  &&
                                (bc.flowValue == lastPeriodBc.flowValue) &&
                                (modelOptions.doEnergyTransport ? (bc.getEnergyTransportType()  == lastPeriodBc.getEnergyTransportType() ) : true) &&
                                (modelOptions.doEnergyTransport ? (bc.getEnergyTransportValue() == lastPeriodBc.getEnergyTransportValue()) : true) &&
                                (modelOptions.doSoluteTransport ? (bc.getSoluteTransportType()  == lastPeriodBc.getSoluteTransportType() ) : true) &&
                                (modelOptions.doSoluteTransport ? (bc.getSoluteTransportValue() == lastPeriodBc.getSoluteTransportValue()) : true)
                                );
                    }
                    if (doExport) {
                        double adjustedFlowValue = bc.flowValue;
                        //double adjustedTransportValue = bc.getTransportValue();
                        double adjustedEnergyTransportValue = bc.getEnergyTransportValue();
                        ///int adjustedSoluteTransportValue = bc.getSoluteTransportValue();
                        // adjust for differences between VS2DTI and VS2DT for normal flux
                        // and vertical flux
                        if (bc.flowType == VERTICAL_FLUID_FLUX_BC || bc.flowType == NORMAL_FLUID_FLUX_BC
                               || bc.getEnergyTransportType() == DIFFUSIVE_FLUX_BC) {
                            mp2Polygon poly = domainData.getBoundary(j);
                            double [] v1 = poly.getVertex(k);
                            double [] v2 = poly.getVertex((k+1)%poly.getNumberOfVertices());
                            double segDist = 0;
                            if (bc.flowType == VERTICAL_FLUID_FLUX_BC) {
                               segDist = Math.abs(v2[0]-v1[0]);
                            } else {
                               segDist = mp2Math.distanceBetweenTwoPoints(v1, v2);
                            }
                            // Adjustment for flux BC for r-z (radial) case:
                            // Compute the total rate across the lateral surface of a
                            // frustrum, which is the surface of revolution swept
                            // out by revolving the boundary segment about z = 0.
                            // Compute the total rate across this surface,
                            // which is flux times surface area. Next find the number
                            // of cells representing this boundary. For each cell, find
                            // the ring of horizontal area represented by this cell, and
                            // sum the areas of all rings. The total rate divided by
                            // the sum of ring areas is the "flux" assigned to cells in VS2DT.
                            if (modelOptions.useRadialCoord) {
                                double lateralSurfaceOfFrustrum = Math.PI * segDist * (v1[0] + v2[0]);
                                double totalFlow = lateralSurfaceOfFrustrum * bc.flowValue;
                                double totalEnergyMassRate = lateralSurfaceOfFrustrum * bc.getEnergyTransportValue();
                                double sumOfCellRings = 0;
                                for (m=0; m<cell.length; m++) {
                                    row = cell[m]/numCol;
                                    col = cell[m] - row*numCol;
                                    sumOfCellRings += Math.PI * (xCoord[col+1] * xCoord[col+1]
                                                                 - xCoord[col] * xCoord[col]);
                                }
                                if (bc.flowType == NORMAL_FLUID_FLUX_BC && sumOfCellRings > 0) {
                                    adjustedFlowValue = totalFlow/sumOfCellRings;
                                }
                                if (bc.getEnergyTransportType() == DIFFUSIVE_FLUX_BC && sumOfCellRings > 0) {
                                    adjustedEnergyTransportValue = totalEnergyMassRate/sumOfCellRings;
                                }
                            }
                            // Adjustment for flux BC for x-z case:
                            // Compute the total rate  across a boundary segment
                            // (that is, the flux times the length of the boundary
                            // segment times a unit thickness normal to the domain).
                            // Next, find the number of cells representing this boundary,
                            // and sum their x spacings. The flux assigned to
                            // VS2DT is the total rate across the boundary divided
                            // by the sum of the cell x-spacings.
                            else {
                                double totalFlow = segDist*bc.flowValue;
                                double totalEnergyMassRate = segDist*bc.getEnergyTransportValue();
                                double cellDist = 0;
                                for (m=0; m<cell.length; m++) {
                                    row = cell[m]/numCol;
                                    col = cell[m] - row*numCol;
                                    cellDist += xSpacing[col];
                                }
                                if ((bc.flowType == NORMAL_FLUID_FLUX_BC || bc.flowType == VERTICAL_FLUID_FLUX_BC)
                                       && cellDist > 0) {
                                    adjustedFlowValue = totalFlow/cellDist;
                                }
                                if (bc.getEnergyTransportType() == DIFFUSIVE_FLUX_BC && cellDist > 0) {
                                    adjustedEnergyTransportValue = totalEnergyMassRate/cellDist;
                                }
                            }
                        }
                        if (bc.flowType == TOTAL_HEAD_BC) {
                            // The total head in VS2DT is measured from the
                            // top of the grid (the top of the second row if
                            // boundary cells are counted).
                            adjustedFlowValue = bc.flowValue + yCoord[0];
                        }
                        // If bc is not volumetric flow rate, export the data
                        if (bc.flowType != VOLUMETRIC_FLOW_BC) {
                            int bcFlowType = bc.flowType;
                            // Use the same code for normal and vertical flux.
                            if (bcFlowType == VERTICAL_FLUID_FLUX_BC) {
                                bcFlowType = NORMAL_FLUID_FLUX_BC;
                            }
                            if (bcFlowType == GRAVITY_DRAIN_BC) {
                                bcFlowType = 7;
                            }
                            for (m=0; m<cell.length; m++) {
                                row = cell[m]/numCol;
                                col = cell[m] - row*numCol;
                                pw.println((row+2) + " " + (col+2) + " " +
                                        +  bcFlowType + " " + (float) adjustedFlowValue +
                                        "     /C11 -- JJ, NN, NTX, PFDUM");
                                if (modelOptions.doEnergyTransport) {
                                    pw.println(bc.getEnergyTransportType() + " " +
                                            (float) adjustedEnergyTransportValue +
                                            "     /C12 -- NTT, TF");
                                }
                                if (modelOptions.doSoluteTransport) {
                                    pw.println(bc.getSoluteTransportType() + " " +
                                            bc.getSoluteTransportValue() +
                                            " -1 1.   /C13 -- NTC, INSBC1, INSBC2, SBFRAC");
                                }                                
                            }
                        }
                        else {
                            // Adjustment for total volumetric flow BC for r-z (radial) case:
                            // Find the number of cells representing this boundary.
                            // For each cell, find the ring of horizontal area represented by
                            // this cell, and sum the areas of all rings. The specified volumetric
                            // flow divided by the sum of ring areas gives a flux. For each cell,
                            // multiply the flux by its ring area. This gives the volumetric flow
                            // rate for the cell. Note that in order to have a uniform flux,
                            // each cell will have a different volumetric flow rate. Cells of
                            // greater radial coordinate will have larger volumetric flow rate
                            // because the flow is spreaded over a larger ring area.
                            double sumOfCellRings = 0;
                            for (m=0; m<cell.length; m++) {
                                row = cell[m]/numCol;
                                col = cell[m] - row*numCol;
                                sumOfCellRings += Math.PI * (xCoord[col+1] * xCoord[col+1]
                                                             - xCoord[col] * xCoord[col]);
                            }
                            double fluidFlux = 0;
                            if (sumOfCellRings > 0) {
                                fluidFlux = bc.flowValue/sumOfCellRings;
                            }

                            for (m=0; m<cell.length; m++) {
                                row = cell[m]/numCol;
                                col = cell[m] - row*numCol;
                                adjustedFlowValue = fluidFlux * Math.PI *
                                        (xCoord[col+1] * xCoord[col+1] - xCoord[col] * xCoord[col]);
                                pw.println((row+2) + " " + (col+2) + " " +
                                        +  bc.flowType + " " + (float) adjustedFlowValue +
                                        "     /C11 -- JJ, NN, NTX, PFDUM");
                                if (modelOptions.doEnergyTransport) {
                                    pw.println(bc.getEnergyTransportType() + " " +
                                            (float) adjustedEnergyTransportValue +
                                            "     /C12 -- NTT, TF");                                    
                                }
                                if (modelOptions.doSoluteTransport) {
                                    pw.println(bc.getSoluteTransportType() + " " +
                                            bc.getSoluteTransportValue() +
                                            " -1 1.   /C13 -- NTC, INSBC1, INSBC2, SBFRAC");
                                }
                            }
                        }
                    }
            }
        }
    }
}
