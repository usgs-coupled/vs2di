/*
 * vs2PostProcessorView.java
 */
package vs2;

import mp2.*;
import java.awt.*;

/**
 * The view for the post processor
 */
public class vs2PostProcessorView extends mp2PostProcessorView implements vs2Constants {

    protected mp2Contour contour;
    protected mp2ColorScale pressureHeadColor;
    protected mp2ColorScale moistureColor;
    protected mp2ColorScale saturationColor;
    protected mp2ColorScale transportColor;
    protected mp2ColorScale velocityColor;
    protected mp2ColorScale totalHeadColor;
    
    protected int nx;
    protected int nz;
    protected int [] ix;
    protected int [] iz;
    protected int gridLeft;
    protected int gridTop;
    protected float [] buffer;
    protected float [] vx;
    protected float [] vy;
    protected float [] kSat;
    protected double [] err;
    protected int [] soilType;
    protected int [] vlen = {0, 0};
    protected int mbLabelShift1;
    protected int mbLabelShift2;
    protected boolean showZonation;
    protected float [] dzz;

    public static final int NO_DISPLAY = 0;
    public static final int PRESSURE_HEAD = 1;
    public static final int MOISTURE_CONTENT = 2;
    public static final int SATURATION = 3;
    public static final int TRANSPORT = 4;
    public static final int VECTOR = 5;
    public static final int TOTAL_HEAD = 6;

    /**
     * Creates a post processor view
     */
    public vs2PostProcessorView(mp2PostProcessorFrame frame) {
        super(frame);
        err = new double [2];
        // default display
        display = PRESSURE_HEAD;
        contour = new mp2Contour(this);
        mbLabelShift1 = 140;
        mbLabelShift2 = 140;
        showZonation = false;
    }
    
    protected void getDataFromModel() {
        
        if (model == null || buffer == null || display == NO_DISPLAY) {
            colorScale = null;
            return;
        }
        
        int i,j;
        // Get velocity data
        vs2Model vsMod = (vs2Model) model;
        if (showVectors || display == VECTOR) {
            vsMod.getVx(vx);
            vsMod.getVz(vy);
            if (model.getVectorMode() == VECTOR_AS_FLUX) {
                vsMod.getMoistureContent(buffer);
                for (i=0; i<nx*nz; i++) {
                    vx[i] *= buffer[i];
                    vy[i] *= buffer[i];
                }
            }
        }
        
        // Get nodal data
        switch (display) {
        case PRESSURE_HEAD:
            colorScale = pressureHeadColor;
            vsMod.getPressureHead(buffer);
            break;
        case MOISTURE_CONTENT:
            colorScale = moistureColor;
            vsMod.getMoistureContent(buffer);
            break;
        case SATURATION: 
            colorScale = saturationColor;
            vsMod.getSaturation(buffer);
            break;
        case TRANSPORT:
            colorScale = transportColor;
            vsMod.getTransport(buffer);
            break;
        case VECTOR:
            colorScale = velocityColor;
            for (i=0; i<nx*nz; i++) {
                buffer[i] = (float) Math.sqrt(vx[i] * vx[i] + 
                                              vy[i] * vy[i]);
            }
            break;
        case TOTAL_HEAD:
            colorScale = totalHeadColor;
            // First, get the pressure head
            vsMod.getPressureHead(buffer);
            // Then, change to total head
            for (j=0; j<nz-1; j++) {
                for (i=0; i<nx-1; i++) {
                    buffer[i*nz + j] -= dzz[j];
                }
            }
            break;
        }        
    }
    
    /**
     * Draws the model
     */
    protected void drawModel(Graphics g) {
        
        if (model == null || buffer == null || display == NO_DISPLAY) {
            return;
        }
        
        int i, j, k, index, cellLeft, cellTop, clipLeft, clipRight, clipTop, clipBot;
        
        Rectangle clipRect = g.getClipBounds();
        clipLeft = clipRect.x;
        clipRight = clipRect.x + clipRect.width;
        clipTop = clipRect.y;
        clipBot = clipRect.y + clipRect.height;
        

        // Draw the model
        if (drawingMode == CONTOUR_DRAWING_MODE) {
            contour.SetCellValues(buffer);
            contour.SetCellMask(kSat);
            contour.SetColorScale(colorScale);
            contour.Draw(g);
        } else {
            cellTop = gridTop;
            for (j=1; j<nz-1; j++) {
                if (cellTop <= clipBot || cellTop + iz[j] >= clipTop) {
                    cellLeft = gridLeft;
                    for (i=1; i<nx-1; i++) {
                        if (cellLeft <= clipRight || cellLeft + ix[i] >= clipLeft) {
                            index = i*nz + j;
                            // If cell is active, determine the color and draw the cell
                            if (kSat[index] != 0)  {
                                g.setColor(colorScale.GetColor(buffer[index]));
                                g.fillRect(cellLeft, cellTop, ix[i], iz[j]);
                            }
                        }
                        // Increment the cell left 
                        cellLeft += ix[i];
                    }
                }
                // Increment the cell top
                cellTop += iz[j];
            }
        }
        // draw zonation
        if (showZonation) {
            cellTop = gridTop;
            for (j=1; j<nz-1; j++) {
                if (cellTop <= clipBot || cellTop + iz[j] >= clipTop) {
                    cellLeft = gridLeft;
                    for (i=1; i<nx-1; i++) {
                        if (cellLeft <= clipRight || cellLeft + ix[i] >= clipLeft) {
                            index = i*nz + j;
                            if (kSat[index] != 0)  {
                                if (kSat[index+nz] != 0 && Math.abs(soilType[index]) != Math.abs(soilType[index+nz])) {
                                    g.setColor(Color.gray);
                                    g.drawLine(cellLeft+ix[i], cellTop, cellLeft+ix[i], cellTop + iz[j]);
                                }
                                if (kSat[index+1] != 0 && Math.abs(soilType[index]) != Math.abs(soilType[index+1])) {
                                    g.setColor(Color.gray);
                                    g.drawLine(cellLeft, cellTop + iz[j], cellLeft+ix[i], cellTop + iz[j]);
                                }
                            }
                        }
                        cellLeft += ix[i];
                    }
                }
                cellTop += iz[j];
            }
        }
        
        // Draw vectors
        if (showVectors) {
            int vectorColInterval = model.getVectorColInterval();
            int vectorRowInterval = model.getVectorRowInterval();
            int xc, yc, xlen, ylen;
            g.setColor(Color.black);
            cellTop = gridTop;
            for (j=1; j<nz-1; j++) {
                if (((j-1)%vectorRowInterval) == 0 &&
                            (cellTop <= clipBot || cellTop + iz[j] >= clipTop)) {
                    cellLeft = gridLeft;
                    for (i=1; i<nx-1; i++) {
                        if (((i-1)%vectorColInterval) == 0 &&
                                (cellLeft <= clipRight || cellLeft + ix[i] >= clipLeft)) {
                            index = i*nz + j;
                            // If cell is active, draw the vector
                            if (kSat[index] != 0)  {
                                computePixelComponentsForVector(vx[index], vy[index], vlen);
                                xc = cellLeft + ix[i]/2;
                                yc = cellTop + iz[j]/2;
                                g.drawLine(xc, yc, xc + vlen[0], yc + vlen[1]);
                                if (model.getShowStems() && (Math.abs(vlen[0]) > 5 || Math.abs(vlen[1]) > 5)) {
                                    g.fillRect(xc-1, yc-1, 3, 3);
                                }
                            }
                        }
                        cellLeft += ix[i];
                    }
                }
                cellTop += iz[j];
            }
        }
    }

    /**
     * Draw the mass balance error text
     */
    protected void drawText(Graphics g) {
        if (model == null || display == NO_DISPLAY) {
            return;
        }

        // Draw the strings for time step, model time, and mass balances
        g.setColor(Color.black);
        int pz = extendedGridY + extendedGridHeight + charHeight;
        int px1 = extendedGridX + gridBorder;
        int px2 = px1 + mbLabelShift1;
        int px3 = px2 + mbLabelShift2;
        g.drawString("Time = " + String.valueOf(model.getModelTime()),
                px1, pz);

        pz += charHeight + 10;
        g.drawString("Mass Balance Error", px1, pz);
        g.drawString("Total for Simulation", px2, pz);
        g.drawString("Rate for this step", px3, pz);

        pz += charHeight + 10;
        g.drawString("Fluid", px1, pz);
        vs2Model vsMod = (vs2Model) model;
        vsMod.getFlowMassBalanceErrors(err);
        g.drawString(nf.format(err[0]) + "%", px2, pz);
        g.drawString(nf.format(err[1]) + "%", px3, pz);

        pz += charHeight + 10;
        if (((vs2PostProcessorFrame) frame).getUsage() == ENERGY_TRANSPORT) {
            g.drawString("Energy", px1, pz);
        } else {
            g.drawString("Solute", px1, pz);
        }
        vsMod.getTransportMassBalanceErrors(err);
        g.drawString(nf.format(err[0]) + "%", px2, pz);
        g.drawString(nf.format(err[1]) + "%", px3, pz);
        
        if (display == TOTAL_HEAD) {
            pz += charHeight + 10;
            g.drawString("Note: Datum for total head is at top of grid.", px1, pz);
        }
    }

    /**
     * Resizes the items to be drawn.
     */
    public void resizeContents(double magnification) {
        super.resizeContents(magnification);
        int i;

        // adjust size of color scales
        int cswidth = (int) (colorScaleWidth * magnification);
        int csheight = (int) (colorScaleHeight * magnification);
        pressureHeadColor.SetScaleDimensions(cswidth, csheight);
        moistureColor.SetScaleDimensions(cswidth, csheight);
        saturationColor.SetScaleDimensions(cswidth, csheight);
        transportColor.SetScaleDimensions(cswidth, csheight);
        velocityColor.SetScaleDimensions(cswidth, csheight);
        totalHeadColor.SetScaleDimensions(cswidth, csheight);

        vs2Model vsMod = (vs2Model) model;
        nx = vsMod.getNumCellAlongX ();  // including border cells
        nz = vsMod.getNumCellAlongZ ();  // including border cells
        float [] dx = new float[nx];
        float [] dz = new float[nz];
        kSat = new float[nx*nz];
        buffer = new float[nx*nz];
        vx = new float[nx*nz];
        vy = new float[nx*nz];
        soilType = new int[nx*nz];

        vsMod.getCellSizesAlongZ(dz);
        vsMod.getCellSizesAlongX(dx);
        vsMod.getKSat(kSat);
        vsMod.getTexturalClassArray(soilType);

        // compute coordinates of grid lines
        float [] xp = new float [nx-1];
        xp[0] = model.getGridReferenceX();
        for (i=1; i<xp.length; i++) {
            xp[i] = xp[i-1] + dx[i];
        }

        float [] zp = new float [nz-1];
        zp[0] = model.getGridReferenceY();
        for (i=1; i<zp.length; i++) {
            zp[i] = zp[i-1] + dz[i];
        }
        
        // compute depth to center of cell
        // The variable dzz is same as the variable DZZ in VS2DT, that is,
        // dzz is the depth from the top of the second row to the cell center. 
        // (The first row belongs to the outer inactive ring.) Therefore, dzz
        // for the first row is negative.
        dzz = new float [nz];   
        dzz[0] = -dz[0]/2;
        for (i=1; i<nz; i++) {
            dzz[i] = dzz[i-1] + (dz[i-1] + dz[i])/2;
        }

        Point p1 = modelToView(xp[0], zp[0]);
        Point p2 = modelToView(xp[xp.length-1], zp[zp.length-1]);
        gridLeft = p1.x;
        gridTop = p1.y;
        extendedGridWidth = p2.x - p1.x + 2*gridBorder;
        extendedGridHeight = p2.y - p1.y + 2*gridBorder;
        extendedGridX = p1.x - gridBorder;
        extendedGridY = p1.y - gridBorder;
        // Determine how many pixels for cell size in x direction
        ix = new int[nx];
        for (i=1; i<nx-1; i++) {
            ix[i] = (int) Math.round(xp[i]/modelDistancePerPixelX) - 
                    (int) Math.round(xp[i-1]/modelDistancePerPixelX);
        }

        // Determine how many pixels for cell size in z direction
        iz = new int[nz];
        for (i=1; i<nz-1; i++) {
            iz[i] = (int) Math.round(zp[i]/modelDistancePerPixelY) - 
                    (int) Math.round(zp[i-1]/modelDistancePerPixelY);
        }

        // Determine the location of the color scale
        colorScaleTop = extendedGridY + (extendedGridHeight - 
                    (int) (colorScaleHeight*magnification))/2;
        colorScaleLeft = extendedGridX + extendedGridWidth;

        // Compute spacing for mass balance text
        mbLabelShift1 = fontMetrics.stringWidth("Mass Balance Error") + 20;
        mbLabelShift2 = fontMetrics.stringWidth("Total for Simulation") + 20;
        
        // Set contour dimensions
        contour.SetDimensions(nx, nz, dx, dz, (float) modelDistancePerPixelX,
                (float) modelDistancePerPixelY, gridLeft, gridTop);
    }

    /**
     * Sets the color scales.
     */
    public void setColorScales(mp2ColorScale [] colorScale) {
        pressureHeadColor = colorScale[0]; 
        moistureColor = colorScale[1];         
        saturationColor = colorScale[2];         
        transportColor = colorScale[3];
        velocityColor = colorScale[4]; 
        totalHeadColor = colorScale[5]; 
    }
    
    public void showZonation(boolean b) {
        showZonation = b;
        draw();
    }
}

