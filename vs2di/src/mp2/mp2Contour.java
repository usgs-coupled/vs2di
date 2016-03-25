/*
 * mp2Contour.java
 */
package mp2;

import java.io.*;
import java.awt.*;

public class mp2Contour {

    protected int nx;   // number of cells in x direction
    protected int ny;   // number of cells in y direction
    protected float [] dx;  // cell sizes in x direction
    protected float [] dy;  // cell sizes in y direction
    protected float [] cellValue;  // cell values to be contoured
    protected float [] cellMask; // indicates inactive cells by zeros
    protected mp2ColorScale colorScale;  // color scale;
    protected int numInterval;  // number of contour intervals
    protected float distancePerPixelX;  // conversion factor
    protected float distancePerPixelY;
    protected int domainLeft;   // number of pixels to domain left
    protected int domainTop;     // number of pixels to domain top
    protected int npx;  // number of points in the x direction
    protected int npy;  // number of points in the y direction
    protected int numPoint;  // total number of points
    protected float [] xp;  // x coordinates of points
    protected float [] yp;  // y coordaintes of points
    protected int [] ix;
    protected int [] iy;
    protected mp2Drawing view;

    public mp2Contour(mp2Drawing view) {
        this.view = view;
        nx = 0;
        ny = 0;
        numInterval = 10;
        distancePerPixelX = 1;
        distancePerPixelY = 1;
        domainLeft = 0;
        domainTop = 0;
        npx = 0;
        npy = 0;
        dx = null;
        dy = null;
        cellValue = null;
        cellMask = null;
        colorScale = null;
        xp = null;
        yp = null;
    }

    public void Draw(Graphics g) {
        int i, j, k, index;
        int [] xpoly = new int[4];
        int [] ypoly = new int[4];
        float [] xx = new float[4];
        float [] yy = new float[4];

        // interpolate cell values to obtain point values
        float [] pointValue = GetPointValuesFromCellValues();

        // determine the values to contour and the colors
        float valueBlue = (float) colorScale.GetValueBlue();
        float valueRed = (float) colorScale.GetValueRed();
        float minValue = valueBlue;
        float maxValue = valueRed;
        if (minValue > maxValue) {
            minValue = valueRed;
            maxValue = valueBlue;
        }
        float deltaValue = (float) colorScale.GetColorInterval();
        int approxNumInterval = (int) ((maxValue-minValue)/deltaValue) + 3;
        float [] contourValue = new float[approxNumInterval];
        Color [] color = new Color[approxNumInterval];
        contourValue[0] = minValue + deltaValue;
        color[0] = colorScale.GetColor(contourValue[0] + deltaValue/2);
        numInterval = 1;
        while (true) {
            contourValue[numInterval] = contourValue[numInterval - 1] + deltaValue;
            color[numInterval] = colorScale.GetColor(contourValue[numInterval] + 
                    deltaValue/2);
            if (contourValue[numInterval] >= maxValue) break;
            numInterval ++;
        }

        Rectangle rect = g.getClipBounds();
        int clipLeft = rect.x;
        int clipTop = rect.y;
        int clipRight = rect.x + rect.width;
        int clipBottom = rect.y + rect.height;
        int cc = 0;
        int rr = 0;
        for (j=1; j<ny-1; j++) {
            rr = Math.round(domainTop + yp[j+1]/distancePerPixelY);
            if (rr > clipTop) {
                for (i=1; i<nx-1; i++) {
                    cc = Math.round(domainLeft + xp[i+1]/distancePerPixelX);
                    if (cc > clipLeft) {
                        index = i*ny + j;
                        if (cellMask[index] != 0) {
                            // First draw the cell in blue
                            xx[0] = xp[i];
                            yy[0] = yp[j];
                            xx[1] = xp[i];
                            yy[1] = yp[j+1];
                            xx[2] = xp[i+1];
                            yy[2] = yp[j+1];
                            xx[3] = xp[i+1];
                            yy[3] = yp[j];
                            for (k=0; k<4; k++)
                            {
                                xpoly[k] = Math.round(domainLeft + xx[k]/distancePerPixelX);
                                ypoly[k] = Math.round(domainTop + yy[k]/distancePerPixelY);
                            }
                            // Draw only cells within the clip area
                            g.setColor(colorScale.GetColor(minValue + deltaValue/2));
                            g.fillPolygon(xpoly, ypoly, 4);
                            if (view.isPrinting()) {
                                g.drawPolygon(xpoly, ypoly, 4);
                            }
                            // The fill in contours
                            DrawCell(i, j, pointValue, contourValue, color, g);
                        }
                    }
                    if (cc >= clipRight) {
                        break;
                    }
                }
            }
            if (rr >= clipBottom) {
                break;
            }
        }
        
    }

   /**
    * Interpolate cell values to obtain point values.
    */
   protected float [] GetPointValuesFromCellValues() {
        // Create array to hold point values and weights, initialize to zero
        float [] pointValue = new float [numPoint];
        float [] weight = new float [numPoint];
        for (int k=0; k<numPoint; k++) {
            pointValue[k] = 0;
            weight[k] = 0;
        }

        // Sum contribution cells to point values, and accumulate weighs
        // Skip the border cells.
        for (int j=1; j<ny-1; j++) {
            for (int i=1; i<nx-1; i++) {
                int cellIndex = i*ny + j;
                if (cellMask[cellIndex] != 0) {
                    int pointIndex = i*npy + j;
                    // use inverse distance weighting
                    float cellWeight = (float) (1.0/Math.sqrt(dx[i]*dx[i] + dy[j]*dy[j]));
                    float contrib = cellValue[cellIndex] * cellWeight;
                    pointValue[pointIndex] += contrib;
                    pointValue[pointIndex+1] += contrib;
                    pointValue[pointIndex+npy] += contrib;
                    pointValue[pointIndex+npy+1] += contrib;
                    weight[pointIndex] += cellWeight;
                    weight[pointIndex+1] += cellWeight;
                    weight[pointIndex+npy] += cellWeight;
                    weight[pointIndex+npy+1] += cellWeight;
                }
            }
        }

        // Divide summed contributions by weight
        for (int k=0; k<numPoint; k++) {
            if (weight[k] != 0) pointValue[k] /= weight[k];
        }
              
        return pointValue;
   }

   /**
    * Set the grid and grid dimensions
    */
   public void SetDimensions(int nx, int ny, float [] dx, float [] dy, 
        float distancePerPixelX, float distancePerPixelY, 
        int domainLeft, int domainTop) {
      this.nx = nx;
      this.ny = ny;
      this.dx = dx;
      this.dy = dy;
      this.distancePerPixelX = distancePerPixelX;
      this.distancePerPixelY = distancePerPixelY;
      this.domainLeft = domainLeft;
      this.domainTop = domainTop;

      npx = nx+1;
      npy = ny+1;
      numPoint = npx*npy;

      // Compute the x and y coordinates of the points, exclude last point
      int i;
      xp = new float[nx];
      yp = new float[ny];
      xp[0] = -dx[0];
      for (i=1; i<nx; i++) xp[i] = xp[i-1] + dx[i-1];
      yp[0] = -dy[0];
      for (i=1; i<ny; i++) yp[i] = yp[i-1] + dy[i-1];

      // Determine how many pixels for cell size in x direction
      ix = new int[nx];
      for (i=1; i<nx-1; i++)
      {
         ix[i] = Math.round(xp[i+1]/distancePerPixelX) - 
            Math.round(xp[i]/distancePerPixelX);
      }

      // Determine how many pixels for cell size in y direction
      iy = new int[ny];
      for (i=1; i<ny-1; i++)
      {
         iy[i] = Math.round(yp[i+1]/distancePerPixelY) - 
            Math.round(yp[i]/distancePerPixelY);
      }
   }

   //--------------------------------------------------------------------------
   // Set the cell mask
   //--------------------------------------------------------------------------
   public void SetCellMask(float [] cellMask)
   {
      this.cellMask = cellMask;
   }

   //--------------------------------------------------------------------------
   // Set the cell values to be contoured
   //--------------------------------------------------------------------------
   public void SetCellValues(float [] cellValue)
   {
      this.cellValue = cellValue;
   }

   //--------------------------------------------------------------------------
   // Set the color scale
   //--------------------------------------------------------------------------
   public void SetColorScale(mp2ColorScale colorScale)
   {
      this.colorScale = colorScale;
   }

   //--------------------------------------------------------------------------
   // Constants
   //--------------------------------------------------------------------------
   protected final static int [][] edges = { {0,1}, {1,2}, {2,3}, {3,0} };

    protected final static int [][] cases =
      { 
         {-1, -1, -1, -1, -1},
         {0, 3, -1, -1, -1},
         {1, 0, -1, -1, -1},
         {1, 3, -1, -1, -1},
         {2, 1, -1, -1, -1},
         {0, 3, 2, 1, -1},
         {2, 0, -1, -1, -1},
         {2, 3, -1, -1, -1},
         {3, 2, -1, -1, -1},
         {0, 2, -1, -1, -1},
         {1, 0, 3, 2, -1},
         {1, 2, -1, -1, -1},
         {3, 1, -1, -1, -1},
         {0, 1, -1, -1, -1},
         {3, 0, -1, -1, -1},
         {-1, -1, -1, -1, -1}
      };

    protected final static int [][] polys =
      {
         {-1, -1, -1, -1, -1, -1, -1, -1},
         { 0, -1, -1, -1, -1, -1, -1, -1},
         { 1, -1, -1, -1, -1, -1, -1, -1},
         { 0,  1, -1, -1, -1, -1, -1, -1},
         { 2, -1, -1, -1, -1, -1, -1, -1},
         { 0, -1, -1, -1,  2, -1, -1, -1},
         { 1,  2, -1, -1, -1, -1, -1, -1},
         { 0,  1,  2, -1, -1, -1, -1, -1},
         { 3, -1, -1, -1, -1, -1, -1, -1},
         { 3,  0, -1, -1, -1, -1, -1, -1},
         { 1, -1, -1, -1,  3, -1, -1, -1},
         { 3,  0,  1, -1, -1, -1, -1, -1},
         { 2,  3, -1, -1, -1, -1, -1, -1},
         { 2,  3,  0, -1, -1, -1, -1, -1},
         { 1,  2,  3, -1, -1, -1, -1, -1},
         { 0,  1,  2,  3, -1, -1, -1, -1}
      };

    protected final static int [] CASE_MASK = {1,2,4,8};

   //--------------------------------------------------------------------------
   // Draw a cell
   //--------------------------------------------------------------------------
   protected void DrawCell(int i, int j, float[] pointValue, float[] contourValue, 
      Color[] color, Graphics g)
   {
      int [] xpoly = new int[5];
      int [] ypoly = new int[5];

      // Determine the values at 4 corners of the cell
      float [] scalar = new float[4];
      int pointIndex = i*npy+j;
      scalar[0] = pointValue[pointIndex];
      scalar[1] = pointValue[pointIndex+1];
      scalar[2] = pointValue[pointIndex+1+npy];
      scalar[3] = pointValue[pointIndex+npy];

      // Determine the coordinates of 4 corners of the cell
      float [] xx = new float[4];
      float [] yy = new float[4];
      xx[0] = xp[i];
      yy[0] = yp[j];
      xx[1] = xp[i];
      yy[1] = yp[j+1];
      xx[2] = xp[i+1];
      yy[2] = yp[j+1];
      xx[3] = xp[i+1];
      yy[3] = yp[j];

      for (int ic = 0; ic <= numInterval; ic++)
      {
          // Determine the case number
         int caseNumber = 0;
          for (int cn=0; cn<4; cn++)
          {
            if (scalar[cn] >= contourValue[ic]) caseNumber |= CASE_MASK[cn];
          }

         // if the contour value is greater than all 4 scalars
         // (i.e., case number 15) then paint the entire cell
         if (caseNumber == 15)
         {
            for (int ii=0; ii<4; ii++)
            {
               xpoly[ii] = Math.round(domainLeft + xx[ii]/distancePerPixelX);
               ypoly[ii] = Math.round(domainTop + yy[ii]/distancePerPixelY);
            }
            g.setColor(color[ic]);
            g.fillPolygon(xpoly, ypoly, 4);
            if (view.isPrinting()) {
                g.drawPolygon(xpoly, ypoly, 4);
            }
         }

         // else if caseNumber is not 0, figure out which part of the
         // cell to color
         else if (caseNumber != 0)
         {
            int [] edge = cases[caseNumber];
            int [] poly = polys[caseNumber];

            int k, m;
            for (k=0, m=0; edge[k]>-1; k+=2, m+=4)
            {
               int npoints = 0;

               // Build the polygon vertices first by including appropriate cell corners
               for (int n = 0; n<4; n++)
               {
                  int p = poly[m+n];
                  if (p != -1)
                  {
                     xpoly[npoints] = Math.round(domainLeft + xx[p]/distancePerPixelX);
                     ypoly[npoints] = Math.round(domainTop + yy[p]/distancePerPixelY);
                     npoints++;
                  }
               }

               for (int ii=0; ii<2; ii++)
               {
                  int [] vert = edges[edge[k+ii]];
                  int e1, e2;
                  // calculate a preferred interpolation direction
                  float deltaScalar = scalar[vert[1]] - scalar[vert[0]];
                  if (deltaScalar > 0)
                  {
                     e1 = vert[0]; e2 = vert[1];
                  }
                  else
                  {
                     e1 = vert[1]; e2 = vert[0];
                     deltaScalar = -deltaScalar;
                  }
      
                  // linear interpolation
                  float t;
                  if (deltaScalar == 0.0f) t = 0.0f;
                  else t = (contourValue[ic] - scalar[e1]) / deltaScalar;

                  float x = xx[e1] + t*(xx[e2] - xx[e1]);
                  float y = yy[e1] + t*(yy[e2] - yy[e1]);
                  xpoly[npoints] = Math.round(domainLeft+x/distancePerPixelX);
                  ypoly[npoints] = Math.round(domainTop+y/distancePerPixelY);
                  npoints++;
               }
               g.setColor(color[ic]);
               g.fillPolygon(xpoly, ypoly, npoints);
               if (view.isPrinting()) {
                   g.drawPolygon(xpoly, ypoly, npoints);
               }
            }
         }
      }
   }
}

