/*
 * mp2ObservationPointsView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.*;

public class mp2ObservationPointsView extends mp2ShapesView {

    protected mp2ToggleButton createPointButton;
    protected mp2ToggleButton discretizeButton;
    public static final Color OBSERVATION_POINT_COLOR = new Color(127, 85, 0);

    public mp2ObservationPointsView(mp2View view, 
                    mp2ObservationPointsData shapesData, 
                    mp2AbstractGridData gridData,
                    String homeDirectory) {

        super(view, shapesData, gridData, homeDirectory);

        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator
                                + "images" + fileSeparator;

        createPointButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "diamond.gif"), false);
        createPointButton.setToolTipText("Add a point");
        createPointButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    onCreateShape();
                }
            }
        });
        discretizeButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "discretize.gif"), false);
        discretizeButton.setToolTipText("Discretize");
        discretizeButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });

        ButtonGroup bg = new ButtonGroup();
        bg.add(selectAndEditButton);
        bg.add(createPointButton);
        bg.add(discretizeButton);
        bg.add(zoomButton);
    }

    protected mp2Shape createNewShape(Point p) {
        mp2Point point = new mp2Point();
        point.setIcon(mp2Point.DIAMOND_ICON);
        point.setForeground(OBSERVATION_POINT_COLOR);
        return point;
    }

    protected void onCreateShape() {
        setActiveModeTo(CREATE_SHAPE);
    }

    public void paint(Graphics g) {
        if (!isActive) {
            shapesData.deselectAllShapes();
        }
        if (!isVisible || shapesData.getNumberOfShapes() == 0) {
            return;
        }
        Color oldColor = g.getColor();
        if (view.isPaintModeDiscrete()) {
            shapesData.discretize();
            drawDiscretizedShapes(g);
        } else {
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                shapesData.getShape(i).draw(g);
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    // override this method for finite element data
    protected void drawDiscretizedShapes(Graphics g) {
        if (!(gridData instanceof mp2RectilinearGridData)) {
            return;
        }
        Rectangle rect = g.getClipBounds();
        int clipRight = rect.x + rect.width;
        int clipBottom = rect.y + rect.height;
        Vector observationCellIndices = 
                ((mp2ObservationPointsData) shapesData).getObservationCellIndices();
        double [] xCoord = ((mp2RectilinearGridData) gridData).getXCoords();
        double [] yCoord = ((mp2RectilinearGridData) gridData).getYCoords();
        int numCol = xCoord.length - 1;
        int numRow = yCoord.length - 1;
        Point p1, p2;
        int col, row;
        g.setColor(OBSERVATION_POINT_COLOR);
        for (int i=0; i<observationCellIndices.size(); i++) {
            int cellIndex = ((Integer) observationCellIndices.elementAt(i)).intValue();
            row = cellIndex/numCol;
            col = cellIndex - row*numCol;
            if (view.getModelVerticalAxisOrientation() ==
                    mp2Drawing.MODEL_VERTICAL_AXIS_DOWNWARD_POSITIVE) {
                p1 = view.modelToView(xCoord[col], yCoord[row]);
                p2 = view.modelToView(xCoord[col+1], yCoord[row+1]);
            } else {
                p1 = view.modelToView(xCoord[col], yCoord[row+1]);
                p2 = view.modelToView(xCoord[col+1], yCoord[row]);
            }
            if (p1.x <= clipRight && p1.y <= clipBottom 
                        && p2.x >= rect.x && p2.y >= rect.y) {
                g.fillRect(p1.x, p1.y, p2.x - p1.x, p2.y - p1.y);
            }
        }
    }

    public void prepareToActivate() {
        super.prepareToActivate();

        // Set up the "Edit" menu
        JMenu editMenu = view.getFrame().getMenu(EDIT_MENU);
        editMenu.removeAll();
        editMenu.add(undoMenuItem);
        editMenu.add(redoMenuItem);
        editMenu.addSeparator();
        editMenu.add(selectAllMenuItem);
        editMenu.add(deleteMenuItem);
        shapesData.deselectAllShapes();
        evaluateEditMenu();

        // Set up buttons on tool bar
        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(selectAndEditButton);
        toolBar.add(createPointButton);
        if (gridData.isDefined()) {
            toolBar.add(discretizeButton);
        }
        toolBar.add(zoomButton);

        if (view.isPaintModeDiscrete()) {
            discretizeButton.setSelected(true);
        } else {
            selectAndEditButton.setSelected(true);
        }

        view.setCursor(Cursor.getDefaultCursor());
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        super.setEditable(b);
        if (!b) {
            if (createPointButton.isSelected()) {
                selectAndEditButton.setSelected(true);
            }
        }
        createPointButton.setEnabled(b);
    }

}
