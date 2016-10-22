/*
 * vs2ColorCodedMapView.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * Manages a color-coded map.
 * Description to be added.
 *
 * @see mp2.mp2ColorCodedMapData
 */
public class vs2ColorCodedMapView extends mp2BufferedShapesView {

    protected mp2ColorCodeWindow colorCodeWindow;
    protected mp2ToggleButton createZoneButton;
    protected mp2ToggleButton discretizeSolutionButton;
    protected mp2ToggleButton discretizeEquilibriumPhasesButton;
    protected mp2ToggleButton discretizeExchangeButton;
    protected mp2ToggleButton discretizeSurfaceButton;
    protected mp2ToggleButton discretizeGasPhaseButton;
    protected mp2ToggleButton discretizeSolidSolutionsButton;
    protected mp2ToggleButton discretizeKineticsButton;
    
    protected mp2ToggleButton[] discretizeButtons;
    
    protected JMenuItem clonePolygonMenuItem;
    protected boolean canClone;
    
    /**
     * Creates a new vs2ColorCodedMapView.
     */
    public vs2ColorCodedMapView(mp2View view, 
                mp2ColorCodedMapData colorCodedMapData,
                mp2AbstractGridData gridData,
                mp2ColorCodeWindow colorCodeWindow, 
                String homeDirectory) {

        super(view, colorCodedMapData, gridData, homeDirectory);
        this.colorCodeWindow = colorCodeWindow;
        this.canClone = false;
                
        // Create tool bar buttons
        createZoneButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/polygon.gif")), false);
        discretizeSolutionButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretizeS.png")), false);
        discretizeEquilibriumPhasesButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretizeEP.png")), false);
        discretizeExchangeButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretizeEx.png")), false);
        discretizeSurfaceButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretizeSu.png")), false);
        discretizeGasPhaseButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretizeGP.png")), false);
        discretizeSolidSolutionsButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretizeSS.png")), false);
        discretizeKineticsButton = new mp2ToggleButton(
                new ImageIcon(ClassLoader.getSystemResource("images/discretizeK.png")), false);
        
        discretizeButtons = new mp2ToggleButton[] {
            discretizeSolutionButton,
            discretizeEquilibriumPhasesButton,
            discretizeExchangeButton,
            discretizeSurfaceButton,
            discretizeGasPhaseButton,
            discretizeSolidSolutionsButton,
            discretizeKineticsButton
        };
        
        createZoneButton.setToolTipText("Add polygon zone");
        discretizeSolutionButton.setToolTipText("Discretize solution");
        discretizeEquilibriumPhasesButton.setToolTipText("Discretize equilibrium phases");
        discretizeExchangeButton.setToolTipText("Discretize exchange");
        discretizeSurfaceButton.setToolTipText("Discretize surface");
        discretizeGasPhaseButton.setToolTipText("Discretize gas phase");
        discretizeSolidSolutionsButton.setToolTipText("Discretize solid solutions");
        discretizeKineticsButton.setToolTipText("Discretize kinetics");
        
        ButtonGroup bg = new ButtonGroup();
        bg.add(selectAndEditButton);
        bg.add(createZoneButton);
        bg.add(addVertexButton);
        for (int i = 0; i < discretizeButtons.length; ++i) {
            bg.add(discretizeButtons[i]);
        }
        bg.add(zoomButton);

        
        createZoneButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setActiveModeTo(CREATE_SHAPE);
                }
            }
        });
        
         discretizeSolutionButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    vs2ChemistryMapData sd = (vs2ChemistryMapData)shapesData;
                    sd.setCurrentIndex(0);
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });
        discretizeEquilibriumPhasesButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    vs2ChemistryMapData sd = (vs2ChemistryMapData)shapesData;
                    sd.setCurrentIndex(1);
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });
        discretizeExchangeButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    vs2ChemistryMapData sd = (vs2ChemistryMapData)shapesData;
                    sd.setCurrentIndex(2);
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });
        discretizeSurfaceButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    vs2ChemistryMapData sd = (vs2ChemistryMapData)shapesData;
                    sd.setCurrentIndex(3);
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });
        discretizeGasPhaseButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    vs2ChemistryMapData sd = (vs2ChemistryMapData)shapesData;
                    sd.setCurrentIndex(4);
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });
        discretizeSolidSolutionsButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    vs2ChemistryMapData sd = (vs2ChemistryMapData)shapesData;
                    sd.setCurrentIndex(5);
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });
        discretizeKineticsButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    vs2ChemistryMapData sd = (vs2ChemistryMapData)shapesData;
                    sd.setCurrentIndex(6);
                    setActiveModeTo(DISCRETIZE);
                }
            }
        });

        // Clone menu item
        clonePolygonMenuItem = new JMenuItem("Clone...");
        clonePolygonMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_C, Event.CTRL_MASK));
        clonePolygonMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onClonePolygon();
            }
        });

    }

    protected void drawDiscretizedShapes(Graphics g) {
        int [] limits = getDrawingLimitsForRectilinearGrid(g);
        double [] xCoord = ((mp2RectilinearGridData) gridData).getXCoords();
        double [] yCoord = ((mp2RectilinearGridData) gridData).getYCoords();
        shapesData.discretize();
        int [] zoneArray = ((mp2ColorCodedMapData) shapesData).getZoneArray();
        Point p1, p2;
        int numCol = xCoord.length - 1;
        for (int j=limits[2]; j<limits[3]; j++) {
            for (int i=limits[0]; i<limits[1]; i++) {
                if (view.getModelVerticalAxisOrientation() == mp2Drawing.MODEL_VERTICAL_AXIS_UPWARD_POSITIVE) {
                    p1 = view.modelToView(xCoord[i], yCoord[j+1]);
                    p2 = view.modelToView(xCoord[i+1], yCoord[j]); 
                } else {
                    p1 = view.modelToView(xCoord[i], yCoord[j]);
                    p2 = view.modelToView(xCoord[i+1], yCoord[j+1]); 
                }
                g.setColor(colorCodeWindow.getColorOfId(zoneArray[j*numCol+i]));
                g.fillRect(p1.x, p1.y, p2.x - p1.x, p2.y - p1.y);
            }
        }
    }

    /**
     * creates a new shape
     */
    protected mp2Shape createNewShape(Point p) {
        return new mp2Polygon();
    }

    protected void evaluateEditMenu() {
        super.evaluateEditMenu();
        clonePolygonMenuItem.setEnabled(shapesData.getNumberOfSelectedShapes() > 0);
    }

    protected void onClonePolygon() {

    }

    /**
     * Invoked when a new shape is completed
     */
    protected boolean onNewShapeCompletion(mp2Shape shape) {
        shape.setId(colorCodeWindow.getIdOfSelectedRow());
        shape.setFilled(true);
        return true;
    }

    /**
     * Paints this color coded map
     */
    public void paint(Graphics g) {
        if (!isActive) {
            shapesData.deselectAllShapes();
        }
        if (!isVisible || shapesData.getNumberOfShapes() == 0) {
            return;
        }
        Color oldColor = g.getColor();
        if (view.isPaintModeDiscrete()) {
            paintDiscrete(g);
        } else {
            for (int i=0; i<shapesData.getNumberOfShapes(); i++) {
                mp2Shape shape = shapesData.getShape(i);
                Color c = colorCodeWindow.getColorOfId(shapesData.getShape(i).getId());
                if (c!= null) {
                    shape.setForeground(c);
                    shape.draw(g);
                }
                else {
                    shapesData.deleteShapeAt(i);
                    i--;
                }
            }
        }
        g.setColor(oldColor);
        g.setPaintMode();
    }

    /**
     * Loads the edit menu and tool bar when this data view becomes active
     */
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
        editMenu.add(bringToFrontMenuItem);
        editMenu.add(sendToBackMenuItem);
        if (canClone) {
            editMenu.add(clonePolygonMenuItem);
        }
        shapesData.deselectAllShapes();
        evaluateEditMenu();

        // Set up buttons on tool bar
        JToolBar toolBar = view.getFrame().getToolBar();
        toolBar.removeAll();
        toolBar.add(selectAndEditButton);
        toolBar.add(createZoneButton);
        toolBar.add(addVertexButton);
        if (gridData.isDefined()) {
            for (int i = 0; i < discretizeButtons.length; ++i) {
                toolBar.add(discretizeButtons[i]);
            }            
        }
        toolBar.add(zoomButton);
        if (view.hasSiteMap()) {
            toolBar.add(Box.createVerticalStrut(10));
            toolBar.add(view.getXORButton());
        }

        if (view.isPaintModeDiscrete()) {
            vs2ChemistryMapData sd = (vs2ChemistryMapData)shapesData;
            int index = sd.getCurrentIndex();
            discretizeButtons[index].setSelected(true);
            remakeImage = true;
        } else {
            selectAndEditButton.setSelected(true);
        }

        view.setCursor(Cursor.getDefaultCursor());
    }

    /**
     * Sets the active mode
     */
    protected void setActiveModeTo(int m) {
        if (isEditable) {
            if (m == DISCRETIZE) {
                colorCodeWindow.setEnabled(false);
            } else {
                colorCodeWindow.setEnabled(true);
            }
        }
        super.setActiveModeTo(m);
    }

    public void setCanClone(boolean b) {
        canClone = b;
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        super.setEditable(b);
        if (!b) {
            if (createZoneButton.isSelected()
                || addVertexButton.isSelected()) {
                selectAndEditButton.setSelected(true);
            }
        }
        createZoneButton.setEnabled(b);
        addVertexButton.setEnabled(b);
    }
}
