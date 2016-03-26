/*
 * vs2View.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Displays graphical and table data
 */
public class vs2View extends mp2View implements vs2Constants {

    // table windows
    protected vs2TexturalClassWindow texturalClassWindow;
    protected vs2EvapotranspirationWindow evapotranspirationWindow;
    protected vs2RechargePeriodWindow rechargePeriodWindow;

    // graphical data views
    protected mp2DomainView domainView;
    protected mp2ColorCodedMapView texturalMapView;
    protected vs2InitialEquilibriumProfileView initialEquilibriumProfileView;
    protected mp2ContourMapView initialPressureHeadView;
    protected mp2ContourMapView initialMoistureContentView;
    protected mp2ContourMapView initialTransportView;
    protected vs2BoundaryConditionsView boundaryConditionsView;
    protected vs2FluidSourceView fluidSourceView;
    protected mp2ObservationPointsView observationPointsView;
    protected mp2RectilinearGridView gridView;
    protected mp2SiteMapView siteMapView;

    /**
     * Disposes of the resources used by this class
     */
    public void dispose() {
        texturalClassWindow.dispose();
        evapotranspirationWindow.dispose();
        rechargePeriodWindow.dispose();
        gridView.disposeGridWindow();
    }

    /**
     * Initializes this view
     *
     * @param  theApp  the application object to which this
     *                 view belongs.
     */
    public void init(mp2App theApp) {
        super.init(theApp);

        // Create the table windows
        vs2ModelOptions modelOptions =
                (vs2ModelOptions) doc.getData(MODEL_OPTIONS);
        texturalClassWindow = new vs2TexturalClassWindow(frame,
                (vs2TexturalClassData) doc.getData(TEXTURAL_CLASS),
                modelOptions, theApp);
        evapotranspirationWindow = new vs2EvapotranspirationWindow(frame,
                (vs2EvapotranspirationData) doc.getData(EVAPOTRANSPIRATION),
                modelOptions, theApp);
        rechargePeriodWindow = new vs2RechargePeriodWindow(frame,
                (vs2RechargePeriodData) doc.getData(RECHARGE_PERIOD),
                (vs2BoundaryConditionsData) doc.getData(BOUNDARY_CONDITIONS),
                (vs2FluidSourceData) doc.getData(FLUID_SOURCE),
                modelOptions);

        // Create the graphical data views
        String homeDirectory = theApp.getHomeDirectory();
        mp2RectilinearGridData gridData =
                (mp2RectilinearGridData) doc.getData(MODEL_GRID);
        domainView = new mp2DomainView(this,
                (mp2DomainData) doc.getData(DOMAIN),
                gridData,
                (vs2BoundaryConditionsData) doc.getData(BOUNDARY_CONDITIONS),
                homeDirectory);
        texturalMapView = new mp2ColorCodedMapView(this,
                (vs2TexturalMapData) doc.getData(TEXTURAL_MAP),
                gridData, texturalClassWindow, homeDirectory);
        initialEquilibriumProfileView = new vs2InitialEquilibriumProfileView(this,
                (vs2InitialEquilibriumProfileData) doc.getData(INITIAL_EQUILIBRIUM_PROFILE),
                gridData, homeDirectory);
        initialPressureHeadView = new mp2ContourMapView(this,
                (vs2InitialData) doc.getData(INITIAL_PRESSURE_HEAD),
                gridData, homeDirectory);
        initialMoistureContentView = new mp2ContourMapView(this,
                (vs2InitialData) doc.getData(INITIAL_MOISTURE_CONTENT),
                gridData, homeDirectory);
        initialMoistureContentView.setMinimumContourValue(0);
        initialMoistureContentView.setMaximumContourValue(1);
        initialTransportView = new mp2ContourMapView(this,
                (vs2InitialData) doc.getData(INITIAL_TRANSPORT),
                gridData, homeDirectory);
        if (! vs2App.doHeat()) {
            initialTransportView.setMinimumContourValue(0);
        }
        boundaryConditionsView = new vs2BoundaryConditionsView(this,
                (vs2BoundaryConditionsData) doc.getData(BOUNDARY_CONDITIONS),
                gridData, modelOptions,
                (mp2DomainData) doc.getData(DOMAIN),
                homeDirectory);
        fluidSourceView = new vs2FluidSourceView(this,
                (vs2FluidSourceData) doc.getData(FLUID_SOURCE),
                gridData, modelOptions, homeDirectory);
        observationPointsView = new mp2ObservationPointsView(this,
                (vs2ObservationPointsData) doc.getData(OBSERVATION_POINTS),
                gridData, homeDirectory);
        gridView = new mp2RectilinearGridView(this, gridData,
                (mp2DomainData) doc.getData(DOMAIN),
                homeDirectory);
        siteMapView = new mp2SiteMapView(this,
                (mp2SiteMapData) doc.getData(SITE_MAP),
                homeDirectory);

        mp2BufferedShapesView.setImageBufferingEnabled(modelOptions.imageBufferingEnabled);

        rechargePeriodWindow.setBoundaryConditionsView(boundaryConditionsView);
        domainView.setUseRadialCoordinates(modelOptions.useRadialCoord);
        if (modelOptions.useRadialCoord) {
            horizontalAxisName = "r";
        } else {
            horizontalAxisName = "x";
        }
        setActiveDataView(DOMAIN);
        vs2FrameManager frameManager = (vs2FrameManager) frame.getManager();
        frameManager.updateDataChooserAndShowMenu(modelOptions);

    }

    /**
     * Designates the specified data view as active. Data managed
     * by the active view can be edited.
     *
     * @param  dataViewId  The data view identified by an
     *                     (code>int</code> value as defined in
     *                     the mp2Constants and vs2Constants interfaces.
     *
     * @see  mp2.mp2Constants, vs2.vs2Constants
     */
    public void setActiveDataView(int dataViewId) {
        if (activeDataView != null) {
            activeDataView.setActive(false);
        }
        if (activeDataView == gridView) {
            gridView.hideGridWindow();
        }
        mp2BufferedShapesView.remakeImage();

        switch (dataViewId) {
        case DOMAIN:
            activeDataView = domainView;
            break;
        case TEXTURAL_MAP:
            activeDataView = texturalMapView;
            break;
        case INITIAL_FLOW:
            vs2ModelOptions modelOptions =
                    (vs2ModelOptions) doc.getData(MODEL_OPTIONS);
            switch (modelOptions.initialFlowType) {
            case INITIAL_EQUILIBRIUM_PROFILE:
                activeDataView = initialEquilibriumProfileView;
                break;
            case INITIAL_PRESSURE_HEAD:
                activeDataView = initialPressureHeadView;
                break;
            case INITIAL_MOISTURE_CONTENT:
                activeDataView = initialMoistureContentView;
                break;
            default:
                activeDataView = domainView;
                break;
            }
            break;
        case INITIAL_TRANSPORT:
            activeDataView = initialTransportView;
            break;
        case BOUNDARY_CONDITIONS:
            activeDataView = boundaryConditionsView;
            break;
        case FLUID_SOURCE:
            activeDataView = fluidSourceView;
            break;
        case OBSERVATION_POINTS:
            activeDataView = observationPointsView;
            break;
        case MODEL_GRID:
            activeDataView = gridView;
            break;
        case SITE_MAP:
            activeDataView = siteMapView;
            XORButton.setEnabled(true);
            break;
        default:
            activeDataView = domainView;
            break;
        }

        activeDataView.setActive(true);
        activeDataView.prepareToActivate();
        frame.getContentPane().validate();
        frame.getContentPane().repaint();
    }

    /**
     * Shows or hides the specified data view or table window,
     * depending on the parameter b.
     *
     * @param  item   the item to be shown or hidden, identified
     *                by an <code>int</code> constant as defined in
     *                the mp2Constants and vs2Constants interfaces.
     *
     * @param  b  If <code>true</code> show the specified item.
     *            Otherwise, hide the specified item.
     *
     * @see  mp2.mp2Constants, vs2.vs2Constants
     */
    public void setVisible(int item, boolean b) {
        switch(item) {
        case TEXTURAL_CLASS:
            texturalClassWindow.setVisible(b);
            break;
        case EVAPOTRANSPIRATION:
            evapotranspirationWindow.setVisible(b);
            break;
        case RECHARGE_PERIOD:
            rechargePeriodWindow.setVisible(b);
            break;
        case TEXTURAL_MAP:
            texturalMapView.setVisible(b);
            mp2BufferedShapesView.remakeImage();
            if (!b && activeDataView.equals(texturalMapView)) {
                frame.getDataChooser().setSelectedIndex(0);
            }
            repaint();
            break;
        case INITIAL_FLOW:
            initialEquilibriumProfileView.setVisible(b);
            initialPressureHeadView.setVisible(b);
            initialMoistureContentView.setVisible(b);
            mp2BufferedShapesView.remakeImage();
            if (!b && (activeDataView.equals(initialEquilibriumProfileView)
                    || activeDataView.equals(initialPressureHeadView)
                    || activeDataView.equals(initialMoistureContentView))) {
                frame.getDataChooser().setSelectedIndex(0);
            }
            repaint();
            break;
        case INITIAL_TRANSPORT:
            initialTransportView.setVisible(b);
            mp2BufferedShapesView.remakeImage();
            if (!b && activeDataView.equals(initialTransportView)) {
                frame.getDataChooser().setSelectedIndex(0);
            }
            repaint();
            break;
        case BOUNDARY_CONDITIONS:
            boundaryConditionsView.setVisible(b);
            if (!b && activeDataView.equals(boundaryConditionsView)) {
                frame.getDataChooser().setSelectedIndex(0);
            }
            repaint();
            break;
        case FLUID_SOURCE:
            fluidSourceView.setVisible(b);
            if (!b && activeDataView.equals(fluidSourceView)) {
                frame.getDataChooser().setSelectedIndex(0);
            }
            repaint();
            break;
        case OBSERVATION_POINTS:
            observationPointsView.setVisible(b);
            if (!b && activeDataView.equals(observationPointsView)) {
                frame.getDataChooser().setSelectedIndex(0);
            }
            repaint();
            break;
        case MODEL_GRID:
            gridView.setVisible(b);
            if (!b && activeDataView.equals(gridView)) {
                frame.getDataChooser().setSelectedIndex(0);
            }
            repaint();
            break;
        case SITE_MAP:
            siteMapView.setVisible(b);
            if (!b && activeDataView.equals(siteMapView)) {
                frame.getDataChooser().setSelectedIndex(0);
            }
            if (!b) {
                XORButton.setSelected(false);
            }
            XORButton.setEnabled(b);
            repaint();
            break;
        default:
            break;
        }
    }

    /**
     * Paint the data views that are visible
     */
    public void paintComponent(Graphics g) {
        // Paint the white drawing background
        Rectangle rect = g.getClipBounds();
        if (rect.x < drawingWidthInPixels && rect.y < drawingHeightInPixels) {
            g.setColor(Color.white);
            g.fillRect(rect.x, rect.y,
                Math.min(rect.width, drawingWidthInPixels - rect.x + 1),
                Math.min(rect.height, drawingHeightInPixels - rect.y + 1));
        }

        // paint the data views. Data views that are set not visible will
        // return without painting.
        if (!paintDiscrete) {
            if (activeDataView != siteMapView && !drawInXORMode) {
                siteMapView.paint(g);
                texturalMapView.paint(g);
            } else {
                texturalMapView.paint(g);
                if (drawInXORMode) {
                    g.setXORMode(Color.white);
                }
                siteMapView.paint(g);
                g.setPaintMode();
            }
            boundaryConditionsView.paint(g);
            vs2ModelOptions modelOptions =
                    (vs2ModelOptions) doc.getData(MODEL_OPTIONS);

            if (activeDataView == initialTransportView) {
                switch (modelOptions.initialFlowType) {
                case INITIAL_EQUILIBRIUM_PROFILE:
                    initialEquilibriumProfileView.paint(g);
                    gridView.paint(g);
                    break;
                case INITIAL_PRESSURE_HEAD:
                    gridView.paint(g);
                    initialPressureHeadView.paint(g);
                    break;
                case INITIAL_MOISTURE_CONTENT:
                    gridView.paint(g);
                    initialMoistureContentView.paint(g);
                    break;
                }
                initialTransportView.paint(g);
            } else {
                initialTransportView.paint(g);
                switch (modelOptions.initialFlowType) {
                case INITIAL_EQUILIBRIUM_PROFILE:
                    initialEquilibriumProfileView.paint(g);
                    gridView.paint(g);
                    break;
                case INITIAL_PRESSURE_HEAD:
                    gridView.paint(g);
                    initialPressureHeadView.paint(g);
                    break;
                case INITIAL_MOISTURE_CONTENT:
                    gridView.paint(g);
                    initialMoistureContentView.paint(g);
                    break;
                }
            }
            fluidSourceView.paint(g);
            observationPointsView.paint(g);
            domainView.paint(g);
            activeDataView.paintLast(g);
        } else {
            if (!drawInXORMode) {
                siteMapView.paint(g);
            }
            vs2ModelOptions modelOptions =
                    (vs2ModelOptions) doc.getData(MODEL_OPTIONS);
            if (activeDataView == initialEquilibriumProfileView
                || activeDataView == initialPressureHeadView
                || activeDataView == initialMoistureContentView) {

                switch (modelOptions.initialFlowType) {
                case INITIAL_EQUILIBRIUM_PROFILE:
                    initialEquilibriumProfileView.paint(g);
                    break;
                case INITIAL_PRESSURE_HEAD:
                    initialPressureHeadView.paint(g);
                    break;
                case INITIAL_MOISTURE_CONTENT:
                    initialMoistureContentView.paint(g);
                    break;
                }
            }
            else if (activeDataView == initialTransportView) {
                initialTransportView.paint(g);
            }
            else {
                if (texturalMapView.isVisible()) {
                    texturalMapView.paint(g);
                }
                else if (modelOptions.initialFlowType == INITIAL_EQUILIBRIUM_PROFILE
                            && initialEquilibriumProfileView.isVisible()) {
                    initialEquilibriumProfileView.paint(g);
                }
                else if (modelOptions.initialFlowType == INITIAL_PRESSURE_HEAD
                            && initialPressureHeadView.isVisible()) {
                    initialPressureHeadView.paint(g);
                }
                else if (modelOptions.initialFlowType == INITIAL_MOISTURE_CONTENT
                            && initialMoistureContentView.isVisible()) {
                    initialMoistureContentView.paint(g);
                }
                else if (initialTransportView.isVisible()) {
                    initialTransportView.paint(g);
                }
            }
            if (drawInXORMode) {
                g.setXORMode(Color.white);
                siteMapView.paint(g);
                g.setPaintMode();
            }
            boundaryConditionsView.paint(g);
            fluidSourceView.paint(g);
            observationPointsView.paint(g);
            gridView.paint(g);
            domainView.paint(g);
            if (gridView.isActive()) {
                gridView.paintLast(g);
            }
        }

    }

    public void setEditable(boolean b) {
        super.setEditable(b);
        texturalClassWindow.setEnabled(b && !paintDiscrete);
        evapotranspirationWindow.setEnabled(b && !paintDiscrete);
        rechargePeriodWindow.setEnabled(b && !paintDiscrete);

        domainView.setEditable(b);
        texturalMapView.setEditable(b);
        initialEquilibriumProfileView.setEditable(b);
        initialPressureHeadView.setEditable(b);
        initialMoistureContentView.setEditable(b);
        initialTransportView.setEditable(b);
        boundaryConditionsView.setEditable(b);
        fluidSourceView.setEditable(b);
        observationPointsView.setEditable(b);
        gridView.setEditable(b);
        siteMapView.setEditable(b);
        repaint();
    }

    public void setPaintModeToDiscrete(boolean b) {
        super.setPaintModeToDiscrete(b);
        texturalClassWindow.setEnabled(!b && isEditable);
        evapotranspirationWindow.setEnabled(!b && isEditable);
        rechargePeriodWindow.setEnabled(!b && isEditable);
    }

    /**
     * Updates the table windows managed by this view. This is called
     * when model options have been changed.
     *
     * @param  modelOptions  the data object describing the model options.
     */
    public void UpdateTableWindows(vs2ModelOptions modelOptions) {
        texturalClassWindow.UpdateTabs(modelOptions);
        if (!modelOptions.doEvaporation && !modelOptions.doTranspiration) {
            frame.getMenuItem(EVAPOTRANSPIRATION).setSelected(false);
        }
        else {
            evapotranspirationWindow.UpdateTabs(modelOptions);
        }
        rechargePeriodWindow.UpdateTable(modelOptions);
    }

    public void setUseRadialCoordinates(boolean b) {
        domainView.setUseRadialCoordinates(b);
        super.setUseRadialCoordinates(b);
    }

    public vs2TexturalClassWindow getTexturalClassWindow() {
        return texturalClassWindow;
    }
}
