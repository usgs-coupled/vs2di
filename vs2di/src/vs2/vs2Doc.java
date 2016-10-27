/*
 * vs2Doc.java
 */
package vs2;

import mp2.*;
import java.io.*;
import java.util.*;

/**
 * The document for the VS2DT preprocessor application. Holds
 * the application data, and handles user selection of model
 * options. A VS2DT document holds three kinds of data:
 * <LI>Model options,</LI>
 * <LI>Table data, and</LI>
 * <LI>graphical data.</LI><BR>
 */
public class vs2Doc extends mp2Doc implements vs2Constants,
        Serializable {

    static final long serialVersionUID = 7951568879105240378L;

    // Indicates usage - for solute or energy transport
    // Deserialization of object saved from version 1.1
    // will have a default usage value of 0, which is solute
    // transport.
    protected int usage;

    // Model options and post processor options
    protected vs2ModelOptions modelOptions;
    protected vs2PostProcessorOptions postProcessorOptions;

    // Table data
    protected vs2TexturalClassData texturalClassData;
    protected vs2ChemistryClassData chemistryClassData;
    protected vs2EvapotranspirationData evapotranspirationData;
    protected vs2RechargePeriodData rechargePeriodData;

    // Graphical data
    protected mp2DomainData domainData;
    protected vs2TexturalMapData texturalMapData;
    protected vs2ChemistryMapData chemistryMapData;
    protected vs2InitialEquilibriumProfileData initialEquilibriumProfileData;
    protected vs2InitialData initialPressureHead;
    protected vs2InitialData initialMoistureContent;
    protected vs2InitialData initialConcentrationData;
    protected vs2InitialData initialTemperatureData;
    protected vs2BoundaryConditionsData boundaryConditionsData;
    protected vs2FluidSourceData fluidSourceData;
    protected vs2ObservationPointsData observationPointsData;
    protected mp2RectilinearGridData gridData;
    protected mp2SiteMapData siteMapData;
    protected boolean isNewDoc;

    /**
     * Creates a document
     */
    public vs2Doc() {
        super();

        currentVersion = vs2Constants.VS2_VERSION;
        serializedVersion = currentVersion;

        usage = SOLUTE_AND_ENERGY_TRANSPORT;

        // Create the model options and post processor options data
        modelOptions = new vs2ModelOptions();
        postProcessorOptions = new vs2PostProcessorOptions();

        // Create table data
        texturalClassData = new vs2TexturalClassData();
        chemistryClassData = new vs2ChemistryClassData();
        evapotranspirationData = new vs2EvapotranspirationData();
        rechargePeriodData = new vs2RechargePeriodData();

        // Create graphical data
        domainData = new mp2DomainData();
        texturalMapData = new vs2TexturalMapData();
        chemistryMapData = new vs2ChemistryMapData();
        initialEquilibriumProfileData = new vs2InitialEquilibriumProfileData();
        initialPressureHead = new vs2InitialData();
        initialMoistureContent = new vs2InitialData();
        initialConcentrationData = new vs2InitialData();
        initialTemperatureData = new vs2InitialData();
        boundaryConditionsData = new vs2BoundaryConditionsData();
        fluidSourceData = new vs2FluidSourceData();
        observationPointsData = new vs2ObservationPointsData();
        gridData = new mp2RectilinearGridData();
        siteMapData = new mp2SiteMapData();
        isNewDoc = true;

    }

    /**
     * Converts the serialized version to current version
     */
    public void convertToCurrentVersion() {
        // Conversion of vs2dti 1.0 beta file to update for revised definition
        // of van Genuchten parameters
        if (serializedVersion.equals("1.0 beta")) {
            if (texturalClassData.updateVanGenuchtenParametersFrom1p0betaTo1p0()) {
                String [] msg = new String[2];
                msg[0] = "The sign of the \"alpha\" parameter in the van Genuchten function";
                msg[1] = "has been reversed to conform with the new definition used in this version.";
                mp2MessageBox.showMessageDialog(msg, "Warning");
            }
        }
        if (serializedVersion.startsWith("1.1") ||
                serializedVersion.startsWith("1.0")) {
            modelOptions.outputToAuxFilesEveryTimeStep = true;
            postProcessorOptions.startupDisplay = 1;
        }

        // version 1.1 textural class data has 34 entries to
        // each row and must be extended to 38 entries.
        texturalClassData.convertToCurrentVersion();
        chemistryClassData.convertToCurrentVersion();
        // extra entries in model options need to be initialized.
        modelOptions.convertToCurrentVersion();

        switch (usage) {
            case SOLUTE_TRANSPORT:
                // 1.3 or previous version
                modelOptions.doSoluteTransport = modelOptions.doTransport;
                modelOptions.doEnergyTransport = false;
                break;
            case ENERGY_TRANSPORT:
                // 1.3 or previous version
                modelOptions.doEnergyTransport = modelOptions.doTransport;
                modelOptions.doSoluteTransport = false;
                break;
            case SOLUTE_AND_ENERGY_TRANSPORT:
                // 1.4 or newer version
                // No-Op
                break;
            default:
                assert(false);
                break;
        }
        usage = SOLUTE_AND_ENERGY_TRANSPORT;

        // version 1.1 file doesn't have initial temperature data,
        // so we create one here.
        if (initialTemperatureData == null) {
            initialTemperatureData = new vs2InitialData();
        }
        if (fluidSourceData == null) {
            fluidSourceData = new vs2FluidSourceData();
        }
        // new in 1.4
        if (chemistryClassData == null) {
            chemistryClassData = new vs2ChemistryClassData();
        }
        // update to the current version
        serializedVersion = vs2Constants.VS2_VERSION;
        isChanged = true;
    }

    /**
     * Gets the usage of this document data.
     */
    public int getUsage() {
        return usage;
    }

    /**
     * Export the data in this document.
     */
    public int exportData(String directory, String file) {
        String mainInputFile =  vs2App.getFilePrefix() + ".dat";
        String mainOutputFile = vs2App.getFilePrefix() + ".out";
        if (file.equalsIgnoreCase(mainInputFile) ||
                file.equalsIgnoreCase(mainOutputFile) ||
                file.equalsIgnoreCase("file07.out") ||
                file.equalsIgnoreCase("variables.out") ||
                file.equalsIgnoreCase("balance.out") ||
                file.equalsIgnoreCase("obsPoints.out")) {
            return ILLEGAL_FILE_NAME_ERROR;
        }
        try {
            // Create an output stream for export
            File outFile = new File(directory, file);
            FileOutputStream fos = new FileOutputStream(outFile);
            PrintWriter pw = new PrintWriter(new BufferedOutputStream(fos));

            pw.println(mainInputFile);
            pw.println(mainOutputFile);
            pw.println("file07.out");
            pw.println("variables.out");
            pw.println("balance.out");
            pw.println("obsPoints.out");
            pw.println("# vs2drt1.1");
            pw.flush();
            fos.close();

            outFile = new File(directory, mainInputFile);
            fos = new FileOutputStream(outFile);
            pw = new PrintWriter(new BufferedOutputStream(fos));
            writeData(pw);
            pw.flush();
            fos.close();
            return 0;
        } catch(IOException ex) {
            return UNABLE_TO_EXPORT_ERROR;
        }
    }

    protected void writeData(PrintWriter pw) {
        int i, j;
        int numObservationCell =
                observationPointsData.getNumberOfObservationCells();
        double [] xCoord = gridData.getXCoords();
        double [] yCoord = gridData.getYCoords();
        int nmb9 = modelOptions.getNumberOfMassBalanceComponents(); // fluid + solute or energy
        boolean f8p = (modelOptions.outputTimeOption
                            != modelOptions.NO_OUTPUT_TIME);
        double maximumSimulationTime =
                    rechargePeriodData.getMaximumSimulationTime();

        // Card A-1
        pw.println(modelOptions.title);
        // Card A-2 (assume initial time = 0, and no grid rotation)
        pw.println(maximumSimulationTime + " 0. 0." + "    /A2 -- TMAX, STIM, ANG");
        // Card A-3
        pw.print(modelOptions.lengthUnit
                 + modelOptions.timeUnit);
        pw.print(modelOptions.massUnit);
        pw.print(modelOptions.energyUnit);
        pw.println("    /A3 -- ZUNIT, TUNIT, CUNX, HUNX");
        // Card A-5 (assumes grid data DO NOT include border cells
        pw.println((xCoord.length + 1) + " " + (yCoord.length + 1) +
                "    /A5 -- NXR, NLY");
        // Card A-6
        pw.print(rechargePeriodData.getNumberOfRows() + " ");
        if (modelOptions.highPrecisionAuxiliaryOutput) {
            pw.println(-modelOptions.maxNumberOfTimeSteps +  "    /A6 -- NRECH, NUMT");
        } else {
            pw.println(modelOptions.maxNumberOfTimeSteps +  "    /A6 -- NRECH, NUMT");
        }
        // Card A-7
        pw.println((modelOptions.useRadialCoord ? "T " : "F ")
                 + (modelOptions.itstop ? "T " : "F ")
                 + (modelOptions.doEnergyTransport ? "T " : "F ")
                 + (modelOptions.doSoluteTransport ? "T " : "F ")
                 + "       /A7 -- RAD, ITSTOP, HEAT, SOLUTE");
        if (modelOptions.doSoluteTransport) {
            // Card A-8 -- CHEMFILE
            pw.println(modelOptions.chemFile);
            // Card A-9 -- DATABASEFILE
            pw.println(modelOptions.databaseFile);
            // Card A-10 -- PREFIX
            pw.println(modelOptions.prefix);
        }

        if (modelOptions.doSoluteTransport || modelOptions.doEnergyTransport) {
            // Card A-11
            pw.print((modelOptions.doSpaceCentered ? "T " : "F ")
                     + (modelOptions.doTimeCentered ? "T " : "F "));
            pw.println("           /A11 -- CIS, CIT");
        }
        if (modelOptions.doSoluteTransport) {
            // Card A-12
            pw.print((modelOptions.phreeqcSelectedOutput ? "1" : "0"));
            pw.println("          /A12 -- INPRXZ");
        }
        
        // Card A-13.  The current implementation assumes if
        // a. F11P is "T" if user specified observation points in the
        //    observation points view
        // b. F7P is always "F", that is maximum head change in each
        //    iteration is not printed out
        // c. F8P is "T" if user specified output times in the "Output"
        //    tab of the model options dialog box.
        // d. F9P is "T" if user checked one or more mass balance
        //    components in the "Fluid balance" or "Solute balance"
        //    tab of the model options dialog box
        // e. F6P is "T" if user checked "at every time step" option
        //    in the "Output" tab of the model options dialog box.
        pw.println(((numObservationCell > 0) ? "T " : "F ")
                + "F "
                + (f8p ? "T " : "F ")
                + ((nmb9 > 0) ? "T " : "F ")
                + (modelOptions.outputMassBalanceEveryTimeStep ? "T" : "F")
                + "      /A13 -- F11P, F7P, F8P, F9P, F6P");
        // Card A-14.
        pw.println((modelOptions.moistureContentOut ? "T " : "F ")
                 + (modelOptions.saturationOut ? "T " : "F ")
                 + (modelOptions.pressureHeadOut ? "T " : "F ")
                 + (modelOptions.totalHeadOut ? "T " : "F ")
                 + (modelOptions.velocityOut ? "T " : "F ")
                 + "     /A14 -- THPT, SPNT, PPNT, HPNT, VPNT");
        // Card A-15 (Always set IFAC = 0, FACX = 1)
        pw.println("0 1" + "            /A15 -- IFAC, FACX. A16 begins next line: DXR");
        // Card A-16 (Grid spacing in x direction. Assumes grid
        // DOES NOT include border. Border cells are added to
        // the export file at  both ends of rows and columns.)
        pw.print((float) (xCoord[1] - xCoord[0]) + " ");       // Border cell
        for (i=0, j=1; i<xCoord.length-1; i++, j++) {
            if (j == 10) {
                pw.println();
                j = 0;
            }
            pw.print((float) (xCoord[i+1] - xCoord[i]) + " ");
        }
        if (j == 10) {
            pw.println();
        }
        pw.println((float) (xCoord[xCoord.length-1]
                          - xCoord[xCoord.length-2]));         // Border cell
        // No Card A-17
        // Card A-18 (Always set JFAC = 0, FACZ = 1)
        pw.println("0 1" + "            /A18 -- JFAC, FACZ. A20 begins next line: DELZ");
        // No Card A-19
        // Card A-20 (Grid spacing in y direction)
        pw.print((float) (yCoord[1] - yCoord[0]) + " ");       // Border cell
        for (i=0, j=1; i<yCoord.length-1; i++, j++) {
            if (j == 10) {
                pw.println();
                j = 0;
            }
            pw.print((float) (yCoord[i+1] - yCoord[i]) + " ");
        }
        if (j == 10) {
            pw.println();
        }
        pw.println((float) (yCoord[yCoord.length-1]
                          - yCoord[yCoord.length-2]));         // Border cell
        // No Card A-21
        // Card A-22 and A-23
        switch (modelOptions.outputTimeOption) {
        case vs2ModelOptions.INTERVAL_OUTPUT_TIME:
            int ts = Math.max (1, (int) (maximumSimulationTime / modelOptions.outputTimeInterval));
            //if (ts > MAX_OUTPUT_TIMES) ts = MAX_OUTPUT_TIMES;
            pw.println(ts + "     /A22 -- NPLT. A23 begins next line: PLTIM");
            double outputTime;
            for (i=1, j=0; i<=ts; i++, j++) {
                outputTime = i * modelOptions.outputTimeInterval;
                if (outputTime > maximumSimulationTime) {
                    outputTime = maximumSimulationTime;
                }
                if (j == 10) {
                    pw.println();
                    j = 0;
                }
                pw.print(((float) outputTime) + " ");
            }
            pw.println();
            break;
        case vs2ModelOptions.SPECIFIED_OUTPUT_TIMES:
            pw.println(modelOptions.outputTimes.size()
                    + "              /A22 -- NPLT. A23 begins next line: PLTIM");
            for (i=0, j=0; i<modelOptions.outputTimes.size(); i++, j++) {
                if (j == 10) {
                    pw.println();
                    j = 0;
                }
                pw.print((String) modelOptions.outputTimes.elementAt(i)
                         + " ");
            }
            pw.println();
        }
        if (numObservationCell > 0) {
            observationPointsData.exportData(pw, modelOptions.outputToAuxFilesEveryTimeStep);
        }
        // Cards A-26 and A-27
        if (nmb9 > 0) {
            // To output mass balance to File 9 at specified times, set nmb9 to negative.
            if (!modelOptions.outputToAuxFilesEveryTimeStep) {
                pw.print("-");
            }
            pw.println(nmb9 + "     /A26 -- NMB9");
            pw.println(modelOptions.getMassBalanceIndices() + "     /A27 -- MB9");  // fluid + solute or energy
        }

        // Card B-1
        pw.println(modelOptions.closureCriterionForHead + " "
                + modelOptions.relaxationParameter + " "
                + modelOptions.getIntercellWeightingValue() + "     /B1 -- EPS, HMAX, WUS");
        if (modelOptions.doEnergyTransport) {
            pw.println(modelOptions.closureCriterionForTemp + " "
                    +  modelOptions.closureCriterionForVelocity
                    + "     /B2 -- EPS1, EPS2");
        }
        if (modelOptions.doSoluteTransport) {
            pw.println(modelOptions.closureCriterionForConc
                    + "     /B3 -- EPS3");
        }
        // Card B-4
        pw.println(modelOptions.minIterationsPerTimeStep + " "
                 + modelOptions.maxIterationsPerTimeStep
                 + "     /B4 -- MINIT, ITMAX");
        // Card B-5  (initial flow conditions)
        pw.println(((modelOptions.initialFlowType ==
                    INITIAL_MOISTURE_CONTENT) ? "F" : "T")
                    + "     /B5 -- PHRD");
        // Card B-6
        pw.print(texturalClassData.getNumberOfRows() + " ");
        switch (modelOptions.soilModel) {
        case BROOKS_COREY: // fall through
        case ROSSI_NIMMO: // fall through
        case VAN_GENUCHTEN:
            pw.print("6 ");
            break;
        case HAVERKAMP:
            pw.print("8 ");
            break;
        case TABULAR_DATA:
            pw.print((3*texturalClassData.getMaxTabularDataRows()+6) + " ");
            break;
        }
        pw.println("     /B6 -- NTEX, NPROP");
        
        // Card B-7
        pw.println(modelOptions.soilModel + "     /B7 -- HFT hydraulicFunctionType");       

        // Cards B-6, B-7, B-7A, B-8, B-9
        // Cards B-8, B-9, B-9A, B-9B, B-10, B-11
        // B-6  -> B-8
        // B-7  -> B-9
        // B-7A -> B-9A and B-9B
        // B-8  -> B-10
        // B-9  -> B-11
        texturalClassData.exportData(pw, modelOptions);
        texturalMapData.exportData(pw, texturalClassData);

        // Cards B-8, B-9
        // Card B-10 is not used.
        // Card B-11 through 13 (initial flow conditions)
        switch (modelOptions.initialFlowType) {
        case INITIAL_EQUILIBRIUM_PROFILE:
            initialEquilibriumProfileData.exportData(pw, yCoord);
            break;
        case INITIAL_PRESSURE_HEAD:
            initialPressureHead.exportData(pw, "B13", "B15");
            break;
        case INITIAL_MOISTURE_CONTENT:
            initialMoistureContent.exportData(pw, "B13", "B15");
            break;
        }
        // Card B-16
        boolean doEvaporation = rechargePeriodData.isEvaporationSimulated()
                                && modelOptions.doEvaporation;
        boolean doTranspiration = rechargePeriodData.isTranspirationSimulated()
                                && modelOptions.doTranspiration;
        pw.println((doEvaporation ? "T " : "F ")
                 + (doTranspiration ? "T " : "F ")
                 + "     /B16 -- BCIT, ETSIM");
        // Card B-17 through B-25
        if (doEvaporation || doTranspiration) {
            evapotranspirationData.exportData(pw, doEvaporation,
                                                  doTranspiration);
        }
        // Cards B-26 and B-27
        if (modelOptions.doEnergyTransport) {
            initialTemperatureData.exportData(pw, "B26", "B27");
        }
        // Cards B-28, B-29 and B-30 (REPLACED BY TEXTURAL CLASS)
        if (modelOptions.doSoluteTransport) {
            chemistryMapData.exportData(pw, chemistryClassData);
        }
        
        // Since F7P is always "F"
        // B-29?? through B-31?? are not used

        // All Cards C
        for (i=0; i<rechargePeriodData.getNumberOfRows(); i++) {
            rechargePeriodData.exportPeriod(pw, i, modelOptions);      // no changes for C-1 through C-6 (VS2DRT)
            boundaryConditionsData.exportPeriod(pw, i, modelOptions);
            fluidSourceData.exportPeriod(pw, i, modelOptions);
            pw.println("-999999 / C19 -- End of data for recharge period " + (i+1));
        }
        pw.println("-999999 / End of input data file");
    }

    /**
     * Initializes the document. This method initializes
     * transient variables in the data objects held in
     * the document.
     *
     * @param  theApp  the application object to which this
     *                 document belongs.
     */
    public void init(mp2App theApp) {
        super.init(theApp);

        texturalClassData.init(this);
        chemistryClassData.init(this);
        evapotranspirationData.init(this);
        rechargePeriodData.init(this);
        domainData.init(this);
        texturalMapData.init(this);
        chemistryMapData.init(this);
        initialEquilibriumProfileData.init(this);
        initialPressureHead.init(this);
        initialMoistureContent.init(this);
        initialMoistureContent.setNullValue(.1);
        initialConcentrationData.init(this);
        initialTemperatureData.init(this);
        boundaryConditionsData.init(this);
        fluidSourceData.init(this);
        observationPointsData.init(this);
        gridData.init(this);
        gridData.setUseRadialCoord(modelOptions.useRadialCoord);
        gridData.setDomainData(domainData);
        siteMapData.init(this);

        domainData.setGridData(gridData);
        texturalMapData.setGridData(gridData);
        chemistryMapData.setGridData(gridData);
        initialPressureHead.setGridData(gridData);
        initialMoistureContent.setGridData(gridData);
        initialConcentrationData.setGridData(gridData);
        initialTemperatureData.setGridData(gridData);
        boundaryConditionsData.setGridData(gridData);
        fluidSourceData.setGridData(gridData);
        observationPointsData.setGridData(gridData);

        boundaryConditionsData.setDomainData(domainData);
        fluidSourceData.setSimulationPeriodData(rechargePeriodData);
        postProcessorOptions.init();
        theApp.getFrame().getMenuItem(BUFFERED_GRAPHICS).setSelected(
                                    modelOptions.imageBufferingEnabled);
    }

    /**
     * Displays a tabbed dialog box for user to edit model options
     */
    public void editModelOptions() {

        // Create the model options dialog dox
        vs2ModelOptionsDialog dlg = new vs2ModelOptionsDialog();

        // Sent current model options from document to dialog
        dlg.basicPanel.title = modelOptions.title;
        dlg.basicPanel.lengthUnit = modelOptions.lengthUnit;
        dlg.basicPanel.timeUnit = modelOptions.timeUnit;
        dlg.basicPanel.energyUnit = modelOptions.energyUnit;
        dlg.basicPanel.massUnit = modelOptions.massUnit;
        dlg.basicPanel.useRadialCoord = modelOptions.useRadialCoord;
        dlg.basicPanel.doEnergyTransport = modelOptions.doEnergyTransport;
        dlg.basicPanel.doSoluteTransport = modelOptions.doSoluteTransport;
        dlg.basicPanel.doEvaporation = modelOptions.doEvaporation;
        dlg.basicPanel.doTranspiration = modelOptions.doTranspiration;
        dlg.basicPanel.radialCoordinatesEnabled = true;
        dlg.basicPanel.unitChangeWarningEnabled = (!isNewDoc);
        for (int i=0; i<domainData.getNumberOfShapes(); i++) {
            mp2RectBounds bounds = domainData.getShape(i).getBounds();
            if (bounds.x < 0) {
                dlg.basicPanel.radialCoordinatesEnabled = false;
                break;
            }
        }
        if (gridData.isDefined() && gridData.getXCoordAt(0) < 0) {
            dlg.basicPanel.radialCoordinatesEnabled = false;
        }
        dlg.flowPanel.weightingOption = modelOptions.weightingOption;
        dlg.flowPanel.soilModel = modelOptions.soilModel;
        dlg.flowPanel.initialFlowType = modelOptions.initialFlowType;
        // transport panel
        dlg.transportPanel.doSpaceCentered = modelOptions.doSpaceCentered;
        dlg.transportPanel.doTimeCentered  = modelOptions.doTimeCentered;
        
        // new for Version 1.4
        dlg.transportPanel.chemFile       = modelOptions.chemFile;
        dlg.transportPanel.databaseFile   = modelOptions.databaseFile;
        dlg.transportPanel.prefix         = modelOptions.prefix;
        
        // solver panel
        dlg.solverPanel.hmax = modelOptions.relaxationParameter;
        dlg.solverPanel.minit = modelOptions.minIterationsPerTimeStep;
        dlg.solverPanel.itmax = modelOptions.maxIterationsPerTimeStep;
        dlg.solverPanel.maxStep = modelOptions.maxNumberOfTimeSteps;
        dlg.solverPanel.eps = modelOptions.closureCriterionForHead;
        dlg.solverPanel.eps1 = modelOptions.closureCriterionForTemp;
        dlg.solverPanel.eps2 = modelOptions.closureCriterionForVelocity;
        dlg.solverPanel.eps3 = modelOptions.closureCriterionForConc;   // eps3 is new for Version 1.4
        dlg.solverPanel.itstop = modelOptions.itstop;

        for (int i=0; i<modelOptions.outputTimes.size(); i++) {
            dlg.outputPanel.listModel.addElement(
                        modelOptions.outputTimes.elementAt(i));
        }
        dlg.outputPanel.moistureContentOut =
                            modelOptions.moistureContentOut;
        dlg.outputPanel.saturationOut =
                            modelOptions.saturationOut;
        dlg.outputPanel.pressureHeadOut =
                            modelOptions.pressureHeadOut;
        dlg.outputPanel.totalHeadOut =
                            modelOptions.totalHeadOut;
        dlg.outputPanel.velocityOut =
                            modelOptions.velocityOut;
        dlg.outputPanel.outputMassBalanceEveryTimeStep =
                            modelOptions.outputMassBalanceEveryTimeStep;
        dlg.outputPanel.outputToAuxFilesEveryTimeStep =
                            modelOptions.outputToAuxFilesEveryTimeStep;
        dlg.outputPanel.highPrecisionAuxiliaryOutput =
                            modelOptions.highPrecisionAuxiliaryOutput;
        dlg.outputPanel.outputTimeOption =
                            modelOptions.outputTimeOption;
        dlg.outputPanel.outputTimeInterval =
                            modelOptions.outputTimeInterval;
        dlg.outputPanel.phreeqcSelectedOutput = 
                modelOptions.phreeqcSelectedOutput;  // new for Version 1.4
        dlg.fluidBalancePanel.inFlowSpecifiedHead =
                            modelOptions.inFlowSpecifiedHead;
        dlg.fluidBalancePanel.outFlowSpecifiedHead =
                            modelOptions.outFlowSpecifiedHead;
        dlg.fluidBalancePanel.inFlowSpecifiedFlux =
                            modelOptions.inFlowSpecifiedFlux;
        dlg.fluidBalancePanel.outFlowSpecifiedFlux =
                            modelOptions.outFlowSpecifiedFlux;
        dlg.fluidBalancePanel.inFlowTotal =
                            modelOptions.inFlowTotal;
        dlg.fluidBalancePanel.outFlowTotal =
                            modelOptions.outFlowTotal;
        dlg.fluidBalancePanel.evaporation =
                            modelOptions.evaporation;
        dlg.fluidBalancePanel.transpiration =
                            modelOptions.transpiration;
        dlg.fluidBalancePanel.evapoTranspiration =
                            modelOptions.evapoTranspiration;
        dlg.fluidBalancePanel.changeInStorage =
                            modelOptions.changeInStorage;
        dlg.fluidBalancePanel.fluidBalance =
                            modelOptions.fluidBalance;
        
        dlg.balancePanel.inEnergySpecifiedHead =
                            modelOptions.inEnergySpecifiedHead;
        dlg.balancePanel.outEnergySpecifiedHead =
                            modelOptions.outEnergySpecifiedHead;
        dlg.balancePanel.inEnergySpecifiedFlux =
                            modelOptions.inEnergySpecifiedFlux;
        dlg.balancePanel.outEnergySpecifiedFlux =
                            modelOptions.outEnergySpecifiedFlux;
        dlg.balancePanel.inEnergyDispersion =
                            modelOptions.inEnergyDispersion;
        dlg.balancePanel.outEnergyDispersion =
                            modelOptions.outEnergyDispersion;
        dlg.balancePanel.inEnergyTotal =
                            modelOptions.inEnergyTotal;
        dlg.balancePanel.outEnergyTotal =
                            modelOptions.outEnergyTotal;
        dlg.balancePanel.outEnergyEvapoTranspiration =
                            modelOptions.outEnergyEvapoTranspiration;
        dlg.balancePanel.changeInEnergyStorage =
                            modelOptions.changeInEnergyStorage;
        dlg.balancePanel.energyBalance =
                            modelOptions.energyBalance;


        if (dlg.doModal() == true) {

            modelOptions.title = dlg.basicPanel.title;
            if (!isNewDoc &&
                    (!modelOptions.lengthUnit.equals(dlg.basicPanel.lengthUnit)
                     || !modelOptions.timeUnit.equals(dlg.basicPanel.timeUnit))) {
                int r = ((vs2View) view).getTexturalClassWindow().getSelectedRow();
                texturalClassData.convertGenericProperties();
                ((vs2View) view).getTexturalClassWindow().selectRow(r);
            }
            modelOptions.lengthUnit = dlg.basicPanel.lengthUnit;
            modelOptions.timeUnit = dlg.basicPanel.timeUnit;
            modelOptions.energyUnit = dlg.basicPanel.energyUnit;
            modelOptions.massUnit = dlg.basicPanel.massUnit;
            modelOptions.useRadialCoord = dlg.basicPanel.useRadialCoord;
            modelOptions.doEvaporation = dlg.basicPanel.doEvaporation;
            modelOptions.doTranspiration = dlg.basicPanel.doTranspiration;
            modelOptions.doEnergyTransport = dlg.basicPanel.doEnergyTransport;
            modelOptions.doSoluteTransport = dlg.basicPanel.doSoluteTransport;

            modelOptions.weightingOption = dlg.flowPanel.weightingOption;
            modelOptions.soilModel = dlg.flowPanel.soilModel;
            modelOptions.initialFlowType = dlg.flowPanel.initialFlowType;

            modelOptions.relaxationParameter =
                                    dlg.solverPanel.hmax;
            modelOptions.minIterationsPerTimeStep =
                                    dlg.solverPanel.minit;
            modelOptions.maxIterationsPerTimeStep =
                                    dlg.solverPanel.itmax;
            modelOptions.maxNumberOfTimeSteps =
                                    dlg.solverPanel.maxStep;
            modelOptions.closureCriterionForHead =
                                    dlg.solverPanel.eps;
            modelOptions.itstop = dlg.solverPanel.itstop;

            modelOptions.moistureContentOut = dlg.outputPanel.moistureContentOut;
            modelOptions.saturationOut = dlg.outputPanel.saturationOut;
            modelOptions.pressureHeadOut = dlg.outputPanel.pressureHeadOut;
            modelOptions.totalHeadOut = dlg.outputPanel.totalHeadOut;
            modelOptions.velocityOut = dlg.outputPanel.velocityOut;
            modelOptions.outputMassBalanceEveryTimeStep =
                            dlg.outputPanel.outputMassBalanceEveryTimeStep;
            modelOptions.outputToAuxFilesEveryTimeStep =
                            dlg.outputPanel.outputToAuxFilesEveryTimeStep;
            modelOptions.highPrecisionAuxiliaryOutput =
                            dlg.outputPanel.highPrecisionAuxiliaryOutput;
            modelOptions.outputTimeOption =
                            dlg.outputPanel.outputTimeOption;
            switch (modelOptions.outputTimeOption) {
            case vs2ModelOptions.INTERVAL_OUTPUT_TIME:
                modelOptions.outputTimeInterval =
                                dlg.outputPanel.outputTimeInterval;
                break;
            case vs2ModelOptions.SPECIFIED_OUTPUT_TIMES:
                modelOptions.outputTimes.removeAllElements();
                for (int i=0; i<dlg.outputPanel.listModel.getSize(); i++) {
                    modelOptions.outputTimes.addElement(dlg.outputPanel.listModel.get(i));
                }
                break;
            }

            modelOptions.inFlowSpecifiedHead = dlg.fluidBalancePanel.inFlowSpecifiedHead;
            modelOptions.outFlowSpecifiedHead = dlg.fluidBalancePanel.outFlowSpecifiedHead;
            modelOptions.inFlowSpecifiedFlux = dlg.fluidBalancePanel.inFlowSpecifiedFlux;
            modelOptions.outFlowSpecifiedFlux = dlg.fluidBalancePanel.outFlowSpecifiedFlux;
            modelOptions.inFlowTotal = dlg.fluidBalancePanel.inFlowTotal;
            modelOptions.outFlowTotal = dlg.fluidBalancePanel.outFlowTotal;
            modelOptions.evaporation = dlg.fluidBalancePanel.evaporation;
            modelOptions.transpiration = dlg.fluidBalancePanel.transpiration;
            modelOptions.evapoTranspiration = dlg.fluidBalancePanel.evapoTranspiration;
            modelOptions.changeInStorage = dlg.fluidBalancePanel.changeInStorage;
            modelOptions.fluidBalance = dlg.fluidBalancePanel.fluidBalance;

            if (modelOptions.doEnergyTransport || modelOptions.doSoluteTransport) {
                modelOptions.doSpaceCentered             = dlg.transportPanel.doSpaceCentered;
                modelOptions.doTimeCentered              = dlg.transportPanel.doTimeCentered;
                if (modelOptions.doEnergyTransport) {
                    modelOptions.closureCriterionForTemp     = dlg.solverPanel.eps1;
                    modelOptions.closureCriterionForVelocity = dlg.solverPanel.eps2;
                }
                if (modelOptions.doSoluteTransport) {
                    modelOptions.chemFile                    = dlg.transportPanel.chemFile;
                    modelOptions.databaseFile                = dlg.transportPanel.databaseFile;
                    modelOptions.prefix                      = dlg.transportPanel.prefix;
                    
                    modelOptions.phreeqcSelectedOutput       = dlg.outputPanel.phreeqcSelectedOutput;
                    
                    modelOptions.closureCriterionForConc     = dlg.solverPanel.eps3;
                }
                

                modelOptions.inEnergySpecifiedHead =
                            dlg.balancePanel.inEnergySpecifiedHead;
                modelOptions.outEnergySpecifiedHead =
                            dlg.balancePanel.outEnergySpecifiedHead;
                modelOptions.inEnergySpecifiedFlux =
                            dlg.balancePanel.inEnergySpecifiedFlux;
                modelOptions.outEnergySpecifiedFlux =
                            dlg.balancePanel.outEnergySpecifiedFlux;
                modelOptions.inEnergyDispersion =
                            dlg.balancePanel.inEnergyDispersion;
                modelOptions.outEnergyDispersion =
                            dlg.balancePanel.outEnergyDispersion;
                modelOptions.inEnergyTotal =
                            dlg.balancePanel.inEnergyTotal;
                modelOptions.outEnergyTotal =
                            dlg.balancePanel.outEnergyTotal;
                modelOptions.outEnergyEvapoTranspiration =
                            dlg.balancePanel.outEnergyEvapoTranspiration;
                modelOptions.changeInEnergyStorage =
                            dlg.balancePanel.changeInEnergyStorage;
                modelOptions.energyBalance =
                            dlg.balancePanel.energyBalance;
            }

            vs2FrameManager frameManager =
                        (vs2FrameManager) theApp.getFrame().getManager();
            frameManager.updateDataChooserAndShowMenu(modelOptions);
            ((vs2View) view).UpdateTableWindows(modelOptions);
            ((vs2View) view).setUseRadialCoordinates(modelOptions.useRadialCoord);
            gridData.setUseRadialCoord(modelOptions.useRadialCoord);
            view.repaint();

            // Mark the document as changed
            setChanged(true);
            view.noUndoableDataChanged();
        }
    }

    /**
     * Gets the current version of this document
     */
    public static String getCurrentVersion() {
        return vs2Constants.VS2_VERSION;
    }

    /**
     * Gets the specified data held in this document.
     *
     * @param  dataId    the specified data identified by
     *                   an <code>int</code> constant as defined
     *                   in the <code>mp2Constants</code> and
     *                   <code>vs2Constants</code> interfaces.
     *
     * @return  the requested data cast to the type
     *          <code>java.lang.Object</code>.
     *
     * @see  mp2.mp2Constants, vs2.vs2Constants
     */
    public Object getData(int dataId) {
        switch (dataId) {
        case MODEL_OPTIONS:
            return modelOptions;
        case TEXTURAL_CLASS:
            return texturalClassData;
        case CHEMISTRY_CLASS:
            return chemistryClassData;
        case EVAPOTRANSPIRATION:
            return evapotranspirationData;
        case RECHARGE_PERIOD:
            return rechargePeriodData;
        case DOMAIN:
            return domainData;
        case TEXTURAL_MAP:
            return texturalMapData;
        case CHEMISTRY_MAP:
            return chemistryMapData;
        case INITIAL_EQUILIBRIUM_PROFILE:
            return initialEquilibriumProfileData;
        case INITIAL_PRESSURE_HEAD:
            return initialPressureHead;
        case INITIAL_MOISTURE_CONTENT:
            return initialMoistureContent;
        case INITIAL_TEMPERATURE:
            return initialTemperatureData;
        case BOUNDARY_CONDITIONS:
            return boundaryConditionsData;
        case FLUID_SOURCE:
            return fluidSourceData;
        case OBSERVATION_POINTS:
            return observationPointsData;
        case MODEL_GRID:
            return gridData;
        case SITE_MAP:
            return siteMapData;
        case POST_PROCESSOR_OPTIONS:
            return postProcessorOptions;
        default:
            return null;
        }
    }

    /**
     * Indicates if the data in this document is sufficient for
     * exporting
     */
    public boolean readyToExport() {
        boolean isReady = true;
        Vector missingData = new Vector();
        if (domainData.getBoundary(0) == null) {
            missingData.addElement("Domain");
            isReady = false;
        }

        if (texturalClassData.getNumberOfRows() == 1) {
            missingData.addElement("Textural Class");
            isReady = false;
        }

        if (texturalMapData.getNumberOfShapes() == 0) {
            missingData.addElement("Textural Map");
            isReady = false;
        }
        
        if (modelOptions.doSoluteTransport) {
            if (chemistryClassData.getNumberOfRows() == 1) {
                missingData.addElement("Chemistry Class");
                isReady = false;
            }
            
            if (chemistryMapData.getNumberOfShapes() == 0) {
                missingData.addElement("Chemistry Map");
                isReady = false;
            }
        }

        switch (modelOptions.initialFlowType) {
        case INITIAL_EQUILIBRIUM_PROFILE:
            if (initialEquilibriumProfileData.getWaterTableLocation() ==
                        Double.NEGATIVE_INFINITY
                        || initialEquilibriumProfileData.getMinimumPressureHead() ==
                        Double.NEGATIVE_INFINITY) {
                missingData.addElement("Initial equilibrium profile");
                isReady = false;
            }
            break;
        case INITIAL_PRESSURE_HEAD:
            if (initialPressureHead.getNumberOfShapes() == 0) {
                missingData.addElement("Initial pressure head");
                isReady = false;
            }
            break;
        case INITIAL_MOISTURE_CONTENT:
            if (initialMoistureContent.getNumberOfShapes() == 0) {
                missingData.addElement("Initial moisture content");
                isReady = false;
            }
            break;
        }

        if (modelOptions.doEnergyTransport) {
            if (initialTemperatureData.getNumberOfShapes() == 0) {
                missingData.addElement("Initial temperature");
                isReady = false;
            }
        }

        if (rechargePeriodData.getNumberOfRows() == 0) {
            missingData.addElement("Recharge period");
            isReady = false;
        }

        if ((modelOptions.doTranspiration || modelOptions.doEvaporation)
            && evapotranspirationData.getNumberOfRows() == 0) {
            missingData.addElement("Evapotranspiration data");
            isReady = false;
        }

        if (!gridData.isDefined()) {
            missingData.addElement("Grid");
            isReady = false;
        }

        if (isReady == false) {
            mp2MissingDataDialog dlg = new mp2MissingDataDialog(theApp.getFrame(),
                                                            missingData);
            dlg.setVisible(true);
        }

        return isReady;
    }

    public void setChanged(boolean b) {
        super.setChanged(b);
        isNewDoc = false;
    }

    /**
     * Determine if nonlinear sorption is being simulated
     */
    protected boolean doNonlinearSorption() {
        // Rick should check to make sure this is
        // correctly determined.

        if (!modelOptions.doTransport) {
            return false;
        }

        // Determine if ion exchange is simulated.
        boolean doIonExchange =
               ((modelOptions.reactionOption == MONO_MONOVALENT_ION_EXCHANGE)
             || (modelOptions.reactionOption == MONO_DIVALENT_ION_EXCHANGE)
             || (modelOptions.reactionOption == DI_MONOVALENT_ION_EXCHANGE)
             || (modelOptions.reactionOption == DI_DIVALENT_ION_EXCHANGE));
        // If ion exchange is simulation, return true. No further
        // checking is needed.
        if (doIonExchange) {
            return true;
        }

        // If Langmuir isotherm is used, return true;
        if (modelOptions.reactionOption == LANGMUIR) {
            return true;
        }

        // If Freundlich isotherm is used and is nonlinear, return true
        if (modelOptions.reactionOption == FREUNDLICH
            && texturalClassData.isFreundlichNonlinear()) {
            return true;
        }

        // for all other cases, return false.
        return false;
    }
    
    private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException {
        // unserialize default
        in.defaultReadObject();
        
        // see revision 11409 for overrides
        if (chemistryClassData == null) {
            chemistryClassData = new vs2ChemistryClassData();
        }
        if (chemistryMapData == null) {
            chemistryMapData = new vs2ChemistryMapData();
        }
    }
    
    private void writeObject(java.io.ObjectOutputStream out) throws IOException {
        // Can't use assert here (the file will get corrupted)
        if (this.usage != SOLUTE_AND_ENERGY_TRANSPORT) {
            // display only if assert is enabled (-ea)
            boolean assertsEnabled = false;
            assert assertsEnabled = true; // Intentional side-effect
            if (assertsEnabled) {
                javax.swing.JOptionPane.showMessageDialog(null, 
                        "this.usage != SOLUTE_AND_ENERGY_TRANSPORT");                
            }
        }
        out.defaultWriteObject();
    }
}
