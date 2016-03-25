/*
 * vs2ModelOptions.java
 */
package vs2;

import java.io.*;
import java.util.*;

/**
 * Encapsulates VS2DT model options
 */
public class vs2ModelOptions implements vs2Constants, Serializable {

    static final long serialVersionUID = 2199392520680548373L;

    // basic options
    public String title;
    public String lengthUnit;
    public String timeUnit;
    public String massUnit;
    public String energyUnit;
    public boolean useRadialCoord;
    public boolean doTransport;
    public boolean doEvaporation;
    public boolean doTranspiration;

    // flow options
    public int weightingOption;
    public int soilModel;
    public int initialFlowType;

    // transport options
    public boolean doSpaceCentered;
    public boolean doTimeCentered;
    public int reactionOption;

    // solver options
    public double relaxationParameter;
    public int minIterationsPerTimeStep;
    public int maxIterationsPerTimeStep;
    public double closureCriterionForHead;
    public double closureCriterionForConc;
    public double closureCriterionForTemp;
    public double closureCriterionForVelocity;
    public boolean itstop;
    public int maxNumberOfTimeSteps;

    // output options
    public boolean moistureContentOut;
    public boolean pressureHeadOut;
    public boolean velocityOut;
    public boolean saturationOut;
    public boolean totalHeadOut;
    public boolean outputMassBalanceEveryTimeStep;
    public int outputTimeOption;
    public Vector outputTimes;
    public double outputTimeInterval;
    public boolean outputToAuxFilesEveryTimeStep;    // new in Version 1.2
    public boolean highPrecisionAuxiliaryOutput;    // new in Version 1.2

    // fluid balance options
    public boolean inFlowSpecifiedHead;
    public boolean outFlowSpecifiedHead;
    public boolean inFlowSpecifiedFlux;
    public boolean outFlowSpecifiedFlux;
    public boolean inFlowTotal;
    public boolean outFlowTotal;
    public boolean evaporation;
    public boolean transpiration;
    public boolean evapoTranspiration;
    public boolean changeInStorage;
    public boolean fluidBalance;

    // solute balance options
    public boolean inSoluteSpecifiedHead;
    public boolean outSoluteSpecifiedHead;
    public boolean inSoluteSpecifiedFlux;
    public boolean outSoluteSpecifiedFlux;
    public boolean inSoluteDispersion;
    public boolean outSoluteDispersion;
    public boolean inSoluteTotal;
    public boolean outSoluteTotal;
    public boolean outSoluteEvapoTranspiration;
    public boolean outSoluteDecay;
    public boolean outSoluteAdsorption;
    public boolean changeInSoluteStorage;
    public boolean soluteBalance;
    
    // energy balance options
    public boolean inEnergySpecifiedHead;
    public boolean outEnergySpecifiedHead;
    public boolean inEnergySpecifiedFlux;
    public boolean outEnergySpecifiedFlux;
    public boolean inEnergyDispersion;
    public boolean outEnergyDispersion;
    public boolean inEnergyTotal;
    public boolean outEnergyTotal;
    public boolean outEnergyEvapoTranspiration;
    public boolean changeInEnergyStorage;
    public boolean energyBalance;
    
    public boolean imageBufferingEnabled;

    public final static int NO_OUTPUT_TIME = 0;
    public final static int INTERVAL_OUTPUT_TIME = 1;
    public final static int SPECIFIED_OUTPUT_TIMES = 2;
    

    public vs2ModelOptions() {
        // Default values for basic options
        title = new String();
        lengthUnit = "m   ";
        timeUnit = "sec ";
        massUnit = "g   ";
        energyUnit = "J   ";

        useRadialCoord = false;
        doTransport = true;
        doEvaporation = false;
        doTranspiration = false;

        // Default values for flow options
        initialFlowType = INITIAL_EQUILIBRIUM_PROFILE;
        weightingOption = ARITHMETIC_MEAN_WEIGHTING;
        soilModel = DEFAULT_SOIL_MODEL;

        // Default values for transport options
        doSpaceCentered = true;
        doTimeCentered = true;
        reactionOption = N0_ADSORPTION_NO_ION_EXCHANGE;

        // Default values for solver options
        relaxationParameter = 0.7;
        minIterationsPerTimeStep = 2;
        maxIterationsPerTimeStep = 80;
        closureCriterionForHead = .001;
        closureCriterionForConc = .001;
        closureCriterionForTemp = .001; // new in Verion 1.1
        closureCriterionForVelocity = .001; // new in Version 1.1
        maxNumberOfTimeSteps = 1000;
        itstop = true;

        // Default values for output options
        moistureContentOut = false;
        saturationOut = false;
        pressureHeadOut = true;
        totalHeadOut = false;
        velocityOut = false;
        outputMassBalanceEveryTimeStep = false;
        outputToAuxFilesEveryTimeStep = false;  // new in Version 1.2
        highPrecisionAuxiliaryOutput = false;    // new in Version 1.2

        outputTimeOption = NO_OUTPUT_TIME;
        outputTimeInterval = 0;
        outputTimes = new Vector();
    
        // fluid balance options
        inFlowSpecifiedHead = false;
        outFlowSpecifiedHead = false;
        inFlowSpecifiedFlux = false;
        outFlowSpecifiedFlux = false;
        inFlowTotal = false;
        outFlowTotal = false;
        evaporation = false;
        transpiration = false;
        evapoTranspiration = false;
        changeInStorage = false;
        fluidBalance = false;

        // solute balance options
        inSoluteSpecifiedHead = false;
        outSoluteSpecifiedHead = false;
        inSoluteSpecifiedFlux = false;
        outSoluteSpecifiedFlux = false;
        inSoluteDispersion = false;
        outSoluteDispersion = false;
        inSoluteTotal = false;
        outSoluteTotal = false;
        outSoluteEvapoTranspiration = false;
        outSoluteDecay = false;
        outSoluteAdsorption = false;
        changeInSoluteStorage = false;
        soluteBalance = false;

        // energy balance options -- new in Version 1.1
        inEnergySpecifiedHead = false;
        outEnergySpecifiedHead = false;
        inEnergySpecifiedFlux = false;
        outEnergySpecifiedFlux = false;
        inEnergyDispersion = false;
        outEnergyDispersion = false;
        inEnergyTotal = false;
        outEnergyTotal = false;
        outEnergyEvapoTranspiration = false;
        changeInEnergyStorage = false;
        energyBalance = false; 
    }
    
    public void convertToCurrentVersion() {
        if (closureCriterionForTemp == 0) {
            closureCriterionForTemp = .001;
        }
        if (closureCriterionForVelocity == 0) {
            closureCriterionForVelocity = .001;
        }
        if (energyUnit == null) {
            energyUnit = "J   ";
        }
    }

    public double getIntercellWeightingValue() {
        switch (weightingOption) {
        case ARITHMETIC_MEAN_WEIGHTING:
            return 0.5;
        case GEOMETRIC_MEAN_WEIGHTING:
            return 0.0;
        case UPSTREAM_WEIGHTING:
            return 1.0;
        default:
            return 0.5;
        }
    }

    public boolean useStandardUnits() {
        if ((lengthUnit.equals("mm  ") 
                || lengthUnit.equals("cm  ")
                || lengthUnit.equals("m   ")
                || lengthUnit.equals("ft  "))
            &&(timeUnit.equals("sec ")
                || timeUnit.equals("hour")
                || timeUnit.equals("day ")
                || timeUnit.equals("year"))) {
            return true;
        } else {
            return false;
        }
    }

    public int getNumberOfMassBalanceComponents() {

        int n = 0;

        if (inFlowSpecifiedHead) {n++;}
        if (outFlowSpecifiedHead) {n++;}
        if (inFlowSpecifiedFlux) {n++;}
        if (outFlowSpecifiedFlux) {n++;}
        if (inFlowTotal) {n++;}
        if (outFlowTotal) {n++;}
        if (evaporation) {n++;}
        if (transpiration) {n++;}
        if (evapoTranspiration) {n++;}
        if (changeInStorage) {n++;}
        if (fluidBalance) {n++;}

        if (!doTransport) {
            return 3*n;
        }

        if (vs2App.doHeat()) {
            // energy balance options
            if (inEnergySpecifiedHead) {n++;}
            if (outEnergySpecifiedHead) {n++;}
            if (inEnergySpecifiedFlux) {n++;}
            if (outEnergySpecifiedFlux) {n++;}
            if (inEnergyDispersion) {n++;}
            if (outEnergyDispersion) {n++;}
            if (inEnergyTotal) {n++;}
            if (outEnergyTotal) {n++;}
            if (outEnergyEvapoTranspiration) {n++;}
            if (changeInEnergyStorage) {n++;}
            if (energyBalance) {n++;}
        } else {
            // solute balance options
            if (inSoluteSpecifiedHead) {n++;}
            if (outSoluteSpecifiedHead) {n++;}
            if (inSoluteSpecifiedFlux) {n++;}
            if (outSoluteSpecifiedFlux) {n++;}
            if (inSoluteDispersion) {n++;}
            if (outSoluteDispersion) {n++;}
            if (inSoluteTotal) {n++;}
            if (outSoluteTotal) {n++;}
            if (outSoluteEvapoTranspiration) {n++;}
            if (outSoluteDecay) {n++;}
            if (outSoluteAdsorption) {n++;}
            if (changeInSoluteStorage) {n++;}
            if (soluteBalance) {n++;}
        }

        return 3*n;
    }

    public String getMassBalanceIndices() {

        StringBuffer line = new StringBuffer();

        if (inFlowSpecifiedHead) {line.append("1 2 3 ");}
        if (outFlowSpecifiedHead) {line.append("4 5 6 ");}
        if (inFlowSpecifiedFlux) {line.append("7 8 9 ");}
        if (outFlowSpecifiedFlux) {line.append("10 11 12 ");}
        if (inFlowTotal) {line.append("13 14 15 ");}
        if (outFlowTotal) {line.append("16 17 18 ");}
        if (evaporation) {line.append("19 20 21 ");}
        if (transpiration) {line.append("22 23 24 ");}
        if (evapoTranspiration) {line.append("25 26 27 ");}
        if (changeInStorage) {line.append("28 29 30 ");}
        if (fluidBalance) {line.append("31 32 33 ");}

        if (!doTransport) {
            return line.toString();
        }

        if (vs2App.doHeat()) {
            if (inEnergySpecifiedHead) {line.append("34 35 36 ");}
            if (outEnergySpecifiedHead) {line.append("37 38 39 ");}
            if (inEnergySpecifiedFlux) {line.append("40 41 42 ");}
            if (outEnergySpecifiedFlux) {line.append("43 44 45 ");}
            if (inEnergyDispersion) {line.append("46 47 48 ");}
            if (outEnergyDispersion) {line.append("49 50 51 ");}
            if (inEnergyTotal) {line.append("52 53 54 ");}
            if (outEnergyTotal) {line.append("55 56 57 ");}
            if (outEnergyEvapoTranspiration) {line.append("58 59 60 ");}
            if (changeInEnergyStorage) {line.append("67 68 69 ");}
            if (energyBalance) {line.append("70 71 72 ");}
        } else {
            if (inSoluteSpecifiedHead) {line.append("34 35 36 ");}
            if (outSoluteSpecifiedHead) {line.append("37 38 39 ");}
            if (inSoluteSpecifiedFlux) {line.append("40 41 42 ");}
            if (outSoluteSpecifiedFlux) {line.append("43 44 45 ");}
            if (inSoluteDispersion) {line.append("46 47 48 ");}
            if (outSoluteDispersion) {line.append("49 50 51 ");}
            if (inSoluteTotal) {line.append("52 53 54 ");}
            if (outSoluteTotal) {line.append("55 56 57 ");}
            if (outSoluteEvapoTranspiration) {line.append("58 59 60 ");}
            if (outSoluteDecay) {line.append("61 62 63 ");}
            if (outSoluteAdsorption) {line.append("64 65 66 ");}
            if (changeInSoluteStorage) {line.append("67 68 69 ");}
            if (soluteBalance) {line.append("70 71 72 ");}
        }

        return line.toString();
    }

}