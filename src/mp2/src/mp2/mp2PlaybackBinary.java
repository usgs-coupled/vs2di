/*
 * mp2PlaybackBinary.java
 */
package mp2;

import java.io.*;

public abstract class mp2PlaybackBinary extends mp2Model {

    protected DataInputStream bin;
    protected int timeStep;
    protected float modelTime;

    // implementations of abstract methods in superclass
    
    public int getType() {
        return PLAYBACK_BINARY;
    }

    public boolean setup(String dataFile, boolean restart) {
        int i;
        try {
            bin = new DataInputStream(new BufferedInputStream(
                                    new FileInputStream(new File(dataFile))));
            if (!readSimulationInfo(restart)) {
                return false;
            }
            readData();
        }
        catch(IOException e) {
            return false;
        }
        return true;
    }

    public int step() {
        try {
            readData();
            if (bin.available() == 0) {
                return 1;
            } else {
                return 0;
            }
        }
        catch(IOException e) {
            return 99;
        }
    }

    public void closeIO() {
        try {
            if (bin != null) {
                bin.close();
                bin = null;
            }
        }
        catch (IOException e) {}
    }
    
    // Get methods for ivars of this class

    public float getModelTime() {
        return modelTime;
    }

    public int getTimeStep() {
        return timeStep;
    }
    
    // Abstract methods of this class
    
    protected abstract void readData() throws IOException;
    
    protected abstract boolean readSimulationInfo(boolean restart) throws IOException;
    
    // Methods of this class
    
    public boolean dataAvailable() {
        try {
            if (bin == null || bin.available() == 0) {
                return false;
            } else {
                return true;
            }
        } catch (IOException e) {
            return false;
        }
    }
}
