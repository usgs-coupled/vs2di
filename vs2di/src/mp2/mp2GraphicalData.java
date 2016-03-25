/*
 * mp2GraphicalData.java
 */
package mp2;

import java.io.*;
import java.awt.*;

/**
 * Defines the behavior of graphical data object. A graphical data 
 * objects hold data that can be illustrated graphically in 2D. 
 * An example is the model domain. All graphical data 
 * objects hold a reference to the document so that the document 
 * can be marked as "changed" when the data object is modified.
 *
 * <P>This class implements the Serializable interface so that
 * graphical data objects can be serialized and deserialized. 
 * Therefore, object creation is a two-step procedure in which
 * the <code>init</code> method is called after construction
 * or deserialization. This is similar to how document objects
 * are created.
 *
 * @see mp2.mp2Doc
 */
public abstract class mp2GraphicalData implements Serializable {

    static final long serialVersionUID = -6384815587890691779L;

    /**
     * The document to which this data belongs. Note that this 
     * variable is transient and is not serialized.
     */
    protected transient mp2Doc doc;

    /**
     * Initializes the data. Sets up the reference to the doc.
     */
    public void init(mp2Doc doc) {
        this.doc = doc;
    }
}