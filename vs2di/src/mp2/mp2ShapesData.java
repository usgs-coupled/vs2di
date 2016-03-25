/*
 * mp2ShapesData.java
 */
package mp2;

import java.awt.*;
import java.io.*;
import java.util.Vector;

/**
 * Holds an array of shapes.
 *
 * <P>This class implements the Serializable interface so
 * objects can be serialized and deserialized. Therefore,
 * object creation is a two-step procedure in which
 * the <code>init</code> method is called after construction
 * or deserialization. This is similar to how document objects
 * are created.</P>
 *
 * @see mp2.mp2Doc
 */
public abstract class mp2ShapesData extends mp2DiscretizableData 
        implements Serializable {

    static final long serialVersionUID = 548212788637966123L;

    /**
     * A collection of shapes, which are mp2Shape objects.
     */
    protected Vector shapes;

    /**
     * Constructor
     */
    public mp2ShapesData() {
        shapes = new Vector();
    }

    /**
     * Adds a shape to the end of shape array
     */
    public void addShape(mp2Shape shape) {
        shapes.addElement(shape);
        setDataHaveChanged();
    }

    /**
     * Bring the selected shapes to the front.
     */
    public void bringSelectedShapesToFront() {
        Vector newShapes = new Vector();
        for (int i=0; i<shapes.size(); i++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(i);
            if (!shape.isSelected()) {
                newShapes.addElement(shape);
            }
        }
        for (int i=0; i<shapes.size(); i++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(i);
            if (shape.isSelected()) {
                newShapes.addElement(shape);
            }
        }
        shapes = newShapes;
        setDataHaveChanged();
    }

    /**
     * Deletes the selected shapes
     */
    public Vector deleteSelectedShapes() {
        Vector newShapes = new Vector();
        Vector oldShapes = shapes;
        for (int i=0; i<shapes.size(); i++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(i);
            if (!shape.isSelected()) {
                newShapes.addElement(shape);
            }
        }
        shapes = newShapes;
        setDataHaveChanged();
        return oldShapes;
    }

    /**
     * Delete the shape at the specified index
     */
    public void deleteShapeAt(int i) {
        if (i>=0 && i<shapes.size()) {
            shapes.removeElementAt(i);
        }
        setDataHaveChanged();
    }

    /**
     * Deselects all shapes
     */
    public void deselectAllShapes() {
        for (int i=0; i<shapes.size(); i++) {
            ((mp2Shape) shapes.elementAt(i)).setSelected(false);
        }
    }

    /**
     * Gets the index of the specified shape
     */
    public int getIndexOfShape(mp2Shape shape) {
        return shapes.indexOf(shape);
    }

    /**
     * Gets the number of selected shapes
     */
    public int getNumberOfSelectedShapes() {
        int n = 0;
        for (int i=0; i<shapes.size(); i++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(i);
            if (shape.isSelected()) {
                n++;
            }
        }
        return n;
    }

    public Vector getClonedShapes() {
        Vector clonedShapes = new Vector();
        for (int i=0; i<shapes.size(); i++) {
            clonedShapes.addElement(shapes.elementAt(i));
        }
        return clonedShapes;
    }

    /**
     * Gets the number of shapes
     */
    public int getNumberOfShapes() {
        return shapes.size();
    }

    /**
     * Gets the shape at the specified index
     */
    public mp2Shape getShape(int i) {
        if (i>=0 && i<shapes.size()) {
            return (mp2Shape) shapes.elementAt(i);
        } else {
            return null;
        }
    }

    /**
     * Initializes this object. This method initializes the
     * transient data members of this object.
     */
    public void init(mp2Doc doc) {
        super.init(doc);

        // If this object has one or more shapes when init 
        // is called, then it was created via deserialization. 
        // In this case, we need to initialize all the shapes
        // so they hold reference to the view. Also, mark the
        // data as changed so they will be discretized.
        mp2View view = doc.getView();
        if (shapes.size() > 0) {
            for (int i=0; i<shapes.size(); i++) {
                ((mp2Shape) shapes.elementAt(i)).init(view);
            }
            dataHaveChanged = true;
        }
    }

    /**
     * Selects all the shapes in this data set
     */
    public void selectAllShapes() {
        for (int i=0; i<shapes.size(); i++) {
            ((mp2Shape) shapes.elementAt(i)).setSelected(true);
        }
    }

    /**
     * Send the selected shapes to the back
     */
    public void sendSelectedShapesToBack() {
        Vector newShapes = new Vector();
        for (int i=0; i<shapes.size(); i++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(i);
            if (shape.isSelected()) {
                newShapes.addElement(shape);
            }
        }
        for (int i=0; i<shapes.size(); i++) {
            mp2Shape shape = (mp2Shape) shapes.elementAt(i);
            if (!shape.isSelected()) {
                newShapes.addElement(shape);
            }
        }
        shapes = newShapes;
        setDataHaveChanged();
    }

    public void setShapes(Vector shapes) {
        this.shapes = shapes;
        setDataHaveChanged();
    }
}
