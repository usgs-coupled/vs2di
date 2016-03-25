/*
 * mp2BoundaryCondition
 */
package mp2;

import java.io.Serializable;
/**
 * An abstract superclass to be subclassed for encapsulating
 * boundary conditions of a specific model
 */
public abstract class mp2BoundaryCondition implements Serializable {

    static final long serialVersionUID = 7772920765954028271L;

    public abstract void copy(mp2BoundaryCondition bc);
}