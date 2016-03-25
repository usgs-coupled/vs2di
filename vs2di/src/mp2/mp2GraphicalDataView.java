/*
 * mp2GraphicalDataView.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.undo.*;
import javax.swing.event.*;

/**
 * Defines the behavior of a graphical data view, which
 * manages graphical data. Has undo capability.
 *
 * @see mp2.mp2GraphicalData
 */
public abstract class mp2GraphicalDataView implements mp2Constants {

    protected mp2View view;
    protected boolean isVisible;
    protected boolean isActive;
    protected boolean isEditable;
    protected UndoManager undoManager;
    protected UndoableEditSupport undoSupport;
    protected UndoAction undoAction;
    protected RedoAction redoAction;
    protected JMenuItem undoMenuItem;
    protected JMenuItem redoMenuItem;
    protected long undoCount;
    protected long docSavedIndex;
    protected mp2ToggleButton zoomButton;

    public mp2GraphicalDataView(mp2View vw, String homeDirectory) {
        this.view = vw;
        isActive = false;
        isVisible = false;
        isEditable = true;
        
        undoManager = new UndoManager();
        undoSupport = new UndoableEditSupport();
        undoSupport.addUndoableEditListener(new UndoHandler());
        undoAction = new UndoAction();
        redoAction = new RedoAction();
        // This is a hack to add accelarators to undo and redo
        // menu items. The undoAction and redoAction are added to a 
        // dummy menu in order to create the menu items.
        // The accelerators are then set to the menu items.
        JMenu dummyMenu = new JMenu("dummy");
        undoMenuItem = dummyMenu.add(undoAction);
        undoMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_Z, Event.CTRL_MASK));
        redoMenuItem = dummyMenu.add(redoAction);
        redoMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_Y, Event.CTRL_MASK));
        undoCount = 0;
        docSavedIndex = 0;

        String fileSeparator = System.getProperty("file.separator");
        String imageDirectory = homeDirectory + fileSeparator + 
                                            "images" + fileSeparator;
        zoomButton = new mp2ToggleButton(
                new ImageIcon(imageDirectory + "zoom.gif"));
        zoomButton.setToolTipText("Zoom");
        zoomButton.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                view.setZoomActive(e.getStateChange() == ItemEvent.SELECTED);
            }
        });
    }

    public void deselectAll() {}
    
    /**
     * Enables or disables items in the edit menu depending on the
     * state of this view.
     */
    protected void evaluateEditMenu() {};

    /**
     * Invoked when the document is saved. Remembers the undo count at which
     * the doc was saved.
     */
    public void docSaved() {
        docSavedIndex = undoCount;
    }

    /**
     * Indicates whether or not this view is active
     */
    public boolean isActive() {
        return isActive;
    }

    /**
     * Indicates whether or not this view is visible
     */
    public boolean isVisible() {
        return isVisible;
    }
    
    public boolean modelVerticalAxisUpward() {
        return (view.getModelVerticalAxisOrientation() 
                == mp2Drawing.MODEL_VERTICAL_AXIS_UPWARD_POSITIVE);
    }

    /**
     * Invoked when non graphical data (table data or model options)
     * are changed. When this happens, there is no possibility for
     * returning back to a state where the doc was unchanged (since last
     * save) by undo. The doc saved index is set to -1.
     */
    public void noUndoableDataChanged() {
        docSavedIndex = -1;
    }

    public void clearUndoQueue() {
        undoManager.discardAllEdits();
        // This is a hack to update the undo and redo menu items
        undoMenuItem.doClick();
        undoCount = 0;
        docSavedIndex = -1;
    }

    /**
     * This default implementation does nothing when a mouse
     * button is clicked on the view.
     */
    public void onMouseClicked(MouseEvent e) {}

    /**
     * This default implementation does nothing when a mouse
     * button is pressed on the view and then dragged.
     */
    public void onMouseDragged(MouseEvent e) {}

    /**
     * This default implementation does nothing when the mouse
     * enters the view.
     */
    public void onMouseEntered(MouseEvent e) {}

    /**
     * This default implementation does nothing when the mouse
     * exits the view.
     */
    public void onMouseExited(MouseEvent e) {}

    /**
     * This default implementation does nothing when the mouse
     * has moved on the view (with no buttons down).
     */
    public void onMouseMoved(MouseEvent e) {}

    /**
     * This default implementation does nothing when a mouse
     * button has been pressed on the view.
     */
    public void onMousePressed(MouseEvent e) {}

    /**
     * This default implementation does nothing when a mouse
     * button has been released on the view.
     */
    public void onMouseReleased(MouseEvent e) {}

    /**
     * Paint this graphical data view
     */
    public abstract void paint(Graphics g);

    /**
     * Paints on top of everything else on the view.
     */
    public void paintLast(Graphics g) {}

    /**
     * Prepares this data view to become active. Resets the undo queue.
     * Subclass should override this method by first calling
     * the super class method and then load menu items to the edit
     * menu and buttons to toolbar.
     */
    public void prepareToActivate() {
        view.setZoomActive(false);
        view.getXORButton().setSelected(false);
        // Get rid of all previous edits
        undoManager.discardAllEdits();
        // This is a hack to update the undo and redo menu items
        undoMenuItem.doClick();
        undoCount = 0;
        docSavedIndex = view.getDoc().isChanged() ? -1 : 0;
    }

    /**
     * Sets this graphical data view as active or inactive depending 
     * on the value of the argument
     *
     * @param  b  If <code>true</code> sets this graphical data view
     *            as active. Otherwise, hides this graphical data view
     *            as inactive.
     */
    public void setActive(boolean b) {
        this.isActive = b;
    }

    /**
     * Enables or disables this view for editing
     */
    public void setEditable(boolean b) {
        isEditable = b;
    }

    /**
     * Shows or hides this graphical data view depending on the value
     * of the argument
     *
     * @param  b  If <code>true</code> shows this graphical data view.
     *            Otherwise, hides this graphical data view.
     */
    public void setVisible(boolean b) {
        isVisible = b;
    }

    /**
     * Inner class to handle undo
     */
    protected class UndoHandler implements UndoableEditListener {

       /**
        * Notified when an undoable edit occurred. The edit is extracted 
        * from the event and added to the undo manager. GUI us updated
        */
        public void undoableEditHappened (UndoableEditEvent e) {
            undoManager.addEdit(e.getEdit());
        undoAction.update();
        redoAction.update();
        }
    }

    /**
     * Undo a previous undoable edit
     */
    protected class UndoAction extends AbstractAction {

        public UndoAction() {
            super("Undo");
            setEnabled(false);
        }

        public void actionPerformed(ActionEvent e) {
            try {
                undoManager.undo();
            } catch (CannotUndoException ex) {}
            update();
            redoAction.update();
            view.repaint();
        }

        protected void update() {
            if(undoManager.canUndo()) {
                setEnabled(true);
                putValue(Action.NAME, undoManager.getUndoPresentationName());
            } else {
                setEnabled(false);
                putValue(Action.NAME, "Undo");
            }
        }
    }

    /**
     * Redo a previously undone edit
     */
    protected class RedoAction extends AbstractAction {

        public RedoAction() {
            super("Redo");
            setEnabled(false);
        }

        public void actionPerformed(ActionEvent e) {
            try {
                undoManager.redo();
            } catch (CannotRedoException ex) {}
            update();
            undoAction.update();
            view.repaint();
        }

        protected void update() {
            if(undoManager.canRedo()) {
                setEnabled(true);
                putValue(Action.NAME, undoManager.getRedoPresentationName());
            } else {
                setEnabled(false);
                putValue(Action.NAME, "Redo");
            }
        }
    }

    /**
     * abstract superclass for undo subclassing
     */
    public abstract class AbstractDataEdit extends AbstractUndoableEdit {
        protected long undoIndex;

        public AbstractDataEdit(long undoIndex) {
            this.undoIndex = undoIndex;
        }

        public void undo() throws CannotUndoException {
            undoCount = undoIndex;
            if (undoIndex == docSavedIndex) {
                view.getDoc().setChanged(false);
            } else {
                view.getDoc().setChanged(true);
            }
            evaluateEditMenu();
        }

        public void redo() throws CannotRedoException {
            undoCount = undoIndex + 1;
            if (undoIndex+1 == docSavedIndex) {
                view.getDoc().setChanged(false);
            } else {
                view.getDoc().setChanged(true);
            }
            evaluateEditMenu();
        }
        
        public boolean canUndo() {
            return true;
        }
        
        public boolean canRedo() {
            return true;
        }
    }
}
