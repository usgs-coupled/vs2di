/*
 * mp2TableWindow.java
 */
package mp2;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Defines the behavior of a modeless dialog box that 
 * holds a table and buttons for editing the table data.
 */
public abstract class mp2TableWindow extends JDialog {

    protected mp2Frame frame;
    protected int width = 600;
    protected int height = 250;
    protected JTable activeTable;
    protected mp2TableData tableData;

    protected JPanel buttonPanel;
    protected JPanel buttonPanelLeft;
    protected JPanel buttonPanelRight;
    protected JButton addButton;
    protected JButton editButton;
    protected JButton deleteButton;
    protected JButton hideButton;
    protected JButton helpButton;
    protected int minRowCount = 0;
    protected boolean isEnabled;

    /**
     * Creates a mp2TableWindow for the specified title, location,
     * and table data.
     */
    public mp2TableWindow(mp2Frame frame, String title, Point location, 
                          mp2TableData tableData) {
        super(frame, title, false);
        this.frame = frame;
        this.tableData = tableData;
        isEnabled = true;

        getContentPane().setLayout(new BorderLayout());

        // Add a panel to the bottom of the window to hold two sub panels
        
        getContentPane().add(buttonPanel = new JPanel(new FlowLayout(
                FlowLayout.CENTER, 50, 10), false), BorderLayout.SOUTH);

        // Add a panel to hold the "Add", "Edit", and "Delete" buttons
        buttonPanel.add(buttonPanelLeft = new JPanel(new FlowLayout(
                FlowLayout.CENTER), false));
        buttonPanelLeft.add(addButton = new JButton("Add"));
        addButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onAdd();
            }
        });
        addButton.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    onAdd();
                }
            }
        });
        addButton.requestFocus();

        buttonPanelLeft.add(editButton = new JButton("Edit"));
        editButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onEdit();
            }
        });
        editButton.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    onEdit();
                }
            }
        });

        buttonPanelLeft.add(deleteButton = new JButton("Delete"));
        deleteButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onDelete();
            }
        });
        deleteButton.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    onDelete();
                }
            }
        });

        // Add another subpanel to hold the "Hide" and "Help" buttons
        buttonPanel.add(buttonPanelRight = new JPanel(new FlowLayout(
                FlowLayout.CENTER), false));
        buttonPanelRight.add(hideButton = new JButton("Hide"));
        hideButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onHide();
            }
        });
        hideButton.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    onHide();
                }
            }
        });
        buttonPanelRight.add(helpButton = new JButton("Help"));
        if (!mp2App.useJavaHelp()) {
            helpButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    onBrowserHelp();
                }
            });
            helpButton.addKeyListener(new KeyAdapter() {
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                        onBrowserHelp();
                    }
                }
            });
        }

// Check the next statement. Doesn't seem to make sense.
        if (tableData.getNumberOfRows() < 0) {
            activeTable.addRowSelectionInterval(0, 0);
        }
        if (tableData.getNumberOfRows() > minRowCount) {
            editButton.setEnabled(true);
            deleteButton.setEnabled(true);
        }
        else {
            editButton.setEnabled(false);
            deleteButton.setEnabled(false);
        }



        // enable handling of window events
        enableEvents(AWTEvent.WINDOW_EVENT_MASK);

        // Set initial location (upper left corner) of the window
        setLocation(location);

        // Initially the window is invisible
        setVisible(false);
    }

    /**
     * Adds a row of data at the specified row
     */
    protected abstract int addRow(int r);

    /**
     * Edits the specified rows of data
     */
    protected abstract void editRows(int [] r);

    /**
     * Invoked when the "Hide" button is clicked. Hides the window.
     */
    protected abstract void onHide();

    /**
     * Gets the preferred size of this window
     */
    public Dimension getPreferredSize() {
        return new Dimension(width, height);
    }

    /**
     * Gets the maximum size of this window
     */
    public Dimension getMaximumSize() {
        return new Dimension(width, height);
    }

    /**
     * Gets the selected row
     */
    public int getSelectedRow() {
        if (activeTable != null) {
            return activeTable.getSelectedRow();
        } else {
            return -1;
        }
    }

    /**
     * Invoked when the "Add" button is clicked. 
     */
    protected void onAdd() {
        if (activeTable == null) {
            return;
        }

        // Find the selected row
        int r = activeTable.getSelectedRow();

        // Add a new data row at the selected row. The AddRow method
        // is abstract here and must be implemented in a subclass.
        // This method returns the number of added rows.
        int numberOfNewRows = addRow(r);
        if (numberOfNewRows > 0) {
            // Select the added row
            activeTable.clearSelection();
            // If there is only one row in the table, then select it.
            if (tableData.getNumberOfRows() == 1) {
                activeTable.addRowSelectionInterval(0, 0);
            }
            // If there are more than one row, then select the last added row
            else {
                activeTable.addRowSelectionInterval(r+numberOfNewRows, r+numberOfNewRows);
            }

            // Scroll the last added row into view.
            activeTable.scrollRectToVisible(activeTable.getCellRect(
                    r+numberOfNewRows, 0, true));

            // If the row count is above mininum, then enable the delete and 
            // edit buttons
            if (tableData.getNumberOfRows() > minRowCount) {
                editButton.setEnabled(true);
                deleteButton.setEnabled(true);
            }

            // Request focus for the add button
            addButton.requestFocus();
        }
        requestFocus();
    }

    /**
     * Invoked when the "Delete" button is clicked
     */
    protected boolean onDelete() {
        if (activeTable == null) {
            return false;
        }

        // Find the selected row(s)
        int r[] = activeTable.getSelectedRows();

        // If no row is selected, just return
        if (r.length == 0) {
            return false;
        }

        // Verify that user really wants to delete the selected row
        int result = mp2MessageBox.showYesNoDialog(
            "Do you want to delete the selected data? (No undo.)", "Confirm");

        if (result == mp2MessageBox.YES_OPTION) {
            // Delete the selected row(s) from the data
            for (int i=0; i<r.length; i++) {
                tableData.deleteRow(r[0]);
            }

            // Determine the revised row count
            int rowCount = tableData.getNumberOfRows();

            // Set row selection and scroll it into view
            activeTable.clearSelection();
            if (rowCount > 0) {
                int k = Math.min(r[0], rowCount-1);
                activeTable.addRowSelectionInterval(k, k);
                activeTable.scrollRectToVisible(activeTable.getCellRect(
                        k, 0, true));
            }

            // If the row count is at minimum, then disable the delete and edit buttons
            if (rowCount <= minRowCount) {
                editButton.setEnabled(false);
                deleteButton.setEnabled(false);
            }
            // Otherwise, request focus for the delete button
            else {
                deleteButton.requestFocus();
            }

            // Repaint the window
            getContentPane().repaint();
            requestFocus();
            return true;
        }
        else {
            return false;
        }
    }

    /**
     * Invoked when the "Edit" button is clicked
     */
    protected void onEdit() {
        if (activeTable == null) {
            return;
        }

        // Get the selected row
        int [] r = activeTable.getSelectedRows();

        // Do nothing if row index is out of bound
        //if (r >= tableData.getNumberOfRows()) return;

        // Edit the row. The EditRow method is abstract here and must be 
        // implemented in a subclass
        editRows(r);

        // Request button for the edit button and for this window
        editButton.requestFocus();
        requestFocus();
    }

    /**
     * Invoked when the "Help" button is clicked. 
     */
    protected void onBrowserHelp() {
        mp2MessageBox.showMessageDialog("Sorry, there is no help.", 
                "Help");
    }

    /**
     * Invoked when the window closes. This is equivalent to
     * clicking the "Hide" button
     */
    protected void processWindowEvent(WindowEvent e) {
        if (e.getID()==WindowEvent.WINDOW_CLOSING) {
            onHide();
        }
        super.processWindowEvent(e);
    }

    /**
     * Enable or disable the editing buttons on the window
     */
    public void setEnabled(boolean b) {
        isEnabled = b;
        if (b) {
            addButton.setEnabled(true);
            if (tableData.getNumberOfRows() > minRowCount) {
                editButton.setEnabled(true);
                deleteButton.setEnabled(true);
            }
        }
        else {
            addButton.setEnabled(false);
            editButton.setEnabled(false);
            deleteButton.setEnabled(false);
        }
    }
}
