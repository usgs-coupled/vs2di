/*
 * vs2ModelOptionsDialog.java
 */
package vs2;

import mp2.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

public class vs2ModelOptionsDialog extends mp2Dialog {

    public vs2BasicPanel basicPanel;
    public vs2FlowPanel flowPanel;
    public vs2TransportPanel transportPanel;
    public vs2SolverPanel solverPanel;
    public vs2OutputPanel outputPanel;
    public vs2FluidBalancePanel fluidBalancePanel;
    public vs2BalancePanel balancePanel;

    protected int activePanelIndex;
    protected vs2ModelOptionsPanel activePanel;
    protected JTabbedPane tabbedPane;
    protected boolean firstCycle;

    /**
     * Constructs the tabbed dialog box to specify model options
     */
    public vs2ModelOptionsDialog() {
        super("Model Options", true);
	    mp2JavaHelp.hb.enableHelpOnButton(helpButton, "basicOptions", null);
    }

    /**
     * Create contents of this dialog box
     */
    protected void makeContents() {

        // Create the panels for this dialog box
        basicPanel = new vs2BasicPanel(this);
        flowPanel = new vs2FlowPanel(this);
        transportPanel = new vs2TransportPanel(this);
        solverPanel = new vs2SolverPanel(this);
        outputPanel = new vs2OutputPanel(this);
        fluidBalancePanel = new vs2FluidBalancePanel(this);
        balancePanel = new vs2BalancePanel(this);

        // Create a tabbed pane to hold the panels, and
        // add the panels
        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Basic", null, basicPanel);
        tabbedPane.addTab("Flow", null, flowPanel);
        tabbedPane.addTab("Transport", null, transportPanel);
        tabbedPane.addTab("Solver", null, solverPanel);
        tabbedPane.addTab("Output", null, outputPanel);
        tabbedPane.addTab("Fluid Balance", null, fluidBalancePanel);
        tabbedPane.addTab("Energy/Solute Balance", null, balancePanel);

        tabbedPane.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                onTabChanged();
            }
        });

        // Add the tabbed pane to the center of the dialog box
        getContentPane().add(tabbedPane, BorderLayout.CENTER);
    }

    /**
     * Show the model dialog box
     */
    public boolean doModal() {
        // Initialize all the panels
        basicPanel.init();
        flowPanel.init();
        transportPanel.init();
        solverPanel.init();
        outputPanel.init();
        fluidBalancePanel.init();
        balancePanel.init();

        // Set the initial tab selection to the basic panel
        tabbedPane.setSelectedIndex(0);
        activePanel = basicPanel;
        firstCycle = true;

        // Call superclass method to show the dialog box
        return super.doModal();
    }

    protected void onTabChanged() {
        if (firstCycle) {
            if (!activePanel.retrieveData()) {
                firstCycle = false;
                tabbedPane.setSelectedComponent(activePanel);
            } else {
                activePanel =
                    (vs2ModelOptionsPanel) tabbedPane.getSelectedComponent();
            }
        } else {
            firstCycle = true;
        }
        String tabTitle = tabbedPane.getTitleAt(tabbedPane.getSelectedIndex());
        if (tabTitle.equalsIgnoreCase("Flow")) {
              mp2JavaHelp.hb.enableHelpOnButton(helpButton, "flowOptions", null);
        }
        else if (tabTitle.equalsIgnoreCase("Transport")) {
              mp2JavaHelp.hb.enableHelpOnButton(helpButton, "transportOptions", null);
        }
        else if (tabTitle.equalsIgnoreCase("Solver")) {
              mp2JavaHelp.hb.enableHelpOnButton(helpButton, "solverOptions", null);
        }
        else if (tabTitle.equalsIgnoreCase("Output")) {
              mp2JavaHelp.hb.enableHelpOnButton(helpButton, "outputOptions", null);
        }
        else if (tabTitle.equalsIgnoreCase("Fluid Balance")) {
              mp2JavaHelp.hb.enableHelpOnButton(helpButton, "fluidBalanceOptions", null);
        }
        else if (tabTitle.equalsIgnoreCase("Energy/Solute Balance")) {
              mp2JavaHelp.hb.enableHelpOnButton(helpButton, "soluteBalanceOptions", null);
        }
        else {
              mp2JavaHelp.hb.enableHelpOnButton(helpButton, "basicOptions", null);
        }
    }

    /**
     * Override the onBrowserHelp() method of the super class
     */
    protected void onBrowserHelp() {
        String tabTitle = tabbedPane.getTitleAt(tabbedPane.getSelectedIndex());
        if (tabTitle.equalsIgnoreCase("Basic")) {
              mp2HelpWindow.showHelpFile ("basicOptions.html");
        }
        else if (tabTitle.equalsIgnoreCase("Flow")) {
              mp2HelpWindow.showHelpFile ("flowOptions.html");
        }
        else if (tabTitle.equalsIgnoreCase("Transport")) {
              mp2HelpWindow.showHelpFile ("transportOptions.html");
        }
        else if (tabTitle.equalsIgnoreCase("Solver")) {
              mp2HelpWindow.showHelpFile ("solverOptions.html");
        }
        else if (tabTitle.equalsIgnoreCase("Output")) {
              mp2HelpWindow.showHelpFile ("outputOptions.html");
        }
        else if (tabTitle.equalsIgnoreCase("Fluid Balance")) {
              mp2HelpWindow.showHelpFile ("fluidBalanceOptions.html");
        }
        else if (tabTitle.equalsIgnoreCase("Energy/Solute Balance")) {
              mp2HelpWindow.showHelpFile ("soluteBalanceOptions.html");
        }
        else {
              mp2HelpWindow.showHelpFile ("contents.html");
        }
    }


    /**
     * Retrieve data from the current panel. Data in non selected panels
     * have already been retrieved upon tab change.
     */
    protected boolean retrieveData() {
        if (!activePanel.retrieveData()) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Remove the transport panel
     */
    protected void removeTransportPanel() {
        for (int i=0; i<tabbedPane.getTabCount(); i++) {
            String title = tabbedPane.getTitleAt(i);
            if (title.equals("Transport") 
                    || title.equals("Energy/Solute Balance")) {
                tabbedPane.removeTabAt(i);
            }
        }
        getContentPane().validate();
        getContentPane().repaint();
    }

    /**
     * Restore the transport panel. The transport panel is inserted after the
     * flow panel. If the flow panel is not found, the transport panel is
     * inserted at the end (this should not happen).
     */
    protected void restoreTransportPanel() {
        // Find the "Flow panel", and also make sure that the tabbed pane does
        // not already contain the transport panel
        int transportPanelIndex = tabbedPane.getTabCount();
        String title;
        for (int i=0; i<tabbedPane.getTabCount(); i++) {
            title = tabbedPane.getTitleAt(i);

            // If the flow panel is found, then transport panel
            // will be inserted after the flow panel
            if (title.equals("Flow")) {
                transportPanelIndex = i+1;
            }

            // If the transport panel is found, then return
            if (tabbedPane.getTitleAt(i).equals("Transport")) {
                return;
            }
        }

        // Add the transport panel
        tabbedPane.insertTab("Transport", null, transportPanel,
                              "", transportPanelIndex);
        // Add the solute balance panel to the end
        tabbedPane.addTab("Energy/Solute Balance", null, balancePanel);
        getContentPane().validate();
        getContentPane().repaint();
    }
}

