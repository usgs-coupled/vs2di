/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vs2;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.Timer;
import mp2.mp2JavaHelp;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import static org.junit.Assert.*;
import static vs2.vs2Constants.PRESSURE_HEAD_BC;
import static vs2.vs2Constants.SPECIFIED_CONC_BC;

/**
 *
 * @author charlton
 */
public class vs2BoundaryConditionsDialogTest {
    
    public vs2BoundaryConditionsDialogTest() {
    }
    
    @BeforeClass
    public static void setUpClass() {
    }
    
    @AfterClass
    public static void tearDownClass() {
    }
    
    @Before
    public void setUp() {
    }
    
    @After
    public void tearDown() {
    }

//    /**
//     * Test of makeContents method, of class vs2BoundaryConditionsDialog.
//     */
//    @Ignore
//    @Test
//    public void testMakeContents() {
//        System.out.println("makeContents");
//        vs2BoundaryConditionsDialog instance = null;
//        instance.makeContents();
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of doModal method, of class vs2BoundaryConditionsDialog.
//     */
//    @Ignore
//    @Test
//    public void testDoModal() {
//        System.out.println("doModal");
//        vs2BoundaryConditionsDialog instance = null;
//        boolean expResult = false;
//        boolean result = instance.doModal();
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of onBrowserHelp method, of class vs2BoundaryConditionsDialog.
//     */
//    @Ignore
//    @Test
//    public void testOnBrowserHelp() {
//        System.out.println("onBrowserHelp");
//        vs2BoundaryConditionsDialog instance = null;
//        instance.onBrowserHelp();
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of onSelectedFlowBC method, of class vs2BoundaryConditionsDialog.
//     */
//    @Ignore
//    @Test
//    public void testOnSelectedFlowBC() {
//        System.out.println("onSelectedFlowBC");
//        vs2BoundaryConditionsDialog instance = null;
//        instance.onSelectedFlowBC();
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }

    /**
     * Test of retrieveData method, of class vs2BoundaryConditionsDialog.
     */
    @Test
    public void testRetrieveData() {
        System.out.println("retrieveData");
        
        mp2JavaHelp.initialize("vs2dtiHelp");        
        vs2ModelOptions options = new vs2ModelOptions();
        options.doSoluteTransport = false;
        
        final vs2BoundaryConditionsDialog dlg = new vs2BoundaryConditionsDialog(1, options);
        
        vs2BoundaryCondition bc = new vs2BoundaryCondition();
        bc.flowType = PRESSURE_HEAD_BC;
        bc.flowValue = 0.0405;
        
        bc.setEnergyTransportType(SPECIFIED_CONC_BC);
        bc.setEnergyTransportValue(22);
        
        dlg.flowBCType = bc.flowType;
        dlg.flowBCValue = bc.flowValue;
        
        dlg.energyTransportBCType = bc.getEnergyTransportType();
        dlg.energyTransportBCValue = bc.getEnergyTransportValue();
        
        Timer timer = new Timer(1000, new ActionListener() { // 1 sec
            public void actionPerformed(ActionEvent ae) {
                dlg.setVisible(false);
                dlg.dispose();
            }
        });        
        
        timer.start();        
        dlg.doModal();
        
        boolean expResult = true;
        boolean result = dlg.retrieveData();   // throws java.lang.NullPointerException on failure
        assertEquals(expResult, result);
    }
    
}
