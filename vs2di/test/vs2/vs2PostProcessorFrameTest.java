/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vs2;

import java.io.BufferedReader;
import java.io.PrintWriter;
import mp2.mp2ComputationalModel;
import mp2.mp2PlaybackBinary;
import mp2.mp2PostProcessorView;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Ignore;

/**
 *
 * @author charlton
 */
public class vs2PostProcessorFrameTest {
    
    public vs2PostProcessorFrameTest() {
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
//     * Test of onExit method, of class vs2PostProcessorFrame.
//     */
//    @Test
//    public void testOnExit() {
//        System.out.println("onExit");
//        try {
//            vs2App.main(null);
//            
//            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/vs2drti", "ex11.vs2");
//            assertEquals(true, java.nio.file.Files.exists(path));
//            
//            java.io.File inFile = new java.io.File(path.toString());
//            vs2App.theApp.openFile(inFile);
//            
//            java.awt.Robot robot = new java.awt.Robot();
//            robot.setAutoDelay(40);
//            robot.setAutoWaitForIdle(true);
//
//            // show postprocessor
//            robot.keyPress(java.awt.event.KeyEvent.VK_F6);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_F6);
//            
//            Thread.sleep(100);
//            
//            // click step
//            vs2PostProcessorFrame instance = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
//            assertNotEquals(instance, null);
//            instance.getStepButton().doClick();
//            
//            Thread.sleep(100);
//            
//            // Action->Done
//            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            
//            Thread.sleep(100);
//            
//            // answer no (The computation is not finished. Do you want to quit anyway?)
//            robot.keyPress(java.awt.event.KeyEvent.VK_TAB);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_TAB);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            
//            instance.getStepButton().doClick();    // this used to cause an EXCEPTION_ACCESS_VIOLATION
//
//            Thread.sleep(100);
//            
//            // Action->Done
//            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            
//            Thread.sleep(100);
//            
//            // answer yes (The computation is not finished. Do you want to quit anyway?)
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            
//            Thread.sleep(100);
//            
//            // Minimize main window
//            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyPress(java.awt.event.KeyEvent.VK_N);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_N);            
//            
//            Thread.sleep(100);
//        }
//        catch (java.awt.AWTException e) {
//        }
//        catch (InterruptedException e) {
//        }
//    }
    
    /**
     * Test of onExit method, of class vs2PostProcessorFrame.
     */
    //@Ignore
    @Test
    public void testRunEx11ThenFlowEx1() {
        System.out.println("testRunEx11ThenFlowEx1");
//        try {
//            vs2App.main(null);
//            
//            // open ex11.vs2
//            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/vs2drti", "ex11.vs2");
//            assertEquals(true, java.nio.file.Files.exists(path));
//            
//            java.io.File inFile = new java.io.File(path.toString());
//            vs2App.theApp.openFile(inFile);
//            
//            java.awt.Robot robot = new java.awt.Robot();
//            robot.setAutoDelay(40);
//            robot.setAutoWaitForIdle(true);
//
//            // show postprocessor
//            robot.keyPress(java.awt.event.KeyEvent.VK_F6);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_F6);
//            
//            Thread.sleep(100);
//            
//            // click step
//            vs2PostProcessorFrame instance = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
//            assertNotEquals(instance, null);
//            instance.getStepButton().doClick();
//            instance.getStepButton().doClick();
//            
//            Thread.sleep(100);
//            
//            // Action->Done
//            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            
//            Thread.sleep(100);
//            
//            // answer yes (The computation is not finished. Do you want to quit anyway?)
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            
//            // open flow/ex1
//            java.nio.file.Path path2 = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/flow/example1", "example1.1_4.vs2");
//            assertEquals(true, java.nio.file.Files.exists(path2));
//            
//            java.io.File inFile2 = new java.io.File(path2.toString());
//            vs2App.theApp.openFile(inFile2);
//            
//            Thread.sleep(100);
//            
//            // show postprocessor    (this used to cause an exception in vs2drtJni
//            robot.keyPress(java.awt.event.KeyEvent.VK_F6);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_F6);
//            
//            Thread.sleep(100);
//            
//            // Action->Done
//            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_DOWN);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            
//            Thread.sleep(100);
//            
//            // answer yes (The computation is not finished. Do you want to quit anyway?)
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            
//            Thread.sleep(100);
//            
//            // Minimize main window
//            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
//            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
//            robot.keyPress(java.awt.event.KeyEvent.VK_N);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_N);            
//
//            Thread.sleep(1000);
//        }
//        catch (java.awt.AWTException e) {
//        }
//        catch (InterruptedException e) {
//        }
    }
}
