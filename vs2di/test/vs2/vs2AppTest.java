/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vs2;

import javax.swing.filechooser.FileFilter;
import mp2.mp2Doc;
import mp2.mp2FrameManager;
import mp2.mp2PostProcessorFrame;
import mp2.mp2View;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author charlton
 */
public class vs2AppTest {
    
    public vs2AppTest() {
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

    /**
     * Test of createDoc method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testCreateDoc() {
        System.out.println("createDoc");
        vs2App instance = new vs2App();
        vs2Doc doc = (vs2Doc)instance.createDoc();
    }

    /**
     * Test of createView method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testCreateView() {
        System.out.println("createView");
        vs2App instance = new vs2App();
        vs2View view = (vs2View)instance.createView();
    }

    /**
     * Test of createFrameManager method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testCreateFrameManager() {
        System.out.println("createFrameManager");
        vs2App instance = new vs2App();
        vs2FrameManager result = (vs2FrameManager)instance.createFrameManager();
    }

    /**
     * Test of createPostProcessorFrame method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testCreatePostProcessorFrame() {
        System.out.println("createPostProcessorFrame");
        vs2App instance = new vs2App();
        vs2PostProcessorFrame result = (vs2PostProcessorFrame)instance.createPostProcessorFrame();
    }

//    /**
//     * Test of doHeat method, of class vs2App.
//     */
//    @Test
//    public void testDoHeat() {
//        System.out.println("doHeat");
//        boolean expResult = false;
//        boolean result = vs2App.doHeat();
//        assertEquals(expResult, result);
//    }

    /**
     * Test of getDocFileFilter method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testGetDocFileFilter() {
        System.out.println("getDocFileFilter");
        vs2App instance = new vs2App();
        FileFilter filter = instance.getDocFileFilter();
        boolean expResult = true;
        boolean result = filter.accept(new java.io.File("test.vs2"));
        assertEquals(expResult, result);
    }

    /**
     * Test of getFilePrefix method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testGetFilePrefix() {
        System.out.println("getFilePrefix");
        String expResult = "vs2drt";
        String result = vs2App.getFilePrefix();
        assertEquals(expResult, result);
    }

    /**
     * Test of getFrameTitle method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testGetFrameTitle() {
        System.out.println("getFrameTitle");
        vs2App instance = new vs2App();
        String expResult = "VS2DRTI Preprocessor";
        String result = instance.getFrameTitle();
        assertEquals(expResult, result);
    }

    /**
     * Test of getPropertiesDirectory method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testGetPropertiesDirectory() {
        System.out.println("getPropertiesDirectory");
        vs2App instance = new vs2App();
        String result = instance.getPropertiesDirectory();
        if (System.getProperty("os.name").startsWith("Windows")) {
            String drive = System.getenv("HOMEDRIVE");
            String path = System.getenv("HOMEPATH");            
            String expResult = drive + path;
            assertEquals(expResult, result);
        }
    }

    /**
     * Test of getPropertiesFileName method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testGetPropertiesFileName() {
        System.out.println("getPropertiesFileName");
        vs2App instance = new vs2App();
        String expResult = "vs2drti.properties";
        String result = instance.getPropertiesFileName();
        assertEquals(expResult, result);
    }

    /**
     * Test of main method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testMain() {
        System.out.println("main");
        String[] args = new String[0];
        vs2App.main(args);
    }

    /**
     * Test restart 
     */
    //@Ignore
    @Test
    public void testRestart() {
        System.out.println("testRestart");
        
        try {
            vs2App.main(null);
        
            // open ex11.vs2
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/vs2drti", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            
            java.awt.Robot robot = new java.awt.Robot();
            robot.setAutoDelay(40);
            robot.setAutoWaitForIdle(true);

            // show postprocessor
            robot.keyPress(java.awt.event.KeyEvent.VK_F6);
            robot.keyRelease(java.awt.event.KeyEvent.VK_F6);
            
            Thread.sleep(100);
            
            // click step twice
            vs2PostProcessorFrame frame = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
            assertNotEquals(frame, null);
            frame.getStepButton().doClick();
            frame.getStepButton().doClick();
            
            Thread.sleep(100);
            
            // Action->Restart computation
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_A);
            robot.keyRelease(java.awt.event.KeyEvent.VK_A);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_R);    // This causes EXCEPTION_ACCESS_VIOLATION (-r 11503)
            robot.keyRelease(java.awt.event.KeyEvent.VK_R);  // in vs2.vs2drt.getSoluteTransportMassBalanceErrors
            
            Thread.sleep(250);
            System.out.println("Hit Yes");            
            
            // Do you want to restart the computation? Yes
            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_A);
            robot.keyRelease(java.awt.event.KeyEvent.VK_A);
            
            Thread.sleep(250);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_D);
            robot.keyRelease(java.awt.event.KeyEvent.VK_D);
            
            // Do you want to restart the computation? Yes
            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);            
            
            Thread.sleep(250);
        }
        catch (java.awt.AWTException e) {
            System.out.println("AWTException");
        }
        catch (InterruptedException e) {
            System.out.println("InterruptedException");
        }
        catch (Exception e) {
            System.out.println("Exception");
        }
    }
    
    /**
     * Test RunResetRestartYesThenNo 
     */
    //@Ignore
    @Test
    public void testRunResetRestartYesThenNo() {
        System.out.println("testRunResetRestartYesThenNo");
        
        try {
            vs2App.main(null);
        
            // open ex11.vs2
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/vs2drti", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            
            java.awt.Robot robot = new java.awt.Robot();
            robot.setAutoDelay(40);
            robot.setAutoWaitForIdle(true);

            // show postprocessor
            // NOTE: robot.keyPress(java.awt.event.KeyEvent.VK_F6) doesnt seem to work in jenkins
            // get frameManager
            vs2FrameManager frameManager =
                    (vs2FrameManager) vs2App.theApp.getFrame().getManager();
            assertNotEquals(null, frameManager);
            frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();            
            
            Thread.sleep(100);

            // get frame
            vs2PostProcessorFrame frame = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
            assertNotEquals(null, frame);
            
            // verify items
            Thread.sleep(100);
            assertNotEquals(null, frame.getDisplayChooser());
            assertEquals(15, frame.getDisplayChooser().getItemCount());            
            
            // start run
            assertNotEquals(null, frame.getRunButton());
            frame.getRunButton().doClick();
            
            // wait until run has finished
            while (frame.getStopButton().isEnabled()) {
                Thread.sleep(100);
            }
            
            // Reset playback to beginning
            assertNotEquals(null, frame.getResetButton());
            frame.getResetButton().doClick();
            Thread.sleep(1000);            
            
            // Action->Restart computation
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);

            robot.keyPress(java.awt.event.KeyEvent.VK_A);
            robot.keyRelease(java.awt.event.KeyEvent.VK_A);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_R);
            robot.keyRelease(java.awt.event.KeyEvent.VK_R);

            // Do you want to restart the computation?
            // Yes
            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);            
            
            // verify items
            Thread.sleep(1000);            
            assertEquals(15, frame.getDisplayChooser().getItemCount());
            
            // start run again
            assertNotEquals(null, frame.getRunButton());
            frame.getRunButton().doClick();
            
            // wait until run has finished
            while (frame.getStopButton().isEnabled()) {
                Thread.sleep(100);
            }
            
            // Reset playback to beginning
            assertNotEquals(null, frame.getResetButton());
            frame.getResetButton().doClick();
            Thread.sleep(1000);            
            
            // Action->Restart computation
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);

            robot.keyPress(java.awt.event.KeyEvent.VK_A);
            robot.keyRelease(java.awt.event.KeyEvent.VK_A);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_R);
            robot.keyRelease(java.awt.event.KeyEvent.VK_R);

            // Do you want to restart the computation?
            // No
            Thread.sleep(100);
            robot.keyPress(java.awt.event.KeyEvent.VK_TAB);
            robot.keyRelease(java.awt.event.KeyEvent.VK_TAB);
            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);            
            
            // must close postprocessor or remaining tests may fail
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyPress(java.awt.event.KeyEvent.VK_F4);
            robot.keyRelease(java.awt.event.KeyEvent.VK_F4);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);            
            
            // verify items
            Thread.sleep(100);
            assertEquals(15, frame.getDisplayChooser().getItemCount());
        }
        catch (java.awt.AWTException e) {
            System.out.println("AWTException");
        }
        catch (InterruptedException e) {
            System.out.println("InterruptedException");
        }
        catch (Exception e) {
            System.out.println("Exception");
        }
    }
    
    /**
     * Test PostCloseOpenPost 
     */
    //@Ignore
    @Test
    public void testPostCloseOpenPost() {
        System.out.println("testPostCloseOpenPost");
        System.out.println("Thread id = " + Thread.currentThread().getId());
        
        try {
            vs2App.main(null);
        
            // open ex11.vs2
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../tests/Example11", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            
            java.awt.Robot robot = new java.awt.Robot();
            robot.setAutoDelay(40);
            robot.setAutoWaitForIdle(true);

            // show postprocessor
            // NOTE: robot.keyPress(java.awt.event.KeyEvent.VK_F6) doesnt seem to work in jenkins
            // get frameManager
            vs2FrameManager frameManager =
                    (vs2FrameManager) vs2App.theApp.getFrame().getManager();
            assertNotEquals(null, frameManager);
            frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();            
            
            Thread.sleep(100);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_A);
            robot.keyRelease(java.awt.event.KeyEvent.VK_A);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_D);
            robot.keyRelease(java.awt.event.KeyEvent.VK_D);

            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);
            
            // open ex11-test.vs2
            java.nio.file.Path path2 = java.nio.file.Paths.get(System.getProperty("user.dir"), "../tests/Example11", "ex11-test.vs2");
            assertEquals(true, java.nio.file.Files.exists(path2));
            
            java.io.File inFile2 = new java.io.File(path2.toString());
            vs2App.theApp.openFile(inFile2);

            Thread.sleep(250);
            
            frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();
            
            // must close postprocessor or remaining tests may fail
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyPress(java.awt.event.KeyEvent.VK_F4);
            robot.keyRelease(java.awt.event.KeyEvent.VK_F4);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
            
            // The computation is not finished. Do you want to quit anyway? Yes
            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);          

            Thread.sleep(500);
        }
        catch (java.awt.AWTException e) {
            System.out.println("AWTException");
        }
        catch (InterruptedException e) {
            System.out.println("InterruptedException");
        }
        catch (Exception e) {
            System.out.println("Exception");
        }
    }    

    /**
     * Test of typeCheck method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testTypeCheck() {
        System.out.println("typeCheck");
        vs2App instance = new vs2App();
        mp2Doc doc = (vs2Doc)instance.createDoc();
        boolean expResult = true;
        boolean result = instance.typeCheck(doc);
        assertEquals(expResult, result);
    }

    /**
     * Test of versionCheck method, of class vs2App.
     */
    //@Ignore
    @Test
    public void testVersionCheck() {
        System.out.println("versionCheck");
        vs2App instance = new vs2App();
        mp2Doc doc = (vs2Doc)instance.createDoc();
        boolean expResult = true;
        boolean result = instance.versionCheck(doc);
        Assert.assertEquals(expResult, result);
    }
}
