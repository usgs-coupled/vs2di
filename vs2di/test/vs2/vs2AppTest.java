/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vs2;

import java.awt.Dimension;
import javax.swing.JMenuItem;
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
        System.out.println("System properties");
        java.util.Properties p = System.getProperties();
        java.util.Enumeration keys = p.keys();
        while (keys.hasMoreElements()) {
            String key = (String)keys.nextElement();
            String value = (String)p.get(key);
            System.out.println(key + ": " + value);
        }
        System.out.println("Environmental variables");
        java.util.Map<String, String> env = System.getenv();
        for (String envName : env.keySet()) {
            System.out.format("%s=%s\n",
                              envName,
                              env.get(envName));
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
     * Test testHelp 
     */
    @Ignore   // still needs work
    @Test  
    public void testHelp() {
        System.out.println("testHelp");
        
        try {
            vs2App.main(null);
        
            java.awt.Robot robot = new java.awt.Robot();
            robot.setAutoDelay(40);
            robot.setAutoWaitForIdle(true);

            // show postprocessor
            // NOTE: robot.keyPress(java.awt.event.KeyEvent.VK_F6) doesnt seem to work in jenkins
            // get frameManager
            vs2FrameManager frameManager =
                    (vs2FrameManager) vs2App.theApp.getFrame().getManager();
            assertNotEquals(null, frameManager);
            
            Thread.sleep(100);

//            // get frame
//            vs2PostProcessorFrame frame = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
//            assertNotEquals(null, frame);

            javax.swing.JFrame frame = vs2App.theApp.getFrame();

            /**
            while (!frame.isShowing()) {
                Thread.sleep(100);
                System.out.println("isShowing");
            }
            */
            //while (!frame.isEnabled()) Thread.sleep(100);
            //while (!frame.isDisplayable()) Thread.sleep(100);
            //while (!frame.hasFocus()) Thread.sleep(100);
            //while (!frame.getFocusableWindowState()) Thread.sleep(100);
            //while (!frame.isActive()) Thread.sleep(100);
            //while (!frame.isFocusable()) Thread.sleep(100);
            //while (!frame.isVisible()) Thread.sleep(100);

            //Thread.sleep(20000);

            
            Dimension d = frame.getSize();
            System.out.println("h=" + d.height + "w=" + d.width);
            java.awt.Rectangle r = frame.getBounds();
            System.out.println("x=" + r.x + "y=" + r.y);
            
            java.awt.Point p = frame.getLocationOnScreen();
            
            //robot.mouseMove(r.x+d.width, r.y);
            //robot.mouseMove(r.x, r.y);
            //robot.mouseMove(r.y, r.x);
            //robot.mouseMove(p.x+20, p.y+20);
            //robot.mouseMove(p.x + r.width, p.y);
            
            //Thread.sleep(10000);
            Thread.sleep(100);
            
            // Help->Contents
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_H);
            robot.keyRelease(java.awt.event.KeyEvent.VK_H);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_C);
            robot.keyRelease(java.awt.event.KeyEvent.VK_C);
            
            Thread.sleep(10000);
            
            //{{
//            java.awt.Component c = java.awt.KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
//            while (c != null) {
//                c = c.getParent();
//            }            
            //}}
            
            robot.mouseMove(p.x + r.width - 1, p.y + 1);
            robot.mousePress(java.awt.event.InputEvent.BUTTON1_MASK);
            robot.mouseRelease(java.awt.event.InputEvent.BUTTON1_MASK);
            
            Thread.sleep(3000);
            
            
            // Switch focus to preprocessor (by minimizing help window)
//            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
//            
//            robot.keyPress(java.awt.event.KeyEvent.VK_TAB);
//            robot.keyRelease(java.awt.event.KeyEvent.VK_TAB);            
//
//            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_SPACE);
            robot.keyRelease(java.awt.event.KeyEvent.VK_SPACE);            

            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_N);
            robot.keyRelease(java.awt.event.KeyEvent.VK_N);
            
            Thread.sleep(250);
            
            // Options->Model...
            robot.keyPress(java.awt.event.KeyEvent.VK_ALT);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ALT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_O);
            robot.keyRelease(java.awt.event.KeyEvent.VK_O);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_M);
            robot.keyRelease(java.awt.event.KeyEvent.VK_M);
            
            Thread.sleep(100);
            
            // Help button
            robot.keyPress(java.awt.event.KeyEvent.VK_SHIFT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_TAB);
            robot.keyRelease(java.awt.event.KeyEvent.VK_TAB);
            
            robot.keyRelease(java.awt.event.KeyEvent.VK_SHIFT);
            
            robot.keyPress(java.awt.event.KeyEvent.VK_ENTER);
            robot.keyRelease(java.awt.event.KeyEvent.VK_ENTER);            

            Thread.sleep(100);
        }
        catch (java.awt.AWTException e) {
            System.out.println("AWTException");
        }
        catch (InterruptedException e) {
            System.out.println("InterruptedException");
        }
        catch (Exception e) {
            System.out.println("Exception");
            e.printStackTrace();
        }
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
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/testRestart", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            
            // show postprocessor
            vs2FrameManager frameManager =
                    (vs2FrameManager) vs2App.theApp.getFrame().getManager();
            assertNotEquals(null, frameManager);
            frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();            

            Thread.sleep(100);
            
            // click step twice
            vs2PostProcessorFrame frame = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
            assertNotEquals(frame, null);
            frame.getStepButton().doClick();
            frame.getStepButton().doClick();
            
            Thread.sleep(100);
            
            // answer yes to restart
            {
                java.util.Properties props = System.getProperties();
                props.put("onRestartComputation", "Yes");

                // Action->Restart computation
                // This causes EXCEPTION_ACCESS_VIOLATION (-r 11503)
                // in vs2.vs2drt.getSoluteTransportMassBalanceErrors                
                frame.getRestartComputationMenuItem().doClick();

                Thread.sleep(250);
            }
        }
        catch (InterruptedException e) {
            System.out.println("InterruptedException");
        }
        catch (Exception e) {
            System.out.println("Exception");
        }
        finally {
            java.util.Properties props = System.getProperties();
            props.remove("onRestartComputation");
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
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/testRunResetRestartYesThenNo", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            

            // show postprocessor
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
            Thread.sleep(100);
            
            // answer yes to restart
            {
                java.util.Properties props = System.getProperties();
                props.put("onRestartComputation", "Yes");

                // Action->Restart computation
                frame.getRestartComputationMenuItem().doClick();

                // verify items
                Thread.sleep(100);
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
            }

            // answer no to restart
            {
                java.util.Properties props = System.getProperties();
                props.put("onRestartComputation", "No");

                // Action->Restart computation
                frame.getRestartComputationMenuItem().doClick();
            }

            // verify items
            Thread.sleep(100);
            assertEquals(15, frame.getDisplayChooser().getItemCount());
        }
        catch (InterruptedException e) {
            System.out.println("InterruptedException");
        }
        catch (Exception e) {
            System.out.println("Exception");
        }
        finally {
            java.util.Properties props = System.getProperties();
            props.remove("onRestartComputation");
        }
    }
    
    /**
     * Test testCleanup 
     */
    //@Ignore
    @Test
    public void testCleanup() {
        System.out.println("testCleanup");
        
        try {
            vs2App.main(null);
        
            // open ex11.vs2
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/testCleanup", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            
            // show postprocessor
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
            
            // change chooser
            frame.getDisplayChooser().setSelectedIndex(0);  // crashed on r12049-r12598
            
            // verify items
            Thread.sleep(100);
            assertEquals(15, frame.getDisplayChooser().getItemCount());
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
        
        // seems to always fail in jenkins
        if (System.getenv("USERDOMAIN").compareTo("JENKINS-WIN7") == 0) return;
        
        System.out.println("testPostCloseOpenPost");
        
        try {
            vs2App.main(null);
        
            // open ex11.vs2
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/testPostCloseOpenPost", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            
            // show postprocessor
            // get frameManager
            vs2FrameManager frameManager =
                    (vs2FrameManager) vs2App.theApp.getFrame().getManager();
            assertNotEquals(null, frameManager);
            frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();            
            
            Thread.sleep(100);
            
            vs2PostProcessorFrame postProcessorFrame = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
            assertNotEquals(null, postProcessorFrame);

            // Action->Done->(The computation is not finished. Do you want to quit anyway?)->Yes
            {
                java.util.Properties props = System.getProperties();
                props.put("quitOK", "Yes");

                JMenuItem doneMenuItem = postProcessorFrame.getExitMenuItem();
                assertNotEquals(null, doneMenuItem);
                assertEquals(true, doneMenuItem.isEnabled());
                doneMenuItem.doClick(); 

                Thread.sleep(100);
            }

            // open ex11-test.vs2
            java.nio.file.Path path2 = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/testPostCloseOpenPost", "ex11-test.vs2");
            assertEquals(true, java.nio.file.Files.exists(path2));
            
            java.io.File inFile2 = new java.io.File(path2.toString());
            vs2App.theApp.openFile(inFile2);

            Thread.sleep(100);
            
            frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();
            
            // Action->Done->(The computation is not finished. Do you want to quit anyway?)->Yes
            {
                java.util.Properties props = System.getProperties();
                props.put("quitOK", "Yes");

                JMenuItem doneMenuItem = postProcessorFrame.getExitMenuItem();
                assertNotEquals(null, doneMenuItem);
                assertEquals(true, doneMenuItem.isEnabled());
                doneMenuItem.doClick(); 

                Thread.sleep(100);
            }
        }
        catch (InterruptedException e) {
            System.out.println("InterruptedException");
        }
        catch (Exception e) {
            System.out.println("Exception");
        }
        finally {
            java.util.Properties props = System.getProperties();
            props.remove("quitOK");
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
