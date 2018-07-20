/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vs2;

import java.io.BufferedReader;
import java.io.PrintWriter;
import javax.swing.JMenuItem;
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

    /**
     * Test of onExit method, of class vs2PostProcessorFrame.
     */
    //@Ignore
    @Test
    public void testOnExit() {
        System.out.println("testOnExit");
        try {
            vs2App.main(null);
            
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/testOnExit", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            
            // show postprocessor
            vs2FrameManager frameManager =
                    (vs2FrameManager) vs2App.theApp.getFrame().getManager();
            frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();

            Thread.sleep(100);

            // click step
            vs2PostProcessorFrame instance = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
            assertNotEquals(null, instance);
            instance.getStepButton().doClick();

            Thread.sleep(100);

            // answer no (The computation is not finished. Do you want to quit anyway?)
            {
                java.util.Properties props = System.getProperties();
                props.put("quitOK", "No");

                // Action->Done
                JMenuItem done = instance.getExitMenuItem();
                assertNotEquals(null, done);
                assertEquals(true, done.isEnabled());
                done.doClick();

                Thread.sleep(100);

                instance.getStepButton().doClick();    // this used to cause an EXCEPTION_ACCESS_VIOLATION

                Thread.sleep(100);
            }

            // answer yes (The computation is not finished. Do you want to quit anyway?)
            {
                java.util.Properties props = System.getProperties();
                props.put("quitOK", "Yes");

                // Action->Done
                JMenuItem done = instance.getExitMenuItem();
                assertNotEquals(null, done);
                assertEquals(true, done.isEnabled());
                done.doClick();

                Thread.sleep(100);
            }
        }
        catch (InterruptedException e) {
        }
    }
    
    /**
     * Test of onExit method, of class vs2PostProcessorFrame.
     */
    //@Ignore
    @Test
    public void testRunEx11ThenFlowEx1() {
        System.out.println("testRunEx11ThenFlowEx1");
        try {
            vs2App.main(null);
            
            // open ex11.vs2
            java.nio.file.Path path = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/Example11/testRunEx11ThenFlowEx1", "ex11.vs2");
            assertEquals(true, java.nio.file.Files.exists(path));
            
            java.io.File inFile = new java.io.File(path.toString());
            vs2App.theApp.openFile(inFile);
            
            // show postprocessor
            vs2FrameManager frameManager =
                    (vs2FrameManager) vs2App.theApp.getFrame().getManager();
            frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();
            
            Thread.sleep(100);
            
            // click step
            vs2PostProcessorFrame instance = (vs2PostProcessorFrame)vs2App.theApp.getPostProcessorFrame();
            assertNotEquals(instance, null);
            instance.getStepButton().doClick();
            instance.getStepButton().doClick();
            
            Thread.sleep(100);
            
            // answer yes (The computation is not finished. Do you want to quit anyway?)
            {
                java.util.Properties props = System.getProperties();
                props.put("quitOK", "Yes");

                // Action->Done
                JMenuItem done = instance.getExitMenuItem();
                assertNotEquals(null, done);
                assertEquals(true, done.isEnabled());
                done.doClick();

                Thread.sleep(100);

                // open flow/ex1
                java.nio.file.Path path2 = java.nio.file.Paths.get(System.getProperty("user.dir"), "../vs2di1.3_examples/flow/testRunEx11ThenFlowEx1", "example1.1_4.vs2");
                assertEquals(true, java.nio.file.Files.exists(path2));

                java.io.File inFile2 = new java.io.File(path2.toString());
                vs2App.theApp.openFile(inFile2);

                Thread.sleep(100);

                // show postprocessor (this used to cause an exception in vs2drtJni
                frameManager.getMenuItem(mp2.mp2Constants.POST_PROCESSOR).doClick();

                Thread.sleep(100);

                // Action->Done
                done = instance.getExitMenuItem();
                assertNotEquals(null, done);
                assertEquals(true, done.isEnabled());
                done.doClick();

                Thread.sleep(1000);
            }
        }
        catch (InterruptedException e) {
        }
    }
}
