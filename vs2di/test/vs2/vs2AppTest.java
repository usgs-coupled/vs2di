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
    @Test
    public void testCreateDoc() {
        System.out.println("createDoc");
        vs2App instance = new vs2App();
        vs2Doc doc = (vs2Doc)instance.createDoc();
    }

    /**
     * Test of createView method, of class vs2App.
     */
    @Test
    public void testCreateView() {
        System.out.println("createView");
        vs2App instance = new vs2App();
        vs2View view = (vs2View)instance.createView();
    }

    /**
     * Test of createFrameManager method, of class vs2App.
     */
    @Test
    public void testCreateFrameManager() {
        System.out.println("createFrameManager");
        vs2App instance = new vs2App();
        vs2FrameManager result = (vs2FrameManager)instance.createFrameManager();
    }

    /**
     * Test of createPostProcessorFrame method, of class vs2App.
     */
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
    @Test
    public void testMain() {
        System.out.println("main");
        String[] args = new String[0];
        vs2App.main(args);
    }

    /**
     * Test of typeCheck method, of class vs2App.
     */
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
    @Test
    public void testVersionCheck() {
        System.out.println("versionCheck");
        vs2App instance = new vs2App();
        mp2Doc doc = (vs2Doc)instance.createDoc();
        boolean expResult = true;
        boolean result = instance.versionCheck(doc);
        assertEquals(expResult, result);
    }
    
  
    
    
}
