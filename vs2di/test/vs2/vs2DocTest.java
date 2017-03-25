/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vs2;

import java.io.IOException;
import java.io.PrintWriter;
import mp2.mp2App;
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
public class vs2DocTest {
    
    public vs2DocTest() {
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
//     * Test of convertToCurrentVersion method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testConvertToCurrentVersion() {
//        System.out.println("convertToCurrentVersion");
//        vs2Doc instance = new vs2Doc();
//        instance.convertToCurrentVersion();
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of getUsage method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testGetUsage() {
//        System.out.println("getUsage");
//        vs2Doc instance = new vs2Doc();
//        int expResult = 0;
//        int result = instance.getUsage();
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of exportData method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testExportData() {
//        System.out.println("exportData");
//        String directory = "";
//        String file = "";
//        vs2Doc instance = new vs2Doc();
//        int expResult = 0;
//        int result = instance.exportData(directory, file);
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of writeData method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testWriteData() {
//        System.out.println("writeData");
//        PrintWriter pw = null;
//        vs2Doc instance = new vs2Doc();
//        instance.writeData(pw);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of init method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testInit() {
//        System.out.println("init");
//        mp2App theApp = null;
//        vs2Doc instance = new vs2Doc();
//        instance.init(theApp);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of editModelOptions method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testEditModelOptions() {
//        System.out.println("editModelOptions");
//        vs2Doc instance = new vs2Doc();
//        instance.editModelOptions();
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of getCurrentVersion method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testGetCurrentVersion() {
//        System.out.println("getCurrentVersion");
//        String expResult = "";
//        String result = vs2Doc.getCurrentVersion();
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of getData method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testGetData() {
//        System.out.println("getData");
//        int dataId = 0;
//        vs2Doc instance = new vs2Doc();
//        Object expResult = null;
//        Object result = instance.getData(dataId);
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of readyToExport method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testReadyToExport() {
//        System.out.println("readyToExport");
//        vs2Doc instance = new vs2Doc();
//        boolean expResult = false;
//        boolean result = instance.readyToExport();
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of setChanged method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testSetChanged() {
//        System.out.println("setChanged");
//        boolean b = false;
//        vs2Doc instance = new vs2Doc();
//        instance.setChanged(b);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of doNonlinearSorption method, of class vs2Doc.
//     */
//    @Ignore
//    @Test
//    public void testDoNonlinearSorption() {
//        System.out.println("doNonlinearSorption");
//        vs2Doc instance = new vs2Doc();
//        boolean expResult = false;
//        boolean result = instance.doNonlinearSorption();
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        //fail("The test case is a prototype.");
//    }
    
    /**
     * Test of FlowExample1 method, of class vs2Doc.
     */
    @Test
    public void testFlowExample1()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("flow_example1");        
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/flow/example1/example1.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        //if (true) return;
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/flow/example1", masterFile);
        
//        String expResult = "fc6c18357d4d1ba1516a3fd3eea4d645";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/flow/example1/vs2drt.dat");
//        org.junit.Assert.assertEquals(expResult, result);        
    }
    /**
     * Test of FlowExample2 method, of class vs2Doc.
     */
    @Test
    public void testFlowExample2()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("flow_example2");        
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/flow/example2/example2.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        //if (true) return;
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/flow/example2", masterFile);
        
//        String expResult = "56a44267b0c0289e002cdd761fdcab84";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/flow/example2/vs2drt.dat");
//        org.junit.Assert.assertEquals(expResult, result);        
    }
    /**
     * Test of FlowExample3 method, of class vs2Doc.
     */
    @Test
    public void testFlowExample3()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("flow_example3");        
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/flow/example3/example3.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        //if (true) return;
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/flow/example3", masterFile);
        
//        String expResult = "f428b5c012cdd72a67343f32d24f1206";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/flow/example3/vs2drt.dat");
//        org.junit.Assert.assertEquals(expResult, result);        
    }
    
    
    /**
     * Test of SoluteExample1 method, of class vs2Doc.
     */
    @Ignore
    @Test
    public void testSoluteExample1()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("solute_example1");        
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/solute/example1/example1.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        //if (true) return;
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/solute/example1", masterFile);
        
//        String expResult = "39982436321edbaa8211bd9c0bee9ecb";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/solute/example1/vs2drt.dat");
//        org.junit.Assert.assertEquals(expResult, result);        
    }
    
    /**
     * Test of SoluteExample2 method, of class vs2Doc.
     */
    @Ignore
    @Test
    public void testSoluteExample2()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("solute_example2");        
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/solute/example2/example2.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        //if (true) return;
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/solute/example2", masterFile);
        
//        String expResult = "22def5dc17f61207fdb38e2716b31cfb";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/solute/example2/vs2drt.dat");
//        assertEquals(expResult, result);        
    }
    
    /**
     * Test of SoluteExample3 method, of class vs2Doc.
     */
    @Ignore
    @Test
    public void testSoluteExample3()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("solute_example3");        
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/solute/example3/example3.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/solute/example3", masterFile);
        
//        String expResult = "9207727c81acb7fdb7c9d38b98b34d23";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/solute/example3/vs2drt.dat");
//        assertEquals(expResult, result);        
    }
    
    /**
     * Test of SoluteExample4 method, of class vs2Doc.
     */
    @Ignore
    @Test
    public void testSoluteExample4()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("solute_example4");        
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/solute/example4/example4.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        //if (true) return;
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/solute/example4", masterFile);
        
//        String expResult = "e184c5f09cd38c49afc822b0f28b0188";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/solute/example4/vs2drt.dat");
//        assertEquals(expResult, result);        
    }
    
    /**
     * Test of SoluteExample5 method, of class vs2Doc.
     */
    @Ignore
    @Test
    public void testSoluteExample5()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("solute_example5");        
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/solute/example5/example5.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        //if (true) return;
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/solute/example5", masterFile);
        
//        String expResult = "ff55725e02012b643c8d23e5fa9cbc44";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/solute/example5/vs2drt.dat");
//        assertEquals(expResult, result);        
    }
    
    /**
     * Test of EnergyExample1 method, of class vs2Doc.
     */
    //@Ignore
    @Test
    public void testEnergyExample1()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("energy_example1");
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/energy/example1/example1.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/energy/example1", masterFile);
        
//        String expResult = "b52e9b83a6d7cd2ca48337a1459cab89";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/energy/example1/vs2drt.dat");
//        assertEquals(expResult, result);        
    }
    
    
    /**
     * Test of EnergyExample2 method, of class vs2Doc.
     */
    //@Ignore
    @Test
    public void testEnergyExample2()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("energy_example2");
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/energy/example2/example2.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/energy/example2", masterFile);
        
//        String expResult = "46203beca71fa5e6acc64253ed1eb276";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/energy/example2/vs2drt.dat");
//        assertEquals(expResult, result);        
    }
    
    /**
     * Test of EnergyExample3 method, of class vs2Doc.
     */
    //@Ignore
    @Test
    public void testEnergyExample3()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("energy_example3");
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/energy/example3/example3.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/energy/example3", masterFile);
        
//        String expResult = "69a3d5f98de978b6ef664d10ea9e4fb3";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/energy/example3/vs2drt.dat");
//        assertEquals(expResult, result);        
    }

    /**
     * Test of EnergyExample3 method, of class vs2Doc.
     */
    //@Ignore
    @Test
    public void testEnergyTutorial1()  throws Exception, IOException, ClassNotFoundException {
        System.out.println("energy_tutorial1");
        
        // Create an input stream for object deserialization.
        String inFile;
        inFile = "../vs2di1.3_examples/energy/tutorial1/tutorial1.vs2";
        java.io.FileInputStream input = new java.io.FileInputStream(inFile);
        java.io.ObjectInputStream objectIn = new java.io.ObjectInputStream(input);
        
        vs2App app = new vs2App();
        
        // Create the document by deserialization from file
        String masterFile = vs2App.getFilePrefix() + ".fil";
        vs2Doc doc = (vs2Doc) objectIn.readObject();
        doc.init(app);        
        
        assertTrue(doc.readyToExport());     
        doc.exportData("../vs2di1.3_examples/energy/tutorial1", masterFile);
        
//        String expResult = "d8c8a6e8361f5d2f30d5bd617d16551c";
//        String result = util.MD5Checksum.getMD5Checksum("../vs2di1.3_examples/energy/tutorial1/vs2drt.dat");
//        assertEquals(expResult, result);        
    }
}
