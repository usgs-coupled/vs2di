/*
 * mp2JavaHelp.java
 */
package mp2;

import javax.help.*;
import java.net.URL;

/**
 * Display help pages
 */
public class mp2JavaHelp {

    public static HelpSet hs = null;
    public static HelpBroker hb;

    public static void initialize(String helpHS) {
        // Find the HelpSet file and create the HelpSet object:
        ClassLoader cl = mp2App.class.getClassLoader();
        try {
            URL hsURL = HelpSet.findHelpSet(cl, helpHS);
            hs = new HelpSet(cl, hsURL);
        } catch (Exception ee) {
            // Say what the exception really is
            System.out.println( "HelpSet " + ee.getMessage());
            System.out.println("HelpSet "+ helpHS +" not found");
            //return;
        }
        // Create a HelpBroker object:
        hb = hs.createHelpBroker();
    }
}

