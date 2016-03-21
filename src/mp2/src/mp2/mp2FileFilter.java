/*
 * vs2FileFilter.java
 */
package mp2;
import java.io.File;
import javax.swing.filechooser.*;import java.util.Vector;

public class mp2FileFilter extends FileFilter {        protected Vector extensions;    protected String description;        public mp2FileFilter() {        extensions = new Vector();        description = null;    }        public void setDescription(String d) {        description = d;    }        public void addExtension(String e) {
        if (e != null) {            extensions.addElement(e.toLowerCase());
        }    }
    
    // Accept all directories and vs2 files.
    public boolean accept(File f) {
        if (f.isDirectory()) {
            return true;
        }

        String ext = null;
        String s = f.getName();
        int i = s.lastIndexOf('.');
        if (i > 0 &&  i < s.length() - 1) {
            ext = s.substring(i+1).toLowerCase();
        }
        if (ext != null) {            for (i=0; i<extensions.size(); i++) {                if (ext.equals((String) extensions.elementAt(i))) {
                    return true;
                }
            }
    	}

        return false;
    }
    
    // The description of this filter
    public String getDescription() {
        return description;
    }
}
