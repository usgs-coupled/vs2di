/*
 * mp2SplashScreen
 */
package mp2;

import java.awt.*;
import java.awt.event.*;

public class mp2SplashScreen extends Window {
    
    private Image splashImage;
    private int splashWidth;
    private int splashHeight;
    private String imageFileName;
    private static final int BORDERSIZE = 0;
    private static final Color BORDERCOLOR = Color.lightGray;
    Toolkit tk;
      
    public mp2SplashScreen(Frame owner, String imageFileName) {
        super(owner);
        this.imageFileName = imageFileName;
        tk = Toolkit.getDefaultToolkit();
        splashImage = loadSplashImage();
        showSplashScreen();
        owner.addWindowListener(new WindowListener());
    }
    
    public mp2SplashScreen(Frame owner, java.net.URL imageURL) {
        super(owner);
        this.imageFileName = imageURL.toString();
        tk = Toolkit.getDefaultToolkit();
        splashImage = loadSplashImage(imageURL);
        showSplashScreen();
        owner.addWindowListener(new WindowListener());
    }    
    
    public Image loadSplashImage() {
        MediaTracker tracker = new MediaTracker(this);
        Image result;
        result = tk.getImage(imageFileName);
        tracker.addImage(result, 0);
        try { 
            tracker.waitForAll(); 
        } catch (Exception e) {
            e.printStackTrace();
        }
        splashWidth = result.getWidth(this);
        splashHeight = result.getHeight(this);
        return (result);
    }
    
    public Image loadSplashImage(java.net.URL url) {
        MediaTracker tracker = new MediaTracker(this);
        Image result;
        result = tk.getImage(url);
        tracker.addImage(result, 0);
        try { 
            tracker.waitForAll(); 
        } catch (Exception e) {
            e.printStackTrace();
        }
        splashWidth = result.getWidth(this);
        splashHeight = result.getHeight(this);
        return (result);
    }
    

    public void showSplashScreen() {
        Dimension screenSize = tk.getScreenSize();
        setBackground(BORDERCOLOR);
        int w = splashWidth + (BORDERSIZE * 2);
        int h = splashHeight + (BORDERSIZE * 2);
        int x = (screenSize.width - w) /2;
        int y = (screenSize.height - h) /2;
        setBounds(x, y, w, h);
        setVisible(true);
    }

    public void paint(Graphics g) {
        g.drawImage(splashImage, BORDERSIZE, BORDERSIZE,
            splashWidth, splashHeight, this);
    }

    class WindowListener extends WindowAdapter {
        public void windowActivated(WindowEvent we) {
            setVisible(false);
            dispose();
        }
    }
}
