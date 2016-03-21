package mp2;

import javax.swing.*;

/**
 * An abstract class that you subclass to perform
 * GUI-related work in a dedicated thread.
 * For instructions on using this class, see 
 * http://java.sun.com/products/jfc/swingdoc/threads.html
 * This version has an extra method called <code>interrupt()</code>,
 * see the Example1 class below for more information.
 */
public abstract class mp2SwingWorker 
{
    private Object value;
    private Thread thread;

    /** 
     * Compute the value to be returned by the <code>get</code> method. 
     */
    public abstract Object construct();


    /**
     * Called on the event dispatching thread (not on the worker thread)
     * after the <code>construct</code> method has returned.
     */
    public void finished() {
    }


    /**
     * A new method that interrupts the worker thread.  Call this method
     * to force the worker to abort what it's doing.
     */
    public void interrupt() {
    Thread t = thread;
    if (t != null) {
        t.interrupt();
    }
    thread = null;
    }


    /**
     * Return the value created by the <code>construct</code> method.  
     */
    public Object get() {
        while (true) {  // keep trying if we're interrupted
            Thread t;
            synchronized (mp2SwingWorker.this) {
                t = thread;
                if (t == null) {
                    return value;
                }
            }
            try {
                t.join();
            }
            catch (InterruptedException e) {
            }
        }
    }

    /**
     * Start a thread that will call the <code>construct</code> method
     * and then exit.
     */
    public mp2SwingWorker() {
        final Runnable doFinished = new Runnable() {
           public void run() { finished(); }
        };

        Runnable doConstruct = new Runnable() { 
            public void run() {
                synchronized(mp2SwingWorker.this) {
                    value = construct();
                    thread = null;
                }
                SwingUtilities.invokeLater(doFinished);
            }
        };

        thread = new Thread(doConstruct);
        thread.start();
    }

}
