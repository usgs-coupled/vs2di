/*
 * mp2BMPFile.java
 */
package mp2;

import java.awt.*;
import java.io.*;
import java.awt.image.*;

public class mp2BMPFile extends Component {

  //--- Private constants
  private final static int BITMAPFILEHEADER_SIZE = 14;
  private final static int BITMAPINFOHEADER_SIZE = 40;

  //--- Private variable declaration

  //--- Bitmap file header
  private byte bitmapFileHeader [] = new byte [14];
  private byte bfType [] = {'B', 'M'};
  // replace the above line with the next line in jdk1.2
  //private byte bfType [] = {(byte)'B', (byte)'M'};
  private int bfSize = 0;
  private int bfReserved1 = 0;
  private int bfReserved2 = 0;
  private int bfOffBits = BITMAPFILEHEADER_SIZE + BITMAPINFOHEADER_SIZE;

  //--- Bitmap info header
  private byte bitmapInfoHeader [] = new byte [40];
  private int biSize = BITMAPINFOHEADER_SIZE;
  private int biWidth = 0;
  private int biHeight = 0;
  private int biPlanes = 1;
  private int biBitCount = 24;
  private int biCompression = 0;
  private int biSizeImage = 0x030000;
  private int biXPelsPerMeter = 0x0;
  private int biYPelsPerMeter = 0x0;
  private int biClrUsed = 0;
  private int biClrImportant = 0;
  private int resolution = 0;

  //--- Bitmap raw data
  private int bitmap [];

  //--- File section
  private DataOutputStream fo;

  //--- Default constructor
  public mp2BMPFile() {

  }

  public void saveBitmap (String parFilename, Image parImage, int parWidth, 
            int parHeight, int resolution) {
    this.resolution = resolution;
     try {
        fo = new DataOutputStream(new BufferedOutputStream(
                    new FileOutputStream(new File(parFilename))));
        save (parImage, parWidth, parHeight);
        fo.close ();
     }
     catch (Exception saveEx) {
        saveEx.printStackTrace ();
     }

  }
  
  public void saveBitmap(String parFilename, Image parImage, int parWidth, 
                    int parHeight) {
      saveBitmap(parFilename, parImage, parWidth, parHeight, 0);
  }

  /*
   *  The saveMethod is the main method of the process. This method
   *  will call the convertImage method to convert the memory image to
   *  a byte array; method writeBitmapFileHeader creates and writes
   *  the bitmap file header; writeBitmapInfoHeader creates the
   *  information header; and writeBitmap writes the image.
   *
   */
  private void save (Image parImage, int parWidth, int parHeight) {

     try {
        convertImage (parImage, parWidth, parHeight);
        writeBitmapFileHeader ();
        writeBitmapInfoHeader ();
        writeBitmap ();
     }
     catch (Exception saveEx) {
        saveEx.printStackTrace ();
     }
  }

  /*
   * convertImage converts the memory image to the bitmap format (BRG).
   * It also computes some information for the bitmap info header.
   *
   */
  private boolean convertImage (Image parImage, int parWidth, int parHeight) {

     int pad;
     bitmap = new int [parWidth * parHeight];

     PixelGrabber pg = new PixelGrabber (parImage, 0, 0, parWidth, parHeight,
                                         bitmap, 0, parWidth);

     try {
        pg.grabPixels ();
     }
     catch (InterruptedException e) {
        e.printStackTrace ();
        return (false);
     }

     pad = (4 - ((parWidth * 3) % 4)) % 4;
     biSizeImage = (parWidth * 3 + pad) * parHeight;
     bfSize = biSizeImage + BITMAPFILEHEADER_SIZE + BITMAPINFOHEADER_SIZE;
     biWidth = parWidth;
     biHeight = parHeight;

     return (true);
  }

  /*
   * writeBitmap converts the image returned from the pixel grabber to
   * the format required. Remember: scan lines are inverted in
   * a bitmap file!
   *
   * Each scan line must be padded to an even 4-byte boundary.
   */
  private void writeBitmap () {

      int value, i, j, k, offset, pad;
      byte rgb [] = new byte [3];

      pad = (4 - ((biWidth * 3) % 4)) % 4;
      try {
        for (j = biHeight-1; j >= 0; j--) {
            offset = j*biWidth;
            for (i = 0; i<biWidth; i++) {
                value = bitmap[offset + i];
                rgb [0] = (byte) (value & 0xFF);
                rgb [1] = (byte) ((value >> 8) & 0xFF);
                rgb [2] = (byte) ((value >> 16) & 0xFF);
                fo.write (rgb);
            }
            for (k=0; k<pad; k++) {
                fo.write(0x00);
            }
        }
        fo.flush();      
      }
      catch (Exception wb) {
         wb.printStackTrace ();
      }
   }

  /*
   * writeBitmapFileHeader writes the bitmap file header to the file.
   *
   */
  private void writeBitmapFileHeader () {

     try {
        fo.write (bfType);
        fo.write (intToDWord (bfSize));
        fo.write (intToWord (bfReserved1));
        fo.write (intToWord (bfReserved2));
        fo.write (intToDWord (bfOffBits));

     }
     catch (Exception wbfh) {
        wbfh.printStackTrace ();
     }

  }

  /*
   *
   * writeBitmapInfoHeader writes the bitmap information header
   * to the file.
   *
   */

  private void writeBitmapInfoHeader () {

      if (resolution == 1) {
          biXPelsPerMeter = 5904;   // 150 ppi
          biYPelsPerMeter = 5904;
      } else if (resolution == 2) {
          biXPelsPerMeter = 11808;	// 300 ppi;
          biYPelsPerMeter = 11808;
      }
     try {
        fo.write (intToDWord (biSize));
        fo.write (intToDWord (biWidth));
        fo.write (intToDWord (biHeight));
        fo.write (intToWord (biPlanes));
        fo.write (intToWord (biBitCount));
        fo.write (intToDWord (biCompression));
        fo.write (intToDWord (biSizeImage));
        fo.write (intToDWord (biXPelsPerMeter));
        fo.write (intToDWord (biYPelsPerMeter));
        fo.write (intToDWord (biClrUsed));
        fo.write (intToDWord (biClrImportant));
     }
     catch (Exception wbih) {
        wbih.printStackTrace ();
     }

  }

  /*
   *
   * intToWord converts an int to a word, where the return
   * value is stored in a 2-byte array.
   *
   */
  private byte [] intToWord (int parValue) {

     byte retValue [] = new byte [2];

     retValue [0] = (byte) (parValue & 0x00FF);
     retValue [1] = (byte) ((parValue >> 8) & 0x00FF);

     return (retValue);

  }

  /*
   *
   * intToDWord converts an int to a double word, where the return
   * value is stored in a 4-byte array.
   *
   */
  private byte [] intToDWord (int parValue) {

     byte retValue [] = new byte [4];

     retValue [0] = (byte) (parValue & 0x00FF);
     retValue [1] = (byte) ((parValue >> 8) & 0x000000FF);
     retValue [2] = (byte) ((parValue >> 16) & 0x000000FF);
     retValue [3] = (byte) ((parValue >> 24) & 0x000000FF);

     return (retValue);

  }

}

