import java.awt.GridLayout;
import java.awt.image.BufferedImage;
import java.io.File;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

/**
 *
 * @author benland100
 */
public class EdgeDetector {

    public static void main(String[] args) throws Exception {
        EdgeDetector edge = new EdgeDetector(0,0.0,DetectionType.Fast,false,0,0);
        BufferedImage orig = ImageIO.read(new File("/home/benland100/Pictures/theone.jpg"));
        int[] imageData = new int [orig.getWidth() * orig.getHeight()];
        orig.getRGB(0,0,orig.getWidth(),orig.getHeight(),imageData,0,orig.getWidth());
        imageData = IntensitytoRGB(edge.process(RGBtoIntensity(imageData), orig.getWidth(), orig.getHeight()));
        BufferedImage img = new BufferedImage(orig.getWidth(), orig.getHeight(), BufferedImage.TYPE_INT_RGB);
        img.setRGB(0,0,img.getWidth(),img.getHeight(),imageData,0,img.getWidth());
        JFrame frame = new JFrame("EdgeDetection");
        frame.setLayout(new GridLayout());
        frame.setDefaultCloseOperation(frame.EXIT_ON_CLOSE);
        frame.add(new JLabel(new ImageIcon(orig)));
        frame.add(new JLabel(new ImageIcon(img)));
        frame.pack();
        frame.setVisible(true);
    }

    /**
     * Converts rgb data to intensity data [0,255]
     * @param rgb RGB data
     * @return Intensity data
     */
    public static int[] RGBtoIntensity(int[] rgb) {
        int[] intensity = new int[rgb.length];
        for (int i = 0; i < rgb.length; i++)
            intensity[i] = (((rgb[i] & 0xFF0000) >> 16) + ((rgb[i] & 0xFF00) >> 8) + (rgb[i] & 0xFF)) / 3;
        return intensity;
    }

    /**
     * Converts intensity data [0,255] to RGB data
     * @param intensity Intensity data
     * @return RGB data
     */
    public static int[] IntensitytoRGB(int[] intensity) {
        int[] rgb = new int[intensity.length];
        for (int i = 0; i < intensity.length; i++)
            rgb[i] = (((intensity[i] & 0xFF) << 16) | ((intensity[i] & 0xFF) << 8) | (intensity[i] & 0xFF));
        return rgb;
    }

    /**
     * Takes an int array and a max value and normalizes the array
     * to the range [0,max]
     * @param data Array to normalize
     * @param max Range specifier [0,max]
     */
    public static void normalize(int[] data, int max) {
        int high = Integer.MIN_VALUE;
        int low = Integer.MAX_VALUE;
        for (int i = 0; i < data.length; i++) {
            if (high < data[i]) high = data[i];
            if (low > data[i]) low = data[i];
        }
        System.out.println("["+low+","+high+"]");
        int range = high - low;
        if (range == 0) return;
        for (int i = 0; i < data.length; i++) {
            data[i] = (data[i] - low) * max / range;
        }
    }

    /**
     * Implements the Sobel, Scharr, and a Fast edge detecting algorithm
     */
    public enum DetectionType { 
        Sobel {
            int[][] process(int[] intensity, int w, int h) {
                int we = w - 1, he = h - 1;
                int[] dx = new int[w*h];
                int[] dy = new int[w*h];
                for (int y = 0, i = 1; y < h; y++, i+=2) {
                    for (int x = 1; x < we; x++, i++) {
                        dx[i] = intensity[i+1] - intensity[i-1];
                        dy[i] = intensity[i-1] + 2*intensity[i] + intensity[i+1];
                    }
                }
                int[] mags = new int[w*h];
                int[] dir = new int[w*h];
                int xmag,ymag;
                for (int y = 1, i = w; y < he; y++) {
                    for (int x = 0; x < w; x++, i++) {
                        xmag = dx[i-w] + 2*dx[i] + dx[i+w];
                        ymag = dy[i+w] - dy[i-w];
                        mags[i] = Math.abs(xmag) + Math.abs(ymag);
                        if (ymag > 0) {
                            if (xmag > 0) {
                                if (ymag > 0.414 * xmag) {
                                    dir[i] = 0;
                                } else if (ymag < 2.414 * xmag) {
                                    dir[i] = 2;
                                } else {
                                    dir[i] = 1;
                                }
                            } else {
                                if (ymag < 0.414 * xmag) {
                                    dir[i] = 4;
                                } else if (ymag > 2.414 * xmag) {
                                    dir[i] = 2;
                                } else {
                                    dir[i] = 3;
                                }
                            }
                        } else {
                            if (xmag > 0) {
                                if (ymag < 0.414 * xmag) {
                                    dir[i] = 0;
                                } else if (ymag > 2.414 * xmag) {
                                    dir[i] = 6;
                                } else {
                                    dir[i] = 7;
                                }
                            } else {
                                if (ymag > 0.414 * xmag) {
                                    dir[i] = 4;
                                } else if (ymag < 2.414 * xmag) {
                                    dir[i] = 6;
                                } else {
                                    dir[i] = 5;
                                }
                            }
                        }
                    }
                }
                return new int[][] {mags,dir};
            }
        }, 
        Fast {
            int[][] process(int[] intensity, int w, int h) {
                int[] mags = new int[w*h];
                int[] dir = new int[w*h];
                int we = w - 1, he = h - 1;
                int i = w + 1;
                int xmag,ymag;
                for (int y = 1; y < he; y++, i+=2) {
                    for (int x = 1; x < we; x++, i++) {
                        xmag = (intensity[i+1] - intensity[i-1]);
                        ymag = (intensity[i+w] - intensity[i-w]);
                        mags[i] = Math.abs(xmag) + Math.abs(ymag);
                        if (ymag > 0) {
                            if (xmag > 0) {
                                if (ymag > 0.414 * xmag) {
                                    dir[i] = 0;
                                } else if (ymag < 2.414 * xmag) {
                                    dir[i] = 2;
                                } else {
                                    dir[i] = 1;
                                }
                            } else {
                                if (ymag < 0.414 * xmag) {
                                    dir[i] = 4;
                                } else if (ymag > 2.414 * xmag) {
                                    dir[i] = 2;
                                } else {
                                    dir[i] = 3;
                                }
                            }
                        } else {
                            if (xmag > 0) {
                                if (ymag < 0.414 * xmag) {
                                    dir[i] = 0;
                                } else if (ymag > 2.414 * xmag) {
                                    dir[i] = 6;
                                } else {
                                    dir[i] = 7;
                                }
                            } else {
                                if (ymag > 0.414 * xmag) {
                                    dir[i] = 4;
                                } else if (ymag < 2.414 * xmag) {
                                    dir[i] = 6;
                                } else {
                                    dir[i] = 5;
                                }
                            }
                        }
                    }
                }
                return new int[][] {mags,dir};
            }
        },
        Scharr {
            int[][] process(int[] intensity, int w, int h) {
                int we = w - 1, he = h - 1;
                int[] dx = new int[w*h];
                int[] dy = new int[w*h];
                for (int y = 0, i = 1; y < h; y++, i+=2) {
                    for (int x = 1; x < we; x++, i++) {
                        dx[i] = intensity[i+1] - intensity[i-1];
                        dy[i] = 3*intensity[i-1] + 10*intensity[i] + 3*intensity[i+1];
                    }
                }
                int[] mags = new int[w*h];
                int[] dir = new int[w*h];
                int xmag,ymag;
                for (int y = 1, i = w; y < he; y++) {
                    for (int x = 0; x < w; x++, i++) {
                        xmag = 3*dx[i-w] + 10*dx[i] + 3*dx[i+w];
                        ymag = dy[i+w] - dy[i-w];
                        mags[i] = Math.abs(xmag) + Math.abs(ymag);
                        if (ymag > 0) {
                            if (xmag > 0) {
                                if (ymag > 0.414 * xmag) {
                                    dir[i] = 0;
                                } else if (ymag < 2.414 * xmag) {
                                    dir[i] = 2;
                                } else {
                                    dir[i] = 1;
                                }
                            } else {
                                if (ymag < 0.414 * xmag) {
                                    dir[i] = 4;
                                } else if (ymag > 2.414 * xmag) {
                                    dir[i] = 2;
                                } else {
                                    dir[i] = 3;
                                }
                            }
                        } else {
                            if (xmag > 0) {
                                if (ymag < 0.414 * xmag) {
                                    dir[i] = 0;
                                } else if (ymag > 2.414 * xmag) {
                                    dir[i] = 6;
                                } else {
                                    dir[i] = 7;
                                }
                            } else {
                                if (ymag > 0.414 * xmag) {
                                    dir[i] = 4;
                                } else if (ymag < 2.414 * xmag) {
                                    dir[i] = 6;
                                } else {
                                    dir[i] = 5;
                                }
                            }
                        }
                    }
                }
                return new int[][] {mags,dir};
            }
        };
        abstract int[][] process(int[] intensity, int w, int h);
    }
    
    private DetectionType detector;
    private boolean guassian, nonmaxima, trace;
    private int[] gaussKernel;
    private int gaussSize,gaussSum,low, high;

    /**
     * Creates an EdgeDetector that can be reused to save initilization time
     * @param gaussSize The size of the gaussian kernal (2 or greater) or zero for no gaussian
     * @param sigma The sigma for the guassian distribution, [0.5,2] works well
     * @param detector The type of detector to use
     * @param nonmaxima True if nonmaxima suppression should be used
     * @param low The minimum value that can continue an edge (if high is nonzero)
     * @param high Nonzero enables tracing, minimum value that can identify an edge
     */
    public EdgeDetector(int gaussSize, double sigma, DetectionType detector, boolean nonmaxima, int low, int high) {
        if (gaussSize != 0) {
            guassian = true;
            this.gaussSize = gaussSize;
            gaussKernel = new int[gaussSize*2-1];
            double min = gaussian(gaussSize,gaussSize,sigma);
            for (int x = 0; x < gaussSize; x++) {
                int val = (int)Math.round(gaussian(gaussSize-x,gaussSize-x,sigma)/min);
                gaussKernel[x] = val;
                gaussKernel[2*gaussSize-2-x] = val;
                gaussSum += x == gaussSize - 1 ? val : val * 2 ;
            }
        }
        this.detector = detector;
        this.nonmaxima = nonmaxima;
        if (low < high && high != 0) {
            trace = true;
            this.low = low;
            this.high = high;
        }
    }

    /**
     * Computes the gaussian distribution value given a point and sigma
     * @param x X coordinate
     * @param y Y coordinate
     * @param sigma Gaussian normal distribution sigma
     * @return Gaussian distribution value
     */
    private double gaussian(int x, int y, double sigma) {
        return Math.exp(-Math.sqrt(x*x+y*y) / (2D * sigma * sigma));
    }

    /**
     * Performs EdgeDetection on the intensity image with this instance's
     * parameters and settings.
     * @param intensity Each index is the intensity as an int
     * @param w Width of the image
     * @param h Height of the image
     * @return Edge detected & normalized [0,255]
     */
    public int[] process(int[] intensity, int w, int h) {
        int s = w * h;
        if (intensity.length != s) throw new RuntimeException("Invalid array size");
        if (guassian) {
            int[] blur = new int[s];
            int we = w - gaussSize + 1, he = h - gaussSize + 1;
            for (int y = 0, i = gaussSize - 1; y < h; y++, i+=(gaussSize-1)*2) {
                for (int x = gaussSize - 1; x < we; x++, i++) {
                    int val = intensity[i]*gaussKernel[gaussSize-1];
                    for (int j = 1; j < gaussSize; j++) {
                        val += intensity[i-gaussSize+j]*gaussKernel[j-1];
                    }
                    blur[i] = val / gaussSum;
                }
            }
            intensity = new int[s];
            for (int y = gaussSize - 1, i = y*w + gaussSize - 1; y < he; y++) {
                for (int x = 0; x < w; x++, i++) {
                    int val = intensity[i]*gaussKernel[gaussSize-1];
                    for (int j = 1; j < gaussSize; j++) {
                        val += blur[i-(gaussSize-j)*w]*gaussKernel[j-1];
                    }
                    intensity[i] = val / gaussSum;
                }
            }
        }
        int[][] data = detector.process(intensity,w,h);
        int[] mags = data[0];
        int[] dirs = data[1];
        int[] maxima;
        if (nonmaxima) {
            maxima = new int[s];
            for (int y = 1, i = w + 1; y < h-1; y++, i+=2) {
                for (int x = 1; x < w-1; x++, i++) {
                    if ((dirs[i-w] + dirs[i+w] + dirs[i-w-1]  + dirs[i+w+1] + dirs[i-1] + dirs[i+1] + dirs[i-w+1] + dirs[i+w-1]) / 8.005 < dirs[i])
                        maxima[i] = mags[i];
                }
            }
        } else {
            maxima = mags;
        }
        normalize(maxima,255);
        int[] detected;
        if (trace) {
            detected = new int[w*h];
            int[] stack = new int[w*h];
            for (int y = 1, i = w + 1; y < h - 1; y++, i+=2) {
                for (int x = 1, c = 0; x < w - 1; x++, i++) {
                    if (detected[i] == 0 && maxima[i] > high) {
                        stack[c++] = i;
                        while(c > 0) {
                            int n = stack[--c];
                            if (detected[n] == 0 && maxima[n] > low) {
                                detected[n] = 255;
                                stack[c++] = n+1;
                                stack[c++] = n-w+1;
                                stack[c++] = n-w;
                                stack[c++] = n-w-1;
                                stack[c++] = n-1;
                                stack[c++] = n+w-1;
                                stack[c++] = n+w;
                                stack[c++] = n+w+1;
                            }
                        }
                    }
                }
            }
        } else {
            detected = maxima;
        }
        return detected;
    }

}
