/**
 *  Copyright 2011 by Benjamin J. Land (a.k.a. BenLand100)
 *
 *  This file is part of ScratchCode.
 *
 *  ScratchCode is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  ScratchCode is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with ScratchCode. If not, see <http://www.gnu.org/licenses/>.
 */

package perspective;

import java.awt.Image;
import java.awt.image.BufferedImage;

/**
 *
 * @author benland100
 */
public class RectRemapper {
    
    private int width, height;
    private Point[] source;
    
    /**
     * Creates a new instance of RectRemapper
     */
    public RectRemapper(int width, int height) {
        this.width = width;
        this.height = height;
        source = new Point[width*height];
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                source[x+y*width] = new Point(x,y);
            }
        }
    }
    
    public BufferedImage remap(BufferedImage img, int x0, int y0, int x1, int y1, int x2, int y2, int x3, int y3) {
        Perspective p = new Perspective(0,0,width-1,0,width-1,height-1,0,height-1,x0, y0, x1, y1, x2, y2, x3, y3);
        BufferedImage res = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB);
        Point[] dest = p.transform(source);
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                Point pt = dest[x+y*height];
                res.setRGB(x,y,img.getRGB((int)Math.round(pt.x),(int)Math.round(pt.y)));
            }
        }
        return res;
    }
    
}
