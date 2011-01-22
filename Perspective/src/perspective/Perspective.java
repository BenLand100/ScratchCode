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

/**
 *
 * @author benland100
 */
public class Perspective {
    
    //3x3 matrix
    public double m00, m01, m02;
    public double m10, m11, m12;
    public double m20, m21, m22;
    
    private Perspective() {
    }
    
    public Perspective(double sx0, double sy0, double sx1, double sy1, double sx2,  double sy2, double sx3, double sy3, double dx0, double dy0, double dx1, double dy1, double dx2,  double dy2, double dx3, double dy3) {
        Perspective step1 = mapUnitToQuad(sx0, sy0, sx1, sy1, sx2, sy2, sx3, sy3).flip();
        Perspective step2 = mapUnitToQuad(dx0, dy0, dx1, dy1, dx2, dy2, dx3, dy3);
        m00 = step1.m00 * step2.m00 + step1.m10 * step2.m01 + step1.m20 * step2.m02;
        m10 = step1.m00 * step2.m10 + step1.m10 * step2.m11 + step1.m20 * step2.m12;
        m20 = step1.m00 * step2.m20 + step1.m10 * step2.m21 + step1.m20 * step2.m22;
        m01 = step1.m01 * step2.m00 + step1.m11 * step2.m01 + step1.m21 * step2.m02;
        m11 = step1.m01 * step2.m10 + step1.m11 * step2.m11 + step1.m21 * step2.m12;
        m21 = step1.m01 * step2.m20 + step1.m11 * step2.m21 + step1.m21 * step2.m22;
        m02 = step1.m02 * step2.m00 + step1.m12 * step2.m01 + step1.m22 * step2.m02;
        m12 = step1.m02 * step2.m10 + step1.m12 * step2.m11 + step1.m22 * step2.m12;
        m22 = step1.m02 * step2.m20 + step1.m12 * step2.m21 + step1.m22 * step2.m22;
    }
    
    public Point[] transform(Point[] src) {
        Point[] res = new Point[src.length];
        for (int i = 0; i < res.length; i++) {
            double x = src[i].x;
            double y = src[i].y;
            double w = m20 * x + m21 * y + m22;
            res[i] = w == 0D ? new Point(x,y) : new Point((m00 * x + m01 * y + m02) / w, (m10 * x + m11 * y + m12) / w);
        }
        return res;
    }
    
    public Point transform(Point src) {
        double x = src.x;
        double y = src.y;
        double w = m20 * x + m21 * y + m22;
        return w == 0D ? new Point(x,y) : new Point((m00 * x + m01 * y + m02) / w, (m10 * x + m11 * y + m12) / w);
    }
    
    public Perspective flip() {
        Perspective flipped = new Perspective();
        flipped.m00 = m11 * m22 - m12 * m21;
        flipped.m10 = m12 * m20 - m10 * m22;
        flipped.m20 = m10 * m21 - m11 * m20;
        flipped.m01 = m02 * m21 - m01 * m22;
        flipped.m11 = m00 * m22 - m02 * m20;
        flipped.m21 = m01 * m20 - m00 * m21;
        flipped.m02 = m01 * m12 - m02 * m11;
        flipped.m12 = m02 * m10 - m00 * m12;
        flipped.m22 = m00 * m11 - m01 * m10;
        return flipped;
    }
    
    public static Perspective mapUnitToQuad(double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3) {
        double dx3 = ((x0 - x1) + x2) - x3;
        double dy3 = ((y0 - y1) + y2) - y3;
        Perspective u2q = new Perspective();
        u2q.m22 = 1D;
        if (dx3 == 0D && dy3 == 0D) {
            u2q.m00 = x1 - x0;
            u2q.m01 = x2 - x1;
            u2q.m02 = x0;
            u2q.m10 = y1 - y0;
            u2q.m11 = y2 - y1;
            u2q.m12 = y0;
            u2q.m20 = 0D;
            u2q.m21 = 0D;
        } else {
            double dx1 = x1 - x2;
            double dy1 = y1 - y2;
            double dx2 = x3 - x2;
            double dy2 = y3 - y2;
            double invdet = 1D / (dx1 * dy2 - dx2 * dy1);
            u2q.m20 = (dx3 * dy2 - dx2 * dy3) * invdet;
            u2q.m21 = (dx1 * dy3 - dx3 * dy1) * invdet;
            u2q.m00 = (x1 - x0) + u2q.m20 * x1;
            u2q.m01 = (x3 - x0) + u2q.m21 * x3;
            u2q.m02 = x0;
            u2q.m10 = (y1 - y0) + u2q.m20 * y1;
            u2q.m11 = (y3 - y0) + u2q.m21 * y3;
            u2q.m12 = y0;
        }
        return u2q;
    }
    
}
