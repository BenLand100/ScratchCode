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

import java.awt.Graphics;
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
public class Main extends JFrame {
    
    Perspective p;
    
    /** Creates a new instance of Main */
    public Main() {
        super();
        setLayout(new GridLayout());
        //p = new Perspective(10,10,200,60,400,400,90,450,100,100,400,100,400,400,100,400);
        //p = new Perspective(100,100,400,100,400,400,100,400,100,100,400,100,400,400,100,400);
        //p = Perspective.mapUnitToQuad(100,100,400,100,400,400,100,400).flip();
        RectRemapper remap = new RectRemapper(200,200);
        try {
            BufferedImage orig = ImageIO.read(new File("./12.bmp"));
            long time = System.currentTimeMillis();
            BufferedImage star = remap.remap(orig,353,231,340,64,357,163,369,297);
            BufferedImage circ = remap.remap(orig,148,250,354,231,368,296,213,306);
            BufferedImage square = remap.remap(orig,354,230,150,249,169,89,340,64);
            System.out.println(System.currentTimeMillis() - time);
            add(new JLabel(new ImageIcon(star)));
            add(new JLabel(new ImageIcon(square)));
            add(new JLabel(new ImageIcon(circ)));
        } catch (Exception e) {
            e.printStackTrace();
        }
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setSize(500,500);
        setVisible(true);
    }
    
    public void paint(Graphics g) {
        super.paint(g);
        /*Graphics2D g2d = (Graphics2D)g;
        Point a = p.transform(new Point(10,10));
        Point b = p.transform(new Point(200,60));
        Point c = p.transform(new Point(400,400));
        Point d = p.transform(new Point(90,450));
        g2d.setColor(Color.RED);
        g2d.drawLine((int)a.x,(int)a.y,(int)b.x,(int)b.y);
        g2d.setColor(Color.GREEN);
        g2d.drawLine((int)b.x,(int)b.y,(int)c.x,(int)c.y);
        g2d.setColor(Color.BLUE);
        g2d.drawLine((int)c.x,(int)c.y,(int)d.x,(int)d.y);
        g2d.setColor(Color.ORANGE);
        g2d.drawLine((int)d.x,(int)d.y,(int)a.x,(int)a.y);*/
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        new Main();
    }
    
}
