#!/usr/bin/python
"""
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
"""

from math import *
from numpy import *
from matplotlib.pyplot import *
from mpl_toolkits.mplot3d.axes3d import *


nc = 5000                     #number of cycles
ns = 235                      #steps per cycle
t0 = 0.0                      #initial time
x0 = 0.2                      #initial position
v0 = 0.0                      #initial velocity

w0 = 1.0                      #natural frequency
q = 0.5                       #dampening coefficient
f = 1.2                       #forcing strength
w = 2.0/3.0                   #forcing frequency


def accel(t,x,v):
    return -w0*w0*sin(x)-q*v+f*sin(w*t)

dt = 2.0*pi/w/ns
t = t0
x = x0
v = v0
xp = [(x+pi) % (2*pi)]
vp = [v]
wp = [(w*t) % (2.0*pi)]
#print (xp[0],vp[0],wp[0])
for i in range(1,nc):    
    a = accel(t,x,v)
    u = v - a*dt*0.5
    for i in range(0,ns):
        u = u + a*dt
        t = t + dt
        x = x + u*dt
        v = u + a*dt*0.5
        a = accel(t, x, v)
        #if i % 5 == 0:
            #print((x+pi) % (2*pi),v,(w*t) % (2.0*pi))
            #xp.append((x+pi) % (2*pi))
            #vp.append(v)
            #wp.append((w*t) % (2.0*pi))
    v = u + a*dt*0.5
    xp.append((x+pi) % (2*pi))
    vp.append(v)
    wp.append((w*t) % (2.0*pi))


scatter(xp,vp,c=array(wp)+pi,cmap='gist_rainbow')
xlabel('X')
ylabel('V')
#set_zlabel('W')
show()
