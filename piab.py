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

"""Particle in a Box, quantum mechanics simulation"""

from math import *
from numpy import *
from matplotlib.pyplot import *

dt		= 1.00e-2
L 		= 5.00
m 		= 1.00-2
hbar 	= 1.05e-34
i		= complex(0,1)

def energy(n):
	return (n**2*pi**2*hbar)/(2*L**2*m)

def wavefunc(n):
	E = energy(n) 
	return lambda x,t: sqrt(2.0/L)*sin(n*pi/L*x)*exp(-i*E/hbar*t)
	
def sumwaves(levels):
	waves = [wavefunc(n) for n in levels]
	return lambda x,t: sum([wave(x,t) for wave in waves])/sqrt(len(levels))
	
fig = figure()
ax = fig.add_subplot(111)

N = 500
dN = L / N
wave = sumwaves(range(1,5))
loc = [n*dN for n in range(0,N)]
prob = [wave(x,0) for x in loc]
prob = [p*p.conjugate() for p in prob]
lines, = ax.plot(loc,prob)
def draw():
	print draw.t*dt
	prob = [wave(x,draw.t*dt) for x in loc]
	prob = [p*p.conjugate() for p in prob]
	#integral = sum([(prob[j]+prob[j+1])/2.0*dN for j in range(0,N-1)])
	#print integral
	lines.set_data(loc,prob)
	ax.figure.canvas.draw()
	draw.t += 1
	fig.canvas.manager.window.after(0, draw)

draw.t = 0
fig.canvas.manager.window.after(0, draw)
show()

	
