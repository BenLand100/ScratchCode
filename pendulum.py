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

from visual import *

class double_pendulum:
	"""The world's most entertaining demonstration of chaos."""
	def __init__(self,orig=vector(0,0,0),l1=10.0,l2=5.0,m1=5.0,m2=1.0,phi1=1.25*pi,phi2=pi,dphi1=0.0,dphi2=0.0,dt=0.01,g=9.81):
		"""Creates a new double_pendulum with the given values and creates the graphics."""
		self._orig = orig
		self._l1 = l1
		self._l2 = l2
		self._m1 = m1
		self._m2 = m2
		self._phi1 = phi1
		self._phi2 = phi2
		self._dphi1 = dphi1
		self._dphi2 = dphi2
		self._dt = dt
		self._g = g
		self._p1 = sphere(pos=self._orig,color=color.red,radius=1)
		self._p2 = sphere(pos=self._orig+vector(sin(self._phi1)*self._l1,-cos(self._phi1)*self._l1,0),color=color.blue,radius=1)
		self._p3 = sphere(pos=self._p2.pos+vector(sin(self._phi2)*self._l2,-cos(self._phi2)*self._l2,0),color=color.green,radius=1)
		self._r1 = cylinder(pos=self._orig,axis=self._p2.pos,radius=0.25)
		self._r2 = cylinder(pos=self._p2.pos, axis=self._p3.pos-self._p2.pos,radius=0.25)
	def update(self):
		"""Updates the position of the graphics to the current state."""
		self._p2.pos = self._orig + vector(sin(self._phi1)*self._l1,-cos(self._phi1)*self._l1,0)
		self._p3.pos = self._p2.pos + vector(sin(self._phi2)*self._l2,-cos(self._phi2)*self._l2,0)
		self._r1.axis=self._p2.pos-self._orig
		self._r2.pos=self._p2.pos
		self._r2.axis=self._p3.pos-self._p2.pos
	def _accel(self,phi1,phi2,dphi1,dphi2):
		"""Second order ODE I derived for an ideal double pendulum, l2math."""
		ddphi2 = (cos(phi1-phi2)*(self._m2/(self._m1+self._m2)*dphi2**2*sin(phi1-phi2) + 
				 self._g/self._l2*sin(phi1)) + self._l1/self._l2*dphi1**2*sin(phi1-phi2) - 
				 self._g/self._l2*sin(phi2) ) / (1-self._m2/(self._m1+self._m2)*cos(phi1-phi2)**2)
		ddphi1 = -self._l2/self._l1*self._m2/(self._m1+self._m2)*(ddphi2*cos(phi1-phi2) + 
		         dphi2**2*sin(phi1-phi2)) - self._g/self._l1*sin(phi1)
		return (ddphi1,ddphi2)
	def step(self):
		"""Uses a second order runge-kutta integration to step the pendulum one dt."""
		(ddphi1, ddphi2) = self._accel(self._phi1,self._phi2,self._dphi1,self._dphi2)
		dphi1h = self._dphi1 + ddphi1 * self._dt/2.0
		dphi2h = self._dphi2 + ddphi2 * self._dt/2.0
		phi1h = self._phi1 + dphi1h * self._dt/2.0
		phi2h = self._phi2 + dphi2h * self._dt/2.0
		(ddphi1, ddphi2) = self._accel(phi1h,phi2h,dphi1h,dphi2h)
		self._dphi1 += ddphi1 * self._dt
		self._dphi2 += ddphi2 * self._dt
		self._phi1 += self._dphi1 * self._dt
		self._phi2 += self._dphi2 * self._dt


a = double_pendulum(orig=vector(-17,-17,0),dphi2=0.0000000)
b = double_pendulum(orig=vector(17,-17,0), dphi2=0.0000001)
c = double_pendulum(orig=vector(-17,17,0), dphi2=0.0000002)
d = double_pendulum(orig=vector(17,17,0),  dphi2=0.0000003)
scene.range = 17*2
while True:
	rate(200)
	a.step()
	b.step()
	c.step()
	d.step()
	a.update()
	b.update()
	c.update()
	d.update()
	
