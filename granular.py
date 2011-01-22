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

from visual import *;
import random;

class Simulation:
    def __init__(self,depth,width,height):
        self.objs = []
        self.minx = -width/2.0
        self.miny = -height/2.0
        self.minz = -depth/2.0
        self.maxx = width/2.0
        self.maxy = height/2.0
        self.maxz = depth/2.0
        self.step = 0.1
    def add(self,obj):
        self.objs.append(obj)
    def run(self):
        for i in range(len(self.objs)):
            ball = self.objs[i];
            ball.pos = ball.pos + ball.velocity*self.step
            if ball.x > self.maxx:
                ball.velocity.x = -ball.velocity.x
                ball.x=2*self.maxx-ball.x
            if ball.x < self.minx:
                ball.velocity.x = -ball.velocity.x
                ball.x=2*self.minx-ball.x
            if ball.y > self.maxy:
                ball.velocity.y = -ball.velocity.y
                ball.y=2*self.maxy-ball.y
            if ball.y < self.miny:
                ball.velocity.y = -ball.velocity.y
                ball.y=2*self.miny-ball.y
            if ball.z > self.maxz:
                ball.velocity.z = -ball.velocity.z
                ball.z=2*self.maxz-ball.z
            if ball.z < self.minz:
                ball.velocity.z = -ball.velocity.z
                ball.z=2*self.minz-ball.z
            for j in range(i+1,len(self.objs)):
                other = self.objs[j]
                distance = mag(ball.pos-other.pos)
                if distance < (ball.radius + other.radius):
                    direction = norm(other.pos - ball.pos)
                    vi = dot(ball.velocity,direction)
                    vj = dot(other.velocity,direction)
                    exchange = vj - vi
                    ball.velocity=ball.velocity + exchange*direction
                    other.velocity=other.velocity - exchange*direction

		
		
sim = Simulation(500,500,500)
random.seed()
for x in range(-250,250,100):
    for y in range(-250,250,100):
        for z in range(-250,250,100):
            print x,y,z
            ball = sphere(pos=vector(x,y,z),radius=20,color=color.red)
            ball.velocity = vector(random.randint(0,50),random.randint(0,50),random.randint(0,50))
            sim.add(ball)

while True:   
    rate(30)
    sim.run()
mac_rotate()
