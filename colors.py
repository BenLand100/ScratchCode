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

import colorsys

r = lambda x: 1-x/8.0 if x <= 8 else (x-16)/8.0 if x >= 16 else 0
g = lambda x: x/8.0 if x <= 8 else 1-(x-8)/8.0 if 8 <= x <= 16 else 0
b = lambda x: (x-8)/8.0 if 8 <= x <= 16 else 1-(x-16)/8.0 if x >= 16 else 0

one = lambda x: hex(int(x*255)).upper()[2:]
two = lambda x: x if len(x) > 1 else '0'+x

fmt = lambda x: two(one(x))

#colors = []
#for t in range(24):
#	colors.append('#'+fmt(r(t))+fmt(g(t))+fmt(b(t)))

#colors = []
#for t in range(24):
#	(r,g,b) = colorsys.hsv_to_rgb(t/25.0, 1, 1)
#	colors.append('#'+fmt(r)+fmt(g)+fmt(b))
#print 'array("'+'","'.join(colors)+'");'

unique = []
for x in range(16):
  for y in range(x+1,16):
    unique.append((x,y))

print len(unique),unique
	
