#!/usr/bin/python

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
	
