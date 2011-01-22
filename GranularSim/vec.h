/**
 *  Copyright 2011 by Benjamin J. Land (a.k.a. BenLand100)
 *
 *  This file is part of GranularSim, a Granular Material Simulator.
 *
 *  GranularSim is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  GranularSim is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GranularSim. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _VEC_H
#define	_VEC_H

typedef struct vec {
    double x, y, z;
} vec;

inline vec vector(double x, double y, double z) {
    vec res;
    res.x = x;
    res.y = y;
    res.z = z;
    return res;
}

inline double dot(const vec &a, const vec &b) {
    return a.x*b.x + a.y*b.y + a.z*b.z;
}

inline double mag(const vec &a) {
    return sqrt(dot(a,a));
}

inline vec cross(const vec &a, const vec &b) {
    return vector(a.y*b.z-a.z*b.y,a.z*b.x-a.x*b.z,a.x*b.y-a.y*b.x);
}

inline bool operator==(const vec &a, const vec &b) {
    return a.x == b.x && a.y == b.y && a.x == b.z;
}

inline vec operator+(const vec &a, const vec &b) {
    return vector(a.x+b.x,a.y+b.y,a.z+b.z);
}

inline vec operator-(const vec &a, const vec &b) {
    return vector(a.x-b.x,a.y-b.y,a.z-b.z);
}

inline double operator*(const vec &a, const vec &b) {
    return dot(a,b);
}

inline vec operator*(const double &a,const vec &b) {
    return vector(a*b.x,a*b.y,a*b.z);
}

inline vec operator*(const vec &a, const double &b) {
    return vector(a.x*b,a.y*b,a.z*b);
}

#endif	/* _VEC_H */

