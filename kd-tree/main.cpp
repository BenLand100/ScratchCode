#include <cstdlib>
#include <iostream>
#include "kdtree.h"

struct point {
    double x, y, z;
};

double dim(point *pt, int d) {
    switch (d) {
        case 0:
            return pt->x;
        case 1:
            return pt->y;
        case 2:
            return pt->z;
    }
}

int main(int argc, char **argv) {
    point pts[500];
    srand(time(0));
    for (int i = 0; i < 500; i++) {
        pts[i].x = rand() % 1000;
        pts[i].y = rand() % 1000;
        pts[i].z = rand() % 1000;
    }
    kdtree<point,3> tree(pts,500,&dim);
}
