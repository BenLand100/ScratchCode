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

#include <cstdlib>
#include <cstring>
#include <cmath>
#include <iostream>
#include <GL/glut.h>
#include "vec.h"
#include "kdtree.h"

using namespace std;

typedef struct obj {
    vec velo,pos;
} obj;

typedef struct sim {
    int numobjs;
    obj* objs;
    kdtree<3>* tree;
    vec* force;
    double dt, t;
    double width, height, depth;
    double mass, radius, k, u;
} sim;

sim s;

double objpos(obj *o, int d) {
    switch (d) {
        case 0: return o->pos.x;
        case 1: return o->pos.y;
        case 2: return o->pos.z;
    }
    return 0;
}

void init(sim* sim) {
    int count = 15*15*15;
    int maxcomp = round(cbrt(500));
    int domain = 500;

    sim->k = 50;
    sim->u = 6;
    sim->radius = 15;
    sim->mass = 5;

    sim->force = new vec[count];

    srand(time(0));

    sim->t = 0;
    sim->dt = 0.1;
    sim->numobjs = count;
    sim->objs = new obj[count];
    sim->width = sim->height = sim->depth = domain;
    int dim = round(cbrt(count));
    for (int x = 0; x < dim; x++) {
        for (int y = 0; y < dim; y++) {
            for (int z = 0; z < dim; z++) {
                obj *o = &sim->objs[x+y*dim+z*dim*dim];
                o->velo = vector(0,0,0);
                o->velo = vector(rand()%(maxcomp*2)-maxcomp,rand()%(maxcomp*2)-maxcomp,rand()%(maxcomp*2)-maxcomp);
                o->pos = vector((double)x*(domain-sim->radius*2)/(dim-1) + sim->radius,(double)y*(domain-sim->radius*2)/(dim-1) + sim->radius,(double)z*(domain-sim->radius*2)/(dim-1) + sim->radius);
            }
        }
    }

    //sim->tree = new kdtree<3>((dimfunc)&objpos,(void*)sim->objs,sizeof(obj),count);
}

void handleKeypress(unsigned char key, int x, int y) {
    switch (key) {
        case 27: //Escape key
            exit(0);
    }
}

void handleResize(int w, int h) {
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(30.0, (float)w/(float)h, 1, 500);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(0.0,0.0,5.0,
              0.0,0.0,-1.0,
              0.0f,1.0f,0.0f);
}

void drawScene() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    int num = s.numobjs;
    obj* o = s.objs;
    double w = s.width;
    double h = s.height;
    double d = s.depth;
    glTranslatef(0.0f,0.0f,-2.0f);
    glColor3f(1,1,1);
    glBegin(GL_QUADS);
    glVertex3f(-0.5,-0.5,0);
    glVertex3f(-0.5,-0.5,-1);
    glVertex3f(-0.5,0.5,-1);
    glVertex3f(-0.5,0.5,0);
    glEnd();
    glBegin(GL_QUADS);
    glVertex3f(0.5,-0.5,0);
    glVertex3f(0.5,-0.5,-1);
    glVertex3f(0.5,0.5,-1);
    glVertex3f(0.5,0.5,0);
    glEnd();
    glBegin(GL_QUADS);
    glVertex3f(0.5,0.5,-1);
    glVertex3f(0.5,0.5,0);
    glVertex3f(-0.5,0.5,0);
    glVertex3f(-0.5,0.5,-1);
    glEnd();
    glBegin(GL_QUADS);
    glVertex3f(0.5,-0.5,-1);
    glVertex3f(0.5,-0.5,0);
    glVertex3f(-0.5,-0.5,0);
    glVertex3f(-0.5,-0.5,-1);
    glEnd();
    glColor3f(0.9,0.9,0.9);
    glBegin(GL_QUADS);
    glVertex3f(0.5,0.5,-1);
    glVertex3f(0.5,-0.5,-1);
    glVertex3f(-0.5,-0.5,-1);
    glVertex3f(-0.5,0.5,-1);
    glEnd();
    glColor3f(0,0,1);
    for (int i = 0; i < num; i++, o++) {
        glTranslatef(o->pos.x/w-0.5,o->pos.y/w-0.5,-o->pos.z/d);
        glutSolidSphere(s.radius/500.0,32,32);
        glTranslatef(-o->pos.x/w+0.5,-o->pos.y/w+0.5,o->pos.z/d);
    }
    glutSwapBuffers();
}

/**
 * d: absolute value of distance between objects
 * v: relative velocity between objects
 */
double force(double d, double v) {
    d = abs(d);
    return s.k*pow(d,3.0/2.0) + s.u*v*pow(d,0.5);
}

void update(int value) {
    vec accel = vector(0,-9.8,0);
    //s.tree->retree();
    memset(s.force,0,sizeof(vec)*s.numobjs);
    for (int i = 0; i < s.numobjs; i++) {
        vec pos = s.objs[i].pos;
        if (pos.x + s.radius > s.width) {
            s.force[i] = s.force[i] + vector(-force(s.width-s.radius-pos.x,s.objs[i].velo.x),0,0);
        }
        if (pos.x -  s.radius < 0) {
            s.force[i] = s.force[i] + vector(force(pos.x-s.radius,-s.objs[i].velo.x),0,0);
        }
        if (pos.y + s.radius > s.height) {
            s.force[i] = s.force[i] + vector(0,-force(s.height-s.radius-pos.y,s.objs[i].velo.y),0);
        }
        if (pos.y - s.radius < 0) {
            s.force[i] = s.force[i] + vector(0,force(pos.y-s.radius,-s.objs[i].velo.y),0);
        }
        if (pos.z + s.radius > s.depth) {
            s.force[i] = s.force[i] + vector(0,0,-force(s.depth-s.radius-pos.z,s.objs[i].velo.z));
        }
        if (pos.z - s.radius < 0) {
            s.force[i] = s.force[i] + vector(0,0,force(pos.z-s.radius,-s.objs[i].velo.z));
        }
        for (int j = i+1; j < s.numobjs; j++) {
            vec r = pos - s.objs[j].pos;
            double magr = mag(r);
            double d = magr - 2 * s.radius;
            if (d < 0) {
                r = r / magr;
                vec f = force(d,dot(s.objs[j].velo,r)-dot(s.objs[i].velo,r)) * r;
                s.force[i] = s.force[i] + f;
                s.force[j] = s.force[j] - f;
            }
        }
    }
    for (int i = 0; i < s.numobjs; i++) {
        s.objs[i].velo = s.objs[i].velo + (accel + s.force[i]/s.mass) * s.dt;
        s.objs[i].pos = s.objs[i].pos + s.objs[i].velo * s.dt;
    }
    glutPostRedisplay();
    glutTimerFunc(25, update, 0);
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(500, 500);
    glutCreateWindow("GranularSim");

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHT1);
    glEnable(GL_COLOR_MATERIAL);
    glShadeModel(GL_SMOOTH);
    
    GLfloat ambient0[] = { 0.0f, 0.0f, 0.0f, 1.0f };
    glLightfv(GL_LIGHT0, GL_AMBIENT, ambient0);
    GLfloat diffuse0[] = {0.8f, 0.8f, 0.8f , 1.0f};
    glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse0);
    GLfloat specular0[] = {0.9f, 0.9f, 0.9f , 1.0f};
    glLightfv(GL_LIGHT0, GL_SPECULAR, specular0);
    GLfloat position0[] = { 0.5f, 0.5f, -1.0f, 0.7f };
    glLightfv(GL_LIGHT0, GL_POSITION, position0);

    GLfloat ambient1[] = { 0.0f, 0.0f, 0.0f, 1.0f };
    glLightfv(GL_LIGHT1, GL_AMBIENT, ambient1);
    GLfloat diffuse1[] = {0.8f, 0.8f, 0.8f , 1.0f};
    glLightfv(GL_LIGHT1, GL_DIFFUSE, diffuse1);
    GLfloat specular1[] = {0.0f, 0.0f, 0.0f , 1.0f};
    glLightfv(GL_LIGHT1, GL_SPECULAR, specular1);
    GLfloat position1[] = { 0.0f, 0.0f, -1.0f, 0.33f };
    glLightfv(GL_LIGHT1, GL_POSITION, position1);



    glutDisplayFunc(drawScene);
    glutKeyboardFunc(handleKeypress);
    glutReshapeFunc(handleResize);
    glutTimerFunc(25, update, 0);

    init(&s);
    glutMainLoop();
    return (EXIT_SUCCESS);
}

