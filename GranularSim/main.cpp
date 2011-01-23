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

#include <stdlib.h>
#include <iostream>
#include <GL/glut.h>
#include "vec.h"

using namespace std;

typedef struct obj {
    vec velo,pos;
    double mass, radius;
} obj;

typedef struct sim {
    int numobjs;
    obj* objs;
    double dt, t;
    double width, height, depth;
} sim;

sim s;

void init(sim* sim) {
    int size = 10;
    int mass = 5;
    int count = 100;
    int maxcomp = round(sqrt(5000));
    int domain = 500;

    srand(time(0));
    sim->dt = 0.05;
    sim->numobjs = count;
    sim->objs = new obj[count];
    sim->width = sim->height = sim->depth = domain;
    int dim = round(sqrt(count));
    for (int x = 0; x < dim; x++) {
        for (int y = 0; y < dim; y++) {
            obj *o = &sim->objs[x+y*dim];
            o->velo = vector(0,0,0);
            o->velo = vector(rand()%(maxcomp*2)-maxcomp,rand()%(maxcomp*2)-maxcomp,rand()%(maxcomp*2)-maxcomp);
            o->mass = mass;
            o->radius = size;
            o->pos = vector((double)x*domain/dim,(double)y*domain/dim,domain/2.0);
        }
    }
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
        glutSolidSphere(o->radius/500.0,32,32);
        glTranslatef(-o->pos.x/w+0.5,-o->pos.y/w+0.5,o->pos.z/d);
    }
    glutSwapBuffers();
}

void update(int value) {
    glutPostRedisplay();
    glutTimerFunc(25, update, 0);
    int num = s.numobjs;
    obj* o = s.objs;
    for (int i = 0; i < num; i++, o++) {
        vec accel = vector(0,0,0);
        o->velo = o->velo + accel * s.dt;
        o->pos = o->pos + o->velo * s.dt;
        if (o->pos.x + o->radius > s.width) {
            o->velo.x = -o->velo.x;
            o->pos.x=2*(s.width-o->radius)-o->pos.x;
        }
        if (o->pos.x - o->radius < 0) {
            o->velo.x = -o->velo.x;
            o->pos.x=2*o->radius-o->pos.x;
        }
        if (o->pos.y + o->radius > s.height) {
            o->velo.y = -o->velo.y;
            o->pos.y=2*(s.height-o->radius)-o->pos.y;
        }
        if (o->pos.y - o->radius < 0) {
            o->velo.y = -o->velo.y;
            o->pos.y=2*o->radius-o->pos.y;
        }
        if (o->pos.z + o->radius > s.depth) {
            o->velo.z = -o->velo.z;
            o->pos.z=2*(s.depth-o->radius)-o->pos.z;
        }
        if (o->pos.z - o->radius < 0) {
            o->velo.z = -o->velo.z;
            o->pos.z=2*o->radius-o->pos.z;
        }
        obj* c = s.objs;
        for (int j = 0; j < num; j++, c++) {
            if (mag(o->pos-c->pos) <= (o->radius+c->radius) && i != j) {
                vec dir = norm(c->pos - o->pos);
                double vi = o->velo * dir;
                double vj = c->velo * dir;
                double exchange = vj - vi;
                o->velo = o->velo + exchange*dir;
                c->velo = c->velo - exchange*dir;
            }
        }
    }
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

