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
#include <iostream>
#include <GL/glut.h>
#include "vec.h"

using namespace std;

typedef struct obj {
    vec velo,pos;
} obj;

typedef struct state {
    vec *a,*b,*delta,*velo;
} state;

typedef struct sim {
    int numobjs;
    obj* objs;
    state c;
    double dt, t;
    double width, height, depth;
    double mass, radius, elasticity;
} sim;

sim s;

void init(sim* sim) {
    int count = 10*10;
    int maxcomp = round(sqrt(5000));
    int domain = 500;

    sim->elasticity = 1.0;
    sim->radius = 15;
    sim->mass = 5;

    sim->c.a = new vec[count];
    sim->c.b = new vec[count];
    sim->c.delta = new vec[count];
    sim->c.velo = new vec[count];

    srand(time(0));

    sim->t = 0;
    sim->dt = 0.005;
    sim->numobjs = count;
    sim->objs = new obj[count];
    sim->width = sim->height = sim->depth = domain;
    int dim = round(sqrt(count));
    for (int x = 0; x < dim; x++) {
        for (int y = 0; y < dim; y++) {
            obj *o = &sim->objs[x+y*dim];
            o->velo = vector(0,0,0);
            o->velo = vector(rand()%(maxcomp*2)-maxcomp,rand()%(maxcomp*2)-maxcomp,rand()%(maxcomp*2)-maxcomp);
            o->pos = vector((double)x*(domain-sim->radius*2)/(dim-1) + sim->radius,(double)y*(domain-sim->radius*2)/(dim-1) + sim->radius,domain/2.0);
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
        glutSolidSphere(s.radius/500.0,32,32);
        glTranslatef(-o->pos.x/w+0.5,-o->pos.y/w+0.5,o->pos.z/d);
    }
    glutSwapBuffers();
}

void update(int value) {
    vec accel = vector(0,0,0);
    for (int i = 0; i < s.numobjs; i++) {
        s.c.a[i] = s.objs[i].pos;
        s.c.velo[i] = s.objs[i].velo + accel * s.dt;
        s.c.delta[i] = s.c.velo[i] * s.dt;
        s.c.b[i] = s.c.a[i] + s.c.delta[i];
    }
    double step = s.dt, tmin = step;
    int imin, jmin,wall;
    bool detected = false;
    do {
        //cout << "step\n";
        //this should be done with kd-trees
        for (int i = 0; i < s.numobjs; i++) {
            vec pos = s.c.b[i];
            if (pos.x + s.radius > s.width) {
                double t = (s.width - (s.c.a[i].x + s.radius)) / s.c.velo[i].x;
                if (t < step && tmin > t) {
                    wall = 0;
                    imin = jmin = i;
                    tmin = t;
                }
            }
            if (pos.x -  s.radius < 0) {
                double t = (0 - (s.c.a[i].x - s.radius)) / s.c.velo[i].x;
                if (t < step && tmin > t) {
                    wall = 0;
                    imin = jmin = i;
                    tmin = t;
                }
            }
            if (pos.y + s.radius > s.height) {
                double t = (s.height - (s.c.a[i].y + s.radius)) / s.c.velo[i].y;
                if (t < step && tmin > t) {
                    wall = 1;
                    imin = jmin = i;
                    tmin = t;
                }
            }
            if (pos.y - s.radius < 0) {
                double t = (0 - (s.c.a[i].y - s.radius)) / s.c.velo[i].y;
                if (t < step && tmin > t) {
                    wall = 1;
                    imin = jmin = i;
                    tmin = t;
                }
            }
            if (pos.z + s.radius > s.depth) {
                double t = (s.depth - (s.c.a[i].z + s.radius)) / s.c.velo[i].z;
                if (t < step && tmin > t) {
                    wall = 2;
                    imin = jmin = i;
                    tmin = t;
                }
            }
            if (pos.z - s.radius < 0) {
                double t = (0 - (s.c.a[i].z - s.radius)) / s.c.velo[i].z;
                if (t < step && tmin > t) {
                    wall = 2;
                    imin = jmin = i;
                    tmin = t;
                }
            }
            double dist2 = s.radius*2 + mag(s.c.delta[i]);
            dist2 *= dist2;
            for (int j = 0; j < s.numobjs; j++) {
                vec a = s.c.a[j]-pos;
                vec b = s.c.b[j]-pos;
                if ((a*a < dist2 || b*b < dist2) && i != j && !(i == jmin && j == imin)) {
                    vec d0 = s.c.a[i] - s.c.a[j];
                    vec d = s.c.velo[i] - s.c.velo[j];
                    double _a = d*d;
                    double _b = 2*d0*d;
                    double _c = d0*d0-4*s.radius*s.radius;
                    double desc = _b*_b-4*_a*_c;
                    if (_a == 0 && desc < 0) continue;
                    double t1 = (-_b+sqrt(desc))/(2.0*_a);
                    double t2 = (-_b-sqrt(desc))/(2.0*_a);
                    double t = min(t1 > 0 ? t1 : t2, t2 > 0 ? t2 : t1);
                    if (t >= 1e-10 && t < step && tmin > t) {
                        //cout << i << ' ' << j << " t=" << t << '\n';
                        tmin = t;
                        imin = i;
                        jmin = j;
                    }
                }
            }
        }
        detected = tmin != step;
        //cout << "collision\n";
        if (detected) {
            //cout << tmin << ' ' << step << " detected\n";
            int i = imin, j = jmin;
            if (i == j) { //wall
                vec velo = s.c.velo[i];
                s.c.a[i] = s.c.a[i] + velo * tmin;
                velo = velo * s.elasticity;
                switch (wall) {
                    case 0:
                        velo.x = -velo.x;
                        break;
                    case 1:
                        velo.y = -velo.y;
                        break;
                    case 2:
                        velo.z = -velo.z;
                        break;
                }
                double tr = step - tmin;
                s.c.velo[i] = velo;
                s.c.b[i] = s.c.a[i] + velo * tr;
            } else { //ball
                vec icur = s.c.a[i] + s.c.velo[i]*tmin;
                vec jcur = s.c.a[j] + s.c.velo[j]*tmin;
                vec dir = norm(jcur - icur);
                //cout << mag(jcur-icur) << '\n';
                double momi = (s.c.velo[i] * dir) * s.elasticity;
                double momj = (s.c.velo[j] * dir) * s.elasticity;
                double ex = momj - momi;
                s.c.velo[i] = s.c.velo[i] + ex*dir;
                s.c.velo[j] = s.c.velo[j] - ex*dir;
                double tr = step - tmin;
                s.c.a[i] = icur;
                s.c.a[j] = jcur;
                s.c.b[i] = s.c.a[i] + s.c.velo[i]*tr;
                s.c.b[j] = s.c.a[j] + s.c.velo[j]*tr;
                s.c.delta[i] = s.c.b[i] - s.c.a[i];
                s.c.delta[j] = s.c.b[j] - s.c.a[j];
            }
            int _min = min(i,j), _max = max(i,j);
            for (int ii = 0; ii < _min; ii++) {
                s.c.a[ii] = s.c.a[ii] + s.c.velo[ii]*tmin;
                s.c.delta[ii] = s.c.b[ii] - s.c.a[ii];
            }
            for (int ii = _min+1; ii < _max; ii++) {
                s.c.a[ii] = s.c.a[ii] + s.c.velo[ii]*tmin;
                s.c.delta[ii] = s.c.b[ii] - s.c.a[ii];
            }
            for (int ii = _max+1; ii < s.numobjs; ii++) {
                s.c.a[ii] = s.c.a[ii] + s.c.velo[ii]*tmin;
                s.c.delta[ii] = s.c.b[ii] - s.c.a[ii];
            }
        }
        step -= tmin;
        tmin = step;
    } while (detected);
    for (int i = 0; i < s.numobjs; i++) {
        s.objs[i].velo = s.c.velo[i];
        s.objs[i].pos = s.c.b[i];
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

