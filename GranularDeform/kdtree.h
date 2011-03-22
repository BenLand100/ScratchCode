/* 
 * File:   kdtree.h
 * Author: benland100
 *
 * Created on March 22, 2011, 5:42 PM
 */

#ifndef _KDTREE_H
#define	_KDTREE_H

#include <cstdlib>
#include <cstring>
#include <cmath>
#include <iostream>
using namespace std;

template <int D>
struct entry {
    void *data;
    double dim[D];
};

template <int D>
struct treenode {
    int parent, left, right;
    struct entry<D> *data;
};

typedef double (*dimfunc)(void* obj, int d);

template <int D>
class kdtree {
private:
    struct treenode<D> *nodes;
    struct entry<D> *entries;
    struct entry<D> **sorted;
    int root, num, nodeidx;
    dimfunc func;
    void sortDim(int left, int right, int pivot, int dim) {
        if (right > left) {
            struct entry<D> *temp = sorted[pivot];
            sorted[pivot] = sorted[right];
            sorted[right] = temp;
            double mid = temp->dim[dim];
            int store = left;
            for (int i = left; i < right; i++) {
                if (sorted[i]->dim[dim] <= mid) {
                    temp = sorted[i];
                    sorted[i] = sorted[store];
                    sorted[store] = temp;
                    store++;
                }
            }
            temp = sorted[store];
            sorted[store] = sorted[right];
            sorted[right] = temp;
            sortDim(left, store - 1, (left + store - 1) / 2, dim);
            sortDim(store + 1, right, (right + store + 1) / 2, dim);
        }
    }
    int tree(int left, int right, int depth) {
        if (left == right) {
            nodes[nodeidx].data = sorted[left];
            return nodeidx++;
        }
        int dim =  depth % D;
        int mid = (left + right) / 2;
        sortDim(left, right, mid, dim);
        int t = nodeidx++;
        if (right - left == 1) {
            nodes[t].data = sorted[right];
            int r = nodeidx++;
            nodes[t].left = r;
            nodes[nodes[t].left].data = sorted[left];
            nodes[nodes[t].left].parent = t;
        } else {
            //condition where the mid point is equal to a few points on the right side
            while (mid < right-1 && sorted[mid]->dim[dim] >= sorted[mid+1]->dim[dim]) mid++;
            nodes[t].data = sorted[mid];
            nodes[t].left = tree(left, mid - 1, depth + 1);
            if (nodes[t].left != -1) {
                nodes[nodes[t].left].parent = t;
            }
            nodes[t].right = tree(mid + 1, right, depth + 1);
            if (nodes[t].right != -1) {
                nodes[nodes[t].right].parent = t;
            }
        }
        return t;
    }
public:
    void retree() {
        for (int i = 0; i < num; i++) {
            memset(nodes,0xff,sizeof(struct treenode<D>)*num);
            for (int j = 0; j < D; j++) {
                entries[i].dim[j] = func(entries[i].data,j);
            }
        }
        nodeidx = 0;
        root = tree(0,num-1,0);
    }
    kdtree(dimfunc _func, void *objs, int size, int _num) : func(_func), num(_num) {
        nodes = new struct treenode<D>[num];
        entries = new struct entry<D>[num];
        sorted = new struct entry<D>*[num];
        for (int i = 0; i < num; i++) {
            entries[i].data = objs + size*i;
            sorted[i] = &entries[i];
        }
        retree();
    }
    virtual ~kdtree() {
        delete nodes;
        delete entries;
        delete sorted;
    }
};

#endif	/* _KDTREE_H */

