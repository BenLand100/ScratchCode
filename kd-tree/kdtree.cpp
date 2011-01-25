#include "kdtree.h"

#include <iostream>

using namespace std;

template<class T, int D>
kdtree<T,D>::kdtree(T *elems, int num, kdtree<T,D>::dim d) {
    this->elems = elems;
    this->size = num;
    node *root = this->root = new node[num];
    remap();
}

template<class T, int D>
kdtree<T,D>::~kdtree() {
    delete this->root;
}

template<class T, int D>
void kdtree<T,D>::remap() {
    memset(root,0,sizeof(node)*num);
    root->val = d(elems[0],0);
    double vals[D];
    for (int i = 1; i < num; i++) {
        cout << i << '\n';
        int depth = 0;
        T *e = elems[i];
        for (int j = 0; j < D; j++) {
            vals[j] = d(e,j);
        }
        node *cur = root;
        while (cur) {
            if (cur->val < vals[depth]) {
                if (cur->r) {
                    cur = cur->r;
                    depth++; depth %= D;
                } else {
                    cur->r = &root[i];
                    cur = cur->r;
                    cur->idx = i;
                    cur->val = vals[depth];
                }
            } else {
                if (cur->l) {
                    cur = cur->l;
                    depth++; depth %= D;
                } else {
                    cur->l = &root[i];
                    cur = cur->l;
                    cur->idx = i;
                    cur->val = vals[depth];
                }
            }
       }
    }
