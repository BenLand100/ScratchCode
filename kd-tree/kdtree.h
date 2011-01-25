#ifndef KDTREE
#define KDTREE

struct node {
    node *l, *r;
    int idx;
    double val;
};

template<class T, int D>
class kdtree {
    public:
        typedef double (*dim)(T *elem, int d);
        kdtree(T *elems, int num, dim d);
        virtual ~kdtree();
        
        void remap(); //rebuilds the tree from the original array
    private:
        T *elems;
        node *root;
        int size;
};

#include "kdtree.cpp"

#endif 
