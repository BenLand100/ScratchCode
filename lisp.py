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

class Cons:
    """A node made from pair of symbols, the base of all objects."""
    def __init__(self,car=None,cdr=None):
        """Initializes the Cons cell, defaults to ( None . None )."""
        self.car = car
        self.cdr = cdr
    def __str__(self):
        """Prints the cons in lisp-like list form."""
        res = '('
        cur = self
        while cur:
            res += str(cur.car) + ' '
            cur = cur.cdr
        return res[0:-1] + ')'
        return '( ' + str(self.car) + ' . ' + str(self.cdr) + ')'

class Symbol(str):
    """Forced upper case representation of symbols."""
    def __new__(self, val):
        return str.__new__(self,val.upper())
    def __init__(self, val):
        pass
	

class Lexer:
    """Generic lexer for stuff based on regex."""
    def __init__(self, definitions):
        """Takes tuples in the form of (name, regex) and creates a lexer."""
        import re
        self.definitions = definitions
        parts = []
        for name, part in definitions:
            parts.append("(?P<%s>%s)" % (name, part))
        self.regexpString = "|".join(parts)
        self.regexp = re.compile(self.regexpString, re.MULTILINE)
    def parse(self, text):
        """Returns a generator for the tokens based on the rules for this lexer."""
        for match in self.regexp.finditer(text):
            found = False
            for name, rexp in self.definitions:
                m = match.group(name)
                if m is not None:
                    yield (name, m)
                    break

class Scope(dict):
    """A dict-derived lexical scoping object."""
    def __init__(self, names=[], values=[], parent=None):
        """Initilizes the scope with the given values assigned to the given names and the specified parent scope."""
        self.update(zip(names,values))
        self.parent = parent
    def __getitem__(self, var):
        """Searches this scope for a var, and repeats on the parent."""
        return dict.__getitem__(self,var) if var in self else self.parent[var]
    def __setitem__(self, var,val):
        """Searches this scope for a var to assign to, and repeats on the parent."""
        dict.__setitem__(self,var,val) if var in self else self.parent.__setitem__(var,val)

class Unknown(Exception):
    """Simple wrapper for not-found exceptions."""
    pass

def quote(x):
    """Returns a quote of a cons cell."""
    return Cons(Symbol('QUOTE'),Cons(x))

def progn(forms):
    """Wraps cons forms in a progn."""
    return Cons(Symbol('PROGN'),forms)

def itertocons(l):
    """Turns a python iterable into a cons list."""
    seq = Cons()
    cur = seq
    for i in l:
        cur.cdr = Cons(i)
        cur = cur.cdr
    return seq.cdr

class Lisp:
    """Class for a lisp instance."""
    def __init__(self):
        """Sets up a lisp instance."""
        self._macros = Scope()
        self._macros.update({'DEFUN':self._defun,'SETQ':self._setq})
        self._globals = Scope()
        self._globals.update({'IF':self._if,'LAMBDA':self._lambda,'APPLY':self._apply,'PROGN':self._progn,'DEFINE':self._define,'SET':self._set,'QUOTE':self._quote,'LIST':self._list,
                              'CAR':self._car,'CDR':self._cdr,'CONS':self._cons})
        self._noeval = set(['IF','LAMBDA','DEFINE','QUOTE'])
        self._Lexer = Lexer([('string','"(\\.|[^"])+"'),('float','\\d+\\.\\d*'),('integer','\\d+'),('lparen','\\('),('rparen','\\)'),('Symbol','[^ \\(\\)]+')])
    def _lex(self,expr):
        """Turns a string representation of an expression into cons cells."""
        root = Cons(Symbol('progn'))
        top = root
        stack = []
        for name,token in self._Lexer.parse(expr):
            if name == 'lparen':
                top.cdr = Cons()
                top.cdr.car = Cons()
                stack.append(top.cdr)
                top = top.cdr.car
            elif name == 'rparen':
                top = stack.pop()
            else:
                if top.car:
                    top.cdr = Cons()
                    top = top.cdr
                if name == 'Symbol':
                    top.car = Symbol(token)
                elif name == 'integer':
                    top.car = int(token)
                elif name == 'float':
                    top.car = float(token)
                elif name == 'sting':
                    top.car = token[1:-1]
                else:
                    raise Unknown('Unknown token: ' + name)
        return root
    def _macro(self,expr,env):
        """Macro expands a cons cell expression; modifies the existing cells!"""
        print 'MACRO: ',expr
        if isinstance(expr,Cons):
            try:
                func = env[expr.car]
                if not func: raise Unknown()
                args = [env]
                cur = expr.cdr
                while cur:
                    args.append(self._macro(cur.car,env))
                    cur = cur.cdr
                return apply(func,args)
            except:
                cur = expr.cdr
                while cur:
                    cur.car = self._macro(cur.car,env)
                    cur = cur.cdr
                return expr
        else:
            return expr
    def _eval(self,expr,env):
        """Evaluates a cons cell expression."""
        print 'EVAL: ',expr
        if isinstance(expr,Symbol):
            try:
                return env[expr]
            except:
                raise Unknown('Unknown Symbol `' + str(expr) + '`')
        elif isinstance(expr,Cons):
            try:
                func = env[expr.car]
                args = [env]
                cur = expr.cdr
                if expr.car in self._noeval:
                    while cur:
                        args.append(cur.car)
                        cur = cur.cdr
                else:
                    while cur:
                        args.append(self._eval(cur.car,env))
                        cur = cur.cdr
                return apply(func,args)
            except Unknown:
                raise
            except Exception as ex:
                print ex
                raise Unknown('Unknown function `' + str(expr.car) + '`')
        else:
            return expr
    def eval(self,expr,env=None):
        """Evaluates a string representation of an expression."""
        if not env: env = self._globals
        expr = self._macro(self._lex(expr),self._macros)
        return self._eval(expr,env)
    def _quote(self,env,form):
        return form
    def _if(self,env,test,true,false=None):
        """Preforms an IF function; noeval."""
        return self._eval(true if self._eval(test,env) else false,env)
    def _lambda(self,parent,params,*forms):
        """Preforms a LAMBDA function; noeval."""
        names = []
        while params:
            names.append(params.car)
            params = params.cdr
        forms = progn(itertocons(forms))
        return lambda env,*args:self._eval(forms,Scope(names,args,parent))
    def _apply(self,env,func,*args):
        """Preforms an APPLY function."""
        return apply(func,(env,)+args)
    def _progn(self,env,*forms):
        """Preforms a PROGN function."""
        return forms[-1]
    def _define(self,env,var,form=None):
        """Adds a variable to the global scope."""
        val = self._eval(form,env)
        self._globals.update({var:val})
        return val
    def _setq(self,env,var,form):
        """Macro to quote the set target."""
        return Cons(Symbol('SET'),Cons(quote(var),Cons(form)))
    def _set(self,env,var,form):
        """Sets a slot to a value."""
        val = self._eval(form,env)
        env[var] = val
        return val
    def _list(self,evn,*elems):
        """Makes a cons list out of the arguments."""
        return itertocons(elems)
    def _defun(self,env,name,params,*forms):
        """Macro for function definition."""
        return Cons(Symbol('DEFINE'),Cons(Symbol(name),Cons(Cons(Symbol('LAMBDA'),Cons(params,itertocons(forms))))))
    def _car(self,env,cons):
        """Returns the car of a cons cell."""
        return cons.car
    def _cdr(self,env,cons):
        """Returns the crd of a cons cell."""
        return cons.cdr
    def _cons(self,env,car=None,cdr=None):
        """Creates a cons cell, defaults to (None . None)."""
        return Cons(car,cdr)
            
        
        
l = Lisp()
#print l.eval('(defun func (x y) (set (quote y) 15) (list x y) (setq y 1655) (list y x)) (func 1 5)')
print l.eval('(car (cdr (cons 1 (cons (cons 2 (cons 3 (cons 4 (cons 5)))) (cons 3 (cons 4 (cons 5)))))))')
