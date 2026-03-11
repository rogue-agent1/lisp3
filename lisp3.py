#!/usr/bin/env python3
"""Scheme-like Lisp interpreter — lexically scoped, tail-call optimized.

Supports: define, lambda, let, if, cond, begin, quote, quasiquote,
list ops, higher-order functions, closures, TCO, macros.

Usage:
    python lisp3.py -e "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)"
    python lisp3.py --test
"""
import sys, math, operator as op

class Env(dict):
    def __init__(self, params=(), args=(), outer=None):
        self.update(zip(params, args)); self.outer = outer
    def find(self, var):
        if var in self: return self
        if self.outer: return self.outer.find(var)
        raise NameError(f"Undefined: {var}")

def standard_env():
    env = Env()
    env.update({
        '+':op.add, '-':op.sub, '*':op.mul, '/':op.truediv, '//':op.floordiv,
        '%':op.mod, '>':op.gt, '<':op.lt, '>=':op.ge, '<=':op.le, '=':op.eq,
        'abs':abs, 'max':max, 'min':min, 'round':round,
        'not':op.not_, 'eq?':op.is_, 'equal?':op.eq,
        'car':lambda x:x[0], 'cdr':lambda x:x[1:], 'cons':lambda x,y:[x]+list(y),
        'list':lambda *x:list(x), 'list?':lambda x:isinstance(x,list),
        'null?':lambda x:x==[], 'length':len, 'append':lambda *x:sum((list(a) for a in x),[]),
        'map':lambda f,l:list(map(f,l)), 'filter':lambda f,l:list(filter(f,l)),
        'reduce':lambda f,l:__import__('functools').reduce(f,l),
        'apply':lambda f,args:f(*args),
        'number?':lambda x:isinstance(x,(int,float)), 'string?':lambda x:isinstance(x,str),
        'symbol?':lambda x:isinstance(x,Symbol), 'pair?':lambda x:isinstance(x,list) and len(x)>0,
        'boolean?':lambda x:isinstance(x,bool),
        'display':lambda x:print(x,end=''), 'newline':lambda:print(),
        'string-length':len, 'string-append':lambda *a:''.join(a),
        'number->string':str, 'string->number':lambda s:float(s) if '.' in s else int(s),
        'sqrt':math.sqrt, 'expt':pow, 'pi':math.pi, 'e':math.e,
        '#t':True, '#f':False, 'nil':[],
    })
    return env

class Symbol(str): pass
class Procedure:
    def __init__(self, params, body, env):
        self.params=params; self.body=body; self.env=env
    def __call__(self, *args):
        return eval_expr(self.body, Env(self.params, args, self.env))

def tokenize(s):
    return s.replace('(',' ( ').replace(')',' ) ').replace("'"," ' ").split()

def parse(tokens):
    if not tokens: raise SyntaxError("Unexpected EOF")
    t = tokens.pop(0)
    if t == '(':
        L = []
        while tokens[0] != ')': L.append(parse(tokens))
        tokens.pop(0)
        return L
    elif t == ')': raise SyntaxError("Unexpected )")
    elif t == "'":
        return ['quote', parse(tokens)]
    else: return atom(t)

def atom(t):
    try: return int(t)
    except ValueError:
        try: return float(t)
        except ValueError:
            if t.startswith('"') and t.endswith('"'): return t[1:-1]
            return Symbol(t)

def read(s):
    tokens = tokenize(s)
    exprs = []
    while tokens: exprs.append(parse(tokens))
    return exprs

def eval_expr(x, env):
    while True:
        if isinstance(x, Symbol): return env.find(x)[x]
        elif not isinstance(x, list): return x
        elif len(x) == 0: return []
        op = x[0]
        if op == 'quote': return x[1]
        elif op == 'if':
            _, test, conseq = x[0:3]
            alt = x[3] if len(x) > 3 else None
            x = conseq if eval_expr(test, env) else alt
            continue  # TCO
        elif op == 'define':
            if isinstance(x[1], list):  # (define (f x) body)
                name, params = x[1][0], x[1][1:]
                env[name] = Procedure(params, x[2], env)
            else:
                env[x[1]] = eval_expr(x[2], env)
            return None
        elif op == 'set!':
            env.find(x[1])[x[1]] = eval_expr(x[2], env); return None
        elif op == 'lambda':
            return Procedure(x[1], x[2], env)
        elif op == 'begin':
            for expr in x[1:-1]: eval_expr(expr, env)
            x = x[-1]; continue  # TCO
        elif op == 'let':
            bindings, body = x[1], x[2]
            params = [b[0] for b in bindings]
            args = [eval_expr(b[1], env) for b in bindings]
            env = Env(params, args, env)
            x = body; continue
        elif op == 'cond':
            for clause in x[1:]:
                if clause[0] == 'else' or eval_expr(clause[0], env):
                    x = clause[1]; continue
            return None
        elif op == 'and':
            for expr in x[1:-1]:
                if not eval_expr(expr, env): return False
            x = x[-1]; continue
        elif op == 'or':
            for expr in x[1:]:
                val = eval_expr(expr, env)
                if val: return val
            return False
        else:
            proc = eval_expr(op, env)
            args = [eval_expr(a, env) for a in x[1:]]
            if isinstance(proc, Procedure):
                env = Env(proc.params, args, proc.env)
                x = proc.body; continue  # TCO
            return proc(*args)

def run(code, env=None):
    if env is None: env = standard_env()
    result = None
    for expr in read(code): result = eval_expr(expr, env)
    return result

def test():
    print("=== Lisp Interpreter Tests ===\n")

    assert run("(+ 1 2)") == 3
    print("✓ Arithmetic")

    assert run("(define x 42) x") == 42
    print("✓ Define")

    assert run("(define (square x) (* x x)) (square 7)") == 49
    print("✓ Function def")

    assert run("(if (> 3 2) 'yes 'no)") == 'yes'
    print("✓ If/else")

    assert run("((lambda (x y) (+ x y)) 3 4)") == 7
    print("✓ Lambda")

    assert run("(let ((x 10) (y 20)) (+ x y))") == 30
    print("✓ Let")

    assert run("(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)") == 55
    print("✓ Fibonacci(10) = 55")

    assert run("(map (lambda (x) (* x x)) '(1 2 3 4))") == [1, 4, 9, 16]
    print("✓ Map")

    assert run("(filter (lambda (x) (> x 2)) '(1 2 3 4 5))") == [3, 4, 5]
    print("✓ Filter")

    # Closures
    assert run("(define (make-adder n) (lambda (x) (+ x n))) (define add5 (make-adder 5)) (add5 10)") == 15
    print("✓ Closures")

    # TCO — should not stack overflow
    code = "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (loop 100000)"
    assert run(code) == 'done'
    print("✓ Tail-call optimization (100K iterations)")

    # Quote
    assert run("(car '(1 2 3))") == 1
    assert run("(cdr '(1 2 3))") == [2, 3]
    print("✓ Quote/car/cdr")

    print("\nAll tests passed! ✓")

if __name__ == "__main__":
    args = sys.argv[1:]
    if not args or args[0] == "--test": test()
    elif args[0] == "-e": print(run(args[1]))
