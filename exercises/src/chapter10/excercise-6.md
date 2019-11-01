 The below 2 functions are taken from the 

```
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))


def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)
    
```

When we call foldMap with m is `endoMonoid[B]` (whose is type `Monoid[B => B]`)

as `foldMap(as, endoMonoid[B] )` will return a function with signature : `(A => B => B) => B => B`


then consider the function:

```
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
```

now, take a look inside the foldLeft, consider each round
suppose we have a list List(A1, A2, A3) and m is `endoMonoid[B]`


first round of foldLeft
 ```
 (b, a) => m.op(b, f(a))
   b : c -> identity(c)
   a : A1

   m.op(b, f(a)) :

   	f(A1) : B => B
   	b: identity

  -> m.op(b, f(a)) = identity.compose(f(A1))
```

next round of foldLeft will be

```
b = identify.compose(f(A1))
a : A2

f(A2) : B => B


-> m.op(b, f(a)) = identity.compose(f(A1)).compose(f(A2))
```
(then it continues)
....

to the last element of the list A3

```
m.op(b, f(a)) = identity.compose(f(A1)).compose(f(A2)).compose(f(A3))
```

so the 
```foldMap(as, endoMonoid[B])(f.curried) ```
will return ```identity.compose(f(A1)).compose(f(A2)).compose(f(A3))```

now, take at the final code :
```
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)
    
```

will be equal to
```
identity.compose(f(A1)).compose(f(A2)).compose(f(A3))(z)
```

which means f(A3)(z) will be evaluated first
then f(A2) and f(A1)

-> f(A1)(f(A2)(f(A3)(z))) exactly what foldRight should do !!!





