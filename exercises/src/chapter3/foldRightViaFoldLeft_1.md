
  def foldLeft[A, B](as: List[A], b: B)(f: (A, B) => B): B = {
    as match {
      case Nil        => b
      case Cons(h, t) => foldLeft(t, f(h, b))(f)
    }
  }


Explain foldRightViaFoldLeft_1 : 
the function to pass to foldLeft here is 
f: (A, B=>B) => (B => B) = (a : A, g : B => B) => (b : B) => g(f(a,b)) 

def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, (b: B) => b)((a: A, g: B => B) => (b: B) => g(f(a, b)))(z)


- Case l =  Nil => (b: B) => b 
  

- Case l = Cons(1, Nil)
  foldLeft(Nil, f(1, (b :B) => b))(f)

  f(1, (b :B) => b) returns  b :B => f(1,b)

  foldLeft(Nil, f(1, (b :B) => b))(f) returns a function : (b :B) => f(1,b)




- Case l = Cons(1,Cons(2, Nil)) => 

1)foldLeft(Cons(2,Nil), f(1, (b : B) => b))(f)


  f(1, (b :B) => b) returns  b :B => f(1,b)


2) Cons(2, Nil ) => foldLeft(Nil, f(2, (b : B) => f(1,b)))(f)


f(2, (b : B) => f(1,b)) returns b : B =>  { f(1,f(2,b))}

3) Nil => f(1,f(2,b))

=> foldLeft(Cons(1,Cons(2, Nil)), (b: B) => b)((a: A, g: B => B) => (b: B) => g(f(a, b))) returns b: B => f(1, f(2,b))

call b: B => f(1, f(2,b)) with z = 0 -> return f(1, (2 +0)) = 1 +2 = 3


Generalization :

when the parameters for foldRightViaFoldLeft_1 is Cons(a, b, c, d) , it returns a function : m : B => f(a,f(b, f(c,f(d,m))))
then it calls the above function with m = z
