```
  def main(args: Array[String]): Unit = {
    val p: Par[List[Double]] = parMap(List.range(1, 3))(math.sqrt(_))
    val x: Seq[Double] = run(Executors.newFixedThreadPool(2))(p)
  }
  ```
  
  Consider how the block of code above works. When pass to `run`, `p`
  must be evaluated. 
  
  ``` val p: Par[List[Double]] = parMap(List.range(1,3))(math.sqrt(_))```
  
  parMap got : List(1,2), f = Math.sqrt
  
  The body of parMap will be run in the `apply` function of the Future
  created in `fork`. 
  
  So the important thing here is to understand what `parMap` do, what is
  it representation. Ok, let's take a look, step by step.
  
```    
  val fbs: List[Par[B]] = ps.map(asyncF(f))
```

ps is our List(1,2). So fbs will be a List of `Par[B]`

Next, `sequence(fbs)` will turn `List[Par[B]]` into a `Par[List[B]]`

In sequence, we use foldLeft to accumulate result of each Par into a
List 

```
    ps.foldLeft(unit(List.empty[A]))((l, f) => {
      println(s"in foldLeft $l, $f")
      map2(l, f)((l, f) => l :+ f)
    })
```

first round of foldLeft, 

`l = unit(List.empty[A])`, 

`f=lazyUnit(Math.sqrt(1))` 

we have a `Par[List[Double]]` par1 for this round (actually, this par1
will return a `Future[List[Double]]` with value `Future[List(1)]`

next round of foldLeft will be 

`l = par1`,

`f=lazyUnit(Math.sqrt(2))` 

and return a new `Par[List[Double]]` par2.

Now, par2 will be the return of `sequence(fbs)` and it will be pass to
`run` as `p`. So next we will look at what `run` does.

```
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    val cb = { a: A =>
      println(s"run, got a=$a")
      ref.set(a); latch.countDown();
    }
    println(s"in run, p=$p, cb=$cb")

    p(es)(cb)
    latch.await()
    ref.get()
  }
```
it call `p` with an ExecutorService to get a Future, call the Future's
apply with the `cb` to get the result and saves it to the
AtomicReference ref and finally returns it. So, when `p` is call with
`es`, what really happens ? `p` is the result of `map2`

```
  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = { es =>
    new Future[C] {
      def apply(cb: C => Unit) = {
        println(s"${Thread.currentThread().getName} in map2, cb=$cb")
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            br match {
              case None    => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
          case Right(b) =>
            ar match {
              case None => br = Some(b)
              case Some(a) =>
                eval(es)(cb(f(a, b)))
            }
        }
        p(es)(a => {
          println(s"${Thread.currentThread().getName} send a=$a")
          combiner ! Left(a)
        })
        p2(es)(b => {
          println(s"${Thread.currentThread().getName} send b=$b")
          combiner ! Right(b)
        })
      }
    }
  }
  ```
  That means, when calling `p(es)` means, a `Future[C]` is returned.
  Then `p(es)(cb)` mean calling the `apply` method on the returned
  `Future[C]` passing the `cb` defined in the `run`. What the `apply` of
  `Future[C]` method do ? (here C is equal to `List[Double]`). Inside
  `apply`, we call each Par with the ExecutorService to get a Future,
  from the Future, call it `apply` method to pass the result to the
  `combiner` (an Actor) 
  
  ```
        p(es)(a => {
          println(s"${Thread.currentThread().getName} send a=$a")
          combiner ! Left(a)
        })
        p2(es)(b => {
          println(s"${Thread.currentThread().getName} send b=$b")
          combiner ! Right(b)
        })
  
  ```
  
  When the combiner received both results of 2 Par, it calls the
  callback `cb` passed to the `Future[C]` on a new Thread (via eval
  method). Let's take a look at `p` and `p2`, from the result of `map2`
  we got above, we have :
  
`p = par1`,

`p2=lazyUnit(Math.sqrt(2))` 

So, `par1` is called with the `es` to produce an `Future[List[Double]]`
and immediately called the Future apply to pass the `List[Double]` to
the combiner. So again, what is `par1` ? `par1` is a `Par[List[Double]`
we got then call `map2` with 

`p = unit(List.empty[A])`, 

`p2=lazyUnit(Math.sqrt(1))` 

calling `par1(es)` is actually to call the body of function returned by
`map2`. Again, it calculate `unit(List.empty[A])(es)` to get an Future,
immediately call its apply method to pass the result (`List.empty[A]`)
to the `combiner`. `p2=lazyUnit(Math.sqrt(1))` is called with `es` and
then the Future `apply` to pass the result (Math.sqrt(1) here) to the
`combiner`. Finally the `combiner` call the function f which combine the
results to `List.empty[Double] :+ Math.sqrt(1)` which in fact is
`List(1)`. Consider `par1(es)` :

```
par1(es)(a => {
  println(s"${Thread.currentThread().getName} send a=$a")
  combiner ! Left(a)
})      
```
the parameter `a` of the callback passed to `par1(es)` is the result of
`f(a, b)` in the `combiner` of Future returned by `par1(es)` which means
`a = List.empty`, `b= Math.sqrt(1)` So `a` here is List(1), that `a` is
pass to `combiner` of Future returned from `p(es)`. Similarly, for `p2=lazyUnit(Math.sqrt(2))` 



```
lazyUnit(Math.sqrt(2))(es)(b => {
          println(s"${Thread.currentThread().getName} send b=$b")
          combiner ! Right(b)
})     
```

the `b` parameter of the callback is `Math.sqrt(2)`, is passed to the
`combiner`, when the `combiner` received both `List(1)` and
`Math.sqrt(2)`, it call the callback `cb` of `run` on the result of
`f(List(1), Math.sqrt(2))` (with f is the function to append an element
to the list) in a new thread, so the final result is created : List(1,
Math.sqrt(2))

So, is `par1` calculated on threads different to `p` (declared inside
main method)?

again we need to see the code : 

```
par1(es)(a => {
  println(s"${Thread.currentThread().getName} send a=$a")
  combiner ! Left(a)
})      
```

the question is how `par1(es)` calculated ? `par1` is the `map2` call
with 2 Par parameters : `p = unit(List.empty)` and
`p2=lazyUnit(Math.sqrt(1))` , `unit` does not submit the task to the
`es` because it just returns a Future whose apply method is called on
the value passed to `unit` (here it is `List.empty`) but `lazyUnit` use
`fork` 

```
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
```
inside `fork`, it calls `eval` which submit the task to a different
thread. So, `par1(es)` is calculated on multi-thread (here there are 2
threads used for `p` and `p2` ) ,further more, `p` and `p2` Future's
callbacks pass results to `combiner` which handles messages on different
threads and call the final callback `eval(es)(cb(f(a, b)))` on different
threads too.
  
  
