```
  def main(args: Array[String]): Unit = {
    val p: Par[List[Double]] = parMap(List.range(1, 3))(math.sqrt(_))
    val x: Seq[Double] = run(Executors.newFixedThreadPool(2))(p)
  }
  ```
  
  Consider how the block of code above works. When pass to `run`, `p`
  must be evaluated (as a Par, Par is just a description for parallel tasks which will be executed by ServiceExecutor in `run`). 
  
  ``` val p: Par[List[Double]] = parMap(List.range(1,3))(math.sqrt(_))```
  
  So `p` is the result when calling `parMap` with : `List(1,2)`, `f =
  Math.sqrt`. What `parMap` returns ?
  
 ```
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    println(s"${Thread.currentThread().getName} parMap for List=$ps")
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    val res = sequence(fbs)
    println(s"${Thread.currentThread().getName} res=$res")
    res
  }
  ```
   Fist of all, it returns a `Par[List[Double]]`. `parMap` simply return
   a Par wich will produces a `Future` whose `apply` method will call
   the `parMap` block of code inside `fork` in a new thread (via `eval`) 
  
 Next we will look at what `run` does.

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
`es`, what really happens ? `p` is the result of calling `parMap`. When
`p(es)(cb)` is executed, the `apply` of Future in `fork` method is
called, 

```
  def fork[A](a: => Par[A]): Par[A] =
    es => {
      // equal to p(es) of line 23, when line 23: p(es)(cb) is executed, it is equal to the below Future's apply is called
      new Future[A] {
        override private[chapter7] def apply(cb: A => Unit): Unit = {
          println(s"${Thread.currentThread().getName} in fork $cb")
          // a is parMap body, run a(es) in a new thread
          eval(es)(a(es)(cb))
        }

      }
    }
 ```

resulting in execute the `parMap` block of code inside `fork` is
executed.

 ```
      println(s"${Thread.currentThread().getName} parMap for List=$ps")
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    val res = sequence(fbs)
    println(s"${Thread.currentThread().getName} res=$res")
    // here with ps is List(1), res will be the result of map2(unit(List.empty[A]), lazyUnit(math.sqrt(1)))((l, f) => l :+ f))
    // the foldLeft is called once
    res
 ```
   The above block will result in a Par, and that Par will be call with
  an `es` and `apply` with the callback `cb` of `run`.
  
  So the important thing here is to understand what the code block of
  `parMap` above does and returns, what is it representation. Ok, let's
  take a look, step by step. Starts from
  
```    
  val fbs: List[Par[B]] = ps.map(asyncF(f))
```

`ps` is our List(1,2). So fbs will be a List of `Par[B]`

Next, `sequence(fbs)` will turn `List[Par[B]]` into a `Par[List[B]]`

In `sequence`, we use foldLeft to accumulate result of each Par into a
List 

```
    ps.foldLeft(unit(List.empty[A]))((l, f) => {
      println(s"in foldLeft $l, $f")
      map2(l, f)((l, f) => l :+ f))
    })
```

first round of foldLeft, 

`l = unit(List.empty[A])`, 

`f=lazyUnit(Math.sqrt(1))` 

we have a `Par[List[Double]]` par1 for this round.

Next round of foldLeft will be 

`l = par1`,

`f=lazyUnit(Math.sqrt(2))` 

and return a new `Par[List[Double]]` par2.

Now, `par2` will be the return of `sequence(fbs)`, `par2` will be called
with `es` to get an Future, that Future will be call with the callback
`cb` of `run`. To understand what `par2` return, let's take a look at
`map2` because `sequence` call foldLeft with `map2` inside.

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
  The part `p(es)` of `p(es)(cb)` in `run` results in a `Future[C]`.
  Actually `p(es)` is equal to `map2(par1, lazyUnit(Math.sqrt(2)))((l,
  f) => l :+ f))(es)`, which returns a `Future[C]` as in the code block
  above . Then `p(es)(cb)` means calling the `apply` method on the
  `Future[C]` passing the `cb` defined in the `run`. What does the
  `apply` of `Future[C]` method do ? (here C is equal to
  `List[Double]`). Inside `apply`, we call each Par with the
  ExecutorService to get a Future, from the Future, call it `apply`
  method to pass the result to the `combiner` (an Actor)
  
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
  
  When the `combiner` received both results of 2 Par, it calls the
  callback `cb` passed to the `Future[C]` on a new Thread (via `eval`
  method). Let's take a look at `p` and `p2` (values passed to `map2`),
  we have :
  
`p = par1`,

`p2=lazyUnit(Math.sqrt(2))` 

So, `par1` is called with the `es` to produce an `Future[List[Double]]`
and immediately called the Future apply to pass the `List[Double]` to
the `combiner`. So again, what is `par1` ? `par1` is a
`Par[List[Double]` we got when calling `map2` with parameters:

`p = unit(List.empty[A])`, 

`p2=lazyUnit(Math.sqrt(1))` 

calling `par1(es)` is actually to call the body of function returned by
`map2` with parameters as above. Again, it calculates
`unit(List.empty[A])(es)` to get an Future, immediately call its apply
method to pass the result (`List.empty[A]`) to the `combiner`.
`p2=lazyUnit(Math.sqrt(1))` is called with `es` to get a Future, the
Future `apply` then is called to pass the result (Math.sqrt(1)) to the
`combiner`. Finally the `combiner` calls the function `f` which combines
the results of 2 Par into a `List.empty[Double] :+ Math.sqrt(1)` which
in fact is `List(1)`. Consider `par1(es)` :

```
par1(es)(a => {
  println(s"${Thread.currentThread().getName} send a=$a")
  combiner ! Left(a)
})      
```
the parameter `a` of the callback passed to `par1(es)` is the result of
`f(a, b)` in the `combiner` of Future returned by `par1(es)` which means
`a = List.empty`, `b= Math.sqrt(1)` and `f(a,b)= List(1)`. So `a` here
is List(1), that `a` is passed to the `combiner` of Future returned from
`p(es)` (called in `run`). Similarly, for `p2=lazyUnit(Math.sqrt(2))`

```
lazyUnit(Math.sqrt(2))(es)(b => {
          println(s"${Thread.currentThread().getName} send b=$b")
          combiner ! Right(b)
})     
```

the `b` parameter of the callback is `Math.sqrt(2)`, is passed to the
`combiner`, when the `combiner` received both `List(1)` and
`Math.sqrt(2)`, it calls the callback `cb` of `run` on the result of
`f(List(1), Math.sqrt(2))` (with f is the function to append an element
to the list) in a new thread, so the final result is created : `List(1,
Math.sqrt(2))`

So, is `par1` calculated on multi threads ?

Again we need to see the code : 

```
par1(es)(a => {
  println(s"${Thread.currentThread().getName} send a=$a")
  combiner ! Left(a)
})      
```

the question is how `par1(es)` calculated ? `par1` is the `map2` call
with 2 Par parameters : `p = unit(List.empty)` and
`p2=lazyUnit(Math.sqrt(1))` , `unit` does not submit the task to the
`es` because it just returns a Future whose `apply` method will call the
passed callback method on the value passed to `unit` (here it is
`List.empty`) but `lazyUnit` use `fork`

```
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
```
inside `fork`, it calls `eval` which submit the task to a different
thread. So, `par1(es)` is calculated on multi-thread (here there are 2
threads used for `p` and `p2` ) ,further more, `p` and `p2` Future's
callbacks pass results to `combiner` which handles messages on different
threads and call the final callback `eval(es)(cb(f(a, b)))` on different
threads too. And remember, `parMap` uses `fork` too, which means the
block inside `fork` is executed on a different thread from the thread
that calls `run`.

### What about ?
  
  ``` 
  val p: Par[List[Double]] =parMap(List.range(1,10))(math.sqrt(_))
```

Almost the same. `foldLeft` with 9 rounds 

- round 1 : `map2` with `unit(List.empty)` and `lazyUnit(Math.sqrt(1))`
  -> `par1` (returned Par which will be passed to next round of
  foldLeft)
- round 2 : `map2` with `par1` and `lazyUnit(Math.sqrt(2))` -> `par2`
- round 3 : `map2` with `par2` and `lazyUnit(Math.sqrt(3))` -> `par3`
- round 4: `map2` with `par3` and `lazyUnit(Math.sqrt(4))` -> `par4`
- round 5: `map2` with `par4` and `lazyUnit(Math.sqrt(5))` -> `par5`
- round 6: `map2` with `par5` and `lazyUnit(Math.sqrt(6))` -> `par6`
- round 7: `map2` with `par6` and `lazyUnit(Math.sqrt(7))` -> `par7`
- round 8: `map2` with `par7` and `lazyUnit(Math.sqrt(8))` -> `par8`
- round 9: `map2` with `par8` and `lazyUnit(Math.sqrt(9))` -> `par9`

The last par `par9(es)(cb)` will be executed by `run`, and from which
`par8`, `par7` ... `par1` will be calculated on many different threads.
Remember, all `lazyUnit(Math.sqrt(n))(es)` will run on different thread
from the caller thread (because of `fork`), so the calculations are
really multi-thread.


