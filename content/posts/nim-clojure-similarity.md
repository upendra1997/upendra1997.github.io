---
title: "Nim Clojure Similarity"
date: 2022-11-26T11:13:56+05:30
tags: ["haskell", "rust", "clojure", "nim"]
draft: false
comments: true
categories: ["Programming Languages"]
description: "How Nim can help learn Systems Programming"
---

Hello world, I am Upendra Upadhyay. This is my first post and I have been trying to write for a long time. I think there is no better time than now.

I have been trying to learn [Haskell](https://www.haskell.org/) for past 3 years in my free time, but was never able to code anything useful; mostly did fibonacci, sieve of eratosthenes, and sudoku - which was taking lot of memory and time because of bad pruning.

### Fibonacci
```haskell
fib = 0:1:zipWith (+) fib (tail fib)
```
```
ghci> take 10 fib
[0,1,1,2,3,5,8,13,21,34]
```

### Sieve of eratosthenes
```haskell
sieve (p:ps) = p:sieve (filter (\x -> x `mod` p /= 0) ps)
prime = sieve [2..]
```
```
ghci> take 10 prime
[2,3,5,7,11,13,17,19,23,29]
```
***

All things considered, I was never able to be productive in Haskell, But I got opportunity to work in [Clojure](https://clojure.org/); which allowed me to write functional code and interop with Java/Javascript. It created a bridge to Haskell, which have great ideas with steep learning curve.


Similar case is with [Rust](https://www.rust-lang.org/), I am still trying to solve [Xorcism](https://exercism.org/tracks/rust/exercises/xorcism) , never seem to be getting closer to solution. I think Rust have great ideas but also a bit of learning curve. This where I think [Nim](https://nim-lang.org/) can become similar bridge like Clojure for Rust. It allows system programming and interop with C, C++, Javascript.

which makes me curious to learn Nim.
