---
title: "Advent of Code 2023 Day 3"
date: 2024-02-28T10:08:34+05:30
# weight: 1
# aliases: ["/first"]
tags: ["haskell"]
categories: ["aoc"]
author: "Upendra Upadhyay"
# author: ["Me", "You"] # multiple authors
showToc: true
TocOpen: true
draft: false
hidemeta: false
comments: true
description: "advent of code 2023 day 3"
canonicalURL: "https://canonical.url/to/page"
disableHLJS: true # to disable highlightjs
disableShare: false
disableHLJS: false
hideSummary: false
searchHidden: false
ShowReadingTime: true
ShowBreadCrumbs: true
ShowPostNavLinks: true
cover:
    image: "<image path/url>" # image path/url
    alt: "<alt text>" # alt text
    caption: "<text>" # display caption under cover
    relative: false # when using page bundles set this to true
    hidden: true # only hide on current single page
editPost:
    URL: "https://github.com/upendra1997/<path_to_repo>/blob/main/content"
    Text: "Suggest Changes" # edit text
    appendFilePath: true # to append file path to Edit link
---
# AOC-2023-3 Gear Ratios

In this [problem](https://adventofcode.com/2023/day/3), we had to parse the input problem and find all the symbols like: `#`, `*`, `+` and `$`. All numbers that are adjacent to symbols are important for us, and we need to compute the result. 


## Part 1

In this part we had to compute the sum of numbers adjacent to the symbol decsribed above. To get the results I was going for the graph traversal algorithm, and to do that in haskell I used [containers](https://hackage.haskell.org/package/containers) library to get `Data.Tree` and used State Monad from [mtl](https://hackage.haskell.org/package/mtl) to keep track of visited nodes using `get` and `put`.

To Debug the issue during the development, I used `drawForest` in the code using `Debug.Trace` and used the hardcoded input and running the `:main` in REPL again and again.

## Part 2

In this part we had to find the pair of numbers around `*` and multiply them and sum all the results from that.

------

So the solution is available at: https://github.com/upendra1997/advent-of-code-2023-hs/blob/main/day3/day3.hs

but please note that `helperFunction` and `main` are not that readable :(
