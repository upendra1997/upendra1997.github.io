---
title: "Advent of Code 2023 Day 5"
date: 2024-03-01T09:11:31+05:30
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
description: "advent of code 2023 day 5"
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
# AOC-2023-5 If You Give A Seed A Fertilizer

This [problem](https://adventofcode.com/2023/day/5) was hardest yet and took longest to solve. so the problem asks us to create a map from seed to location such as:
```
seed -> soil -> fertilizer -> water -> light -> temprature -> humidity -> location
```
where we are given certain mappings from one entity to another, eg: `seed 1` corresponds to `soil 10`
and given this mapping and intial seed list we need to find the minimum location.

## Part 1
In this part we are given list of intial seeds, and mapping, which we use to construct the [Data.Map](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map.html) and traverse it.

An interesting choice was to handle entries which were not provided in the mapping, so basically if given key is not present in the map then we look into trasition map, which map over entities to functions(which map from entity to entity). eg: if `soil 12` was not found in the map then we look for `soil INT_MIN` in transition map, which will return a function that will take `soil 12` and returns `fertilizer 12`.

check [`toLocation`](https://github.com/upendra1997/advent-of-code-2023-hs/blob/main/day5/day5.hs#L169) function to understand the approach mentioned above.

## Part 2
In this part we are givent ranges of seeds intead of just few seeds. If we go with the previous approach, our Map will become very large since it cannot contains that many elements and we will reach out of memory very soon.

Other approach to solve the issue is to not fit all of them into memory and consider ranges as an entity. We will represent seed 5 to 10 as `Seed(5, 10)` and we will find all the intersecting entities and we will keep doing until we reach the location entity.

suppose that we had the mapping:
```
Seed(2, 7) -> Soil(10, 15)
Seed(9, 15) -> Soil(20, 26)
```

then the intersections and ther mapping would be: 
```
intersection -> mapping
Seed(5, 7) -> Soil(13, 15)
Seed(9, 10) -> Soil(20, 21)
Seed(7, 9) -> Soil(7, 9)
```

To speed up solution and preventing code going into infintite recursion, we will be using the State Monad from [mtl](https://hackage.haskell.org/package/mtl) to keep track of visited nodes and use `Data.Tree` from [containers](https://hackage.haskell.org/package/containers) library to explore the search space.

check [`locationsFn`](https://github.com/upendra1997/advent-of-code-2023-hs/blob/main/day5/day5.hs#L195) for the approach mentioned above. Please use the `drawForest` in code to visualize the search space and note that the code is not that readable :(
