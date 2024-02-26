---
title: "Advent of Code 2023 Day 1"
date: 2024-02-26T15:12:36+05:30
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
description: "advent of code 2023 day 1"
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

# AOC-2023-1 Trebuchet?!

I have tried solving advent of code 2023 challenges in haskell, and this was my attempt to solve day 1.
In this problem we have to find the first and last digits in a line and sum them.


problem link: https://adventofcode.com/2023/day/1

## Part 1

First part is very simple as it is just a parsing problem, and we can use my favourite library for parsing: [parsec](https://hackage.haskell.org/package/parsec) to solve it.


## Part 2

Second part have a variation that say the digits may be spelled out like `one` instead of `1`. which seems to be easy to solve as we can just change our parsing logic to consider both `1` and `one` as `1`. But when I treid to solve it using that approach, I was getting the wrong result.

so I thought that I am missing an edge case; i.e.: `twone`, which should return `21`.
There are two issues here that needs to be solved:
1. We don't want to consume the input stream event though we have found the matching token.
2. We want our parser to be greedy for searching the first digit and lazy for searching the last digit.

To understand the issues, we need to understad how parsec works.

paresc consumes the input stream and at each returns the token found and rest of the input stream, so for our example it would be:

{{< figure src="images/parsec.svg" align="center" >}}

one way to solve the issue is to run the parser at every character and collect the result, like so:

{{< figure src="images/parsec-2.svg" align="center" >}}

but that seemed to be very complicated, so I decided to reverse the string and run the parser with a switch to reverse the string.


you can check the code at: https://github.com/upendra1997/advent-of-code-2023-hs/blob/main/day1/day1.hs
