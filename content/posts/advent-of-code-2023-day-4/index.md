---
title: "Advent of Code 2023 Day 4"
date: 2024-02-29T09:41:09+05:30
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
description: "advent of code 2023 day 4"
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

# AOC-2023-4 Scratchcards

This [problem](https://adventofcode.com/2023/day/4) was easy in the sense that it required just required correct parsing and following the rules to get the result in the problem.

## Part 1

Part 1 just required correct parsing and computing the solution.


## Part 2

Part 2, says that for each winning card with n numbers, we will get extra copies of next n cards, so to maintain the number of extra cards we used a state monad, to keep track of how may extra copies each card have. We use that information to multiply our score with that.

-------

Solution is available at: https://github.com/upendra1997/advent-of-code-2023-hs/blob/main/day4/day4.hs
