---
title: "Rust Refactoring and function closures"
date: 2023-04-15T18:03:11+05:30
# weight: 1
# aliases: ["/first"]
tags: ["Refactoring"]
categories: ["Rust"]
author: "Upendra Upadhyay"
# author: ["Me", "You"] # multiple authors
showToc: true
TocOpen: true
draft: false
hidemeta: false
comments: true
description: "Me refactoring some rust code"
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
    URL: "https://github.com/upendra1997/upendra1997.github.io/blob/main/content"
    Text: "Suggest Changes" # edit text
    appendFilePath: true # to append file path to Edit link
---
I was recently watching [Jon Gjengset](https://www.youtube.com/watch?v=gboGyccRVXI)'s Video on solving fly.io distributed system [challanges](https://fly.io/dist-sys/) 
and I always liked doing these kind of things, and it was a good excuse for me test my rust skills. I started doing these exercises in rust [here](https://github.com/upendra1997/maelstorm_rust). 
I was able to solve [echo](https://fly.io/dist-sys/1/) and [unique-id](https://fly.io/dist-sys/2/) generator challanges, but the decisions I took 
while doing them was becoming difficult to work with
for the next [challange](https://fly.io/dist-sys/3a/).
so below is me working on refactoring some code to support multiple handlers, I am not sure if it is a good decision, but here we go:
{{< asciinema "../../../data/adding-request-handler.cast" "../../../data/rust-cast.m4a" "audio/x-m4a" >}}
