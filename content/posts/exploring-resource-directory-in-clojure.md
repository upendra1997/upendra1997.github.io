---
title: "Exploring Resource Directory in Clojure"
date: 2024-03-02T14:46:06+05:30
# weight: 1
# aliases: ["/first"]
tags: ["clojure"]
categories: ["clojure"]
author: "Upendra Upadhyay"
# author: ["Me", "You"] # multiple authors
showToc: true
TocOpen: true
draft: false
hidemeta: false
comments: true
description: "looking into clojure macros, java's jar quirks and fun"
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

I was trying to explore the files available in resource directory and read only the .txt files available.

suppose that I have these files in resources:
```
resources/
└── hello
    ├── a.txt
    ├── b.txt
    └── c.not-txt
```

and this is the clojure code that I have to read the files:
```clojure
(ns jar-resource-read.core
  (:require [clojure.java.io :refer [resource file]])
  (:gen-class))

(defn read-all-txt-files [] 
  (let [txt-files (->> (resource "hello")
                       (file)
                       (file-seq)
                       (filter
                        #(-> %1
                             (.getName)
                             (.endsWith ".txt"))))]
    (for [f txt-files] 
      {:name (.getName f) 
       :content (slurp f)})))

(defn -main
  [& args]
  (doseq [{file-name :name content :content} (read-all-txt-files)]
    (println (format "%s:" file-name))
    (println content)
    (println "---------------")))
```
and running `lein run` actually returns the correct result:
```
b.txt:
hello

---------------
a.txt:
hi

---------------

```
but when I try to creat a jar using `lein uberjar` and run the jar `java -jar target/uberjar/jar-resource-read-0.1.0-SNAPSHOT-standalone.jar`, but this is the error that I got:
```
Exception in thread "main" java.lang.IllegalArgumentException: Not a file: jar:file:/home/hdggxin/workspace/jar-resource-read/target/uberjar/jar-resource-read-0.1.0-SNAPSHOT-standalone.jar!/hello
        at clojure.java.io$fn__11513.invokeStatic(io.clj:61)
        at clojure.java.io$fn__11513.invoke(io.clj:44)
        at clojure.java.io$fn__11487$G__11469__11492.invoke(io.clj:35)
        at clojure.java.io$file.invokeStatic(io.clj:424)
        at jar_resource_read.core$read_all_txt_files.invokeStatic(core.clj:5)
        at jar_resource_read.core$_main.invokeStatic(core.clj:16)
        at jar_resource_read.core$_main.doInvoke(core.clj:16)
        at clojure.lang.RestFn.invoke(RestFn.java:397)
        at clojure.lang.AFn.applyToHelper(AFn.java:152)
        at clojure.lang.RestFn.applyTo(RestFn.java:132)
        at jar_resource_read.core.main(Unknown Source)

```
further looking into it, found that there are two main reasons for these errors:
1. `jar` is a compressed file.
2. `file-seq` requires a file as argument.

so basically `file-seq` cannot iterate over a location in compressed archive, i.e. `jar:file:/home/hdggxin/workspace/jar-resource-read/target/uberjar/jar-resource-read-0.1.0-SNAPSHOT-standalone.jar!/hello`

We can uncompress the jar during the program startup and iterate over the extracted location, but this will make program not run through `lein`, so finally decided to read the file during the compile time and put the object in `jar`.

which can be done using:
```clojure
(defmacro all-txt-files []
  (into [] (read-all-txt-files)))
```
and then using this macro we can change the main function:
```clojure
(defn -main
  [& args]
  (doseq [{file-name :name content :content} (all-txt-files)]
    (println (format "%s:" file-name))
    (println content)
    (println "---------------")))
```
This obviously solves the issue and we get the same output using the jar files also :)
---------
Please check the full code [here](https://github.com/upendra1997/jar-resource-read).
