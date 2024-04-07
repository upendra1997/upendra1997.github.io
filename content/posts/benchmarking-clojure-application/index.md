---
title: "Benchmarking Clojure Application"
date: 2024-03-03T08:23:31+05:30
weight: 2
# aliases: ["/first"]
tags: ["clojure", "benchmarking"]
categories: ["clojure"]
author: "Upendra Upadhyay"
# author: ["Me", "You"] # multiple authors
showToc: true
TocOpen: true
draft: false
hidemeta: false
comments: true
description: "snippets to setup benchamrking tests in clojure"
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

This is a snippet for benchmarking clojure application which I have used quite often in clojure applications at my job. 

First an introduction to the tools:
1. [clj-asyc-profiler](https://clojure-goes-fast.com/kb/profiling/clj-async-profiler/): for flamegraphs and call graph.
2. [criterion](https://github.com/hugoduncan/criterium/): for statistically correct benchmarking.
3. [tufte](https://github.com/taoensso/tufte): application level profiling.

from this we can keep both #1 and #2 in separate profile just for benchmarking and #3 must be included in the `:default` profile, otherwise it would defeat its purpose.

below is the code snippet for having these libraries setup in project:
```clojure
(defproject clojure-benchmark "0.1.0-SNAPSHOT"
  :description "snippets to setup benchamrking tests in clojure"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :test-selectors {:default (complement :bench)
                   :not     (fn [m s] (not (contains? m (keyword s))))
                   :bench   :bench}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.taoensso/tufte "2.4.5"]]
  :profiles {:test    [{:dependencies [[org.clojure/test.check "1.1.1"]
                                      [criterium "0.4.6"]]}]
             :bench   [{:test-paths   ["bench"]
                        :global-vars  {*assert* true}
                        :jvm-opts     ["-Djdk.attach.allowAttachSelf"
                                       "-XX:+UnlockDiagnosticVMOptions"
                                       "-XX:+DebugNonSafepoints"]
                        :dependencies [[com.clojure-goes-fast/clj-async-profiler "1.0.4"]]}
                       :test]}
  :repl-options {:init-ns clojure-benchmark.core})
```
important points to note are:
1. We have created a new source locaction called `bench`, this is added so that all of these overhead of benchmarking is not included in production code, and it can be isolated to run during local run or pipeline run.
2. There are `:test-selectors` being added so that we can specifically mark some test to be benchmark tests and run only them using `lein with-profile +bench test :bench`.


The overhead mentioned above will be due to including below functionlities in the code:
1. Runnig flamgraph profiling for the whole duration of the benchmark run.
2. Making sure that `tufte` check all the namespaces for gathering the application level metrics.
3. Making sure that all benchmark are being written to a file.

The code for above mentioned functionalities is:

```clojure
(ns clojure-benchmark.bench
  (:require
    [clojure.java.io :as io]
    [clj-async-profiler.core :as prof]
    [taoensso.tufte :as tufte]))

;; necessary to print it so that jvm know that we are using its result
(def prof-started (let [start (atom (prof/start {}))
                        _     (println "profiling started." (prof/status))]
                       @start))

(alter-var-root #'tufte/*ns-filter* (constantly "*"))

;; accumulator for tufte result
(def stats-accumulator (tufte/add-accumulating-handler! {:ns-pattern "*"}))

;; before exiting make sure that we are printing profiling result
(System/setSecurityManager
  (proxy [SecurityManager] []
         (checkExit [status]
                    (let [_         (println "profiling ended." (prof/status))
                          _         (io/make-parents "profiling-result/random-file")
                          result    (prof/stop)
                          file-name (.getName result)]
                      (io/make-parents (format "profiling-result/%s" file-name))
                      (io/copy result (io/as-file (format "profiling-result/%s" file-name)))
                         (with-open [writer (io/writer (format "profiling-result/tufte-prof-%d.txt" (System/currentTimeMillis)))]
                                    (.write writer (tufte/format-grouped-pstats @stats-accumulator)))))
         (checkCreateClassLoader [& args]
                                 true)
         (checkPermission [& args]
                          true)))

```
```clojure
(ns clojure-benchmark.util
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))


(defn write-to-file [file-name f & args]
  (let [file-name (format "profiling-result/%s" file-name)
        _         (io/make-parents file-name)]
    (with-open [file (io/writer file-name)]
      (binding [*out* file]
        (apply f args)))))
```

Please check the sample test to understand how to use write the benchmark
```clojure
(ns clojure-benchmark.core-test
  (:require [clojure.test :refer :all]
            [taoensso.tufte :refer :all]
            [criterium.core :refer [benchmark report-result]]
            [clojure-benchmark.util :refer [write-to-file]]
            [clojure-benchmark.core :refer :all]))

(defn fib [n]
  (cond (<= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn noob-fib [n] (p :noob (fib n)))

(def lazy-fib (lazy-cat [0 1] (map + lazy-fib (rest lazy-fib))))

(defn pro-fib [n]
    (p :pro (nth lazy-fib n)))

(deftest a-test
  (testing "test"
    (is (= (noob-fib 10) 55))
    (is (= (noob-fib 10) 55))
    (is (= (pro-fib 13) 233))
    (is (= (pro-fib 13) 233))))


(deftest ^:bench benchmark-fib
  (profile {}
    (let [input    [10 13]
          noob-fib (benchmark (doseq [inp input] (noob-fib inp)) {:verbose true :runtime true})
          pro-fib  (benchmark (doseq [inp input] (pro-fib inp)) {:verbose true :runtime true})]
      (write-to-file "pro-fib.txt" report-result pro-fib :verbose :os :runtime)
      (write-to-file "noob-fib.txt" report-result noob-fib :verbose :os :runtime))))

```
and its output for `lein test`:
```
lein test clojure-benchmark.core-test

Ran 1 tests containing 4 assertions.
0 failures, 0 errors.
```
`lein with-profile +bench test :bench`
```
profiling started. Profiling is running for 0 seconds

lein test clojure-benchmark.core-test

Ran 1 tests containing 0 assertions.
0 failures, 0 errors.
profiling ended. Profiling is running for 182 seconds

Profiling started

```
output of tufte:
```
:tufte/nil-id,
pId                   nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total

:noob              2,426,660     6.13μs    28.29μs    60.32μs    78.76μs   299.16μs    25.60ms    38.21μs  ±76%     1.55m     52%
:pro              34,272,466   396.00ns   786.00ns     1.24μs     2.29μs     3.17μs   917.90ms     1.04μs  ±59%    35.66s     20%
:tufte/compaction         45    39.54ms   155.80ms   456.94ms   791.48ms     1.52s      1.52s    219.91ms  ±77%     9.90s      6%

Accounted                                                                                                           2.30m     78%
Clock                                                                                                               2.96m    100%
```
output of benchmark:
```
[hdggxin@nixos:~/workspace/clojure-benchmark/profiling-result]$ cat noob-fib.txt
amd64 Linux 6.1.51 4 cpu(s)
OpenJDK 64-Bit Server VM 25.362-bga
Runtime arguments: -Dfile.encoding=UTF-8 -Djdk.attach.allowAttachSelf -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints -Dclojure.compile.path=/
home/hdggxin/workspace/clojure-benchmark/target/classes -Dclojure-benchmark.version=0.1.0-SNAPSHOT -Dclojure.debug=false
Evaluation count : 2773980 in 60 samples of 46233 calls.
      Execution time sample mean : 21.751842 µs
             Execution time mean : 21.807202 µs
Execution time sample std-deviation : 2.868742 µs
    Execution time std-deviation : 2.972753 µs
   Execution time lower quantile : 20.927824 µs ( 2.5%)
   Execution time upper quantile : 25.620445 µs (97.5%)
                   Overhead used : 2.454320 ns

Found 13 outliers in 60 samples (21.6667 %)
        low-severe       4 (6.6667 %)
        low-mild         9 (15.0000 %)
 Variance from outliers : 80.7320 % Variance is severely inflated by outliers

[hdggxin@nixos:~/workspace/clojure-benchmark/profiling-result]$ cat pro-fib.txt
amd64 Linux 6.1.51 4 cpu(s)
OpenJDK 64-Bit Server VM 25.362-bga
Runtime arguments: -Dfile.encoding=UTF-8 -Djdk.attach.allowAttachSelf -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints -Dclojure.compile.path=/
home/hdggxin/workspace/clojure-benchmark/target/classes -Dclojure-benchmark.version=0.1.0-SNAPSHOT -Dclojure.debug=false
Evaluation count : 34700400 in 60 samples of 578340 calls.
      Execution time sample mean : 1.807223 µs
             Execution time mean : 1.808515 µs
Execution time sample std-deviation : 165.871690 ns
    Execution time std-deviation : 166.917813 ns
   Execution time lower quantile : 1.516781 µs ( 2.5%)
   Execution time upper quantile : 2.079813 µs (97.5%)
                   Overhead used : 2.454320 ns
```
and finally the flamegraph:
{{< figure src="images/flamegraph.png" align="center" >}}

------

please check full sample repo here: https://github.com/upendra1997/clojure-benchmark

