(defproject showoff "1.0.0-SNAPSHOT"
  :description "a clojurescript 'game' library"
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :dev-dependencies [[lein-cljsbuild "0.1.6"]]

  :cljsbuild
  {:builds
   [{:source-path "src-cljs"
     :compiler
     {:output-to "javascripts/main.js"
      :optimizations :whitespace
      :pretty-print true
      }
     }
    ]
   })