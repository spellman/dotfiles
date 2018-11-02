;; Source: https://github.com/roomkey/apij/wiki/Getting-started
{
 ;; s3 conflicts with lein-npm and anything that uses cheshire, amazon-sdk, etc.
 ;; dependencies
 :s3 {:plugins [[s3-wagon-private "1.3.1"]]}

 ;; Linter
 :kibit {:plugins [[lein-kibit "0.1.6-beta2"]]}

 :user {:plugins [[lein-ring "0.12.2"]
                  [lein-midje "3.2.1"] ;; Only needed if you run tests
                  [lein-pprint "1.1.1"] ;; Convenience for viewing effective project config
                  [lein-cljfmt "0.5.7"]

                  #_[s3-wagon-private "1.3.1"]

                  ;; CIDER
                  [cider/cider-nrepl "0.18.0"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]
                  ]

;;         :repl-options {:init (do
;;                                (try
;;                                  (require '[clojure.spec.alpha :as s])
;;                                  (require '[clojure.spec.test.alpha :as stest])
;;                                  (add-watch (deref #'s/registry-ref) :spec-instrumentation
;;                                             (fn [_ _ _ registry]
;;                                               (stest/instrument (filter symbol? (keys registry)))))
;;                                  (catch Exception e
;; (prn "Warning" e))))}


        :dependencies [
                       ;; CIDER
                       [org.clojure/tools.nrepl "0.2.13"]

                       ;; CLJS CIDER
                       [cider/piggieback "0.3.6"]
                       [figwheel-sidecar "0.5.16"]
                       ]


        ;; ;; CLJS CIDER
        ;; :repl-options {:nrepl-middleware [cider.nrepl/cider-middleware
        ;;                                   refactor-nrepl.middleware/wrap-refactor
        ;;                                   cider.piggieback/wrap-cljs-repl]}

        :figwheel {
                   ;; Start an nREPL server into the running figwheel process
                   :nrepl-port 7890
                   :repl false

                   ;; Load CIDER, refactor-nrepl and piggieback middleware
                   :nrepl-middleware [cider.nrepl/cider-middleware
                                      refactor-nrepl.middleware/wrap-refactor
                                      cider.piggieback/wrap-cljs-repl]
                   }
        }

 ;; CIDER
 :repl {:plugins [
                  [cider/cider-nrepl "0.18.0"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]
                  ]
        :dependencies [
                       [org.clojure/tools.nrepl "0.2.13"]

                       [cider/piggieback "0.3.6"]
                       [figwheel-sidecar "0.5.16"]
                       ]

        ;; Load CIDER, refactor-nrepl and piggieback middleware
        :nrepl-middleware [cider.nrepl/cider-middleware
                           refactor-nrepl.middleware/wrap-refactor
                           cider.piggieback/wrap-cljs-repl]
        }
 }
