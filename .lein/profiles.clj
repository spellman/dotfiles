;; Source: https://github.com/roomkey/apij/wiki/Getting-started
{
 ;; s3 conflicts with lein-npm and anything that uses cheshire, amazon-sdk, etc.
 ;; dependencies
 :s3 {:plugins [[s3-wagon-private "1.3.1"]]}

 ;; Linter
 :kibit {:plugins [[lein-kibit "0.1.7"]]}

 :user {:plugins [[lein-ring "0.12.5"]
                  [lein-midje "3.2.1"] ;; Only needed if you run tests
                  [lein-pprint "1.2.0"] ;; Convenience for viewing effective project config
                  [lein-ancient "0.6.15"]
                  [lein-cljfmt "0.6.4"]

                  #_[s3-wagon-private "1.3.1"]

                  ;; CIDER
                  [cider/cider-nrepl "0.22.0"]
                  [refactor-nrepl "2.5.0-SNAPSHOT"]
                  ]

        :aliases {"omni" ["do" ["clean"] ["with-profile" "-user" "deps" ":tree"] ["v"] ["midje"]]}

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
                       [nrepl "0.6.0"]

                       ;; CLJS CIDER
                       [cider/piggieback "0.4.0"]
                       [figwheel-sidecar "0.5.17"]
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

        :signing {:gpg-key "6E6CB388C3001DEDCD86801833587A2B87E07510"}
        }

 ;; CIDER
 :repl {:plugins [
                  [cider/cider-nrepl "0.22.0"]
                  [refactor-nrepl "2.5.0-SNAPSHOT"]
                  ]
        :dependencies [
                       ;; CIDER
                       [nrepl "0.6.0"]

                       ;; CLJS CIDER
                       [cider/piggieback "0.4.0"]
                       [figwheel-sidecar "0.5.17"]
                       ]

        ;; Load CIDER, refactor-nrepl and piggieback middleware
        :nrepl-middleware [cider.nrepl/cider-middleware
                           refactor-nrepl.middleware/wrap-refactor
                           cider.piggieback/wrap-cljs-repl]
        }
 }
