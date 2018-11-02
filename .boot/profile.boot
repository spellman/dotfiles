(require 'boot.repl)

(set-env! :dependencies '[[boot-deps "RELEASE"]
                          [nrepl "0.4.4"]
                          [cider/cider-nrepl "0.18.0"]
                          [refactor-nrepl "2.4.0"]])

(require '[boot-deps :refer [ancient]]
         '[cider.tasks :refer [add-middleware nrepl-server]])

(task-options! add-middleware
               {:middleware '[cider.nrepl/wrap-apropos
                              cider.nrepl/wrap-classpath
                              cider.nrepl/wrap-complete
                              cider.nrepl/wrap-debug
                              cider.nrepl/wrap-format
                              cider.nrepl/wrap-info
                              cider.nrepl/wrap-inspect
                              cider.nrepl/wrap-macroexpand
                              cider.nrepl/wrap-ns
                              cider.nrepl/wrap-spec
                              cider.nrepl/wrap-pprint
                              cider.nrepl/wrap-pprint-fn
                              cider.nrepl/wrap-profile
                              cider.nrepl/wrap-refresh
                              cider.nrepl/wrap-resource
                              cider.nrepl/wrap-stacktrace
                              cider.nrepl/wrap-test
                              cider.nrepl/wrap-trace
                              cider.nrepl/wrap-out
                              cider.nrepl/wrap-undef
                              cider.nrepl/wrap-version]})

(task-options!
 repl {:eval '(do
                (require '[clojure.spec.alpha :as s])
                (require '[clojure.spec.test.alpha :as stest])
                (add-watch (deref #'s/registry-ref) :spec-instrumentation
                           (fn [_ _ _ registry]
                             (stest/instrument (filter symbol? (keys registry))))))})
