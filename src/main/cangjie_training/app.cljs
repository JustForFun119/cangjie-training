(ns cangjie-training.app
  (:require [rum.core :as rum]
            [cangjie-training.model :as model :refer [*learner-db new-model]]
            [cangjie-training.ui :refer [app-main]]))


(defonce *app-viewmodel (atom nil)) ; overall app view state

(defn ^:dev/after-load mount-root []
  (when-let [app-div (.getElementById js/document "root")]
    (rum/mount (app-main *app-viewmodel) app-div)))

;; init called by shadow-cljs.edn init-fn
(defn init []
  (reset! *app-viewmodel (new-model @*learner-db))
  (mount-root))
