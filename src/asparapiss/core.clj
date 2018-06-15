;; **JavaFX** is used through the [**fn-fx**](https://github.com/halgari/fn-fx)
;; library. When the application is run through `lein run` it should initialize
;; the **JavaFX** GUI and **fn-fx** itself.
;;
;; [**thi.ng/geom-viz**](https://github.com/thi-ng/geom/)
;;is for making plots and other pretty SVG things
;;
;; [**FranzXaver**](afester.github.io/FranzXaver/) is for converting
;; the output SVGs to something that can be put into a Group in JavaFX. 
(ns asparapiss.core
  (:require [asparapiss.math :as math]
            [asparapiss.plot :as plot]
            [fn-fx.fx-dom :as dom] ;; The JavaFX libraries
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]))

;; # Globals

(def main-font (ui/font :family "Helvetica" :size 20))

;; # Event Handler

(defmulti handle-event
  "This is the event handler multimethod through which all events go through.
  It will 'switch' on the **:event** key"
  (fn [state event]
    (:event event)))

;; ## ClickyGraph
;; The area of the window where you click to add point.
;; The points are accumulated into the state map
;; and are then are used to generate a plot

(defui ClickyGraph
  (render [this {:keys [width height points degree]}]
          (ui/pane
           :on-mouse-pressed {:event :mouse-click ;; this part is black-magic
                              :fn-fx/include {:fn-fx/event #{:x :y}}} 
           :children [(plot/plot-points points degree width height)])))

(defmethod handle-event :mouse-click
  [state {:keys [fn-fx/includes]}]
  (let [{:keys [x y]} (:fn-fx/event includes)]
    (cond (and (> x 0) (> y 0))
          (update-in state [:points] conj [x y])
          :else
          state)))

;; ## MainWindow
;; the root node of the scene-graph. It will track the scene size
;; and redraw the plot when it changes

(defui MainWindow
  (render [this args];{:keys [points]}]
          (ui/v-box
           :id ::graph
           :style
           "-fx-base: rgb(255, 255, 255);
-fx-focus-color: transparent;"
           :listen/height {:event :resize-height ;; more black-magic
                           :fn-fx/include {::graph #{:height}}} 
           :listen/width {:event :resize-width
                          :fn-fx/include {::graph #{:width}}}
           :children [(ui/slider
                       :id ::degree-spinner
                       :min 0
                       :max (double (count (:points args)))
                       :show-tick-marks true
                       :show-tick-labels true
                       :major-tick-unit 1
                       :block-increment 1
                       :value (:degree args)
                       :listen/value {:event :change-degree ;; more black-magic
                                       :fn-fx/include {::degree-spinner #{:value}}} 
)
                      (clicky-graph args)])))

(defmethod handle-event :resize-width
  [state {:keys [fn-fx/includes]}]
  (assoc-in state [:width] (get-in includes [::graph :width])))

(defmethod handle-event :resize-height
  [state {:keys [fn-fx/includes]}]
  (assoc-in state [:height] (get-in includes [::graph :height])))

(defmethod handle-event :change-degree
  [state {:keys [fn-fx/includes]}]
  (assoc-in state [:degree] (get-in includes [::degree-spinner :value])))

;; ## Stage
;; the JavaFX top level container that stands for a window
;; The stage has a scene container for all content ie. a scene-graph of nodes.
;; Each Stage/Window displays *one* scene at a time

(defui Stage 
  (render [this args]
          (ui/stage
           :title "Asparapiss"
           :shown true
           :scene (ui/scene 
                   :root (main-window args)))))

;; # Launching fn-fx
;; - create an initial state
;; - add an intial picture to the state
;; - intialize the event handlers so that they update the state.
;;   The eventhandler multimethod itself will generate new states
;; - add a watch on the state. When the state changes, update/redraw the UI

(defn -main 
  "This is where we initialize the whole fn-fx monster"
  []
  (let [data-state (atom {:width 500.0
                          :height 500.0
                          :points [];[[0 0][100 100][200 200]]
                          :degree 0})
        handler-fn (fn [event]
                     (try
                       (swap! data-state handle-event event)
                       (catch Throwable ex
                         (println ex))))

        ui-state   (agent (dom/app (stage @data-state) handler-fn))]

    (add-watch data-state
               :ui (fn [_ _ _ _]
                     (send ui-state
                           (fn [old-ui]
                             (try
                               (dom/update-app
                                old-ui
                                (stage @data-state))
                               (catch Throwable ex
                                 (println ex)))))))))
