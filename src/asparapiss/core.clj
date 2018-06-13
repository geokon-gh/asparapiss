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
  (:require [fn-fx.fx-dom :as dom] ;; The JavaFX libraries
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]
            [clojure.math.numeric-tower :as math]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.linear :as matrix-linear]
            [thi.ng.geom.core :as g] ;; The graphing libraires
            [thi.ng.math.core :as m]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svgthing])
  (:import  [afester.javafx.svg SvgLoader]))

;; # Globals

(def main-font (ui/font :family "Helvetica" :size 20))

;; # Event Handler

(defmulti handle-event
  "This is the event handler multimethod through which all events go through.
  It will 'switch' on the **:event** key"
  (fn [state event]
    (:event event)))


;; # Converting SVGs to JavaFX objects

(defn string->stream
  "Takes a string and turns it into an input stream"
  ([s] (string->stream s "UTF-8"))
  ([s encoding]
   (-> s
       (.getBytes encoding)
       (java.io.ByteArrayInputStream.))))

(defn svg-to-javafx-group
  "Use the FranzXaver library to turn a string of XML describing an SVG 
  into a JavaFX compatible Group Node (which shows up as a picture)
  This is using Batik under the hood somehow"
  [svg-xml-string]
  (.loadSvg (SvgLoader.) (string->stream svg-xml-string)))

;; ## Vandermonde Matrix
;; We want to solve for a polynomial that will fit all the given points

;; set the core.matrix backend
(matrix/set-current-implementation :vectorz)

;; the polynomial for each point is of the form:
;; a0 + a1 x + a2 x^2 + a3 x^3 + ... = y
;; So given an *x* we need to generate the polynomials x, x^2, x^3 ...
(defn index-vector
  "Take a vector of numbers [a b c d ..] and makes an indexed-pair version
  [[0 a] [1 b] [2 c] [3 d] ..]"
  ([vector]
   (index-vector vector (count vector)))
  ([vector length]
   (map (fn [i] [i (get vector i)]) (range 0  length))))

(defn polynomial-vector
  "Given an **x**, generate a vector of [x x^2 x^3 .. x^LENGTH]"
  [x length]
  (map #(math/expt x %) (range 0 length)))

(defn polynomial-row
  "Wrapper for the previous function that puts it in a row-matrix"
  [x length]
  (matrix/row-matrix (polynomial-vector x length)))

(defn vandermonde-matrix
  "Take a vector of x's and build a vandermonde matrix"
  [x]
  (let [length (count x)
        vandermonde-rows (map #(polynomial-row % length) x)]
    (matrix/matrix (reduce (fn [matrix next-row] (matrix/join matrix next-row)) vandermonde-rows))))

(defn fit-polynomial
  "Given several points, return a polynomial function (given an x, returns a y)"
  [points]
  (let [xs (map first points)
        ys (map second points)
        polynomial-factors (matrix-linear/solve (vandermonde-matrix xs) (matrix/array ys))
        indexed-polynomial-factors (index-vector (matrix/to-nested-vectors polynomial-factors))]
    (fn [x] [x (reduce
                (fn [accumulated-value next-exponent]
                  (+ accumulated-value
                     (* (second next-exponent) (math/expt x (first next-exponent)))))
                0
                indexed-polynomial-factors)])))

;; ## Plots

(defn plot-spec
  "Given a size (WIDTH HEIGHT) the output *spec* describes how the plot looks.
  More detail are in **geom-viz**.
  The data has been left initialized"
  [points width height]
  {:x-axis (viz/linear-axis
            {:domain [0 width]
             :range  [0 width]
             ;; puts the axis out of view (can't show the grid with no axis)
             :pos    -100 
             :major 100
             })
   :y-axis (viz/linear-axis
            {:domain      [0 height]
             :range       [0 height]
             ;; puts the axis out of view (can't show the grid with no axis)
             :pos         -100 
             :label-dist  0
             :major 100
             :label-style {:text-anchor "end"}
             })
   :grid   {:attribs {:stroke "#caa"}
            :minor-x false
            :minor-y false}
   :data   [{:values  (map (fit-polynomial points) (range 10000))
             :attribs {:fill "none" :stroke "#0af" :stroke-width 2.25}
             :layout  viz/svg-line-plot}
            {:values  nil
             :attribs {:fill "none" :stroke "#f60" :stroke-width 2.25}
             :shape   (viz/svg-triangle-down 6)
             :layout  viz/svg-scatter-plot}]})

(defn plot-points
  "Adds data (POINTS) to the spec and generates an SVG"
  [points output-width output-height]
  (print points)
  (svg-to-javafx-group  (-> (plot-spec points output-width output-height)
                            (assoc-in  [:data 1 :values] points)
                             (viz/svg-plot2d-cartesian)
                             (#(svgthing/svg {:width output-width
                                              :height output-height}
                                             %))
                             (svgthing/serialize))))

;; ## ClickyGraph
;; The area of the window where you click to add point.
;; The points are accumulated into the state map
;; and are then are used to generate a plot

(defui ClickyGraph
  (render [this {:keys [width height points]}]
          (ui/pane
           :on-mouse-pressed {:event :mouse-click ;; this part is black-magic
                              :fn-fx/include {:fn-fx/event #{:x :y}}} 
           :children [(plot-points points width height)])))

(defmethod handle-event :mouse-click
  [state {:keys [fn-fx/includes]}]
  (let [{:keys [x y]} (:fn-fx/event includes)]
    (update-in state [:points] conj [x y])))

;; ## MainWindow
;; the root node of the scene-graph. It will track the scene size
;; and redraw the plot when it changes

(defui MainWindow
  (render [this args];this {:keys [points]}]
          (ui/v-box
           :id ::graph
           :style
           "-fx-base: rgb(255, 255, 255);
-fx-focus-color: transparent;"
           :listen/height {:event :resize-height ;; more black-magic
                           :fn-fx/include {::graph #{:height}}} 
           :listen/width {:event :resize-width
                          :fn-fx/include {::graph #{:width}}}
           :children [(ui/text-field
                       :id ::new-item
                       :prompt-text "Asparapiss"
                       :font main-font)
                      (clicky-graph args)])))

(defmethod handle-event :resize-width
  [state {:keys [fn-fx/includes]}]
  (assoc-in state [:width] (get-in includes [::graph :width])))

(defmethod handle-event :resize-height
  [state {:keys [fn-fx/includes]}]
  (assoc-in state [:height] (get-in includes [::graph :height])))

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
                          :points [[0 0]]})
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
