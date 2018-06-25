(ns asparapiss.plot
  (:require [asparapiss.math :as math]
            [asparapiss.svg2jfx :as svg2jfx]
            [thi.ng.geom.core :as g] ;; The graphing libraires
            [thi.ng.math.core :as m]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svgthing]))

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
   :data   [{:values  nil
             :attribs {:fill "none" :stroke "#f60" :stroke-width 2.25}
             :shape   (viz/svg-triangle-down 6)
             :layout  viz/svg-scatter-plot}
            {:values  nil
             :attribs {:fill "none" :stroke "#0af" :stroke-width 2.25}
             :layout  viz/svg-line-plot}
            {:values  nil
             :attribs {:fill "none" :stroke "#0ff" :stroke-width 2.25}
             :layout  viz/svg-line-plot}]})

(defn plot-points
  "Adds data (POINTS) to the spec and generates an SVG"
  [points degree output-width output-height]
  (svg2jfx/svg-to-javafx-group
   (-> (plot-spec points output-width output-height)
       (assoc-in  [:data 0 :values]
                  points)
       (assoc-in  [:data 1 :values]
                  (map (math/least-squares-polynomial points degree) (range 10000)))
       (assoc-in  [:data 2 :values]
                  (map (math/least-squares-polynomial-unstable points degree) (range 10000)))
       (viz/svg-plot2d-cartesian)
       (#(svgthing/svg {:width output-width
                        :height output-height}
                       %))
       (svgthing/serialize))))
