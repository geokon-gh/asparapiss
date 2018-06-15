(ns asparapiss.math
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.linear :as matrix-linear]))

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
  "Take a vector of x's and build a vandermonde matrix of polynomials
  of a given degree. By default the degree matches the number of points.
  ie. it's square"
  ([x]
   (vandermonde-matrix x (count x)))
  ([x degree]
   (let [vandermonde-rows (map #(polynomial-row % degree) x)]
     (matrix/matrix
      (reduce (fn [matrix next-row] (matrix/join matrix next-row))
              vandermonde-rows)))))

(defn polynomial-function
  "Given polynomial factors, return the polynomial function
  (ie. given x, returns y)
  polynomial factors : a0 a1 a2 a3 ...
  function returned:
  y = a0 +a1x + a2x^2 + a3x^3 ..."
[indexed-polynomial-factors]
  (fn [x] [x (reduce
              (fn [accumulated-value next-exponent]
                (+ accumulated-value
                   (* (second next-exponent)
                      (math/expt x (first next-exponent)))))
              0
              indexed-polynomial-factors)]))

(defn fit-polynomial
  "Given several points, return a polynomial function (given an x, returns a y)"
  [points]
  (cond (empty? points) ;; degenerate case
        (fn [x] [x 0.0])
        :else
        (let [xs (map first points)
              ys (map second points)
              polynomial-factors
              (matrix-linear/solve (vandermonde-matrix xs)
                                   (matrix/array ys))
              indexed-polynomial-factors (-> polynomial-factors
                                             matrix/to-nested-vectors
                                             index-vector)]
          (polynomial-function indexed-polynomial-factors))))

(defn least-squares-polynomial
  "Fit a polynomial of a given degree using a naiive least-squares solution
  of the form A^T*A=A^Tb"
  [points degree]
  (cond (< degree 1) ;; degenerate case
        (fn [x] [x 0.0])
        :else
        (let [xs (map first points)
              ys (map second points)
              A (vandermonde-matrix xs degree)
              AT (matrix/transpose A)
              ATA (matrix/mmul AT A)
              ATb (matrix/to-vector (matrix/mmul AT (matrix/column-matrix ys)))
              polynomial-factors (matrix-linear/solve ATA
                                                      ATb)
              indexed-polynomial-factors (-> polynomial-factors
                                             matrix/to-nested-vectors
                                             index-vector)]
          (polynomial-function indexed-polynomial-factors))))
