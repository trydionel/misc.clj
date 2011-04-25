(ns lorenz.core
  (:import (java.awt Color Graphics RenderingHints)
           (java.awt.image BufferedImage)
           (java.awt.event MouseListener)
           (javax.swing JFrame JPanel)))

(def dim-screen [800 700])
(def iterations 250)
(def num-points 1000)
(def running (atom true))

(defn dim-axis-for-model [{:keys [c-min c-max]}]
  (vec (map #(Math/abs (- %1 %2)) c-min c-max)))

(defn axis-seqs [{:keys [dim-axis c-min c-max]}]
  [(vec (take (dim-screen 0) (iterate #(+ (/ (dim-axis 0) (dim-screen 0)) %) (c-min 0))))
   (vec (take (dim-screen 1) (iterate #(+ (/ (dim-axis 1) (dim-screen 1)) %) (c-min 1))))])

(defn map-points [{:keys [map-fn params] :as amodel} cnt]
  (let [axis-seqs (axis-seqs amodel)]
    (take cnt (repeatedly
                #(let [x (nth (axis-seqs 0) (rand-int (dim-screen 0)))
                       y (nth (axis-seqs 1) (rand-int (dim-screen 1)))]
                   (map-fn x y params))))))

(defn screen-pt-for-model [{:keys [c-min c-max]}]
  (fn [coordinate]
    (map #(* (/ (- %1 %2) (- %3 %2)) %4) coordinate c-min c-max dim-screen)))

(defn draw-map [#^Graphics canvas {:keys [n params pts] :as amodel}]
  (when (zero? n)
    (doto canvas
      (.setColor Color/WHITE)
      (.fillRect 0 0 (first dim-screen) (last dim-screen))))
  (let [point-color (int (+ 155 (* (/ 100 iterations) (- n iterations))))]
    (.setColor canvas (Color. point-color point-color point-color))
    (doseq [coords pts]
      (let [screen-pt (screen-pt-for-model amodel)
            [x1 y1] (screen-pt (first coords))
            [x2 y2] (screen-pt (second coords))]
        (.drawLine canvas x1 y1 x1 y1)
        (.drawLine canvas x2 y2 x2 y2)))
    (doto canvas
      (.setColor Color/WHITE)
      (.fillRect 0 3 160 30)
      (.setColor Color/BLACK)
      (.drawRect -5 2 161 31))
    (.drawString canvas (format "params: %s" params) 3 15)
    (.drawString canvas (format "iterations: %d" n) 3 28)))

(defn render [g {:keys [pts] :as amodel}]
  (when pts
    (let [img (BufferedImage. (first dim-screen) (last dim-screen) BufferedImage/TYPE_INT_ARGB)
          bg (.getGraphics img)]
      (.setRenderingHint bg RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (draw-map bg amodel)
      (.drawImage g img 0 0 nil)
      (.dispose bg))))

(defn animate [surface amodel]
  (let [{:keys [map-fn params]} @amodel]
    (loop [n 0 pts (map-points @amodel num-points)]
      (when (< n iterations)
        (swap! amodel merge {:n n :params params :pts pts})
        (.repaint surface)
        (Thread/sleep 20)
        (recur (inc n) (map next pts))))))

(defn animate-loop [surface amodel]
  (when @running
    (animate surface amodel)
    (swap! amodel merge {:params ((:update-fn @amodel) (:params @amodel)) :pts nil})
    (recur surface amodel)))

(defn build-model [amodel]
  (merge amodel {:dim-axis (dim-axis-for-model amodel)}))

(defmacro defmodel [name & forms]
  `(def ~name
     (build-model (merge {} ~@forms))))

(defmacro mapping [fn]
  `(hash-map :map-fn ~fn))

(defmacro initial-params [params]
  `(hash-map :params ~params))

(defmacro update-params [fn]
  `(hash-map :update-fn ~fn))

(ns-unmap 'lorenz.core 'range)
(defmacro range [range]
  `(hash-map :c-min [(first (:x ~range)) (first (:y ~range))]
             :c-max [(last  (:y ~range)) (last  (:y ~range))]))

; Duffing Map example
(defmodel duffing
          (mapping (fn [x y {:keys [a b]}]
                     (iterate
                       (fn [[xn yn]]
                         [yn
                          (+ (* (- b) xn) (* a yn) (- (* yn yn yn)))])
                       [x y])))
          (initial-params {:a 2.75 :b 0.2})
          (update-params (fn [{:keys [a b] :as params}] (merge params {:b (+ b 0.05)})))
          (range {:x [-1.6 1.6], :y [-1.6 1.6]}))

; Cherikov Standard Map example
(defmodel cherikov
          (mapping (fn [p t {:keys [k]}]
                     (iterate
                       (fn [[pn tn]]
                         [(+ pn (* k (Math/sin tn)))
                          (+ tn pn)])
                       [(mod p (* 2 Math/PI)) (mod t (* 2 Math/PI))])))
          (initial-params {:k 0})
          (update-params (fn [{:keys [k] :as params}] (merge params {:k (+ k 0.1)})))
          (range {:x [0 (* 2 Math/PI)], :y [0 (* 2 Math/PI)]}))

; 'Tinkerbell' map
(defmodel tinkerbell
          (mapping (fn [x y {:keys [a b c d]}]
                     (iterate
                       (fn [[xn yn]]
                         [(+ (* xn xn) (- (* yn yn)) (* a xn) (* b yn))
                          (+ (* 2 xn yn) (* c xn) (* d yn))])
                       [x y])))
          (initial-params {:a 0.9, :b -0.6013, :c 2, :d 0.5})
          (update-params identity)
          (range {:x [-1.5 0.5], :y [-1.5 0.5]}))

; Ikeda map
(defmodel ikeda
          (mapping (fn [x y {:keys [u]}]
                     (iterate (fn [[xn yn]]
                                (let [tn (- 0.4 (/ 6 (+ 1 (* xn xn) (* yn yn))))]
                                  [(inc (* u (- (* xn (Math/cos tn))
                                                (* yn (Math/sin tn)))))
                                   (* u (+ (* xn (Math/sin tn))
                                           (* yn (Math/cos tn))))]))
                              [x y])))
          (initial-params {:u 0.92})
          (update-params (fn [{:keys [u] :as params}] (merge params {:u (+ u 0.01)})))
          (range {:x [-20 25], :y [-25 30]}))

; Baker's map
(defmodel baker
          (mapping (fn [x y _]
                     (iterate
                       (fn [[xn yn]]
                         (let [z (Math/floor (* 2 xn))]
                           [(- (* 2 xn) z)
                            (/ (+ yn z) 2)]))
                       [x y])))
          (initial-params {})
          (update-params identity)
          (range {:x [0 1], :y [0 1]}))

; Kaplan-Yorke map
(defmodel kaplan-yorke
          (mapping (fn [x y {:keys [a]}]
                     (iterate
                       (fn [[xn yn]]
                         [(mod (* 2 xn) 1)
                          (+ (* a yn) (Math/cos (* 4 Math/PI xn)))])
                       [x y])))
          (initial-params {:a 0})
          (update-params (fn [{:keys [a] :as params}] (update-in params [:a] + 0.1)))
          (range {:x [0 1], :y [0 1]}))

(defn -main [name]
  (let [amodel (atom kaplan-yorke)
        frame  (JFrame. "Chaotic Map Simulation")
        panel  (doto (proxy [JPanel] [] (paint [g] (render g @amodel))))]
    (doto frame
      (.add panel)
      .pack
      (.setSize (dim-screen 0) (dim-screen 1))
      .show
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
    (future (animate-loop panel amodel))))
