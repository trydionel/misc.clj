(ns analytics.clj.routing
  (:use clojure.contrib.str-utils)
  (:import java.net.URLDecoder))

(defn route-re [format]
  (re-pattern
   (re-gsub #"(:[a-z]+)" "([^/]+?)" format)))

(defn- matches-route?
  "Returns true if the URI of the given request matches the regular expression of the given route."
  [{:keys [re] :as route}
   {:keys [uri] :as request}]
  (not (nil? (re-matches re uri))))

(defn route-keys [format]
  (map
   (fn [group] (keyword (second group)))
   (re-seq #":([a-z]+)" format)))

(defn- split-uri
  "Splits the given URI on the ? character.  Always returns a two element list, with empty strings as the default elements."
  [uri]
  (take 2 (concat (re-split #"\?" uri 2) '("" ""))))

(defn- decode [str]
  (. URLDecoder (decode str)))

(defn- parse-path [{:keys [keys re]} url]
  (zipmap
   keys
   (rest (re-matches re url))))

(defn- parse-querystring [query]
  (reduce
   (fn [param-map param]
     (if-let [[_ key val] (re-matches #"([^=]+)=(.*)" param)]
       (assoc param-map (keyword key) (decode val))
       param-map))
   {}
   (re-split #"&" query)))

(defn- extract-params [route request]
  (let [[path query] (split-uri (request :uri))]
    (merge
     (parse-path route path)
     (parse-querystring query))))

(defn get-route
  "Returns the first element from routes which matches the given request."
  [routes request]
  (first
   (filter
    (fn [route] (matches-route? route request))
    routes)))

(defn call-route [route request]
  ((route :action)
   (merge
    request
    {:params (extract-params route request)})))

(defmacro defrouter [name & routes]
  `(defn ~name [request#]
     (call-route
      (get-route (vector ~@routes) request#)
      request#)))

(defmacro route [format action]
  `(hash-map
    :re (route-re (str (re-gsub #"(\(.*:.+\))" "$1?" ~format) "(\\?.*)?"))
    :keys (route-keys ~format)
    :action ~action))