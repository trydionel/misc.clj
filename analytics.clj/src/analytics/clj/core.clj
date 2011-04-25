(ns analytics.clj.core
  (:require redis)
  (:use aleph
	analytics.clj.routing
	clojure.contrib.str-utils
	[clojure.contrib.duck-streams :only [pwd]])
  (:import java.io.File)
  (:gen-class))

(def *redis-server* {:host "127.0.0.1", :port 6379, :db 1})

(defmacro redis-request [& forms]
  "Issues the given forms against the redis-server"
  `(redis/with-server
     *redis-server*
     (do ~@forms)))

(defn now
  "Returns the current time in nanos since Jan 1 1970"
  []
  (System/nanoTime))

(defn record-event [sid event]
  (redis-request
   (redis/zadd sid (now) event)
   (redis/rpush "sids" sid)))

(defn get-events [sid]
  (redis-request
   (redis/zrange sid 0 -1)))

(defmacro with-layout [& forms]
  `(str "<html><body>" ~@forms "</body></html>"))

(defn make-response [body]
  {:status 200
   :headers {"Content-Type" "text/html",
	     "Content-Length" (count body)}
   :body body})

(defn not-found-response
  {:status 404
   :headers {"Content-Type" "text/html"}
   :body nil})

(defn serve-file [name]
  (File. (str (pwd) "/public/" name)))

(defn home-response [request]
  (respond! request
	    {:status 200
	     :headers {"Content-Type" "text/html"}
	     :body (serve-file "index.html")}))

(defn new-event-response [request]
  (do
    (future (record-event ((request :params) :sid) ((request :params) :event)))
    (respond! request (make-response "OK"))))

(defn get-events-response [request]
  (future
   (let [events (get-events ((request :params) :sid))
	 body (str "{events:[" (str-join "," events) "]}")]
     (respond! request (make-response body)))))

(defn javascript-response [request]
  (future
   (respond! request {:status 200
		      :headers {"Content-Type" "text/javascript"}
		      :body (serve-file (str ((request :params) :file) ".js"))})))

(defrouter router
  (route "/" home-response)
  (route "/events/new" new-event-response)
  (route "/events/:sid" get-events-response)
  (route "/:file.js" javascript-response))

(defn -main [& args]
  (run-aleph router {:port 8080}))