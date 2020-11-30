(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @player/*current-room*)
       player/eol "Exits: " (keys @(:exits @player/*current-room*)) player/eol
       (str/join player/eol (map #(str "There is " % " here." player/eol)
                           @(:items @player/*current-room*)))
       (if (empty? (disj @(:inhabitants @player/*current-room*) player/*name*))
          (str "You are alone in the room." player/eol)
          (str "Players: " (str/join ", " (disj @(:inhabitants @player/*current-room*) player/*name*)) "." player/eol))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (look))
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
    (if (rooms/room-contains? @player/*current-room* thing)
      (if (= (compare thing "keys") 0)
        (do 
          (.set player/*keys-count* (inc (.get player/*keys-count*)))
          (alter (:items @player/*current-room*) disj :keys)
          "You picked up keys.")
        (do 
          (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
          (str "You picked up the " thing ".")))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
    (if (= (compare thing "keys") 0)
      (do 
        (.set player/*keys-count* (dec (.get player/*keys-count*)))
        (alter (:items @player/*current-room*) conj :keys)
        "You dropped keys.")
      (do 
        (if (player/carrying? thing)
          (do 
            (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
            (str "You dropped the " thing "."))
          (str "You're not carrying a " thing "."))))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:" player/eol
       (str/join player/eol (seq @player/*inventory*))
       "You have " (.get player/*keys-count*) " keys."))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println message)
        (println player/prompt)))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  []
  (str/join player/eol (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

(defn score
  "Show players score."
  []
  (str "Scoreboard" player/eol
  (str/join player/eol (map #(str (key %) ": " (val %)) @player/scores))))

(defn get-points
  "MORE POINTS!!!!!!!"
  []
  (player/add-points 25000)
  "MORE POINTS!!!!!!!")

;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "help" help
               "score" score
               "hesoyam" get-points})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))
