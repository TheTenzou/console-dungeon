(ns mire.rooms
  (:require [mire.map_generation :as map_gen])
)

(def rooms (ref {}))

(defn load-room [rooms file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (def items (map_gen/gen_items (rand-int 3)))
    (if (or (contains? (set @items) :gold) (contains? (set @items) :diamond) (contains? (set @items) :ruby) (contains? (set @items) :emerald))
        (def monster (map_gen/get_high_level_monster))
        (def monster (map_gen/get_low_level_monster))
    )
    (conj rooms
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc room)
            :exits (ref (:exits room))
            :items items ;:items (ref (or (:items room) #{}))
            :monsters monster
            :inhabitants (ref #{})
            :access (map_gen/gen_secret)
           }
          }
    )
  )
)

(defn load-rooms
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing room data."
  [rooms dir]
  (dosync
   (reduce load-room rooms
           (.listFiles (java.io.File. dir)))))

(defn add-rooms
  "Look through all the files in a dir for files describing rooms and add
  them to the mire.rooms/rooms map."
  [dir]
  ;(def rooms (ref (map_gen/gen_rooms 3))) 
  (dosync
   (alter rooms load-rooms dir)) 
)

(defn room-contains?
  [room thing]
  (@(:items room) (keyword thing)))
