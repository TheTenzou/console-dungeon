(ns mire.server
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]
  ))

(defn- cleanup []
  "Drop all inventory and remove player from room and player list."
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "That name is in use; try again: ")
        (flush)
        (recur (read-line)))
    name))

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    ;; We have to nest this in another binding call instead of using
    ;; the one above so *in* and *out* will be bound to the socket
    (print player/eol "What is your name? ") (flush)
    (binding [player/*name* (get-unique-player-name (read-line))
              player/*current-room* (ref (@rooms/rooms :start))
              player/*inventory* (ref #{})
              player/*sets* (ref #{})]
      (dosync
        (commute (:inhabitants @player/*current-room*) conj player/*name*)
        (commute player/streams assoc player/*name* *out*)
        (.set player/*keys-count* 0)
        (.set player/*courier-available* 0)
        (.set player/*invis-available* 0)
        (.set player/*is-visible* 1)
        (.set player/*anecs* 0)

        (commute player/existing-items conj
          :wood-sword :wood-armor :ruby
          :emerald :diamond :banana :apple :kiwi :branches
          :keys :sword :bow :axe :gold :firstAidKit
        )
        (commute player/scores assoc player/*name* 0)
        (commute player/health assoc player/*name* player/max-health)
        (commute player/attack-values assoc player/*name* player/base-attack-value))

      (println (commands/look)) (print player/prompt) (flush)

      (try
        (loop [input (read-line)]
          (when input

            (if (> (player/get-health) 0)
              (do
                (println (commands/execute input))
                (.flush *err*))
              (println "You are dead!"))
            (print player/prompt) (flush)

            (if (not @player/finished)
              (recur (read-line)))))
        (finally (cleanup)))
      (println)
      (println "Game is finished!")
      (println)
      (println (commands/score)))))

(defn -main
  (  [port dir]
     (rooms/add-rooms dir)
     (defonce server (socket/create-server (Integer. port) mire-handle-client))
     (println "Launching Console Dungeon server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))
