(ns mire.map_generation)

(def game_items (ref #{:wood-sword :wood-armor :ruby :emerald :diamond :banana :apple :kiwi :branches}))

(def rooms_graf_head (ref {}))

(def secrets 0)

(defn gen_lvl [cur_room lvl]
  "Создание комнаты на lvl-ном уровне графа"
  (try
  (dosync
    (loop
      [lv 1
      cr cur_room
      pr nil]
      (do
        (commute cr assoc :room_name (apply str ["room " lv]) :exits (ref {}))
        (commute (:exits @cr) assoc :next (ref {}) :prev pr)
        (println lv)
          (if (< lv lvl)
            (do
              (println "we are here")
              (recur (+ lv 1) (:next @(:exits @cr)) cr)
            )
            (commute (:exits @cr) assoc :next nil)
          )
      )
    )
  )
    (catch Exception e
    (.printStackTrace e (new java.io.PrintWriter *err*)))
  )
)

(defn get_n_rand_from_set [n set]
  "Возвращает сет из n случайных элементов переданного сета (n <= len(set))"
  (loop [iter n
        rand_n_set #{}
        old_set set]
    (let [rand_value (rand-int (count old_set))]
      (if (> iter 0)
        (recur
          (- iter 1)
          (conj rand_n_set (nth (vec old_set) rand_value))
          (disj old_set (nth (vec old_set) rand_value))
        )
        rand_n_set
      )
    )
  )
)

(defn gen_sides [origin_way]
  "Принимает ключ, возвращает случайный сет размерности от 0 до 3,
   состоящий из случайных ключей сторон света"
  (let [sides (disj #{:north :south :west :east} origin_way)
        sides_count (if (nil? origin_way) 4 (rand-int 4))]
    (get_n_rand_from_set sides_count sides)
  )
)

(defn opposite_way [way]
  "Возвращает ключ противоположной стороны света"
  (keyword (way {:north "south" :east "west" :south "north" :west "east"}))
)


(defn gen_secret [room sec_count]
  "Создаёт запертую комнату и меняет ключи"
  (dosync
    (if (>= (rand-int 101) 50)
      (do
        (commute room assoc :access "locked")
        (def secrets (+ sec_count 1))
      )
    )
  )
)

(defn gen_items [items_count]
  (get_n_rand_from_set items_count @game_items)
)

(defn gen_graph [current_room direction_from_arrived prev_room lvl]
  "Принимает ссылку на мапу;
   сторону света, откуда пришли в комнату;
   ссылку на мапу, откуда пришли в комнату;
   уровень графа.
   Создаёт граф комнат.
   Переданная вначале ссылка будет указателем на структуру"
  (dosync
    (commute current_room assoc :exits (ref {}))
    (commute current_room assoc :desc "")
    (commute current_room assoc :access "open")
    (commute current_room assoc :items (ref #{}))
    (commute current_room assoc :inhabitants (ref #{}))
    (doseq [item (gen_items (+ (rand-int 3) 1))]
      (alter (:items @current_room) conj item)
    )
    (commute current_room assoc :name (str "room " lvl "-" (rand-int 1000)))
    (if (not (nil? direction_from_arrived))
      (commute (:exits @current_room) assoc (opposite_way direction_from_arrived) (ref prev_room))
    )
    (if (> (- lvl 1) 0)
      (do
        (doseq [direction (gen_sides direction_from_arrived)]
          (commute (:exits @current_room) assoc direction (ref {}))
          (gen_graph
            (direction @(:exits @current_room))
            direction
            @current_room
            (- lvl 1)
          )
        )
      )
      (gen_secret current_room secrets)
    )
  )
)

(defn gen_rooms [levels_count]
  (let [rooms (ref {})]
    (gen_graph rooms nil nil levels_count)
    rooms
  )
)

(defn print_room [room name_from]
  "Тестовая функция. Выводит название комнаты и
   название комнаты, откуда мы пришли"
  (println (str (:name @room) " from " name_from))
  (print "exits: ")
  (doseq [dirs (keys @(:exits @room))] (print dirs " "))
  (println "")
)

(defn see_graph [graph dir_from name_from]
  "Просмотр графа.
   Принимает ссылку на мапу комнаты;
   направление, откуда пришли в комнату
   название комнаты, откуда пришли"
  (if (> (count (keys @(:exits @graph))) 1)
    (do
      (print_room graph name_from)
      (doseq [dirs (keys @(:exits @graph))]
        (if (not (= dirs dir_from))
          (see_graph (dirs @(:exits @graph)) (opposite_way dirs) (:name @graph))
        )
      )
    )
    (print_room graph name_from)
  )
)
