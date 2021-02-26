(ns mire.map_generation
(:require
            [mire.player :as player]))

(def game_items (ref #{:wood-sword :wood-armor :ruby :emerald :diamond :banana :apple :kiwi :branches
:keys :sword :bow :axe :gold :firstAidKit}))

(def low_level_monsters [
    ["Ogre" 4]
    ["Troll" 5]
    ["Mad dog" 2]
    ["Ghost" 3]
    ["Brownie" 1]
])

(def high_level_monsters [
    ["Dragon" 40]
    ["Zombie" 25]
    ["Vampire" 30]
])

(def anecdots [
  "Pregnant horses run faster because they have more horsepower."

  "- You have cancer, you have six months to live.
  - Oh my god, doctor!
  - Yes, I'm joking, since April 1, what six months, at most a month.

  The meaning of life is to find yourself before alcohol addiction finds you."

  "Osteochondrosis - look at the world from a different angle!"

  "A normal person only needs another 5 minutes to get enough sleep."

  "- Mom, look at the dove! do you have bread?
  - eat without bread!"

  "- Do you want coffee in bed?
  - Not! Better in a cup!"

  "Childhood is when the cat is older than you."

  "The standard of hectare was stolen from the French Chamber of Weights and Measures."

  "Every now and then I see the news that the next battle of the century will take place. Boxers generally know how long a century lasts?
  Crab stick syndrome - you know you are being cheated, but that suits you."

  "Hello, I'm Ivan, 42, and I'm a food alcoholic. I eat up all my salary."

  "Please enter my savings into the Red Book. They are on the verge of extinction!"

  "I decided to switch to a healthy lifestyle. Сleaned a tangerine for breakfast, it suddenly splashed in the eye... Sausage doesn't do that."
])

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
        rand_n_set (ref #{})
        old_set set]
    (let [rand_value (rand-int (count old_set))]
      (if (> iter 0)
        (recur
          (- iter 1)
          (ref (conj @rand_n_set (nth (vec old_set) rand_value)))
          (disj old_set (nth (vec old_set) rand_value))
        )
        rand_n_set
      )
    )
  )
)

(defn get_low_level_monster []
    "Генерация случайного монстра низкого уровня, возвращается вектор [Имя Урон]"
    (def monsters_count (count low_level_monsters))
    (def random_index (rand-int (+ monsters_count 3)))
    (if (>= random_index monsters_count)
        ["No monsters" 0]
        (get low_level_monsters random_index)
    )
)

(defn get_high_level_monster []
    "Генерация случайного монстра высокого уровня, возвращается вектор [Имя Урон]"
    (def monsters_count (count high_level_monsters))
    (get high_level_monsters (rand-int monsters_count))
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


(defn gen_secret []
  "Создаёт запертую комнату и меняет ключи"
    (if (>= (rand-int 101) 30)
      (str "open")
      (str "locked")
    )
)

(defn gen_items [items_count]
  (get_n_rand_from_set items_count @game_items)
)
;; в создание комнаты внедрить генерацию предметов
; посмотреть, где лежат эти предметы
; заполнить файлы
(defn gen_graph [current_room direction_from_arrived lvl]
  "Принимает ссылку на мапу;
   сторону света, откуда пришли в комнату;
   ссылку на мапу, откуда пришли в комнату;
   уровень графа.
   Создаёт граф комнат.
   Переданная вначале ссылка будет указателем на структуру"
  (dosync
    (commute current_room assoc :desc "")
    (commute current_room assoc :access "open")
    (commute current_room assoc :items (ref #{}))
    (commute current_room assoc :inhabitants (ref #{}))
    (commute current_room assoc :monster [])
    (doseq [item (gen_items (+ (rand-int 3) 1))]
      (alter (:items @current_room) conj item)
    )
    (commute current_room assoc :name (str "room " lvl "-" (rand-int 1000)))
    (let [sides_arr (gen_sides direction_from_arrived)]
      (if  (nil? direction_from_arrived) (commute current_room assoc :exits (ref {}) ) )
      (if (> (- lvl 1) 0)
        (do
          (doseq [direction sides_arr]
            (commute (:exits @current_room) assoc direction (ref {}))
            (commute (direction @(:exits @current_room)) assoc :exits (ref {}))
            (commute (:exits @(direction @(:exits @current_room))) assoc (opposite_way direction) (ref @current_room))
          )
          (doseq [direction sides_arr]
            (gen_graph (direction @(:exits @current_room)) direction (- lvl 1))
          )
        )
        (gen_secret current_room secrets)
      )
    )
  )
)

(defn gen_rooms [levels_count]
  (let [rooms (ref {})]
    (gen_graph rooms nil levels_count)
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
