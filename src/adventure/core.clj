(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling that you should get out of here before something horrible happen. Exits are north, south, east, west."
           :title "in the foyer"
           :dir {:east :pantry
                 :south :muddy-pool
                 :west :gloomy-room
                 :north :corridor1}
           :contents #{}}
   :pantry {:desc "It is very dark.  You may turn on the light first. An exit leads west."
            :title "in the pantry"
            :dir {:west :foyer}
            :contents #{:apple
                        :raw-meat}}
   :muddy-pool {:desc "It is a muddy pool that you may drop into the pool and die soon, Be careful. Emmm.. There is a frog looking straight at you. So disgusting. An exit leads north"
                :title "in the muddy pool"
                :dir {:north :foyer}
                :contents #{:frog}}
   :gloomy-room {:desc "There are no windows but few old wooden cases at the corner rotted. An exit leads east."
                 :title "in the gloomy room"
                 :dir {:east :foyer}
                 :contents #{:wine}}
   :corridor1 {:desc "You are in the corridor at level 1. Exits are north, south, east, west."
               :title "in the corridor1"
               :dir {:south :foyer
                     :east :living-room
                     :west :end-of-corridor1
                     :north :lounge}
               :contents #{}}
   :end-of-corridor1 {:desc "You are at the west end of a corridor in the house. An exit leads east"
                      :title "at end of the corridor1"
                      :dir {:east :corridor1}
                      :contents #{}}
   :living-room {:desc "It's a cozy and nice living room with few paintings and a big sofa. An exit leads west. "
                 :title "in the living room"
                 :dir {:west :corridor1}
                 :contents #{:knife}}
   :lounge {:desc "You are in the big lounge. Exits are north, west, south, east."
            :title "in the lounge"
            :dir {:south :corridor1
                  :west :Alice-bedroom
                  :east :toilet
                  :north :floor}
            :contents #{}}
   :Alice-bedroom {:decs "This room has horrid pink walls and a steel safe. There are flowers and cute things all over.An exit leads east."
                   :title "in Alice's bedroom"
                   :dir {:east :lounge}
                   :contents #{:key}}
   :toilet {:desc "It's a luxurirous palatial toilet that you have never seen. An exit leads west."
            :title "in the toilet"
            :dir {:west :lounge}
            :contents #{:hammar}}
   :floor {:desc "The floor is so long that seems never end. You can either go north or south."
           :title "in the way to upper level"
           :dir {:south :lounge
                 :north :corridor2}
           :contents #{}}
   :corridor2 {:desc "You are in the corridor at level 2. Exits are north, south, east, west."
               :title "in the :corridor2"
               :dir {:south :floor
                     :north :room-of-unknown
                     :east :balcony
                     :west :end-of-corridor2}
               :contents #{}}
   :end-of-corridor2 {:desc "You are at the west end of a corridor in the house. An exit leads east"
                      :title "at the end of corridor2"
                      :dir {:east :corridor2}
                      :contents #{}}
   :balcony {:desc "You are on the balcony. A ghost is here. An exit leads west."
            :title "on the balcony"
            :dir {:west :corridor2}
            :contents #{:ghost}}
   :room-of-unknown {:desc "something sparking in the box. An exit leads south."
                     :title "in the mystery room"
                     :dir {:south :corridor2}
                     :puzzle [
                               {:question "There are 10 people died in this room and you will be the next if you answered wrong. So how many people died in this room?"
                                :answer "2"
                                }]
                     :contents #{:magic-ring}}
   })

(def adventurer
  {:location :foyer
   :inventory #{}
   :tick 0
   :seen #{}
   :health 100})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn get [object player]
  (let [location (player :location)
        content (->> the-map location :contents object)]
    (if (nil? content)
      (do (println "You can't do that.")
        player)
      (do (assoc-in player [:inventory] content)
          (str "You got " content "." )))))

(defn time [player]
  (let
    [time (player :tick)]
  (do
    (println time)))

(defn ask [player]
  (let [location (player :location)]
    (if (= location :balcony)
      (do
        (println "Hello, my name is Alice. I lost my favorite toy. If you could help me find my toy, I will tell you how to leave here") player))))

(defn solve [player]
  (let [location (get-in player [:location])
        puz (get-in the-map [location :puzzle])]
        (if (nil? puz)
          (do
            (println "There is nothing for you to solve. Please go away")
            player)
          (do
            (println (str "Here is the puzzle: " puz "."))
            player))))

(defn hit [object player]
  (let [location (player :location)]
    (if (= location :balcony)
      (do (println "Your attack pass through the ghost without any damage but irritate the ghost.")
        (update-in player [:health] - 20))
      (do (println "You hit on the ground and hurt yourself")
        (update-in player [:health] - 20)))))

(defn eat [object player]
  (let [inven (player :inventory)]
    (if (= object "apple")
      (+ player [:health] 30)
      (do (println "An apple a day, make the doctor away")
        player))
    (if (= object "raw-meat")
      (- player [:health] 50)
      (do (println "Better than nothing")
        player))
    (if (= object "frog")
      (- player [:health] 90)
      (do (println "The frog is poisonous. Dying....")
        player))
    (do (println "You can't eat that")
      player)))

(defn use [object player]
  (let [inven (player :inventory)
        location (player :location)
        content (->> the-map inven object)]
    (if (= location :room-of-unknown)
      (if (inven (name object))
        (case (name object)
          "key" (do (println "You found and opened the mystery box and a magic ring shining in the air.")
                  player)
          "hammar" (do (println "You smash the box and hurt yourself, but found nothing")
                     (update-in player [:health] - 20))
          "knife" (do (println "You careless cut yourself. You are bleeding to death")
                    (update-in player [:health] - 80))
    )

(defn drop [object player]
  (let [inven (player :inventory)]
    (if (nil? object)
      (do (println "You can't do that") player))
    (do (remove inven object)(println (str "You just dropped" object ".")))))

(defn diagnose [player]
  (let [blood (player :health)]
    (if (< blood 30)
      (do (println "You are close to death") player)
      (do (println "You are in good health") player))))


(defn tock [player]
  (update-in player [:tick] inc))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         (:or [:s] [:south] ) (go :south player)
         (:or [:w] [:west] ) (go :west player)
         (:or [:e] [:east] ) (go :east player)
         [:hit] (hit :object player)
         [:get] (get :object player)
         [:eat] (eat :object player)
         [:ask] (ask player)
         [:use] (use :object player)
         [:drop] (drop :object player)
         [:solve] (solve player)
         [:status] (status player)
         [:time] (time player)
         [:diagnose] (diagnose player)

         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main
  ""
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
