(ns showoff.states
  (:use [showoff.showoff :only [format display cycle-once]])
  (:require [showoff.timer :as timer]))

(def *current-game-state* (atom nil))
(def *requested-state* (atom nil))

(def request-animation (or window/requestAnimationFrame   
                           window/webkitRequestAnimationFrame
                           window/mozRequestAnimationFrame
                           window/oRequestAnimationFrame
                           window/msRequestAnimationFrame
                           #(window/setTimeout % (/ 1000 60))))


(defn perform-after-ticks [ticks states]
  (let [after-ticks (:after-ticks (@*current-game-state* states))]
    (after-ticks ticks)))

(defn with-changed-game-state [new-state states callback]
  (let [change-complete (fn []
                          (reset! *current-game-state* new-state)
                          (callback))]
    (if (not (= @*current-game-state* new-state))
      ;; actually changing state
      (if-let [setup (:setup (new-state states))]
        ;; we have a setup function, run and finish the state change
        ;; when it's done
        (setup change-complete)
        ;; no setup function
        (change-complete))
      
      ;; not really changing state so we don't look for a setup function
      (change-complete))))

(defn request-next-state [new-state]
  (reset! *requested-state* new-state))

(defn game-loop [states]
  ;; service our timers
  (timer/service)
  
  (if (= @*current-game-state* nil)
    ;; set the state to start
    (with-changed-game-state :start states
      (fn []
        (perform-after-ticks 0 states)
        (request-animation #(game-loop states) (display))))
      
    ;; otherwise, perform in our current state
    (let [next-state (cycle-once #(perform-after-ticks % states))
          next-state (if @*requested-state*
                       @*requested-state*
                       next-state)]
      
      (with-changed-game-state next-state states
        (fn []
          (request-animation #(game-loop states) (display)))))))

