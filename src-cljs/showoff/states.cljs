(ns showoff.states
  (:use [showoff.showoff :only [format display cycle-once]]))

(def *current-game-state* (atom nil))

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


(defn game-loop [states every-cycle-fn]
  ;; give the sound system a chance to run
  (every-cycle-fn)

  (if (= @*current-game-state* nil)
    ;; set the state to start
    (with-changed-game-state :start states
      (fn []
        (perform-after-ticks 0 states)
        (request-animation #(game-loop states every-cycle-fn) (display))))
      
    ;; otherwise, perform in our current state
    (let [next-state (cycle-once #(perform-after-ticks % states))]
      (with-changed-game-state next-state states
        (fn []
          (request-animation #(game-loop states every-cycle-fn) (display)))))))

