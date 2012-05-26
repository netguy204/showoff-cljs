(ns showoff.ai
  (:use [showoff.showoff :only [Tickable]])
  (:require [showoff.input :as input]))

(defprotocol Brain
  (state [brain]))

(defn state? [brain query]
  ((state brain) query))

(defrecord KeyboardBrain []
  Brain
  (state [brain] (input/state)))

(defrecord NullBrain []
  Brain
  (state [brain] {}))


(defrecord BrainRecorder [brain recording]
  Tickable
  (tick [recorder]
    (if (empty? @recording)
      ;; bootstrap
      (swap! recording conj {:brain (state brain)
                             :count 1})

      (let [idx (dec (count @recording))
            prev (peek @recording)
            prev-count (:count prev)
            state (state brain)]
        (if (= (:brain prev) state)
          (swap! recording assoc idx
                 (conj prev {:count (inc prev-count)}))
          (swap! recording conj {:brain state
                                 :count 1}))))))

(defrecord RecordedBrain [recording index-state remaining-state]
  Tickable
  (tick [brain]
    (when-not (= @index-state :end)
      (let [index (if @index-state
                    @index-state
                    0)
            remaining (dec (if @index-state
                             @remaining-state
                             (:count (aget recording 0))))]
        (cond
         ;; bootstrap
         (not @index-state)
         (do
           (reset! index-state index)
           (reset! remaining-state remaining))

         ;; moving to next chunk
         (= remaining 0)
         (if (< (inc index) (.-length recording))
           
           (do
             (reset! index-state (inc index))
             (reset! remaining-state (:count (aget recording (inc index)))))
           
           ;; end of the recording
           (reset! index-state :end))

         ;; working through current chunk
         :else
         (reset! remaining-state remaining)))))

  Brain
  (state [brain]
    (let [idx @index-state]
      (cond
       (not idx)
       #{}

       (= idx :end)
       #{}

       :else
       ((aget recording idx) :brain))))

  IHash
  (-hash [brain]
    (.getUid js/goog brain)))


