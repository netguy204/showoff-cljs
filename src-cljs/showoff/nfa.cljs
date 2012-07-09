;;; implementation of a non-deterministic finite state machine
;;; inspired by ibdknox/waltz

(ns showoff.nfa)

(defn define [name & definitions]
  (reduce (fn [nfa definition]
            (definition nfa))
          {:states {}
           :events {}
           :name name}
          definitions))

(defn define-enter [state func]
  (fn [nfa]
    (assoc-in nfa [:states state :enter] func)))

(defn define-exit [state func]
  (fn [nfa]
    (assoc-in nfa [:states state :exit] func)))

(defn define-event [name func]
  (fn [nfa]
    (assoc-in nfa [:events name] func)))

(defn realize [m]
  {:machine m
   :active-states (atom #{})})

(defn enter [rm state & args]
  (if-let [on-enter (get-in rm [:machine :states state :enter] nil)]
    (apply on-enter args))
  (swap! (:active-states rm) conj state))

(defn exit [rm state & args]
  (if-let [on-exit (get-in rm [:machine :states state :exit] nil)]
    (apply on-exit args))
  (swap! (:active-states rm) disj state))

(defn trigger [rm event & args]
  (let [event-fn (get-in rm [:machine :events event])]
    (apply event-fn rm args)
    rm))

(comment
  (def nfa
    (define :foo
      
      (define-enter :running
        (fn [] :stuff))
      
      (define-exit :running
        (fn [] :more))
      
      (define-event :go
        (fn [m]
          (enter m :running)))

      (define-event :stop
        (fn [m]
          (exit m :running)))))

  (def rnfa (realize nfa))

  (trigger rnfa :go)

  )