(ns guess)

(defn guess-the-number
  "Prompt the user to guess a number between 1 and 100 until they guess the correct number,
  then print a congratulatory message."
  []
  (let [target (rand-int 100)]
    (loop [guess nil]
      (println "Please enter your guess:")
      (let [guess (read-line)]
        (cond
         (= guess target) (do (println "Congratulations, you guessed the correct number!")
                             (recur))
         (< guess target) (do (println "Your guess is too low. Please try again.")
                             (recur))
         :else (do (println "Your guess is too high. Please try again.")
                   (recur)))))))

(guess-the-number)
