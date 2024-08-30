(ns dinero.utils
  (:import [java.math RoundingMode]))

(defn keyword->rounding-mode
  "Returns the rounding mode for the given keyword."
  [mode]
  (case mode
    :up RoundingMode/UP
    :down RoundingMode/DOWN
    :ceiling RoundingMode/CEILING
    :floor RoundingMode/FLOOR
    :half-up RoundingMode/HALF_UP
    :half-down RoundingMode/HALF_DOWN
    :half-even RoundingMode/HALF_EVEN
    :unnecessary RoundingMode/UNNECESSARY
    (throw (ex-info "Invalid rounding mode" {:mode mode}))))
