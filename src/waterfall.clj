;; Copyright (C) 2012, Eduardo Julián. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the 
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns ^{:author "Eduardo Julian <eduardoejp@gmail.com>",
      :doc "Easy and extensible CSS generation engine."}
     waterfall
  (:use clojure.template))

(declare preprocess adaptive-rules special-rules special-fns)

(defn props-merger [x y]
  (if (seq? x)
    (concat x (if (seq? y) y [y]))
    (list x y)))

; Globals
(def ^{:doc "If enabled, hacks for simulating certain properties will be added for IE.\nIt's disabled by default.."}
      ^:dynamic *ie-mode* false)

(def ^{:doc "If enabled, the appareance of known prefixed properties will cause the prefixed versions to be automatically added.\nIt's disabled by default."}
      ^:dynamic *prefix-mode* false)

(def ^:private ^:dynamic *compilation-mode* :simple)
(def ^:private ^:dynamic *outer-prefix* nil)

; <Constants>
(def ^:private +color-6-re+ #"#[0-9,a-f,A-F]{6}")
(def ^:private +color-3-re+ #"#[0-9,a-f,A-F]{3}")
(def ^:private +color-rgb-re+ #"rgb\(\d{1,3},\s*\d{1,3},\s*\d{1,3}\)")
(def ^:private +color-rgba-re+ #"rgba\(\d{1,3},\s*\d{1,3},\s*\d{1,3},\s*[01]?\.?\d{0,2}\)")
(def ^:private +color-hsl-re+ #"hsl\(\d{1,3},\s*\d{1,3}%,\s*\d{1,3}%\)")
(def ^:private +color-hsla-re+ #"hsla\(\d{1,3},\s*\d{1,3}%,\s*\d{1,3}%,\s*[01]?\.?\d{0,2}\)")
(def ^:private +color-names-re+
  ["AliceBlue" "AntiqueWhite" "Aquamarine" "Aqua" "Azure"
   "Beige" "Bisque" "Black" "BlanchedAlmond" "BlueViolet" "Blue" "Brown" "BurlyWood"
   "CadetBlue" "Chartreuse" "Chocolate" "Coral" "CornflowerBlue" "Cornsilk" "Crimson" "Cyan"
   "DarkBlue" "DarkCyan" "DarkGoldenRod" "DarkGray" "DarkGrey" "DarkGreen" "DarkKhaki" "DarkMagenta" "DarkOliveGreen"
   "Darkorange" "DarkOrchid" "DarkRed" "DarkSalmon" "DarkSeaGreen" "DarkSlateBlue" "DarkSlateGray" "DarkSlateGrey" "DarkTurquoise" "DarkViolet"
   "DeepPink" "DeepSkyBlue" "DimGray" "DimGrey" "DodgerBlue"
   "FireBrick" "FloralWhite" "ForestGreen" "Fuchsia"
   "Gainsboro" "GhostWhite" "GoldenRod" "Gold" "Gray" "Grey" "GreenYellow" "Green"
   "HoneyDew" "HotPink"
   "IndianRed " "Indigo " "Ivory"
   "Khaki"
   "Lavender" "LavenderBlush" "LawnGreen" "LemonChiffon" "LightBlue" "LightCoral" "LightCyan" "LightGoldenRodYellow" "LightGray" "LightGrey"
   "LightGreen" "LightPink" "LightSalmon" "LightSeaGreen" "LightSkyBlue" "LightSlateGray" "LightSlateGrey" "LightSteelBlue" "LightYellow" "Lime"
   "LimeGreen" "Linen"
   "Magenta" "Maroon" "MediumAquaMarine" "MediumBlue" "MediumOrchid" "MediumPurple" "MediumSeaGreen" "MediumSlateBlue" "MediumSpringGreen"
   "MediumTurquoise" "MediumVioletRed" "MidnightBlue" "MintCream" "MistyRose" "Moccasin"
   "NavajoWhite" "Navy"
   "OldLace" "OliveDrab" "Olive" "OrangeRed" "Orange" "Orchid"
   "PaleGoldenRod" "PaleGreen" "PaleTurquoise" "PaleVioletRed" "PapayaWhip" "PeachPuff" "Peru" "Pink" "Plum" "PowderBlue" "Purple"
   "Red" "RosyBrown" "RoyalBlue"
   "SaddleBrown" "Salmon" "SandyBrown" "SeaGreen" "SeaShell" "Sienna" "Silver" "SkyBlue"
   "SlateBlue" "SlateGray" "SlateGrey" "Snow" "SpringGreen" "SteelBlue"
   "Tan" "Teal" "Thistle" "Tomato" "Turquoise"
   "Violet"
   "Wheat" "WhiteSmoke" "White"
   "YellowGreen" "Yellow"])
(def ^:private +color-names-re+
  (re-pattern (str "(?i)(" (apply str (interpose "|" +color-names-re+)) ")")))
(def ^:private +css-color-re+
  (re-pattern (str "("
                   (apply str
                          (interpose "|"
                                     (map str [+color-6-re+,   +color-3-re+
                                               +color-rgb-re+, +color-rgba-re+
                                               +color-hsl-re+, +color-hsla-re+
                                               +color-names-re+])))
                   ")")))

(def ^:private +box-shadow-color-re+ #"(inset )?(\d+)px (\d+)px (\d+px )*(#[0-9,a-f,A-F]{6}|#[0-9,a-f,A-F]{3}|rgba?\(\d+,\s?\d+,\s?\d+.*\)|hsla?\(\d+,\s*\d+%,\s*\d+%].*\))")

; <Utils>
(defn simple? [] (or (= *compilation-mode* :simple) nil))

; Color Translation
(defn- hue->rgb [p q t]
  (let [t (if (< t 0) (inc t) t)
        t (if (> t 1) (dec t) t)]
    (cond (< t 1/6) (* (+ p (- q p)) 6 t)
          (< t 1/2) q
          (< t 2/3) (* (+ p (- q p)) 6 (double (- 2/3 t)))
          :else p)))

(defn- hsl->rgb [h s l]
  (let [[r g b] (if (zero? s)
                  [l l l]
                  (let [q (if (< l 0.5) (* l (inc s)) (+ l s (- (* s l))))
                        p (- (* 2 l) q)]
                    [(hue->rgb p q (+ h (/ 1.0 3)))
                     (hue->rgb p q h)
                     (hue->rgb p q (- h (/ 1.0 3)))]))]
    (map #(Math/round (* 255.0 %)) [r g b])))

(def ^:private +hexa-digits+ [0 1 2 3 4 5 6 7 8 9 \A \B \C \D \E \F])
(defn- int->hexa [i]
  (str (+hexa-digits+ (int (/ i 16)))
       (+hexa-digits+ (mod i 16))))

(def ^:private +color-8-chunks-re+ #"#([0-9,a-f,A-F]{2})([0-9,a-f,A-F]{2})([0-9,a-f,A-F]{2})([0-9,a-f,A-F]{2})")
(def ^:private +color-6-chunks-re+ #"#([0-9,a-f,A-F]{2})([0-9,a-f,A-F]{2})([0-9,a-f,A-F]{2})")
(def ^:private +color-rgba-chunks-re+ #"rgba\((\d{1,3}),\s*(\d{1,3}),\s*(\d{1,3}),\s*([01]?\.?\d{0,2})\)")
(def ^:private +color-rgb-chunks-re+ #"rgb\((\d{1,3}),\s*(\d{1,3}),\s*(\d{1,3})\)")
(def ^:private +color-hsla-chunks-re+ #"hsla\((\d{1,3}),\s*(\d{1,3})%,\s*(\d{1,3})%,\s*([01]?\.?\d{0,2})\)")
(def ^:private +color-hsl-chunks-re+ #"hsl\((\d{1,3}),\s*(\d{1,3})%,\s*(\d{1,3})%\)")

(defmulti ^:private chunks->ie-color-str (fn [t & _] t))
(defmethod chunks->ie-color-str :6 [_ r g b] (str "#" r g b "FF"))
(defmethod chunks->ie-color-str :8 [_ r g b a] (str "#" r g b a))
(defmethod chunks->ie-color-str :rgb [_ r g b] (str (apply str "#" (map (comp int->hexa #(Integer/parseInt %)) [r g b])) "FF"))
(defmethod chunks->ie-color-str :rgba [_ r g b a]
  (str (apply str "#" (map (comp int->hexa #(Integer/parseInt %)) [r g b])) (->> a Float/parseFloat (* 255.0) Math/round int->hexa)))
(defmethod chunks->ie-color-str :hsl [_ h s l]
  (let [[h s l] (map #(Integer/parseInt %) [h s l])
        [r g b] (map str (hsl->rgb (/ h 360.0) (/ s 100.0) (/ l 100.0)))]
    (chunks->ie-color-str :rgb r g b)))
(defmethod chunks->ie-color-str :hsla [_ h s l a]
  (let [[h s l] (map #(Integer/parseInt %) [h s l])
        [r g b] (map str (hsl->rgb (/ h 360.0) (/ s 100.0) (/ l 100.0)))]
    (chunks->ie-color-str :rgba r g b a)))

(defn- color->ie-color-str [color]
  (let [meth (if-let [[_ r g b a] (re-find +color-8-chunks-re+ color)]
               (cons :8 [r g b a])
               (if-let [[_ r g b] (re-find +color-6-chunks-re+ color)]
                 (cons :6 [r g b])
                 (if-let [[_ r g b a] (re-find +color-rgba-chunks-re+ color)]
                   (cons :rgba [r g b a])
                   (if-let [[_ r g b] (re-find +color-rgb-chunks-re+ color)]
                     (cons :rgb [r g b])
                     (if-let [[_ h s l a] (re-find +color-hsla-chunks-re+ color)]
                       (cons :hsla [h s l a])
                       (if-let [[_ h s l] (re-find +color-hsl-chunks-re+ color)]
                         (cons :hsl [h s l])))
                     ))))]
    (if meth (apply chunks->ie-color-str meth))))

; Gradient Generation
(def ^:private +gradient-sides+ #{:top :bottom :left :right})
(def ^:private +gradient-sides-inverse+ {:top "to bottom", :bottom "to top", :left "to right", :right "to left"})
(def ^:private from->start-end {:top ["left top" "left bottom"]
                                :bottom ["left bottom" "left top"]
                                :left ["top left" "top right"]
                                :right ["top right" "top left"]})

(defn webkit-gradient [pname type start end & stops]
  (let [from (first stops)
        to (last stops)
        stops (next (butlast stops))
        evenly? (not (some vector? stops))
        stops (if evenly?
                (let [c (inc (count stops))
                      p (Math/round (/ 100.0 c))
                      ps (take (count stops) (iterate #(+ p) p))]
                  (map (fn [[k v]] [k v]) (partition 2 (interleave ps stops))))
                stops)
        css (str "from(" (name from) "),"
                 (apply str (interleave (map (fn [[p c]] (str "color-stop(" (if (integer? p) (str p "%") p) "," (name c) ")")) stops)
                                        (repeat ",")))
                 "to(" (name to) ")")]
    (str "-webkit-gradient(" (name type) "," (name start) "," (name end) "," css ")")))

(defn- webkit-linear-gradient "Creates a linear gradient for older versions of webkit."
  [pname start end & stops] (apply webkit-linear-gradient pname :linear start end stops))

(defn- webkit-radial-gradient "Creates a radial gradient for older versions of webkit."
  [pname start end & stops] (apply webkit-linear-gradient pname :radial start end stops))

(defn- ie-gradient "Creates a linear gradient for IE using the \"filter\" property."
  [start-clr end-clr]
  (if *ie-mode*
    {:!filter (str "progid:DXImageTransform.Microsoft.gradient(startColorstr='"
                   (color->ie-color-str start-clr)
                   "', endColorstr='"
                   (color->ie-color-str end-clr)
                   "')")}
    nil))

(defn- linear-gradient "Create a linear gradient for the given property."
  [pname from & color-stops]
  (let [color-stops (if (+gradient-sides+ from) color-stops (cons from color-stops))
        from (if (+gradient-sides+ from) from :top)
        css (apply str (interpose ", " (map name (cons (+gradient-sides-inverse+ from) color-stops))))
        prefix-css (apply str (interpose ", " (map name (cons from color-stops))))
        wk-start-end (from->start-end from)
        webkit-css (apply webkit-gradient pname :linear (concat wk-start-end color-stops))
        ie-css (ie-gradient (first color-stops) (last color-stops))]
    (merge ie-css
           {pname (list 
                    (str "-ms-linear-gradient(" prefix-css ")")
                    (str "-o-linear-gradient(" prefix-css ")")
                    (str "-moz-linear-gradient(" prefix-css ")")
                    webkit-css
                    (str "-webkit-linear-gradient(" prefix-css ")")
                    (str "linear-gradient(" css ")"))})))

(defn- radial-gradient "Create a radial gradient for the given property."
  [pname & description]
  (let [css (apply str (interpose "," description))]
    {pname (list 
             (str "-ms-radial-gradient(" css ")")
             (str "-o-radial-gradient(" css ")")
             (str "-moz-radial-gradient(" css ")")
             (str "-webkit-radial-gradient(" css ")")
             (str "radial-gradient(" css ")"))}))

; Shadows
(defn ie-box-shadow [x y color]
  (let [strength (Math/round (Math/sqrt (+ (* x x) (* y y))))
        direction (int (Math/toDegrees (Math/atan2 x y)))
        color (cond (.startsWith color "#") (if (= 4 (count color))
                                              (apply str "#" (interleave (rest color) (rest color)))
                                              color)
                    
                    (or (.startsWith color "rgb")
                        (.startsWith color "hsl"))
                    (color->ie-color-str color))
        color (if (= 9 (count color)) (apply str (take 7 color)) color)]
    {:!filter (str "progid:DXImageTransform.Microsoft.Shadow(Strength=" strength ", Direction=" direction ", Color='" color "')")}))

; CSS Generation
(defn- add-tab [t] (str t \tab))
(defn- separate [pred coll] [(filter pred coll) (filter (complement pred) coll)])
(defn- group-filter [[k v]] (map? v))

(defn- preprocess-groups [rule]
  (reduce (fn [rule [k v]]
            (reduce (fn [rule [k2 v]] (conj rule [(str (name k) "-" (name k2)) v]))
                    (if (v :&global) (conj rule [k (v :&all)]) rule)
                    v))
          (vec (filter (complement group-filter) rule))
          (filter group-filter rule)))
(defn- prop->css [n v tabs]
  (cond (true? v) (str (and (simple?) (str "\n\t" tabs)) (name n) ": " (name n) ";")
        (false? v) (str (and (simple?) (str "\n\t" tabs)) (name n) ": none;")
        (or (string? v) (keyword? v)) (str (and (simple?) (str "\n\t" tabs)) (name n) ": " (name v) ";")
        (seq? v) (apply str (for [v v] (prop->css n v tabs)))
        (vector? v) (let [[sr & args] v
                          res (preprocess (apply conj {} (apply (@special-fns sr) n args)))]
                      (apply str (interpose (and (simple?) "\n") (for [[n v] res] (prop->css n v tabs)))))
        (nil? v) ""
        :else (str (and (simple?) (str "\n\t" tabs)) (name n) ": " v ";")))
(defn- rule->css [rname rprops tabs]
  (if-not (empty? rprops)
    (let [rprops (preprocess-groups (vec rprops))]
      (str tabs (if (set? rname)
                  (apply str (interpose ", " (map name rname)))
                  (name rname))
           " {"
           (apply str (map (fn [[n v]] (if (and n (not (nil? v)))
                               (prop->css n v tabs)))
                           rprops))
           (and (simple?) (str "\n" tabs)) "}" (and (simple?) "\n")))))

; <Rules>
; Adaptive Rules (rules that provide multiple implementations for multiple browsers).
(defn- !box-shadow [rules x]
  (merge (assoc rules :box-shadow x, :-moz-box-shadow x, :-webkit-box-shadow x)
         (if (and *ie-mode* x)
           (let [[_ _ x y _ c] (re-find +box-shadow-color-re+ x)
                 [x y] (map #(Integer/parseInt %) [x y])]
             (ie-box-shadow x y c)))))
(defn- !opacity [rules x] (merge-with props-merger (assoc rules :opacity (float x))
                                 (if *ie-mode*
                                   {:!filter (str "alpha(opacity=" (int (* x 100)) ")")}
                                   nil)))
(defn- !filter [rules x] (merge-with props-merger rules {:filter x, :-ms-filter (pr-str x)}))

(defmacro ^:private defadaptive-rule [sym prefixes]
  `(defn- ~(symbol (str "!" sym)) [rules# x#]
     (apply assoc rules#
            (interleave (map #(str % ~(name sym)) (map name (cons "" ~prefixes)))
                        (repeat x#))
            )))

(do-template [<sym>]
  (defadaptive-rule <sym> ["-webkit-"])
  transform-style
  column-span
  perspective, perspective-origin
  transform-style
  )

(do-template [<sym>]
  (defadaptive-rule <sym> ["-webkit-" "-moz-"])
  animation, animation-name, animation-iteration-count, animation-timing-function
  animation-duration, animation-direction, animation-play-state, animation-delay
  border-radius
  appearance
  backface-visibility
  box-ordinal-group, box-orient, box-pack, box-align, box-lines
  column-count, column-gap, column-rule, column-rule-color, column-rule-style, column-rule-width, column-width, columns
  )

(do-template [<sym>]
  (defadaptive-rule <sym> ["-webkit-" "-moz-" "-o-"])
  transition, transition-property, transition-duration, transition-timing-function, transition-delay)

(do-template [<sym>]
  (defadaptive-rule <sym> ["-webkit-" "-moz-" "-ms-" "-o-"])
  transform, transform-origin)

; Special Rules
(defn- $size [rules x]
  (if (vector? x)
    (assoc rules :width (first x) :height (second x))
    (assoc rules :width x :height x)))
(defn- $position [rules x]
  (if (coll? x)
    (let [[type top bottom left right] x]
      (assoc rules :position type :top top :bottom bottom :left left :right right))
    (assoc rules :position x)))
(defn- +linear-gradient [pname from & color-stops] (apply linear-gradient pname from color-stops))
(defn- +radial-gradient [pname & description] (apply radial-gradient pname (map #(if (keyword? %) (name %) %) description)))

(do-template [<sym> <prefix>] ; I <3 Metaprogramming!
  (def <sym>
    (->> *ns* ns-map
      (filter (fn [[k v]] (and (-> v meta :ns)
                               (-> v meta :ns ns-name (= 'waterfall))
                               (.startsWith (name k) <prefix>)
                               (not (.endsWith (name k) <prefix>)))))
      (map (fn [[k v]] [(keyword k) (var-get v)]))
      flatten
      (apply hash-map)
      atom))
  adaptive-rules "!"
  special-rules "$"
  special-fns "+")

(defn- preprocess [rules]
  (let [rules (reduce (fn [rules [k v]] (if (and *prefix-mode* (contains? rules k)) (v (dissoc rules k) (rules k)) rules))
                      rules @adaptive-rules)
        rules (reduce (fn [rules [k v]] (if (contains? rules k) (v (dissoc rules k) (rules k)) rules))
                      rules @special-rules)]
    (if (some (fn [[k v]] (let [k (name k)] (or (.startsWith k "!") (.startsWith k "$")))) rules)
      (recur rules)
      rules)))

; <CSS Generation>
(defn css
  "Transforms a Clojure vector into valid CSS."
  ([[rname rprop & subrules] tabs]
   (let [subrules (filter (complement nil?) subrules)
         subrules (if-not (map? rprop) (cons rprop subrules) subrules)
         rprop (if (map? rprop) rprop)
         [rule-mixins subrules] (separate seq? subrules)
         subrules (apply concat subrules rule-mixins)
         [mixins subrules] (separate map? subrules)
         rprop (if (empty? mixins)
                 rprop
                 (apply merge-with props-merger rprop mixins))
         [pseudoclasses subrules] (separate #(and (-> % first) (-> % first name (.startsWith "&"))) subrules)
         rprop (preprocess rprop)
         rprop (if *outer-prefix*
                 (->> rprop (filter (fn [[k v]]
                                      (let [k (name k)] (if *outer-prefix*
                                                          (cond (= *outer-prefix* "") (or (.contains k "filter") (not (.startsWith k "-")))
                                                                :else (or (.contains k "filter") (not (.startsWith k "-")) (.startsWith k *outer-prefix*))))
                                        )))
                   (map (fn [[k v]] (cond (and (string? v) (.startsWith v "-") (not (.startsWith v *outer-prefix*)))
                                          nil
                                          
                                          (and (seq? v) (some #(and (.startsWith % "-") (not (.startsWith % *outer-prefix*))) v))
                                          [k (filter #(or (not (.startsWith % "-")) (.startsWith % *outer-prefix*)) v)]
                                          
                                          :else
                                          [k v])))
                   (filter (complement nil?))
                   (apply concat)
                   (apply hash-map))
                 rprop)
         my-css (rule->css rname rprop tabs)
         sub-tabs (if my-css (add-tab tabs) tabs)
         subrules (filter (complement nil?) subrules)]
     (apply str my-css
            (map #(css % sub-tabs)
                 (concat (map (fn [[h & body]] (cons (str (name rname) ":" (.substring (name h) 1)) body))
                              pseudoclasses)
                         (map (fn [[h & body]] (cons (str (name rname) " " (name h)) body))
                              subrules))))
     ))
  ([rule] (if (string? rule) rule (css rule nil))))

(defn save-css! [path mode & rules]
  {:pre [(#{:simple :advanced} mode)]}
  (let [[lists rules] (separate seq? rules)
        rules (apply concat rules lists)]
    (binding [*compilation-mode* mode]
      (spit path (apply str (map css rules))))))

; <Fns>
(defn key-frame [kname & steps]
  (let [_css (->> steps (partition 2)
               (map (fn [[k v]]
                      [(cond (float? k) (str (int (* k 100)) "%")
                             (integer? k) (str k "%")
                             (keyword? k) (name k)
                             :else k) v]))
               (map vec))
        [css moz-css webkit-css ms-css o-css]
        (for [prefix ["" "-moz-" "-webkit-" "-ms-" "-o-"]]
          (binding [*outer-prefix* prefix]
            (apply str (map css _css))))]
    (list
      (str "@keyframes " (name kname) " { " (apply str css) " }")
      (str "@-moz-keyframes " (name kname) " { " (apply str moz-css) " }")
      (str "@-webkit-keyframes " (name kname) " { " (apply str webkit-css) " }")
      (str "@-ms-keyframes " (name kname) " { " (apply str ms-css) " }")
      (str "@-o-keyframes " (name kname) " { " (apply str o-css) " }"))))

(defn font-face [{:keys [src family stretch style weight unicode-range]}]
  (css ["@font-face" {:src src, :unicode-range unicode-range
                      :font-family family, :font-stretch stretch,
                      :font-style style, :font-weight weight}]))

(defn media "Creates a @media rule" [type rules]
  {:pre [(#{:all :braille :embossed :handheld :print :projection :screen :speech :tty :tv} type)]}
  (css [(str "@media " (name type))
        rules]))

; <Macros>
(defmacro defprop "Installs a (namespaced) CSS $property."
  [sym args & body]
  {:pre [(namespace sym)]}
  `(swap! special-rules assoc ~(keyword (str "$" sym)) (fn ~args ~@body)))

(defmacro defcssfn "Installs a (namespaced) CSS +function."
  [sym args & body]
  {:pre [(namespace sym)]}
  `(swap! special-fns assoc ~(keyword (str "+" sym)) (fn ~args ~@body)))
