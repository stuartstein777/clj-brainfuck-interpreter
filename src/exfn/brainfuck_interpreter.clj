(ns exfn.brainfuck-interpreter)

;; > : increment data pointer.
;; < : decrement data pointer
;; + : increment the byte at the data pointer
;; - : decrement the byte at the data pointer
;; . : output the byte at the data pointer
;; , : accept one byte of input, storing its value in the byte at the data pointer.
;; [ : if the byte at the data pointer is zero, then instead of moving the 
;;     ip forward to the next command, jump it forward to the command after the
;;     matching ]
;; ] : if the byte at the data pointer is nonzero, then instead of moving the
;;     ip forward to the next command, jump it back to the command after the
;;     matching [

(defn increment [n]
  (mod (inc n) 256))

(defn decrement [n]
  (mod (dec n) 256))

(defn reducer [{:keys [instr open] :as state} i]
  (cond
    (= i \[) (-> state
                 (update :open conj instr)
                 (update :instr inc))
    (= i \]) (-> state
                 (update :open pop)
                 (update :instr inc)
                 (update :res assoc instr (peek open))
                 (update :res assoc (peek open) instr))
    :else    (update state :instr inc)))

(defn build-jmp-table [code]
  (->> code
       (reduce reducer {:instr 0 :open [] :res {}})
       :res))

(comment
  (build-jmp-table (seq "+[--[+[-+-[++]]]]")))

;; representation of the vm
;; {:ip      0        the instruction pointer, current instruction we point to.
;;  :input   []       this is what , reads from. Consuming the input, removes the item.
;;  :jmp-table {}     the jump table for [ and ]
;;  :output  []       this is what . writes to
;;  :dp      0        the data pointer.
;;  :memory  {}       memory map, key is the memory address
;; }

(defn get-fwd-jump-target [{:keys [dp memory ip jmp-table]}]
  (let [byte-at-dp (memory dp 0)]
    (if (zero? byte-at-dp)
      (inc (jmp-table ip))
      (inc ip))))

(defn get-bkwd-jump-target [{:keys [dp memory ip jmp-table]}]
  (let [byte-at-dp (memory dp 0)]
    (if (zero? byte-at-dp)
      (inc ip)
      (inc (jmp-table ip)))))

(defn brain-fuck [code input]
  (let [code (seq code)
        jmp-table (build-jmp-table code)
        code-len (count code)]
    (loop [vm {:ip        0
               :input     (seq input)
               :jmp-table jmp-table
               :output    []
               :dp        0
               :memory    {}}]
      (if (= (:ip vm) code-len)
        (apply str (vm :output))
        (let [cur (nth code (:ip vm))]
          (condp = cur
            \+ (recur (-> vm
                          (update :ip inc)
                          (update-in [:memory (vm :dp)] (fnil increment 0))))

            \- (recur (-> vm
                          (update :ip inc)
                          (update-in [:memory (vm :dp)] (fnil decrement 0))))

            \> (recur (-> vm
                          (update :ip inc)
                          (update :dp inc)))

            \< (recur (-> vm
                          (update :ip inc)
                          (update :dp dec)))

            \. (recur (-> vm
                          (update :ip inc)
                          (update :output conj (char (get-in vm [:memory (vm :dp)] 0)))))

            \, (recur (-> vm
                          (update :ip inc)
                          (assoc-in [:memory (vm :dp)] (int (nth (vm :input) 0 0)))
                          (update :input rest)))

            \[ (recur (let [new-ip (get-fwd-jump-target vm)]
                        (assoc-in vm [:ip] new-ip)))

            \] (recur (let [new-ip (get-bkwd-jump-target vm)]
                        (assoc-in vm [:ip] new-ip)))

            (recur (-> vm (update :ip inc)))))))))


(brain-fuck
 "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
 ""
 )

(brain-fuck
 "++++++++[>+++++++++++>+++++++++>++++<<<-]
  >-.>.-------.<---.>>.<<.>+++++++.---.>
  .<---.<--.>-.++++++++.<----.>---.<
  +++++++.>---.++++++++.--.+++++.-------.>.<-.<.>---.++++++++."
 ""
 )

(brain-fuck
 ",[.,]"
 "ABC")

(brain-fuck "+++++++++++
>+>>>>++++++++++++++++++++++++++++++++++++++++++++
>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
>[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
+++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
++++++++++++++++++++++++++++++++++++++++++++.[-]<<
<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]" "")

