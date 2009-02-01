; Albert Cardona 20090129
; Released under the General Public License in its latest version.
;
; Assumes xmms2 is installed and in the $PATH
; and that there is already a playlist loaded into it.
;
; Shows the list of tracks and enables play on double-click
; by calling "xmms2 jump <playlist-index>"
;
; From a clojure prompt, run xmms2 commands like:
; (in-ns 'xmms2.music)
; (xmms2 "info")
; (xmms2 "pause")
; (xmms2 "jump 10")
;
; Do NOT run (xmms2 "status")


(ns xmms2.gui
  (:import (javax.swing JTable JFrame JScrollPane JPanel JLabel BoxLayout)
           (javax.swing.table AbstractTableModel)
           (java.io BufferedReader InputStreamReader)
           (java.awt.event MouseAdapter WindowAdapter KeyAdapter KeyEvent)))

(defn- assoc-track
  "[0/1] Some title here --> {0 \"Some title here\"})"
  [tracks line]
  (let [i1 (.indexOf line (int \[))
        i2 (.indexOf line (int \/) i1)
        i3 (.indexOf line (int \]) i2)]
    (if (some #{-1} [i1 i2 i3])
      tracks
      (assoc tracks (Integer/parseInt (.substring line (inc i1) i2)) (.substring line (+ 2 i3))))))

(defmacro exec
  "Execute a command on the shell, passing to the given function the lazy sequence of lines read as output, and the rest of arguments.
   Errors printing to stderr are println'ed."
  [cmd pred & args]
  `(let [proc# (.exec (Runtime/getRuntime) ~cmd)
         to-br# (fn [s#] (BufferedReader. (InputStreamReader. s#)))]
    (with-open [in# (to-br# (.getInputStream proc#))
                err# (to-br# (.getErrorStream proc#))]
      (doseq [l# (line-seq err#)] (println l#))
      (~pred (line-seq in#) ~@args))))

(defn exec-pr
  "Execute a command on the shell and print its output."
  [cmd]
  (exec cmd
        #(doseq [line %1] (println line))))

(defn find-tracks
  "Query xmms2 list, returns a map of track number vs. title."
  []
  (exec "xmms2 list"
        (fn [lines] (reduce assoc-track {} lines))))

(defn create-table []
  (let [col-titles {0 "Track" 1 "Title"}
        tracks (find-tracks)
        model (proxy [AbstractTableModel] []
                (getColumnName [col]
                  (if-let [title (col-titles col)]
                    title
                    ""))
                (getRowCount []
                  (count tracks))
                (getColumnCount []
                  (count col-titles))
                (getValueAt [row col]
                  (if (== 0 col)
                    (inc row)
                    (tracks row)))
                (isCellEditable [row col]
                  false)
                (setValueAt [ob row col] nil))]
    ;(doseq [[k v] tracks]
    ;  (println k v))
    (JTable. model)))

(defn xmms2
  "Execute an XMMS2 command+args, and print its output."
  [cmd]
  (if (= (.trim cmd) "status")
    (println "Can't execute 'status'!")
    (exec-pr (str "xmms2 " cmd))))

(let [status-thread (ref nil)]
  (defn monitor-status
    "Monitors current song status in a separate thread
     printing the status to the given label.
     If called more than once, it interrupts the previous thread
     and starts a new one with the given label."
    [label]
    (dosync (alter status-thread
                   (fn [old]
                     (if old
                       (.interrupt old))
                     (let [self (ref nil)]
                       (dosync (alter self (fn [_] (Thread. (fn []
                         (with-open [br (BufferedReader.
                                          (InputStreamReader. 
                                            (.getInputStream
                                              (.exec (Runtime/getRuntime) "xmms2 status"))))]
                           ; Could use (doseq [line (take-while .... but then I'd hold onto the head
                           ; of the potentially infinite line-seq!
                           (loop [lines (line-seq br)]
                             (if (not (.isInterrupted @self))
                               (do
                                 (.setText label (first lines))
                                 (recur (rest lines)))))))
                                                            "XMMS2 Status Monitor"))))
                       (.start @self)
                       @self)))))

  (defn debug-status
    []
    (println "Status thread: " @status-thread))

  (defn quit-status
    "Stop monitoring status."
    []
    (dosync (alter status-thread
                   (fn [old]
                     (if old
                       (.interrupt old))
                     nil)))))

(defn play
  "Play a song given its index in the playlist."
  [row]
  (xmms2 (str "jump " row)))


(defn make-gui []
  (let [table (create-table)
        jsp (JScrollPane. table)
        label (JLabel. "Not playing.")
        panel (JPanel.)
        frame (JFrame. "XMMS2")]

    (.addMouseListener table
      (proxy [MouseAdapter] []
        (mousePressed [evt]
          (if (= 2 (.getClickCount evt))
            (let [row (.rowAtPoint table (.getPoint evt))]
              (play row))))))

    (doto panel
      (.setLayout (BoxLayout. panel BoxLayout/Y_AXIS))
      (.add label)
      (.add jsp))

    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true)
      (.addWindowListener (proxy (WindowAdapter) []
                            (windowClosing [evt]
                              (quit-status)))))

    (monitor-status label)

    (let [kl (proxy (KeyAdapter) []
                      (keyPressed [evt]
                        (let [key-code (.getKeyCode evt)]
                          (cond
                            (= key-code KeyEvent/VK_SPACE) (xmms2 "togglePlay")
                            (= key-code KeyEvent/VK_Q) (println "Q: queuing not implemented yet.")))
                        (.consume evt)))]
      (.addKeyListener table kl)
      (.addKeyListener frame kl))))


(make-gui)
