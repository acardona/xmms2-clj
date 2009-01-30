; Albert Cardona 20090129
; Assumes xmms2 is installed and in the $PATH
; and that there is already a playlist loaded into it.
;
; Shows the list of files and enables play on double-click
; by calling "xmms2 jump <playlist-index>"


(ns xmms2.music
  (:import (javax.swing JTable JFrame JScrollPane)
           (javax.swing.table AbstractTableModel)
           (java.io BufferedReader InputStreamReader)
           (java.awt.event MouseAdapter)))

(defn- assoc-track
  "[0/1] Some title here --> {0 \"Some title here\"})"
  [tracks line]
  (let [i1 (.indexOf line (int \[))
        i2 (.indexOf line (int \/) i1)
        i3 (.indexOf line (int \]) i2)]
    (if (some #{-1} [i1 i2 i3])
      tracks
      (assoc tracks (Integer/parseInt (.substring line (inc i1) i2)) (.substring line (+ 2 i3))))))

;(defn find-tracks
;  "Query xmms2 list, returns a map"
;  []
;  (with-open [br (BufferedReader. (InputStreamReader. (.getInputStream (.exec (Runtime/getRuntime) "xmms2 list"))))]
;    (loop [row (int 0)
;           tracks {}
;           line (.readLine br)]
;      (if (or (not line)
;              (== 0 (.length line)))
;        tracks
;        (recur (int (inc row))
;               (assoc-track tracks line)
;               (.readLine br))))))

(defmacro exec
  "Execute a command on the shell, passing to the given function the lazy sequence of lines read as output, and the rest of arguments."
  [cmd pred & args]
  `(with-open [br# (BufferedReader. (InputStreamReader. (.getInputStream (.exec (Runtime/getRuntime) ~cmd))))]
    (~pred (line-seq br#) ~@args)))

;(defn find-tracks
;  "Query xmms2 list, returns a map of track number vs. title."
;  []
;  (with-open [br (BufferedReader. (InputStreamReader. (.getInputStream (.exec (Runtime/getRuntime) "xmms2 list"))))]
;    (reduce assoc-track {} (line-seq br))))

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

;(defn xmms2
;  "Execute an XMMS2 command+args."
;  [cmd]
;  (.exec (Runtime/getRuntime) (str "xmms2 " cmd)))

(defn xmms2
  "Execute an XMMS2 command+args, and print its output."
  [cmd]
  (exec (str "xmms2 " cmd)
        #(doseq [line %1] (println line))))

(defn make-gui []
  (let [table (create-table)
        panel (JScrollPane. table)
        frame (JFrame. "XMMS2")]
    (.addMouseListener table
      (proxy [MouseAdapter] []
        (mousePressed [evt]
          (if (= 2 (.getClickCount evt))
            (let [row (.rowAtPoint table (.getPoint evt))]
              (xmms2 (str "jump " row)))))))

    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))))


(make-gui)
