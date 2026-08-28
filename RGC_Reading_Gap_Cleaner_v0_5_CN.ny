;nyquist plug-in
;version 4
;type process
;name "RGC - 朗读间隙自动清理 v0.5"
;action "RGC 正在处理..."
;author "OpenAI prototype"
;release 0.5
;copyright "CC0"

;control MODE "功能" choice "1 学习空白,2 学习人声,3 学习朗读节奏,4 执行清理,5 查看校准状态,6 重置校准" 0
;control STRENGTH "清理强度" choice "保守,标准,强力" 1
;control TRANSIENT "空白中的短促孤立声音" choice "作为杂音处理,保留" 0
;control TARGET "清理后的空白电平" choice "数字静音,降低 60 dB,降低 40 dB" 0
;control text "提示：前三次用于自动校准。完成后全选需要处理的录音，选择“4 执行清理”即可。下方三个处理选项仅在执行清理时生效。"

; ============================================================
; RGC v0.5
; Calibrated Reading Gap Cleaner
;
; Calibration order:
;   1) Silence / room tone
;   2) Voice (noise-informed filtering)
;   3) Natural reading rhythm
;   4) Clean selected audio
;
; Analysis frame: 10 ms (100 Hz)
; ============================================================

(defun rgc-clamp (x lo hi)
  (max lo (min hi x)))

(defun rgc-r1 (x)
  (/ (float (round (* 10.0 x))) 10.0))

(defun rgc-log10 (x)
  (/ (log x) (log 10.0)))

(defun rgc-mono (sig)
  (if (arrayp sig)
      (mult 0.5 (sum (aref sig 0) (aref sig 1)))
      sig))

(defun rgc-db-list (sig)
  ; Convert selected audio to 10 ms RMS dBFS frames.
  (let* ((mono (rgc-mono sig))
         (sr (snd-srate mono))
         (winsamps (max 1 (round (* sr 0.010))))
         (env (rms mono 100.0 winsamps))
         (vals nil))
    (do ((x (snd-fetch env) (snd-fetch env)))
        ((not x) (reverse vals))
      (let ((a (max 0.000001 x)))
        (push (* 20.0 (rgc-log10 a)) vals)))))

(defun rgc-percentile (vals p)
  (if (null vals)
      nil
      (let* ((s (sort (append vals nil) '<))
             (n (length s))
             (i (truncate (* p (- n 1)))))
        (nth (max 0 (min (- n 1) i)) s))))

(defun rgc-store (key val)
  (putprop '*SCRATCH* val key)
  val)

(defun rgc-load (key)
  (get '*SCRATCH* key))

(defun rgc-have (key)
  (not (null (get '*SCRATCH* key))))

(defun rgc-msf (ms)
  (max 0 (round (/ ms 10.0))))

(defun rgc-list-array (lst)
  (let* ((a (make-array (length lst)))
         (i 0))
    (dolist (x lst a)
      (setf (aref a i) x)
      (setf i (1+ i)))))

(defun rgc-count-ones (m)
  (let ((n 0))
    (dotimes (i (length m) n)
      (when (= (aref m i) 1)
        (setf n (1+ n))))))

(defun rgc-threshold-mask-simple (dbs threshold)
  (let* ((a (make-array (length dbs)))
         (i 0))
    (dolist (v dbs a)
      (setf (aref a i) (if (>= v threshold) 1 0))
      (setf i (1+ i)))))

(defun rgc-fill-holes (m maxf)
  ; Fill short zero-runs only when surrounded by ones.
  (let ((n (length m))
        (i 0))
    (do ()
        ((>= i n) m)
      (if (= (aref m i) 0)
          (let ((s i))
            (do ()
                ((or (>= i n) (= (aref m i) 1)))
              (setf i (1+ i)))
            (when (and (> s 0)
                       (< i n)
                       (<= (- i s) maxf))
              (do ((j s (1+ j)))
                  ((>= j i))
                (setf (aref m j) 1))))
          (setf i (1+ i))))))

(defun rgc-remove-short-ones (m maxf)
  ; Remove very short isolated candidate-speech islands.
  (let ((n (length m))
        (i 0))
    (do ()
        ((>= i n) m)
      (if (= (aref m i) 1)
          (let ((s i))
            (do ()
                ((or (>= i n) (= (aref m i) 0)))
              (setf i (1+ i)))
            (when (<= (- i s) maxf)
              (do ((j s (1+ j)))
                  ((>= j i))
                (setf (aref m j) 0))))
          (setf i (1+ i))))))

(defun rgc-values-under-mask (dbs m)
  (let ((vals nil)
        (i 0))
    (dolist (v dbs (reverse vals))
      (when (= (aref m i) 1)
        (push v vals))
      (setf i (1+ i)))))

(defun rgc-thresholds ()
  ; Returns: entry-dB, exit-dB, voice/noise separation.
  ; Voice reference = P25 of filtered candidate voice.
  ; Noise reference = P95 of learned room tone.
  (let ((v (rgc-load 'RGC3-VOICE-P25))
        (n (rgc-load 'RGC3-NOISE-P95)))
    (if (or (null v) (null n))
        nil
        (let* ((sep (- v n))
               (enter (+ n (rgc-clamp (* 0.55 sep) 3.5 10.0)))
               (exit  (+ n (rgc-clamp (* 0.30 sep) 2.0 6.0))))
          ; Keep entry below the low body of valid speech.
          (setf enter (min enter (- v 1.0)))
          ; At least 2 dB hysteresis.
          (setf exit (min exit (- enter 2.0)))
          (list enter exit sep)))))

(defun rgc-confidence (sep)
  (cond
    ((>= sep 14.0) "很好")
    ((>= sep 10.0) "良好")
    ((>= sep 7.0)  "可用")
    ((>= sep 5.0)  "偏低")
    (t             "很低")))

(defun rgc-hysteresis-mask (dbs enter exit)
  ; 1 = preserve / speech, 0 = candidate quiet.
  (let* ((a (make-array (length dbs)))
         (state 0)
         (i 0))
    (dolist (v dbs a)
      (cond
        ((and (= state 0) (>= v enter))
         (setf state 1))
        ((and (= state 1) (< v exit))
         (setf state 0)))
      (setf (aref a i) state)
      (setf i (1+ i)))))

(defun rgc-qbefore (m pos count)
  (if (< (- pos count) 0)
      nil
      (let ((ok t))
        (do ((j (- pos count) (1+ j)))
            ((>= j pos) ok)
          (when (= (aref m j) 1)
            (setf ok nil))))))

(defun rgc-qafter (m pos count)
  (let ((n (length m)))
    (if (> (+ pos count) n)
        nil
        (let ((ok t))
          (do ((j pos (1+ j)))
              ((>= j (+ pos count)) ok)
            (when (= (aref m j) 1)
              (setf ok nil)))))))

(defun rgc-remove-islands (m maxf contextf)
  ; Remove short speech islands only if both sides are long quiet context.
  (let ((n (length m))
        (i 0))
    (do ()
        ((>= i n) m)
      (if (= (aref m i) 1)
          (let ((s i))
            (do ()
                ((or (>= i n) (= (aref m i) 0)))
              (setf i (1+ i)))
            (when (and (> s 0)
                       (< i n)
                       (<= (- i s) maxf)
                       (rgc-qbefore m s contextf)
                       (rgc-qafter m i contextf))
              (do ((j s (1+ j)))
                  ((>= j i))
                (setf (aref m j) 0))))
          (setf i (1+ i))))))

(defun rgc-protect (m pref postf)
  ; Expand speech regions into neighboring frames.
  (let* ((n (length m))
         (out (make-array n))
         (i 0))
    (dotimes (k n)
      (setf (aref out k) 0))
    (do ()
        ((>= i n) out)
      (if (= (aref m i) 1)
          (let ((s i))
            (do ()
                ((or (>= i n) (= (aref m i) 0)))
              (setf i (1+ i)))
            (let ((a (max 0 (- s pref)))
                  (b (min n (+ i postf))))
              (do ((j a (1+ j)))
                  ((>= j b))
                (setf (aref out j) 1))))
          (setf i (1+ i))))))

(defun rgc-preserve-short-gaps (m minf)
  (let ((n (length m))
        (i 0))
    (do ()
        ((>= i n) m)
      (if (= (aref m i) 0)
          (let ((s i))
            (do ()
                ((or (>= i n) (= (aref m i) 1)))
              (setf i (1+ i)))
            (when (< (- i s) minf)
              (do ((j s (1+ j)))
                  ((>= j i))
                (setf (aref m j) 1))))
          (setf i (1+ i))))))

(defun rgc-gap-runs-ms (m)
  ; Internal quiet runs from 30 ms to 2000 ms.
  (let ((n (length m))
        (i 0)
        (runs nil))
    (do ()
        ((>= i n) (reverse runs))
      (if (= (aref m i) 0)
          (let ((s i))
            (do ()
                ((or (>= i n) (= (aref m i) 1)))
              (setf i (1+ i)))
            (when (and (> s 0) (< i n))
              (let ((ms (* 10.0 (- i s))))
                (when (and (>= ms 30.0)
                           (<= ms 2000.0))
                  (push ms runs)))))
          (setf i (1+ i))))))

(defun rgc-kmeans-gap-split (runs)
  ; Two-cluster k-means on LOG pause duration.
  ; This is more stable than clustering raw milliseconds because pause lengths
  ; are strongly skewed.
  ;
  ; Returns:
  ;   (split-ms short-center-ms long-center-ms short-count long-count ratio)
  ; or NIL when two meaningful groups are not supported.
  (if (< (length runs) 6)
      nil
      (let* ((logs (mapcar #'(lambda (x) (log (max 1.0 x))) runs))
             (c1 (rgc-percentile logs 0.25))
             (c2 (rgc-percentile logs 0.75)))
        (when (> c1 c2)
          (let ((tmp c1))
            (setf c1 c2)
            (setf c2 tmp)))
        (dotimes (iter 12)
          (let ((s1 0.0) (s2 0.0) (n1 0) (n2 0))
            (dolist (x logs)
              (if (<= (abs (- x c1)) (abs (- x c2)))
                  (progn
                    (setf s1 (+ s1 x))
                    (setf n1 (1+ n1)))
                  (progn
                    (setf s2 (+ s2 x))
                    (setf n2 (1+ n2)))))
            (when (> n1 0) (setf c1 (/ s1 n1)))
            (when (> n2 0) (setf c2 (/ s2 n2)))))
        (when (> c1 c2)
          (let ((tmp c1))
            (setf c1 c2)
            (setf c2 tmp)))
        (let ((n1 0) (n2 0))
          (dolist (x logs)
            (if (<= (abs (- x c1)) (abs (- x c2)))
                (setf n1 (1+ n1))
                (setf n2 (1+ n2))))
          (let* ((m1 (exp c1))
                 (m2 (exp c2))
                 (ratio (/ m2 (max 1.0 m1)))
                 (split (exp (/ (+ c1 c2) 2.0))))
            (if (and (>= n1 2)
                     (>= n2 2)
                     (>= ratio 1.60)
                     (>= split 100.0)
                     (<= split 700.0))
                (list split m1 m2 n1 n2 ratio)
                nil))))))

(defun rgc-short-cluster (runs split)
  (let ((r nil))
    (dolist (x runs (reverse r))
      (when (< x split)
        (push x r)))))

(defun rgc-transition-times (dbs enter exit)
  ; Estimate the time spent around speech onset / speech tail.
  ; Returns (rise-times-ms fall-times-ms).
  (let* ((a (rgc-list-array dbs))
         (n (length a))
         (state 0)
         (rises nil)
         (falls nil))
    (do ((i 0 (1+ i)))
        ((>= i n) (list (reverse rises) (reverse falls)))
      (let ((v (aref a i)))
        (cond
          ((and (= state 0) (>= v enter))
           (let ((j i)
                 (lim (max 0 (- i 30))))
             (do ()
                 ((or (<= j lim)
                      (< (aref a j) exit)))
               (setf j (- j 1)))
             (push (* 10.0 (- i j)) rises))
           (setf state 1))
          ((and (= state 1) (< v exit))
           (let ((j i)
                 (lim (max 0 (- i 50))))
             (do ()
                 ((or (<= j lim)
                      (>= (aref a j) enter)))
               (setf j (- j 1)))
             (push (* 10.0 (- i j)) falls))
           (setf state 0)))))))

(defun rgc-ready ()
  (and (rgc-have 'RGC3-NOISE-P95)
       (rgc-have 'RGC3-VOICE-P25)
       (rgc-have 'RGC3-FILL-MS)
       (rgc-have 'RGC3-MINGAP-MS)
       (rgc-have 'RGC3-PRE-MS)
       (rgc-have 'RGC3-POST-MS)
       (rgc-have 'RGC3-ISLAND-MS)
       (rgc-have 'RGC3-CONTEXT-MS)))

(defun rgc-strength-params (strength)
  ; Returns fill, gap, pre, post, island, context, fade (ms).
  (let ((fill (rgc-load 'RGC3-FILL-MS))
        (gap (rgc-load 'RGC3-MINGAP-MS))
        (pre (rgc-load 'RGC3-PRE-MS))
        (post (rgc-load 'RGC3-POST-MS))
        (isl (rgc-load 'RGC3-ISLAND-MS))
        (ctx (rgc-load 'RGC3-CONTEXT-MS)))
    (cond
      ((= strength 0)
       ; Conservative = clean fewer regions, protect speech more.
       (setf gap (* 1.25 gap))
       (setf pre (* 1.25 pre))
       (setf post (* 1.25 post))
       (setf isl (* 0.75 isl))
       (setf ctx (* 1.10 ctx)))
      ((= strength 2)
       ; Strong = clean shorter gaps and absorb more tiny islands.
       (setf gap (* 0.80 gap))
       (setf pre (* 0.80 pre))
       (setf post (* 0.80 post))
       (setf isl (* 1.25 isl))
       (setf ctx (* 0.85 ctx))))
    (list fill gap pre post isl ctx
          (rgc-clamp (* 0.30 pre) 15.0 40.0))))

(defun rgc-final-mask (sig strength transient-on)
  (let* ((dbs (rgc-db-list sig))
         (th (rgc-thresholds))
         (enter (first th))
         (exit (second th))
         (p (rgc-strength-params strength))
         (m (rgc-hysteresis-mask dbs enter exit)))
    (rgc-fill-holes m (rgc-msf (first p)))
    (when transient-on
      (rgc-remove-islands m
                          (rgc-msf (nth 4 p))
                          (rgc-msf (nth 5 p))))
    (setf m (rgc-protect m
                         (rgc-msf (third p))
                         (rgc-msf (fourth p))))
    (rgc-preserve-short-gaps m
                             (max 1 (rgc-msf (second p))))
    m))

(defun rgc-gain-array (m floor fadef)
  ; Build a 100 Hz gain envelope. Fades stay inside detected gaps.
  (let* ((n (length m))
         (g (make-array (+ n 4))))
    (dotimes (i (+ n 4))
      (setf (aref g i) 1.0))
    (dotimes (i n)
      (setf (aref g i)
            (if (= (aref m i) 1) 1.0 floor)))

    ; Speech -> gap fade-down.
    (when (> fadef 0)
      (do ((i 1 (1+ i)))
          ((>= i n))
        (when (and (= (aref m (- i 1)) 1)
                   (= (aref m i) 0))
          (let ((stop (min n (+ i fadef))))
            (do ((j i (1+ j)))
                ((>= j stop))
              (let* ((x (/ (float (- j i)) (max 1 fadef)))
                     (v (+ floor
                           (* (- 1.0 floor) (- 1.0 x)))))
                (setf (aref g j)
                      (max (aref g j) v))))))))

    ; Gap -> speech fade-up.
    (when (> fadef 0)
      (do ((i 1 (1+ i)))
          ((>= i n))
        (when (and (= (aref m (- i 1)) 0)
                   (= (aref m i) 1))
          (let ((start (max 0 (- i fadef))))
            (do ((j start (1+ j)))
                ((>= j i))
              (let* ((x (/ (float (- j start)) (max 1 fadef)))
                     (v (+ floor
                           (* (- 1.0 floor) x))))
                (setf (aref g j)
                      (max (aref g j) v))))))))

    (let ((last (if (> n 0)
                    (aref g (- n 1))
                    1.0)))
      (setf (aref g n) last)
      (setf (aref g (+ n 1)) last)
      (setf (aref g (+ n 2)) last)
      (setf (aref g (+ n 3)) last))
    g))

(defun rgc-reset ()
  (dolist (k '(RGC3-NOISE-P50
               RGC3-NOISE-P90
               RGC3-NOISE-P95
               RGC3-NOISE-P99
               RGC3-NOISE-DUR
               RGC3-VOICE-P10
               RGC3-VOICE-P25
               RGC3-VOICE-P50
               RGC3-VOICE-P90
               RGC3-VOICE-DUR
               RGC3-VOICE-RATIO
               RGC3-ENTER-DB
               RGC3-EXIT-DB
               RGC3-SEPARATION-DB
               RGC3-FILL-MS
               RGC3-MINGAP-MS
               RGC3-PRE-MS
               RGC3-POST-MS
               RGC3-ISLAND-MS
               RGC3-CONTEXT-MS
               RGC3-GAP-SPLIT-MS
               RGC3-GAP-SHORT-CENTER
               RGC3-GAP-LONG-CENTER
               RGC3-GAP-RATIO
               RGC3-RHYTHM-DUR
               RGC3-RHYTHM-METHOD))
    (remprop '*SCRATCH* k))
  "RGC v0.5：校准数据已全部清空。")

; ---------------- Mode 1: Learn silence ----------------

(defun rgc-learn-silence ()
  (let* ((dbs (rgc-db-list *track*))
         (dur (* 0.010 (length dbs))))
    (if (< dur 1.5)
        "【学习空白】选区略短。最低建议 1.5 秒，最好选 2～5 秒完全不说话的房间底噪。"
        (let ((p50 (rgc-percentile dbs 0.50))
              (p90 (rgc-percentile dbs 0.90))
              (p95 (rgc-percentile dbs 0.95))
              (p99 (rgc-percentile dbs 0.99)))
          (rgc-store 'RGC3-NOISE-P50 p50)
          (rgc-store 'RGC3-NOISE-P90 p90)
          (rgc-store 'RGC3-NOISE-P95 p95)
          (rgc-store 'RGC3-NOISE-P99 p99)
          (rgc-store 'RGC3-NOISE-DUR dur)
          ; New silence invalidates later calibration.
          (dolist (k '(RGC3-VOICE-P10 RGC3-VOICE-P25 RGC3-VOICE-P50 RGC3-VOICE-P90
                       RGC3-VOICE-DUR RGC3-VOICE-RATIO
                       RGC3-ENTER-DB RGC3-EXIT-DB RGC3-SEPARATION-DB
                       RGC3-FILL-MS RGC3-MINGAP-MS RGC3-PRE-MS RGC3-POST-MS
                       RGC3-ISLAND-MS RGC3-CONTEXT-MS RGC3-GAP-SPLIT-MS
                       RGC3-GAP-SHORT-CENTER RGC3-GAP-LONG-CENTER
                       RGC3-GAP-RATIO RGC3-RHYTHM-DUR RGC3-RHYTHM-METHOD))
            (remprop '*SCRATCH* k))
          (format nil
                  "【空白学习完成】~%样本长度：~a 秒~%典型底噪（P50）：~a dBFS~%底噪上沿（P95）：~a dBFS~%极少数较大波动（P99）：~a dBFS~%~%下一步：选 2～5 秒自然朗读的一整句话，运行“2 学习人声”。不需要连续发声，词与词之间的小空隙会先被过滤。"
                  (rgc-r1 dur)
                  (rgc-r1 p50)
                  (rgc-r1 p95)
                  (rgc-r1 p99))))))

; ---------------- Mode 2: Learn voice ----------------

(defun rgc-learn-voice ()
  (if (not (rgc-have 'RGC3-NOISE-P95))
      "【学习人声】请先运行“1 学习空白”。v0.3 会先用底噪数据排除词间/音素间的低能量部分。"
      (let* ((dbs (rgc-db-list *track*))
             (dur (* 0.010 (length dbs)))
             (noise95 (rgc-load 'RGC3-NOISE-P95))
             ; Only frames clearly above room tone enter the first candidate mask.
             ; 4 dB is intentionally permissive; continuity rules clean it up.
             (prefloor (+ noise95 4.0))
             (m (rgc-threshold-mask-simple dbs prefloor)))
        (if (< dur 1.0)
            "【学习人声】选区太短。最低 1 秒；推荐 2～5 秒自然朗读的一整句话。"
            (progn
              ; Bridge tiny <=20 ms holes, then discard <=30 ms isolated spikes.
              ; This keeps ordinary speech structure while rejecting tiny clicks.
              (rgc-fill-holes m 2)
              (rgc-remove-short-ones m 3)
              (let* ((valid (rgc-values-under-mask dbs m))
                     (validn (length valid))
                     (totaln (max 1 (length dbs)))
                     (ratio (/ (float validn) totaln)))
                (cond
                  ((< validn 30)
                   "【学习人声失败】有效语音帧太少。请选一整句正常朗读；如果已经如此，可能是录音电平太低或“学习空白”的样本里混入了声音。")
                  ((< ratio 0.18)
                   (format nil
                           "【学习人声失败】有效语音只占约 ~a%%，比例过低。请重新选择更连续的一整句话。词间小停顿没有关系，但不要选含有很长停顿的片段。"
                           (round (* 100.0 ratio))))
                  (t
                   (let* ((p10 (rgc-percentile valid 0.10))
                          (p25 (rgc-percentile valid 0.25))
                          (p50 (rgc-percentile valid 0.50))
                          (p90 (rgc-percentile valid 0.90))
                          (sep (- p25 noise95)))
                     (if (< sep 3.0)
                         (format nil
                                 "【学习人声失败】有效人声与底噪只相差约 ~a dB，太难安全区分。请检查麦克风增益、距离，或重新学习更纯净的空白。"
                                 (rgc-r1 sep))
                         (progn
                           (rgc-store 'RGC3-VOICE-P10 p10)
                           (rgc-store 'RGC3-VOICE-P25 p25)
                           (rgc-store 'RGC3-VOICE-P50 p50)
                           (rgc-store 'RGC3-VOICE-P90 p90)
                           (rgc-store 'RGC3-VOICE-DUR dur)
                           (rgc-store 'RGC3-VOICE-RATIO ratio)
                           (let* ((th (rgc-thresholds))
                                  (enter (first th))
                                  (exit (second th))
                                  (sep2 (third th)))
                             (rgc-store 'RGC3-ENTER-DB enter)
                             (rgc-store 'RGC3-EXIT-DB exit)
                             (rgc-store 'RGC3-SEPARATION-DB sep2)
                             ; New voice invalidates rhythm only.
                             (dolist (k '(RGC3-FILL-MS RGC3-MINGAP-MS
                                          RGC3-PRE-MS RGC3-POST-MS
                                          RGC3-ISLAND-MS RGC3-CONTEXT-MS
                                          RGC3-GAP-SPLIT-MS
                                          RGC3-GAP-SHORT-CENTER
                                          RGC3-GAP-LONG-CENTER
                                          RGC3-GAP-RATIO
                                          RGC3-RHYTHM-DUR
                                          RGC3-RHYTHM-METHOD))
                               (remprop '*SCRATCH* k))
                             (format nil
                                     "【人声学习完成】~%样本长度：~a 秒~%有效语音占比：~a%%~%偏轻人声参考（P25）：~a dBFS~%典型人声（P50）：~a dBFS~%人声 / 底噪分离度：~a dB（~a）~%自动“进入人声”阈值：~a dBFS~%自动“离开人声”阈值：~a dBFS~%~%说明：本次统计只使用通过底噪筛选且具有连续性的候选语音帧，词间很短的低谷没有直接混入人声分位数。~%~%下一步：选 20～60 秒自然朗读，包含你平时正常的词间、逗号和句间停顿，运行“3 学习朗读节奏”。"
                                     (rgc-r1 dur)
                                     (round (* 100.0 ratio))
                                     (rgc-r1 p25)
                                     (rgc-r1 p50)
                                     (rgc-r1 sep2)
                                     (rgc-confidence sep2)
                                     (rgc-r1 enter)
                                     (rgc-r1 exit))))))))))))))

; ---------------- Mode 3: Learn rhythm ----------------

(defun rgc-learn-rhythm ()
  (let ((th (rgc-thresholds)))
    (if (null th)
        "【学习朗读节奏】请先依次完成“1 学习空白”和“2 学习人声”。"
        (let* ((dbs (rgc-db-list *track*))
               (dur (* 0.010 (length dbs)))
               (enter (first th))
               (exit (second th)))
          (if (< dur 10.0)
              "【学习朗读节奏】样本太短。最低 10 秒；推荐 20～60 秒自然朗读，越接近平时录制节奏越好。"
              (let* ((m (rgc-hysteresis-mask dbs enter exit)))
                ; Remove only ultra-tiny <=30 ms holes before measuring pauses.
                ; Longer gaps remain visible to the rhythm learner.
                (rgc-fill-holes m 3)
                (let* ((runs (rgc-gap-runs-ms m))
                       (km (rgc-kmeans-gap-split runs))
                       (split (if km (first km) 300.0))
                       (short-center (if km (second km) nil))
                       (long-center (if km (third km) nil))
                       (gapratio (if km (nth 5 km) nil))
                       (method (if km "对数时长双聚类" "保守后备值"))
                       (shorts (rgc-short-cluster runs split))
                       (trans (rgc-transition-times dbs enter exit))
                       (rises (first trans))
                       (falls (second trans))
                       (rise75 (if rises (rgc-percentile rises 0.75) 30.0))
                       (fall75 (if falls (rgc-percentile falls 0.75) 60.0))
                       (fill (if shorts
                                 (rgc-clamp
                                   (+ 20.0 (rgc-percentile shorts 0.90))
                                   70.0 180.0)
                                 120.0))
                       ; Slightly above learned split to avoid borderline over-cleaning.
                       (mingap (rgc-clamp (* 1.12 split) 220.0 700.0))
                       (pre (rgc-clamp (+ 30.0 rise75) 45.0 130.0))
                       (post (rgc-clamp (+ 60.0 fall75) 100.0 240.0))
                       ; Transient deletion remains deliberately conservative.
                       (island (rgc-clamp (* 0.35 mingap) 80.0 150.0))
                       (context (rgc-clamp (* 0.70 mingap) 170.0 400.0)))
                  (rgc-store 'RGC3-FILL-MS fill)
                  (rgc-store 'RGC3-MINGAP-MS mingap)
                  (rgc-store 'RGC3-PRE-MS pre)
                  (rgc-store 'RGC3-POST-MS post)
                  (rgc-store 'RGC3-ISLAND-MS island)
                  (rgc-store 'RGC3-CONTEXT-MS context)
                  (rgc-store 'RGC3-GAP-SPLIT-MS split)
                  (rgc-store 'RGC3-GAP-SHORT-CENTER short-center)
                  (rgc-store 'RGC3-GAP-LONG-CENTER long-center)
                  (rgc-store 'RGC3-GAP-RATIO gapratio)
                  (rgc-store 'RGC3-RHYTHM-DUR dur)
                  (rgc-store 'RGC3-RHYTHM-METHOD method)
                  (format nil
                          "【朗读节奏学习完成】~%样本长度：~a 秒~%检测到的内部停顿数：~a~%分界方法：~a~%微停顿 / 真正空白分界：~a ms~%自动填补的短低谷上限：~a ms~%最短可清理空白：~a ms~%人声前保护：~a ms~%人声后保护：~a ms~%孤立短声音最大长度：~a ms~%孤立声音所需安静上下文：~a ms~%~%下一步：全选需要处理的录音，运行“4 执行清理”。第一次使用时建议先处理一份测试副本。"
                          (rgc-r1 dur)
                          (length runs)
                          method
                          (rgc-r1 split)
                          (rgc-r1 fill)
                          (rgc-r1 mingap)
                          (rgc-r1 pre)
                          (rgc-r1 post)
                          (rgc-r1 island)
                          (rgc-r1 context)))))))))

; ---------------- Status ----------------

(defun rgc-status ()
  (format nil
          "【RGC v0.5 校准状态】~%~%空白 P50：~a~%空白 P95：~a~%人声 P25：~a~%有效人声占比：~a~%进入人声阈值：~a~%离开人声阈值：~a~%人声 / 底噪分离度：~a~%~%节奏分界方法：~a~%微停顿 / 空白分界：~a ms~%填补短低谷：~a ms~%最短清理空白：~a ms~%人声前保护：~a ms~%人声后保护：~a ms~%孤立短声音上限：~a ms~%安静上下文：~a ms"
          (if (rgc-have 'RGC3-NOISE-P50)
              (format nil "~a dBFS" (rgc-r1 (rgc-load 'RGC3-NOISE-P50)))
              "未学习")
          (if (rgc-have 'RGC3-NOISE-P95)
              (format nil "~a dBFS" (rgc-r1 (rgc-load 'RGC3-NOISE-P95)))
              "未学习")
          (if (rgc-have 'RGC3-VOICE-P25)
              (format nil "~a dBFS" (rgc-r1 (rgc-load 'RGC3-VOICE-P25)))
              "未学习")
          (if (rgc-have 'RGC3-VOICE-RATIO)
              (format nil "~a%%" (round (* 100.0 (rgc-load 'RGC3-VOICE-RATIO))))
              "未学习")
          (if (rgc-have 'RGC3-ENTER-DB)
              (format nil "~a dBFS" (rgc-r1 (rgc-load 'RGC3-ENTER-DB)))
              "未学习")
          (if (rgc-have 'RGC3-EXIT-DB)
              (format nil "~a dBFS" (rgc-r1 (rgc-load 'RGC3-EXIT-DB)))
              "未学习")
          (if (rgc-have 'RGC3-SEPARATION-DB)
              (format nil "~a dB" (rgc-r1 (rgc-load 'RGC3-SEPARATION-DB)))
              "未学习")
          (if (rgc-have 'RGC3-RHYTHM-METHOD)
              (rgc-load 'RGC3-RHYTHM-METHOD)
              "未学习")
          (if (rgc-have 'RGC3-GAP-SPLIT-MS)
              (rgc-r1 (rgc-load 'RGC3-GAP-SPLIT-MS))
              "未学习")
          (if (rgc-have 'RGC3-FILL-MS)
              (rgc-r1 (rgc-load 'RGC3-FILL-MS))
              "未学习")
          (if (rgc-have 'RGC3-MINGAP-MS)
              (rgc-r1 (rgc-load 'RGC3-MINGAP-MS))
              "未学习")
          (if (rgc-have 'RGC3-PRE-MS)
              (rgc-r1 (rgc-load 'RGC3-PRE-MS))
              "未学习")
          (if (rgc-have 'RGC3-POST-MS)
              (rgc-r1 (rgc-load 'RGC3-POST-MS))
              "未学习")
          (if (rgc-have 'RGC3-ISLAND-MS)
              (rgc-r1 (rgc-load 'RGC3-ISLAND-MS))
              "未学习")
          (if (rgc-have 'RGC3-CONTEXT-MS)
              (rgc-r1 (rgc-load 'RGC3-CONTEXT-MS))
              "未学习")))

; ---------------- Main dispatcher ----------------

(cond
  ; 1 学习空白
  ((= MODE 0)
   (rgc-learn-silence))

  ; 2 学习人声
  ((= MODE 1)
   (rgc-learn-voice))

  ; 3 学习朗读节奏
  ((= MODE 2)
   (rgc-learn-rhythm))

  ; 4 执行清理
  ((= MODE 3)
   (if (not (rgc-ready))
       "【清理】校准尚未完成。请依次完成：1 学习空白 → 2 学习人声 → 3 学习朗读节奏。"
       (let* ((m (rgc-final-mask *track*
                                 STRENGTH
                                 (= TRANSIENT 0)))
              (p (rgc-strength-params STRENGTH))
              (fadef (rgc-msf (nth 6 p)))
              (floor (cond
                       ((= TARGET 0) 0.0)
                       ((= TARGET 1) (db-to-linear -60.0))
                       (t            (db-to-linear -40.0))))
              (ga (rgc-gain-array m floor fadef))
              (ge (snd-from-array 0 100.0 ga)))
         (mult *track* ge))))

  ; 5 查看状态
  ((= MODE 4)
   (rgc-status))

  ; 6 重置
  (t
   (rgc-reset)))

