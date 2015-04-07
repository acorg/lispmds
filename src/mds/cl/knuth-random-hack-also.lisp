(in-package user)

(defvar MBIG 1000000000)
(defvar MSEED 161803398)
(defvar FAC (/ 1.0 MBIG))

(defvar inext)
(defvar inextp)
(defvar ma (make-array '(56)))

;;potential problem here, this once returned 1.0, see the bottom for potential reconstruction
(defun knuth_random ()  ;;0.46 secs for 1000 calls.
  (setq inext (+ 1 inext))
  (if (= inext 56) (setq inext 1))
  (setq inextp (+ 1 inextp))
  (if (= inextp 56) (setq inextp 1))
  (let ((mj (- (aref ma inext) (aref ma inextp))))
    (if (minusp mj) (setq mj (+ mj MBIG)))
    (setf (aref ma inext) mj)
    (* mj FAC)))

(defun knuth_random-10^9 ()  ;;0.18 secs for 1000 calls  (system is 0.14)
  (setq inext (+ 1 inext))
  (if (= inext 56) (setq inext 1))
  (setq inextp (+ 1 inextp))
  (if (= inextp 56) (setq inextp 1))
  (let ((mj (- (aref ma inext) (aref ma inextp))))
    (if (minusp mj) (setq mj (+ mj MBIG)))
    (setf (aref ma inext) mj)))

(defun seed_random (seed)
  (if (minusp seed)
      (setq seed (mod (get-internal-real-time) 1000000000)))
  ;;(- (get-universal-time) 2500000000)))
  (if (or (minusp seed) (> seed MBIG))
    (error "Seed ~a is out of the range [0, ~a]." seed MBIG))
  (let ((mj seed)
        (mk 1))
    (setf (aref ma 55) seed)
    (loop for i from 1 to 54 do
          (let ((ii (mod (* i 21) 55)))
            (setf (aref ma ii) mk)
            (setf mk (- mj mk))
            (if (minusp mk) (setq mk (+ mk MBIG)))
            (setq mj (aref ma ii))))
    (loop for k below 4 do
          (loop for i from 1 to 55 do
                (setf (aref ma i) (- (aref ma i) (aref ma (+ 1 (mod (+ i 30) 55)))))
                (if (minusp (aref ma i)) (setf (aref ma i) (+ (aref ma i) MBIG)))))
    (setq inext 0)
    (setq inextp 31)
    seed))

  
#|
? (setq seed (seed-random -1))
465477391
0.43239633600000005
0.086771358
0.5925778810000001
0.8075161580000001
0.78701938
0.797703255
0.5921364370000001   ;;close to previous to 1 in 1000  ugh!
|#

#|
[1] USER(83): inext
18
[1] USER(84): inextp
49
[1] USER(85): ma
#(NIL 270123589 106099394 10080253 579048121 831044943 890732309 461736624 80260485
  879818523 ...)
[1] USER(86): (pp ma)
NIL  27012358910609939410080253579048121831044943890732309461736624802604858798185236423389203302031998873367155632200596849526213640156761689746505109260999999993705841732394649002437166735280112502892723135721726703369023128882512291902603560683836011473780641653507995186366593880974378678107198914668093627016945026011231568949177852253039956602985737369091482924557213273204379475952837677281484952943189520603919002526127782169738895626802353592931351674361647752279839930870128539407541
NIL
[1] USER(87): (loop for i below 56 collect (aref ma i))
(NIL 270123589 106099394 10080253 579048121 831044943 890732309 461736624 80260485
 879818523 ...)
[1] USER(88): (pp *)
NIL
270123589
106099394
10080253
579048121
831044943
890732309
461736624
80260485
879818523
642338920
330203199
887336715
563220059
68495262
13640156
761689746
505109260
999999993
705841732
394649002
437166735
280112502
892723135
721726703
369023128
882512291
902603560
68383601
147378064
165350799
518636659
388097437
867810719
891466809
362701694
502601123
156894917
785225303
995660298
573736909
148292455
721327320
437947595
283767728
148495294
318952060
391900252
612778216
973889562
680235359
29313516
743616477
522798399
30870128
539407541
NIL
[1] USER(89): 

(defun check-for-close-starters (&optional (num-starters 20) (closeness 0.001) (seed -1))
  (setq seed (seed-random seed))
  ;;(loop for i below 1000 do (knuth-random))
  (filter #'null
	  (all-comparisons
	   (loop for i below num-starters collect (knuth-random))
	   (^ (x y) (if (< (abs (- x y)) closeness)
			(list x y seed))))))

(filter #'null (loop for i below 100 collect (progn (sleep 1) (check-for-close-starters))))

((0.500467414d0 0.500004903d0 791327905))
((0.074254555d0 0.074888567d0 791327907))
((0.631520384d0 0.6307326830000001d0 791327920))
((0.09121264400000001d0 0.09159231000000001d0 791327927))
((0.660066028d0 0.659438166d0 791327930))
((0.993803187d0 0.994292562d0 791327931))
((0.6422616320000001d0 0.642560443d0 791327933))
((0.61634988d0 0.6168322580000001d0 791327936))
((0.5135151250000001d0 0.5129159910000001d0 791327943))
((0.259508293d0 0.258856765d0 791327945))
((0.32107615500000003d0 0.32052186000000005d0 791327951))
((0.297212839d0 0.29810589200000004d0 791327953))
((0.7276585250000001d0 0.726774196d0 791327954))
((0.542646195d0 0.541923464d0 791327955))
((0.48561821200000005d0 0.48470947200000003d0 791327959)
 (0.485654407d0 0.48470947200000003d0 791327959)
 (0.485654407d0 0.48561821200000005d0 791327959))
((0.472936505d0 0.47314134d0 791327962))
((0.9019638050000001d0 0.9013626830000001d0 791327966))
((0.337686525d0 0.337109144d0 791327969))
((0.6771004820000001d0 0.6775890050000001d0 791327971))
((0.8053477250000001d0 0.8045833010000001d0 791327972)
 (0.9500762850000001d0 0.9499682330000001d0 791327972))
((0.48325863900000005d0 0.48273710300000006d0 791327978)
 (0.5785414090000001d0 0.578281276d0 791327978))
((0.5996479100000001d0 0.598680823d0 791327986))
((0.1845329d0 0.18354673000000002d0 791327987)
 (0.315206515d0 0.314652991d0 791327987))
((0.311242786d0 0.311798258d0 791327988))
((0.5547581540000001d0 0.555604532d0 791327989)
 (0.16757998400000002d0 0.16787343200000002d0 791327989))
((0.79880178d0 0.798064836d0 791327996))
((0.0322058d0 0.032640168000000004d0 791327997))
((0.28530914500000004d0 0.286067669d0 791328000))
((0.381495625d0 0.381862607d0 791328002))
((0.961085685d0 0.960741265d0 791328006))
((0.7958336450000001d0 0.7964565250000001d0 791328007))

;;and with 4 seed intializations, not 3, about the same
((0.294908604d0 0.295421633d0 791328377))
((0.772503316d0 0.773090273d0 791328381))
((0.25133547700000003d0 0.250758913d0 791328385)
 (0.35348536900000005d0 0.35444366000000005d0 791328385))
((0.174104379d0 0.17399841100000002d0 791328392))
((0.5638689170000001d0 0.5639962580000001d0 791328398))
((0.525270248d0 0.5246449000000001d0 791328400))
((0.7744891540000001d0 0.7735373670000001d0 791328401))
((0.6513418820000001d0 0.6522330660000001d0 791328406))
((0.877603605d0 0.877936433d0 791328407)
 (0.051342187000000004d0 0.052145553000000004d0 791328407))
((0.45134249200000004d0 0.45219440400000005d0 791328408))
((0.794503346d0 0.7949968180000001d0 791328418))
((0.005603797000000001d0 0.006151831d0 791328421))
((0.647400922d0 0.646862473d0 791328426))
((0.24474150900000002d0 0.244047962d0 791328427))
((0.7435050430000001d0 0.742526443d0 791328429)
 (0.8334512690000001d0 0.8324983760000001d0 791328429))
((0.08174416100000001d0 0.082283644d0 791328431))
((0.602067392d0 0.6021997530000001d0 791328434))
((0.723998573d0 0.7246964760000001d0 791328436)
 (0.045794675d0 0.045608246000000005d0 791328436))
((0.712503039d0 0.713013302d0 791328437)
 (0.10229220300000001d0 0.103097956d0 791328437))
((0.120285759d0 0.11946013700000001d0 791328439))
((0.451354692d0 0.45076707800000004d0 791328448))
((0.699003213d0 0.698941094d0 791328451))
((0.7416254280000001d0 0.741481523d0 791328452))
((0.9560203500000001d0 0.9569921830000001d0 791328453))
((0.251356827d0 0.25134889d0 791328455))
((0.954736503d0 0.9539292880000001d0 791328457))
((0.9653074540000001d0 0.9652712900000001d0 791328458))
((0.35444362900000004d0 0.353487837d0 791328463))
((0.140597303d0 0.14005183300000001d0 791328472))
((0.9180695700000001d0 0.918684351d0 791328473))


|#