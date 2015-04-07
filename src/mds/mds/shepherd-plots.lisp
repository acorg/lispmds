(in-package user)

#|
;;here are my coordss on hi85, stress 62
(setq my-coordss '(
(-15.8339615 5.7491155 6.695419) 
(-19.487225 8.931915 -4.3033075) 
(-21.186424 -15.2170105 12.945486) 
(-22.328325 16.78205 -9.470088) 
(-10.390739 31.786587 7.3210344) 
(-13.995343 18.022863 -11.294607) 
(16.263794 0.6044546 -5.9192324) 
(16.08133 7.9236803 -5.790329) 
(7.2381372 10.442173 0.18705538) 
(10.531695 20.148993 -0.03421218) 
(44.911583 -14.54054 2.3198113) 
(-11.188126 -3.9235044 0.10796019) 
(-19.328365 -9.699995 -15.997232) 
(-8.547327 -20.165123 20.919458) 
(-24.06641 0.5879571 -13.404852) 
(-2.5206885 10.485255 24.452208) 
(-14.226655 4.1094875 -29.804626) 
(11.701634 -4.882919 -29.160555) 
(10.107625 -7.0543013 -2.2286437) 
(3.9058661 -0.9095979 21.428913) 
(7.177698 -1.1388956 17.67886) 
(22.912601 -8.960544 27.768377) 
))


;;and lapedes coordss on hi85
(setq lapedes-coordss '(
(0.20554075 0.45110667 -0.374702) 
(0.9248883 0.0955828 -0.27926627) 
(-0.0728781 0.8697445 -0.78988206) 
(1.3873857 0.02965487 -0.09346332) 
(0.77004373 1.4361522 0.82884425) 
(0.941528 -0.5640282 -0.14115411) 
(-0.05580657 -0.44930318 0.58800566) 
(-0.057789132 -0.48441228 0.5668258) 
(-0.24465705 -0.44277492 0.23996648) 
(-0.50371814 -1.0387692 -0.16066352) 
(-1.6981552 -1.7370219 0.061084937) 
(0.30585593 0.35478386 -0.3905201) 
(0.89841247 0.09649786 -1.2974774) 
(-0.74981356 1.1664083 -0.9577057) 
(1.2673243 0.28422436 -0.7663154) 
(-0.35420012 0.8374294 0.5119428) 
(1.429288 -0.96225065 -0.7116416) 
(0.080771744 -0.64007586 1.7187155) 
(-0.7118809 -0.16711602 0.7840815) 
(-0.8989286 0.49194068 0.17986783) 
(-0.9445943 0.16758117 -0.0557305) 
(-1.9498559 0.18703188 0.50957173) 
))


USER(113): (ppl (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss #'e-dist))))

(12.5911875 27.673855 30.44612 22.325583 22.693123 36.57219 46.442436 30.274284 25.516172 26.412445 46.494514) 
(15.924702 21.998209 40.031326 13.169232 33.42388 26.481064 42.20755 33.70042 36.141933 35.99521 56.093475) 
(19.806849 29.522377 15.742136 30.861492 33.78492 47.42915 54.418163 35.72396 30.10471 32.01752 46.94236) 
(25.388245 27.43906 49.784878 16.755653 39.78334 25.29277 44.89013 40.898666 44.22641 43.91846 64.00098) 
(36.44002 48.422714 53.733562 39.87406 28.445766 46.465588 56.24617 44.944344 38.372723 38.730118 56.458256) 
(24.890602 28.620144 50.256836 20.244888 38.29253 23.15721 38.78404 35.944527 41.829655 40.680862 60.13499) 
(28.468182 38.399864 42.03906 41.019024 37.052822 38.890423 24.312212 10.496448 30.048822 25.34692 35.644783) 
(30.311197 40.84876 45.923462 41.51668 35.597862 38.85625 27.006632 16.513954 31.099129 26.68722 38.18285) 
(23.364634 37.059643 40.197266 35.52211 26.154047 37.42113 33.407536 17.894028 24.314259 20.978275 37.187138) 
(32.42308 45.137604 49.277718 41.933723 29.3826 41.910847 38.42277 27.294956 30.79012 27.895813 42.114555) 
(57.13835 66.97549 56.880905 72.347084 58.01684 69.83648 46.767338 35.88938 47.24859 42.88765 34.098698) 

USER(115): (ppl (hi-table-values hi85))

(2560 640 320 1280 1280 320 80 320 640 640 160) 
(2560 1280 160 2560 320 640 160 320 320 320 40) 
(2560 320 2560 320 320 80 80 320 320 320 80) 
(640 640 80 2560 160 640 160 160 160 160 40) 
(320 80 80 160 640 80 80 160 160 160 40) 
(1280 320 80 1280 160 1280 160 320 160 160 40) 
(640 160 160 160 320 160 1280 2560 320 640 320) 
(320 160 160 160 320 160 640 2560 320 640 160) 
(640 320 160 320 640 320 320 1280 1280 1280 320) 
(320 160 80 160 320 160 160 640 320 640 160) 
(80 40 40 40 40 40 80 320 80 160 320) 

(g-plot (transpose (flatten (hi-table-values hi85)) 
		   (flatten (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss #'e-dist))))) :style 'scatter)
(g-plot (transpose (flatten (hi-table-values hi85)) 
		   (flatten (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full lapedes-coordss #'e-dist))))) :style 'scatter)


USER(125): (pp-hi-table (make-hi-table (hi-table-antigens hi85) 
				       (hi-table-sera hi85) 
				       (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss #'e-dist)))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80               13   28   30   22   23   37   46   30   26   26   46
A/BANGKOK/1/79              16   22   40   13   33   26   42   34   36   36   56
A/BANGKOK/2/79              20   30   16   31   34   47   54   36   30   32   47
A/SHANGHAI/31/80            25   27   50   17   40   25   45   41   44   44   64
A/PHILIPPINES/2/82_EQ       36   48   54   40   28   46   56   45   38   39   56
A/TAIWAN/16/83              25   29   50   20   38   23   39   36   42   41   60
A/PANAMA/1/83               28   38   42   41   37   39   24   10   30   25   36
A/CAEN/1/84                 30   41   46   42   36   39   27   17   31   27   38
A/MISSISSIPPI/1/85          23   37   40   36   26   37   33   18   24   21   37
A/WASHINGTON/1/85           32   45   49   42   29   42   38   27   31   28   42
A/NEWJERSEY/1/85            57   67   57   72   58   70   47   36   47   43   34


USER(126): (pp-hi-table (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full lapedes-coordss #'e-dist)))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80              0.1  1.2  1.3  1.1  1.1  1.9  2.4  1.6  1.2  1.2  2.3
A/BANGKOK/1/79             0.7  1.0  2.1  0.6  1.7  1.2  2.3  2.0  1.9  1.9  3.0
A/BANGKOK/2/79             0.8  1.3  0.8  1.5  1.3  2.4  2.9  2.0  1.3  1.3  2.4
A/SHANGHAI/31/80           1.2  1.3  2.6  0.7  2.0  1.2  2.3  2.3  2.3  2.3  3.4
A/PHILIPPINES/2/82_EQ      1.7  2.5  2.4  2.0  1.3  2.9  2.4  2.2  2.0  2.3  3.0
A/TAIWAN/16/83             1.1  1.3  2.6  1.1  2.0  0.8  2.1  1.9  2.1  2.0  3.1
A/PANAMA/1/83              1.3  2.2  2.3  2.0  1.3  2.0  1.2  0.7  1.3  1.3  2.0
A/CAEN/1/84                1.3  2.2  2.4  2.0  1.4  2.0  1.2  0.8  1.3  1.3  2.0
A/MISSISSIPPI/1/85         1.2  2.0  2.1  2.0  1.3  2.0  1.5  0.8  1.1  1.0  1.8
A/WASHINGTON/1/85          1.6  2.1  2.4  2.3  2.0  2.0  2.0  1.3  1.6  1.3  2.0
A/NEWJERSEY/1/85           2.9  3.5  3.2  3.7  2.9  3.3  2.7  2.0  2.4  2.1  2.0




USER(138): (pp-hi-table (put-hi-table-into-range-0-1 (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full lapedes-coordss #'e-dist))))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80             -0.0  0.3  0.3  0.3  0.3  0.5  0.6  0.4  0.3  0.3  0.6
A/BANGKOK/1/79             0.2  0.2  0.6  0.1  0.4  0.3  0.6  0.5  0.5  0.5  0.8
A/BANGKOK/2/79             0.2  0.3  0.2  0.4  0.3  0.6  0.8  0.5  0.3  0.3  0.6
A/SHANGHAI/31/80           0.3  0.3  0.7  0.2  0.5  0.3  0.6  0.6  0.6  0.6  0.9
A/PHILIPPINES/2/82_EQ      0.4  0.7  0.6  0.5  0.3  0.8  0.6  0.6  0.5  0.6  0.8
A/TAIWAN/16/83             0.3  0.3  0.7  0.3  0.5  0.2  0.5  0.5  0.6  0.5  0.8
A/PANAMA/1/83              0.3  0.6  0.6  0.5  0.3  0.5  0.3  0.2  0.3  0.3  0.5
A/CAEN/1/84                0.3  0.6  0.6  0.5  0.3  0.5  0.3  0.2  0.3  0.3  0.5
A/MISSISSIPPI/1/85         0.3  0.5  0.5  0.5  0.3  0.5  0.4  0.2  0.3  0.2  0.5
A/WASHINGTON/1/85          0.4  0.6  0.6  0.6  0.5  0.5  0.5  0.3  0.4  0.3  0.5
A/NEWJERSEY/1/85           0.8  0.9  0.9  1.0  0.8  0.9  0.7  0.5  0.6  0.5  0.5
NIL
USER(139): (pp-hi-table (put-hi-table-into-range-0-1 (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss #'e-dist))))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80              0.0  0.3  0.3  0.2  0.2  0.4  0.6  0.3  0.2  0.3  0.6
A/BANGKOK/1/79             0.1  0.2  0.5  0.0  0.4  0.3  0.5  0.4  0.4  0.4  0.7
A/BANGKOK/2/79             0.2  0.3  0.1  0.3  0.4  0.6  0.7  0.4  0.3  0.3  0.6
A/SHANGHAI/31/80           0.2  0.3  0.6  0.1  0.5  0.2  0.6  0.5  0.5  0.5  0.9
A/PHILIPPINES/2/82_EQ      0.4  0.6  0.7  0.5  0.3  0.6  0.7  0.6  0.5  0.5  0.7
A/TAIWAN/16/83             0.2  0.3  0.6  0.2  0.4  0.2  0.5  0.4  0.5  0.5  0.8
A/PANAMA/1/83              0.3  0.5  0.5  0.5  0.4  0.5  0.2 -0.0  0.3  0.2  0.4
A/CAEN/1/84                0.3  0.5  0.6  0.5  0.4  0.5  0.3  0.1  0.3  0.3  0.4
A/MISSISSIPPI/1/85         0.2  0.4  0.5  0.4  0.3  0.4  0.4  0.1  0.2  0.2  0.4
A/WASHINGTON/1/85          0.4  0.6  0.6  0.5  0.3  0.5  0.5  0.3  0.3  0.3  0.5
A/NEWJERSEY/1/85           0.8  0.9  0.7  1.0  0.8  1.0  0.6  0.4  0.6  0.5  0.4




USER(144): (pp-hi-table (put-hi-table-into-range-0-1 (f-hi-table (^ (x) (- (log (/ x 10) 2))) hi85)))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80              ---  0.3  0.5  0.2  0.2  0.5  0.8  0.5  0.3  0.3  0.7
A/BANGKOK/1/79             0.0  0.2  0.7  0.0  0.5  0.3  0.7  0.5  0.5  0.5  1.0
A/BANGKOK/2/79             0.0  0.5  ---  0.5  0.5  0.8  0.8  0.5  0.5  0.5  0.8
A/SHANGHAI/31/80           0.3  0.3  0.8  ---  0.7  0.3  0.7  0.7  0.7  0.7  1.0
A/PHILIPPINES/2/82_EQ      0.5  0.8  0.8  0.7  0.3  0.8  0.8  0.7  0.7  0.7  1.0
A/TAIWAN/16/83             0.2  0.5  0.8  0.2  0.7  0.2  0.7  0.5  0.7  0.7  1.0
A/PANAMA/1/83              0.3  0.7  0.7  0.7  0.5  0.7  0.2  0.0  0.5  0.3  0.5
A/CAEN/1/84                0.5  0.7  0.7  0.7  0.5  0.7  0.3  ---  0.5  0.3  0.7
A/MISSISSIPPI/1/85         0.3  0.5  0.7  0.5  0.3  0.5  0.5  0.2  0.2  0.2  0.5
A/WASHINGTON/1/85          0.5  0.7  0.8  0.7  0.5  0.7  0.7  0.3  0.5  0.3  0.7
A/NEWJERSEY/1/85           0.8  1.0  1.0  1.0  1.0  1.0  0.8  0.5  0.8  0.7  0.5




                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80             -0.0  0.3  0.3  0.3  0.3  0.5  0.6  0.4  0.3  0.3  0.6
A/BANGKOK/1/79             0.2  0.2  0.6  0.1  0.4  0.3  0.6  0.5  0.5  0.5  0.8
A/BANGKOK/2/79             0.2  0.3  0.2  0.4  0.3  0.6  0.8  0.5  0.3  0.3  0.6
A/SHANGHAI/31/80           0.3  0.3  0.7  0.2  0.5  0.3  0.6  0.6  0.6  0.6  0.9
A/PHILIPPINES/2/82_EQ      0.4  0.7  0.6  0.5  0.3  0.8  0.6  0.6  0.5  0.6  0.8
A/TAIWAN/16/83             0.3  0.3  0.7  0.3  0.5  0.2  0.5  0.5  0.6  0.5  0.8
A/PANAMA/1/83              0.3  0.6  0.6  0.5  0.3  0.5  0.3  0.2  0.3  0.3  0.5
A/CAEN/1/84                0.3  0.6  0.6  0.5  0.3  0.5  0.3  0.2  0.3  0.3  0.5
A/MISSISSIPPI/1/85         0.3  0.5  0.5  0.5  0.3  0.5  0.4  0.2  0.3  0.2  0.5
A/WASHINGTON/1/85          0.4  0.6  0.6  0.6  0.5  0.5  0.5  0.3  0.4  0.3  0.5
A/NEWJERSEY/1/85           0.8  0.9  0.9  1.0  0.8  0.9  0.7  0.5  0.6  0.5  0.5

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80              0.0  0.3  0.3  0.2  0.2  0.4  0.6  0.3  0.2  0.3  0.6
A/BANGKOK/1/79             0.1  0.2  0.5  0.0  0.4  0.3  0.5  0.4  0.4  0.4  0.7
A/BANGKOK/2/79             0.2  0.3  0.1  0.3  0.4  0.6  0.7  0.4  0.3  0.3  0.6
A/SHANGHAI/31/80           0.2  0.3  0.6  0.1  0.5  0.2  0.6  0.5  0.5  0.5  0.9
A/PHILIPPINES/2/82_EQ      0.4  0.6  0.7  0.5  0.3  0.6  0.7  0.6  0.5  0.5  0.7
A/TAIWAN/16/83             0.2  0.3  0.6  0.2  0.4  0.2  0.5  0.4  0.5  0.5  0.8
A/PANAMA/1/83              0.3  0.5  0.5  0.5  0.4  0.5  0.2 -0.0  0.3  0.2  0.4
A/CAEN/1/84                0.3  0.5  0.6  0.5  0.4  0.5  0.3  0.1  0.3  0.3  0.4
A/MISSISSIPPI/1/85         0.2  0.4  0.5  0.4  0.3  0.4  0.4  0.1  0.2  0.2  0.4
A/WASHINGTON/1/85          0.4  0.6  0.6  0.5  0.3  0.5  0.5  0.3  0.3  0.3  0.5
A/NEWJERSEY/1/85           0.8  0.9  0.7  1.0  0.8  1.0  0.6  0.4  0.6  0.5  0.4

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80              ---  0.3  0.5  0.2  0.2  0.5  0.8  0.5  0.3  0.3  0.7
A/BANGKOK/1/79             0.0  0.2  0.7  0.0  0.5  0.3  0.7  0.5  0.5  0.5  1.0
A/BANGKOK/2/79             0.0  0.5  ---  0.5  0.5  0.8  0.8  0.5  0.5  0.5  0.8
A/SHANGHAI/31/80           0.3  0.3  0.8  ---  0.7  0.3  0.7  0.7  0.7  0.7  1.0
A/PHILIPPINES/2/82_EQ      0.5  0.8  0.8  0.7  0.3  0.8  0.8  0.7  0.7  0.7  1.0
A/TAIWAN/16/83             0.2  0.5  0.8  0.2  0.7  0.2  0.7  0.5  0.7  0.7  1.0
A/PANAMA/1/83              0.3  0.7  0.7  0.7  0.5  0.7  0.2  0.0  0.5  0.3  0.5
A/CAEN/1/84                0.5  0.7  0.7  0.7  0.5  0.7  0.3  ---  0.5  0.3  0.7
A/MISSISSIPPI/1/85         0.3  0.5  0.7  0.5  0.3  0.5  0.5  0.2  0.2  0.2  0.5
A/WASHINGTON/1/85          0.5  0.7  0.8  0.7  0.5  0.7  0.7  0.3  0.5  0.3  0.7
A/NEWJERSEY/1/85           0.8  1.0  1.0  1.0  1.0  1.0  0.8  0.5  0.8  0.7  0.5


;;new my coordss (stress 3.2)
(setq my-coordss-2 '(
(-65.020615 10.397474 32.704987) 
(-80.696724 32.79792 -12.570631) 
(-86.262375 -51.80286 54.88443) 
(-91.51251 53.82491 -29.865702) 
(-49.10398 109.92156 28.526415) 
(-59.286236 63.294704 -35.742596) 
(72.09821 11.53216 -24.050001) 
(68.77503 35.461514 -24.666983) 
(30.729137 48.051678 1.7145377) 
(43.0124 74.86014 0.4489137) 
(182.26149 -27.939901 -3.0502017) 
(-33.818176 -32.5189 -10.438482) 
(-61.803 -51.747944 -61.554157) 
(-21.12302 -101.30669 38.17484) 
(-78.21514 -20.984049 -61.139866) 
(-1.0936756 37.09949 103.72856) 
(-51.715958 1.3685324 -114.42727) 
(37.480217 -31.356033 -109.6953) 
(35.930073 -38.752544 -11.215985) 
(24.552166 -13.345925 82.46068) 
(32.34376 -14.205464 70.64002) 
(86.42115 -43.513374 106.057556) 
))

USER(155): (pp-hi-table (put-hi-table-into-range-0-1 (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss-2 #'e-dist))))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80              0.0  0.2  0.3  0.2  0.2  0.4  0.6  0.3  0.2  0.2  0.6
A/BANGKOK/1/79             0.1  0.2  0.5  0.0  0.4  0.2  0.5  0.4  0.4  0.4  0.8
A/BANGKOK/2/79             0.1  0.3  0.1  0.3  0.3  0.6  0.7  0.4  0.3  0.3  0.6
A/SHANGHAI/31/80           0.2  0.2  0.6  0.1  0.5  0.2  0.5  0.5  0.5  0.5  0.9
A/PHILIPPINES/2/82_EQ      0.4  0.6  0.7  0.5  0.3  0.6  0.7  0.6  0.4  0.4  0.8
A/TAIWAN/16/83             0.2  0.3  0.6  0.1  0.4  0.2  0.4  0.4  0.5  0.5  0.8
A/PANAMA/1/83              0.3  0.4  0.5  0.5  0.4  0.4  0.2  0.0  0.3  0.2  0.4
A/CAEN/1/84                0.3  0.5  0.6  0.5  0.4  0.4  0.2  0.1  0.3  0.2  0.4
A/MISSISSIPPI/1/85         0.2  0.4  0.5  0.4  0.2  0.4  0.4  0.1  0.2  0.1  0.4
A/WASHINGTON/1/85          0.3  0.6  0.6  0.5  0.3  0.5  0.4  0.3  0.3  0.2  0.5
A/NEWJERSEY/1/85           0.8  0.9  0.8  1.0  0.8  1.0  0.6  0.4  0.6  0.5  0.4

(g-plot (transpose (flatten (hi-table-values hi85)) 
		   (flatten (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss-2 #'e-dist))))) :style 'scatter)




;;;----------------------------------------------------------------------
;;;                      now back into hi tables
;;;----------------------------------------------------------------------

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80              0.2  2.0  2.2  1.4  1.4  3.3  4.6  2.3  1.6  1.7  4.5
A/BANGKOK/1/79             0.7  1.4  3.6  0.4  3.1  1.9  4.0  2.9  3.4  3.3  6.1
A/BANGKOK/2/79             0.9  2.2  0.8  2.2  2.7  4.6  5.6  3.0  2.2  2.4  4.6
A/SHANGHAI/31/80           1.7  2.0  4.7  0.7  3.9  1.7  4.3  3.7  4.4  4.3  7.1
A/PHILIPPINES/2/82_EQ      3.3  4.8  5.9  3.9  2.0  4.6  6.0  4.4  3.5  3.6  6.1
A/TAIWAN/16/83             1.5  2.1  4.8  1.1  3.5  1.5  3.6  3.1  4.0  3.8  6.5
A/PANAMA/1/83              2.1  3.5  3.8  3.7  3.4  3.5  1.5  0.0  2.2  1.7  3.1
A/CAEN/1/84                2.4  3.9  4.4  3.9  3.3  3.6  1.9  0.7  2.5  2.0  3.6
A/MISSISSIPPI/1/85         1.6  3.4  3.9  3.2  1.7  3.4  2.9  1.0  1.5  1.2  3.4
A/WASHINGTON/1/85          2.7  4.4  5.0  4.0  2.2  4.0  3.5  2.0  2.3  2.0  4.0
A/NEWJERSEY/1/85           6.0  7.4  6.2  8.0  6.2  7.8  4.6  3.3  4.6  4.1  3.3
NIL
USER(161): (pp-hi-table (f-hi-table (^ (x) (round (* x 8))) (put-hi-table-into-range-0-1 (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss-2 #'e-dist)))))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80                0    2    2    1    1    3    5    2    2    2    4
A/BANGKOK/1/79               1    1    4    0    3    2    4    3    3    3    6
A/BANGKOK/2/79               1    2    1    2    3    5    6    3    2    2    5
A/SHANGHAI/31/80             2    2    5    1    4    2    4    4    4    4    7
A/PHILIPPINES/2/82_EQ        3    5    6    4    2    5    6    4    4    4    6
A/TAIWAN/16/83               2    2    5    1    4    1    4    3    4    4    7
A/PANAMA/1/83                2    4    4    4    3    4    2    0    2    2    3
A/CAEN/1/84                  2    4    4    4    3    4    2    1    2    2    4
A/MISSISSIPPI/1/85           2    3    4    3    2    3    3    1    2    1    3
A/WASHINGTON/1/85            3    4    5    4    2    4    4    2    2    2    4
A/NEWJERSEY/1/85             6    7    6    8    6    8    5    3    5    4    3
NIL
USER(162): (pp-hi-table (f-hi-table (^ (x) (round (* x -8))) (put-hi-table-into-range-0-1 (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss-2 #'e-dist)))))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80                0   -2   -2   -1   -1   -3   -5   -2   -2   -2   -4
A/BANGKOK/1/79              -1   -1   -4    0   -3   -2   -4   -3   -3   -3   -6
A/BANGKOK/2/79              -1   -2   -1   -2   -3   -5   -6   -3   -2   -2   -5
A/SHANGHAI/31/80            -2   -2   -5   -1   -4   -2   -4   -4   -4   -4   -7
A/PHILIPPINES/2/82_EQ       -3   -5   -6   -4   -2   -5   -6   -4   -4   -4   -6
A/TAIWAN/16/83              -2   -2   -5   -1   -4   -1   -4   -3   -4   -4   -7
A/PANAMA/1/83               -2   -4   -4   -4   -3   -4   -2    0   -2   -2   -3
A/CAEN/1/84                 -2   -4   -4   -4   -3   -4   -2   -1   -2   -2   -4
A/MISSISSIPPI/1/85          -2   -3   -4   -3   -2   -3   -3   -1   -2   -1   -3
A/WASHINGTON/1/85           -3   -4   -5   -4   -2   -4   -4   -2   -2   -2   -4
A/NEWJERSEY/1/85            -6   -7   -6   -8   -6   -8   -5   -3   -5   -4   -3
NIL
USER(163): (pp-hi-table (f-hi-table (^ (x) (+ 8 (round (* x -8)))) (put-hi-table-into-range-0-1 (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss-2 #'e-dist)))))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80                8    6    6    7    7    5    3    6    6    6    4
A/BANGKOK/1/79               7    7    4    8    5    6    4    5    5    5    2
A/BANGKOK/2/79               7    6    7    6    5    3    2    5    6    6    3
A/SHANGHAI/31/80             6    6    3    7    4    6    4    4    4    4    1
A/PHILIPPINES/2/82_EQ        5    3    2    4    6    3    2    4    4    4    2
A/TAIWAN/16/83               6    6    3    7    4    7    4    5    4    4    1
A/PANAMA/1/83                6    4    4    4    5    4    6    8    6    6    5
A/CAEN/1/84                  6    4    4    4    5    4    6    7    6    6    4
A/MISSISSIPPI/1/85           6    5    4    5    6    5    5    7    6    7    5
A/WASHINGTON/1/85            5    4    3    4    6    4    4    6    6    6    4
A/NEWJERSEY/1/85             2    1    2    0    2    0    3    5    3    4    5
NIL
USER(164): (pp-hi-table (f-hi-table (^ (x) (expt 2 (+ 8 (round (* x -8))))) (put-hi-table-into-range-0-1 (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss-2 #'e-dist)))))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80              256   64   64  128  128   32    8   64   64   64   16
A/BANGKOK/1/79             128  128   16  256   32   64   16   32   32   32    4
A/BANGKOK/2/79             128   64  128   64   32    8    4   32   64   64    8
A/SHANGHAI/31/80            64   64    8  128   16   64   16   16   16   16    2
A/PHILIPPINES/2/82_EQ       32    8    4   16   64    8    4   16   16   16    4
A/TAIWAN/16/83              64   64    8  128   16  128   16   32   16   16    2
A/PANAMA/1/83               64   16   16   16   32   16   64  256   64   64   32
A/CAEN/1/84                 64   16   16   16   32   16   64  128   64   64   16
A/MISSISSIPPI/1/85          64   32   16   32   64   32   32  128   64  128   32
A/WASHINGTON/1/85           32   16    8   16   64   16   16   64   64   64   16
A/NEWJERSEY/1/85             4    2    4    1    4    1    8   32    8   16   32
NIL
USER(165): (pp-hi-table (f-hi-table (^ (x) (* 10 (expt 2 (+ 8 (round (* x -8)))))) (put-hi-table-into-range-0-1 (make-hi-table (hi-table-antigens hi85) (hi-table-sera hi85) (firstn 11 (mapcar (^ (x) (lastn 11 x)) (all-comparisons-full my-coordss-2 #'e-dist)))))))

                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80             2560  640  640 1280 1280  320   80  640  640  640  160
A/BANGKOK/1/79            1280 1280  160 2560  320  640  160  320  320  320   40
A/BANGKOK/2/79            1280  640 1280  640  320   80   40  320  640  640   80
A/SHANGHAI/31/80           640  640   80 1280  160  640  160  160  160  160   20
A/PHILIPPINES/2/82_EQ      320   80   40  160  640   80   40  160  160  160   40
A/TAIWAN/16/83             640  640   80 1280  160 1280  160  320  160  160   20
A/PANAMA/1/83              640  160  160  160  320  160  640 2560  640  640  320
A/CAEN/1/84                640  160  160  160  320  160  640 1280  640  640  160
A/MISSISSIPPI/1/85         640  320  160  320  640  320  320 1280  640 1280  320
A/WASHINGTON/1/85          320  160   80  160  640  160  160  640  640  640  160
A/NEWJERSEY/1/85            40   20   40   10   40   10   80  320   80  160  320
NIL



---------------
lapedes
                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80             2560  640  320  640  640  160   80  320  640  640   80
A/BANGKOK/1/79            1280  640  160 1280  320  320   80  160  160  160   40
A/BANGKOK/2/79            1280  320 1280  320  320   80   40  160  320  320   80
A/SHANGHAI/31/80           640  320   80 1280  160  640   80   80   80   80   20
A/PHILIPPINES/2/82_EQ      160   80   80  160  320   40   80   80  160   80   40
A/TAIWAN/16/83             640  320   80  640  160  640  160  160   80  160   20
A/PANAMA/1/83              320   80   80  160  320  160  640 1280  320  320  160
A/CAEN/1/84                320   80   80  160  320  160  640 1280  320  320  160
A/MISSISSIPPI/1/85         640  160  160  160  320  160  320 1280  640  640  160
A/WASHINGTON/1/85          320  160   80   80  160  160  160  320  320  320  160
A/NEWJERSEY/1/85            40   20   20   10   40   20   40  160   80  160  160

smith
                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80             2560  640  320  640  640  320   80  320  640  640   80
A/BANGKOK/1/79            1280 1280  160 2560  320  640  160  320  320  320   40
A/BANGKOK/2/79            1280  640 1280  320  320   80   40  320  320  320   80
A/SHANGHAI/31/80           640  640   80 1280  160  640  160  160  160  160   20
A/PHILIPPINES/2/82_EQ      320   80   40  160  640   80   40  160  160  160   40
A/TAIWAN/16/83             640  640   80 1280  160  640  160  320  160  160   40
A/PANAMA/1/83              640  160  160  160  320  160  640 2560  320  640  320
A/CAEN/1/84                320  160   80  160  320  160  640 1280  320  640  160
A/MISSISSIPPI/1/85         640  320  160  320  640  320  320 1280  640 1280  320
A/WASHINGTON/1/85          320  160   80  160  640  160  160  640  320  640  160
A/NEWJERSEY/1/85            40   20   40   10   40   10   80  320   80  160  320

smith2
                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80             2560  640  640 1280 1280  320   80  640  640  640  160
A/BANGKOK/1/79            1280 1280  160 2560  320  640  160  320  320  320   40
A/BANGKOK/2/79            1280  640 1280  640  320   80   40  320  640  640   80
A/SHANGHAI/31/80           640  640   80 1280  160  640  160  160  160  160   20
A/PHILIPPINES/2/82_EQ      320   80   40  160  640   80   40  160  160  160   40
A/TAIWAN/16/83             640  640   80 1280  160 1280  160  320  160  160   20
A/PANAMA/1/83              640  160  160  160  320  160  640 2560  640  640  320
A/CAEN/1/84                640  160  160  160  320  160  640 1280  640  640  160
A/MISSISSIPPI/1/85         640  320  160  320  640  320  320 1280  640 1280  320
A/WASHINGTON/1/85          320  160   80  160  640  160  160  640  640  640  160
A/NEWJERSEY/1/85            40   20   40   10   40   10   80  320   80  160  320

hi85
                          OR80 BA79 BA79 SH80 PHEQ TA83 PA83 CA84 MI85 WA85 NE85
A/OREGON/4/80             2560  640  320 1280 1280  320   80  320  640  640  160
A/BANGKOK/1/79            2560 1280  160 2560  320  640  160  320  320  320   40
A/BANGKOK/2/79            2560  320 2560  320  320   80   80  320  320  320   80
A/SHANGHAI/31/80           640  640   80 2560  160  640  160  160  160  160   40
A/PHILIPPINES/2/82_EQ      320   80   80  160  640   80   80  160  160  160   40
A/TAIWAN/16/83            1280  320   80 1280  160 1280  160  320  160  160   40
A/PANAMA/1/83              640  160  160  160  320  160 1280 2560  320  640  320
A/CAEN/1/84                320  160  160  160  320  160  640 2560  320  640  160
A/MISSISSIPPI/1/85         640  320  160  320  640  320  320 1280 1280 1280  320
A/WASHINGTON/1/85          320  160   80  160  320  160  160  640  320  640  160
A/NEWJERSEY/1/85            80   40   40   40   40   40   80  320   80  160  320
|#



;;;----------------------------------------------------------------------
;;;                      much later (july 2001)
;;;----------------------------------------------------------------------

(defun shepherd-two-configurations (names-1 coordss-1 names-2 coordss-2)
  (let ((common-names (reverse (intersection names-1 names-2))))
    (all-comparisons 
     common-names
     (^ (name-a name-b)
	(list (e-dist (nth (position name-a names-1) coordss-1)
		      (nth (position name-b names-1) coordss-1))
	      (e-dist (nth (position name-a names-2) coordss-2)
		      (nth (position name-b names-2) coordss-2))
	      name-b name-a)))))

(defun shepherd-plot-two-configurations (names-1 coordss-1 names-2 coordss-2 &key plot-args max-x)
  (let* ((shepherd (shepherd-two-configurations names-1 coordss-1 names-2 coordss-2)))
    (if max-x
	(setq shepherd
	  (collect (^ (l) (<= (nth 0 l) max-x)) shepherd))) 
    (apply #'gnuplot-correlation
	   (mapcar (^ (l) (firstn 2 l)) shepherd)
	   plot-args)))


;;;----------------------------------------------------------------------
;;;                   and later again (May 2006)
;;;----------------------------------------------------------------------

(defun shepard-plot-data-from-save (save &optional &key include-names plot plot-filename x-max)  ;; use x-max for setting the range
  (let* ((hi-table (table-from-save save))
	 (similarity-table-p (similarity-table-p hi-table))
	 (mds-coordss (starting-coordss-from-save save))
	 (coordss (coordss mds-coordss))
	 (col-bases (col-bases mds-coordss))
	 (distances (filter 
		     (^ (l) 
			(let ((d1 (nth 0 (reverse l)))
			      (d2 (nth 1 (reverse l))))
			  (or (eql '* d1)
			      (eql '* d2)
			      (dont-care-p d1)
			      (dont-care-p d2))))
		     (loop for (a-name . rest-names) on (hi-table-antigens hi-table) 
			 for (a-coords . rest-coordss) on coordss 
			 for row-values in (hi-table-values hi-table) 
			 for antigen-number from 0 append
			   (loop for b-name in rest-names 
			       for b-coords in rest-coordss 
			       for value in (nthcdr (inc antigen-number) row-values) 
			       for serum-number from (inc antigen-number) collect
				 (append
				  (if include-names (sort-strains (list a-name b-name)))
				  (list 
				   (if similarity-table-p 
				       (cond ((numberp value) (- (nth serum-number col-bases) value))
					     ((thresholdp value) (read-from-string 
								  (format nil ">~d" 
									  (coerce (nth serum-number col-bases) 'single-float))))
					     ((true-dont-care-p value) '*)
					     (t value))
				     value)
				   (e-dist a-coords b-coords))))))))
    (if plot
	(progn
	  (gnuplot-correlation 
	   (if include-names
	       (mapcar #'cdr distances)
	     distances)
	   :x-title "Target distance"
	   :y-title "Map distance"
	   :equal-axes-range t
	   :show-diagonal t
           :x-max x-max)
	  (if plot-filename
	      (progn
		(sleep 1)
		(gnuplot-png 14 plot-filename 1.0)
		(sleep 1)
		(gnuplot-exit)))))
    distances
    ))



  
  