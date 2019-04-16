
;; define some global variables

(defvar seconds)
(defvar minutes)
(defvar hour)
(defvar day)
(defvar date)
(defvar month)
(defvar year)
(defvar Pres)
(defvar C1)
(defvar C2)
(defvar C3)
(defvar C4)
(defvar C5)
(defvar C6)
(defvar 5611D1)
(defvar 5611D2)
(defvar dT)
(defvar TEMP)
(defvar TEMP2)
(defvar TEMP3)
(defvar OFF)
(defvar SENS)
(defvar P)
(defvar LUXADC0)
(defvar LUXADC1)
(defvar LUX)
(defvar CH1/CH0)
(defvar dst)							;1=summertime , 0=wintertime
(defvar hourrise)
(defvar minuterise)
(defvar hourset)
(defvar minuteset)

; next variables are needed for calculating sunrise-set

(defvar longitude)
(defvar latitude)
(defvar zenith)
(defvar D2R)
(defvar R2D)
(defvar PI)
(defvar lnhour)
(defvar sunrise)			;false for sunrise, true for sunset
(defvar t)
(defvar doy)					;day of the year
(defvar M)
(defvar L)
(defvar RA)
(defvar Lquadrant)
(defvar RAquadrant)
(defvar sinDec)
(defvar cosDec)
(defvar cosH)
(defvar H)
(defvar T)
(defvar UT)
(defvar localT)
(defvar HH)
(defvar MM)
(defvar JD)

;variables for moonphase
(defvar MD)					;# of moondays since 5 april 2019 which was a newmoon



; some variables for sunrise-set are in fact constants

(setq PI 3.1415926)
(setq longitude 4.2521)
(setq latitude 51.2175)
(setq zenith 90.83333)
(setq D2R (/ PI 180))
(setq R2D (/ 180 PI))
(setq sunrise 0)
(setq doy 1)



;; scani2c : detect all connected i2c devices on the bus
;;           and print the base address in hex

(defun scani2c ()
(dotimes (p 127)
(with-i2c (str p)
(when (restart-i2c str) (princ (* p 2)) (terpri) ))))


(defun pc (i) (princ (code-char (+ i (if (<= i 9) 48 55))) s))

(defun pb (i) (pc (ash i -4)) (pc (logand i 15)))





;; ----------------------------
;; RTC functions
;; Read the time from the RTC
;; RTC is a DS1338 chip
;; ----------------------------

(defun rtc ()
     	(with-i2c (str #x68)
       	(write-byte 0 str)
       	(restart-i2c str 9)								;we have to read 9 bytes now !
       	(setq seconds (read-byte str))
       	(setq minutes (read-byte str))
       	(setq hour (read-byte str))
				(setq day (read-byte str))
       	(setq date (read-byte str))
       	(setq month (read-byte str))
       	(setq year (read-byte str))
				(setq dst (read-byte str))		;dummy read , reads control register
				(setq dst (read-byte str))))	;this reads register 8 , being the dst info
	

;; setclock :
;; Set sec min hr day dow month year dst
;; Note : use #x input because numbers must be hex !
;; dow : monday = 1 , ...sunday=7
;; dst is daylight saving time , set to 1 if dst is active =summertime , set to 0 otherwise

(defun setclock (sec min hr dow day month year dsat)
    	(with-i2c (str #x68)
    	(write-byte 0 str)
    	(write-byte sec str)
    	(write-byte min str)
    	(write-byte hr str)    
    	(write-byte dow str)
    	(write-byte day str)
    	(write-byte month str)
    	(write-byte year str)
			(write-byte #x93 str)
			;dst is stored to register 8
			(write-byte dsat str)))


;; set summertime
;; 02 hour becomes 03 hour , and set the dst byte 

(defun set_summertime ()
			(with-i2c (str #x68)
    	(write-byte 2 str)
    	(write-byte 3 str))
			(with-i2c (str #x68)
    	(write-byte 8 str)
    	(write-byte 1 str)))



;; set wintertime
;; 03 hour becomes 02 hour , and clear the dst byte

(defun set_wintertime ()
			(with-i2c (str #x68)
    	(write-byte 2 str)
    	(write-byte 2 str))
			(with-i2c (str #x68)
    	(write-byte 8 str)
    	(write-byte 0 str)))



;;	------------------------------------------------
;;	si7021 temperature and humidity sensor functions
;;	------------------------------------------------

    (defun si7021-reset ()
      (with-i2c (s #x40)
        (write-byte #xFE s))
      (delay 50)
      nil)
     
     
    (defun si7021-temperature ()
      (with-i2c (s #x40)
        (write-byte #xE3 s))
      (delay 30)
      (with-i2c (s #x40 2)
        (let* ((hi (read-byte s))
               (lo (read-byte s))
               (raw-temp (float (logior (ash hi 8) (logand lo #xFF)))))
          	(- (/ (* raw-temp 175.72) 65536.0) 46.85))))
     
     
    (defun si7021-humidity ()
      (with-i2c (s #x40)
        (write-byte #xE5 s))
      (delay 30)
      (with-i2c (s #x40 2)
        (let* ((hi (read-byte s))
               (lo (read-byte s))
               (raw-temp (float (logior (ash hi 8) (logand lo #xFF)))))
          	(- (/ (* raw-temp 125.0) 65536.0) 6.0))))


;;	--------------------------------
;;	MS5611 pressure sensor functions
;;	--------------------------------

(defun ms5611-reset ()
	(with-i2c (str #x76)
	(write-byte #x1E str))
	(delay 10))

(defun ms5611-readprom ()
	(with-i2c (str #x76)
	(write-byte #xA2 str))
	(with-i2c (str #x76 2)
	(setq C1 (+(*(read-byte str) 255) (read-byte str))))
	
	(with-i2c (str #x76)
	(write-byte #xA4 str))
	(with-i2c (str #x76 2)
	(setq C2 (+(*(read-byte str) 255) (read-byte str))))

	(with-i2c (str #x76)
	(write-byte #xA6 str))
	(with-i2c (str #x76 2)
	(setq C3 (+(*(read-byte str) 255) (read-byte str))))

	(with-i2c (str #x76)
	(write-byte #xA8 str))
	(with-i2c (str #x76 2)
	(setq C4 (+(*(read-byte str) 255) (read-byte str))))

	(with-i2c (str #x76)
	(write-byte #xAA str))
	(with-i2c (str #x76 2)
	(setq C5 (+(*(read-byte str) 255) (read-byte str))))

	(with-i2c (str #x76)
	(write-byte #xAC str))
	(with-i2c (str #x76 2)
	(setq C6 (+(*(read-byte str) 255) (read-byte str)))))



(defun ms5611-readadc ()
	;read D1 with OSR=4096
	(with-i2c (str #x76)
	(write-byte #x48 str))
	(delay 10)	
	(with-i2c (str #x76)
	(write-byte #x00 str))
	(with-i2c (str #x76 3)
	(setq 5611D1 (+(+(*(read-byte str) 65536) (*(read-byte str) 255) (read-byte str)))))
	;read D2 with OSR=4096
	(with-i2c (str #x76)
	(write-byte #x58 str))
	(delay 10)	
	(with-i2c (str #x76)
	(write-byte #x00 str))
	(with-i2c (str #x76 3)
	(setq 5611D2 (+(+(*(read-byte str) 65536) (*(read-byte str) 255) (read-byte str)))))
	;calculate the pressure ,see datasheet !
	(setq dT (- 5611D2 (* C5 256)))
	(setq TEMP (round (/(+ 2000 (* dT (/ C6 8388608))) 100)))
	(setq OFF (+(* C2 65536) (/(* C4 dT) 128)))
	(setq SENS (+(* C1 32768) (/(* C3 dT) 256)))
	(setq P (/(/(-(* 5611D1 (/ SENS 2097152)) OFF) 32768) 100)))


(defun apds9301-init ()
	;power on sensor
	(with-i2c (str #x39)
	(write-byte #x80 str)
	(write-byte #x03 str))
	;test communication ,  should return 3
    	(with-i2c (str #x39)
       	(write-byte #x80 str)
       	(restart-i2c str 1)
       	(read-byte str)))

(defun apds9301-readadc ()
	;int on every adc cycle
	(with-i2c (str #x39)
	(write-byte #x86 str)
	(write-byte #x10 str))	
	;set sensor to 16bit mode
	;and read 16bit ADC0
	(loop (if (not(digitalread 27)) (return)))
	(with-i2c (str #x39)
	(write-byte #xAC str)
	(restart-i2c str 2)
	(setq LUXADC0 (+ (read-byte str) (*(read-byte str) 256))))
	;now read ADC1
	(with-i2c (str #x39)
	(write-byte #xAE str)
	(restart-i2c str 2)
	(setq LUXADC1 (+ (read-byte str)  (*(read-byte str) 256))))
	;clear int
	(with-i2c (str #x39)
	(write-byte #xC0 str))
	;now convert channels to Lux
	;next line is to prevent division by zero error if LUXADC should be zero	
	(if (= 0 LUXADC0) (setq CH1/CH0 1.31) (setq CH1/CH0 (/ LUXADC1 LUXADC0)))
	(cond 
				
		((and (> CH1/CH0 0) (<= CH1/CH0 0.50)) (setq LUX (- (* 0.0304 LUXADC0) (* 0.062 LUXADC0 (expt (/ LUXADC1 LUXADC0) 1.4)))))
		((and (> CH1/CH0 0.50) (<= CH1/CH0 0.61)) (setq LUX (- (* 0.0224 LUXADC0) (* 0.031 LUXADC0))))
		((and (> CH1/CH0 0.61) (<= CH1/CH0 0.80)) (setq LUX (- (* 0.0128 LUXADC0) (* 0.0153 LUXADC0))))
		((and (> CH1/CH0 0.80) (<= CH1/CH0 1.30)) (setq LUX (- (* 0.00146 LUXADC0) (* 0.00112 LUXADC0))))
		((> CH1/CH0 1.30) (setq LUX 0)))

	(if (< LUX 0) (setq (abs LUX)))	

)
		


(defun println (x s) (princ x s) (princ #\return s) (princ #\newline s))




(defun webpage ()
	
		(pinmode 13 1)
		(digitalwrite 13 0)		
		(delay 2000)
		(digitalwrite 13 1)
		(loop
		(delay 1000)
		(if (string= "192.168.0.212" (wifi-connect "MYSSID" "MY PASSWORD")) (return)))
		
		(wifi-server)
		(si7021-reset)
		(ms5611-reset)
		(ms5611-readprom)
		(apds9301-init)
		
    
  	(loop
		(rtc)
		;check for summer or wintertime , and adjust time if needed
		(if (and (= month 3) (>= (dd date) 25) (= day 7) (= hour 2) (= dst 0)) (set_summertime))
		(if (and (= (dd month) 10) (>= (dd date) 25) (= day 7) (= hour 3) (= dst 1)) (set_wintertime))
		;sleep for 5 seconds for power saving
		(sleep 5)
		(dotimes (x 4)
		(digitalwrite 13 1)
  	(si7021-humidity)
		(si7021-temperature)
		(ms5611-readadc)
		(with-client (s)
  		(loop
  		(let ((line (read-line s)))
  		(when (null line) (return))
  		(print line)))
    (println "HTTP/1.1 200 OK" s)
    (println "Content-Type: text/html" s)
    (println "Connection: close" s)
    (println "Refresh: 60" s)
    (println "" s)
		(princ "<body bgcolor='#E8FFCC'>" s)
		(princ "<tt>" s)
    (princ "<font color = '#7F00FF' > <h1>WEATHER STATION WEBPAGE HOSTED BY SUY RONNY</h1>" s)
    (princ "<font color = '#7F00FF' > <h1>9120 BEVEREN - BELGIUM , 51.2175 N , 4.2521 E</h1>" s)
		(princ "<font color = '#7F0040' > <h1>Local time : " s)		
		(princ (pb hour) )
		(princ ":" s)
		(princ (pb minutes) )
		(princ "  (GMT+1)  " s)
		(princ "<br>" s)
		(princ (subseq "MonTueWedThuFriSatSun" (* (- day 1) 3) (* 3 day)) s)
		(princ "  " s)
		(princ (pb date) )
		(princ " " s)
		(princ (subseq "JanFebMarAprMayJunJulAugSep------------------OctNovDec" (* (- month 1) 3) (* 3 month)) s)
		(princ " 20" s)
		(princ (pb year))
		(princ " DST=" s)
		(princ dst s)	
		(princ "</h1>" s)
		(princ "<br>" s)
		(princ "<font color = '#003366' >" s)
		(princ "<h1> - SENSOR READINGS - " s)
		(princ "</h1>" s)
    (princ "<tt>" s)
		(princ "<h1>Temperature ....... : " s)
  	(princ (si7021-temperature) s)
		(princ " C</h1>" s)
    (princ "<h1>Humidity .......... : " s)
    (princ (si7021-humidity) s)
		(princ " %RH</h1>" s)
		(princ "<h1>Pressure .......... : " s)
    (princ P s)
    (princ " HPa</h1>" s)
		(princ "<h1>Ambient Light ..... : " s)
		(apds9301-readadc)    
		(princ LUX s)
    (princ " Lux</h1>" s)

		(princ "<br>" s)
		(princ "<font color = '#994c00' >" s)
		(princ "<h1> - EPHEMERESIS -" s)
		(princ "</h1>" s)
		(princ "<h1>Sunrise ........... : " s)
		(setq sunrise 1)
		(DayOfYear)
		(srs)
		(if (< HH 10) (princ "0" s))
		(princ HH s)
		(princ ":" s)
		(if (< MM 10) (princ "0" s))
		(princ MM s)
		(princ "<br>" s)
		(princ "Tomorrow .......... : " s)
		(setq doy (+ doy 1))
		(srs)
		(if (< HH 10) (princ "0" s))
		(princ HH s)
		(princ ":" s)
		(if (< MM 10) (princ "0" s))
		(princ MM s)
		(princ "<br>" s)
		(princ "Day After Tomorrow. : " s)
		(setq doy (+ doy 1))
		(srs)
		(if (< HH 10) (princ "0" s))
		(princ HH s)
		(princ ":" s)
		(if (< MM 10) (princ "0" s))
		(princ MM s)
		(setq doy (- doy 2))
		(princ "<h1>Sunset ............ : " s)
		(setq sunrise 0)
		(srs)
		(if (< HH 10) (princ "0" s))
		(princ HH s)
		(princ ":" s)
		(if (< MM 10) (princ "0" s))		
		(princ MM s)
		(princ "<br>" s)
		(princ "Tomorrow .......... : " s)
		(setq doy (+ doy 1))
		(srs)
		(if (< HH 10) (princ "0" s))
		(princ HH s)
		(princ ":" s)
		(if (< MM 10) (princ "0" s))
		(princ MM s)
		(princ "<br>" s)
		(princ "Day After Tomorrow. : " s)
		(setq doy (+ doy 1))
		(srs)
		(if (< HH 10) (princ "0" s))
		(princ HH s)
		(princ ":" s)
		(if (< MM 10) (princ "0" s))
		(princ MM s)

		(princ "<h1>Moonphase ......... : " s)
		(setq TEMP (- TEMP3 3381))							;3381 = 5 march 2019 and a new moon
		(setq MD (mod TEMP 29.53))

		(cond 
      ;full moon going to second quarter			
      ((and (>= MD 16 ) (<= MD 21))	(with-sd-card (sd "fm2sq.txt")
                                  	(loop
                                  	(let ((data (read-line sd)))
                                  	(when (null data) (return))
                                  	(princ data s)))))
      ; second quarter going to new moon			
      ((and (>= MD 23) (<= MD 28))	(with-sd-card (sd "sq2nm.txt")
                                  	(loop
                                  	(let ((data (read-line sd)))
                                  	(when (null data) (return))
                                  	(princ data s)))))
      ;new moon going to first quarter
			((and (>= MD 2) (<= MD 6) ) (with-sd-card (sd "nm2fq.txt")
                                  (loop
                                  (let ((data (read-line sd)))
                                  (when (null data) (return))
                                  (princ data s)))))
      ;first quarter going to full moon
			((and (>= MD 8) (<= MD 14)) (with-sd-card (sd "fq2fm.txt")
                                  (loop
                                  (let ((data (read-line sd)))
                                  (when (null data) (return))
                                  (princ data s)))))
      ;full moon
  		((and (> MD 14)	(< MD 16))		(with-sd-card (sd "fm.txt")
                                  (loop
                                  (let ((data (read-line sd)))
                                  (when (null data) (return))
                                  (princ data s)))))
      ;second quarter
			((and (> MD 21) (< MD 23))	(with-sd-card (sd "sq.txt")
                                  (loop
                                  (let ((data (read-line sd)))
                                  (when (null data) (return))
                                  (princ data s)))))
      ;new moon
			((or (>= MD 29 ) (< MD 2)) 	(with-sd-card (sd "nm.txt")
                                  (loop
                                  (let ((data (read-line sd)))
                                  (when (null data) (return))
                                  (princ data s)))))
      ;first quarter
			((and (> MD 6) (< MD 8))		(with-sd-card (sd "fq.txt")
                                  (loop
                                  (let ((data (read-line sd)))
                                  (when (null data) (return))

                                  (princ data s))))))

		(princ "</h1>" s) 	
		(princ "<br>" s)
		(princ "<font color = '#99004c' >" s)
		(princ "<h1> - SYSTEM INFO -" s)
		(princ "</h1>" s)
		(princ "<h1>Battery Voltage ... : " s)
    (princ (*(analogread 35) (/ 3.3 4096) 2 1.08) s) 
    (princ " V</h1>" s)
		(princ "<h1>State of Charge ... : " s)
		(princ (round(*(-(*(analogread 35) (/ 3.3 4096) 2 1.08) 3.25) (/ 100 0.925))) s) 
		(princ " %</h1>" s) 
		(princ "<h1>Solar Panel Voltage : " s)
    (setq TEMP (*(analogread 34) (/ 3.3 4096) (/ 66 27) 1.14))
		(princ TEMP s)
    (princ " V</h1>" s)
		(if ( > TEMP 4.75)
						(princ "<h1>Station powered by .: Solar Panel" s)
						(princ "<h1>Station powered by .: Battery" s))
		(princ "</h1>" s)
		(princ "</tt>" s)
		(println "</h1></body></html>" s)
    (println "" s))

		;blink a led to show the station is running
		(digitalwrite 13 0))
		
		;for debug remove if ok
		(print (room))))

;ephemeris

; dd converts the rtc values to a decimal number
(defun dd (x)
(+(*(floor(/ x 16)) 10) (mod x 16)))



(defun correctmy ()
(setq M (+ M 12))
(setq Y (- Y 1)))



;calculate julianday on D,M,Y since 1/1/2010 

(defun JulianDay (D M Y)

(if (< M 3) (correctmy)) 
(setq TEMP (+ (- (+ D (truncate(/(-(* 153 M) 457) 5)) (* 365 Y) (floor(/ Y 4))) (floor(/ Y 100))) (floor (/ Y 400)) 1721118.5))
; now calculate JD since [1,1,2010]
(setq JD (- TEMP 2455197.5)))		
;2455197.5 is JD for 1/1/2010 at 12:00:00 hour

(defun DayOfYear ()
(setq TEMP3 (JulianDay (dd date) (dd month) (+(dd year) 2000)))
(setq TEMP2 (JulianDay 1 1 (+(dd year) 2000)))
(setq doy (+(- TEMP3 TEMP2) 1)) 
)

(defun srs()

;convert the longitude to hour value and calculate an approximate time
(setq lnhour (/ longitude 15))
(if (= 1 sunrise) (setq t (+ doy (/(- 6 lnhour) 24))) (setq t (+ doy (/(- 18 lnhour) 24))))

;calculate the sun's mean anomaly
(setq M (- (* 0.9856 t) 3.289))

;calculate the sun's true longitude
(setq L (+ M (* 1.916 (sin(* M D2R))) (* 0.020 (sin(* 2 M D2R))) 282.634))
(if (> L 360) (setq L (- L 360)))
(if (< L 0)   (setq L (+ L 360)))

;calculate the suns right ascension
(setq RA (* R2D (atan(* 0.91764 (tan(* L D2R))))))
(if (> RA 360) (setq RA (- RA 360)))
(if (< RA 0)   (setq RA (+ RA 360)))

;right ascension needs to be in same quadrant
(setq Lquadrant (*(floor(/ L 90)) 90))
(setq RAquadrant (*(floor(/ RA 90)) 90))
(setq RA (+ RA (- Lquadrant RAquadrant)))

;convert right ascension into hours
(setq RA (/ RA 15))

;calculate the suns declination
(setq sinDec (* 0.39782 (sin(* L D2R))))
(setq cosDec (cos(asin sinDec)))

;calculate the suns local hour angle

(setq cosH (/(- (cos(* zenith D2R)) (* sinDec (sin(* latitude D2R)))) (* cosDec (cos(* latitude D2R)))))

(if (= 1 sunrise) (setq H (- 360 (* R2D (acos cosH)))) (setq H (* R2D (acos cosH))))

(setq H (/ H 15))

;calculate local mean time of rising/setting

(setq T (-(+ H RA) (* 0.06571 t) 6.622))

;adjust back to UTC

(setq UT (- T lnhour))
(if (> UT 24) (setq UT (- UT 24)))
(if (< UT 0)  (setq UT (+ UT 24)))

;convert UT value to local zone of lattitude/longitude

(setq localT (+ UT 1))
(setq localT (* localT 3600))

;convert localT to HH:MM
(setq HH (floor(/ localT 3600)))
(setq MM (round(/(- localT (* HH 3600)) 60)))
(if (= MM 60) (adjustHHMM))
;check/adjust time if dst=1
(if (= dst 1) (setq HH (+ HH 1)))
)

(defun adjustHHMM ()
(setq MM 0)
(setq HH (+ HH 1)))

