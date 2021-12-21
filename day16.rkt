#lang racket

(define (string->string-list str)
  (map string (string->list str)))

;(define input "D2FE28")
;(define input "38006F45291200")
;(define input "EE00D40C823060")
;(define input "8A004A801A8002F478")
;(define input "620080001611562C8802118E34")
;(define input "C0015000016115A2E0802F182340")
;(define input "A0016C880162017C3686B18A3D4780")

;(define input "C200B40A82") ; 1 + 2
;(define input "04005AC33890") ; 6 * 9
;(define input "880086C3E88112") ; min 7 8 9
;(define input "CE00C43D881120") ; max 7 8 9
;(define input "D8005AC2A8F0") ; 5 < 15
;(define input "F600BC2D8F") ; 5 > 15
;(define input "9C005AC2F8F0") ; 5 = 15
;(define input "9C0141080250320F1802104A08") ; 1 + 3 = 2 * 2

(define input (first (file->lines "day16.txt")))

(define bins (list (list "0" "0000")
                   (list "1" "0001")
                   (list "2" "0010")
                   (list "3" "0011")
                   (list "4" "0100")
                   (list "5" "0101")
                   (list "6" "0110")
                   (list "7" "0111")
                   (list "8" "1000")
                   (list "9" "1001")
                   (list "A" "1010")
                   (list "B" "1011")
                   (list "C" "1100")
                   (list "D" "1101")
                   (list "E" "1110")
                   (list "F" "1111")))

(define (all-but-last lst)
  (take lst (- (length lst) 1)))

(define (h2b hex-str) ; todo make this suck less
  (let* ([hex-list (string->string-list hex-str)]
         [binary-list (map (λ (x) (second (assoc x bins))) hex-list)])
    (apply string-append binary-list)))

(define (b2d bin-str)
  (string->number bin-str 2))

(define versions (make-hash))

(define (parse-literal str)
  (let* ([res (read-literal str)]
         [len (string-length res)]
         [rest (substring str (+ (/ len 4) len))]
         [dec (b2d res)])
    ;(printf "parse-literal. returning: ~s~n" (list dec rest))
    (list dec rest)))

(define (read-literal str)
  ;(printf "read-literal ~s~n" str)
  (let loop ([group str])
    (let* ([first (substring group 0 1)]
           [next-four (substring group 1 5)]
           [rest (substring group 5)])
      (if (equal? first "0")
          next-four
          (string-append next-four (loop rest))))))

(define (length-id-0 str)
  (let* ([len (b2d (substring str 0 15))]
         [inner (substring str 15 (+ 15 len))]
         [rest (substring str (+ 15 len))]
         ;[_ (printf "length-id-0 str: ~s len: ~s inner: ~s rest: ~s~n" str len inner rest)]
         [res (parse-multiple inner)]
         [res-value (all-but-last res)])
    (list res-value rest)))

(define (parse-multiple str)
  ;(printf "parse-multpile ~s~n" str)
  (let loop ([group str])
    (let* ([res (parse group)]
           [data (first res)]
           [rest (second res)])
      ;(printf "parse-multiple loop res: ~s data: ~s rest: ~s~n" res data rest)
      (if (= 0 (string-length rest))
          (list data rest)
          (cons data (loop rest))))))

(define (length-id-1 str)
  (let* ([num (b2d (substring str 0 11))]
         [rest (substring str 11)]
         ;[_ (printf "length-id-1. str: ~s num: ~s rest: ~s~n" str num rest)]
         [res (parse-n rest num)])
    ;(printf "length-id-1 done. res: ~s~n" res)
    res))

(define (parse-n str num)
  ;(printf "parse-n ~s ~s~n" str num)
  (let loop ([group str]
             [n num])
    ;(printf "parse-n loop ~s ~s~n" group n)
    (let* ([res (parse group)]
           [data (first res)]
           [rest (second res)])
      ;(printf "parse-n loop res: ~s data: ~s rest: ~s n: ~s~n" res data rest n)
      (if (= 1 n)
          (list data rest)
          (cons data (loop rest (sub1 n)))))))

(define (parse-operator str)
  (let* ([length-type-id (substring str 0 1)])
    ;(printf "parse-operator ~s. length-type-id: ~s~n" str length-type-id)
    (if (equal? length-type-id "0")
        (length-id-0 (substring str 1))
        (length-id-1 (substring str 1)))))

(define (gt x y) (if (> x y) 1 0))
(define (lt x y) (if (< x y) 1 0))
(define (eq x y) (if (equal? x y) 1 0))

(define (parse str)
  ;(printf "parse ~s~n" str)
  (let* ([version (substring str 0 3)]
         [dec-version (b2d version)]
         [type-id (substring str 3 6)]
         [body (substring str 6)]
         [op (cond [(equal? type-id "100") first]
                   [(equal? type-id "000") (λ (x) (apply + x))]
                   [(equal? type-id "001") (λ (x) (apply * x))]
                   [(equal? type-id "010") (λ (x) (apply min x))]
                   [(equal? type-id "011") (λ (x) (apply max x))]
                   [(equal? type-id "101") (λ (x) (gt (first x) (second x)))]
                   [(equal? type-id "110") (λ (x) (lt (first x) (second x)))]
                   [(equal? type-id "111") (λ (x) (eq (first x) (second x)))])]
         [res-list ((if (equal? type-id "100") parse-literal parse-operator) body)]
         [res (flatten (all-but-last res-list))]
         [post-op (op res)]
         [rest (last res-list)])
    ;(printf "bottom of parse. res: ~s rest: ~s post-op: ~s~n" res rest post-op)
    (hash-set! versions 'count (+ dec-version (hash-ref! versions 'count 0)))
    (list post-op rest)))

(define go
  (let* ([str (h2b input)])
    (first (parse str))))

go
versions