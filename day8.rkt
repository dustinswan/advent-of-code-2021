#lang racket

(define (map-map f lst) ; map . map
  (map (λ (x) (map (λ (y) (f y)) x)) lst))

(define data
  (map-map string-split
           (map (λ (l) (string-split l " | "))
                (file->lines "day8.txt"))))

(define (is-easy? d)
  (let ([len (string-length d)])
    (or (= len 2) (= len 3) (= len 4) (= len 7))))

(define part1
  (let* ([just-output (map second data)]
         [just-easy-digits (map (λ (x) (filter is-easy? x)) just-output)])
    (length (flatten just-easy-digits))))

part1

; part 2

; Can we just count the number of times a segment appears?
; No. Damn. But it gets us close. Naming the segments A - G
;
; A has 8 instances and IS NOT in 1
; B has 6 instances
; C has 8 instances and IS in 1
; D has 7 instances and IS in 4
; E has 4 instances
; F has 9 instances
; G has 7 instances and IS NOT in 4

(define (num-instances letter lst)
  (length (filter (λ (s) (string-contains? s letter)) lst)))

(define letters '("a" "b" "c" "d" "e" "f" "g"))

(define (assign-segments lst) ; returns a vector with each position corresponing to a segment's letter. e.g. vect[0] = c, vect[1] = g, etc.
  (let ([one (findf (λ (s) (= (string-length s) 2)) lst)]
        [four (findf (λ (s) (= (string-length s) 4)) lst)])
    (vector
     (findf (λ (l) (and (= (num-instances l lst) 8) (not (string-contains? one l)))) letters) ; A has 8 instances and isn't in 1
     (findf (λ (l) (= (num-instances l lst) 6)) letters) ; B
     (findf (λ (l) (and (= (num-instances l lst) 8) (string-contains? one l))) letters) ; C
     (findf (λ (l) (and (= (num-instances l lst) 7) (string-contains? four l))) letters) ; D
     (findf (λ (l) (= (num-instances l lst) 4)) letters) ; E
     (findf (λ (l) (= (num-instances l lst) 9)) letters) ; F
     (findf (λ (l) (and (= (num-instances l lst) 7) (not (string-contains? four l)))) letters)))) ; G

(define (all-segments-in-str? key segments str)
  (foldl (λ (seg all) (and all (string-contains? str (vector-ref key seg)))) true segments))

(define (segments->number lst key) ; takes some segments ("bc"), the key, and returns the corresponding number
  (cond
    [(and (= (string-length lst) 6) (all-segments-in-str? key '(0 1 2 4 5 6) lst)) 0]
    [(= (string-length lst) 2) 1]
    [(and (= (string-length lst) 5) (all-segments-in-str? key '(0 2 3 4 6) lst)) 2]
    [(and (= (string-length lst) 5) (all-segments-in-str? key '(0 2 3 5 6) lst)) 3]
    [(= (string-length lst) 4) 4]
    [(and (= (string-length lst) 5) (all-segments-in-str? key '(0 1 3 5 6) lst)) 5]
    [(and (= (string-length lst) 6) (all-segments-in-str? key '(0 1 3 4 5 6) lst)) 6]
    [(= (string-length lst) 3) 7]
    [(= (string-length lst) 7) 8]
    [(and (= (string-length lst) 6) (all-segments-in-str? key '(0 1 2 3 5 6) lst)) 9]))


(define (num-list->number lst) ; '(1 2 3) -> 123
  (string->number
   (apply string-append
          (map number->string lst))))

(define (line->number line)
  (let* ([key (assign-segments (first line))]
         [output (second line)]
         [num-list (map (λ (s) (segments->number s key)) output)])
    (num-list->number num-list)))

(define part2
  (foldl (λ (l acc) (+ acc (line->number l))) 0 data))

part2
