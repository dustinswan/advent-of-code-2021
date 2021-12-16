#lang racket

(define input
  (let* ([lines (file->lines "day14.txt")]
         [template (first lines)]
         [rule-strings (drop lines 2)]
         [rules (map (λ (rs) (string-split rs " -> ")) rule-strings)])
    (list template rules)))

(define template (first input))
(define rules (second input))

(define (find-pairs template)
  (reverse
   (foldl (λ (el acc) (cons (substring template el (+ 2 el)) acc))
          '()
          (range 0 (sub1 (string-length template))))))

(define (find-inserts pairs rules)
  (map (λ (p) (second (assoc p rules))) pairs))

(define (insert-pairs template inserts)
  (foldl (λ (idx acc)
           (let* ([x (string (string-ref template idx))]
                  [insert (list-ref (append inserts (list "")) idx)])
             (string-append acc x insert)))
         "" (range 0 (string-length template))))

(define (step template rules)
  (let* ([pairs (find-pairs template)]
         [inserts (find-inserts pairs rules)]
         [next (insert-pairs template inserts)])
    next))

(define (element-counts str)
  (let* ([lst (map string (string->list str))]
         [ht (make-hash)])
    (for-each (λ (x) (hash-set! ht x (+ 1 (hash-ref ht x 0)))) lst)
    ht))

(define part1
  (let* ([final (foldl (λ (i acc) (step acc rules)) template (range 0 10))]
         [counts (element-counts final)]
         [values (hash-values counts)]
         [minimum (apply min values)]
         [maximum (apply max values)]
         [diff (- maximum minimum)])
    diff))

part1

; PART 2
; uhoh. start over

(define (inc-hash ht key amount)
  (hash-set! ht key (+ amount (hash-ref! ht key 0))))

(define (dec-hash ht key amount)
   (hash-set! ht key (- (hash-ref! ht key 0) amount)))

(define (step2 ht)
  (define pairs (filter (λ (p) (> (cdr p) 0)) (hash->list ht)))
  (for-each (λ (pair-and-count)
              (let* ([pair (car pair-and-count)]
                     [count (cdr pair-and-count)]
                     [ins (second (assoc pair rules))]
                [new1 (string-append (substring pair 0 1) ins)]
                [new2 (string-append ins (substring pair 1 2))])
                (inc-hash ht new1 count)
                (inc-hash ht new2 count)
                (dec-hash ht pair count)))
            pairs)
  ht)

(define (pairs->element-counts ht)
  (let* ([el-ht (make-hash)]
         [pair-counts (hash->list ht)])
    (for-each (λ (p) (let* ([pair (car p)]
                            [el1 (substring pair 0 1)]
                            [el2 (substring pair 1 2)]
                            [count (cdr p)])
                       (inc-hash el-ht el1 count)
                       (inc-hash el-ht el2 count)))
              pair-counts)
    el-ht))

(define part2
  (let* ([ht (make-hash)]
         [pairs (find-pairs template)]) ; find the initial pairs in the template
    (for-each (λ (pair) (inc-hash ht pair 1)) pairs) ; add them to the hash table
    (let* ([res (foldl (λ (el acc) (step2 acc)) ht (range 0 40))] ; step!
           [el-counts (pairs->element-counts res)]
           [first-char (substring template 0 1)]
           [last-char (substring template (sub1 (string-length template)))])
      (inc-hash el-counts first-char 1) ; add the first..
      (inc-hash el-counts last-char 1) ; and last chars twice..
      (let* ([lst (hash-map el-counts (λ (key val) (list key (/ val 2))))] ; then divide everything by 2
             [values (map second lst)]
             [minimum (apply min values)]
             [maximum (apply max values)]
             [diff (- maximum minimum)])
        diff))))
  
part2