#lang racket

(define data (map string->list (file->lines "day10.txt")))

(define pairs (list (list #\{ #\}) (list #\( #\)) (list #\[ #\]) (list #\< #\>)))
(define opening (map first pairs))
(define closing (map second pairs))

(define (go input stack) ; returns the incomplete list OR the first broken bracket if corrupted
  (if (= 0 (length input)) ; if we've run out of input..
      stack ; we've got an incomplete chunk. return the stack
      (let ([x (first input)] [xs (rest input)]) ; grab the first element of the input
        (if (member x opening) ; if it is an opening bracket
            (go xs (cons x stack)) ; push it on the stack
            (let* ([y (first stack)] ; otherwise, it's a closing bracket. grab the top of the stack..
                   [matching (second (assoc y pairs))]) ; and find its matching closing bracket
              (if (equal? matching x) ; if they match..
                  (go xs (rest stack)) ; remove it and recurse
                  x)))))) ; if they don't match, return the offending bracket

(define part1
  (let* ([res (map (λ (x) (go x '())) data)]
        [bads (filter (λ (x) (not (list? x))) res)] ; filter out the lists - they're the incomplete chunks
        [costs (list (list #\) 3) (list #\] 57) (list #\} 1197) (list #\> 25137))]
        [points (map (λ (x) (second (assoc x costs))) bads)]
        [total (apply + points)])
    total))

part1

; PART 2

(define (complete input stack) ; find the necessary closing brackets to complete the chunk
  (if (= 0 (length input))
      stack
      (let* ([x (first input)]
             [matching (second (assoc x pairs))])
        (complete (rest input) (cons matching stack)))))

(define (score chunk)
  (let ([costs (list (list #\) 1) (list #\] 2) (list #\} 3) (list #\> 4))])
  (foldl (λ (x acc) (+ (second (assoc x costs)) (* 5 acc))) 0 chunk)))

(define part2
  (let* ([res (map (λ (x) (go x '())) data)]
         [rev (reverse res)] ; reverse so we can use `first` & `cons`
         [incompletes (filter (λ (x) (list? x)) res)] ; filter out the lists - they're the incomplete chunks
         [completing (map reverse (map (λ (x) (complete x '())) incompletes))] ; un-reverse it
         [scores (map score completing)]
         [sorted (sort scores <)])
    (list-ref sorted (/ (- (length sorted) 1) 2)))) ; find the median scoring chunk

part2