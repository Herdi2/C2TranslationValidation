;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SBV: Starting at 2026-05-05 20:49:32.407611921 CEST
;;;
;;;           Solver    : Z3
;;;           Executable: /home/herdi/.nix-profile/bin/z3
;;;           Options   : -nw -in -smt2
;;;
;;; This file is an auto-generated loadable SMT-Lib file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; [20:49:32.408] [Timeout: 5s] Sending:
(set-option :print-success true)
; [20:49:32.410] Received: success
; [20:49:32.410] [Timeout: 1s] Sending:
(set-option :global-declarations true)
; [20:49:32.410] Received: success
; [20:49:32.410] [Timeout: 1s] Sending:
(set-option :timeout 60000)
; [20:49:32.410] Received: success
; [20:49:32.410] [Timeout: 1s] Sending:
(set-option :produce-models true)
; [20:49:32.411] Received: success
; [20:49:32.411] Sending:
(set-logic QF_BV)
; [20:49:32.411] Received: success
; [20:49:32.411] Sending:
(define-fun s3 () (_ BitVec 32) (bvneg #x00000001))
; [20:49:32.417] Received: success
; [20:49:32.418] Sending:
(define-fun s5 () (_ BitVec 32) #x00000001)
; [20:49:32.418] Received: success
; [20:49:32.418] Sending:
(define-fun s6 () (_ BitVec 32) #x00000000)
; [20:49:32.418] Received: success
; [20:49:32.418] Sending:
(define-fun s14 () (_ BitVec 32) #x00000003)
; [20:49:32.418] Received: success
; [20:49:32.418] Sending:
(define-fun s18 () (_ BitVec 32) #x00000002)
; [20:49:32.418] Received: success
; [20:49:32.418] Sending:
(declare-fun s0 () (_ BitVec 32)) ; tracks user variable "parm11"
; [20:49:32.418] Received: success
; [20:49:32.419] Sending:
(declare-fun s1 () (_ BitVec 32)) ; tracks user variable "parm12"
; [20:49:32.419] Received: success
; [20:49:32.419] Sending:
(define-fun s2 () Bool (bvslt s0 s1))
; [20:49:32.419] Received: success
; [20:49:32.419] Sending:
(define-fun s4 () Bool (bvslt s1 s0))
; [20:49:32.419] Received: success
; [20:49:32.419] Sending:
(define-fun s7 () (_ BitVec 32) (ite s4 s5 s6))
; [20:49:32.419] Received: success
; [20:49:32.419] Sending:
(define-fun s8 () (_ BitVec 32) (ite s2 s3 s7))
; [20:49:32.419] Received: success
; [20:49:32.419] Sending:
(define-fun s9 () Bool (= s3 s8))
; [20:49:32.420] Received: success
; [20:49:32.420] Sending:
(define-fun s10 () Bool (= s6 s8))
; [20:49:32.420] Received: success
; [20:49:32.420] Sending:
(define-fun s11 () Bool (or s9 s10))
; [20:49:32.420] Received: success
; [20:49:32.420] Sending:
(define-fun s12 () (_ BitVec 32) (ite s11 s5 s6))
; [20:49:32.420] Received: success
; [20:49:32.420] Sending:
(define-fun s13 () Bool (= s5 s12))
; [20:49:32.420] Received: success
; [20:49:32.420] Sending:
(define-fun s15 () (_ BitVec 32) (bvsub s14 s0))
; [20:49:32.420] Received: success
; [20:49:32.420] Sending:
(define-fun s16 () (_ BitVec 32) (ite s10 s6 s5))
; [20:49:32.421] Received: success
; [20:49:32.421] Sending:
(define-fun s17 () Bool (= s5 s16))
; [20:49:32.421] Received: success
; [20:49:32.421] Sending:
(define-fun s19 () (_ BitVec 32) (ite s17 s18 s5))
; [20:49:32.421] Received: success
; [20:49:32.421] Sending:
(define-fun s20 () (_ BitVec 32) (ite s13 s15 s19))
; [20:49:32.421] Received: success
; [20:49:32.421] Sending:
(define-fun s21 () Bool (= s5 s8))
; [20:49:32.421] Received: success
; [20:49:32.421] Sending:
(define-fun s22 () (_ BitVec 32) (ite s21 s5 s6))
; [20:49:32.421] Received: success
; [20:49:32.421] Sending:
(define-fun s23 () Bool (= s5 s22))
; [20:49:32.422] Received: success
; [20:49:32.422] Sending:
(define-fun s24 () (_ BitVec 32) (ite s23 s5 s15))
; [20:49:32.422] Received: success
; [20:49:32.422] Sending:
(define-fun s25 () Bool (= s20 s24))
; [20:49:32.422] Received: success
; [20:49:32.422] Sending:
(define-fun s26 () Bool (not s25))
; [20:49:32.422] Received: success
; [20:49:32.422] Sending:
(assert s26)
; [20:49:32.422] Received: success
; [20:49:32.422] [SEND] (check-sat)
; [20:49:32.422] Sending:
(check-sat)
; [20:49:32.429] Received: sat
; [20:49:32.429] [RECV] sat
; [20:49:32.429] [SEND] (get-value (s0))
; [20:49:32.429] Sending:
(get-value (s0))
; [20:49:32.429] Received: ((s0 #x7f0003ff))
; [20:49:32.429] [RECV] ((s0 #x7f0003ff))
; [20:49:32.429] [SEND] (get-value (s1))
; [20:49:32.430] Sending:
(get-value (s1))
; [20:49:32.430] Received: ((s1 #x80fffc00))
; [20:49:32.430] [RECV] ((s1 #x80fffc00))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SBV: Finished at 2026-05-05 20:49:32.431656663 CEST
;;;
;;; Exit code: ExitSuccess
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
