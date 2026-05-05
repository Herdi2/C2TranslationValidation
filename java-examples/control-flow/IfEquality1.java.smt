;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SBV: Starting at 2026-05-05 20:58:41.749751542 CEST
;;;
;;;           Solver    : Z3
;;;           Executable: /home/herdi/.nix-profile/bin/z3
;;;           Options   : -nw -in -smt2
;;;
;;; This file is an auto-generated loadable SMT-Lib file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; [20:58:41.750] [Timeout: 5s] Sending:
(set-option :print-success true)
; [20:58:41.752] Received: success
; [20:58:41.752] [Timeout: 1s] Sending:
(set-option :global-declarations true)
; [20:58:41.752] Received: success
; [20:58:41.752] [Timeout: 1s] Sending:
(set-option :timeout 60000)
; [20:58:41.752] Received: success
; [20:58:41.752] [Timeout: 1s] Sending:
(set-option :produce-models true)
; [20:58:41.753] Received: success
; [20:58:41.753] Sending:
(set-logic QF_BV)
; [20:58:41.753] Received: success
; [20:58:41.753] Sending:
(define-fun s1 () (_ BitVec 32) #x00000000)
; [20:58:41.761] Received: success
; [20:58:41.761] Sending:
(define-fun s3 () (_ BitVec 32) (bvneg #x00000001))
; [20:58:41.761] Received: success
; [20:58:41.761] Sending:
(define-fun s5 () (_ BitVec 32) #x00000001)
; [20:58:41.761] Received: success
; [20:58:41.761] Sending:
(define-fun s22 () (_ BitVec 32) #x0000000b)
; [20:58:41.761] Received: success
; [20:58:41.761] Sending:
(declare-fun s0 () (_ BitVec 32)) ; tracks user variable "parm11"
; [20:58:41.761] Received: success
; [20:58:41.762] Sending:
(define-fun s2 () Bool (bvslt s0 s1))
; [20:58:41.762] Received: success
; [20:58:41.762] Sending:
(define-fun s4 () Bool (bvslt s1 s0))
; [20:58:41.762] Received: success
; [20:58:41.762] Sending:
(define-fun s6 () (_ BitVec 32) (ite s4 s5 s1))
; [20:58:41.762] Received: success
; [20:58:41.762] Sending:
(define-fun s7 () (_ BitVec 32) (ite s2 s3 s6))
; [20:58:41.762] Received: success
; [20:58:41.762] Sending:
(define-fun s8 () Bool (= s3 s7))
; [20:58:41.762] Received: success
; [20:58:41.762] Sending:
(define-fun s9 () Bool (= s1 s7))
; [20:58:41.763] Received: success
; [20:58:41.763] Sending:
(define-fun s10 () Bool (or s8 s9))
; [20:58:41.763] Received: success
; [20:58:41.763] Sending:
(define-fun s11 () (_ BitVec 32) (ite s10 s5 s1))
; [20:58:41.763] Received: success
; [20:58:41.763] Sending:
(define-fun s12 () Bool (= s5 s11))
; [20:58:41.763] Received: success
; [20:58:41.763] Sending:
(define-fun s13 () Bool (bvslt s0 s5))
; [20:58:41.763] Received: success
; [20:58:41.763] Sending:
(define-fun s14 () Bool (bvslt s5 s0))
; [20:58:41.763] Received: success
; [20:58:41.763] Sending:
(define-fun s15 () (_ BitVec 32) (ite s14 s5 s1))
; [20:58:41.764] Received: success
; [20:58:41.764] Sending:
(define-fun s16 () (_ BitVec 32) (ite s13 s3 s15))
; [20:58:41.764] Received: success
; [20:58:41.764] Sending:
(define-fun s17 () Bool (= s3 s16))
; [20:58:41.764] Received: success
; [20:58:41.764] Sending:
(define-fun s18 () Bool (= s1 s16))
; [20:58:41.764] Received: success
; [20:58:41.764] Sending:
(define-fun s19 () Bool (or s17 s18))
; [20:58:41.764] Received: success
; [20:58:41.764] Sending:
(define-fun s20 () (_ BitVec 32) (ite s19 s5 s1))
; [20:58:41.764] Received: success
; [20:58:41.765] Sending:
(define-fun s21 () Bool (= s5 s20))
; [20:58:41.765] Received: success
; [20:58:41.765] Sending:
(define-fun s23 () (_ BitVec 32) (ite s21 s5 s22))
; [20:58:41.765] Received: success
; [20:58:41.765] Sending:
(define-fun s24 () (_ BitVec 32) (ite s12 s1 s23))
; [20:58:41.765] Received: success
; [20:58:41.765] Sending:
(assert false)
; [20:58:41.765] Received: success
; [20:58:41.765] [SEND] (check-sat)
; [20:58:41.765] Sending:
(check-sat)
; [20:58:41.766] Received: unsat
; [20:58:41.766] [RECV] unsat

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SBV: Finished at 2026-05-05 20:58:41.767383895 CEST
;;;
;;; Exit code: ExitSuccess
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
