;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SBV: Starting at 2026-05-05 20:52:55.614355089 CEST
;;;
;;;           Solver    : Z3
;;;           Executable: /home/herdi/.nix-profile/bin/z3
;;;           Options   : -nw -in -smt2
;;;
;;; This file is an auto-generated loadable SMT-Lib file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; [20:52:55.614] [Timeout: 5s] Sending:
(set-option :print-success true)
; [20:52:55.617] Received: success
; [20:52:55.617] [Timeout: 1s] Sending:
(set-option :global-declarations true)
; [20:52:55.617] Received: success
; [20:52:55.617] [Timeout: 1s] Sending:
(set-option :timeout 60000)
; [20:52:55.617] Received: success
; [20:52:55.617] [Timeout: 1s] Sending:
(set-option :produce-models true)
; [20:52:55.617] Received: success
; [20:52:55.617] Sending:
(set-logic QF_BV)
; [20:52:55.617] Received: success
; [20:52:55.617] Sending:
(define-fun s1 () (_ BitVec 32) #x00000001)
; [20:52:55.624] Received: success
; [20:52:55.624] Sending:
(define-fun s3 () (_ BitVec 32) (bvneg #x00000001))
; [20:52:55.624] Received: success
; [20:52:55.624] Sending:
(define-fun s5 () (_ BitVec 32) #x00000000)
; [20:52:55.624] Received: success
; [20:52:55.624] Sending:
(define-fun s13 () (_ BitVec 32) #x00000002)
; [20:52:55.625] Received: success
; [20:52:55.625] Sending:
(define-fun s23 () (_ BitVec 32) #x00000014)
; [20:52:55.625] Received: success
; [20:52:55.625] Sending:
(define-fun s26 () (_ BitVec 32) #x0000000a)
; [20:52:55.625] Received: success
; [20:52:55.625] Sending:
(define-fun s41 () (_ BitVec 32) #x0000001e)
; [20:52:55.625] Received: success
; [20:52:55.625] Sending:
(declare-fun s0 () (_ BitVec 32)) ; tracks user variable "parm11"
; [20:52:55.625] Received: success
; [20:52:55.625] Sending:
(define-fun s2 () Bool (bvslt s0 s1))
; [20:52:55.625] Received: success
; [20:52:55.625] Sending:
(define-fun s4 () Bool (bvslt s1 s0))
; [20:52:55.626] Received: success
; [20:52:55.626] Sending:
(define-fun s6 () (_ BitVec 32) (ite s4 s1 s5))
; [20:52:55.626] Received: success
; [20:52:55.626] Sending:
(define-fun s7 () (_ BitVec 32) (ite s2 s3 s6))
; [20:52:55.626] Received: success
; [20:52:55.626] Sending:
(define-fun s8 () Bool (= s3 s7))
; [20:52:55.626] Received: success
; [20:52:55.626] Sending:
(define-fun s9 () Bool (= s5 s7))
; [20:52:55.626] Received: success
; [20:52:55.626] Sending:
(define-fun s10 () Bool (or s8 s9))
; [20:52:55.627] Received: success
; [20:52:55.627] Sending:
(define-fun s11 () (_ BitVec 32) (ite s10 s1 s5))
; [20:52:55.627] Received: success
; [20:52:55.627] Sending:
(define-fun s12 () Bool (= s1 s11))
; [20:52:55.627] Received: success
; [20:52:55.627] Sending:
(define-fun s14 () Bool (bvslt s0 s13))
; [20:52:55.627] Received: success
; [20:52:55.627] Sending:
(define-fun s15 () Bool (bvslt s13 s0))
; [20:52:55.627] Received: success
; [20:52:55.627] Sending:
(define-fun s16 () (_ BitVec 32) (ite s15 s1 s5))
; [20:52:55.627] Received: success
; [20:52:55.628] Sending:
(define-fun s17 () (_ BitVec 32) (ite s14 s3 s16))
; [20:52:55.628] Received: success
; [20:52:55.628] Sending:
(define-fun s18 () Bool (= s3 s17))
; [20:52:55.628] Received: success
; [20:52:55.628] Sending:
(define-fun s19 () Bool (= s5 s17))
; [20:52:55.628] Received: success
; [20:52:55.628] Sending:
(define-fun s20 () Bool (or s18 s19))
; [20:52:55.628] Received: success
; [20:52:55.628] Sending:
(define-fun s21 () (_ BitVec 32) (ite s20 s1 s5))
; [20:52:55.628] Received: success
; [20:52:55.629] Sending:
(define-fun s22 () Bool (= s1 s21))
; [20:52:55.629] Received: success
; [20:52:55.629] Sending:
(define-fun s24 () (_ BitVec 32) (bvadd s0 s23))
; [20:52:55.629] Received: success
; [20:52:55.629] Sending:
(define-fun s25 () (_ BitVec 32) (ite s22 s0 s24))
; [20:52:55.629] Received: success
; [20:52:55.629] Sending:
(define-fun s27 () (_ BitVec 32) (bvadd s0 s26))
; [20:52:55.629] Received: success
; [20:52:55.629] Sending:
(define-fun s28 () Bool (bvslt s27 s13))
; [20:52:55.629] Received: success
; [20:52:55.630] Sending:
(define-fun s29 () Bool (bvslt s13 s27))
; [20:52:55.630] Received: success
; [20:52:55.630] Sending:
(define-fun s30 () (_ BitVec 32) (ite s29 s1 s5))
; [20:52:55.630] Received: success
; [20:52:55.630] Sending:
(define-fun s31 () (_ BitVec 32) (ite s28 s3 s30))
; [20:52:55.630] Received: success
; [20:52:55.630] Sending:
(define-fun s32 () Bool (= s3 s31))
; [20:52:55.630] Received: success
; [20:52:55.630] Sending:
(define-fun s33 () Bool (= s5 s31))
; [20:52:55.630] Received: success
; [20:52:55.631] Sending:
(define-fun s34 () Bool (or s32 s33))
; [20:52:55.631] Received: success
; [20:52:55.631] Sending:
(define-fun s35 () (_ BitVec 32) (ite s34 s1 s5))
; [20:52:55.631] Received: success
; [20:52:55.631] Sending:
(define-fun s36 () Bool (= s1 s35))
; [20:52:55.631] Received: success
; [20:52:55.631] Sending:
(define-fun s37 () (_ BitVec 32) (bvadd s23 s27))
; [20:52:55.631] Received: success
; [20:52:55.631] Sending:
(define-fun s38 () (_ BitVec 32) (ite s36 s27 s37))
; [20:52:55.631] Received: success
; [20:52:55.632] Sending:
(define-fun s39 () (_ BitVec 32) (ite s22 s25 s38))
; [20:52:55.632] Received: success
; [20:52:55.632] Sending:
(define-fun s40 () (_ BitVec 32) (ite s12 s0 s39))
; [20:52:55.632] Received: success
; [20:52:55.632] Sending:
(define-fun s42 () (_ BitVec 32) (bvadd s0 s41))
; [20:52:55.632] Received: success
; [20:52:55.632] Sending:
(define-fun s43 () (_ BitVec 32) (ite s36 s27 s42))
; [20:52:55.632] Received: success
; [20:52:55.632] Sending:
(define-fun s44 () (_ BitVec 32) (ite s22 s0 s43))
; [20:52:55.632] Received: success
; [20:52:55.633] Sending:
(define-fun s45 () (_ BitVec 32) (ite s12 s0 s44))
; [20:52:55.633] Received: success
; [20:52:55.633] Sending:
(define-fun s46 () Bool (= s40 s45))
; [20:52:55.633] Received: success
; [20:52:55.633] Sending:
(define-fun s47 () Bool (not s46))
; [20:52:55.633] Received: success
; [20:52:55.633] Sending:
(assert s47)
; [20:52:55.633] Received: success
; [20:52:55.633] [SEND] (check-sat)
; [20:52:55.633] Sending:
(check-sat)
; [20:52:55.634] Received: unsat
; [20:52:55.634] [RECV] unsat

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SBV: Finished at 2026-05-05 20:52:55.635884225 CEST
;;;
;;; Exit code: ExitSuccess
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
