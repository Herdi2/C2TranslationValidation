;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SBV: Starting at 2026-05-05 20:47:18.283220903 CEST
;;;
;;;           Solver    : Z3
;;;           Executable: /home/herdi/.nix-profile/bin/z3
;;;           Options   : -nw -in -smt2
;;;
;;; This file is an auto-generated loadable SMT-Lib file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; [20:47:18.283] [Timeout: 5s] Sending:
(set-option :print-success true)
; [20:47:18.285] Received: success
; [20:47:18.285] [Timeout: 1s] Sending:
(set-option :global-declarations true)
; [20:47:18.285] Received: success
; [20:47:18.286] [Timeout: 1s] Sending:
(set-option :timeout 60000)
; [20:47:18.286] Received: success
; [20:47:18.286] [Timeout: 1s] Sending:
(set-option :produce-models true)
; [20:47:18.286] Received: success
; [20:47:18.286] Sending:
(set-logic QF_UFBV)
; [20:47:18.286] Received: success
; [20:47:18.286] Sending:
(define-fun s2 () (_ BitVec 32) #x00000000)
; [20:47:18.293] Received: success
; [20:47:18.293] Sending:
(define-fun s4 () (_ BitVec 32) (bvneg #x00000001))
; [20:47:18.293] Received: success
; [20:47:18.293] Sending:
(define-fun s6 () (_ BitVec 32) #x00000001)
; [20:47:18.293] Received: success
; [20:47:18.293] Sending:
(define-fun s12 () (_ BitVec 32) #x00000027)
; [20:47:18.293] Received: success
; [20:47:18.293] Sending:
(define-fun s22 () (_ BitVec 32) #x0000003b)
; [20:47:18.293] Received: success
; [20:47:18.293] Sending:
(define-fun s32 () (_ BitVec 32) #x00000040)
; [20:47:18.294] Received: success
; [20:47:18.294] Sending:
(define-fun s35 () (_ BitVec 32) (bvneg #x80000000))
; [20:47:18.294] Received: success
; [20:47:18.294] Sending:
(define-fun s41 () (_ BitVec 32) #x7fffffff)
; [20:47:18.294] Received: success
; [20:47:18.294] Sending:
(define-fun s53 () (_ BitVec 32) #x00000002)
; [20:47:18.294] Received: success
; [20:47:18.294] Sending:
(define-fun s64 () (_ BitVec 32) #x00000000)
; [20:47:18.294] Received: success
; [20:47:18.294] Sending:
(declare-fun s0 () Bool) ; tracks user variable "parm10"
; [20:47:18.294] Received: success
; [20:47:18.294] Sending:
(declare-fun s1 () (_ BitVec 32)) ; tracks user variable "parm11"
; [20:47:18.294] Received: success
; [20:47:18.295] Sending:
(declare-fun aliasclass_CmpUControl_12 ((_ BitVec 32)) (_ BitVec 64))
; [20:47:18.295] Received: success
; [20:47:18.295] Sending:
(declare-fun initialIntMem ((_ BitVec 64)) (_ BitVec 32))
; [20:47:18.295] Received: success
; [20:47:18.295] Sending:
(define-fun s3 () Bool (bvslt s1 s2))
; [20:47:18.295] Received: success
; [20:47:18.295] Sending:
(define-fun s5 () Bool (bvslt s2 s1))
; [20:47:18.295] Received: success
; [20:47:18.295] Sending:
(define-fun s7 () (_ BitVec 32) (ite s5 s6 s2))
; [20:47:18.295] Received: success
; [20:47:18.295] Sending:
(define-fun s8 () (_ BitVec 32) (ite s3 s4 s7))
; [20:47:18.295] Received: success
; [20:47:18.295] Sending:
(define-fun s9 () Bool (= s2 s8))
; [20:47:18.296] Received: success
; [20:47:18.296] Sending:
(define-fun s10 () (_ BitVec 32) (ite s9 s2 s6))
; [20:47:18.296] Received: success
; [20:47:18.296] Sending:
(define-fun s11 () Bool (= s6 s10))
; [20:47:18.296] Received: success
; [20:47:18.296] Sending:
(define-fun s13 () (_ BitVec 64) (aliasclass_CmpUControl_12 s12))
; [20:47:18.296] Received: success
; [20:47:18.296] Sending:
(define-fun s14 () (_ BitVec 32) (initialIntMem s13))
; [20:47:18.296] Received: success
; [20:47:18.296] Sending:
(define-fun s15 () Bool (bvslt s14 s2))
; [20:47:18.296] Received: success
; [20:47:18.296] Sending:
(define-fun s16 () Bool (bvslt s2 s14))
; [20:47:18.296] Received: success
; [20:47:18.297] Sending:
(define-fun s17 () (_ BitVec 32) (ite s16 s6 s2))
; [20:47:18.297] Received: success
; [20:47:18.297] Sending:
(define-fun s18 () (_ BitVec 32) (ite s15 s4 s17))
; [20:47:18.297] Received: success
; [20:47:18.297] Sending:
(define-fun s19 () Bool (= s4 s18))
; [20:47:18.297] Received: success
; [20:47:18.297] Sending:
(define-fun s20 () (_ BitVec 32) (ite s19 s6 s2))
; [20:47:18.297] Received: success
; [20:47:18.297] Sending:
(define-fun s21 () Bool (= s6 s20))
; [20:47:18.297] Received: success
; [20:47:18.297] Sending:
(define-fun s23 () Bool (bvslt s14 s6))
; [20:47:18.297] Received: success
; [20:47:18.297] Sending:
(define-fun s24 () Bool (bvslt s6 s14))
; [20:47:18.298] Received: success
; [20:47:18.298] Sending:
(define-fun s25 () (_ BitVec 32) (ite s24 s6 s2))
; [20:47:18.298] Received: success
; [20:47:18.298] Sending:
(define-fun s26 () (_ BitVec 32) (ite s23 s4 s25))
; [20:47:18.298] Received: success
; [20:47:18.298] Sending:
(define-fun s27 () Bool (= s4 s26))
; [20:47:18.298] Received: success
; [20:47:18.298] Sending:
(define-fun s28 () Bool (= s2 s26))
; [20:47:18.298] Received: success
; [20:47:18.298] Sending:
(define-fun s29 () Bool (or s27 s28))
; [20:47:18.298] Received: success
; [20:47:18.298] Sending:
(define-fun s30 () (_ BitVec 32) (ite s29 s6 s2))
; [20:47:18.298] Received: success
; [20:47:18.299] Sending:
(define-fun s31 () Bool (= s6 s30))
; [20:47:18.299] Received: success
; [20:47:18.299] Sending:
(define-fun s33 () (_ BitVec 32) (ite s31 s32 s22))
; [20:47:18.299] Received: success
; [20:47:18.299] Sending:
(define-fun s34 () (_ BitVec 32) (ite s21 s22 s33))
; [20:47:18.299] Received: success
; [20:47:18.299] Sending:
(define-fun s36 () Bool (bvslt s35 s14))
; [20:47:18.299] Received: success
; [20:47:18.299] Sending:
(define-fun s37 () (_ BitVec 32) (ite s36 s6 s2))
; [20:47:18.299] Received: success
; [20:47:18.299] Sending:
(define-fun s38 () Bool (= s4 s37))
; [20:47:18.299] Received: success
; [20:47:18.300] Sending:
(define-fun s39 () (_ BitVec 32) (ite s38 s6 s2))
; [20:47:18.300] Received: success
; [20:47:18.300] Sending:
(define-fun s40 () Bool (= s6 s39))
; [20:47:18.300] Received: success
; [20:47:18.300] Sending:
(define-fun s42 () Bool (bvslt s14 s41))
; [20:47:18.300] Received: success
; [20:47:18.300] Sending:
(define-fun s43 () (_ BitVec 32) (ite s42 s4 s2))
; [20:47:18.300] Received: success
; [20:47:18.300] Sending:
(define-fun s44 () Bool (= s4 s43))
; [20:47:18.300] Received: success
; [20:47:18.300] Sending:
(define-fun s45 () Bool (= s2 s43))
; [20:47:18.300] Received: success
; [20:47:18.300] Sending:
(define-fun s46 () Bool (or s44 s45))
; [20:47:18.301] Received: success
; [20:47:18.301] Sending:
(define-fun s47 () (_ BitVec 32) (ite s46 s6 s2))
; [20:47:18.301] Received: success
; [20:47:18.301] Sending:
(define-fun s48 () Bool (= s6 s47))
; [20:47:18.301] Received: success
; [20:47:18.301] Sending:
(define-fun s49 () (_ BitVec 32) (ite s48 s32 s22))
; [20:47:18.301] Received: success
; [20:47:18.301] Sending:
(define-fun s50 () (_ BitVec 32) (ite s40 s22 s49))
; [20:47:18.301] Received: success
; [20:47:18.301] Sending:
(define-fun s51 () (_ BitVec 32) (ite s11 s34 s50))
; [20:47:18.301] Received: success
; [20:47:18.301] Sending:
(define-fun s52 () (_ BitVec 32) s14)
; [20:47:18.301] Received: success
; [20:47:18.302] Sending:
(define-fun s54 () Bool (bvult s52 s53))
; [20:47:18.302] Received: success
; [20:47:18.302] Sending:
(define-fun s55 () Bool (bvugt s52 s53))
; [20:47:18.302] Received: success
; [20:47:18.302] Sending:
(define-fun s56 () (_ BitVec 32) (ite s55 s6 s2))
; [20:47:18.302] Received: success
; [20:47:18.302] Sending:
(define-fun s57 () (_ BitVec 32) (ite s54 s4 s56))
; [20:47:18.302] Received: success
; [20:47:18.302] Sending:
(define-fun s58 () Bool (= s4 s57))
; [20:47:18.302] Received: success
; [20:47:18.302] Sending:
(define-fun s59 () (_ BitVec 32) (ite s58 s6 s2))
; [20:47:18.302] Received: success
; [20:47:18.302] Sending:
(define-fun s60 () Bool (= s6 s59))
; [20:47:18.303] Received: success
; [20:47:18.303] Sending:
(define-fun s61 () (_ BitVec 32) (ite s60 s32 s22))
; [20:47:18.303] Received: success
; [20:47:18.303] Sending:
(define-fun s62 () (_ BitVec 32) (bvsub s14 s35))
; [20:47:18.303] Received: success
; [20:47:18.304] Sending:
(define-fun s63 () (_ BitVec 32) s62)
; [20:47:18.304] Received: success
; [20:47:18.304] Sending:
(define-fun s65 () Bool (bvugt s63 s64))
; [20:47:18.304] Received: success
; [20:47:18.304] Sending:
(define-fun s66 () (_ BitVec 32) (ite s65 s6 s2))
; [20:47:18.304] Received: success
; [20:47:18.304] Sending:
(define-fun s67 () Bool (= s4 s66))
; [20:47:18.304] Received: success
; [20:47:18.304] Sending:
(define-fun s68 () (_ BitVec 32) (ite s67 s6 s2))
; [20:47:18.304] Received: success
; [20:47:18.304] Sending:
(define-fun s69 () Bool (= s6 s68))
; [20:47:18.304] Received: success
; [20:47:18.304] Sending:
(define-fun s70 () (_ BitVec 32) (ite s69 s32 s22))
; [20:47:18.304] Received: success
; [20:47:18.304] Sending:
(define-fun s71 () (_ BitVec 32) (ite s11 s61 s70))
; [20:47:18.305] Received: success
; [20:47:18.305] Sending:
(define-fun s72 () Bool (= s51 s71))
; [20:47:18.305] Received: success
; [20:47:18.305] Sending:
(define-fun s73 () Bool (not s72))
; [20:47:18.305] Received: success
; [20:47:18.305] Sending:
(assert s73)
; [20:47:18.305] Received: success
; [20:47:18.305] [SEND] (check-sat)
; [20:47:18.305] Sending:
(check-sat)
; [20:47:18.311] Received: sat
; [20:47:18.311] [RECV] sat
; [20:47:18.311] [SEND] (get-value (s0))
; [20:47:18.311] Sending:
(get-value (s0))
; [20:47:18.311] Received: ((s0 false))
; [20:47:18.311] [RECV] ((s0 false))
; [20:47:18.311] [SEND] (get-value (s1))
; [20:47:18.311] Sending:
(get-value (s1))
; [20:47:18.311] Received: ((s1 #x00000000))
; [20:47:18.312] [RECV] ((s1 #x00000000))
; [20:47:18.312] [Timeout: 1s] Sending:
(set-option :pp.max_depth      4294967295)
; [20:47:18.312] Received: success
; [20:47:18.312] [GOOD] (set-option :pp.max_depth      4294967295)
; [20:47:18.312] [Timeout: 1s] Sending:
(set-option :pp.min_alias_size 4294967295)
; [20:47:18.312] Received: success
; [20:47:18.312] [GOOD] (set-option :pp.min_alias_size 4294967295)
; [20:47:18.312] [Timeout: 1s] Sending:
(set-option :model.inline_def  true      )
; [20:47:18.312] Received: success
; [20:47:18.312] [GOOD] (set-option :model.inline_def  true      )
; [20:47:18.312] [SEND] (get-value (aliasclass_CmpUControl_12))
; [20:47:18.313] Sending:
(get-value (aliasclass_CmpUControl_12))
; [20:47:18.313] Received: ((aliasclass_CmpUControl_12 ((as const (Array (_ BitVec 32) (_ BitVec 64))) #x0000000000000000)))
; [20:47:18.313] [RECV] ((aliasclass_CmpUControl_12 ((as const (Array (_ BitVec 32) (_ BitVec 64))) #x0000000000000000)))
; [20:47:18.313] [SEND] (get-value (initialIntMem))
; [20:47:18.313] Sending:
(get-value (initialIntMem))
; [20:47:18.313] Received: ((initialIntMem ((as const (Array (_ BitVec 64) (_ BitVec 32))) #x07a2606a)))
; [20:47:18.314] [RECV] ((initialIntMem ((as const (Array (_ BitVec 64) (_ BitVec 32))) #x07a2606a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SBV: Finished at 2026-05-05 20:47:18.315689059 CEST
;;;
;;; Exit code: ExitSuccess
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
