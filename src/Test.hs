module Test where

import WhileParser
import Process
import Control.Exception
import Test.QuickCheck

-- get value from Variable
getDoubleValue :: Variable -> Double
getDoubleValue v = case v of
                      (DoubleVar d) -> d
                      _ -> error $ "error!"
getPairValue :: Variable -> (Variable, Variable)
getPairValue v = case v of 
                      (PairVar v1 v2) -> (v1, v2)
                      _ -> error $ "error!"


-- expressions
test_expr :: String -> Variable
test_expr a = runProg $ "(define (main) (return " ++ a ++ "))"

-- bool expressions
bool2str :: Bool -> String
bool2str True = "True"
bool2str False = "False"

arr1 = [True, False]
test_expr1_1 = and [(test_expr $ bool2str e) == (BoolVar e) | e <- arr1]
test_expr1_2 = and [(test_expr $ "(not " ++ (bool2str e) ++ ")") == (BoolVar (not e)) | e <- arr1]
test_expr1_3 = and [(test_expr $ "(and " ++ (bool2str e1) ++ " " ++ (bool2str e2) ++ ")") == (BoolVar (e1 && e2)) | e1 <- arr1, e2 <- arr1]
test_expr1_4 = and [(test_expr $ "(or " ++ (bool2str e1) ++ " " ++ (bool2str e2) ++ ")") == (BoolVar (e1 || e2)) | e1 <- arr1, e2 <- arr1]
test_expr1_5 = do res <- try (print $ test_expr "(or 1 True)") :: IO (Either SomeException ())
                  case res of
                      Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                      otherwise -> putStrLn $ "OK, passed this test."
test_expr1_6 = do res <- try (print $ test_expr "(not 't')") :: IO (Either SomeException ())
                  case res of
                      Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                      otherwise -> putStrLn $ "OK, passed this test."

-- number expressions
test_double_expr :: String -> Double
test_double_expr a = getDoubleValue $ test_expr a

equal_d :: Double -> Double -> Bool
equal_d a b = if (a - b) < 0.00000001 && (a - b) > -0.00000001 then True else False

test_expr2_1 = (test_expr $ "(+ 11 -10)") == (IntVar 1)
test_expr2_2 = (test_expr $ "(- 11 50)") == (IntVar (-39))
test_expr2_3 = (test_expr $ "(* 34 16)") == (IntVar 544)
test_expr2_4 = (test_expr $ "(/ -104 10)") == (IntVar (-11))
test_expr2_5 = equal_d (test_double_expr $ "(+ -4 0.5)") (-3.5)
test_expr2_6 = equal_d (test_double_expr $ "(- 12.06 10.075)") 1.985
test_expr2_7 = equal_d (test_double_expr $ "(* 1.1 6.5)") 7.15
test_expr2_8 = equal_d (test_double_expr $ "(/ 2 9.0)") 0.222222222
test_expr2_9 = (show $ test_double_expr $ "(/ 0.0 0)") == "NaN"
test_expr2_10 = (show $ test_double_expr $ "(/ 1.0 0)") == "Infinity"
test_expr2_11 = (test_expr $ "(= 10.4 10.4)") == (BoolVar True)
test_expr2_12 = (test_expr $ "(< 10.57 10.57)") == (BoolVar False)
test_expr2_13 = (test_expr $ "(<= 10.57 10.58)") == (BoolVar True)
test_expr2_14 = (test_expr $ "(> 157 -210)") == (BoolVar True)
test_expr2_15 = (test_expr $ "(>= -1.57 -1.10)") == (BoolVar False)
--
test_expr2_16 = do res <- try (print $ test_expr "(>= (-1.57) False)") :: IO (Either SomeException ())
                   case res of
                       Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                       otherwise -> putStrLn $ "OK, passed this test."
test_expr2_17 = do res <- try (print $ test_expr "(+ 14 'c')") :: IO (Either SomeException ())
                   case res of
                       Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                       otherwise -> putStrLn $ "OK, passed this test."
test_expr2_18 = do res <- try (print $ test_expr "(/ 52 Infinity)") :: IO (Either SomeException ())
                   case res of
                       Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                       otherwise -> putStrLn $ "OK, passed this test."

-- list expressions
test_pair_expr :: String -> (Variable, Variable)
test_pair_expr a = getPairValue $ test_expr a

test_expr3_1 = (fst x) == (CharVar 'b') && (snd x) == (IntVar 13) where x = test_pair_expr $ "(cons 'b' 13)"
test_expr3_2 = (test_expr "(car (cons \"bst\" 13))") == (test_expr "\"bst\"")
test_expr3_3 = (test_expr "(cdr (cons 13 False))") == (BoolVar False)

-- statements
test_stmt :: String -> Variable
test_stmt a = runProg $ "(define (main) (begin " ++ a ++ "))"

test_stmt1_1 = test_stmt "(set! a 1) (return a)" == (IntVar 1)
test_stmt1_2 = test_stmt "(set! a 1) skip (set! a 21) (return a)" == (IntVar 21)
test_stmt1_3 = test_stmt "(set! a 1) (set! a False) (return a)" == (BoolVar False)
test_stmt1_4 = test_stmt "(set! a 8.8) (set! b 4.4) (if (< a b) (set! c 'a') (set! c 'b')) (return c)" == (CharVar 'b')
test_stmt1_5 = test_stmt "(set! a 8) (set! b 4) (if (>= a b) (return 'a') (return 'b'))" == (CharVar 'a')
test_stmt1_6 = test_stmt "(set! a 80) (set! b 40) (set! c 0) (while (>= a (+ b c)) (set! c (+ c 1))) (return c)" == (IntVar 41)
test_stmt1_7 = test_stmt "(set! a 10) (if (a) (set! c 'a') (set! c 'b')) (return c)" == (CharVar 'b')

-- Some cases for testing implementation of array, the let clause, first order functions
test_ret = "(define (main) (begin (return 10)))" -- Outputs 10
test_call = "(define (test x y z) (begin (return (let q (+ x y) (+ z q))) )) (define (main) (return (test 5 10 15) ) )" -- Outputs 30
test_earlyhalt = "(define (main) (begin (set! x 10) (while (> x 0) (begin (set! x (- x 1)) (return x) ) ) ) )" -- Outputs 9 instead of 0
test_assign = "(define (main) (begin (make-vector a 10) (vector-set! a 5 10) (return (vector-ref a 5))) )" -- Outputs 10
test_miss = "(define (main) (begin (make-vector a 10) (vector-set! a 5 10) (return (vector-ref a 6))) )" -- Reports no initialization
test_bound = "(define (main) (begin (make-vector a 10) (vector-set! a 10 10)) )" -- Reports out of bound
test_missret = "(define (main) (set! x 10) )" -- Reports no return value
test_shadowing = "(define (main) (begin (set! x 10) (set! y (let x 15 x)) (return y)))" -- Outputs 15
test_recursive = "(define (add x) (if (= x 0) (return 100) (return (+ x (add (- x 1)))))) (define (main) (return (add 100)))" -- Outputs 5150
test_subarray = "(define (main) (begin (make-vector a 4) (make-vector b 4) (vector-set! a 0 b) (vector-set! b 0 1) (set! c (vector-ref a 0))" ++
                "(return (vector-ref c 0)) ))" -- Reports uninitialized value, which is actually the correct behaviour.

test_stmt2_1 = (runProg test_ret) == (IntVar 10)
test_stmt2_2 = (runProg test_call) == (IntVar 30)
test_stmt2_3 = (runProg test_earlyhalt) == (IntVar 9)
test_stmt2_4 = (runProg test_assign) == (IntVar 10)
test_stmt2_5 = do res <- try (print $ runProg test_miss) :: IO (Either SomeException ())
                  case res of
                      Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                      otherwise -> putStrLn $ "OK, passed this test."
test_stmt2_6 = do res <- try (print $ runProg test_bound) :: IO (Either SomeException ())
                  case res of
                      Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                      otherwise -> putStrLn $ "OK, passed this test."
test_stmt2_7 = do res <- try (print $ runProg test_missret) :: IO (Either SomeException ())
                  case res of
                      Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                      otherwise -> putStrLn $ "OK, passed this test."
test_stmt2_8 = (runProg test_shadowing) == (IntVar 15)
test_stmt2_9 = (runProg test_recursive) == (IntVar 5150)
test_stmt2_10 = do res <- try (print $ runProg test_subarray) :: IO (Either SomeException ())
                   case res of
                      Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                      otherwise -> putStrLn $ "OK, passed this test."

-- Cases for anonymous function, and passing anonymous functions as parameters
test_lambda_base = "(set! x (lambda d (+ d 5))) (define (main) (return (x 10)))"
test_multi_lambda = "(set! x (lambda (q w e) (* q (+ w e)))) (define (main) (return (x 42 53 53)))"
test_passing_partial = "(set! x (lambda (q w e) (+ q (+ w e)))) (set! y (lambda d (+ (d 10) (d 15)))) (define (main) (return (y (x -10 -15))))"

test_stmt3_1 = (runProg test_lambda_base) == (IntVar 15)
test_stmt3_2 = (runProg test_multi_lambda) == (IntVar 4452)
test_stmt3_3 = (runProg test_multi_lambda) == (IntVar (-25))

-- Cases for dynamic scoping and zero parameter functions
test_scoping_1 = "(define (f1) (begin (set! a 10) (return (f2)))) (define (f2) (return a)) (set! main f1)"
test_scoping_2 = "(define (f1 x) (begin (set! a x) (return 0))) (set! tmp (lambda x (f1 x))) (define (main) (return (+ (tmp 5) a)))"
test_scoping_3 = "(define (f1 x) (begin (set! a x) (return (tmp x)))) (set! tmp (lambda x (+ a x))) (define (main) (return (f1 5)))"
test_scoping_4 = "(define (main) (return (+ a b))) (set! a 5) (set! b 5) "

test_stmt3_4 = (runProg test_scoping_1) == (IntVar 10)
test_stmt3_5 = do res <- try (print $ runProg test_scoping_2) :: IO (Either SomeException ())
                  case res of
                       Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                       otherwise -> putStrLn $ "OK, passed this test."
test_stmt3_6 = (runProg test_scoping_3) == (IntVar 10)
test_stmt3_7 = (runProg test_scoping_4) == (IntVar 10)

-- This should report error
test_zero_params = "(define (f1) (return 0)) (define (main) (return (f1 0)))"
-- test_func1 = "(define (main x y) (begin (set! a (let z 100 (* x y))) (set! b (othercall a b 199)) (return z))) (define (PureRandom) (return 4))"
-- test_func2 = "(set! a 0) (set! y (lambda (p q r s) (* q r))) (set! x (lambda p (+ p 5))) (set! z (x y y))"

test_stmt3_8 = do res <- try (print $ runProg test_zero_params) :: IO (Either SomeException ())
                  case res of
                       Left e -> putStrLn $ "+++ OK, Wrong Input: " ++ show e
                       otherwise -> putStrLn $ "OK, passed this test."

test_exprs = do
          quickCheck test_expr1_1
          quickCheck test_expr1_2
          quickCheck test_expr1_3
          quickCheck test_expr1_4
          test_expr1_5
          test_expr1_6
          quickCheck test_expr2_1
          quickCheck test_expr2_2
          quickCheck test_expr2_3
          quickCheck test_expr2_4
          quickCheck test_expr2_5
          quickCheck test_expr2_6
          quickCheck test_expr2_7
          quickCheck test_expr2_8
          quickCheck test_expr2_9
          quickCheck test_expr2_10
          quickCheck test_expr2_11
          quickCheck test_expr2_12
          quickCheck test_expr2_13
          quickCheck test_expr2_14
          quickCheck test_expr2_15
          test_expr2_16
          test_expr2_17
          test_expr2_18
          quickCheck test_expr3_1
          quickCheck test_expr3_2
          quickCheck test_expr3_3

test_stmts = do        
          quickCheck test_stmt1_1
          quickCheck test_stmt1_2
          quickCheck test_stmt1_3
          quickCheck test_stmt1_4
          quickCheck test_stmt1_5
          quickCheck test_stmt1_6
          quickCheck test_stmt2_1
          quickCheck test_stmt2_2
          quickCheck test_stmt2_3
          quickCheck test_stmt2_4
          test_stmt2_5
          test_stmt2_6
          test_stmt2_7
          quickCheck test_stmt2_8
          quickCheck test_stmt2_9
          test_stmt2_10
          quickCheck test_stmt3_1
          quickCheck test_stmt3_2
          quickCheck test_stmt3_3
          quickCheck test_stmt3_4
          test_stmt3_5
          quickCheck test_stmt3_6
          quickCheck test_stmt3_7
          test_stmt3_8